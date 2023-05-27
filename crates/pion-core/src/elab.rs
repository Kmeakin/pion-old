use bumpalo::Bump;
use pion_source::location::ByteRange;
use pion_source::FileId;
use pion_surface::syntax::{self as surface, Symbol};

use self::unify::{PartialRenaming, UnifyCtx};
use crate::db::CoreDatabase;
use crate::distill::DistillCtx;
use crate::env::{EnvLen, Index, Level, SharedEnv, UniqueEnv};
use crate::reporting::Message;
use crate::semantics::{ElimEnv, EvalEnv, QuoteEnv, ZonkEnv};
use crate::syntax::*;

mod expr;
mod r#match;
mod pat;
pub mod unify;

/// Elaboration context.
pub struct ElabCtx<'db, 'arena, 'message> {
    db: &'db dyn CoreDatabase,
    arena: &'arena Bump,
    file: FileId,
    temp_arena: Bump,
    local_env: LocalEnv<'arena>,
    meta_env: MetaEnv<'arena>,
    renaming: PartialRenaming,
    on_message: &'message mut dyn FnMut(Message),
}

impl<'db, 'arena, 'message> ElabCtx<'db, 'arena, 'message> {
    pub fn new(
        db: &'db dyn CoreDatabase,
        arena: &'arena Bump,
        file: FileId,
        on_message: &'message mut dyn FnMut(Message),
    ) -> Self {
        Self {
            db,
            arena,
            file,
            temp_arena: Bump::new(),
            local_env: LocalEnv::default(),
            meta_env: MetaEnv::default(),
            renaming: PartialRenaming::default(),
            on_message,
        }
    }

    fn expr_builder(&self) -> ExprBuilder<'arena> { ExprBuilder::new(self.arena) }

    pub fn elab_expr(&mut self, expr: surface::Expr<'_>) -> (Expr<'arena>, Expr<'arena>) {
        let (expr, r#type) = self.synth(&expr);
        let r#type = self.quote_env().quote(&r#type);

        let expr = self.zonk_env(self.arena).zonk(&expr);
        let r#type = self.zonk_env(self.arena).zonk(&r#type);

        self.report_unsolved_metas();
        (expr, r#type)
    }

    fn emit_message(&mut self, message: Message) { (self.on_message)(message) }

    fn report_unsolved_metas(&mut self) {
        let handler = &mut self.on_message;
        let meta_env = &self.meta_env;

        for (source, _, value) in meta_env.iter() {
            match (value, source) {
                // Ignore solved metas
                (Some(_), _)

                // These should produce an unsolved HoleExpr/PlaceholderExpr, so avoid reporting
                // unsolved meta twice
                | (None, MetaSource::HoleType(..) | MetaSource::PlaceholderType(..)) => {}

                (None, source) => handler(Message::UnsolvedMeta { source }),
            }
        }
    }

    pub fn distill_ctx(&mut self) -> DistillCtx<'arena, '_> {
        DistillCtx::new(
            self.arena,
            &mut self.local_env.names,
            &self.meta_env.sources,
        )
    }

    pub fn elim_env(&self) -> ElimEnv<'arena, '_> {
        ElimEnv::new(self.arena, &self.meta_env.values)
    }

    pub fn eval_env(&mut self) -> EvalEnv<'arena, '_> {
        let elim_env = ElimEnv::new(self.arena, &self.meta_env.values);
        elim_env.eval_env(&mut self.local_env.values)
    }

    pub fn quote_env(&self) -> QuoteEnv<'arena, 'arena, '_> {
        QuoteEnv::new(self.arena, self.elim_env(), self.local_env.values.len())
    }

    pub fn zonk_env<'out_arena>(
        &mut self,
        arena: &'out_arena Bump,
    ) -> ZonkEnv<'out_arena, 'arena, '_> {
        ZonkEnv::new(arena, self.eval_env())
    }

    pub fn unifiy_ctx(&mut self) -> UnifyCtx<'arena, '_> {
        UnifyCtx::new(
            self.arena,
            &mut self.renaming,
            self.local_env.len(),
            &mut self.meta_env.values,
        )
    }

    /// Push an unsolved expr onto the context, to be updated later during
    /// unification.
    fn push_unsolved_expr(&mut self, source: MetaSource, r#type: Type<'arena>) -> Expr<'arena> {
        let level = self.meta_env.push(source, r#type);
        Expr::InsertedMeta(
            level,
            self.arena.alloc_slice_copy(self.local_env.infos.as_slice()),
        )
    }

    fn push_unsolved_type(&mut self, source: MetaSource) -> Value<'arena> {
        let expr = self.push_unsolved_expr(source, Value::TYPE);
        self.eval_env().eval(&expr)
    }

    fn pretty_value(&mut self, value: &Value) -> String {
        let elim_env = ElimEnv::new(&self.temp_arena, &self.meta_env.values);
        let mut quote_env = QuoteEnv::new(&self.temp_arena, elim_env, self.local_env.values.len());
        let mut distill_ctx = DistillCtx::new(
            &self.temp_arena,
            &mut self.local_env.names,
            &self.meta_env.sources,
        );

        let expr = quote_env.quote(value);
        let surface_expr = distill_ctx.expr(&expr);
        let ctx = pion_surface::pretty::PrettyCtx::new(&self.temp_arena);
        let doc = ctx.expr(&surface_expr);
        let ret = doc.pretty(usize::MAX).to_string();
        self.temp_arena.reset();
        ret
    }

    /// Run `f`, potentially modifying the local environment, then restore the
    /// local environment to its previous state.
    fn with_scope<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        let initial_len = self.local_env.len();
        let result = f(self);
        self.local_env.truncate(initial_len);
        result
    }

    fn with_param<T>(
        &mut self,
        name: Option<Symbol>,
        r#type: Type<'arena>,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        self.local_env.push_param(name, r#type);
        let result = f(self);
        self.local_env.pop();
        result
    }
}

/// Local variable environment.
///
/// This is used for keeping track of local variables that are bound by the
/// program, for example by function parameters, let bindings, or pattern
/// matching.
///
/// This environment behaves as a stack.
/// - As scopes are entered, it is important to remember to call either
///   [`LocalEnv::push_def`] or [`LocalEnv::push_param`].
/// - On scope exit, it is important to remember to call [`LocalEnv::pop`].
/// - Multiple bindings can be removed at once with [`LocalEnv::truncate`].
#[derive(Default)]
struct LocalEnv<'arena> {
    /// Names of local variables.
    names: UniqueEnv<Option<Symbol>>,
    /// Information about the local binders. Used when inserting new
    /// metavariables during [evaluation][semantics::EvalEnv::eval].
    infos: UniqueEnv<BinderInfo>,
    /// Types of local variables.
    types: UniqueEnv<Value<'arena>>,
    /// Values that will be substituted for local variables during
    /// [evaluation][semantics::EvalEnv::eval].
    values: SharedEnv<Value<'arena>>,
}

impl<'arena> LocalEnv<'arena> {
    /// Get the length of the local environment.
    fn len(&self) -> EnvLen { self.names.len() }

    /// Reserve space for `additional` extra elements.
    fn reserve(&mut self, additional: usize) {
        self.names.reserve(additional);
        self.types.reserve(additional);
        self.infos.reserve(additional);
        self.values.reserve(additional);
    }

    /// Push a local binder onto the environment.
    fn push(
        &mut self,
        name: Option<Symbol>,
        info: BinderInfo,
        r#type: Value<'arena>,
        value: Value<'arena>,
    ) {
        self.names.push(name);
        self.infos.push(info);
        self.types.push(r#type);
        self.values.push(value);
    }

    fn push_def(&mut self, name: Option<Symbol>, r#type: Value<'arena>, value: Value<'arena>) {
        self.push(name, BinderInfo::Def, r#type, value);
    }

    fn push_param(&mut self, name: Option<Symbol>, r#type: Value<'arena>) {
        let value = self.next_var();
        self.push(name, BinderInfo::Param, r#type, value);
    }

    /// Pop a local binder off the environment.
    fn pop(&mut self) {
        self.names.pop();
        self.infos.pop();
        self.types.pop();
        self.values.pop();
    }

    /// Truncate the local environment.
    fn truncate(&mut self, len: EnvLen) {
        self.names.truncate(len);
        self.infos.truncate(len);
        self.types.truncate(len);
        self.values.truncate(len);
    }

    fn next_var(&self) -> Value<'arena> { Value::local(self.values.len().to_level()) }

    fn lookup(&self, name: Symbol) -> Option<(Index, &Type<'arena>)> {
        let local_var = self.names.index_of_elem(&Some(name))?;
        let local_type = self.types.get_index(local_var)?;
        Some((local_var, local_type))
    }
}

/// Metavariable environment.
///
/// This is used for keeping track of the state of [metavariables] whose
/// definitions are intended to be found through the use of [unification].
#[derive(Default)]
struct MetaEnv<'arena> {
    /// The source of inserted metavariables, used when reporting unsolved
    /// metavariables.
    sources: UniqueEnv<MetaSource>,
    /// Types of metavariables.
    types: UniqueEnv<Value<'arena>>,
    /// Values that will be substituted for metavariables during
    /// [evaluation][semantics::EvalEnv::eval].
    ///
    /// These will be set to [`None`] when a metavariable is first
    /// [inserted][Context::push_unsolved_expr], then will be set to [`Some`]
    /// if a solution is found during [`unification`].
    values: UniqueEnv<Option<Value<'arena>>>,
}

impl<'arena> MetaEnv<'arena> {
    /// Push an unsolved metavariable onto the environment.
    fn push(&mut self, source: MetaSource, r#type: Value<'arena>) -> Level {
        let level = self.types.len().to_level();

        self.sources.push(source);
        self.types.push(r#type);
        self.values.push(None);

        level
    }

    fn iter(&self) -> impl Iterator<Item = (MetaSource, &Value<'arena>, &Option<Value<'arena>>)> {
        self.sources
            .iter()
            .zip(self.types.iter())
            .zip(self.values.iter())
            .map(|((source, r#type), value)| (*source, r#type, value))
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MetaSource {
    PlaceholderType(ByteRange),
    PlaceholderExpr(ByteRange),

    HoleType(ByteRange, Symbol),
    HoleExpr(ByteRange, Symbol),

    PatType(ByteRange),

    ImplicitArg(ByteRange, Option<Symbol>),

    MatchType(ByteRange),
}

pub fn elab_def<'db, 'surface, 'core>(
    db: &'db dyn CoreDatabase,
    arena: &'core Bump,
    on_message: &'_ mut dyn FnMut(Message),
    file: FileId,
    def: surface::Def<'surface>,
) -> Def<'core> {
    let temp_arena = Bump::new();
    let mut elab_ctx = ElabCtx::new(db, &temp_arena, file, on_message);
    let (expr, r#type) = match def.r#type {
        Some(r#type) => {
            let r#type = elab_ctx.check(&r#type, &Type::TYPE);
            let type_value = elab_ctx.eval_env().eval(&r#type);
            let expr = elab_ctx.check(&def.expr, &type_value);

            (expr, r#type)
        }
        None => {
            let (expr, type_value) = elab_ctx.synth(&def.expr);
            let r#type = elab_ctx.quote_env().quote(&type_value);

            (expr, r#type)
        }
    };

    let r#type = elab_ctx.zonk_env(arena).zonk(&r#type);
    let expr = elab_ctx.zonk_env(arena).zonk(&expr);

    elab_ctx.report_unsolved_metas();

    Def::new(def.name, r#type, expr)
}
