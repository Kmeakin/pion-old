use pion_source::location::ByteRange;
use pion_surface::syntax::{self as surface, Symbol};
use scoped_arena::Scope;

use self::unify::{PartialRenaming, UnifyCtx};
use crate::distill::DistillCtx;
use crate::env::{EnvLen, Index, Level, SharedEnv, UniqueEnv};
use crate::prim::PrimEnv;
use crate::reporting::ElabError;
use crate::semantics::{ElimEnv, EvalEnv, QuoteEnv};
use crate::syntax::*;

mod expr;
mod r#match;
mod pat;
pub mod unify;

/// Elaboration context.
pub struct ElabCtx<'arena, 'error> {
    scope: &'arena Scope<'arena>,
    prim_env: PrimEnv<'arena>,
    local_env: LocalEnv<'arena>,
    meta_env: MetaEnv<'arena>,
    renaming: PartialRenaming,
    on_error: &'error mut dyn FnMut(ElabError),
}

impl<'arena, 'error> ElabCtx<'arena, 'error> {
    pub fn new(scope: &'arena Scope<'arena>, on_error: &'error mut dyn FnMut(ElabError)) -> Self {
        Self {
            scope,
            prim_env: PrimEnv::new(),
            local_env: LocalEnv::default(),
            meta_env: MetaEnv::default(),
            renaming: PartialRenaming::default(),
            on_error,
        }
    }

    fn expr_builder(&self) -> ExprBuilder<'arena> { ExprBuilder::new(self.scope) }

    pub fn elab_expr(&mut self, expr: surface::Expr<'_>) -> (Expr<'arena>, Expr<'arena>) {
        let (expr, r#type) = self.synth(&expr);
        let r#type = self.quote_env().quote(&r#type);

        let expr = self.eval_env().zonk(&expr);
        let r#type = self.eval_env().zonk(&r#type);

        self.report_unsolved_metas();
        (expr, r#type)
    }

    fn emit_error(&mut self, error: ElabError) { (self.on_error)(error) }

    fn report_unsolved_metas(&mut self) {
        let handler = &mut self.on_error;
        let meta_env = &self.meta_env;

        for (source, _, value) in meta_env.iter() {
            match (value, source) {
                // Ignore solved metas
                (Some(_), _)

                // These should produce an unsolved HoleExpr/PlaceholderExpr, so avoid reporting
                // unsolved meta twice
                | (None, MetaSource::HoleType(..) | MetaSource::PlaceholderType(..)) => {}

                (None, source) => handler(ElabError::UnsolvedMeta { source }),
            }
        }
    }

    pub fn distill_ctx(&mut self) -> DistillCtx<'arena, '_> {
        DistillCtx::new(
            self.scope,
            &mut self.local_env.names,
            &self.meta_env.sources,
        )
    }

    pub fn elim_env(&self) -> ElimEnv<'arena, '_> {
        ElimEnv::new(self.scope, &self.meta_env.values)
    }

    pub fn eval_env(&mut self) -> EvalEnv<'arena, '_> {
        let elim_env = ElimEnv::new(self.scope, &self.meta_env.values);
        elim_env.eval_env(&mut self.local_env.values)
    }

    pub fn quote_env(&self) -> QuoteEnv<'arena, '_> {
        QuoteEnv::new(self.scope, self.elim_env(), self.local_env.values.len())
    }

    pub fn unifiy_ctx(&mut self) -> UnifyCtx<'arena, '_> {
        UnifyCtx::new(
            self.scope,
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
            (self.scope).to_scope_from_iter(self.local_env.infos.iter().copied()),
        )
    }

    fn push_unsolved_type(&mut self, source: MetaSource) -> Value<'arena> {
        let expr = self.push_unsolved_expr(source, Value::TYPE);
        self.eval_env().eval(&expr)
    }

    fn pretty_value(&mut self, value: &Value) -> String {
        let expr = self.quote_env().quote(value);
        let surface_expr = self.distill_ctx().expr(&expr);
        let ctx = pion_surface::pretty::PrettyCtx::new(self.scope);
        let doc = ctx.expr(&surface_expr);
        doc.pretty(usize::MAX).to_string()
    }

    /// Run `f`, potentially modifying the local environment, then restore the
    /// local environment to its previous state.
    fn with_scope<T>(&mut self, mut f: impl FnMut(&mut Self) -> T) -> T {
        let initial_len = self.local_env.len();
        let result = f(self);
        self.local_env.truncate(initial_len);
        result
    }

    fn with_param<T>(
        &mut self,
        name: Option<Symbol>,
        r#type: Type<'arena>,
        mut f: impl FnMut(&mut Self) -> T,
    ) -> T {
        self.local_env.push_param(name, r#type);
        let result = f(self);
        self.local_env.pop();
        result
    }

    fn with_def<T>(
        &mut self,
        name: Option<Symbol>,
        r#type: Type<'arena>,
        value: Value<'arena>,
        mut f: impl FnMut(&mut Self) -> T,
    ) -> T {
        self.local_env.push_def(name, r#type, value);
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

#[derive(Debug, Copy, Clone)]
pub enum MetaSource {
    PlaceholderType(ByteRange),
    PlaceholderExpr(ByteRange),

    HoleType(ByteRange, Symbol),
    HoleExpr(ByteRange, Symbol),

    PatType(ByteRange),

    ImplicitArg(ByteRange, Option<Symbol>),

    MatchType(ByteRange),
}
