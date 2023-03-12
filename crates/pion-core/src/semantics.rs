use either::*;
use pion_surface::syntax::Symbol;
use scoped_arena::Scope;

use crate::env::{EnvLen, Index, Level, SliceEnv, UniqueEnv};
use crate::syntax::*;

pub struct EvalEnv<'arena, 'env> {
    elim_env: ElimEnv<'arena, 'env>,
    local_values: &'env mut UniqueEnv<Value<'arena>>,
}

impl<'arena, 'env> EvalEnv<'arena, 'env> {
    pub fn new(
        elim_env: ElimEnv<'arena, 'env>,
        local_values: &'env mut UniqueEnv<Value<'arena>>,
    ) -> Self {
        Self {
            elim_env,
            local_values,
        }
    }

    fn quote_env(&self) -> QuoteEnv<'arena, 'env> {
        QuoteEnv::new(self.elim_env, self.local_values.len())
    }

    fn get_local<'this: 'env>(&'this self, var: Index) -> &'env Value<'arena> {
        let value = self.local_values.get_index(var);
        match value {
            Some(value) => value,
            None => panic!("Unbound local variable: {var:?}"),
        }
    }

    pub fn eval(&mut self, expr: &Expr<'arena>) -> Value<'arena> {
        match expr {
            Expr::Error => Value::ERROR,
            Expr::Lit(lit) => Value::Lit(*lit),
            Expr::Prim(prim) => Value::prim(*prim),
            Expr::Local(var) => self.get_local(*var).clone(),
            Expr::Meta(var) => match self.elim_env.get_meta(*var) {
                Some(value) => value.clone(),
                None => Value::meta(*var),
            },
            Expr::InsertedMeta(var, infos) => {
                let head = self.eval(&Expr::Meta(*var));
                self.apply_binder_infos(head, infos)
            }
            Expr::Let((def, body)) => {
                let expr_value = self.eval(&def.expr);
                self.local_values.push(expr_value);
                let body_value = self.eval(body);
                self.local_values.pop();
                body_value
            }
            Expr::FunType(name, (domain, codomain)) => {
                let scope = self.elim_env.scope;
                let domain_value = self.eval(domain);
                let codomain = Closure::new(self.local_values.clone(), codomain);
                Value::FunType(*name, scope.to_scope(domain_value), codomain)
            }
            Expr::FunLit(name, (domain, body)) => {
                let scope = self.elim_env.scope;
                let type_value = self.eval(domain);
                let body = Closure::new(self.local_values.clone(), body);
                Value::FunLit(*name, scope.to_scope(type_value), body)
            }
            Expr::FunApp((fun, arg)) => {
                let fun_value = self.eval(fun);
                let arg_value = self.eval(arg);
                self.elim_env.fun_app(fun_value, arg_value)
            }
            Expr::RecordType(labels, types) => {
                let telescope = Telescope::new(self.local_values.clone(), types);
                Value::RecordType(labels, telescope)
            }
            Expr::RecordLit(labels, exprs) => {
                let scope = self.elim_env.scope;
                let exprs = exprs.iter().map(|expr| self.eval(expr));
                Value::RecordLit(labels, scope.to_scope_from_iter(exprs))
            }
            Expr::RecordProj(head, label) => {
                let head = self.eval(head);
                self.elim_env.record_proj(head, *label)
            }
        }
    }

    pub fn zonk(&mut self, expr: &Expr<'arena>) -> Expr<'arena> {
        match expr {
            Expr::Error => Expr::Error,
            Expr::Lit(lit) => Expr::Lit(*lit),
            Expr::Prim(prim) => Expr::Prim(*prim),
            Expr::Local(var) => Expr::Local(*var),
            Expr::InsertedMeta(var, infos) => match self.elim_env.get_meta(*var) {
                None => Expr::InsertedMeta(*var, infos),
                Some(value) => {
                    let value = self.apply_binder_infos(value.clone(), infos);
                    self.quote_env().quote(&value)
                }
            },
            // These exprs might be elimination spines with metavariables at
            // their head that need to be unfolded.
            Expr::Meta(..) | Expr::FunApp(..) | Expr::RecordProj(..) => {
                match self.zonk_meta_var_spines(expr) {
                    Left(expr) => expr,
                    Right(value) => self.quote_env().quote(&value),
                }
            }
            Expr::Let((def, body)) => {
                let def = LetDef {
                    name: def.name,
                    r#type: self.zonk(&def.r#type),
                    expr: self.zonk(&def.expr),
                };
                (self.local_values).push(Value::local(self.local_values.len().to_level()));
                let body = self.zonk(body);
                self.local_values.pop();
                Expr::Let(self.elim_env.scope.to_scope((def, body)))
            }
            Expr::FunType(name, (domain, body)) => {
                let domain = self.zonk(domain);
                (self.local_values).push(Value::local(self.local_values.len().to_level()));
                let body = self.zonk(body);
                self.local_values.pop();
                Expr::FunType(*name, self.elim_env.scope.to_scope((domain, body)))
            }
            Expr::FunLit(name, (domain, body)) => {
                let domain = self.zonk(domain);
                (self.local_values).push(Value::local(self.local_values.len().to_level()));
                let body = self.zonk(body);
                self.local_values.pop();
                Expr::FunLit(*name, self.elim_env.scope.to_scope((domain, body)))
            }
            Expr::RecordType(labels, types) => {
                let len = self.local_values.len();
                let types = (self.elim_env.scope).to_scope_from_iter(types.iter().map(|r#type| {
                    let r#type = self.zonk(r#type);
                    let var = Value::local(self.local_values.len().to_level());
                    self.local_values.push(var);
                    r#type
                }));
                self.local_values.truncate(len);
                Expr::RecordType(labels, types)
            }
            Expr::RecordLit(labels, exprs) => Expr::RecordLit(
                labels,
                self.elim_env
                    .scope
                    .to_scope_from_iter(exprs.iter().map(|expr| self.zonk(expr))),
            ),
        }
    }

    /// Unfold elimination spines with solved metavariables at their head.
    pub fn zonk_meta_var_spines(
        &mut self,
        expr: &Expr<'arena>,
    ) -> Either<Expr<'arena>, Value<'arena>> {
        match expr {
            Expr::Meta(var) => match self.elim_env.get_meta(*var) {
                None => Left(Expr::Meta(*var)),
                Some(value) => Right(value.clone()),
            },
            Expr::InsertedMeta(var, infos) => match self.elim_env.get_meta(*var) {
                None => Left(Expr::InsertedMeta(*var, infos)),
                Some(value) => Right(self.apply_binder_infos(value.clone(), infos)),
            },
            Expr::FunApp((fun, arg)) => match self.zonk_meta_var_spines(fun) {
                Left(fun) => {
                    let arg = self.zonk(arg);
                    Left(Expr::FunApp(self.elim_env.scope.to_scope((fun, arg))))
                }
                Right(fun_value) => {
                    let arg_value = self.eval(arg);
                    Right(self.elim_env.fun_app(fun_value, arg_value))
                }
            },
            Expr::RecordProj(head, label) => match self.zonk_meta_var_spines(head) {
                Left(head) => Left(Expr::RecordProj(self.elim_env.scope.to_scope(head), *label)),
                Right(head_value) => Right(self.elim_env.record_proj(head_value, *label)),
            },
            expr => Left(self.zonk(expr)),
        }
    }

    fn apply_binder_infos(
        &mut self,
        mut head: Value<'arena>,
        infos: &[BinderInfo],
    ) -> Value<'arena> {
        for (info, value) in Iterator::zip(infos.iter(), self.local_values.iter()) {
            head = match info {
                BinderInfo::Def => head,
                BinderInfo::Param => self.elim_env.fun_app(head, value.clone()),
            };
        }
        head
    }
}

#[derive(Clone, Copy)]
pub struct ElimEnv<'arena, 'env> {
    scope: &'arena Scope<'arena>,
    meta_values: &'env SliceEnv<Option<Value<'arena>>>,
}

impl<'arena, 'env> ElimEnv<'arena, 'env> {
    pub fn new(
        scope: &'arena Scope<'arena>,
        meta_values: &'env SliceEnv<Option<Value<'arena>>>,
    ) -> Self {
        Self { scope, meta_values }
    }

    fn get_meta<'this: 'env>(&'this self, var: Level) -> &'env Option<Value<'arena>> {
        let value = self.meta_values.get_level(var);
        match value {
            Some(value) => value,
            None => panic!("Unbound meta variable: {var:?}"),
        }
    }

    pub fn eval_env(
        &self,
        local_values: &'env mut UniqueEnv<Value<'arena>>,
    ) -> EvalEnv<'arena, 'env> {
        EvalEnv::new(*self, local_values)
    }

    /// Bring a value up-to-date with any new unification solutions that
    /// might now be present at the head of in the given value.
    pub fn update_metas(&self, value: &Value<'arena>) -> Value<'arena> {
        let mut forced_value = value.clone();
        while let Value::Stuck(Head::Meta(var), spine) = &forced_value {
            match self.get_meta(*var) {
                Some(value) => forced_value = self.apply_spine(value.clone(), spine),
                None => break,
            }
        }
        forced_value
    }

    /// Apply an expression to an elimination spine.
    fn apply_spine(&self, head: Value<'arena>, spine: &[Elim<'arena>]) -> Value<'arena> {
        spine.iter().fold(head, |head, elim| match elim {
            Elim::FunApp(arg) => self.fun_app(head, arg.clone()),
            Elim::RecordProj(label) => self.record_proj(head, *label),
        })
    }

    pub fn fun_app(&self, fun: Value<'arena>, arg: Value<'arena>) -> Value<'arena> {
        match fun {
            Value::Stuck(head, mut spine) => {
                spine.push(Elim::FunApp(arg));
                Value::Stuck(head, spine)
            }
            Value::FunLit(.., body) => self.apply_closure(body, arg),
            _ => panic!("Bad fun app: {fun:?} {arg:?}"),
        }
    }

    pub fn record_proj(&self, head: Value<'arena>, label: Symbol) -> Value<'arena> {
        match head {
            Value::Stuck(head, mut spine) => {
                spine.push(Elim::RecordProj(label));
                Value::Stuck(head, spine)
            }
            Value::RecordLit(labels, values) => {
                match Iterator::zip(labels.iter(), values.iter()).find(|(l, _)| **l == label) {
                    Some((_, value)) => value.clone(),
                    None => panic!("Bad record proj: label `{label}` not found in `{labels:?}`"),
                }
            }
            _ => panic!("Bad record proj: {head:?} {label}"),
        }
    }

    pub fn apply_closure(
        &self,
        mut closure: Closure<'arena>,
        value: Value<'arena>,
    ) -> Value<'arena> {
        closure.local_values.push(value);
        self.eval_env(&mut closure.local_values).eval(closure.expr)
    }

    pub fn split_telescope(
        &self,
        mut telescope: Telescope<'arena>,
    ) -> Option<(
        Value<'arena>,
        impl FnOnce(Value<'arena>) -> Telescope<'arena>,
    )> {
        let (expr, exprs) = telescope.exprs.split_first()?;
        let value = self.eval_env(&mut telescope.local_values).eval(expr);
        Some((value, move |prev| {
            telescope.local_values.push(prev);
            telescope.exprs = exprs;
            telescope
        }))
    }
}

/// Quotation environment.
///
/// This environment keeps track of the length of the local environment,
/// and the values of metavariables, allowing for quotation.
pub struct QuoteEnv<'arena, 'env> {
    elim_env: ElimEnv<'arena, 'env>,
    local_env: EnvLen,
}

impl<'arena, 'env> QuoteEnv<'arena, 'env> {
    pub fn new(elim_env: ElimEnv<'arena, 'env>, local_values: EnvLen) -> Self {
        Self {
            elim_env,
            local_env: local_values,
        }
    }

    /// Quote a [value][Value] back into a [expr][Expr].
    pub fn quote(&mut self, value: &Value) -> Expr<'arena> {
        let value = self.elim_env.update_metas(value);
        match value {
            Value::Lit(lit) => Expr::Lit(lit),
            Value::Stuck(head, spine) => {
                let scope = self.elim_env.scope;
                (spine.iter()).fold(self.quote_head(head), |head, elim| match elim {
                    Elim::FunApp(arg) => Expr::FunApp(scope.to_scope((head, self.quote(arg)))),
                    Elim::RecordProj(label) => Expr::RecordProj(scope.to_scope(head), *label),
                })
            }
            Value::FunType(name, domain, codomain) => {
                let scope = self.elim_env.scope;
                let domain = self.quote(domain);
                let codomain = self.quote_closure(&codomain);
                Expr::FunType(name, scope.to_scope((domain, codomain)))
            }
            Value::FunLit(name, domain, body) => {
                let scope = self.elim_env.scope;
                let domain = self.quote(domain);
                let body = self.quote_closure(&body);
                Expr::FunType(name, scope.to_scope((domain, body)))
            }
            Value::RecordType(labels, telescope) => {
                let scope = self.elim_env.scope;
                let types = self.quote_telescope(telescope);
                Expr::RecordType(scope.to_scope_from_iter(labels.iter().copied()), types)
            }
            Value::RecordLit(labels, values) => {
                let scope = self.elim_env.scope;
                let values = values.iter().map(|value| self.quote(value));
                Expr::RecordLit(
                    scope.to_scope_from_iter(labels.iter().copied()),
                    scope.to_scope_from_iter(values),
                )
            }
        }
    }

    /// Quote an [elimination head][Head] back into a [expr][Expr].
    fn quote_head(&mut self, head: Head) -> Expr<'arena> {
        let elim_env = self.elim_env;
        match head {
            Head::Error => Expr::Error,
            Head::Prim(prim) => Expr::Prim(prim),
            Head::Local(var) => match self.local_env.level_to_index(var) {
                Some(var) => Expr::Local(var),
                None => panic!("Unbound local variable: {var:?}"),
            },
            Head::Meta(var) => match elim_env.get_meta(var) {
                Some(value) => self.quote(value),
                None => Expr::Meta(var),
            },
        }
    }

    /// Quote a [closure][Closure] back into an [expr][Expr].
    fn quote_closure(&mut self, closure: &Closure) -> Expr<'arena> {
        let arg = Value::local(self.local_env.to_level());
        let value = self.elim_env.apply_closure(closure.clone(), arg);

        self.push_local();
        let expr = self.quote(&value);
        self.pop_local();

        expr
    }

    /// Quote a [telescope][Telescope] back into a slice of [exprs][Expr].
    fn quote_telescope(&mut self, telescope: Telescope) -> &'arena [Expr<'arena>] {
        let initial_local_len = self.local_env;
        let mut telescope = telescope;
        let mut exprs = Vec::with_capacity(telescope.len());

        while let Some((value, cont)) = self.elim_env.split_telescope(telescope) {
            let var = Value::local(self.local_env.to_level());
            telescope = cont(var);
            exprs.push(self.quote(&value));
            self.local_env.push();
        }

        self.local_env.truncate(initial_local_len);
        self.elim_env.scope.to_scope_from_iter(exprs)
    }

    fn push_local(&mut self) { self.local_env.push(); }
    fn pop_local(&mut self) { self.local_env.pop(); }
}
