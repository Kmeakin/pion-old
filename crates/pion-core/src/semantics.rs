use std::ops::Deref;

use scoped_arena::Scope;

use crate::env::{EnvLen, Index, Level, SliceEnv, UniqueEnv};
use crate::syntax::*;

pub struct EvalEnv<'arena, 'env> {
    elim_env: ElimEnv<'arena, 'env>,
    local_values: &'env mut UniqueEnv<Value<'arena>>,
}

impl<'arena, 'env> Deref for EvalEnv<'arena, 'env> {
    type Target = Scope<'arena>;
    fn deref(&self) -> &'arena Self::Target { self.elim_env.scope }
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
            Expr::Let(_, (_, rhs, body)) => {
                let rhs_value = self.eval(rhs);
                self.local_values.push(rhs_value);
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

impl<'arena, 'env> Deref for ElimEnv<'arena, 'env> {
    type Target = Scope<'arena>;
    fn deref(&self) -> &'arena Self::Target { self.scope }
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
        })
    }

    pub fn fun_app(&self, fun: Value<'arena>, arg: Value<'arena>) -> Value<'arena> {
        match fun {
            Value::FunLit(.., body) => self.apply_closure(body, arg),
            Value::Stuck(head, mut spine) => {
                spine.push(Elim::FunApp(arg));
                Value::Stuck(head, spine)
            }
            _ => panic!("Bad fun app: {fun:?} {arg:?}"),
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
}

/// Quotation environment.
///
/// This environment keeps track of the length of the local environment,
/// and the values of metavariables, allowing for quotation.
pub struct QuoteEnv<'arena, 'env> {
    elim_env: ElimEnv<'arena, 'env>,
    local_env: EnvLen,
}

impl<'arena, 'env> Deref for QuoteEnv<'arena, 'env> {
    type Target = Scope<'arena>;
    fn deref(&self) -> &'arena Self::Target { self.elim_env.scope }
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
                (spine.iter()).fold(self.quote_head(&head), |head, elim| match elim {
                    Elim::FunApp(arg) => Expr::FunApp(scope.to_scope((head, self.quote(arg)))),
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
        }
    }

    /// Quote an [elimination head][Head] back into a [expr][Expr].
    fn quote_head(&mut self, head: &Head) -> Expr<'arena> {
        let elim_env = self.elim_env;
        match head {
            Head::Error => Expr::Error,
            Head::Prim(prim) => Expr::Prim(*prim),
            Head::Local(var) => match self.local_env.level_to_index(*var) {
                Some(var) => Expr::Local(var),
                None => panic!("Unbound local variable: {var:?}"),
            },
            Head::Meta(var) => match elim_env.get_meta(*var) {
                Some(value) => self.quote(value),
                None => Expr::Meta(*var),
            },
        }
    }

    fn quote_closure(&mut self, closure: &Closure) -> Expr<'arena> {
        let arg = Value::local(self.local_env.next_level());
        let value = self.elim_env.apply_closure(closure.clone(), arg);

        self.push_local();
        let expr = self.quote(&value);
        self.pop_local();

        expr
    }

    fn push_local(&mut self) { self.local_env.push(); }
    fn pop_local(&mut self) { self.local_env.pop(); }
}
