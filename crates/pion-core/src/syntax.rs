use std::ops::ControlFlow;

use internal_iterator::InternalIterator;
use pion_source::location::ByteRange;
use pion_surface::syntax::{Plicity, Symbol};
use scoped_arena::Scope;

use crate::env::{EnvLen, Index, Level, SharedEnv};
use crate::prim::Prim;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Expr<'arena> {
    Error,
    Lit(Lit),
    Prim(Prim),
    Local(Index),
    Meta(Level),
    InsertedMeta(Level, &'arena [BinderInfo]),
    Let(&'arena (LetDef<'arena>, Self)),
    FunType(Plicity, Option<Symbol>, &'arena (Self, Self)),
    FunLit(Plicity, Option<Symbol>, &'arena (Self, Self)),
    FunApp(Plicity, &'arena (Self, Self)),
    RecordType(&'arena [Symbol], &'arena [Self]),
    RecordLit(&'arena [Symbol], &'arena [Self]),
    RecordProj(&'arena Self, Symbol),
    Match(
        &'arena (Self, Option<(Option<Symbol>, Self)>),
        &'arena [(Lit, Self)],
    ),
}

impl<'arena> Expr<'arena> {
    pub fn binds_local(&self, var: EnvLen) -> bool {
        self.subexprs(var)
            .any(|(env, expr)| matches!(expr, Expr::Local(var) if env == *var))
    }

    #[must_use]
    pub fn shift(&self, scope: &'arena Scope<'arena>, amount: EnvLen) -> Expr<'arena> {
        self.shift_inner(scope, Index::new(), amount)
    }

    fn shift_inner(
        &self,
        scope: &'arena Scope<'arena>,
        mut min: Index,
        amount: EnvLen,
    ) -> Expr<'arena> {
        // Skip traversing and rebuilding the term if it would make no change. Increases
        // sharing.
        if amount == EnvLen::new() {
            return *self;
        }

        let builder = ExprBuilder::new(scope);

        match self {
            Expr::Local(var) if *var >= min => Expr::Local(*var + amount),

            Expr::Error
            | Expr::Lit(_)
            | Expr::Prim(_)
            | Expr::Local(_)
            | Expr::Meta(_)
            | Expr::InsertedMeta(..) => *self,

            Expr::Let((def, body)) => builder.r#let(
                LetDef {
                    name: def.name,
                    r#type: def.r#type.shift_inner(scope, min, amount),
                    expr: def.expr.shift_inner(scope, min, amount),
                },
                body.shift_inner(scope, min.next(), amount),
            ),
            Expr::FunType(plicity, name, (domain, body)) => builder.fun_type(
                *plicity,
                *name,
                domain.shift_inner(scope, min, amount),
                body.shift_inner(scope, min.next(), amount),
            ),
            Expr::FunLit(plicity, name, (domain, body)) => builder.fun_lit(
                *plicity,
                *name,
                domain.shift_inner(scope, min, amount),
                body.shift_inner(scope, min.next(), amount),
            ),
            Expr::FunApp(plicity, (fun, arg)) => builder.fun_app(
                *plicity,
                fun.shift_inner(scope, min, amount),
                arg.shift_inner(scope, min, amount),
            ),
            Expr::RecordType(labels, types) => {
                let types = types.iter().map(|expr| {
                    let ret = expr.shift_inner(scope, min, amount);
                    min = min.next();
                    ret
                });
                let types = scope.to_scope_from_iter(types);
                Expr::RecordType(labels, types)
            }
            Expr::RecordLit(labels, exprs) => {
                let exprs = exprs
                    .iter()
                    .map(|expr| expr.shift_inner(scope, min, amount));
                let exprs = scope.to_scope_from_iter(exprs);
                Expr::RecordLit(labels, exprs)
            }
            Expr::RecordProj(expr, label) => {
                builder.record_proj(expr.shift_inner(scope, min, amount), *label)
            }
            Expr::Match((scrut, default), branches) => {
                let scrut = scrut.shift_inner(scope, min, amount);
                let default =
                    default.map(|(name, expr)| (name, expr.shift_inner(scope, min.next(), amount)));
                let branches = branches
                    .iter()
                    .map(|(lit, expr)| (*lit, expr.shift_inner(scope, min, amount)));
                Expr::Match(
                    scope.to_scope((scrut, default)),
                    scope.to_scope_from_iter(branches),
                )
            }
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct LetDef<'arena> {
    pub name: Option<Symbol>,
    pub r#type: Expr<'arena>,
    pub expr: Expr<'arena>,
}

impl<'arena> LetDef<'arena> {
    pub fn new(name: Option<Symbol>, r#type: Expr<'arena>, expr: Expr<'arena>) -> Self {
        Self { name, r#type, expr }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinderInfo {
    Def,
    Param,
}

impl<'arena> Expr<'arena> {
    pub fn is_error(&self) -> bool { matches!(self, Self::Error) }
}

pub type Type<'arena> = Value<'arena>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value<'arena> {
    Lit(Lit),
    Stuck(Head, Vec<Elim<'arena>>),
    FunType(Plicity, Option<Symbol>, &'arena Self, Closure<'arena>),
    FunLit(Plicity, Option<Symbol>, &'arena Self, Closure<'arena>),
    RecordType(&'arena [Symbol], Telescope<'arena>),
    RecordLit(&'arena [Symbol], &'arena [Self]),
}

impl<'arena> Value<'arena> {
    pub const ERROR: Self = Self::Stuck(Head::Error, Vec::new());

    pub const TYPE: Self = Self::prim(Prim::Type);
    pub const BOOL: Self = Self::prim(Prim::Bool);
    pub const INT: Self = Self::prim(Prim::Int);

    pub const fn prim(prim: Prim) -> Self { Self::Stuck(Head::Prim(prim), Vec::new()) }

    pub const fn local(level: Level) -> Self { Self::Stuck(Head::Local(level), Vec::new()) }

    pub const fn meta(level: Level) -> Self { Self::Stuck(Head::Meta(level), Vec::new()) }

    pub fn is_type(&self) -> bool {
        matches!(self, Value::Stuck(Head::Prim(Prim::Type), elims) if elims.is_empty())
    }

    pub const fn is_error(&self) -> bool { matches!(self, Self::Stuck(Head::Error, _)) }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Head {
    Error,
    Prim(Prim),
    Local(Level),
    Meta(Level),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Elim<'arena> {
    FunApp(Plicity, Value<'arena>),
    RecordProj(Symbol),
    Match(Cases<'arena, Lit>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Closure<'arena> {
    pub local_values: SharedEnv<Value<'arena>>,
    pub expr: &'arena Expr<'arena>,
}

impl<'arena> Closure<'arena> {
    pub fn new(local_values: SharedEnv<Value<'arena>>, expr: &'arena Expr<'arena>) -> Self {
        Self { local_values, expr }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Telescope<'arena> {
    pub local_values: SharedEnv<Value<'arena>>,
    pub exprs: &'arena [Expr<'arena>],
}

impl<'arena> Telescope<'arena> {
    pub fn new(local_values: SharedEnv<Value<'arena>>, exprs: &'arena [Expr<'arena>]) -> Self {
        Self {
            local_values,
            exprs,
        }
    }

    pub fn len(&self) -> usize { self.exprs.len() }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Cases<'arena, P> {
    pub local_values: SharedEnv<Value<'arena>>,
    pub pattern_cases: &'arena [(P, Expr<'arena>)],
    pub default_case: &'arena Option<(Option<Symbol>, Expr<'arena>)>,
}

impl<'arena, P> Cases<'arena, P> {
    pub fn new(
        local_values: SharedEnv<Value<'arena>>,
        pattern_cases: &'arena [(P, Expr<'arena>)],
        default_case: &'arena Option<(Option<Symbol>, Expr<'arena>)>,
    ) -> Self {
        Self {
            local_values,
            pattern_cases,
            default_case,
        }
    }
}

pub type PatternCase<'arena, P> = (P, Value<'arena>);

#[derive(Debug, Clone)]
pub enum SplitCases<'arena, P> {
    Case(PatternCase<'arena, P>, Cases<'arena, P>),
    Default(Option<Symbol>, Closure<'arena>),
    None,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Pat<'arena, Extra = ByteRange> {
    Error(Extra),
    Ignore(Extra),
    Ident(Extra, Symbol),
    Lit(Extra, Lit),
    RecordLit(Extra, &'arena [Symbol], &'arena [Self]),
}

impl<'arena, Extra> Pat<'arena, Extra> {
    pub fn range(&self) -> Extra
    where
        Extra: Clone,
    {
        match self {
            Pat::Error(range, ..)
            | Pat::Ignore(range, ..)
            | Pat::Ident(range, ..)
            | Pat::Lit(range, ..)
            | Pat::RecordLit(range, ..) => range.clone(),
        }
    }

    pub fn name(&self) -> Option<Symbol> {
        match self {
            Pat::Ident(_, name) => Some(*name),
            _ => None,
        }
    }

    pub fn is_err(&self) -> bool { matches!(self, Self::Error(..)) }

    /// Returns `true` if `self` is a "wildcard" pattern - ie always matches its
    /// scrutinee
    pub fn is_wildcard(&self) -> bool {
        matches!(self, Pat::Error(..) | Pat::Ignore(..) | Pat::Ident(..))
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Lit {
    Bool(bool),
    Int(u32),
}
impl Lit {
    pub fn num_inhabitants(&self) -> Option<u128> {
        match self {
            Self::Bool(_) => Some(1 << 1),
            Self::Int(_) => Some(1 << 32),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::mem::size_of;

    use super::*;

    #[test]
    fn expr_size() {
        assert_eq!(size_of::<Expr>(), 40);
    }

    #[test]
    fn pat_size() {
        assert_eq!(size_of::<Pat>(), 48);
    }

    #[test]
    fn value_size() {
        assert_eq!(size_of::<Value>(), 48);
    }

    #[test]
    fn lit_size() {
        assert_eq!(size_of::<Lit>(), 8);
    }
}

impl<'arena> Expr<'arena> {
    /// Iterate over the subexpressions of `self`
    pub fn subexprs<'a>(
        &'a self,
        env: EnvLen,
    ) -> impl InternalIterator<Item = (EnvLen, &'a Expr<'arena>)> {
        Subexprs { expr: self, env }
    }
}

struct Subexprs<'a, 'arena> {
    env: EnvLen,
    expr: &'a Expr<'arena>,
}

impl<'a, 'arena> InternalIterator for Subexprs<'a, 'arena> {
    type Item = (EnvLen, &'a Expr<'arena>);

    fn try_for_each<R, F>(mut self, mut f: F) -> ControlFlow<R>
    where
        F: FnMut(Self::Item) -> ControlFlow<R>,
    {
        self.helper(self.expr, &mut f)
    }
}

impl<'a, 'arena> Subexprs<'a, 'arena> {
    fn helper<R>(
        &mut self,
        expr: &'a Expr<'arena>,
        f: &mut impl FnMut((EnvLen, &'a Expr<'arena>)) -> ControlFlow<R>,
    ) -> ControlFlow<R> {
        f((self.env, expr))?;

        match expr {
            Expr::Error
            | Expr::Lit(_)
            | Expr::Prim(_)
            | Expr::Local(_)
            | Expr::Meta(_)
            | Expr::InsertedMeta(..) => {}
            Expr::Let((def, body)) => {
                self.helper(&def.r#type, f)?;
                self.helper(&def.expr, f)?;
                self.env.push();
                self.helper(body, f)?;
                self.env.pop();
            }
            Expr::FunType(.., (r#type, body)) | Expr::FunLit(.., (r#type, body)) => {
                self.helper(r#type, f)?;
                self.env.push();
                self.helper(body, f)?;
                self.env.pop();
            }
            Expr::FunApp(_, (fun, arg)) => {
                self.helper(fun, f)?;
                self.helper(arg, f)?;
            }
            Expr::RecordType(_, exprs) => {
                let len = self.env;
                for expr in exprs.iter() {
                    self.helper(expr, f)?;
                    self.env.push();
                }
                self.env.truncate(len);
            }
            Expr::RecordLit(_, exprs) => {
                for expr in exprs.iter() {
                    self.helper(expr, f)?;
                }
            }
            Expr::RecordProj(head, ..) => self.helper(head, f)?,
            Expr::Match((scrut, default), cases) => {
                self.helper(scrut, f)?;

                for (_, expr) in cases.iter() {
                    self.helper(expr, f)?;
                }

                if let Some((_, default)) = default {
                    self.env.push();
                    self.helper(default, f)?;
                    self.env.pop();
                }
            }
        }

        ControlFlow::Continue(())
    }
}

pub struct ExprBuilder<'arena> {
    scope: &'arena Scope<'arena>,
}

impl<'arena> ExprBuilder<'arena> {
    pub fn new(scope: &'arena Scope<'arena>) -> Self { Self { scope } }

    pub fn r#let(&self, def: LetDef<'arena>, body: Expr<'arena>) -> Expr<'arena> {
        Expr::Let(self.scope.to_scope((def, body)))
    }

    pub fn lets<I>(&self, defs: I, body: Expr<'arena>) -> Expr<'arena>
    where
        I: IntoIterator<Item = LetDef<'arena>>,
        I::IntoIter: DoubleEndedIterator,
    {
        defs.into_iter().rev().fold(body, |body, def| {
            Expr::Let(self.scope.to_scope((def, body)))
        })
    }

    pub fn fun_lit(
        &self,
        plicity: Plicity,
        name: Option<Symbol>,
        domain: Expr<'arena>,
        body: Expr<'arena>,
    ) -> Expr<'arena> {
        Expr::FunLit(plicity, name, self.scope.to_scope((domain, body)))
    }

    pub fn fun_type(
        &self,
        plicity: Plicity,
        name: Option<Symbol>,
        domain: Expr<'arena>,
        codomain: Expr<'arena>,
    ) -> Expr<'arena> {
        Expr::FunType(plicity, name, self.scope.to_scope((domain, codomain)))
    }

    pub fn fun_app(&self, plicity: Plicity, fun: Expr<'arena>, arg: Expr<'arena>) -> Expr<'arena> {
        Expr::FunApp(plicity, self.scope.to_scope((fun, arg)))
    }

    pub fn record_proj(&self, head: Expr<'arena>, label: Symbol) -> Expr<'arena> {
        Expr::RecordProj(self.scope.to_scope(head), label)
    }

    pub fn if_then_else(
        &self,
        cond: Expr<'arena>,
        then: Expr<'arena>,
        r#else: Expr<'arena>,
    ) -> Expr<'arena> {
        Expr::Match(
            self.scope.to_scope((cond, None)),
            self.scope
                .to_scope_from_iter([(Lit::Bool(false), r#else), (Lit::Bool(true), then)]),
        )
    }
}
