use pion_surface::syntax::Symbol;

use crate::env::{Index, Level, UniqueEnv};
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
    FunType(Option<Symbol>, &'arena (Self, Self)),
    FunLit(Option<Symbol>, &'arena (Self, Self)),
    FunApp(&'arena (Self, Self)),
    RecordType(&'arena [Symbol], &'arena [Self]),
    RecordLit(&'arena [Symbol], &'arena [Self]),
    RecordProj(&'arena Self, Symbol),
}

impl<'arena> Expr<'arena> {
    pub fn binds_local(&self, var: Index) -> bool {
        match self {
            Expr::Error | Expr::Lit(_) | Expr::Prim(_) | Expr::Meta(_) | Expr::InsertedMeta(..) => {
                false
            }
            Expr::Local(v) => *v == var,
            Expr::Let((def, body)) => {
                def.r#type.binds_local(var)
                    || def.expr.binds_local(var)
                    || body.binds_local(var.next())
            }
            Expr::FunType(_, (param, body)) | Expr::FunLit(_, (param, body)) => {
                param.binds_local(var) || body.binds_local(var.next())
            }
            Expr::FunApp((fun, arg)) => fun.binds_local(var) || arg.binds_local(var),
            Expr::RecordType(_, types) => Index::iter_from(var)
                .zip(types.iter())
                .any(|(var, expr)| expr.binds_local(var)),
            Expr::RecordLit(_, exprs) => exprs.iter().any(|expr| expr.binds_local(var)),
            Expr::RecordProj(expr, _) => expr.binds_local(var),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct LetDef<'arena> {
    pub name: Option<Symbol>,
    pub r#type: Expr<'arena>,
    pub expr: Expr<'arena>,
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
    FunType(Option<Symbol>, &'arena Self, Closure<'arena>),
    FunLit(Option<Symbol>, &'arena Self, Closure<'arena>),
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
    FunApp(Value<'arena>),
    RecordProj(Symbol),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Closure<'arena> {
    pub local_values: UniqueEnv<Value<'arena>>,
    pub expr: &'arena Expr<'arena>,
}

impl<'arena> Closure<'arena> {
    pub fn new(local_values: UniqueEnv<Value<'arena>>, expr: &'arena Expr<'arena>) -> Self {
        Self { local_values, expr }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Telescope<'arena> {
    pub local_values: UniqueEnv<Value<'arena>>,
    pub exprs: &'arena [Expr<'arena>],
}

impl<'arena> Telescope<'arena> {
    pub fn new(local_values: UniqueEnv<Value<'arena>>, exprs: &'arena [Expr<'arena>]) -> Self {
        Self {
            local_values,
            exprs,
        }
    }

    pub fn len(&self) -> usize { self.exprs.len() }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Pat<'arena> {
    Error(&'arena ()),
    Lit(Lit),
    Binder(Option<Symbol>),
}

impl<'arena> Pat<'arena> {
    pub fn name(&self) -> Option<Symbol> {
        match self {
            Pat::Binder(name) => *name,
            _ => None,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Lit {
    Bool(bool),
    Int(u32),
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
    fn value_size() {
        assert_eq!(size_of::<Value>(), 64);
    }

    #[test]
    fn lit_size() {
        assert_eq!(size_of::<Lit>(), 8);
    }
}
