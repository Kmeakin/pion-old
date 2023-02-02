use pion_surface::syntax::Symbol;

use crate::env::{Index, Level, UniqueEnv};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Expr<'arena> {
    Error,
    Lit(Lit),
    Prim(Prim),
    Local(Index),
    Meta(Level),
    InsertedMeta(Level, &'arena [BinderInfo]),
    Let(Option<Symbol>, &'arena (Self, Self, Self)),
    FunType(Option<Symbol>, &'arena (Self, Self)),
    FunLit(Option<Symbol>, &'arena (Self, Self)),
    FunApp(&'arena (Self, Self)),
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
}

impl<'arena> Value<'arena> {
    pub const ERROR: Self = Self::Stuck(Head::Error, Vec::new());

    pub const TYPE: Self = Self::prim(Prim::Type);
    pub const BOOL: Self = Self::prim(Prim::BoolType);
    pub const INT: Self = Self::prim(Prim::IntType);

    pub const fn prim(prim: Prim) -> Self { Self::Stuck(Head::Prim(prim), Vec::new()) }

    pub const fn local(level: Level) -> Self { Self::Stuck(Head::Local(level), Vec::new()) }

    pub const fn meta(level: Level) -> Self { Self::Stuck(Head::Meta(level), Vec::new()) }

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

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Prim {
    Type,
    BoolType,
    IntType,
}

#[cfg(test)]
mod tests {
    use std::mem::size_of;

    use super::*;

    #[test]
    fn expr_size() {
        assert_eq!(size_of::<Expr>(), 24);
    }

    #[test]
    fn value_size() {
        assert_eq!(size_of::<Value>(), 56);
    }

    #[test]
    fn lit_size() {
        assert_eq!(size_of::<Lit>(), 8);
    }

    #[test]
    fn prim_size() {
        assert_eq!(size_of::<Prim>(), 1);
    }
}
