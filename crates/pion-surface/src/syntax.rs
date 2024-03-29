use core::fmt;

use bumpalo::Bump;
pub use pion_source::input::InputString;
use pion_source::location::{BytePos, ByteRange};

use crate::reporting::{Message, TokenError};
use crate::tokens::{self, Token};

pub type Symbol = ustr::Ustr;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Module<'arena, Extra = ByteRange> {
    pub items: &'arena [Item<'arena, Extra>],
}

impl<'arena, Extra> Module<'arena, Extra> {
    pub fn new(items: &'arena [Item<'arena, Extra>]) -> Self { Self { items } }
}

impl<'arena, 'message> Module<'arena, ByteRange> {
    pub fn parse(
        arena: &'arena Bump,
        on_message: &'message mut dyn FnMut(Message),
        input: &InputString,
    ) -> Self {
        let tokens = tokens::tokens(input);
        match crate::grammar::ModuleParser::new().parse(
            Builder::new(arena),
            arena,
            on_message,
            tokens,
        ) {
            Ok(module) => module,
            Err(err) => {
                let err = Message::from_lalrpop(err);
                on_message(err);
                Module::new(&[])
            }
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Item<'arena, Extra = ByteRange> {
    Def(Def<'arena, Extra>),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Def<'arena, Extra = ByteRange> {
    pub extra: Extra,
    pub name: Symbol,
    pub r#type: Option<Expr<'arena, Extra>>,
    pub expr: Expr<'arena, Extra>,
}

impl<'arena, Extra> Def<'arena, Extra> {
    pub fn new(
        extra: Extra,
        name: Symbol,
        r#type: Option<Expr<'arena, Extra>>,
        expr: Expr<'arena, Extra>,
    ) -> Self {
        Self {
            extra,
            name,
            r#type,
            expr,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Expr<'arena, Extra = ByteRange> {
    Error(Extra),
    Paren(Extra, &'arena Self),
    Ann(Extra, &'arena (Self, Self)),
    Lit(Extra, Lit<Extra>),
    Placeholder(Extra),
    Hole(Extra, Symbol),
    Ident(Extra, Symbol),
    Let(Extra, &'arena (LetDef<'arena, Extra>, Self)),
    Arrow(Extra, Plicity, &'arena (Self, Self)),
    FunType(Extra, &'arena [Param<'arena, Extra>], &'arena Self),
    FunLit(Extra, &'arena [Param<'arena, Extra>], &'arena Self),
    FunApp(Extra, &'arena Self, &'arena [Arg<'arena, Extra>]),
    RecordType(Extra, &'arena [TypeField<'arena, Extra>]),
    RecordLit(Extra, &'arena [ExprField<'arena, Extra>]),
    TupleLit(Extra, &'arena [Self]),
    RecordProj(Extra, &'arena Self, &'arena [(Extra, Symbol)]),
    Match(Extra, &'arena Self, &'arena [MatchCase<'arena, Extra>]),
    If(Extra, &'arena (Self, Self, Self)),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct LetDef<'arena, Extra = ByteRange> {
    pub pat: Pat<'arena, Extra>,
    pub r#type: Option<Expr<'arena, Extra>>,
    pub expr: Expr<'arena, Extra>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct TypeField<'arena, Extra = ByteRange> {
    pub label: (Extra, Symbol),
    pub r#type: Expr<'arena, Extra>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ExprField<'arena, Extra = ByteRange> {
    pub label: (Extra, Symbol),
    pub expr: Expr<'arena, Extra>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct PatField<'arena, Extra = ByteRange> {
    pub label: (Extra, Symbol),
    pub pat: Pat<'arena, Extra>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct MatchCase<'arena, Extra = ByteRange> {
    pub pat: Pat<'arena, Extra>,
    pub expr: Expr<'arena, Extra>,
}

impl<'arena, Extra> Expr<'arena, Extra> {
    pub fn range(&self) -> Extra
    where
        Extra: Clone,
    {
        match self {
            Expr::Error(range, ..)
            | Expr::Paren(range, ..)
            | Expr::Ann(range, ..)
            | Expr::Lit(range, ..)
            | Expr::Placeholder(range, ..)
            | Expr::Hole(range, ..)
            | Expr::Ident(range, ..)
            | Expr::Let(range, ..)
            | Expr::Arrow(range, ..)
            | Expr::FunType(range, ..)
            | Expr::FunLit(range, ..)
            | Expr::FunApp(range, ..)
            | Expr::RecordType(range, ..)
            | Expr::RecordLit(range, ..)
            | Expr::TupleLit(range, ..)
            | Expr::RecordProj(range, ..)
            | Expr::Match(range, ..)
            | Expr::If(range, _) => range.clone(),
        }
    }
}

impl<'arena, 'message> Expr<'arena, ByteRange> {
    pub fn parse(
        arena: &'arena Bump,
        on_message: &'message mut dyn FnMut(Message),
        input: &InputString,
    ) -> Self {
        let tokens = tokens::tokens(input);
        match crate::grammar::ExprParser::new().parse(
            Builder::new(arena),
            arena,
            on_message,
            tokens,
        ) {
            Ok(expr) => expr,
            Err(err) => {
                let err = Message::from_lalrpop(err);
                let range = err.range();
                on_message(err);
                Expr::Error(range)
            }
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Param<'arena, Extra = ByteRange> {
    pub plicity: Plicity,
    pub pat: Pat<'arena, Extra>,
    pub r#type: Option<Expr<'arena, Extra>>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Arg<'arena, Extra = ByteRange> {
    pub extra: Extra,
    pub plicity: Plicity,
    pub expr: Expr<'arena, Extra>,
}

impl<'arena, Extra> Arg<'arena, Extra>
where
    Extra: Clone,
{
    pub fn range(&self) -> Extra { self.extra.clone() }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Plicity {
    Explicit,
    Implicit,
}

impl fmt::Display for Plicity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Explicit => f.write_str("explicit"),
            Self::Implicit => f.write_str("implicit"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Pat<'arena, Extra = ByteRange> {
    Paren(Extra, &'arena Self),
    Underscore(Extra),
    Ident(Extra, Symbol),
    Lit(Extra, Lit<Extra>),
    RecordLit(Extra, &'arena [PatField<'arena, Extra>]),
    TupleLit(Extra, &'arena [Self]),
}

impl<'arena, Extra> Pat<'arena, Extra> {
    pub fn range(&self) -> Extra
    where
        Extra: Clone,
    {
        match self {
            Pat::Paren(range, ..)
            | Pat::Lit(range, ..)
            | Pat::Ident(range, ..)
            | Pat::Underscore(range, ..)
            | Pat::RecordLit(range, ..)
            | Pat::TupleLit(range, ..) => range.clone(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Lit<Extra = ByteRange> {
    Bool(Extra, bool),
    Int(Extra, u32),
}

impl<Extra> Lit<Extra> {
    pub fn range(&self) -> Extra
    where
        Extra: Clone,
    {
        match self {
            Self::Bool(range, ..) | Self::Int(range, ..) => range.clone(),
        }
    }
}

pub fn parse_decimal_integer(input: &str) -> Result<u32, lexical::Error> {
    const FORMAT: u128 = lexical::NumberFormatBuilder::new()
        .digit_separator(std::num::NonZeroU8::new(b'_'))
        .consecutive_digit_separator(true)
        .internal_digit_separator(true)
        .trailing_digit_separator(true)
        .radix(10)
        .build();
    lexical::parse_with_options::<_, _, FORMAT>(input, &lexical::ParseIntegerOptions::new())
}

pub fn parse_binary_integer(input: &str) -> Result<u32, lexical::Error> {
    const FORMAT: u128 = lexical::NumberFormatBuilder::new()
        .digit_separator(std::num::NonZeroU8::new(b'_'))
        .consecutive_digit_separator(true)
        .internal_digit_separator(true)
        .trailing_digit_separator(true)
        .radix(2)
        .build();
    lexical::parse_with_options::<_, _, FORMAT>(input, &lexical::ParseIntegerOptions::new())
}

pub fn parse_hexadecimal_integer(input: &str) -> Result<u32, lexical::Error> {
    const FORMAT: u128 = lexical::NumberFormatBuilder::new()
        .digit_separator(std::num::NonZeroU8::new(b'_'))
        .consecutive_digit_separator(true)
        .internal_digit_separator(true)
        .trailing_digit_separator(true)
        .radix(2)
        .build();
    lexical::parse_with_options::<_, _, FORMAT>(input, &lexical::ParseIntegerOptions::new())
}

pub fn print_decimal_integer(input: u32) -> String {
    const FORMAT: u128 = lexical::NumberFormatBuilder::new().build();
    lexical::to_string_with_options::<_, FORMAT>(input, &lexical::WriteIntegerOptions::new())
}

pub type LalrpopParseError<'src> = lalrpop_util::ParseError<BytePos, Token<'src>, TokenError>;
pub type LalrpopErrorRecovery<'src> = lalrpop_util::ErrorRecovery<BytePos, Token<'src>, TokenError>;

impl Message {
    pub const fn range(&self) -> ByteRange {
        match self {
            Self::Lexer(err) => err.range(),
            Self::IntLit(range, ..)
            | Self::InvalidToken(range, ..)
            | Self::UnrecognizedEof { range, .. }
            | Self::UnrecognizedToken { range, .. }
            | Self::ExtraToken { range, .. } => *range,
        }
    }

    pub fn from_lalrpop(err: LalrpopParseError) -> Self {
        match err {
            LalrpopParseError::InvalidToken { location } => {
                Self::InvalidToken(ByteRange::new(location, location))
            }
            LalrpopParseError::UnrecognizedEof { location, expected } => {
                let range = ByteRange::new(location, location);
                Self::UnrecognizedEof {
                    range,
                    expected_tokens: expected.into_boxed_slice(),
                }
            }
            LalrpopParseError::UnrecognizedToken { token, expected } => {
                let (start, token, end) = token;
                let range = ByteRange::new(start, end);
                let found_token = token.description();
                Self::UnrecognizedToken {
                    range,
                    found_token,
                    expected_tokens: expected.into_boxed_slice(),
                }
            }
            LalrpopParseError::ExtraToken { token } => {
                let (start, token, end) = token;
                let range = ByteRange::new(start, end);
                let found_token = token.description();
                Self::ExtraToken { range, found_token }
            }
            LalrpopParseError::User { error } => Self::Lexer(error),
        }
    }
}

#[derive(Copy, Clone)]
pub struct Builder<'arena> {
    arena: &'arena Bump,
}

impl<'arena> Builder<'arena> {
    pub fn new(arena: &'arena Bump) -> Self { Self { arena } }

    pub fn def<Extra>(
        &self,
        range: impl Into<Extra>,
        name: Symbol,
        r#type: impl Into<Option<Expr<'arena, Extra>>>,
        expr: Expr<'arena, Extra>,
    ) -> Def<'arena, Extra> {
        Def {
            extra: range.into(),
            name,
            r#type: r#type.into(),
            expr,
        }
    }

    pub fn paren<Extra>(
        &self,
        range: impl Into<Extra>,
        expr: Expr<'arena, Extra>,
    ) -> Expr<'arena, Extra> {
        Expr::Paren(range.into(), self.arena.alloc(expr))
    }

    pub fn ann<Extra>(
        &self,
        range: impl Into<Extra>,
        expr: Expr<'arena, Extra>,
        r#type: Expr<'arena, Extra>,
    ) -> Expr<'arena, Extra> {
        Expr::Ann(range.into(), self.arena.alloc((expr, r#type)))
    }

    pub fn r#let<Extra>(
        &self,
        range: impl Into<Extra>,
        def: LetDef<'arena, Extra>,
        body: Expr<'arena, Extra>,
    ) -> Expr<'arena, Extra> {
        Expr::Let(range.into(), self.arena.alloc((def, body)))
    }

    pub fn arrow<Extra>(
        &self,
        range: impl Into<Extra>,
        plicity: Plicity,
        domain: Expr<'arena, Extra>,
        codomain: Expr<'arena, Extra>,
    ) -> Expr<'arena, Extra> {
        Expr::Arrow(range.into(), plicity, self.arena.alloc((domain, codomain)))
    }

    pub fn fun_type<Extra>(
        &self,
        range: impl Into<Extra>,
        params: &'arena [Param<'arena, Extra>],
        codomain: Expr<'arena, Extra>,
    ) -> Expr<'arena, Extra> {
        Expr::FunType(range.into(), params, self.arena.alloc(codomain))
    }

    pub fn fun_lit<Extra>(
        &self,
        range: impl Into<Extra>,
        params: &'arena [Param<'arena, Extra>],
        body: Expr<'arena, Extra>,
    ) -> Expr<'arena, Extra> {
        Expr::FunLit(range.into(), params, self.arena.alloc(body))
    }

    pub fn fun_app<Extra>(
        &self,
        range: impl Into<Extra>,
        fun: Expr<'arena, Extra>,
        args: &'arena [Arg<'arena, Extra>],
    ) -> Expr<'arena, Extra> {
        Expr::FunApp(range.into(), self.arena.alloc(fun), args)
    }

    pub fn record_proj<Extra>(
        &self,
        range: impl Into<Extra>,
        head: Expr<'arena, Extra>,
        labels: &'arena [(Extra, Symbol)],
    ) -> Expr<'arena, Extra> {
        Expr::RecordProj(range.into(), self.arena.alloc(head), labels)
    }

    pub fn if_then_else<Extra>(
        &self,
        range: impl Into<Extra>,
        cond: Expr<'arena, Extra>,
        then: Expr<'arena, Extra>,
        r#else: Expr<'arena, Extra>,
    ) -> Expr<'arena, Extra> {
        Expr::If(range.into(), self.arena.alloc((cond, then, r#else)))
    }
}

#[cfg(test)]
mod tests {
    use std::mem::size_of;

    use super::*;

    #[test]
    fn expr_size() {
        assert_eq!(size_of::<Expr<()>>(), 32);
        assert_eq!(size_of::<Expr<ByteRange>>(), 40);
    }

    #[test]
    fn pat_size() {
        assert_eq!(size_of::<Pat<()>>(), 24);
        assert_eq!(size_of::<Pat<ByteRange>>(), 32);
    }

    #[test]
    fn lit_size() {
        assert_eq!(size_of::<Lit<()>>(), 8);
        assert_eq!(size_of::<Lit<ByteRange>>(), 16);
    }
}
