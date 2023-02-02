use pion_source::input::InputString;
use pion_source::location::{BytePos, ByteRange};
use scoped_arena::Scope;

use crate::tokens::{self, Token};

pub type Symbol = ustr::Ustr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module<'arena, Extra = ByteRange> {
    pub items: &'arena [Item<'arena, Extra>],
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Item<'arena, Extra = ByteRange> {
    Def(Def<'arena, Extra>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Def<'arena, Extra = ByteRange> {
    pub extra: Extra,
    pub name: Symbol,
    pub r#type: Option<&'arena Expr<'arena, Extra>>,
    pub expr: Expr<'arena, Extra>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Expr<'arena, Extra = ByteRange> {
    Error(Extra),
    Paren(Extra, &'arena Self),
    Lit(Extra, Lit),
    Ident(Extra, Symbol),
    Let(
        Extra,
        &'arena (Pat<'arena, Extra>, Option<Self>, Self, Self),
    ),
    Arrow(Extra, &'arena (Self, Self)),
    FunType(Extra, &'arena [Param<'arena, Extra>], &'arena Self),
    FunLit(Extra, &'arena [Param<'arena, Extra>], &'arena Self),
    FunApp(Extra, &'arena Self, &'arena [Self]),
}

impl<'arena, Extra> Expr<'arena, Extra> {
    pub fn range(&self) -> Extra
    where
        Extra: Clone,
    {
        match self {
            Expr::Error(range, ..)
            | Expr::Paren(range, ..)
            | Expr::Lit(range, ..)
            | Expr::Ident(range, ..)
            | Expr::Let(range, ..)
            | Expr::Arrow(range, ..)
            | Expr::FunType(range, ..)
            | Expr::FunLit(range, ..)
            | Expr::FunApp(range, ..) => range.clone(),
        }
    }
}

impl<'arena> Expr<'arena, ByteRange> {
    pub fn parse(
        scope: &'arena Scope<'arena>,
        errors: &mut Vec<Error>,
        input: InputString,
    ) -> Self {
        let tokens = tokens::tokens(&input);
        match crate::grammar::ExprParser::new().parse(scope, errors, tokens) {
            Ok(expr) => expr,
            Err(err) => {
                let err = Error::from_lalrpop(err);
                let range = err.range();
                errors.push(err);
                Expr::Error(range)
            }
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Param<'arena, Extra = ByteRange> {
    pub pat: Pat<'arena, Extra>,
    pub r#type: Option<Expr<'arena, Extra>>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Pat<'arena, Extra = ByteRange> {
    Paren(Extra, &'arena Self),
    Lit(Extra, Lit),
    Ident(Extra, Symbol),
    Underscore(Extra),
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
            | Pat::Underscore(range, ..) => range.clone(),
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

pub type LalrpopParseError<'src> = lalrpop_util::ParseError<BytePos, Token<'src>, tokens::Error>;
pub type LalrpopErrorRecovery<'src> =
    lalrpop_util::ErrorRecovery<BytePos, Token<'src>, tokens::Error>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    Lexer(tokens::Error),
    IntLit(ByteRange, lexical::Error),
    InvalidToken(ByteRange),
    UnrecognizedEOF {
        range: ByteRange,
        expected_tokens: Box<[&'static str]>,
    },
    UnrecognizedToken {
        range: ByteRange,
        found_token: &'static str,
        expected_tokens: Box<[&'static str]>,
    },
    ExtraToken {
        range: ByteRange,
        found_token: &'static str,
    },
}

impl Error {
    pub const fn range(&self) -> ByteRange {
        match self {
            Self::Lexer(err) => err.range(),
            Self::IntLit(range, ..)
            | Self::InvalidToken(range, ..)
            | Self::UnrecognizedEOF { range, .. }
            | Self::UnrecognizedToken { range, .. }
            | Self::ExtraToken { range, .. } => *range,
        }
    }

    pub fn from_lalrpop(err: LalrpopParseError) -> Self {
        match err {
            LalrpopParseError::InvalidToken { location } => {
                Self::InvalidToken(ByteRange::new(location, location))
            }
            LalrpopParseError::UnrecognizedEOF { location, expected } => {
                let range = ByteRange::new(location, location);
                let expected_tokens = expected
                    .into_iter()
                    .map(|name| Token::from_name(&name).description())
                    .collect();
                Self::UnrecognizedEOF {
                    range,
                    expected_tokens,
                }
            }
            LalrpopParseError::UnrecognizedToken { token, expected } => {
                let (start, token, end) = token;
                let range = ByteRange::new(start, end);
                let found_token = token.description();
                let expected_tokens = expected
                    .into_iter()
                    .map(|name| Token::from_name(&name).description())
                    .collect();
                Self::UnrecognizedToken {
                    range,
                    found_token,
                    expected_tokens,
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
        assert_eq!(size_of::<Pat<()>>(), 16);
        assert_eq!(size_of::<Pat<ByteRange>>(), 24);
    }

    #[test]
    fn lit_size() {
        assert_eq!(size_of::<Lit<()>>(), 8);
        assert_eq!(size_of::<Lit<ByteRange>>(), 16);
    }
}
