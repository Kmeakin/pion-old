use logos::{Lexer, Logos};
use pion_source::input::InputString;
use pion_source::location::{BytePos, ByteRange};

use crate::reporting::TokenError;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Logos)]
#[rustfmt::skip]
#[logos(skip r"\s+")]
#[logos(skip r"//[^\n]*")]
pub enum Token<'src> {
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_-]*")]
    Ident(&'src str),

    #[regex(r"\?[a-zA-Z_][a-zA-Z0-9_-]*", |lex| &lex.slice()[1..])]
    Hole(&'src str),

    #[regex(r"[0-9][0-9_]*")]                   DecInt(&'src str),
    #[regex(r"0(b|B)[0-9][0-9_]*")]             BinInt(&'src str),
    #[regex(r"0(x|X)[0-9a-fA-F][0-9a-fA-F_]*")] HexInt(&'src str),

    #[token("def")]   KwDef,
    #[token("false")] KwFalse,
    #[token("fun")]   KwFun,
    #[token("let")]   KwLet,
    #[token("match")] KwMatch,
    #[token("true")]  KwTrue,

    #[token("_")]  Underscore,
    #[token("->")] ThinArrow,
    #[token(",")]  Comma,
    #[token(";")]  Semicolon,
    #[token(":")]  Colon,
    #[token(".")]  Dot,
    #[token("(")]  LParen,
    #[token(")")]  RParen,
    #[token("{")]  LCurly,
    #[token("}")]  RCurly,
    #[token("@")]  At,
    #[token("=")]  Eq,
    #[token("=>")] FatArrow,
}

impl<'src> Token<'src> {
    pub const fn description(&self) -> &'static str {
        match self {
            Token::Ident(_) => "identifier",
            Token::Hole(_) => "hole",
            Token::DecInt(_) => "decimal integer",
            Token::BinInt(_) => "binary integer",
            Token::HexInt(_) => "hexadecimal integer",
            Token::KwDef => "`def`",
            Token::KwFalse => "`false`",
            Token::KwFun => "`fun`",
            Token::KwLet => "`let`",
            Token::KwMatch => "`match`",
            Token::KwTrue => "`true`",
            Token::Underscore => "`_`",
            Token::ThinArrow => "`->`",
            Token::Comma => "`,`",
            Token::Semicolon => "`;`",
            Token::Colon => "`:`",
            Token::Dot => "`.`",
            Token::LParen => "`(`",
            Token::RParen => "`)`",
            Token::LCurly => "`{`",
            Token::RCurly => "`}`",
            Token::At => "`@`",
            Token::Eq => "`=`",
            Token::FatArrow => "`=>`",
        }
    }
}

pub fn tokens(
    input: &InputString,
) -> impl Iterator<Item = Result<(BytePos, Token, BytePos), TokenError>> {
    Lexer::new(input.as_str()).spanned().map(|(token, range)| {
        let start = BytePos::truncate(range.start);
        let end = BytePos::truncate(range.end);
        match token {
            Ok(token) => Ok((start, token, end)),
            Err(_) => Err(TokenError::Unknown(ByteRange::new(start, end))),
        }
    })
}
