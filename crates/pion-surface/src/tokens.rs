use logos::{Lexer, Logos};
use pion_source::input::InputString;
use pion_source::location::{BytePos, ByteRange};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Logos)]
#[rustfmt::skip]
pub enum Token<'src> {
    #[error]
    #[regex(r"\s+", logos::skip)]
    #[regex(r"//[^\n]*", logos::skip)]
    Error,

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_-]*")] Ident(&'src str),

    #[regex(r"[0-9][0-9_]*")]                   DecInt(&'src str),
    #[regex(r"0(b|B)[0-9][0-9_]*")]             BinInt(&'src str),
    #[regex(r"0(x|X)[0-9a-fA-F][0-9a-fA-F_]*")] HexInt(&'src str),

    #[token("def")]   KwDef,
    #[token("false")] KwFalse,
    #[token("fun")]   KwFun,
    #[token("let")]   KwLet,
    #[token("true")]  KwTrue,

    #[token("_")]  Underscore,
    #[token("->")] ThinArrow,
    #[token(",")]  Comma,
    #[token(";")]  Semicolon,
    #[token(":")]  Colon,
    #[token("(")]  LParen,
    #[token(")")]  RParen,
    #[token("=")]  Eq,
    #[token("=>")] FatArrow,
}
impl<'src> Token<'src> {
    pub fn description(&self) -> &'static str {
        match self {
            Token::Error => "unknown token",
            Token::Ident(_) => "identifier",
            Token::DecInt(_) => "decimal integer",
            Token::BinInt(_) => "binary integer",
            Token::HexInt(_) => "hexadecimal integer",
            Token::KwDef => "keyword `def`",
            Token::KwFalse => "keyword `false`",
            Token::KwFun => "keyword `fun`",
            Token::KwLet => "keyword `let`",
            Token::KwTrue => "keyword `true`",
            Token::Underscore => "`_`",
            Token::ThinArrow => "`->`",
            Token::Comma => "`,`",
            Token::Semicolon => "`;`",
            Token::Colon => "`:`",
            Token::LParen => "`(`",
            Token::RParen => "`)`",
            Token::Eq => "`=`",
            Token::FatArrow => "`=>`",
        }
    }

    pub fn from_name(name: &str) -> Self {
        match name {
            "Ident" => Self::Ident(""),
            "DecInt" => Self::DecInt(""),
            "BinInt" => Self::BinInt(""),
            "HexInt" => Self::HexInt(""),
            "def" => Self::KwDef,
            "false" => Self::KwFalse,
            "fun" => Self::KwFun,
            "let" => Self::KwLet,
            "true" => Self::KwTrue,
            "_" => Self::Underscore,
            "->" => Self::ThinArrow,
            "," => Self::Comma,
            ";" => Self::Semicolon,
            ":" => Self::Colon,
            "(" => Self::LParen,
            ")" => Self::RParen,
            "=" => Self::Eq,
            "=>" => Self::FatArrow,
            _ => Self::Error,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    Unknown(ByteRange),
}

impl Error {
    pub fn range(&self) -> ByteRange {
        match self {
            Error::Unknown(range, ..) => *range,
        }
    }
}

pub fn tokens<'src>(
    input: InputString<'src>,
) -> impl Iterator<Item = Result<(BytePos, Token<'src>, BytePos), Error>> {
    Lexer::new(*input).spanned().map(|(token, range)| {
        let start = BytePos::truncate(range.start);
        let end = BytePos::truncate(range.end);
        match token {
            Token::Error => Err(Error::Unknown(ByteRange::new(start, end))),
            _ => Ok((start, token, end)),
        }
    })
}
