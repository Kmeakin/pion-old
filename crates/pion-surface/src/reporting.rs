use codespan_reporting::diagnostic::{Diagnostic, Label};
use pion_source::location::ByteRange;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenError {
    Unknown(ByteRange),
}

impl TokenError {
    pub const fn range(&self) -> ByteRange {
        match self {
            Self::Unknown(range, ..) => *range,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SyntaxError {
    Lexer(TokenError),
    IntLit(ByteRange, lexical::Error),
    InvalidToken(ByteRange),
    UnrecognizedEOF {
        range: ByteRange,
        expected_tokens: Box<[String]>,
    },
    UnrecognizedToken {
        range: ByteRange,
        found_token: &'static str,
        expected_tokens: Box<[String]>,
    },
    ExtraToken {
        range: ByteRange,
        found_token: &'static str,
    },
}

impl TokenError {
    pub fn to_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
        let primary_label = |range: &ByteRange| Label::primary(file_id, *range);

        match self {
            Self::Unknown(range) => Diagnostic::error()
                .with_message("unexpected character")
                .with_labels(vec![primary_label(range)]),
        }
    }
}

impl SyntaxError {
    pub fn to_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
        let primary_label = |range: &ByteRange| Label::primary(file_id, *range);

        match self {
            Self::Lexer(error) => error.to_diagnostic(file_id),
            Self::IntLit(range, error) => Diagnostic::error()
                .with_message("invalid integer literal")
                .with_labels(vec![primary_label(range).with_message(error.to_string())]),
            Self::InvalidToken(range) => Diagnostic::error()
                .with_message("invalid token")
                .with_labels(vec![primary_label(range)]),
            Self::UnrecognizedEOF {
                range,
                expected_tokens,
            } => Diagnostic::error()
                .with_message("unexpected error of file")
                .with_labels(vec![
                    primary_label(range).with_message("unexpected end of file")
                ])
                .with_notes(
                    format_expected(expected_tokens).map_or(Vec::new(), |message| vec![message]),
                ),
            Self::UnrecognizedToken {
                range,
                found_token,
                expected_tokens: expected,
            } => Diagnostic::error()
                .with_message(format!("unexpected token {found_token}"))
                .with_labels(vec![primary_label(range).with_message("unexpected token")])
                .with_notes(format_expected(expected).map_or(Vec::new(), |message| vec![message])),
            Self::ExtraToken { range, found_token } => Diagnostic::error()
                .with_message(format!("extra token {found_token}"))
                .with_labels(vec![primary_label(range).with_message("extra token")]),
        }
    }
}

fn format_expected(expected: &[String]) -> Option<String> {
    match expected {
        [] => None,
        [first] => Some(format!("help: expected {first}")),
        [first, rest @ .., last] => {
            let mut out = format!("help: expected one of {first}");
            for expected in rest {
                out.push_str(&format!(", {expected}"));
            }
            out.push_str(&format!(" or {last}"));
            Some(out)
        }
    }
}
