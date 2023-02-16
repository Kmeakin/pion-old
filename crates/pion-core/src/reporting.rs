use codespan_reporting::diagnostic::{Diagnostic, Label};
use pion_source::location::ByteRange;
use pion_surface::syntax::Symbol;

use crate::elab::unify::{RenameError, SpineError, UnifyError};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ElabError {
    UnboundName {
        range: ByteRange,
        name: Symbol,
    },
    UnexpectedArgument {
        fun_range: ByteRange,
        fun_type: String,
        arg_range: ByteRange,
    },
    Unification {
        range: ByteRange,
        found: String,
        expected: String,
        error: UnifyError,
    },
}

impl ElabError {
    pub fn to_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
        let primary_label = |range: &ByteRange| Label::primary(file_id, *range);
        let secondary_label = |range: &ByteRange| Label::secondary(file_id, *range);

        match self {
            Self::UnboundName { range, name } => Diagnostic::error()
                .with_message(format!("cannot find {name} in scope"))
                .with_labels(vec![primary_label(range)]),
            Self::UnexpectedArgument {
                fun_range,
                fun_type,
                arg_range,
            } => Diagnostic::error()
                .with_message("expression was applied to an unexpected argument")
                .with_labels(vec![
                    primary_label(arg_range).with_message("unexpected argument"),
                    secondary_label(fun_range)
                        .with_message(format!("expression of type {fun_type}")),
                ]),
            Self::Unification {
                range,
                found,
                expected,
                error,
            } => match error {
                UnifyError::Mismatch => Diagnostic::error()
                    .with_message("mismatched types")
                    .with_labels(vec![primary_label(range).with_message(format!(
                        "type mismatch, expected `{expected}`, found `{found}`"
                    ))]),
                UnifyError::Spine(error) => {
                    let message = match error {
                        SpineError::NonLinearSpine(_) => {
                            "variable appeared more than once in problem spine"
                        }
                        SpineError::NonLocalFunApp => {
                            "non-variable function application in problem spine"
                        }
                    };
                    Diagnostic::error()
                        .with_message(message)
                        .with_labels(vec![primary_label(range)])
                }
                UnifyError::Rename(error) => {
                    let message = match error {
                        RenameError::EscapingLocalVar(_) => "escaping local variable",
                        RenameError::InfiniteSolution => "infinite solution",
                    };
                    Diagnostic::error()
                        .with_message(message)
                        .with_labels(vec![primary_label(range)])
                }
            },
        }
    }
}
