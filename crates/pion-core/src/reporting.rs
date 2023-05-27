use codespan_reporting::diagnostic::{Diagnostic, Label};
use pion_source::location::ByteRange;
use pion_surface::syntax::{Plicity, Symbol};

use crate::elab::unify::{RenameError, SpineError, UnifyError};
use crate::elab::MetaSource;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Message {
    UnboundName {
        range: ByteRange,
        name: Symbol,
    },
    DuplicateLocalName {
        name: Symbol,
        first_range: ByteRange,
        duplicate_range: ByteRange,
    },
    FunAppPlicity {
        fun_range: ByteRange,
        fun_type: String,
        fun_plicity: Plicity,
        arg_range: ByteRange,
        arg_plicity: Plicity,
    },
    FunAppNotFun {
        fun_range: ByteRange,
        fun_type: String,
        num_args: usize,
        args_range: ByteRange,
    },
    FunAppTooManyArgs {
        fun_range: ByteRange,
        fun_type: String,
        expected_arity: usize,
        actual_arity: usize,
        extra_args_range: ByteRange,
    },
    RecordFieldDuplicate {
        name: &'static str,
        label: Symbol,
        first_range: ByteRange,
        duplicate_range: ByteRange,
    },
    RecordProjNotRecord {
        head_range: ByteRange,
        head_type: String,
        label_range: ByteRange,
        label: Symbol,
    },
    RecordProjNotFound {
        head_range: ByteRange,
        head_type: String,
        label_range: ByteRange,
        label: Symbol,
    },
    UnsolvedMeta {
        source: MetaSource,
    },
    Unification {
        range: ByteRange,
        found: String,
        expected: String,
        error: UnifyError,
    },
    UnreachablePat {
        range: ByteRange,
    },
    InexhaustiveMatch {
        range: ByteRange,
        scrut_range: ByteRange,
    },
}

impl Message {
    pub fn to_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
        let primary_label = |range: &ByteRange| Label::primary(file_id, *range);
        let secondary_label = |range: &ByteRange| Label::secondary(file_id, *range);

        match self {
            Self::UnboundName { range, name } => Diagnostic::error()
                .with_message(format!("cannot find `{name}` in scope"))
                .with_labels(vec![primary_label(range)]),
            Self::DuplicateLocalName {
                name,
                first_range,
                duplicate_range,
            } => Diagnostic::error()
                .with_message(format!("duplicate definition of local name `{name}`"))
                .with_labels(vec![
                    secondary_label(first_range).with_message("first definition"),
                    primary_label(duplicate_range).with_message("duplicate definition"),
                ]),
            Self::FunAppPlicity {
                fun_range,
                fun_type,
                fun_plicity,
                arg_range,
                arg_plicity,
            } => Diagnostic::error()
                .with_message(format!(
                    "tried to apply {arg_plicity} argument where {fun_plicity} argument was \
                     expected"
                ))
                .with_labels(vec![
                    primary_label(arg_range).with_message(format!("{arg_plicity} argument")),
                    secondary_label(fun_range)
                        .with_message(format!("{fun_plicity} function of type `{fun_type}`")),
                ]),
            Self::FunAppNotFun {
                fun_range,
                fun_type,
                num_args,
                args_range,
            } => Diagnostic::error()
                .with_message(pluralize(
                    *num_args,
                    "tried to apply argument to non-function expression",
                    "tried to apply arguments to non-function expression",
                ))
                .with_labels(vec![
                    primary_label(fun_range)
                        .with_message(format!("expression of type `{fun_type}`")),
                    secondary_label(args_range).with_message(pluralize(
                        *num_args,
                        "argument",
                        "arguments",
                    )),
                ]),
            Self::FunAppTooManyArgs {
                fun_range,
                fun_type,
                expected_arity,
                actual_arity,
                extra_args_range,
            } => Diagnostic::error()
                .with_message("tried to apply too many arguments to function")
                .with_labels(vec![
                    primary_label(fun_range)
                        .with_message(format!("expression of type `{fun_type}`")),
                    secondary_label(extra_args_range).with_message(pluralize(
                        actual_arity - expected_arity,
                        "extra argument",
                        "extra arguments",
                    )),
                ])
                .with_notes(vec![format!(
                    "help: function expects {expected_arity} {}, but recieved {actual_arity} \
                     arguments",
                    pluralize(*expected_arity, "argument", "arguments"),
                )]),
            Self::RecordFieldDuplicate {
                name,
                label,
                first_range,
                duplicate_range,
            } => Diagnostic::error()
                .with_message(format!("duplicate field `{label}` in {name}"))
                .with_labels(vec![
                    secondary_label(first_range).with_message("first occurence"),
                    primary_label(duplicate_range).with_message("duplicate occurence"),
                ]),
            Self::RecordProjNotRecord {
                head_range,
                head_type,
                label_range,
                label,
            } => Diagnostic::error()
                .with_message(format!(
                    "tried to access field `{label}` of non-record expression"
                ))
                .with_labels(vec![
                    primary_label(head_range)
                        .with_message(format!("expression of type `{head_type}`")),
                    secondary_label(label_range).with_message("field access"),
                ]),
            Self::RecordProjNotFound {
                head_range,
                head_type,
                label_range,
                label,
            } => Diagnostic::error()
                .with_message(format!("no field named `{label}` in record"))
                .with_labels(vec![
                    primary_label(head_range)
                        .with_message(format!("expression of type `{head_type}`")),
                    secondary_label(label_range).with_message("unknown field"),
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
                        SpineError::RecordProj(_) => "record projection found in problem spine",
                        SpineError::Match => "pattern match found in problem spine",
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
            Self::UnsolvedMeta { source } => {
                let (range, name) = match source {
                    MetaSource::PlaceholderExpr(range) => (range, "placeholder expression"),
                    MetaSource::HoleExpr(range, _) => (range, "hole expression"),
                    MetaSource::PatType(range) => (range, "pattern type"),
                    MetaSource::ImplicitArg(range, _) => (range, "implicit argument"),
                    MetaSource::MatchType(range) => (range, "type of match expression"),

                    // should be impossible
                    MetaSource::HoleType(range, _) => (range, "hole type"),
                    MetaSource::PlaceholderType(range) => (range, "placeholder type"),
                };
                Diagnostic::error()
                    .with_message(format!("unable to infer {name}"))
                    .with_labels(vec![primary_label(range)])
            }
            Self::UnreachablePat { range } => Diagnostic::warning()
                .with_message("unreachable pattern")
                .with_labels(vec![primary_label(range)]),
            Self::InexhaustiveMatch { range, scrut_range } => Diagnostic::error()
                .with_message("inexhaustive match expression")
                .with_labels(vec![
                    primary_label(scrut_range).with_message("patterns not covered"),
                    secondary_label(range).with_message("in match expression"),
                ]),
        }
    }
}

fn pluralize<T>(amount: usize, single: T, plural: T) -> T {
    if amount == 1 {
        single
    } else {
        plural
    }
}
