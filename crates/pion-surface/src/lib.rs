#![warn(clippy::all, clippy::nursery, clippy::pedantic, unused_qualifications)]
#![allow(
    clippy::match_bool,
    clippy::missing_const_for_fn,
    clippy::module_name_repetitions,
    clippy::must_use_candidate,
    clippy::new_without_default,
    clippy::option_if_let_else,
    clippy::wildcard_imports,
    clippy::missing_errors_doc,
    clippy::missing_panics_doc
)]

pub mod pretty;
pub mod reporting;
pub mod syntax;
pub mod tokens;

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(
    #[allow(
        clippy::all,
        clippy::nursery,
        clippy::pedantic,
        dead_code,
        unused_qualifications
    )]
    grammar
);
