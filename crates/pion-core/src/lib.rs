#![warn(clippy::all, clippy::nursery, clippy::pedantic, unused_qualifications)]
#![allow(
    clippy::match_bool,
    clippy::missing_const_for_fn,
    clippy::missing_errors_doc,
    clippy::missing_panics_doc,
    clippy::module_name_repetitions,
    clippy::must_use_candidate,
    clippy::new_without_default,
    clippy::option_if_let_else,
    clippy::too_many_lines,
    clippy::wildcard_imports
)]

pub mod distill;
pub mod elab;
pub mod prim;
pub mod semantics;
pub mod syntax;

mod env;
mod reporting;
