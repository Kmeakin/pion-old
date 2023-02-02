#![warn(clippy::all, clippy::nursery, unused_qualifications)]
#![allow(clippy::missing_const_for_fn)]

pub mod pretty;
pub mod syntax;
pub mod tokens;

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(
    #[allow(clippy::all, clippy::nursery, dead_code, unused_qualifications)]
    grammar
);
