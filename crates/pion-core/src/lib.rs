#![warn(clippy::all, unused_qualifications)]
#![allow(clippy::new_without_default)]

pub mod distill;
pub mod elab;
pub mod prim;
pub mod semantics;
pub mod syntax;

mod env;
mod reporting;
