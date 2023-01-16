#![warn(clippy::all, clippy::nursery, unused_qualifications)]

use std::path::{Path, PathBuf};

use clap::Parser;
use pion_source::input::InputString;
use scoped_arena::Scope;

#[derive(Parser)]
#[clap(author, version, about)]
enum Opts {
    Parse { file: PathBuf },
    Elab { file: PathBuf },
}

fn read_file(file: &Path) -> InputString {
    let input = match std::fs::read_to_string(file) {
        Ok(input) => input,
        Err(err) => {
            eprintln!("Cannot read `{}`: {err}", file.display());
            std::process::exit(-1);
        }
    };

    match pion_source::input::InputString::new(input) {
        Ok(input) => input,
        Err(err) => {
            eprintln!("{err}");
            std::process::exit(-1);
        }
    }
}

fn main() {
    match Opts::parse() {
        Opts::Parse { file } => {
            let input = read_file(&file);
            let scope = Scope::new();
            let mut errors = Vec::new();
            let expr = pion_surface::syntax::Expr::parse(&scope, &mut errors, input);
            for error in errors {
                eprintln!("error: {error:?}");
            }
            println!("{expr:?}")
        }
        Opts::Elab { file } => {
            let input = read_file(&file);
            let scope = Scope::new();
            let mut errors = Vec::new();
            let expr = pion_surface::syntax::Expr::parse(&scope, &mut errors, input);
            for error in errors {
                eprintln!("error: {error:?}");
            }

            let mut elab_ctx = pion_core::elab::ElabCtx::new(&scope);
            let (expr, r#type) = elab_ctx.synth(&expr);
            for error in elab_ctx.errors {
                eprintln!("error: {error:?}");
            }
            #[allow(clippy::uninlined_format_args)]
            {
                println!("expr: {:?}", expr);
                println!("type: {:?}", r#type);
            }
        }
    }
}
