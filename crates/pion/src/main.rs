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
    Eval { file: PathBuf },
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
            let pretty_ctx = pion_surface::pretty::PrettyCtx::new(&scope);
            let doc = pretty_ctx.expr(&expr).into_doc();
            println!("{}", doc.pretty(80))
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
            let r#type = elab_ctx.quote_env().quote(&r#type);
            for error in &elab_ctx.errors {
                eprintln!("error: {error:?}");
            }

            let mut distill_ctx = elab_ctx.distill_ctx();

            let expr = distill_ctx.expr(&expr);
            let r#type = distill_ctx.expr(&r#type);
            let expr = pion_surface::syntax::Expr::Ann((), scope.to_scope((expr, r#type)));

            let pretty_ctx = pion_surface::pretty::PrettyCtx::new(&scope);
            let expr = pretty_ctx.expr(&expr).into_doc();

            println!("{}", expr.pretty(80))
        }
        Opts::Eval { file } => {
            let input = read_file(&file);
            let scope = Scope::new();
            let mut errors = Vec::new();
            let expr = pion_surface::syntax::Expr::parse(&scope, &mut errors, input);
            for error in errors {
                eprintln!("error: {error:?}");
            }

            let mut elab_ctx = pion_core::elab::ElabCtx::new(&scope);
            let (expr, _) = elab_ctx.synth(&expr);
            for error in &elab_ctx.errors {
                eprintln!("error: {error:?}");
            }

            let value = elab_ctx.eval_env().eval(&expr);
            let expr = elab_ctx.quote_env().quote(&value);

            let mut distill_ctx = elab_ctx.distill_ctx();

            let expr = distill_ctx.expr(&expr);

            let pretty_ctx = pion_surface::pretty::PrettyCtx::new(&scope);

            let expr = pretty_ctx.expr(&expr).into_doc();

            println!("{}", expr.pretty(80))
        }
    }
}
