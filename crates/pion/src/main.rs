#![warn(clippy::all, clippy::nursery, unused_qualifications)]

use std::path::{Path, PathBuf};

use clap::Parser;
use driver::Driver;
use pion_source::input::InputString;
use scoped_arena::Scope;

mod driver;

#[derive(Parser)]
#[clap(author, version, about)]
enum Opts {
    Parse { file: PathBuf },
    Elab { file: PathBuf },
    Eval { file: PathBuf },
}

fn read_file(file: &Path) -> Result<InputString, String> {
    let input = match std::fs::read_to_string(file) {
        Ok(input) => input,
        Err(err) => return Err(format!("Cannot read `{}`: {err}", file.display())),
    };

    match pion_source::input::InputString::new(input) {
        Ok(input) => Ok(input),
        Err(err) => Err(err.to_string()),
    }
}

fn unwrap_or_exit<T, E: std::fmt::Display>(value: Result<T, E>) -> T {
    match value {
        Ok(value) => value,
        Err(error) => {
            eprintln!("{error}");
            std::process::exit(1)
        }
    }
}

fn main() {
    let scope = Scope::new();
    let mut driver = Driver::new();
    match Opts::parse() {
        Opts::Parse { file } => {
            let contents = unwrap_or_exit(read_file(&file));
            let file_id = driver.add_file(&file, contents);
            let expr = driver.parse_expr(&scope, file_id);
            driver.emit_expr(&scope, &expr)
        }
        Opts::Elab { file } => {
            let contents = unwrap_or_exit(read_file(&file));
            let file_id = driver.add_file(&file, contents);
            let expr = driver.parse_expr(&scope, file_id);

            let mut on_message = |message: pion_core::reporting::Message| {
                let diag = message.to_diagnostic(file_id);
                driver.emit_diagnostic(diag);
            };
            let mut elab_ctx = pion_core::elab::ElabCtx::new(&scope, &mut on_message);
            let (expr, r#type) = elab_ctx.elab_expr(expr);

            let mut distill_ctx = elab_ctx.distill_ctx();
            let expr = distill_ctx.expr(&expr);
            let r#type = distill_ctx.expr(&r#type);

            driver.emit_expr(
                &scope,
                &pion_surface::syntax::Expr::Ann((), scope.to_scope((expr, r#type))),
            );
        }
        Opts::Eval { file } => {
            let contents = unwrap_or_exit(read_file(&file));
            let file_id = driver.add_file(&file, contents);
            let expr = driver.parse_expr(&scope, file_id);

            let mut on_message = |message: pion_core::reporting::Message| {
                let diag = message.to_diagnostic(file_id);
                driver.emit_diagnostic(diag);
            };
            let mut elab_ctx = pion_core::elab::ElabCtx::new(&scope, &mut on_message);
            let (expr, _) = elab_ctx.elab_expr(expr);

            let expr_value = elab_ctx.eval_env().eval(&expr);
            let expr = elab_ctx.quote_env().quote(&expr_value);
            let expr = elab_ctx.distill_ctx().expr(&expr);
            driver.emit_expr(&scope, &expr);
        }
    };
}
