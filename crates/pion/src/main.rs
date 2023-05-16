#![warn(clippy::all, clippy::nursery, unused_qualifications)]

use clap::Parser;
use driver::Driver;
use pion_source::input::InputString;
use scoped_arena::Scope;

mod driver;

#[derive(Parser)]
#[clap(author, version, about)]
enum Opts {
    Parse { file: String },
    Elab { file: String },
    ElabModule { file: String },
    Eval { file: String },
}

fn read_file(file: &String) -> Result<InputString, String> {
    let input = match std::fs::read_to_string(file) {
        Ok(input) => input,
        Err(err) => return Err(format!("Cannot read `{file}`: {err}")),
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
            let file_id = driver.add_file(file, contents);
            let expr = driver.parse_expr(&scope, file_id);
            driver.emit_expr(&scope, &expr)
        }
        Opts::Elab { file } => {
            let contents = unwrap_or_exit(read_file(&file));
            let file_id = driver.add_file(file, contents);
            let expr = driver.parse_expr(&scope, file_id);

            let mut on_message = |message: pion_core::reporting::Message| {
                let diag = message.to_diagnostic(file_id);
                driver.emit_diagnostic(diag);
            };
            let mut elab_ctx = pion_core::elab::ElabCtx::new(&scope, &mut on_message);
            let (expr, r#type) = elab_ctx.elab_expr(expr);

            let mut distill_ctx = elab_ctx.distill_ctx();
            let expr = distill_ctx.ann_expr(&expr, &r#type);
            driver.emit_expr(&scope, &expr);
        }
        Opts::ElabModule { file } => {
            let contents = unwrap_or_exit(read_file(&file));
            let file_id = driver.add_file(file, contents);
            let module = driver.parse_module(&scope, file_id);

            let mut on_message = |message: pion_core::reporting::Message| {
                let diag = message.to_diagnostic(file_id);
                driver.emit_diagnostic(diag);
            };

            let module = pion_core::elab::elab_module(&scope, &module, &mut on_message);

            let mut local_names = Default::default();
            let meta_sources = Default::default();
            let mut distill_ctx =
                pion_core::distill::DistillCtx::new(&scope, &mut local_names, &meta_sources);
            let module = distill_ctx.module(&module);
            driver.emit_module(&scope, &module);
        }
        Opts::Eval { file } => {
            let contents = unwrap_or_exit(read_file(&file));
            let file_id = driver.add_file(file, contents);
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
