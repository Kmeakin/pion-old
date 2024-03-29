use std::io::IsTerminal;

use bumpalo::Bump;
use codespan_reporting::diagnostic::Diagnostic;
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::termcolor::ColorChoice;
use pion_source::input::InputString;

pub struct Driver {
    files: SimpleFiles<String, InputString>,
    codespan_config: codespan_reporting::term::Config,
    output_width: usize,
}

impl Driver {
    pub fn new() -> Self {
        Self {
            files: SimpleFiles::new(),
            codespan_config: codespan_reporting::term::Config::default(),
            output_width: 80,
        }
    }

    pub fn emit_expr<'arena, Extra>(
        &self,
        arena: &'arena Bump,
        expr: &pion_surface::syntax::Expr<'arena, Extra>,
    ) {
        let pretty_ctx = pion_surface::pretty::PrettyCtx::new(arena);
        let doc = pretty_ctx.expr(expr).into_doc();
        self.emit_doc(doc)
    }

    pub fn emit_module<'arena, Extra>(
        &self,
        arena: &'arena Bump,
        module: &pion_surface::syntax::Module<'arena, Extra>,
    ) {
        let pretty_ctx = pion_surface::pretty::PrettyCtx::new(arena);
        let doc = pretty_ctx.module(module).into_doc();
        self.emit_doc(doc)
    }

    pub fn emit_doc(&self, doc: pretty::RefDoc) {
        println!("{}", doc.pretty(self.output_width));
    }

    pub fn emit_diagnostic(&self, diagnostic: Diagnostic<usize>) {
        let color = if std::io::stderr().is_terminal() {
            ColorChoice::Auto
        } else {
            ColorChoice::Never
        };
        let mut writer = codespan_reporting::term::termcolor::StandardStream::stderr(color);
        codespan_reporting::term::emit(
            &mut writer,
            &self.codespan_config,
            &self.files,
            &diagnostic,
        )
        .expect("Cannot emit diagnostic");
    }

    pub fn add_file(&mut self, path: String, contents: InputString) -> usize {
        self.files.add(path, contents)
    }

    pub fn parse_expr<'arena>(
        &self,
        arena: &'arena Bump,
        file_id: usize,
    ) -> pion_surface::syntax::Expr<'arena> {
        let mut on_message = |message: pion_surface::reporting::Message| {
            self.emit_diagnostic(message.to_diagnostic(file_id))
        };
        let input = self.files.get(file_id).unwrap();
        let expr = pion_surface::syntax::Expr::parse(arena, &mut on_message, input.source());
        expr
    }

    pub fn parse_module<'arena>(
        &self,
        arena: &'arena Bump,
        file_id: usize,
    ) -> pion_surface::syntax::Module<'arena> {
        let mut on_message = |message: pion_surface::reporting::Message| {
            self.emit_diagnostic(message.to_diagnostic(file_id))
        };
        let input = self.files.get(file_id).unwrap();
        let module = pion_surface::syntax::Module::parse(arena, &mut on_message, input.source());
        module
    }
}
