use std::path::Path;

use codespan_reporting::diagnostic::Diagnostic;
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::termcolor::ColorChoice;
use pion_source::input::InputString;
use scoped_arena::Scope;

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

    pub fn emit_expr<'scope, Extra>(
        &self,
        scope: &'scope Scope<'scope>,
        expr: &pion_surface::syntax::Expr<'_, Extra>,
    ) {
        let pretty_ctx = pion_surface::pretty::PrettyCtx::new(scope);
        let doc = pretty_ctx.expr(expr).into_doc();
        self.emit_doc(doc)
    }

    pub fn emit_module<'scope, Extra>(
        &self,
        scope: &'scope Scope<'scope>,
        module: &pion_surface::syntax::Module<'_, Extra>,
    ) {
        let pretty_ctx = pion_surface::pretty::PrettyCtx::new(scope);
        let doc = pretty_ctx.module(module).into_doc();
        self.emit_doc(doc)
    }

    pub fn emit_doc(&self, doc: pretty::RefDoc) {
        println!("{}", doc.pretty(self.output_width));
    }

    pub fn emit_diagnostic(&self, diagnostic: Diagnostic<usize>) {
        let color = if atty::is(atty::Stream::Stderr) {
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

    pub fn add_file(&mut self, path: &Path, contents: InputString) -> usize {
        self.files
            .add(path.to_string_lossy().into_owned(), contents)
    }

    pub fn parse_expr<'scope>(
        &self,
        scope: &'scope Scope<'scope>,
        file_id: usize,
    ) -> pion_surface::syntax::Expr<'scope> {
        let mut on_message = |message: pion_surface::reporting::Message| {
            self.emit_diagnostic(message.to_diagnostic(file_id))
        };
        let input = self.files.get(file_id).unwrap();
        let expr = pion_surface::syntax::Expr::parse(scope, &mut on_message, input.source());
        expr
    }

    pub fn parse_module<'scope>(
        &self,
        scope: &'scope Scope<'scope>,
        file_id: usize,
    ) -> pion_surface::syntax::Module<'scope> {
        let mut on_message = |message: pion_surface::reporting::Message| {
            self.emit_diagnostic(message.to_diagnostic(file_id))
        };
        let input = self.files.get(file_id).unwrap();
        let module = pion_surface::syntax::Module::parse(scope, &mut on_message, input.source());
        module
    }
}
