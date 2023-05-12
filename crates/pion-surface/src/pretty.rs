use pretty::{Doc, DocAllocator, DocPtr, RefDoc};
use scoped_arena::Scope;

use crate::syntax::*;

pub struct PrettyCtx<'arena> {
    scope: &'arena Scope<'arena>,
}

const INDENT: isize = 4;

type DocBuilder<'arena> = pretty::DocBuilder<'arena, PrettyCtx<'arena>>;

impl<'arena> PrettyCtx<'arena> {
    pub fn new(scope: &'arena Scope<'arena>) -> Self { Self { scope } }

    #[allow(clippy::too_many_lines)]
    pub fn expr<Extra>(&'arena self, expr: &Expr<'_, Extra>) -> DocBuilder<'arena> {
        match expr {
            Expr::Error(_) => self.text("#error"),
            Expr::Paren(_, expr) => self.expr(expr).parens().group(),
            Expr::Ann(_, (expr, r#type)) => self.expr(expr).append(" : ").append(self.expr(r#type)),
            Expr::Lit(_, lit) => self.lit(lit),
            Expr::Placeholder(_) => self.text("_"),
            Expr::Hole(_, name) => self.text("?").append(name.as_str()),
            Expr::Ident(_, name) => self.text(name.as_str()),
            Expr::Let(_, (def, body)) => self
                .text("let ")
                .append(self.pat(&def.pat))
                .append(
                    def.r#type
                        .as_ref()
                        .map(|r#type| self.text(" : ").append(self.expr(r#type))),
                )
                .append(" =")
                .append(self.line().append(self.expr(&def.expr)).nest(INDENT))
                .append(";")
                .group()
                .append(self.line())
                .append(self.expr(body)),
            Expr::Arrow(_, plicity, (r#type, body)) => self
                .plicity(*plicity)
                .append(self.expr(r#type))
                .append(" ->")
                .append(self.softline())
                .append(self.expr(body)),
            Expr::FunType(_, params, body) => self
                .text("fun ")
                .append(
                    self.intersperse(params.iter().map(|param| self.param(param)), self.line())
                        .group(),
                )
                .append(" ->")
                .append(self.softline())
                .append(self.expr(body)),
            Expr::FunLit(_, params, body) => self
                .text("fun ")
                .append(
                    self.intersperse(params.iter().map(|param| self.param(param)), self.line())
                        .group(),
                )
                .append(" =>")
                .append(self.softline())
                .append(self.expr(body)),
            Expr::FunApp(_, fun, args) => self
                .expr(fun)
                .append(self.concat(args.iter().map(|arg| self.space().append(self.arg(arg))))),
            Expr::RecordType(_, fields) => self.sequence(
                true,
                self.text("{"),
                fields.iter().map(|field| {
                    self.ident(field.label.1)
                        .append(" : ")
                        .append(self.expr(&field.r#type))
                }),
                self.text(","),
                self.text("}"),
            ),
            Expr::RecordLit(_, fields) => self.sequence(
                true,
                self.text("{"),
                fields.iter().map(|field| {
                    self.ident(field.label.1)
                        .append(" = ")
                        .append(self.expr(&field.expr))
                }),
                self.text(","),
                self.text("}"),
            ),
            Expr::TupleLit(_, exprs) => self.tuple(exprs, |expr| self.expr(expr)),
            Expr::RecordProj(_, head, labels) => self.expr(head).append(self.concat(
                (labels.iter()).map(|(_, label)| self.text(".").append(self.ident(*label))),
            )),
            Expr::Match(_, scrut, cases) => {
                let cases = cases.iter().map(|case| {
                    let pat = self.pat(&case.pat);
                    let expr = self.expr(&case.expr);
                    self.hardline()
                        .append(pat)
                        .append(" => ")
                        .append(expr)
                        .append(",")
                });
                self.text("match ")
                    .append(self.expr(scrut))
                    .append(" {")
                    .append(self.concat(cases).nest(INDENT))
                    .append(self.line_())
                    .append("}")
                    .group()
            }
            Expr::If(_, (cond, then, r#else)) => {
                let cond = self.expr(cond);
                let then = self.block(then);
                let r#else = self.block(r#else);
                self.text("if ")
                    .append(cond)
                    .append(" then ")
                    .append(then)
                    .append(" else ")
                    .append(r#else)
            }
            Expr::Block(_, block) => self.block(block),
        }
    }

    fn block<Extra>(&'arena self, block: &Block<'_, Extra>) -> DocBuilder<'arena> {
        let stmts = block
            .stmts
            .iter()
            .map(|stmt| self.stmt(stmt))
            .chain(block.expr.map(|expr| self.expr(expr)));
        let docs = self.intersperse(stmts, self.hardline());

        self.concat([
            self.text("{"),
            self.concat([self.line(), docs]).nest(INDENT),
            self.line(),
            self.text("}"),
        ])
        .group()
    }

    fn stmt<Extra>(&'arena self, stmt: &Stmt<'_, Extra>) -> DocBuilder<'arena> {
        match stmt {
            Stmt::Let(def) => self
                .text("let ")
                .append(self.pat(&def.pat))
                .append(
                    def.r#type
                        .as_ref()
                        .map(|r#type| self.text(" : ").append(self.expr(r#type))),
                )
                .append(" = ")
                .append(self.expr(&def.expr))
                .append(";")
                .group(),
            Stmt::Expr(expr) => self.expr(expr).append(";"),
            Stmt::Semi => self.text(";"),
        }
    }

    /// Pretty prints a delimited sequence of documents with a trailing
    /// separator if it is formatted over multiple lines.
    /// If `space` is true, extra spaces are added before and after the
    /// delimiters
    pub fn sequence(
        &'arena self,
        space: bool,
        start_delim: DocBuilder<'arena>,
        docs: impl Iterator<Item = DocBuilder<'arena>>,
        separator: DocBuilder<'arena>,
        end_delim: DocBuilder<'arena>,
    ) -> DocBuilder<'arena> {
        // if docs.len() == 0 {
        //     return self.concat([start_delim, end_delim]);
        // }

        let docs = self.intersperse(docs, self.concat([separator.clone(), self.line()]));
        self.concat([
            start_delim,
            self.concat([
                if space { self.line() } else { self.line_() },
                docs,
                DocBuilder::flat_alt(separator, self.nil()),
            ])
            .nest(INDENT),
            if space { self.line() } else { self.line_() },
            end_delim,
        ])
        .group()
    }

    fn param<Extra>(&'arena self, param: &Param<'_, Extra>) -> DocBuilder<'arena> {
        match param {
            Param {
                plicity,
                pat,
                r#type: None,
            } => self.plicity(*plicity).append(self.pat(pat)),
            Param {
                plicity,
                pat,
                r#type: Some(r#type),
            } => self
                .plicity(*plicity)
                .append(self.pat(pat))
                .append(" : ")
                .append(self.expr(r#type))
                .parens()
                .group(),
        }
    }

    fn arg<Extra>(&'arena self, arg: &Arg<'_, Extra>) -> DocBuilder<'arena> {
        self.plicity(arg.plicity).append(self.expr(&arg.expr))
    }

    fn plicity(&'arena self, plicity: Plicity) -> DocBuilder<'arena> {
        match plicity {
            Plicity::Explicit => self.nil(),
            Plicity::Implicit => self.text("@"),
        }
    }

    fn pat<Extra>(&'arena self, pat: &Pat<'_, Extra>) -> DocBuilder<'arena> {
        match pat {
            Pat::Paren(.., pat) => self.pat(pat).parens(),
            Pat::Lit(.., lit) => self.lit(lit),
            Pat::Ident(.., name) => self.text(name.as_str()),
            Pat::Underscore(_) => self.text("_"),
            Pat::RecordLit(.., fields) => self.sequence(
                true,
                self.text("{"),
                fields.iter().map(|field| {
                    self.ident(field.label.1)
                        .append(" = ")
                        .append(self.pat(&field.pat))
                }),
                self.text(","),
                self.text("}"),
            ),
            Pat::TupleLit(.., pats) => self.tuple(pats, |pat| self.pat(pat)),
        }
    }

    fn tuple<T>(
        &'arena self,
        elems: &[T],
        pretty: impl Fn(&T) -> DocBuilder<'arena>,
    ) -> DocBuilder<'arena> {
        if elems.len() == 1 {
            self.text("(").append(pretty(&elems[0]).append(",)"))
        } else {
            self.sequence(
                false,
                self.text("("),
                elems.iter().map(pretty),
                self.text(","),
                self.text(")"),
            )
        }
    }

    fn ident(&'arena self, name: Symbol) -> DocBuilder<'arena> { self.text(name.to_owned()) }

    fn lit<Extra>(&'arena self, lit: &Lit<Extra>) -> DocBuilder<'arena> {
        match lit {
            Lit::Bool(_, true) => self.text("true"),
            Lit::Bool(_, false) => self.text("false"),
            Lit::Int(_, value) => self.text(print_decimal_integer(*value)),
        }
    }
}

impl<'arena, A: 'arena> DocAllocator<'arena, A> for PrettyCtx<'arena> {
    type Doc = RefDoc<'arena, A>;

    fn alloc(&'arena self, doc: Doc<'arena, Self::Doc, A>) -> Self::Doc {
        RefDoc(self.scope.to_scope(doc))
    }

    fn alloc_column_fn(
        &'arena self,
        f: impl Fn(usize) -> Self::Doc + 'arena,
    ) -> <Self::Doc as DocPtr<'arena, A>>::ColumnFn {
        self.scope.to_scope(f)
    }

    fn alloc_width_fn(
        &'arena self,
        f: impl Fn(isize) -> Self::Doc + 'arena,
    ) -> <Self::Doc as DocPtr<'arena, A>>::WidthFn {
        self.scope.to_scope(f)
    }
}
