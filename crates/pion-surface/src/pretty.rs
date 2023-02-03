use pretty::{Doc, DocAllocator, DocPtr, RefDoc};
use scoped_arena::Scope;

use crate::syntax::*;

pub struct PrettyCtx<'arena> {
    scope: &'arena Scope<'arena>,
}

type DocBuilder<'arena> = pretty::DocBuilder<'arena, PrettyCtx<'arena>>;

impl<'arena> PrettyCtx<'arena> {
    pub fn new(scope: &'arena Scope<'arena>) -> Self { Self { scope } }

    pub fn expr<Extra>(&'arena self, expr: &Expr<'_, Extra>) -> DocBuilder<'arena> {
        match expr {
            Expr::Error(_) => self.text("#error"),
            Expr::Paren(_, expr) => self.expr(expr).parens(),
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
                .append(" = ")
                .append(self.expr(&def.expr))
                .append("; ")
                .append(self.expr(body)),
            Expr::Arrow(_, (r#type, body)) => {
                self.expr(r#type).append(" -> ").append(self.expr(body))
            }
            Expr::FunType(_, params, body) => self
                .text("fun ")
                .append(
                    self.intersperse(params.iter().map(|param| self.param(param)), self.space()),
                )
                .append(" -> ")
                .append(self.expr(body)),
            Expr::FunLit(_, params, body) => self
                .text("fun ")
                .append(
                    self.intersperse(params.iter().map(|param| self.param(param)), self.space()),
                )
                .append(" => ")
                .append(self.expr(body)),
            Expr::FunApp(_, fun, args) => self
                .expr(fun)
                .append(self.concat(args.iter().map(|arg| self.space().append(self.expr(arg))))),
        }
    }

    fn param<Extra>(&'arena self, param: &Param<'_, Extra>) -> DocBuilder<'arena> {
        match param {
            Param { pat, r#type: None } => self.pat(pat),
            Param {
                pat,
                r#type: Some(r#type),
            } => self
                .pat(pat)
                .append(" : ")
                .append(self.expr(r#type))
                .parens(),
        }
    }

    fn pat<Extra>(&'arena self, pat: &Pat<'_, Extra>) -> DocBuilder<'arena> {
        match pat {
            Pat::Paren(.., pat) => self.pat(pat).parens(),
            Pat::Lit(.., lit) => self.lit(lit),
            Pat::Ident(.., name) => self.text(name.as_str()),
            Pat::Underscore(_) => self.text("_"),
        }
    }

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
