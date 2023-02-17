use pion_surface::syntax::{self as surface, Symbol};
use scoped_arena::Scope;

use crate::elab::MetaSource;
use crate::env::{EnvLen, Index, Level, UniqueEnv};
use crate::prim::Prim;
use crate::syntax::*;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Prec {
    Top,
    Let,
    Fun,
    App,
    Atom,
}

/// Distillation context.
pub struct DistillCtx<'arena, 'env> {
    scope: &'arena Scope<'arena>,
    local_names: &'env mut UniqueEnv<Option<Symbol>>,
    meta_sources: &'env UniqueEnv<MetaSource>,
}

impl<'arena, 'env> DistillCtx<'arena, 'env> {
    pub fn new(
        scope: &'arena Scope<'arena>,
        local_names: &'env mut UniqueEnv<Option<Symbol>>,
        meta_sources: &'env UniqueEnv<MetaSource>,
    ) -> Self {
        Self {
            scope,
            local_names,
            meta_sources,
        }
    }

    fn local_len(&mut self) -> EnvLen { self.local_names.len() }

    fn truncate_local(&mut self, len: EnvLen) { self.local_names.truncate(len); }

    fn push_local(&mut self, name: Option<Symbol>) { self.local_names.push(name) }

    fn pop_local(&mut self) { self.local_names.pop(); }

    fn param(&mut self, name: Option<Symbol>, r#type: &Expr<'_>) -> surface::Param<'arena, ()> {
        let param = surface::Param {
            pat: name_to_pat(name),
            r#type: Some(self.expr_prec(Prec::Top, r#type)),
        };
        self.push_local(name);
        param
    }

    fn let_def(&mut self, def: &LetDef<'_>) -> surface::LetDef<'arena, ()> {
        let name = def.name;
        let def = surface::LetDef {
            pat: name_to_pat(def.name),
            r#type: Some(self.expr_prec(Prec::Top, &def.r#type)),
            expr: (self.expr_prec(Prec::Let, &def.expr)),
        };
        self.push_local(name);
        def
    }

    pub fn expr(&mut self, expr: &Expr<'_>) -> surface::Expr<'arena, ()> {
        self.expr_prec(Prec::Top, expr)
    }

    fn paren(&self, wrap: bool, expr: surface::Expr<'arena, ()>) -> surface::Expr<'arena, ()> {
        if wrap {
            surface::Expr::Paren((), self.scope.to_scope(expr))
        } else {
            expr
        }
    }

    fn expr_prec(&mut self, prec: Prec, expr: &Expr<'_>) -> surface::Expr<'arena, ()> {
        match expr {
            Expr::Error => surface::Expr::Error(()),
            Expr::Local(var) => match self.local_names.get_index(*var) {
                Some(Some(name)) => surface::Expr::Ident((), *name),
                Some(None) => unreachable!("Referenced local variable without name"),
                None => panic!("Unbound local variable: {var:?}"),
            },
            Expr::Meta(var) => match self.meta_sources.get_level(*var) {
                Some(MetaSource::HoleExpr(_, name)) => surface::Expr::Hole((), *name),
                Some(_) => surface::Expr::Hole((), var.to_string().into()),
                None => panic!("Unbound meta variable: {var:?}"),
            },
            Expr::InsertedMeta(var, spine) => {
                let head = self.expr(&Expr::Meta(*var));
                let args = self.scope.to_scope_from_iter(
                    Level::iter()
                        .zip(spine.iter())
                        .filter_map(|(level, info)| match info {
                            BinderInfo::Def => None,
                            BinderInfo::Param => Some(level),
                        })
                        .map(|var| {
                            let var = self.local_len().level_to_index(var).unwrap();
                            self.expr_prec(Prec::Top, &Expr::Local(var))
                        }),
                );
                self.paren(
                    prec > Prec::App,
                    surface::Expr::FunApp((), self.scope.to_scope(head), args),
                )
            }
            Expr::Lit(literal) => surface::Expr::Lit((), lit(*literal)),
            Expr::Prim(primitive) => prim(*primitive),
            Expr::Let((def, body)) => {
                let def = self.let_def(def);
                let body = self.expr_prec(Prec::Let, body);
                self.pop_local();

                self.paren(
                    prec > Prec::Let,
                    surface::Expr::Let((), self.scope.to_scope((def, body))),
                )
            }
            Expr::FunType(..) => {
                let mut body = expr;
                let mut params = Vec::new();

                let initial_len = self.local_len();
                let body = loop {
                    match body {
                        // Use an explicit parameter if it is referenced in the body
                        Expr::FunType(name, (r#type, next_body))
                            if next_body.binds_local(Index::new()) =>
                        {
                            params.push(self.param(*name, r#type));
                            body = next_body;
                        }
                        // Use arrow sugar if the parameter is not referenced in the body type.
                        Expr::FunType(_, (r#type, body)) => {
                            let param = self.expr_prec(Prec::App, r#type);

                            self.push_local(None);
                            let body = self.expr_prec(Prec::Fun, body);
                            self.pop_local();

                            break surface::Expr::Arrow((), self.scope.to_scope((param, body)));
                        }
                        _ => break self.expr_prec(Prec::Fun, body),
                    }
                };
                self.truncate_local(initial_len);

                if params.is_empty() {
                    self.paren(prec > Prec::Fun, body)
                } else {
                    self.paren(
                        prec > Prec::Fun,
                        surface::Expr::FunType(
                            (),
                            self.scope.to_scope_from_iter(params),
                            self.scope.to_scope(body),
                        ),
                    )
                }
            }
            Expr::FunLit(..) => {
                let mut body = expr;
                let mut params = Vec::new();

                let initial_len = self.local_len();
                while let Expr::FunLit(name, (r#type, next_body)) = body {
                    params.push(self.param(*name, r#type));
                    body = next_body;
                }
                let body = self.expr_prec(Prec::Fun, body);
                self.truncate_local(initial_len);

                self.paren(
                    prec > Prec::Fun,
                    surface::Expr::FunLit(
                        (),
                        self.scope.to_scope_from_iter(params),
                        self.scope.to_scope(body),
                    ),
                )
            }
            Expr::FunApp(..) => {
                let mut args = Vec::new();
                let mut fun = expr;

                while let Expr::FunApp((next_fun, arg)) = fun {
                    fun = next_fun;
                    args.push(self.expr_prec(Prec::Atom, arg));
                }

                let fun = self.expr_prec(Prec::Atom, fun);

                self.paren(
                    prec > Prec::App,
                    surface::Expr::FunApp(
                        (),
                        self.scope.to_scope(fun),
                        self.scope.to_scope_from_iter(args.into_iter().rev()),
                    ),
                )
            }
        }
    }
}

fn lit(lit: Lit) -> surface::Lit<()> {
    match lit {
        Lit::Bool(value) => surface::Lit::Bool((), value),
        Lit::Int(value) => surface::Lit::Int((), value),
    }
}

fn prim<'arena>(prim: Prim) -> surface::Expr<'arena, ()> {
    let name = prim.name();
    surface::Expr::Ident((), Symbol::from(name))
}

fn name_to_pat<'arena>(name: Option<Symbol>) -> surface::Pat<'arena, ()> {
    match name {
        Some(name) => surface::Pat::Ident((), name),
        None => surface::Pat::Underscore(()),
    }
}
