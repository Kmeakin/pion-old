use pion_surface::syntax::{self as surface, Arg, ExprField, Plicity, Symbol, TypeField};
use scoped_arena::Scope;

use crate::elab::MetaSource;
use crate::env::{EnvLen, Level, UniqueEnv};
use crate::prim::Prim;
use crate::syntax::*;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Prec {
    Top,
    Let,
    Fun,
    App,
    Proj,
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

    fn builder(&self) -> surface::Builder<'arena> { surface::Builder::new(self.scope) }

    fn local_len(&mut self) -> EnvLen { self.local_names.len() }

    fn truncate_local(&mut self, len: EnvLen) { self.local_names.truncate(len); }

    fn push_local(&mut self, name: Option<Symbol>) { self.local_names.push(name) }

    fn pop_local(&mut self) { self.local_names.pop(); }

    fn is_bound(&self, name: Symbol) -> bool {
        self.local_names
            .iter()
            .any(|local_name| *local_name == Some(name))
    }

    /// Generate a fresh name that does not appear in `self.local_names`
    fn gen_fresh_name(&mut self) -> Symbol {
        fn to_str(x: u32) -> String {
            let base = x / 26;
            let letter = x % 26;
            let letter = (letter as u8 + b'a') as char;
            if base == 0 {
                format!("{letter}")
            } else {
                format!("{letter}{base}")
            }
        }

        let mut counter = 0;
        loop {
            let name = to_str(counter).into();
            match self.is_bound(name) {
                true => {}
                false => return name,
            }
            counter += 1;
        }
    }

    /// Replace `name` with a fresh name if it is `_` and occurs in `body`
    fn freshen_name(&mut self, name: Option<Symbol>, body: &Expr<'_>) -> Option<Symbol> {
        match name {
            Some(name) => Some(name),
            None => match body.binds_local(EnvLen::default()) {
                false => None,
                true => Some(self.gen_fresh_name()),
            },
        }
    }

    pub fn module(&mut self, module: &Module<'_>) -> surface::Module<'arena, ()> {
        surface::Module {
            items: (self.scope).to_scope_from_iter(module.items.iter().map(|item| self.item(item))),
        }
    }

    fn item(&mut self, item: &Item<'_>) -> surface::Item<'arena, ()> {
        match item {
            Item::Def(def) => surface::Item::Def(self.def(def)),
        }
    }

    fn def(&mut self, def: &Def<'_>) -> surface::Def<'arena, ()> {
        let Def { name, r#type, expr } = def;
        let r#type = self.expr(r#type);
        let expr = self.expr(expr);
        surface::Def {
            extra: (),
            name: *name,
            r#type: Some(r#type),
            expr,
        }
    }

    pub fn ann_expr(&mut self, expr: &Expr<'_>, r#type: &Expr<'_>) -> surface::Expr<'arena, ()> {
        let expr = self.expr_prec(Prec::Let, expr);
        let r#type = self.expr_prec(Prec::Let, r#type);
        self.builder().ann((), expr, r#type)
    }

    pub fn expr(&mut self, expr: &Expr<'_>) -> surface::Expr<'arena, ()> {
        self.expr_prec(Prec::Top, expr)
    }

    fn paren(&self, wrap: bool, expr: surface::Expr<'arena, ()>) -> surface::Expr<'arena, ()> {
        if wrap {
            self.builder().paren((), expr)
        } else {
            expr
        }
    }

    fn expr_prec(&mut self, prec: Prec, expr: &Expr<'_>) -> surface::Expr<'arena, ()> {
        let builder = self.builder();
        match expr {
            Expr::Error => surface::Expr::Error(()),
            Expr::Local(var) => match self.local_names.get_index(*var) {
                Some(Some(name)) => surface::Expr::Ident((), *name),
                Some(None) => unreachable!("Referenced local variable without name: {var:?}"),
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
                            let expr = self.expr_prec(Prec::Top, &Expr::Local(var));
                            Arg {
                                extra: (),
                                plicity: Plicity::Explicit,
                                expr,
                            }
                        }),
                );
                self.paren(prec > Prec::App, builder.fun_app((), head, args))
            }
            Expr::Lit(literal) => surface::Expr::Lit((), lit(*literal)),
            Expr::Prim(primitive) => prim(*primitive),
            Expr::Let((def, body)) => {
                let name = def.name;
                let def = surface::LetDef {
                    pat: name_to_pat(def.name),
                    r#type: Some(self.expr_prec(Prec::Top, &def.r#type)),
                    expr: self.expr_prec(Prec::Let, &def.expr),
                };

                let name = self.freshen_name(name, body);
                self.push_local(name);
                let body = self.expr_prec(Prec::Let, body);
                self.pop_local();

                self.paren(prec > Prec::Let, builder.r#let((), def, body))
            }
            Expr::FunType(..) => {
                let mut body = expr;
                let mut params = Vec::new();

                let initial_len = self.local_len();
                let body = loop {
                    match body {
                        // Use an explicit parameter if it is referenced in the body
                        Expr::FunType(plicity, name, (r#type, next_body))
                            if next_body.binds_local(EnvLen::default()) =>
                        {
                            let r#type = self.expr_prec(Prec::Top, r#type);
                            let name = self.freshen_name(*name, next_body);
                            self.push_local(name);
                            params.push(surface::Param {
                                plicity: *plicity,
                                pat: name_to_pat(name),
                                r#type: Some(r#type),
                            });
                            body = next_body;
                        }
                        // Use arrow sugar if the parameter is not referenced in the body type.
                        Expr::FunType(plicity, _, (r#type, body)) => {
                            let domain = self.expr_prec(Prec::App, r#type);

                            self.push_local(None);
                            let codomain = self.expr_prec(Prec::Fun, body);
                            self.pop_local();

                            break builder.arrow((), *plicity, domain, codomain);
                        }
                        _ => break self.expr_prec(Prec::Fun, body),
                    }
                };
                self.truncate_local(initial_len);

                if params.is_empty() {
                    self.paren(prec > Prec::Fun, body)
                } else {
                    let params = self.scope.to_scope_from_iter(params);
                    self.paren(prec > Prec::Fun, builder.fun_type((), params, body))
                }
            }
            Expr::FunLit(..) => {
                let mut body = expr;
                let mut params = Vec::new();

                let initial_len = self.local_len();
                while let Expr::FunLit(plicity, name, (r#type, next_body)) = body {
                    let r#type = self.expr_prec(Prec::Top, r#type);
                    let name = self.freshen_name(*name, next_body);
                    self.push_local(name);
                    params.push(surface::Param {
                        plicity: *plicity,
                        pat: name_to_pat(name),
                        r#type: Some(r#type),
                    });
                    body = next_body;
                }
                let body = self.expr_prec(Prec::Let, body);
                self.truncate_local(initial_len);

                let params = self.scope.to_scope_from_iter(params);
                self.paren(prec > Prec::Fun, builder.fun_lit((), params, body))
            }
            Expr::FunApp(..) => {
                let mut args = Vec::new();
                let mut fun = expr;

                while let Expr::FunApp(plicity, (next_fun, arg)) = fun {
                    fun = next_fun;
                    let expr = self.expr_prec(Prec::Proj, arg);
                    args.push(Arg {
                        extra: (),
                        plicity: *plicity,
                        expr,
                    });
                }

                let fun = self.expr_prec(Prec::Proj, fun);
                let args = self.scope.to_scope_from_iter(args.into_iter().rev());
                self.paren(prec > Prec::App, builder.fun_app((), fun, args))
            }
            Expr::RecordType(labels, types) if is_tuple_telescope(labels, types) => {
                let initial_len = self.local_len();
                let types = (self.scope).to_scope_from_iter(labels.iter().zip(types.iter()).map(
                    |(label, r#type)| {
                        let expr = self.expr_prec(Prec::Top, r#type);
                        self.push_local(Some(*label));
                        expr
                    },
                ));
                self.truncate_local(initial_len);
                surface::Expr::TupleLit((), types)
            }
            Expr::RecordType(labels, types) => {
                let scope = self.scope;
                self.local_names.reserve(labels.len());

                let initial_local_len = self.local_len();
                let fields = labels.iter().zip(types.iter()).map(|(label, r#type)| {
                    let r#type = self.expr_prec(Prec::Top, r#type);
                    self.push_local(Some(*label));
                    TypeField {
                        label: ((), *label),
                        r#type,
                    }
                });
                let fields = scope.to_scope_from_iter(fields);
                self.truncate_local(initial_local_len);
                surface::Expr::RecordType((), fields)
            }
            Expr::RecordLit(labels, exprs) if is_tuple_labels(labels) => {
                let scope = self.scope;
                let exprs = exprs.iter().map(|expr| self.expr(expr));
                surface::Expr::TupleLit((), scope.to_scope_from_iter(exprs))
            }
            Expr::RecordLit(labels, exprs) => {
                let scope = self.scope;
                let fields = labels
                    .iter()
                    .zip(exprs.iter())
                    .map(|(label, expr)| ExprField {
                        label: ((), *label),
                        expr: self.expr_prec(Prec::Top, expr),
                    });
                surface::Expr::RecordLit((), scope.to_scope_from_iter(fields))
            }
            Expr::RecordProj(..) => {
                let mut labels = Vec::new();
                let mut head = expr;

                while let Expr::RecordProj(next_head, label) = head {
                    head = next_head;
                    labels.push(((), *label));
                }

                let head = self.expr_prec(Prec::Atom, head);
                let labels = self.scope.to_scope_from_iter(labels.into_iter().rev());
                builder.record_proj((), head, labels)
            }
            Expr::Match((scrut, default), cases) => {
                let scope = self.scope;
                let scrut = self.expr_prec(Prec::Proj, scrut);
                let default = default.map(|(name, expr)| {
                    let name = self.freshen_name(name, &expr);
                    self.push_local(name);
                    let expr = self.expr(&expr);
                    self.pop_local();
                    let pat = name_to_pat(name);
                    surface::MatchCase { pat, expr }
                });
                let cases = cases.iter().map(|(literal, expr)| {
                    let pat = surface::Pat::Lit((), lit(*literal));
                    let expr = self.expr(expr);
                    surface::MatchCase { pat, expr }
                });
                let cases = scope.to_scope_from_iter(cases.chain(default));
                surface::Expr::Match((), self.scope.to_scope(scrut), cases)
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

fn is_tuple_labels(labels: &[Symbol]) -> bool {
    labels
        .iter()
        .enumerate()
        .all(|(idx, label)| *label == Symbol::from(&format!("_{idx}")))
}

fn is_tuple_telescope(labels: &[Symbol], r#types: &[Expr]) -> bool {
    is_tuple_labels(labels)
        && (1..=types.len()).all(|index| {
            EnvLen::iter()
                .zip(types[index..].iter())
                .all(|(var, expr)| !expr.binds_local(var))
        })
}
