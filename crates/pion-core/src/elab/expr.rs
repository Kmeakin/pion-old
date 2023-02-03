use pion_source::location::ByteRange;

use super::*;

impl<'arena> ElabCtx<'arena> {
    pub(super) fn synth_lit(&mut self, lit: &surface::Lit) -> (Lit, Type<'arena>) {
        match lit {
            surface::Lit::Bool(_, value) => (Lit::Bool(*value), Type::BOOL),
            surface::Lit::Int(_, value) => (Lit::Int(*value), Type::INT),
        }
    }

    /// Synthesize the type of the given surface expr.
    ///
    /// Returns the elaborated expr in the core language and its type.
    pub fn synth(&mut self, expr: &surface::Expr) -> (Expr<'arena>, Type<'arena>) {
        match expr {
            surface::Expr::Error(_) => (Expr::Error, Type::ERROR),
            surface::Expr::Paren(_, expr) => self.synth(expr),
            surface::Expr::Lit(_, lit) => {
                let (lit, r#type) = self.synth_lit(lit);
                (Expr::Lit(lit), r#type)
            }
            surface::Expr::Placeholder(range) => {
                let type_source = MetaSource::PlaceholderType(*range);
                let expr_source = MetaSource::PlaceholderExpr(*range);

                let r#type = self.push_unsolved_type(type_source);
                let expr = self.push_unsolved_expr(expr_source, r#type.clone());

                (expr, r#type)
            }
            surface::Expr::Hole(range, name) => {
                let type_source = MetaSource::HoleType(*range, *name);
                let expr_source = MetaSource::HoleExpr(*range, *name);

                let r#type = self.push_unsolved_type(type_source);
                let expr = self.push_unsolved_expr(expr_source, r#type.clone());

                (expr, r#type)
            }
            surface::Expr::Ident(range, name) => {
                if let Some((index, r#type)) = self.local_env.lookup(*name) {
                    return (Expr::Local(index), r#type.clone());
                }

                if let Some((prim, r#type)) = self.prim_env.lookup(*name) {
                    return (Expr::Prim(prim), r#type.clone());
                }

                self.errors.push(Error::UnboundName {
                    range: *range,
                    name: *name,
                });
                self.synth_error_expr()
            }
            surface::Expr::Let(_, (def, body)) => {
                let def = self.synth_let_def(def);
                let (body_expr, body_type) = self.synth(body);
                self.local_env.pop();
                (Expr::Let(self.scope.to_scope((def, body_expr))), body_type)
            }
            surface::Expr::Arrow(_, (domain, codomain)) => {
                let domain = self.check(domain, &Type::TYPE);
                let domain_value = self.eval_env().eval(&domain);

                self.local_env.push_param(None, domain_value);
                let codomain = self.check(codomain, &Type::TYPE);
                self.local_env.pop();

                let expr = Expr::FunLit(None, self.scope.to_scope((domain, codomain)));
                (expr, Type::TYPE)
            }
            surface::Expr::FunType(_, params, body) => {
                self.local_env.reserve(params.len());
                self.synth_fun_type(params, body)
            }
            surface::Expr::FunLit(_, params, body) => {
                self.local_env.reserve(params.len());
                self.synth_fun_lit(params, body)
            }
            surface::Expr::FunApp(_, fun, args) => {
                let (mut expr, mut r#type) = self.synth(fun);
                let mut fun_range = fun.range();
                for arg in args.iter() {
                    r#type = self.elim_env().update_metas(&r#type);
                    match r#type {
                        Value::FunType(_, domain, codomain) => {
                            let arg_expr = self.check(arg, domain);
                            let arg_value = self.eval_env().eval(&arg_expr);

                            fun_range = ByteRange::merge(fun_range, arg.range());
                            expr = Expr::FunApp(self.scope.to_scope((expr, arg_expr)));
                            r#type = self.elim_env().apply_closure(codomain, arg_value);
                        }
                        _ if expr.is_error() || r#type.is_error() => {
                            return self.synth_error_expr()
                        }
                        _ => {
                            self.errors.push(Error::UnexpectedArgument {
                                fun_range,
                                arg_range: arg.range(),
                            });
                            return self.synth_error_expr();
                        }
                    }
                }
                (expr, r#type)
            }
        }
    }

    fn synth_error_expr(&mut self) -> (Expr<'arena>, Type<'arena>) { (Expr::Error, Type::ERROR) }

    fn synth_let_def(&mut self, def: &surface::LetDef) -> LetDef<'arena> {
        let (pat, type_value) = self.synth_ann_pat(&def.pat, &def.r#type);
        let name = pat.name();

        let expr = self.check(&def.expr, &type_value);
        let expr_value = self.eval_env().eval(&expr);

        let r#type = self.quote_env().quote(&type_value);

        self.local_env.push_def(name, type_value, expr_value);
        LetDef { name, r#type, expr }
    }

    fn synth_fun_type(
        &mut self,
        params: &[surface::Param],
        body: &surface::Expr,
    ) -> (Expr<'arena>, Type<'arena>) {
        match params.split_first() {
            None => {
                let expr = self.check(body, &Value::TYPE);
                (expr, Value::TYPE)
            }
            Some((param, params)) => {
                let (pat, param_type) = self.synth_ann_pat(&param.pat, &param.r#type);
                let param_type_expr = self.quote_env().quote(&param_type);
                let name = pat.name();

                self.local_env.push_param(name, param_type);
                let (body_expr, _) = self.synth_fun_type(params, body);
                self.local_env.pop();

                let expr = Expr::FunType(name, self.scope.to_scope((param_type_expr, body_expr)));
                let r#type = Value::TYPE;
                (expr, r#type)
            }
        }
    }

    fn synth_fun_lit(
        &mut self,
        params: &[surface::Param],
        body: &surface::Expr,
    ) -> (Expr<'arena>, Type<'arena>) {
        match params.split_first() {
            None => self.synth(body),
            Some((param, params)) => {
                let (pat, param_type) = self.synth_ann_pat(&param.pat, &param.r#type);
                let param_type_expr = self.quote_env().quote(&param_type);
                let name = pat.name();

                self.local_env.push_param(name, param_type.clone());
                let (body_expr, body_type) = self.synth_fun_lit(params, body);
                let body_type_expr = self.quote_env().quote(&body_type);
                self.local_env.pop();

                let expr = Expr::FunLit(name, self.scope.to_scope((param_type_expr, body_expr)));
                let r#type = Value::FunType(
                    name,
                    self.scope.to_scope(param_type),
                    Closure::new(
                        self.local_env.values.clone(),
                        self.scope.to_scope(body_type_expr),
                    ),
                );
                (expr, r#type)
            }
        }
    }

    fn check_fun_lit(
        &mut self,
        range: ByteRange,
        params: &[surface::Param],
        body: &surface::Expr,
        expected: &Type<'arena>,
    ) -> Expr<'arena> {
        match params.split_first() {
            None => self.check(body, expected),
            Some((param, params)) => {
                let expected = self.elim_env().update_metas(expected);
                match expected {
                    Value::FunType(_, domain, codomain) => {
                        let pat = self.check_ann_pat(&param.pat, &param.r#type, domain);
                        let param_type_expr = self.quote_env().quote(domain);
                        let name = pat.name();

                        let arg = self.local_env.next_var();
                        self.local_env.push_param(name, domain.clone());
                        let body_type = self.elim_env().apply_closure(codomain, arg);
                        let body_expr = self.check_fun_lit(range, params, body, &body_type);
                        self.local_env.pop();

                        Expr::FunLit(name, self.scope.to_scope((param_type_expr, body_expr)))
                    }
                    _ if expected.is_error() => Expr::Error,
                    _ => {
                        let (expr, r#type) = self.synth_fun_lit(params, body);
                        self.convert(range, expr, &r#type, &expected)
                    }
                }
            }
        }
    }

    pub fn check(&mut self, expr: &surface::Expr, expected: &Type<'arena>) -> Expr<'arena> {
        let expected = self.elim_env().update_metas(expected);
        match (expr, &expected) {
            (surface::Expr::Error(_), _) => Expr::Error,
            (surface::Expr::Paren(..), _) => self.check(expr, &expected),
            (surface::Expr::Let(_, (def, body)), _) => {
                let def = self.synth_let_def(def);
                let body = self.check(body, &expected);
                self.local_env.pop();
                Expr::Let(self.scope.to_scope((def, body)))
            }
            (surface::Expr::FunLit(range, params, body), _) => {
                self.local_env.reserve(params.len());
                self.check_fun_lit(*range, params, body, &expected)
            }
            _ => {
                let range = expr.range();
                let (expr, r#type) = self.synth(expr);
                self.convert(range, expr, &r#type, &expected)
            }
        }
    }

    fn convert(
        &mut self,
        range: ByteRange,
        expr: Expr<'arena>,
        from: &Type<'arena>,
        to: &Type<'arena>,
    ) -> Expr<'arena> {
        match self.unifiy_ctx().unify(from, to) {
            Ok(()) => expr,
            Err(error) => {
                self.errors.push(Error::Unification { range, error });
                Expr::Error
            }
        }
    }
}
