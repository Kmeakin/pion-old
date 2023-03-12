use pion_source::location::ByteRange;

use super::*;

impl<'arena, E: FnMut(ElabError)> ElabCtx<'arena, E> {
    /// Synthesize the type of the given surface expr.
    ///
    /// Returns the elaborated expr in the core language and its type.
    pub fn synth(&mut self, expr: &surface::Expr) -> (Expr<'arena>, Type<'arena>) {
        match expr {
            surface::Expr::Error(_) => (Expr::Error, Type::ERROR),
            surface::Expr::Paren(_, expr) => self.synth(expr),
            surface::Expr::Ann(_, (expr, r#type)) => {
                let r#type = self.check(r#type, &Value::TYPE);
                let type_value = self.eval_env().eval(&r#type);
                let expr = self.check(expr, &type_value);
                (expr, type_value)
            }
            surface::Expr::Lit(_, lit) => {
                let (lit, r#type) = synth_lit(lit);
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

                self.emit_error(ElabError::UnboundName {
                    range: *range,
                    name: *name,
                });
                synth_error_expr()
            }
            surface::Expr::Let(_, (def, body)) => {
                let def = self.synth_let_def(def);
                let (body_expr, body_type) = self.synth(body);
                self.local_env.pop();
                (Expr::Let(self.scope.to_scope((def, body_expr))), body_type)
            }
            surface::Expr::Arrow(_, plicity, (domain, codomain)) => {
                let domain = self.check(domain, &Type::TYPE);
                let domain_value = self.eval_env().eval(&domain);

                self.local_env.push_param(None, domain_value);
                let codomain = self.check(codomain, &Type::TYPE);
                self.local_env.pop();

                let expr = Expr::FunType(*plicity, None, self.scope.to_scope((domain, codomain)));
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
                let (mut expr, r#type) = self.synth(fun);

                let fun_range = fun.range();
                let fun_type = self.elim_env().update_metas(&r#type);
                let mut r#type = fun_type.clone();

                for (arity, arg) in args.iter().enumerate() {
                    r#type = self.elim_env().update_metas(&r#type);
                    match r#type {
                        Value::FunType(plicity, _, domain, codomain) => {
                            let arg_expr = self.check(&arg.expr, domain);
                            let arg_value = self.eval_env().eval(&arg_expr);

                            expr = Expr::FunApp(plicity, self.scope.to_scope((expr, arg_expr)));
                            r#type = self.elim_env().apply_closure(codomain, arg_value);
                        }
                        _ if expr.is_error() || r#type.is_error() => return synth_error_expr(),
                        _ if arity == 0 => {
                            let fun_type = self.pretty_value(&fun_type);
                            self.emit_error(ElabError::FunAppNotFun {
                                fun_range,
                                fun_type,
                                num_args: args.len(),
                                args_range: {
                                    let first = args.first().unwrap();
                                    let last = args.last().unwrap();
                                    ByteRange::merge(first.range(), last.range())
                                },
                            });
                            return synth_error_expr();
                        }
                        _ => {
                            let fun_type = self.pretty_value(&fun_type);
                            self.emit_error(ElabError::FunAppTooManyArgs {
                                fun_range,
                                fun_type,
                                expected_arity: arity,
                                actual_arity: args.len(),
                                extra_args_range: {
                                    let extra_args = &args[arity..];
                                    let first_extra = extra_args.first().unwrap();
                                    let last_extra = extra_args.last().unwrap();
                                    ByteRange::merge(first_extra.range(), last_extra.range())
                                },
                            });
                            return synth_error_expr();
                        }
                    }
                }
                (expr, r#type)
            }
            surface::Expr::RecordType(_, fields) => {
                let mut labels = Vec::with_capacity(fields.len());
                let mut types = Vec::with_capacity(fields.len());

                let len = self.local_env.len();
                for field in fields.iter() {
                    let r#type = self.check(&field.r#type, &Type::TYPE);
                    let type_value = self.eval_env().eval(&r#type);

                    let (label_range, label) = field.label;
                    if let Some((first_range, _)) = labels.iter().find(|(_, l)| *l == label) {
                        self.emit_error(ElabError::RecordFieldDuplicate {
                            name: "record type",
                            label,
                            first_range: *first_range,
                            duplicate_range: label_range,
                        });
                    } else {
                        self.local_env.push_param(Some(label), type_value);
                        labels.push((label_range, label));
                        types.push(r#type);
                    }
                }
                self.local_env.truncate(len);

                let labels = self
                    .scope
                    .to_scope_from_iter(labels.iter().map(|(_, label)| *label));
                let types = self.scope.to_scope_from_iter(types);
                (Expr::RecordType(labels, types), Type::TYPE)
            }
            surface::Expr::RecordLit(_, fields) => {
                let mut labels = Vec::with_capacity(fields.len());
                let mut exprs = Vec::with_capacity(fields.len());
                let mut types = Vec::with_capacity(fields.len());

                for field in fields.iter() {
                    let (expr, type_value) = self.synth(&field.expr);
                    let r#type = self.quote_env().quote(&type_value);

                    let (label_range, label) = field.label;
                    if let Some((first_range, _)) = labels.iter().find(|(_, l)| *l == label) {
                        self.emit_error(ElabError::RecordFieldDuplicate {
                            name: "record literal",
                            label,
                            first_range: *first_range,
                            duplicate_range: label_range,
                        });
                    } else {
                        labels.push((label_range, label));
                        exprs.push(expr);
                        types.push(r#type);
                    }
                }

                let labels = self
                    .scope
                    .to_scope_from_iter(labels.iter().map(|(_, label)| *label));
                let exprs = self.scope.to_scope_from_iter(exprs);
                let types = self.scope.to_scope_from_iter(types);

                let telescope = Telescope::new(self.local_env.values.clone(), types);
                (
                    Expr::RecordLit(labels, exprs),
                    Value::RecordType(labels, telescope),
                )
            }
            surface::Expr::TupleLit(_, elems) => {
                let mut labels = Vec::with_capacity(elems.len());
                let mut exprs = Vec::with_capacity(elems.len());
                let mut types = Vec::with_capacity(elems.len());

                for (idx, expr) in elems.iter().enumerate() {
                    let (expr, type_value) = self.synth(expr);
                    let r#type = self.quote_env().quote(&type_value);
                    labels.push(Symbol::from(&format!("_{idx}")));
                    exprs.push(expr);
                    types.push(r#type);
                }

                let labels = self.scope.to_scope_from_iter(labels);
                let exprs = self.scope.to_scope_from_iter(exprs);
                let types = self.scope.to_scope_from_iter(types);

                let telescope = Telescope::new(self.local_env.values.clone(), types);
                (
                    Expr::RecordLit(labels, exprs),
                    Value::RecordType(labels, telescope),
                )
            }
            surface::Expr::RecordProj(_, head, labels) => {
                let mut head_range = head.range();
                let (mut head_expr, mut head_type) = self.synth(head);

                for (label_range, proj_label) in labels.iter() {
                    head_type = self.elim_env().update_metas(&head_type);
                    match &head_type {
                        _ if head_expr.is_error() || head_type.is_error() => {
                            return synth_error_expr()
                        }

                        Value::RecordType(labels, telescope) => {
                            let mut telescope = telescope.clone();
                            let mut labels = labels.iter();
                            while let Some((label, (r#type, cont))) = Option::zip(
                                labels.next(),
                                self.elim_env().split_telescope(telescope.clone()),
                            ) {
                                if proj_label == label {
                                    head_range = ByteRange::merge(head_range, *label_range);
                                    head_expr = Expr::RecordProj(
                                        self.scope.to_scope(head_expr),
                                        *proj_label,
                                    );
                                    head_type = r#type;
                                    continue;
                                }

                                let head_value = self.eval_env().eval(&head_expr);
                                let value = self.elim_env().record_proj(head_value, *label);
                                telescope = cont(value);
                            }
                        }

                        _ => {
                            let head_type = self.pretty_value(&head_type);
                            self.emit_error(ElabError::RecordProjNotRecord {
                                head_range,
                                head_type,
                                label_range: *label_range,
                                label: *proj_label,
                            });
                            return synth_error_expr();
                        }
                    }

                    let head_type = self.pretty_value(&head_type);
                    self.emit_error(ElabError::RecordProjNotFound {
                        head_range,
                        head_type,
                        label_range: *label_range,
                        label: *proj_label,
                    });
                }
                (head_expr, head_type)
            }
        }
    }

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

                let expr = Expr::FunType(
                    param.plicity,
                    name,
                    self.scope.to_scope((param_type_expr, body_expr)),
                );
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

                let expr = Expr::FunLit(
                    param.plicity,
                    name,
                    self.scope.to_scope((param_type_expr, body_expr)),
                );
                let r#type = Value::FunType(
                    param.plicity,
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
            Some((param, next_params)) => {
                let expected = self.elim_env().update_metas(expected);
                match expected {
                    Value::FunType(plicity, _, domain, codomain) => {
                        let pat = self.check_ann_pat(&param.pat, &param.r#type, domain);
                        let param_type_expr = self.quote_env().quote(domain);
                        let name = pat.name();

                        let arg = self.local_env.next_var();
                        self.local_env.push_param(name, domain.clone());
                        let body_type = self.elim_env().apply_closure(codomain, arg);
                        let body_expr = self.check_fun_lit(range, next_params, body, &body_type);
                        self.local_env.pop();

                        Expr::FunLit(
                            plicity,
                            name,
                            self.scope.to_scope((param_type_expr, body_expr)),
                        )
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
            (surface::Expr::Paren(_, expr), _) => self.check(expr, &expected),
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
            (surface::Expr::TupleLit(_, elems), _) if expected.is_type() => {
                let mut labels = Vec::with_capacity(elems.len());
                let mut types = Vec::with_capacity(elems.len());

                let len = self.local_env.len();
                for (idx, expr) in elems.iter().enumerate() {
                    let r#type = self.check(expr, &Type::TYPE);
                    let label = Symbol::from(&format!("_{idx}"));
                    labels.push(label);
                    types.push(r#type);
                    let type_value = self.eval_env().eval(&r#type);
                    self.local_env.push_param(Some(label), type_value);
                }
                self.local_env.truncate(len);

                let labels = self.scope.to_scope_from_iter(labels);
                let types = self.scope.to_scope_from_iter(types);

                Expr::RecordType(labels, types)
            }
            (surface::Expr::RecordLit(_, fields), Value::RecordType(labels, telescope))
                if Iterator::eq(
                    fields.iter().map(|field| field.label.1),
                    labels.iter().copied(),
                ) =>
            {
                let mut telescope = telescope.clone();
                let mut fields = fields.iter();
                let mut exprs = Vec::with_capacity(telescope.len());

                while let Some((field, (r#type, cont))) =
                    Option::zip(fields.next(), self.elim_env().split_telescope(telescope))
                {
                    let expr = self.check(&field.expr, &r#type);
                    telescope = cont(self.eval_env().eval(&expr));
                    exprs.push(expr);
                }

                Expr::RecordLit(labels, self.scope.to_scope_from_iter(exprs))
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
                let found = self.pretty_value(from);
                let expected = self.pretty_value(to);
                self.emit_error(ElabError::Unification {
                    range,
                    found,
                    expected,
                    error,
                });
                Expr::Error
            }
        }
    }
}

fn synth_error_expr<'arena>() -> (Expr<'arena>, Type<'arena>) { (Expr::Error, Type::ERROR) }

pub(super) fn synth_lit<'arena>(lit: &surface::Lit) -> (Lit, Type<'arena>) {
    match lit {
        surface::Lit::Bool(_, value) => (Lit::Bool(*value), Type::BOOL),
        surface::Lit::Int(_, value) => (Lit::Int(*value), Type::INT),
    }
}
