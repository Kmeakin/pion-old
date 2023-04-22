use pion_source::location::ByteRange;

use super::expr::synth_lit;
use super::*;

impl<'arena, 'error> ElabCtx<'arena, 'error> {
    pub fn synth_ann_pat(
        &mut self,
        pat: &surface::Pat,
        ann: &Option<surface::Expr>,
    ) -> (Pat<'arena>, Type<'arena>) {
        match ann {
            None => self.synth_pat(pat),
            Some(ann) => {
                let ann_expr = self.check(ann, &Value::TYPE);
                let ann_value = self.eval_env().eval(&ann_expr);
                (self.check_pat(pat, &ann_value), ann_value)
            }
        }
    }

    pub fn check_ann_pat(
        &mut self,
        pat: &surface::Pat,
        ann: &Option<surface::Expr>,
        expected: &Type<'arena>,
    ) -> Pat<'arena> {
        match ann {
            None => self.check_pat(pat, expected),
            Some(ann) => {
                let ann_expr = self.check(ann, &Value::TYPE);
                let ann_value = self.eval_env().eval(&ann_expr);
                match self.unifiy_ctx().unify(&ann_value, expected) {
                    Ok(()) => self.check_pat(pat, &ann_value),
                    Err(error) => {
                        let found = self.pretty_value(&ann_value);
                        let expected = self.pretty_value(expected);
                        self.emit_error(ElabError::Unification {
                            range: ann.range(),
                            error,
                            found,
                            expected,
                        });
                        Pat::Error
                    }
                }
            }
        }
    }

    pub fn synth_pat(&mut self, pat: &surface::Pat) -> (Pat<'arena>, Type<'arena>) {
        match pat {
            surface::Pat::Paren(_, pat) => self.synth_pat(pat),
            surface::Pat::Lit(_, lit) => {
                let (lit, r#type) = synth_lit(lit);
                (Pat::Lit(lit), r#type)
            }
            surface::Pat::Ident(range, name) => {
                let source = MetaSource::PatType(*range);
                let r#type = self.push_unsolved_type(source);
                (Pat::Ident(*name), r#type)
            }
            surface::Pat::Underscore(range) => {
                let source = MetaSource::PatType(*range);
                let r#type = self.push_unsolved_type(source);
                (Pat::Ignore, r#type)
            }
            surface::Pat::RecordLit(_, fields) => {
                let mut labels = Vec::with_capacity(fields.len());
                let mut pats = Vec::with_capacity(fields.len());
                let mut types = Vec::with_capacity(fields.len());

                for field in fields.iter() {
                    let (pat, type_value) = self.synth_pat(&field.pat);
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
                        pats.push(pat);
                        types.push(r#type);
                    }
                }

                let labels = self
                    .scope
                    .to_scope_from_iter(labels.iter().map(|(_, label)| *label));
                let pats = self.scope.to_scope_from_iter(pats);
                let types = self.scope.to_scope_from_iter(types);

                let telescope = Telescope::new(self.local_env.values.clone(), types);
                (
                    Pat::RecordLit(labels, pats),
                    Value::RecordType(labels, telescope),
                )
            }
            surface::Pat::TupleLit(_, elems) => {
                let mut labels = Vec::with_capacity(elems.len());
                let mut pats = Vec::with_capacity(elems.len());
                let mut types = Vec::with_capacity(elems.len());

                for (idx, pat) in elems.iter().enumerate() {
                    let (pat, type_value) = self.synth_pat(pat);
                    let r#type = self.quote_env().quote(&type_value);
                    labels.push(Symbol::from(&format!("_{idx}")));
                    pats.push(pat);
                    types.push(r#type);
                }

                let labels = self.scope.to_scope_from_iter(labels);
                let pats = self.scope.to_scope_from_iter(pats);
                let types = self.scope.to_scope_from_iter(types);

                let telescope = Telescope::new(self.local_env.values.clone(), types);
                (
                    Pat::RecordLit(labels, pats),
                    Value::RecordType(labels, telescope),
                )
            }
        }
    }

    pub fn check_pat(&mut self, pat: &surface::Pat, expected: &Type<'arena>) -> Pat<'arena> {
        match pat {
            surface::Pat::Paren(..) => self.check_pat(pat, expected),
            surface::Pat::Ident(_, name) => Pat::Ident(*name),
            surface::Pat::Underscore(_) => Pat::Ignore,
            _ => {
                let range = pat.range();
                let (pat, r#type) = self.synth_pat(pat);
                self.convert_pat(range, pat, &r#type, expected)
            }
        }
    }

    fn convert_pat(
        &mut self,
        range: ByteRange,
        pat: Pat<'arena>,
        from: &Type<'arena>,
        to: &Type<'arena>,
    ) -> Pat<'arena> {
        match self.unifiy_ctx().unify(from, to) {
            Ok(()) => pat,
            Err(error) => {
                let found = self.pretty_value(from);
                let expected = self.pretty_value(to);
                self.emit_error(ElabError::Unification {
                    range,
                    found,
                    expected,
                    error,
                });
                Pat::Error
            }
        }
    }
}
