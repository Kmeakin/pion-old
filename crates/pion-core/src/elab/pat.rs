use pion_common::slice_vec::SliceVec;

use super::expr::synth_lit;
use super::r#match::Scrut;
use super::*;

impl<'arena, 'message> ElabCtx<'arena, 'message> {
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
                        self.emit_message(Message::Unification {
                            range: ann.range(),
                            error,
                            found,
                            expected,
                        });
                        Pat::Error(pat.range())
                    }
                }
            }
        }
    }

    pub fn synth_pat(&mut self, pat: &surface::Pat) -> (Pat<'arena>, Type<'arena>) {
        let range = pat.range();
        match pat {
            surface::Pat::Paren(_, pat) => self.synth_pat(pat),
            surface::Pat::Lit(_, lit) => {
                let (lit, r#type) = synth_lit(lit);
                (Pat::Lit(range, lit), r#type)
            }
            surface::Pat::Ident(_, name) => {
                let source = MetaSource::PatType(range);
                let r#type = self.push_unsolved_type(source);
                (Pat::Ident(range, *name), r#type)
            }
            surface::Pat::Underscore(..) => {
                let source = MetaSource::PatType(range);
                let r#type = self.push_unsolved_type(source);
                (Pat::Ignore(range), r#type)
            }
            surface::Pat::RecordLit(_, fields) => {
                let mut label_ranges = Vec::with_capacity(fields.len());
                let mut labels = SliceVec::new(self.scope, fields.len());
                let mut pats = SliceVec::new(self.scope, fields.len());
                let mut types = SliceVec::new(self.scope, fields.len());

                let initial_len = self.local_env.len();
                for field in fields.iter() {
                    let (pat, type_value) = self.synth_pat(&field.pat);
                    let r#type = self.quote_env().quote(&type_value);

                    let (label_range, label) = field.label;
                    if let Some(idx) = labels.iter().position(|l| *l == label) {
                        self.emit_message(Message::RecordFieldDuplicate {
                            name: "record literal",
                            label,
                            first_range: label_ranges[idx],
                            duplicate_range: label_range,
                        });
                    } else {
                        label_ranges.push(label_range);
                        labels.push(label);
                        pats.push(pat);
                        types.push(r#type);
                        self.local_env.push_param(Some(label), type_value);
                    }
                }
                self.local_env.truncate(initial_len);

                let labels = labels.into();
                let telescope = Telescope::new(self.local_env.values.clone(), types.into());
                (
                    Pat::RecordLit(range, labels, pats.into()),
                    Value::RecordType(labels, telescope),
                )
            }
            surface::Pat::TupleLit(_, elems) => {
                let mut labels = SliceVec::new(self.scope, elems.len());
                let mut pats = SliceVec::new(self.scope, elems.len());
                let mut types = SliceVec::new(self.scope, elems.len());

                let initial_len = self.local_env.len();
                for (idx, pat) in elems.iter().enumerate() {
                    let (pat, type_value) = self.synth_pat(pat);
                    let r#type = self.quote_env().quote(&type_value);
                    let label = Symbol::from(&format!("_{idx}"));
                    labels.push(label);
                    pats.push(pat);
                    types.push(r#type);
                    self.local_env.push_param(Some(label), type_value);
                }
                self.local_env.truncate(initial_len);

                let labels = labels.into();
                let telescope = Telescope::new(self.local_env.values.clone(), types.into());
                (
                    Pat::RecordLit(range, labels, pats.into()),
                    Value::RecordType(labels, telescope),
                )
            }
        }
    }

    pub fn check_pat(&mut self, pat: &surface::Pat, expected: &Type<'arena>) -> Pat<'arena> {
        let range = pat.range();
        let expected = &self.elim_env().update_metas(expected);

        match (pat, expected) {
            (surface::Pat::Paren(..), _) => self.check_pat(pat, expected),
            (surface::Pat::Ident(_, name), _) => Pat::Ident(range, *name),
            (surface::Pat::Underscore(_), _) => Pat::Ignore(range),
            _ => {
                let (pat, r#type) = self.synth_pat(pat);
                self.convert_pat(pat, &r#type, expected)
            }
        }
    }

    fn convert_pat(
        &mut self,
        pat: Pat<'arena>,
        from: &Type<'arena>,
        to: &Type<'arena>,
    ) -> Pat<'arena> {
        let range = pat.range();
        let from = self.elim_env().update_metas(from);
        let to = self.elim_env().update_metas(to);

        match self.unifiy_ctx().unify(&from, &to) {
            Ok(()) => pat,
            Err(error) => {
                let found = self.pretty_value(&from);
                let expected = self.pretty_value(&to);
                self.emit_message(Message::Unification {
                    range,
                    found,
                    expected,
                    error,
                });
                Pat::Error(range)
            }
        }
    }

    pub fn push_param_pat(
        &mut self,
        pat: &Pat<'arena>,
        scrut: &Scrut<'arena>,
    ) -> Vec<(Option<Symbol>, Scrut<'arena>)> {
        let mut defs = Vec::new();
        match pat {
            Pat::Error(..) | Pat::Ignore(..) | Pat::Ident(..) | Pat::Lit(..) => {
                self.local_env.push_param(pat.name(), scrut.r#type.clone());
            }
            Pat::RecordLit(_, labels, pats) => {
                let value = self.local_env.next_var();
                self.local_env.push_param(None, scrut.r#type.clone());
                self.push_record_pat(labels, pats, scrut, &value, &mut defs);
            }
        }
        defs
    }

    pub fn push_def_pat(
        &mut self,
        pat: &Pat<'arena>,
        scrut: &Scrut<'arena>,
        value: &Value<'arena>,
    ) -> Vec<(Option<Symbol>, Scrut<'arena>)> {
        let mut defs = Vec::new();
        match pat {
            Pat::Error(..) | Pat::Ignore(..) | Pat::Ident(..) | Pat::Lit(..) => {
                let r#type = scrut.r#type.clone();
                defs.push((pat.name(), scrut.clone()));
                self.local_env.push_def(pat.name(), r#type, value.clone());
            }
            Pat::RecordLit(_, labels, pats) => {
                self.push_record_pat(labels, pats, scrut, value, &mut defs);
            }
        }
        defs
    }

    pub fn push_match_pat(
        &mut self,
        pat: &Pat<'arena>,
        scrut: &Scrut<'arena>,
        value: &Value<'arena>,
    ) -> Vec<(Option<Symbol>, Scrut<'arena>)> {
        let mut defs = Vec::new();
        match pat {
            Pat::Error(..) | Pat::Ignore(..) => {
                self.local_env
                    .push_def(None, scrut.r#type.clone(), value.clone());
            }
            Pat::Ident(_, name) => {
                let r#type = scrut.r#type.clone();
                defs.push((Some(*name), scrut.clone()));
                self.local_env.push_def(Some(*name), r#type, value.clone());
            }
            Pat::Lit(..) => {}
            Pat::RecordLit(_, labels, pats) => {
                self.push_record_pat(labels, pats, scrut, value, &mut defs);
            }
        }
        defs
    }

    fn push_record_pat(
        &mut self,
        labels: &[Symbol],
        pats: &[Pat<'arena>],
        scrut: &Scrut<'arena>,
        value: &Value<'arena>,
        defs: &mut Vec<(Option<Symbol>, Scrut<'arena>)>,
    ) {
        let mut iter = Iterator::zip(labels.iter(), pats.iter());
        let Type::RecordType(_, mut telescope) = self.elim_env().update_metas(&scrut.r#type) else {
            unreachable!("expected record type, got")
        };

        while let Some(((label, pat), (r#type, cont))) =
            Option::zip(iter.next(), self.elim_env().split_telescope(telescope))
        {
            telescope = cont(self.local_env.next_var());
            let expr = self.expr_builder().record_proj(scrut.expr, *label);
            let value = self.elim_env().record_proj(value.clone(), *label);
            let scrut = Scrut::new(expr, r#type);

            match pat {
                Pat::Error(..) | Pat::Ignore(..) | Pat::Lit(..) => {}
                Pat::Ident(_, name) => {
                    let r#type = scrut.r#type.clone();
                    defs.push((Some(*name), scrut));
                    self.local_env.push_def(Some(*name), r#type, value);
                }
                Pat::RecordLit(_, labels, pats) => {
                    self.push_record_pat(labels, pats, &scrut, &value, defs);
                }
            }
        }
    }
}
