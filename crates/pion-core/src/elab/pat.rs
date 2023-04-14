use pion_source::location::ByteRange;

use super::expr::synth_lit;
use super::*;

impl<'arena, E: FnMut(ElabError)> ElabCtx<'arena, E> {
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
                        Pat::Error(&())
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
            surface::Pat::RecordLit(..) => todo!(),
            surface::Pat::TupleLit(..) => todo!(),
        }
    }

    pub fn check_pat(&mut self, pat: &surface::Pat, expected: &Type<'arena>) -> Pat<'arena> {
        match pat {
            surface::Pat::Paren(..) => self.check_pat(pat, expected),
            surface::Pat::Ident(_, name) => Pat::Ident(*name),
            surface::Pat::Underscore(_) => Pat::Ignore,
            surface::Pat::Lit(..) => {
                let range = pat.range();
                let (pat, r#type) = self.synth_pat(pat);
                self.convert_pat(range, pat, &r#type, expected)
            }
            surface::Pat::RecordLit(..) => todo!(),
            surface::Pat::TupleLit(..) => todo!(),
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
                Pat::Error(&())
            }
        }
    }
}
