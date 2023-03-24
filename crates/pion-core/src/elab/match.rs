use pion_source::location::ByteRange;

use super::*;

#[derive(Debug, Clone)]
struct MatchInfo<'arena> {
    range: ByteRange,
    scrut_range: ByteRange,

    expected_type: Value<'arena>,
    scrut_type: Type<'arena>,
    scrut_expr: Expr<'arena>,
}

impl<'arena, E: FnMut(ElabError)> ElabCtx<'arena, E> {
    pub fn check_match(
        &mut self,
        range: ByteRange,
        scrut: &surface::Expr<'_>,
        cases: &[surface::MatchCase<'_>],
        expected: &Type<'arena>,
    ) -> Expr<'arena> {
        let (scrut_expr, scrut_type) = self.synth(scrut);
        let info = MatchInfo {
            range,
            scrut_range: scrut.range(),
            expected_type: expected.clone(),
            scrut_type,
            scrut_expr,
        };
        self.elab_match(&info, true, cases)
    }

    fn elab_match(
        &mut self,
        info: &MatchInfo<'arena>,
        is_reachable: bool,
        cases: &[surface::MatchCase<'_>],
    ) -> Expr<'arena> {
        match cases.split_first() {
            None => self.elab_match_absurd(info, is_reachable),
            Some((case, cases)) => match self.check_pat(&case.pat, &info.expected_type) {
                Pat::Lit(lit) => {
                    self.check_pat_reachable(is_reachable, case.pat.range());

                    let mut lit_cases = vec![(lit, self.check(&case.expr, &info.expected_type))];

                    let mut cases = cases.iter();
                    while let Some(case) = cases.next() {
                        let pat = self.check_pat(&case.pat, &info.expected_type);
                        match pat {
                            Pat::Lit(lit) => {
                                let body_expr = self.check(&case.expr, &info.expected_type);

                                match lit_cases.binary_search_by(|(l, _)| Lit::cmp(l, &lit)) {
                                    Ok(_) => self.emit_error(ElabError::UnreachablePat {
                                        range: case.pat.range(),
                                    }),
                                    Err(index) => {
                                        self.check_pat_reachable(is_reachable, case.pat.range());
                                        lit_cases.insert(index, (lit, body_expr));
                                    }
                                }

                                if let Some(n) = lit.num_inhabitants() {
                                    if lit_cases.len() as u128 >= n {
                                        // The match is exhaustive.
                                        // No need to elaborate the rest of the patterns
                                        self.elab_match_unreachable(info, cases.as_slice());

                                        return Expr::Match(
                                            self.scope.to_scope(info.scrut_expr),
                                            self.scope.to_scope_from_iter(lit_cases.into_iter()),
                                            None,
                                        );
                                    }
                                }
                            }

                            Pat::Ident(_) | Pat::Ignore | Pat::Error(_) => {
                                let name = pat.name();
                                let range = case.pat.range();

                                if !pat.is_err() {
                                    self.check_pat_reachable(is_reachable, range);
                                }
                                self.elab_match_unreachable(info, cases.as_slice());

                                let default =
                                    self.with_param(name, info.scrut_type.clone(), |this| {
                                        this.check(&case.expr, &info.expected_type)
                                    });

                                return Expr::Match(
                                    self.scope.to_scope(info.scrut_expr),
                                    self.scope.to_scope_from_iter(lit_cases),
                                    Some((name, self.scope.to_scope(default))),
                                );
                            }
                        }
                    }

                    // Finished all the literal patterns without encountering a default
                    // case or an exhaustive match
                    let default_expr = self.elab_match_absurd(info, is_reachable);

                    Expr::Match(
                        self.scope.to_scope(info.scrut_expr),
                        self.scope.to_scope_from_iter(lit_cases),
                        Some((None, self.scope.to_scope(default_expr))),
                    )
                }
                Pat::Ident(name) => {
                    self.check_pat_reachable(is_reachable, case.pat.range());

                    let def_name = Some(name);
                    let def_expr = self.eval_env().eval(&info.scrut_expr);
                    let def_type = self.quote_env().quote(&info.scrut_type);

                    let body_expr =
                        self.with_def(def_name, def_expr, info.scrut_type.clone(), |this| {
                            this.check(&case.expr, &info.expected_type)
                        });

                    self.elab_match_unreachable(info, cases);

                    self.expr_builder().r#let(
                        LetDef {
                            name: def_name,
                            r#type: def_type,
                            expr: info.scrut_expr,
                        },
                        body_expr,
                    )
                }
                Pat::Ignore => {
                    self.check_pat_reachable(is_reachable, case.pat.range());
                    let expr = self.check(&case.expr, &info.expected_type);
                    self.elab_match_unreachable(info, cases);
                    expr
                }
                // If we hit an error, propagate it, while still checking
                // the body expression and the subsequent branches.
                Pat::Error(..) => {
                    self.check(&case.expr, &info.expected_type);
                    self.elab_match_unreachable(info, cases);
                    Expr::Error
                }
            },
        }
    }

    /// Elaborate unreachable match cases. This is useful for that these cases
    /// are correctly typed, even if they are never actually needed.
    fn elab_match_unreachable(
        &mut self,
        info: &MatchInfo<'arena>,
        cases: &[surface::MatchCase<'_>],
    ) -> Expr<'arena> {
        self.elab_match(info, false, cases)
    }

    /// All the equations have been consumed.
    fn elab_match_absurd(&mut self, info: &MatchInfo<'arena>, is_reachable: bool) -> Expr<'arena> {
        // Report if we can still reach this point
        if is_reachable {
            // TODO: this should be admitted if the scrutinee type is uninhabited
            self.emit_error(ElabError::InexhaustiveMatch {
                range: info.range,
                scrut_range: info.scrut_range,
            });
        }
        Expr::Error
    }

    /// Ensure that this part of a match expression is reachable, reporting
    /// a message if it is not.
    fn check_pat_reachable(&mut self, is_reachable: bool, range: ByteRange) {
        if !is_reachable {
            self.emit_error(ElabError::UnreachablePat { range });
        }
    }
}
