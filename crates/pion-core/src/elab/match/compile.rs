use pion_common::slice_vec::SliceVec;

use super::*;

/// Compilation of pattern matrices to decision trees.
/// This is the `CC` function in *Compiling pattern matching to good decision
/// trees*.
pub fn compile_match<'arena>(
    ctx: &mut ElabCtx<'arena, '_>,
    matrix: &mut PatMatrix<'arena>,
    bodies: &[Body<'arena>],
    mut shift_amount: EnvLen,
) -> Expr<'arena> {
    // Base case 1:
    // If the matrix is empty, matching always fails.
    if matrix.is_null() {
        return Expr::Error;
    }

    // Base case 2:
    // If the first row is all wildcards, matching always suceeds.
    if matrix.row(0).all_wildcards() {
        let scope = ctx.scope;
        let index = matrix.row_index(0);
        let Body { expr: body, defs } = &bodies[index];

        let initial_len = ctx.local_env.len();
        let defs = defs.iter().map(|(def_name, scrut)| {
            let def_name = *def_name;
            let def_type = ctx.quote_env().quote(&scrut.r#type);
            let def_expr = scrut.expr.shift(ctx.scope, shift_amount);
            let def_value = ctx.eval_env().eval(&def_expr);
            ctx.local_env
                .push_def(def_name, def_value, scrut.r#type.clone());
            shift_amount.push();
            (LetDef::new(def_name, def_type, def_expr), Expr::Error)
        });
        let defs = scope.to_scope_from_iter(defs);
        ctx.local_env.truncate(initial_len);

        return defs.iter_mut().rev().fold(*body, |body, pair| {
            pair.1 = body;
            Expr::Let(pair)
        });
    }

    // Inductive case:
    // The matrix must have at least one column with at least one non-wildcard
    // pattern. Select such a column, and for each constructor in the column,
    // generate a decision subtree. If the column is non-exhaustive, generate a
    // default branch as well.
    let column = matrix.column_to_split_on().unwrap();
    matrix.swap_columns(0, column);
    for (pat, scrut) in matrix.column(0) {
        match pat {
            Pat::Lit(..) => {
                let scrut_expr = scrut.expr.shift(ctx.scope, shift_amount);
                let ctors = matrix.column_constructors(0);

                let mut branches = SliceVec::new(ctx.scope, ctors.len());
                for ctor in &ctors {
                    let lit = ctor.as_lit().unwrap();
                    let mut matrix = matrix.specialize(ctx, ctor);
                    let expr = compile_match(ctx, &mut matrix, bodies, shift_amount);
                    branches.push((*lit, expr));
                }

                let default_branch = match Constructor::is_exhaustive(&ctors) {
                    true => None,
                    false => {
                        let mut matrix = matrix.default();

                        let value = ctx.eval_env().eval(&scrut_expr);
                        ctx.local_env.push_def(None, value, scrut.r#type.clone());
                        shift_amount.push();
                        let body = compile_match(ctx, &mut matrix, bodies, shift_amount);
                        ctx.local_env.pop();

                        match body {
                            #[cfg(FALSE)]
                            Expr::Let((LetDef { name, expr, .. }, body))
                                if expr == &scrut_expr.shift(ctx.scope, shift_amount) =>
                            {
                                Some((*name, body))
                            }
                            _ => Some((None, body)),
                        }
                    }
                };

                return Expr::Match(
                    ctx.scope.to_scope((scrut_expr, default_branch)),
                    branches.into(),
                );
            }

            // There is only one constructor for each record type,
            // so we only need to generate a single subtree (ie no branching needed)
            Pat::RecordLit(_, labels, _) => {
                let mut matrix = matrix.specialize(ctx, &Constructor::Record(labels));
                return compile_match(ctx, &mut matrix, bodies, shift_amount);
            }

            // Skip over non-constructor patterns
            Pat::Error(..) | Pat::Ignore(..) | Pat::Ident(..) => continue,
        }
    }

    unreachable!()
}

impl<'arena> PatMatrix<'arena> {
    /// Return the index of any column in the matrix with at least one
    /// non-wildcard pattern. At the moment, we simply selec the leftmost
    /// column, but more advanced splitting heuristcs can be used to
    /// minimize the size of the decision tree and potentially skip some tests
    /// altogether (see section 8 of *Compiling pattern matching to good
    /// decision trees*)
    pub fn column_to_split_on(&self) -> Option<usize> {
        assert!(!self.is_null(), "Cannot split null `PatternMatrix`");

        (0..self.num_columns().unwrap()).find(|&column| {
            self.column(column).any(|(pat, _)| match pat {
                Pat::Lit(..) | Pat::RecordLit(..) => true,
                Pat::Error(..) | Pat::Ignore(..) | Pat::Ident(..) => false,
            })
        })
    }

    pub fn swap_columns(&mut self, column1: usize, column2: usize) {
        assert!(
            column1 < self.num_columns().unwrap_or(0),
            "column1 is out of bounds (num_columns = {:?})",
            self.num_columns()
        );
        assert!(
            column2 < self.num_columns().unwrap_or(0),
            "column2 is out of bounds (num_columns = {:?})",
            self.num_columns()
        );

        for row in &mut self.rows {
            row.entries.swap(column1, column2);
        }
    }
}
