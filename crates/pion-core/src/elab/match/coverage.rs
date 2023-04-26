use super::*;

pub fn check_coverage<'arena>(
    ctx: &mut ElabCtx<'arena, '_>,
    matrix: &PatMatrix<'arena>,
    match_range: ByteRange,
    scrut_range: ByteRange,
) {
    let dummy_scrut = Scrut {
        expr: Expr::Error,
        r#type: Value::ERROR,
    };
    let row = PatRow::singleton((Pat::Ignore(scrut_range), dummy_scrut));

    // A matrix is exhaustive iff the the wildcard pattern `_` is not useful
    if is_useful(ctx, matrix, &row) {
        ctx.emit_error(ElabError::InexhaustiveMatch {
            range: match_range,
            scrut_range,
        });
    }

    // A matrix row is reachable iff it is useful relative to the rows in the matrix
    // above it
    let mut rows = Vec::with_capacity(matrix.num_rows());
    for (row, _) in matrix.iter() {
        let matrix = PatMatrix::new(rows.clone());
        rows.push(row.clone());

        // Don't check reachability for patterns with errors
        if row.patterns().any(Pat::is_err) {
            continue;
        }

        if !is_useful(ctx, &matrix, row) {
            let range = row.first().unwrap().0.range();
            ctx.emit_error(ElabError::UnreachablePat { range });
        }
    }
}

/// A row of patterns, *q*, is useful relative to a matrix *m* iff there is a
/// value matched by `q` and not matched by *m*. This is the `U` function in
/// *Warnings for pattern matching*
fn is_useful<'arena>(
    ctx: &mut ElabCtx<'arena, '_>,
    matrix: &PatMatrix<'arena>,
    row: &PatRow<'arena>,
) -> bool {
    if let Some(n) = matrix.num_columns() {
        debug_assert_eq!(
            n,
            row.len(),
            "`row` must have a pattern for each column of `matrix`"
        );
    }

    // Base case 1:
    // If the matrix has no columns, but at least one row, the test row is not
    // useful
    if matrix.is_unit() {
        return false;
    }

    // Base case 2:
    // If the matrix has no columns and no rows, the test row is useful
    if matrix.is_null() {
        return true;
    }

    let (pat, _) = row.first().unwrap();
    match pat {
        // Inductive case 1:
        // If the first pattern is a constructed pattern, specialise the matrix and test row and
        // recurse
        Pat::Lit(_, lit) => is_useful_ctor(ctx, matrix, row, &Constructor::Lit(*lit)),
        Pat::RecordLit(_, labels, _) => {
            is_useful_ctor(ctx, matrix, row, &Constructor::Record(labels))
        }

        // Inductive case 2:
        // If the first pattern is a wildcard pattern, collect all the constructors in the first
        // column of matrix and test for exhaustiveness
        Pat::Error(..) | Pat::Ignore(..) | Pat::Ident(..) => {
            let ctors = matrix.column_constructors(0);
            match Constructor::is_exhaustive(&ctors) {
                // Inductive case 2a:
                // If the constructors are exhaustive, specialise the matrix and test row against
                // each constructor and recurse
                true => ctors
                    .into_iter()
                    .any(|ctor| is_useful_ctor(ctx, matrix, row, &ctor)),
                // Inductive case 2b:
                // If the constructors are not exhaustive, recurse on the defaulted matrix
                false => {
                    let matrix = matrix.default();
                    is_useful(ctx, &matrix, &row.tail())
                }
            }
        }
    }
}

fn is_useful_ctor<'arena>(
    ctx: &mut ElabCtx<'arena, '_>,
    matrix: &PatMatrix<'arena>,
    row: &PatRow<'arena>,
    ctor: &Constructor<'arena>,
) -> bool {
    let matrix = matrix.specialize(ctx, ctor);
    let row = row.specialize(ctx, ctor);
    match row {
        None => false,
        Some(row) => is_useful(ctx, &matrix, &row),
    }
}
