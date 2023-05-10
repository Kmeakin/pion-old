use pion_source::location::ByteRange;

use super::*;

pub mod compile;
pub mod coverage;

#[derive(Debug, Clone)]
pub struct Scrut<'arena> {
    pub expr: Expr<'arena>,
    pub r#type: Type<'arena>,
}

impl<'arena> Scrut<'arena> {
    pub fn new(expr: Expr<'arena>, r#type: Type<'arena>) -> Self { Self { expr, r#type } }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Constructor<'arena> {
    Lit(Lit),
    Record(&'arena [Symbol]),
}

impl<'arena> Constructor<'arena> {
    /// Return number of fields `self` carries
    pub fn arity(&self) -> usize {
        match self {
            Constructor::Lit(_) => 0,
            Constructor::Record(labels) => labels.len(),
        }
    }

    pub fn is_exhaustive(ctors: &[Constructor]) -> bool {
        match ctors.first() {
            None => false,
            Some(ctor) => match ctor.num_inhabitants() {
                None => false,
                Some(n) => ctors.len() as u128 >= n,
            },
        }
    }

    /// Return the number of inhabitants of `self`.
    /// `None` represents infinity
    pub fn num_inhabitants(&self) -> Option<u128> {
        match self {
            Constructor::Lit(lit) => lit.num_inhabitants(),
            Constructor::Record(_) => Some(1),
        }
    }

    pub fn as_lit(&self) -> Option<&Lit> {
        match self {
            Constructor::Lit(lit) => Some(lit),
            Constructor::Record(_) => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct PatMatrix<'arena> {
    rows: Vec<PatRow<'arena>>,
    indices: Vec<usize>,
}

impl<'arena> PatMatrix<'arena> {
    pub fn new(rows: Vec<PatRow<'arena>>) -> Self {
        if let Some((first, rows)) = rows.split_first() {
            for row in rows {
                debug_assert_eq!(
                    first.entries.len(),
                    row.entries.len(),
                    "All rows must be same length"
                );
            }
        }
        let indices = (0..rows.len()).collect();
        Self { rows, indices }
    }

    pub fn singleton(scrut: Scrut<'arena>, pat: Pat<'arena>) -> Self {
        Self::new(vec![PatRow::singleton((pat, scrut))])
    }

    pub fn num_rows(&self) -> usize { self.rows.len() }

    pub fn num_columns(&self) -> Option<usize> { self.rows.first().map(PatRow::len) }

    /// Return true if `self` is the null matrix, `∅` - ie `self` has zero rows
    pub fn is_null(&self) -> bool { self.num_rows() == 0 }

    /// Return true if `self` is the unit matrix, `()` - ie `self` has zero
    /// columns and at least one row
    pub fn is_unit(&self) -> bool { self.num_columns() == Some(0) }

    /// Iterate over all the pairs in the `index`th column
    pub fn column(&self, index: usize) -> impl ExactSizeIterator<Item = &RowEntry<'arena>> + '_ {
        self.rows.iter().map(move |row| &row.entries[index])
    }

    pub fn row(&self, index: usize) -> &PatRow<'arena> { &self.rows[index] }

    pub fn row_index(&self, index: usize) -> usize { self.indices[index] }

    /// Collect all the `Constructor`s in the `index`th column
    pub fn column_constructors(&self, index: usize) -> Vec<Constructor<'arena>> {
        let mut ctors = Vec::with_capacity(self.num_rows());
        for (pat, _) in self.column(index) {
            match pat {
                Pat::Error(..) | Pat::Ignore(..) | Pat::Ident(..) => continue,
                Pat::Lit(_, r#const) => ctors.push(Constructor::Lit(*r#const)),
                Pat::RecordLit(_, labels, _) => ctors.push(Constructor::Record(labels)),
            }
        }
        ctors.sort_unstable();
        ctors.dedup();
        ctors
    }

    pub fn iter(&self) -> impl ExactSizeIterator<Item = (&PatRow<'arena>, usize)> {
        self.rows.iter().zip(self.indices.iter().copied())
    }
}

#[derive(Debug, Clone)]
pub struct PatRow<'arena> {
    entries: Vec<RowEntry<'arena>>,
}

impl<'arena> PatRow<'arena> {
    pub fn new(entries: Vec<RowEntry<'arena>>) -> Self { Self { entries } }

    pub fn singleton(entry: RowEntry<'arena>) -> Self { Self::new(vec![entry]) }

    pub fn tail(&self) -> Self {
        assert!(!self.is_empty(), "Cannot take tail of empty `PatRow`");
        Self::new(self.entries[1..].to_vec())
    }

    pub fn len(&self) -> usize { self.entries.len() }

    pub fn is_empty(&self) -> bool { self.entries.is_empty() }

    pub fn first(&self) -> Option<&RowEntry<'arena>> { self.entries.first() }

    pub fn all_wildcards(&self) -> bool { self.entries.iter().all(|(pat, _)| pat.is_wildcard()) }

    pub fn split_first(&self) -> Option<(&RowEntry<'arena>, Self)> {
        let (first, rest) = self.entries.split_first()?;
        Some((first, Self::new(rest.to_vec())))
    }

    pub fn append(&mut self, mut other: Self) { self.entries.append(&mut other.entries); }

    pub fn patterns(&self) -> impl ExactSizeIterator<Item = &Pat<'arena>> {
        self.entries.iter().map(|(pattern, _)| pattern)
    }
}

/// An element in a `PatRow`: `<scrut.expr> is <pat>`.
/// This notation is taken from [How to compile pattern matching]
pub type RowEntry<'arena> = (Pat<'arena>, Scrut<'arena>);

#[derive(Debug, Clone)]
/// The right hand side of a match clause
pub struct Body<'arena> {
    /// The expression to be evaluated
    expr: Expr<'arena>,
    /// The variables to be let-bound before `expr` is evaluated
    defs: Vec<(Option<Symbol>, Scrut<'arena>)>,
}

impl<'arena> Body<'arena> {
    pub fn new(expr: Expr<'arena>, defs: Vec<(Option<Symbol>, Scrut<'arena>)>) -> Self {
        Self { expr, defs }
    }
}

impl<'arena> Pat<'arena> {
    /// Specialise `self` with respect to the constructor `ctor`.
    pub fn specialize<'message>(
        &self,
        ctx: &mut ElabCtx<'arena, 'message>,
        ctor: &Constructor,
        scrut: &Scrut<'arena>,
    ) -> Option<PatRow<'arena>> {
        match self {
            Pat::Error(..) | Pat::Ignore(..) | Pat::Ident(..) => {
                let columns = vec![(Pat::Ignore(self.range()), scrut.clone()); ctor.arity()];
                Some(PatRow::new(columns))
            }
            Pat::Lit(_, lit) if *ctor == Constructor::Lit(*lit) => Some(PatRow::new(vec![])),
            Pat::RecordLit(_, labels, patterns) if *ctor == Constructor::Record(labels) => {
                let mut columns = Vec::with_capacity(labels.len());
                let mut iter = Iterator::zip(labels.iter(), patterns.iter());
                let Type::RecordType(_, mut telescope) = ctx.elim_env().update_metas(&scrut.r#type) else {
                    unreachable!("expected record type")
                };

                while let Some(((label, pattern), (r#type, next_telescope))) = Option::zip(
                    iter.next(),
                    ctx.elim_env().split_telescope(telescope.clone()),
                ) {
                    telescope = next_telescope(ctx.local_env.next_var());
                    let scrut_expr = ctx.arena.alloc(scrut.expr);
                    let scrut_expr = Expr::RecordProj(scrut_expr, *label);
                    let scrut = Scrut {
                        expr: scrut_expr,
                        r#type,
                    };
                    columns.push((*pattern, scrut));
                }
                Some(PatRow::new(columns))
            }
            Pat::Lit(..) | Pat::RecordLit(..) => None,
        }
    }
}

impl<'arena> PatRow<'arena> {
    /// Specialise `self` with respect to the constructor `ctor`.
    pub fn specialize<'message>(
        &self,
        ctx: &mut ElabCtx<'arena, 'message>,
        ctor: &Constructor,
    ) -> Option<PatRow<'arena>> {
        assert!(!self.entries.is_empty(), "Cannot specialize empty `PatRow`");
        let ((pat, scrut), rest) = self.split_first().unwrap();
        let mut row = pat.specialize(ctx, ctor, scrut)?;
        row.append(rest);
        Some(row)
    }
}

impl<'arena> PatMatrix<'arena> {
    /// Specialise `self` with respect to the constructor `ctor`.
    /// This is the `S` function in *Compiling pattern matching to good decision
    /// trees*
    pub fn specialize<'message>(
        &self,
        ctx: &mut ElabCtx<'arena, 'message>,
        ctor: &Constructor,
    ) -> Self {
        let (rows, indices) = self
            .iter()
            .filter_map(|(row, body)| {
                let row = row.specialize(ctx, ctor)?;
                Some((row, body))
            })
            .unzip();
        Self { rows, indices }
    }

    /// Discard the first column, and all rows starting with a constructed
    /// pattern. This is the `D` function in *Compiling pattern matching to
    /// good decision trees*
    pub fn default(&self) -> Self {
        assert!(
            !self.is_unit(),
            "Cannot default `PatMatrix` with no columns"
        );
        let (rows, indices) = self
            .iter()
            .filter_map(|(row, body)| match row.first().unwrap().0 {
                Pat::Error(..) | Pat::Ignore(..) | Pat::Ident(..) => Some((row.tail(), body)),
                Pat::Lit(..) | Pat::RecordLit(..) => None,
            })
            .unzip();
        Self { rows, indices }
    }
}

impl<'arena, 'message> ElabCtx<'arena, 'message> {
    pub fn elab_match(
        &mut self,
        mut matrix: PatMatrix<'arena>,
        bodies: &[Body<'arena>],
        match_range: ByteRange,
        scrut_range: ByteRange,
    ) -> Expr<'arena> {
        debug_assert_eq!(
            matrix.num_rows(),
            bodies.len(),
            "Must have one body for each row"
        );
        coverage::check_coverage(self, &matrix, match_range, scrut_range);
        compile::compile_match(self, &mut matrix, bodies, EnvLen::new())
    }
}
