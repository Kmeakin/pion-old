use super::*;
use crate::env::{Level, SliceEnv};

/// Unification context.
pub struct UnifyCtx<'arena, 'env> {
    /// Scoped arena for storing [renamed][Context::rename] exprs.
    scope: &'arena Scope<'arena>,
    /// A renaming that is used when solving metavariables using pattern
    /// unification. We store it in the parent context, re-initialising it on
    /// each call to [`Context::solve`] in order to reuse previous allocations.
    renaming: &'env mut PartialRenaming,
    /// The length of the local environment.
    local_env: EnvLen,
    /// Solutions for metavariables.
    meta_values: &'env mut SliceEnv<Option<Value<'arena>>>,
}

/// A partial renaming from a source environment to a target environment.
#[derive(Default)]
pub struct PartialRenaming {
    /// Mapping from local variables in the source environment to local
    /// variables in the target environment.
    source: UniqueEnv<Option<Level>>,
    /// The length of the target binding environment
    target: EnvLen,
}

impl PartialRenaming {
    /// Create a new, empty renaming.
    pub fn new() -> PartialRenaming {
        PartialRenaming {
            source: UniqueEnv::default(),
            target: EnvLen::default(),
        }
    }

    /// Re-initialise the renaming to the requested `source_len`, reusing the
    /// previous allocation.
    fn init(&mut self, source_len: EnvLen) {
        self.source.clear();
        self.source.resize(source_len, None);
        self.target.clear();
    }

    fn next_local_var<'arena>(&self) -> Value<'arena> {
        Value::local(self.source.len().next_level())
    }

    /// Set a local source variable to local target variable mapping, ensuring
    /// that the variable appears uniquely.
    ///
    /// # Returns
    ///
    /// - `true` if the local binding was set successfully.
    /// - `false` if the local binding was already set.
    fn set_local(&mut self, source_var: Level) -> bool {
        let is_unique = self.get_as_level(source_var).is_none();

        if is_unique {
            let target_var = Some(self.target.next_level());
            self.source.set_level(source_var, target_var);
            self.target.push();
        }

        is_unique
    }

    /// Push an extra local binding onto the renaming.
    fn push_local(&mut self) {
        let target_var = self.target.next_level();
        self.source.push(Some(target_var));
        self.target.push();
    }

    /// Pop a local binding off the renaming.
    fn pop_local(&mut self) {
        self.source.pop();
        self.target.pop();
    }

    /// Get the local variable in the target environment that will be used in
    /// place of the `source_var`.
    fn get_as_level(&self, source_var: Level) -> Option<Level> {
        self.source.get_level(source_var).copied().flatten()
    }

    /// Rename a local variable in the source environment to a local variable in
    /// the target environment.
    fn get_as_index(&self, source_var: Level) -> Option<Index> {
        let target_var = self.get_as_level(source_var)?;
        Some(self.target.level_to_index(target_var).unwrap())
    }

    fn len(&self) -> (EnvLen, EnvLen) { (self.source.len(), self.target) }

    fn truncate(&mut self, (source_len, target_len): (EnvLen, EnvLen)) {
        self.source.truncate(source_len);
        self.target.truncate(target_len);
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Error {
    /// A known part of one value failed to match with a known part of the other
    /// value that we are comparing against.
    //
    // TODO: Return some sort of type-diff
    Mismatch,
    /// An error that was found in the problem spine.
    Spine(SpineError),
    /// An error that occurred when renaming the solution.
    Rename(RenameError),
}

impl From<SpineError> for Error {
    fn from(error: SpineError) -> Error { Error::Spine(error) }
}

impl From<RenameError> for Error {
    fn from(error: RenameError) -> Error { Error::Rename(error) }
}

/// An error that was found in the spine of a unification problem.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum SpineError {
    /// A local variable appeared multiple times in the spine of a unification
    /// problem.
    ///
    /// For example:
    ///
    /// ```text
    /// ?α x x =? x`
    /// ```
    ///
    /// This results in two distinct solutions:
    ///
    /// - `?α := fun x _ => x`
    /// - `?α := fun _ x => x`
    ///
    /// We only want unification to result in a unique solution, so we fail
    /// to unify in this case.
    ///
    /// Another example, assuming `true : Bool`, is:
    ///
    /// ```text
    /// ?α true =? true
    /// ```
    ///
    /// This also has multiple solutions, for example:
    ///
    /// - `?α := fun _ => true`
    /// - `?α := fun x => x`
    /// - `?α := fun x => if x then true else false`
    ///
    /// It's also possible that the return type of `?α` is not always `Bool`,
    /// for example:
    ///
    /// ```text
    /// ?α : fun (b : Bool) -> if b then Bool else (Bool -> Bool)
    /// ```
    ///
    /// In this case the example solution `?α := fun _ => true` is not even
    /// well-typed! In contrast, if the problem spine only has distinct local
    /// variables, even if the return type is dependent, local variables block
    /// all computation in the return type, and the pattern solution is
    /// guaranteed to be well-typed.
    NonLinearSpine(Level),
    /// An eliminator was found in the problem spine which was not a
    /// metavariable.
    NonLocalFunApp,
}

/// An error that occurred when renaming the solution.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum RenameError {
    /// A free local variable in the compared value does not occur in the
    /// problem spine.
    ///
    /// For example, where `z : Type` is a local variable:
    ///
    /// ```text
    /// ?α x y =? z -> z
    /// ```
    ///
    /// There is no solution for this metavariable because `?α` is the
    /// topmost-level scope, so it can only abstract over `x` and `y`, but
    /// these don't occur in `z -> z`.
    EscapingLocalVar(Level),
    /// The metavariable occurs in the value being compared against.
    /// This is sometimes referred to as an 'occurs check' failure.
    ///
    /// For example:
    ///
    /// ```text
    /// ?α =? ?α -> ?α
    /// ```
    ///
    /// Here `?α` occurs in the right hand side, so in order to solve this
    /// metavariable we would end up going into an infinite loop,
    /// attempting to construct larger and larger solutions:
    ///
    /// - `?α =? ?α -> ?α`
    /// - `?α =? (?α -> ?α) -> (?α -> ?α)`
    /// - `?α =? ((?α -> ?α) -> (?α -> ?α)) -> ((?α -> ?α) -> (?α -> ?α))`
    InfiniteSolution,
}

impl<'arena, 'env> UnifyCtx<'arena, 'env> {
    pub fn new(
        scope: &'arena Scope<'arena>,
        renaming: &'env mut PartialRenaming,
        local_env: EnvLen,
        meta_values: &'env mut SliceEnv<Option<Value<'arena>>>,
    ) -> Self {
        Self {
            scope,
            renaming,
            local_env,
            meta_values,
        }
    }

    fn elim_env(&self) -> ElimEnv<'arena, '_> { ElimEnv::new(self.scope, self.meta_values) }

    /// Unify two values, updating the solution environment if necessary.
    pub fn unify(&mut self, value1: &Value<'arena>, value2: &Value<'arena>) -> Result<(), Error> {
        if std::ptr::eq(value1, value2) {
            return Ok(());
        }

        let value1 = self.elim_env().update_metas(value1);
        let value2 = self.elim_env().update_metas(value2);

        match (&value1, &value2) {
            _ if value1.is_error() || value2.is_error() => Ok(()),

            (Value::Lit(lit1), Value::Lit(lit2)) if lit1 == lit2 => Ok(()),

            (Value::Stuck(head1, spine1), Value::Stuck(head2, spine2)) if head1 == head2 => {
                self.unify_spines(spine1, spine2)
            }

            (Value::FunType(_, domain1, body1), Value::FunType(_, domain2, body2))
            | (Value::FunLit(_, domain1, body1), Value::FunLit(_, domain2, body2)) => {
                self.unify(domain1, domain2)?;
                self.unify_closures(body1, body2)
            }

            (Value::FunLit(_, _, body), other) | (other, Value::FunLit(_, _, body)) => {
                self.unify_fun_lit(body, other)
            }

            // One of the values has a metavariable at its head, so we
            // attempt to solve it using pattern unification.
            (Value::Stuck(Head::Meta(var), spine), other)
            | (other, Value::Stuck(Head::Meta(var), spine)) => self.solve(*var, spine, other),

            _ => Err(Error::Mismatch),
        }
    }

    /// Unify two elimination spines.
    fn unify_spines(
        &mut self,
        spine1: &[Elim<'arena>],
        spine2: &[Elim<'arena>],
    ) -> Result<(), Error> {
        if spine1.len() != spine2.len() {
            return Err(Error::Mismatch);
        }

        for (elim1, elim2) in Iterator::zip(spine1.iter(), spine2.iter()) {
            match (elim1, elim2) {
                (Elim::FunApp(arg1), Elim::FunApp(arg2)) => self.unify(arg1, arg2)?,
            }
        }
        Ok(())
    }

    /// Unify two [closures][Closure].
    fn unify_closures(
        &mut self,
        closure1: &Closure<'arena>,
        closure2: &Closure<'arena>,
    ) -> Result<(), Error> {
        let var = Value::local(self.local_env.next_level());

        let value1 = self.elim_env().apply_closure(closure1.clone(), var.clone());
        let value2 = self.elim_env().apply_closure(closure2.clone(), var.clone());

        self.local_env.push();
        let result = self.unify(&value1, &value2);
        self.local_env.pop();

        result
    }

    /// Unify a function literal with a value, using eta-conversion.
    ///
    /// ```fathom
    /// (fun x => f x) = f
    /// ```
    fn unify_fun_lit(
        &mut self,
        body: &Closure<'arena>,
        value: &Value<'arena>,
    ) -> Result<(), Error> {
        let var = Value::local(self.local_env.next_level());
        let value1 = self.elim_env().apply_closure(body.clone(), var.clone());
        let value2 = self.elim_env().fun_app(value.clone(), var.clone());

        self.local_env.push();
        let result = self.unify(&value1, &value2);
        self.local_env.pop();

        result
    }

    /// Solve a pattern unification problem that looks like:
    ///
    /// ```text
    /// ?α spine =? value`
    /// ```
    ///
    /// If successful, the metavariable environment will be updated with a
    /// solution that looks something like:
    ///
    /// ```text
    /// ?α := fun spine => value
    /// ```
    fn solve(
        &mut self,
        meta_var: Level,
        spine: &[Elim<'arena>],
        value: &Value<'arena>,
    ) -> Result<(), Error> {
        self.init_renaming(spine)?;
        let expr = self.rename(meta_var, value)?;
        let fun_expr = self.fun_intros(spine, expr);
        let mut local_values = UniqueEnv::default();
        let solution = self.elim_env().eval_env(&mut local_values).eval(&fun_expr);
        self.meta_values.set_level(meta_var, Some(solution));
        Ok(())
    }

    /// Re-initialise the [`UnificationCtx::renaming`] by mapping the local
    /// variables in the spine to the local variables in the solution. This
    /// can fail if the spine does not contain distinct local variables.
    fn init_renaming(&mut self, spine: &[Elim<'arena>]) -> Result<(), SpineError> {
        self.renaming.init(self.local_env);

        for elim in spine {
            match elim {
                Elim::FunApp(arg) => match self.elim_env().update_metas(arg) {
                    Value::Stuck(Head::Local(source_var), spine)
                        if spine.is_empty() && self.renaming.set_local(source_var) => {}
                    Value::Stuck(Head::Local(source_var), _) => {
                        return Err(SpineError::NonLinearSpine(source_var))
                    }
                    _ => return Err(SpineError::NonLocalFunApp),
                },
            }
        }
        Ok(())
    }

    /// Wrap `expr` in [function literals][Expr::FunLit] that correspond to the
    /// given `spine`.
    fn fun_intros(&self, spine: &[Elim<'arena>], expr: Expr<'arena>) -> Expr<'arena> {
        spine.iter().fold(expr, |expr, elim| match elim {
            Elim::FunApp(..) => Expr::FunLit(None, self.scope.to_scope((Expr::Error, expr))),
        })
    }

    /// Rename `value` to an [`Expr`], while at the same time using the current
    /// renaming to update variable indices, failing if the partial renaming is
    /// not defined (resulting in an [scope error][Error::ScopeError]), and also
    /// checking for occurrences of the `meta_var` (resulting in an [occurs
    /// check error][Error::InfiniteSolution]).
    ///
    /// This allows us to subsequently wrap the returned expr in function
    /// literals, using [`UnificationContext::function_intros`].
    fn rename(
        &mut self,
        meta_var: Level,
        value: &Value<'arena>,
    ) -> Result<Expr<'arena>, RenameError> {
        let value = self.elim_env().update_metas(value);
        match value {
            Value::Lit(lit) => Ok(Expr::Lit(lit)),
            Value::Stuck(head, spine) => {
                let head = match head {
                    Head::Error => Expr::Error,
                    Head::Prim(prim) => Expr::Prim(prim),
                    Head::Local(source_var) => match self.renaming.get_as_index(source_var) {
                        None => return Err(RenameError::EscapingLocalVar(source_var)),
                        Some(target_var) => Expr::Local(target_var),
                    },
                    Head::Meta(var) => match meta_var == var {
                        true => return Err(RenameError::InfiniteSolution),
                        false => Expr::Meta(var),
                    },
                };
                (spine.iter()).try_fold(head, |head, elim| match elim {
                    Elim::FunApp(arg) => {
                        let arg = self.rename(meta_var, arg)?;
                        Ok(Expr::FunApp(self.scope.to_scope((head, arg))))
                    }
                })
            }
            Value::FunType(name, domain, body) => {
                let domain = self.rename(meta_var, domain)?;
                let body = self.rename_closure(meta_var, &body)?;
                Ok(Expr::FunType(name, self.scope.to_scope((domain, body))))
            }
            Value::FunLit(name, domain, body) => {
                let domain = self.rename(meta_var, domain)?;
                let body = self.rename_closure(meta_var, &body)?;
                Ok(Expr::FunLit(name, self.scope.to_scope((domain, body))))
            }
        }
    }

    /// Rename a closure back into an [`Expr`].
    fn rename_closure(
        &mut self,
        meta_var: Level,
        closure: &Closure<'arena>,
    ) -> Result<Expr<'arena>, RenameError> {
        let source_var = self.renaming.next_local_var();
        let value = self.elim_env().apply_closure(closure.clone(), source_var);

        self.renaming.push_local();
        let expr = self.rename(meta_var, &value);
        self.renaming.pop_local();

        expr
    }
}
