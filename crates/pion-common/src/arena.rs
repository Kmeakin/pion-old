#![allow(clippy::module_name_repetitions)]

use std::sync::Arc;

pub use yoke;
use yoke::trait_hack::YokeTraitHack;
use yoke::{Yoke, Yokeable};

#[derive(Debug)]
pub struct ArenaAnd<T>
where
    T: for<'a> Yokeable<'a>,
{
    yoke: Yoke<T, Arc<bumpalo::Bump>>,
}

impl<T> ArenaAnd<T>
where
    T: for<'a> Yokeable<'a>,
{
    pub const fn new(yoke: Yoke<T, Arc<bumpalo::Bump>>) -> Self { Self { yoke } }

    #[allow(clippy::needless_lifetimes)]
    pub fn get<'a>(&'a self) -> &'a <T as Yokeable<'a>>::Output { self.yoke.get() }
}

impl<T> PartialEq for ArenaAnd<T>
where
    T: for<'a> Yokeable<'a>,
    for<'a> <T as Yokeable<'a>>::Output: PartialEq,
{
    fn eq(&self, other: &Self) -> bool { self.yoke.get() == other.yoke.get() }
}

impl<T> Eq for ArenaAnd<T>
where
    T: for<'a> Yokeable<'a>,
    for<'a> <T as Yokeable<'a>>::Output: Eq,
{
}

impl<T> Clone for ArenaAnd<T>
where
    T: for<'a> Yokeable<'a>,
    for<'a> YokeTraitHack<<T as Yokeable<'a>>::Output>: Clone,
{
    fn clone(&self) -> Self {
        Self {
            yoke: self.yoke.clone(),
        }
    }
}

// TODO: is this safe?
#[allow(clippy::non_send_fields_in_send_ty)]
unsafe impl<T> Send for ArenaAnd<T>
where
    T: for<'a> Yokeable<'a>,
    T: Send,
{
}
unsafe impl<T> Sync for ArenaAnd<T>
where
    T: for<'a> Yokeable<'a>,
    T: Sync,
{
}
