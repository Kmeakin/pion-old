use std::fmt;

type RawIdx = u32;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub struct EnvLen(RawIdx);

impl EnvLen {
    /// Reset the environment to the empty environment.
    pub fn clear(&mut self) { self.0 = 0; }

    /// Convert `index` to a `Level` in the current environment.
    pub fn index_to_level(self, index: Index) -> Option<Level> {
        Some(Level(self.0.checked_sub(index.0)?.checked_sub(1)?))
    }

    /// Convert `level` to an `Index` in the current environment.
    pub fn level_to_index(self, level: Level) -> Option<Index> {
        Some(Index(self.0.checked_sub(level.0)?.checked_sub(1)?))
    }

    /// The next `Level` that will be bound in this environment.
    pub fn next_level(self) -> Level { Level(self.0) }

    /// Push an element onto the environment.
    pub fn push(&mut self) { self.0 += 1; }

    /// Pop an element off the environment.
    pub fn pop(&mut self) { self.0 -= 1; }

    /// Truncate the environment to `len`.
    pub fn truncate(&mut self, len: EnvLen) { *self = len; }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub struct Index(RawIdx);

impl Index {
    pub fn iter() -> impl Iterator<Item = Self> { (0..).map(Self) }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub struct Level(RawIdx);

impl fmt::Display for Level {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { self.0.fmt(f) }
}

impl Level {
    pub fn iter() -> impl Iterator<Item = Self> { (0..).map(Self) }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UniqueEnv<T> {
    entries: Vec<T>,
}

impl<T> Default for UniqueEnv<T> {
    fn default() -> Self {
        Self {
            entries: Default::default(),
        }
    }
}

impl<T> UniqueEnv<T> {
    /// Clear the renaming. This is useful for reusing environment allocations.
    pub fn clear(&mut self) { self.entries.clear() }

    /// Resize the environment to `len`, filling new entries with `elem`.
    pub fn resize(&mut self, len: EnvLen, elem: T)
    where
        T: Clone,
    {
        self.entries.resize(len.0 as usize, elem)
    }

    /// Push `elem` onto the environment.
    pub fn push(&mut self, elem: T) { self.entries.push(elem) }

    /// Pop an element off the environment.
    pub fn pop(&mut self) -> Option<T> { self.entries.pop() }

    /// Truncate the environment to `len`.
    pub fn truncate(&mut self, len: EnvLen) { self.entries.truncate(len.0 as usize) }

    /// Reserve space for `additional` extra elements.
    pub fn reserve(&mut self, additional: usize) { self.entries.reserve(additional) }
}

impl<T> std::ops::Deref for UniqueEnv<T> {
    type Target = SliceEnv<T>;

    fn deref(&self) -> &SliceEnv<T> { self.entries[..].into() }
}

impl<T> std::ops::DerefMut for UniqueEnv<T> {
    fn deref_mut(&mut self) -> &mut SliceEnv<T> { (&mut self.entries[..]).into() }
}

/// An environment backed by a slice.
#[derive(Debug)]
pub struct SliceEnv<T> {
    entries: [T],
}

impl<T> SliceEnv<T> {
    /// The length of the environment.
    pub fn len(&self) -> EnvLen {
        // SAFETY:
        // - The only way to construct a `SliceEnv` is via `UniqueEnv`. We ensure that
        //   the length of the environment never exceeds the the maximum `RawVar`, so
        //   this should never overflow.
        EnvLen(self.entries.len() as RawIdx)
    }

    /// Lookup an element in the environment using a level.
    pub fn get_level(&self, level: Level) -> Option<&T> { self.entries.get(level.0 as usize) }

    /// Lookup an element in the environment using an index.
    pub fn get_index(&self, index: Index) -> Option<&T> {
        self.get_level(self.len().index_to_level(index)?)
    }

    /// Set an element in the environment using a level
    pub fn set_level(&mut self, level: Level, entry: T) {
        self.entries[(level.0 as usize)] = entry;
    }

    /// Iterate over the elements in the environment.
    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &T> { self.entries.iter() }
}

impl<Entry: PartialEq> SliceEnv<Entry> {
    pub fn leve_of_elem(&self, elem: &Entry) -> Option<Level> {
        Iterator::zip(Level::iter(), self.iter()).find_map(|(var, e)| (elem == e).then_some(var))
    }

    pub fn index_of_elem(&self, elem: &Entry) -> Option<Index> {
        Iterator::zip(Index::iter(), self.iter().rev())
            .find_map(|(var, e)| (elem == e).then_some(var))
    }
}

impl<'a, Entry> From<&'a [Entry]> for &'a SliceEnv<Entry> {
    fn from(entries: &'a [Entry]) -> &'a SliceEnv<Entry> {
        // SAFETY:
        // - `SliceEnv<Entry>` is equivalent to an `[Entry]` internally
        unsafe { std::mem::transmute::<&[_], &SliceEnv<_>>(entries) }
    }
}

impl<'a, Entry> From<&'a mut [Entry]> for &'a mut SliceEnv<Entry> {
    fn from(entries: &'a mut [Entry]) -> &'a mut SliceEnv<Entry> {
        // SAFETY:
        // - `SliceEnv<Entry>` is equivalent to an `[Entry]` internally
        unsafe { std::mem::transmute::<&mut [_], &mut SliceEnv<_>>(entries) }
    }
}
