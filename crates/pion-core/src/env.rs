use std::fmt;
use std::ops::Add;
use std::rc::Rc;

type RawIdx = usize;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub struct EnvLen(RawIdx);

impl EnvLen {
    pub fn new() -> Self { Self(0) }

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
    pub fn to_level(self) -> Level { Level(self.0) }

    /// Push an element onto the environment.
    pub fn push(&mut self) { self.0 += 1; }

    /// Pop an element off the environment.
    pub fn pop(&mut self) { self.0 -= 1; }

    /// Truncate the environment to `len`.
    pub fn truncate(&mut self, len: Self) { *self = len; }

    pub fn iter() -> impl Iterator<Item = Self> { (0..).map(Self) }
}

impl PartialEq<Index> for EnvLen {
    fn eq(&self, other: &Index) -> bool { self.0 == other.0 }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct Index(RawIdx);

impl Index {
    pub fn new() -> Self { Self(0) }

    pub fn iter() -> impl Iterator<Item = Self> { (0..).map(Self) }

    pub fn iter_from(self) -> impl Iterator<Item = Self> { (self.0..).map(Self) }

    pub fn next(self) -> Self { Self(self.0 + 1) }
}

impl Add<EnvLen> for Index {
    type Output = Self;
    fn add(self, rhs: EnvLen) -> Self::Output { Self(self.0 + rhs.0) }
}

impl PartialEq<EnvLen> for Index {
    fn eq(&self, other: &EnvLen) -> bool { self.0 == other.0 }
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
            entries: Vec::default(),
        }
    }
}

impl<T> UniqueEnv<T> {
    /// Clear the environment. This is useful for reusing environment
    /// allocations.
    pub fn clear(&mut self) { self.entries.clear() }

    /// Resize the environment to `len`, filling new entries with `elem`.
    pub fn resize(&mut self, len: EnvLen, elem: T)
    where
        T: Clone,
    {
        self.entries.resize(len.0, elem);
    }

    /// Push `elem` onto the environment.
    pub fn push(&mut self, elem: T) { self.entries.push(elem) }

    /// Pop an element off the environment.
    pub fn pop(&mut self) -> Option<T> { self.entries.pop() }

    /// Truncate the environment to `len`.
    pub fn truncate(&mut self, len: EnvLen) { self.entries.truncate(len.0) }

    /// Reserve space for `additional` extra elements.
    pub fn reserve(&mut self, additional: usize) { self.entries.reserve(additional) }

    pub fn as_slice(&self) -> &[T] { &self.entries }
}

impl<T> std::ops::Deref for UniqueEnv<T> {
    type Target = SliceEnv<T>;

    fn deref(&self) -> &SliceEnv<T> { self.entries[..].into() }
}

impl<T> std::ops::DerefMut for UniqueEnv<T> {
    fn deref_mut(&mut self) -> &mut SliceEnv<T> { (&mut self.entries[..]).into() }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SharedEnv<T> {
    entries: Rc<Vec<T>>,
}

impl<T> Default for SharedEnv<T> {
    fn default() -> Self {
        Self {
            entries: Rc::new(Vec::default()),
        }
    }
}

impl<T: Clone> SharedEnv<T> {
    /// Clear the environment. This is useful for reusing environment
    /// allocations.
    pub fn clear(&mut self) { Rc::make_mut(&mut self.entries).clear() }

    /// Resize the environment to `len`, filling new entries with `elem`.
    pub fn resize(&mut self, len: EnvLen, elem: T) {
        Rc::make_mut(&mut self.entries).resize(len.0, elem);
    }

    /// Push `elem` onto the environment.
    pub fn push(&mut self, elem: T) { Rc::make_mut(&mut self.entries).push(elem) }

    /// Pop an element off the environment.
    pub fn pop(&mut self) -> Option<T> { Rc::make_mut(&mut self.entries).pop() }

    /// Truncate the environment to `len`.
    pub fn truncate(&mut self, len: EnvLen) { Rc::make_mut(&mut self.entries).truncate(len.0) }

    /// Reserve space for `additional` extra elements.
    pub fn reserve(&mut self, additional: usize) {
        Rc::make_mut(&mut self.entries).reserve(additional);
    }
}

impl<T> std::ops::Deref for SharedEnv<T> {
    type Target = SliceEnv<T>;

    fn deref(&self) -> &SliceEnv<T> { self.entries[..].into() }
}

impl<T: Clone> std::ops::DerefMut for SharedEnv<T> {
    fn deref_mut(&mut self) -> &mut SliceEnv<T> {
        (&mut Rc::make_mut(&mut self.entries)[..]).into()
    }
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
    pub fn get_level(&self, level: Level) -> Option<&T> { self.entries.get(level.0) }

    /// Lookup an element in the environment using an index.
    pub fn get_index(&self, index: Index) -> Option<&T> {
        self.get_level(self.len().index_to_level(index)?)
    }

    /// Set an element in the environment using a level
    pub fn set_level(&mut self, level: Level, entry: T) { self.entries[level.0] = entry; }

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
        unsafe { &*(entries as *const [Entry] as *const SliceEnv<Entry>) }
    }
}

impl<'a, Entry> From<&'a mut [Entry]> for &'a mut SliceEnv<Entry> {
    fn from(entries: &'a mut [Entry]) -> &'a mut SliceEnv<Entry> {
        // SAFETY:
        // - `SliceEnv<Entry>` is equivalent to an `[Entry]` internally
        unsafe { &mut *(entries as *mut [Entry] as *mut SliceEnv<Entry>) }
    }
}
