use std::fmt;
use std::ops::Range;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct BytePos(u32);

impl fmt::Debug for BytePos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "{}", self.0) }
}
impl fmt::Display for BytePos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "{self:?}") }
}

impl BytePos {
    #[allow(clippy::cast_possible_truncation)]
    pub fn truncate(n: usize) -> Self { Self(n as u32) }
}

impl From<BytePos> for u32 {
    fn from(other: BytePos) -> Self { other.0 }
}
impl From<BytePos> for usize {
    fn from(other: BytePos) -> Self { other.0 as Self }
}
impl From<u32> for BytePos {
    fn from(other: u32) -> Self { Self(other) }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ByteRange {
    start: BytePos,
    end: BytePos,
}

impl ByteRange {
    pub fn new(start: BytePos, end: BytePos) -> Self { Self { start, end } }

    pub fn merge(this: Self, other: Self) -> Self {
        Self::new(
            std::cmp::min(this.start, other.start),
            std::cmp::max(this.end, other.end),
        )
    }
}

impl From<ByteRange> for Range<usize> {
    fn from(range: ByteRange) -> Self { (range.start.into())..(range.end.into()) }
}

impl fmt::Debug for ByteRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl fmt::Display for ByteRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "{self:?}") }
}
