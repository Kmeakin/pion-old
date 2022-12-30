use std::ops::Deref;

pub const MAX_LENGTH: usize = u32::MAX as usize;

pub struct InputString<'src> {
    string: &'src str,
}

impl<'string> InputString<'string> {
    pub fn new(string: &'string str) -> Result<Self, TooBigError> {
        match string.len() <= MAX_LENGTH {
            true => Ok(Self { string }),
            false => Err(TooBigError {
                actual_len: string.len(),
            }),
        }
    }
}

impl<'string> Deref for InputString<'string> {
    type Target = &'string str;
    fn deref(&self) -> &Self::Target { &self.string }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct TooBigError {
    actual_len: usize,
}
