use std::fmt;
use std::ops::Deref;

pub const MAX_LENGTH: usize = u32::MAX as usize;

pub struct InputString {
    string: String,
}

impl InputString {
    pub fn new(string: String) -> Result<Self, TooBigError> {
        match string.len() <= MAX_LENGTH {
            true => Ok(Self { string }),
            false => Err(TooBigError {
                actual_len: string.len(),
            }),
        }
    }
}

impl Deref for InputString {
    type Target = String;
    fn deref(&self) -> &Self::Target { &self.string }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct TooBigError {
    actual_len: usize,
}

impl fmt::Display for TooBigError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Input too long: input must be less than 4GiB (it is {} bytes)",
            self.actual_len
        )
    }
}
