use ustr::{Ustr, UstrMap};

use crate::syntax::Type;

macro_rules! define_prims {
    ($($name:ident => $str:expr),*,) => {
        define_prims!($($name => $str),*);
    };

    ($($name:ident => $str:expr),*) => {
        #[derive(Debug, Copy, Clone, PartialEq, Eq)]
        pub enum Prim {
            $($name),*
        }

        impl Prim {
            pub fn name(&self)-> &'static str {
                match self {
                    $(Self::$name => $str),*
                }
            }
        }

        impl Prim {
            pub const ALL: &[Self] = &[$(Self::$name,)*];
        }

        impl std::str::FromStr for Prim {
                type Err = ();

                fn from_str(s: &str)-> Result<Self, ()> {
                    match s {
                        $($str => Ok(Self::$name),)*
                        _ => Err(()),
                    }
                }
            }
    };
}

define_prims! {
    Type => "Type",
    Bool => "Bool",
    Int => "Int",
}

pub struct PrimEnv<'arena> {
    name_to_prim: UstrMap<(Prim, Type<'arena>)>,
}

impl<'arena> PrimEnv<'arena> {
    fn define_prim(&mut self, prim: Prim, r#type: Type<'arena>) {
        self.name_to_prim
            .insert(Ustr::from(prim.name()), (prim, r#type));
    }

    pub fn new() -> Self {
        let mut this = Self {
            name_to_prim: UstrMap::default(),
        };
        this.name_to_prim.reserve(Prim::ALL.len());

        this.define_prim(Prim::Type, Type::TYPE);
        this.define_prim(Prim::Bool, Type::TYPE);
        this.define_prim(Prim::Int, Type::TYPE);

        this
    }

    pub fn lookup(&self, name: Ustr) -> Option<(Prim, Type<'arena>)> {
        self.name_to_prim.get(&name).cloned()
    }
}

#[cfg(test)]
mod tests {
    use std::mem::size_of;

    use super::*;

    #[test]
    fn prim_size() {
        assert_eq!(size_of::<Prim>(), 1);
    }
}
