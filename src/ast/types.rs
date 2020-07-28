use itertools::sorted;

use super::structs::Ident;

use crate::source::Pos;

use std::collections::HashMap;
use std::fmt::{
    Display,
    Formatter,
    Result as FmtResult,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Any {
        pos: Pos,
    },
    Null {
        question: Pos,
        inner:    Box<Type>,
    },
    Named {
        ident: Ident,
    },
    Object {
        obrace:   Pos,
        elements: HashMap<String, Type>,
        cbrace:   Pos,
    },
    Map {
        kwmap:   Pos,
        osquare: Pos,
        element: Box<Type>,
        csquare: Pos,
    },
    Tuple {
        osquare:  Pos,
        elements: Vec<Type>,
        csquare:  Pos,
    },
    Array {
        kwarray: Pos,
        osquare: Pos,
        element: Box<Type>,
        csquare: Pos,
    },
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Type::Any { .. } => write!(f, "any"),
            Type::Null { inner, .. } => write!(f, "?{}", inner),
            Type::Named { ident, .. } => write!(f, "{}", ident),
            Type::Object { elements, .. } => {
                write!(f, "{{")?;
                for (i, key) in sorted(elements.keys()).enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key, elements.get(key).unwrap())?;
                }
                write!(f, "}}")
            }
            Type::Map { element, .. } => write!(f, "map[{}]", element),
            Type::Tuple { elements, .. } => {
                write!(f, "[")?;
                for (i, elt) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", elt)?;
                }
                write!(f, "]")
            }
            Type::Array { element, .. } => write!(f, "array[{}]", element),
        }
    }
}
