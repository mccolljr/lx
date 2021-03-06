use super::structs::{
    Ident,
    ObjTypeField,
};

use crate::source::Pos;

use std::fmt::{
    Display,
    Formatter,
    Result as FmtResult,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Any {
        kwany: Pos,
    },
    Int {
        kwint: Pos,
    },
    Float {
        kwfloat: Pos,
    },
    Bool {
        kwbool: Pos,
    },
    Str {
        kwstr: Pos,
    },
    Null {
        kwnull: Pos,
    },
    Nullable {
        question: Pos,
        element:  Box<Type>,
    },
    Named {
        ident: Ident,
    },
    Object {
        obrace: Pos,
        fields: Vec<ObjTypeField>,
        cbrace: Pos,
    },
    Map {
        kwmap:   Pos,
        osquare: Pos,
        element: Box<Type>,
        csquare: Pos,
    },
    Tuple {
        osquare: Pos,
        fields:  Vec<Type>,
        csquare: Pos,
    },
    Array {
        kwarray: Pos,
        osquare: Pos,
        element: Box<Type>,
        csquare: Pos,
    },
    Union {
        alts: Vec<Type>,
    },
    Func {
        kwfn:   Pos,
        oparen: Pos,
        args:   Vec<Type>,
        cparen: Pos,
        colon:  Pos,
        ret:    Box<Type>,
    },
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Type::Any { .. } => write!(f, "any"),
            Type::Int { .. } => write!(f, "int"),
            Type::Float { .. } => write!(f, "float"),
            Type::Bool { .. } => write!(f, "bool"),
            Type::Str { .. } => write!(f, "str"),
            Type::Null { .. } => write!(f, "null"),
            Type::Nullable { element, .. } => write!(f, "?{}", element),
            Type::Named { ident, .. } => write!(f, "{}", ident),
            Type::Object { fields, .. } => {
                write!(f, "{{")?;
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", field)?;
                }
                write!(f, "}}")
            }
            Type::Map { element, .. } => write!(f, "map[{}]", element),
            Type::Tuple { fields, .. } => {
                write!(f, "[")?;
                for (i, elt) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", elt)?;
                }
                write!(f, "]")
            }
            Type::Array { element, .. } => write!(f, "array[{}]", element),
            Type::Union { alts } => {
                for (i, alt) in alts.iter().enumerate() {
                    if i > 0 {
                        write!(f, " | ")?;
                    }
                    write!(f, "{}", alt)?;
                }
                Ok(())
            }
            Type::Func { args, ret, .. } => {
                write!(f, "fn (")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, "): {}", ret)
            }
        }
    }
}
