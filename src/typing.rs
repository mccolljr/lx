use std::cmp::Ordering;
use std::collections::{
    BTreeMap,
    BTreeSet,
};
use std::fmt::{
    Display,
    Formatter,
    Result as FmtResult,
};
use std::hash::{
    Hash,
    Hasher,
};

#[derive(Clone, Debug, PartialEq, Eq, Ord)]
pub enum Typing {
    Any,
    Int,
    Float,
    Bool,
    Str,
    Null,
    Object(BTreeMap<String, Typing>),
    Map(Box<Typing>),
    Tuple(Vec<Typing>),
    Array(Box<Typing>),
    Union(BTreeSet<Typing>),
    Func(
        // argument types
        Vec<Typing>,
        // is last argument variadic?
        bool,
        // return type
        Box<Typing>,
    ),
}

impl Typing {
    pub fn variant(&self) -> u8 {
        #[rustfmt::skip]
        return match self {
            Self::Any        => 0x01,
            Self::Int        => 0x02,
            Self::Float      => 0x03,
            Self::Bool       => 0x04,
            Self::Str        => 0x05,
            Self::Null       => 0x06,
            Self::Object(..) => 0x07,
            Self::Map(..)    => 0x08,
            Self::Tuple(..)  => 0x09,
            Self::Array(..)  => 0x0a,
            Self::Union(..)  => 0x0b,
            Self::Func(..)   => 0x0c,
        };
    }

    pub fn must_object(&self) -> &BTreeMap<String, Typing> {
        match self {
            Self::Object(fields) => fields,
            _ => unreachable!("type is not an object"),
        }
    }

    pub fn must_map(&self) -> &Typing {
        match self {
            Self::Map(element) => element.as_ref(),
            _ => unreachable!("type is not a map"),
        }
    }

    pub fn must_tuple(&self) -> &Vec<Typing> {
        match self {
            Self::Tuple(fields) => fields,
            _ => unreachable!("type is not a tuple"),
        }
    }

    pub fn must_array(&self) -> &Typing {
        match self {
            Self::Array(element) => element.as_ref(),
            _ => unreachable!("type is not an array"),
        }
    }

    pub fn must_union(&self) -> &BTreeSet<Typing> {
        match self {
            Self::Union(alts) => alts,
            _ => unreachable!("type is not a union"),
        }
    }

    pub fn must_func(&self) -> (&Vec<Typing>, &bool, &Typing) {
        match self {
            Self::Func(args, is_variadic, ret) => (args, is_variadic, ret),
            _ => unreachable!("type is not a func"),
        }
    }

    pub fn flatten_union(union: BTreeSet<Self>) -> BTreeSet<Self> {
        let mut alts = BTreeSet::<Self>::new();
        for alt in union {
            match alt {
                Self::Union(subunion) => {
                    Self::flatten_union(subunion).into_iter().for_each(|t| {
                        alts.insert(t);
                    });
                }
                _ => {
                    alts.insert(alt);
                }
            }
        }
        return alts;
    }

    pub fn coalesce(parts: Vec<Self>) -> Self {
        let mut alts = Typing::unionize(parts);
        match alts.len() {
            0 => return Self::Null,
            1 => return alts.pop_first().unwrap(),
            _ => return Self::Union(alts),
        }
    }

    pub fn unionize(elts: impl IntoIterator<Item = Self>) -> BTreeSet<Self> {
        let mut alts = BTreeSet::<Self>::new();
        elts.into_iter().for_each(|t| {
            alts.insert(t);
        });
        Typing::flatten_union(alts)
    }

    pub fn array_of(elt: Self) -> Self { Self::Array(Box::new(elt)) }

    pub fn map_of(elt: Self) -> Self { Self::Map(Box::new(elt)) }

    pub fn union_of(elts: impl IntoIterator<Item = Self>) -> Self {
        let unionized = Typing::unionize(elts);
        if unionized.contains(&Typing::Any) {
            return Typing::Any;
        }
        return Typing::Union(unionized);
    }
}

impl Display for Typing {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            Typing::Any => write!(f, "any"),
            Typing::Int => write!(f, "int"),
            Typing::Float => write!(f, "float"),
            Typing::Bool => write!(f, "bool"),
            Typing::Str => write!(f, "str"),
            Typing::Null => write!(f, "null"),
            Typing::Object(fields) => {
                write!(f, "{{")?;
                for (i, (name, spec)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", name, spec)?;
                }
                write!(f, "}}")
            }
            Typing::Map(elt) => write!(f, "map[{}]", elt),
            Typing::Tuple(fields) => {
                write!(f, "[")?;
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", field)?;
                }
                write!(f, "]")
            }
            Typing::Array(elt) => write!(f, "array[{}]", elt),
            Typing::Func(args, is_variadic, ret) => {
                write!(f, "fn (")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                if *is_variadic {
                    write!(f, "...")?;
                }
                write!(f, "): ")?;
                write!(f, "{}", ret)
            }
            Typing::Union(alts) => {
                for (i, alt) in alts.iter().enumerate() {
                    if i > 0 {
                        write!(f, " | ")?;
                    }
                    write!(f, "{}", alt)?;
                }
                Ok(())
            }
        }
    }
}

impl Hash for Typing {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // always start the hash with the variant
        state.write_u8(self.variant());

        match self {
            Self::Any
            | Self::Int
            | Self::Float
            | Self::Bool
            | Self::Str
            | Self::Null => {
                // these types are simple variants
            }
            Self::Object(fields) => {
                state.write_u8(0x06);
                for (i, (k, v)) in fields.iter().enumerate() {
                    state.write_usize(i);
                    k.hash(state);
                    v.hash(state);
                }
            }
            Self::Map(elt_type) => {
                state.write_u8(0x07);
                elt_type.hash(state);
            }
            Self::Tuple(fields) => {
                state.write_u8(0x08);
                for (i, v) in fields.iter().enumerate() {
                    state.write_usize(i);
                    v.hash(state);
                }
            }
            Self::Array(elt_type) => {
                state.write_u8(0x09);
                elt_type.hash(state);
            }
            Self::Func(args, is_variadic, ret_typ) => {
                state.write_u8(0x0a);
                state.write_usize(args.len());
                state.write_u8(if *is_variadic { 1 } else { 0 });
                for (i, arg) in args.iter().enumerate() {
                    state.write_usize(i);
                    arg.hash(state);
                }
                ret_typ.hash(state)
            }
            Self::Union(alts) => {
                state.write_u8(0x0a);
                state.write_usize(alts.len());
                for (i, v) in alts.iter().enumerate() {
                    state.write_usize(i);
                    v.hash(state);
                }
            }
        }
    }
}

impl PartialOrd for Typing {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let s_var = self.variant();
        let o_var = other.variant();
        if s_var != o_var {
            // order different types by their variant order
            return s_var.partial_cmp(&o_var);
        }
        match self {
            Self::Any
            | Self::Int
            | Self::Float
            | Self::Bool
            | Self::Str
            | Self::Null => {
                // these types are simple variants
                return Some(Ordering::Equal);
            }
            Self::Object(fields) => {
                return fields.partial_cmp(other.must_object());
            }
            Self::Map(elt_type) => {
                return elt_type.as_ref().partial_cmp(other.must_map());
            }
            Self::Tuple(fields) => {
                return fields.partial_cmp(other.must_tuple());
            }
            Self::Array(elt_type) => {
                return elt_type.as_ref().partial_cmp(other.must_array());
            }
            Self::Func(args, is_variadic, ret_typ) => {
                let (other_args, other_is_variadic, other_ret_typ) =
                    other.must_func();
                match args.partial_cmp(other_args) {
                    Some(ord) => {
                        if ord == Ordering::Equal {
                            match is_variadic.partial_cmp(other_is_variadic) {
                                Some(ord) => {
                                    if ord == Ordering::Equal {
                                        return ret_typ
                                            .as_ref()
                                            .partial_cmp(other_ret_typ);
                                    }
                                    return Some(ord);
                                }
                                None => return None,
                            }
                        }
                        return Some(ord);
                    }
                    None => return None,
                }
            }
            Self::Union(alts) => {
                return alts.partial_cmp(other.must_union());
            }
        }
    }
}
