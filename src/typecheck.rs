#![allow(unused_imports, dead_code, unused_variables)]

use itertools;

use crate::ast::{
    Expr,
    FnArg,
    Ident,
    LetDeclTarget,
    ObjDestructItem,
    ObjKey,
    ObjLitField,
    ObjTypeField,
    Stmt,
    Type,
    TypeAnnotation,
};
use crate::token::TokenType;

use std::cell::RefCell;
use std::cmp::{
    Ord,
    Ordering,
    PartialOrd,
};
use std::collections::{
    BTreeMap,
    BTreeSet,
};
use std::hash::{
    Hash,
    Hasher,
};
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord)]
struct ArgType {
    t:        Box<TypeSpec>,
    variadic: bool,
}

#[derive(Clone, Debug, PartialEq, Eq, Ord)]
enum TypeSpec {
    Any,
    Int,
    Float,
    Bool,
    Str,
    Null,
    Object(BTreeMap<String, TypeSpec>),
    Map(Box<TypeSpec>),
    Tuple(Vec<TypeSpec>),
    Array(Box<TypeSpec>),
    Union(BTreeSet<TypeSpec>),
    Func(Vec<ArgType>, Box<TypeSpec>),
}

impl TypeSpec {
    fn variant(&self) -> u8 {
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

    fn must_object(&self) -> &BTreeMap<String, TypeSpec> {
        match self {
            Self::Object(fields) => fields,
            _ => unreachable!("type is not an object"),
        }
    }

    fn must_map(&self) -> &TypeSpec {
        match self {
            Self::Map(element) => element.as_ref(),
            _ => unreachable!("type is not a map"),
        }
    }

    fn must_tuple(&self) -> &Vec<TypeSpec> {
        match self {
            Self::Tuple(fields) => fields,
            _ => unreachable!("type is not a tuple"),
        }
    }

    fn must_array(&self) -> &TypeSpec {
        match self {
            Self::Array(element) => element.as_ref(),
            _ => unreachable!("type is not an array"),
        }
    }

    fn must_union(&self) -> &BTreeSet<TypeSpec> {
        match self {
            Self::Union(alts) => alts,
            _ => unreachable!("type is not a union"),
        }
    }

    fn must_func(&self) -> (&Vec<ArgType>, &TypeSpec) {
        match self {
            Self::Func(args, ret) => (args, ret),
            _ => unreachable!("type is not a func"),
        }
    }

    fn flatten_union(union: BTreeSet<Self>) -> BTreeSet<Self> {
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

    fn coalesce(parts: Vec<Self>) -> Self {
        let mut alts = TypeSpec::unionize(parts);
        match alts.len() {
            0 => return Self::Null,
            1 => return alts.pop_first().unwrap(),
            _ => return Self::Union(alts),
        }
    }

    fn unionize(elts: impl IntoIterator<Item = Self>) -> BTreeSet<Self> {
        let mut alts = BTreeSet::<Self>::new();
        elts.into_iter().for_each(|t| {
            alts.insert(t);
        });
        TypeSpec::flatten_union(alts)
    }

    fn array_of(elt: Self) -> Self { Self::Array(Box::new(elt)) }

    fn map_of(elt: Self) -> Self { Self::Map(Box::new(elt)) }

    fn union_of(elts: impl IntoIterator<Item = Self>) -> Self {
        TypeSpec::Union(TypeSpec::unionize(elts))
    }
}

impl Hash for TypeSpec {
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
            Self::Func(args, ret_typ) => {
                state.write_u8(0x0a);
                state.write_usize(args.len());
                for (i, arg) in args.iter().enumerate() {
                    state.write_usize(i);
                    state.write_u8(if arg.variadic { 1 } else { 0 });
                    arg.t.hash(state);
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

impl PartialOrd for TypeSpec {
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
            Self::Func(args, ret_typ) => {
                let (other_args, other_ret_typ) = other.must_func();
                match args.partial_cmp(other_args) {
                    Some(ord) => {
                        if ord == Ordering::Equal {
                            return ret_typ.as_ref().partial_cmp(other_ret_typ);
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

struct Scope {
    parent:     Option<Rc<Scope>>,
    decls:      RefCell<BTreeMap<String, TypeSpec>>,
    type_decls: RefCell<BTreeMap<String, TypeSpec>>,
    returns:    Option<TypeSpec>,
}

impl Scope {
    fn new() -> Rc<Self> {
        Rc::new(Scope {
            parent:     None,
            decls:      RefCell::new(BTreeMap::new()),
            type_decls: RefCell::new(BTreeMap::new()),
            returns:    None,
        })
    }

    fn extend(self: &Rc<Self>) -> Rc<Self> {
        Rc::new(Scope {
            parent:     Some(Rc::clone(self)),
            decls:      RefCell::new(BTreeMap::new()),
            type_decls: RefCell::new(BTreeMap::new()),
            returns:    None,
        })
    }

    fn extend_return(self: &Rc<Self>, returns: TypeSpec) -> Rc<Self> {
        Rc::new(Scope {
            parent:     Some(Rc::clone(self)),
            decls:      RefCell::new(BTreeMap::new()),
            type_decls: RefCell::new(BTreeMap::new()),
            returns:    Some(returns),
        })
    }

    fn declare_type(self: &Rc<Self>, name: String, spec: TypeSpec) {
        if self.type_decls.borrow().get(&name).is_some() {
            unreachable!("type checker encountered redeclared type");
        }
        self.type_decls.borrow_mut().insert(name, spec);
    }

    fn lookup_type(self: &Rc<Self>, name: &String) -> TypeSpec {
        if let Some(typespec) = self.type_decls.borrow().get(name) {
            return typespec.clone();
        }
        if let Some(parent) = self.parent.as_ref() {
            return parent.lookup_type(name);
        }
        unreachable!("type checker encountered undeclared type");
    }

    fn declare_var(self: &Rc<Self>, name: String, spec: TypeSpec) {
        if self.decls.borrow().get(&name).is_some() {
            unreachable!("type checker encountered redeclared variable");
        }
        self.decls.borrow_mut().insert(name, spec);
    }

    fn lookup_var(self: &Rc<Self>, name: &String) -> TypeSpec {
        if let Some(typespec) = self.decls.borrow().get(name) {
            return typespec.clone();
        }
        if let Some(parent) = self.parent.as_ref() {
            return parent.lookup_var(name);
        }
        unreachable!("type checker encountered undeclared variable");
    }
}

struct Checker {
    scope: Rc<Scope>,
}

impl Checker {
    fn new() -> Self {
        Checker {
            scope: Scope::new(),
        }
    }

    fn open_scope(&mut self) { self.scope = Scope::extend(&self.scope); }

    fn open_fn_scope(&mut self, returns: TypeSpec) {
        self.scope = Scope::extend_return(&self.scope, returns);
    }

    fn close_scope(&mut self) {
        self.scope = self.scope.parent.as_ref().unwrap().clone()
    }

    fn from_ast(&mut self, src: &Type) -> TypeSpec {
        match src {
            Type::Any { .. } => TypeSpec::Any,
            Type::Int { .. } => TypeSpec::Int,
            Type::Float { .. } => TypeSpec::Float,
            Type::Bool { .. } => TypeSpec::Bool,
            Type::Str { .. } => TypeSpec::Str,
            Type::Null { .. } => TypeSpec::Null,
            Type::Nullable { element, .. } => {
                TypeSpec::union_of(vec![
                    TypeSpec::Null,
                    TypeSpec::coalesce(vec![self.from_ast(element.as_ref())]),
                ])
            }
            Type::Named { ident } => self.scope.lookup_type(&ident.name),
            Type::Array { element, .. } => {
                TypeSpec::array_of(self.from_ast(element.as_ref()))
            }
            Type::Map { element, .. } => {
                TypeSpec::map_of(self.from_ast(element.as_ref()))
            }
            Type::Object { fields, .. } => {
                TypeSpec::Object(
                    fields
                        .iter()
                        .fold(RefCell::new(BTreeMap::new()), |iter, f| {
                            iter.borrow_mut().insert(
                                f.key.name.clone(),
                                self.from_ast(f.typ.as_ref()),
                            );
                            iter
                        })
                        .into_inner(),
                )
            }
            Type::Tuple { fields, .. } => {
                TypeSpec::Tuple(
                    fields.iter().map(|t| self.from_ast(t)).collect(),
                )
            }
            Type::Func { args, ret, .. } => {
                TypeSpec::Func(
                    args.iter()
                        .map(|a| {
                            ArgType {
                                t:        Box::new(self.from_ast(a)),
                                variadic: false,
                            }
                        })
                        .collect(),
                    Box::new(self.from_ast(ret.as_ref())),
                )
            }
            Type::Union { alts, .. } => {
                TypeSpec::union_of(alts.iter().map(|t| self.from_ast(t)))
            }
        }
    }

    fn check_assign(
        &mut self,
        src: &TypeSpec,
        target: &TypeSpec,
    ) -> Result<(), ()> {
        if src == target {
            // A type is always assignable to itself
            return Ok(());
        }

        if src == &TypeSpec::Any || target == &TypeSpec::Any {
            // Any is assignable to and from any other type
            // (used to escape type safety)
            return Ok(());
        }

        match (src, target) {
            (TypeSpec::Object(src_fields), TypeSpec::Map(target_elt_type)) => {
                for src_field_typ in src_fields.values() {
                    self.check_assign(src_field_typ, target_elt_type)?;
                }
                return Ok(());
            }
            (TypeSpec::Object(src_fields), TypeSpec::Object(target_fields)) => {
                for (target_name, target_type) in target_fields {
                    let src_type =
                        src_fields.get(target_name).unwrap_or(&TypeSpec::Null);
                    self.check_assign(src_type, target_type)?;
                }
                return Ok(());
            }
            (TypeSpec::Tuple(src_fields), TypeSpec::Array(target_elt_type)) => {
                for src_field_typ in src_fields {
                    self.check_assign(src_field_typ, target_elt_type)?;
                }
                return Ok(());
            }
            (TypeSpec::Tuple(src_fields), TypeSpec::Tuple(target_fields)) => {
                for (i, target_type) in itertools::enumerate(target_fields) {
                    let src_type = src_fields.get(i).unwrap_or(&TypeSpec::Null);
                    self.check_assign(src_type, target_type)?;
                }
                return Ok(());
            }
            (src_type, TypeSpec::Union(alts)) => {
                for alt in alts {
                    if self.check_assign(src_type, alt).is_ok() {
                        return Ok(());
                    }
                }
                return Err(());
            }
            (
                TypeSpec::Func(src_args, src_ret),
                TypeSpec::Func(target_args, target_ret),
            ) => {
                if src_args.len() != target_args.len() {
                    return Err(());
                }
                self.check_assign(src_ret, target_ret)?;
                for (i, target_arg) in itertools::enumerate(target_args) {
                    // We need to check that the target argument type
                    // is assignable to the source argument type, since
                    // the values passed in by the eventual caller will
                    // conform to the target argument type, not the
                    // source argument type
                    let src_arg = src_args.get(i).unwrap();
                    self.check_assign(&target_arg.t, &src_arg.t)?;
                    if src_arg.variadic != target_arg.variadic {
                        return Err(());
                    }
                }
                return Ok(());
            }
            _ => return Err(()),
        }
    }

    fn check_op_add(
        &mut self,
        lhs: &TypeSpec,
        rhs: &TypeSpec,
    ) -> Result<TypeSpec, ()> {
        match (lhs, rhs) {
            (TypeSpec::Int, TypeSpec::Int) => Ok(TypeSpec::Int),
            (TypeSpec::Int, TypeSpec::Float) => Ok(TypeSpec::Float),
            (TypeSpec::Float, TypeSpec::Int) => Ok(TypeSpec::Float),
            (TypeSpec::Float, TypeSpec::Float) => Ok(TypeSpec::Float),
            (TypeSpec::Str, TypeSpec::Str) => Ok(TypeSpec::Str),
            (TypeSpec::Array(arr_lhs), TypeSpec::Array(arr_rhs)) => {
                Ok(TypeSpec::array_of(TypeSpec::coalesce(vec![
                    *arr_lhs.clone(),
                    *arr_rhs.clone(),
                ])))
            }
            (TypeSpec::Array(arr_lhs), TypeSpec::Tuple(tup_rhs)) => {
                Ok(TypeSpec::array_of(TypeSpec::coalesce(
                    tup_rhs
                        .iter()
                        .map(Clone::clone)
                        .chain(vec![*arr_lhs.clone()])
                        .collect(),
                )))
            }
            (TypeSpec::Tuple(tup_lhs), TypeSpec::Tuple(tup_rhs)) => {
                Ok(TypeSpec::array_of(TypeSpec::coalesce(
                    tup_lhs
                        .iter()
                        .map(Clone::clone)
                        .chain(tup_rhs.iter().map(Clone::clone))
                        .collect(),
                )))
            }
            _ => Err(()),
        }
    }

    fn check_op_sub(
        &mut self,
        lhs: &TypeSpec,
        rhs: &TypeSpec,
    ) -> Result<TypeSpec, ()> {
        match (lhs, rhs) {
            (TypeSpec::Int, TypeSpec::Int) => Ok(TypeSpec::Int),
            (TypeSpec::Int, TypeSpec::Float) => Ok(TypeSpec::Float),
            (TypeSpec::Float, TypeSpec::Int) => Ok(TypeSpec::Float),
            (TypeSpec::Float, TypeSpec::Float) => Ok(TypeSpec::Float),
            _ => Err(()),
        }
    }

    fn check_op_mul(
        &mut self,
        lhs: &TypeSpec,
        rhs: &TypeSpec,
    ) -> Result<TypeSpec, ()> {
        match (lhs, rhs) {
            (TypeSpec::Int, TypeSpec::Int) => Ok(TypeSpec::Int),
            (TypeSpec::Int, TypeSpec::Float) => Ok(TypeSpec::Float),
            (TypeSpec::Float, TypeSpec::Int) => Ok(TypeSpec::Float),
            (TypeSpec::Float, TypeSpec::Float) => Ok(TypeSpec::Float),
            _ => Err(()),
        }
    }

    fn check_op_div(
        &mut self,
        lhs: &TypeSpec,
        rhs: &TypeSpec,
    ) -> Result<TypeSpec, ()> {
        match (lhs, rhs) {
            (TypeSpec::Int, TypeSpec::Int) => Ok(TypeSpec::Int),
            (TypeSpec::Int, TypeSpec::Float) => Ok(TypeSpec::Float),
            (TypeSpec::Float, TypeSpec::Int) => Ok(TypeSpec::Float),
            (TypeSpec::Float, TypeSpec::Float) => Ok(TypeSpec::Float),
            _ => Err(()),
        }
    }

    fn check_op_rem(
        &mut self,
        lhs: &TypeSpec,
        rhs: &TypeSpec,
    ) -> Result<TypeSpec, ()> {
        match (lhs, rhs) {
            (TypeSpec::Int, TypeSpec::Int) => Ok(TypeSpec::Int),
            (TypeSpec::Int, TypeSpec::Float) => Ok(TypeSpec::Float),
            (TypeSpec::Float, TypeSpec::Int) => Ok(TypeSpec::Float),
            (TypeSpec::Float, TypeSpec::Float) => Ok(TypeSpec::Float),
            _ => Err(()),
        }
    }

    fn check_op_eq(
        &mut self,
        lhs: &TypeSpec,
        rhs: &TypeSpec,
    ) -> Result<TypeSpec, ()> {
        // TODO: catch cases where types ensure result will always be false
        Ok(TypeSpec::Bool)
    }

    fn check_op_neq(
        &mut self,
        lhs: &TypeSpec,
        rhs: &TypeSpec,
    ) -> Result<TypeSpec, ()> {
        // TODO: catch cases where types ensure result will always be true
        Ok(TypeSpec::Bool)
    }

    fn check_op_gt(
        &mut self,
        lhs: &TypeSpec,
        rhs: &TypeSpec,
    ) -> Result<TypeSpec, ()> {
        match (lhs, rhs) {
            (TypeSpec::Int, TypeSpec::Int) => Ok(TypeSpec::Int),
            (TypeSpec::Int, TypeSpec::Float) => Ok(TypeSpec::Float),
            (TypeSpec::Float, TypeSpec::Int) => Ok(TypeSpec::Float),
            (TypeSpec::Float, TypeSpec::Float) => Ok(TypeSpec::Float),
            _ => Err(()),
        }
    }

    fn check_op_lt(
        &mut self,
        lhs: &TypeSpec,
        rhs: &TypeSpec,
    ) -> Result<TypeSpec, ()> {
        match (lhs, rhs) {
            (TypeSpec::Int, TypeSpec::Int) => Ok(TypeSpec::Int),
            (TypeSpec::Int, TypeSpec::Float) => Ok(TypeSpec::Float),
            (TypeSpec::Float, TypeSpec::Int) => Ok(TypeSpec::Float),
            (TypeSpec::Float, TypeSpec::Float) => Ok(TypeSpec::Float),
            _ => Err(()),
        }
    }

    fn check_op_geq(
        &mut self,
        lhs: &TypeSpec,
        rhs: &TypeSpec,
    ) -> Result<TypeSpec, ()> {
        match (lhs, rhs) {
            (TypeSpec::Int, TypeSpec::Int) => Ok(TypeSpec::Int),
            (TypeSpec::Int, TypeSpec::Float) => Ok(TypeSpec::Float),
            (TypeSpec::Float, TypeSpec::Int) => Ok(TypeSpec::Float),
            (TypeSpec::Float, TypeSpec::Float) => Ok(TypeSpec::Float),
            _ => Err(()),
        }
    }

    fn check_op_leq(
        &mut self,
        lhs: &TypeSpec,
        rhs: &TypeSpec,
    ) -> Result<TypeSpec, ()> {
        match (lhs, rhs) {
            (TypeSpec::Int, TypeSpec::Int) => Ok(TypeSpec::Int),
            (TypeSpec::Int, TypeSpec::Float) => Ok(TypeSpec::Float),
            (TypeSpec::Float, TypeSpec::Int) => Ok(TypeSpec::Float),
            (TypeSpec::Float, TypeSpec::Float) => Ok(TypeSpec::Float),
            _ => Err(()),
        }
    }

    fn check_binary_expr(
        &mut self,
        lhs: &TypeSpec,
        rhs: &TypeSpec,
        op_typ: TokenType,
    ) -> Result<TypeSpec, ()> {
        match op_typ {
            TokenType::OpAdd => self.check_op_add(lhs, rhs),
            TokenType::OpSub => self.check_op_sub(lhs, rhs),
            TokenType::OpMul => self.check_op_mul(lhs, rhs),
            TokenType::OpDiv => self.check_op_div(lhs, rhs),
            TokenType::OpRem => self.check_op_rem(lhs, rhs),
            TokenType::OpEq => self.check_op_eq(lhs, rhs),
            TokenType::OpNeq => self.check_op_neq(lhs, rhs),
            TokenType::OpGt => self.check_op_gt(lhs, rhs),
            TokenType::OpLt => self.check_op_lt(lhs, rhs),
            TokenType::OpGeq => self.check_op_geq(lhs, rhs),
            TokenType::OpLeq => self.check_op_leq(lhs, rhs),
            _ => Err(()),
        }
    }

    fn check_unary_not(&mut self, _: &TypeSpec) -> Result<TypeSpec, ()> {
        // TODO: check for cases where type will always return true
        Ok(TypeSpec::Bool)
    }

    fn check_unary_negative(
        &mut self,
        item: &TypeSpec,
    ) -> Result<TypeSpec, ()> {
        match item {
            TypeSpec::Int => Ok(TypeSpec::Int),
            TypeSpec::Float => Ok(TypeSpec::Float),
            _ => Err(()),
        }
    }

    fn check_unary_expr(
        &mut self,
        item: &TypeSpec,
        op_typ: TokenType,
    ) -> Result<TypeSpec, ()> {
        match op_typ {
            TokenType::Bang => self.check_unary_not(item),
            TokenType::OpSub => self.check_unary_negative(item),
            _ => Err(()),
        }
    }

    fn check_stmt(&mut self, stmt: &Stmt) -> Result<(), ()> {
        match stmt {
            Stmt::LetDecl {
                target,
                expr,
                annotation,
                ..
            } => {
                return self.check_let_decl(target, expr, annotation);
            }
            Stmt::FnDecl {
                name,
                args,
                ret_typ,
                body,
                ..
            } => {
                return self.check_fn_decl(name, args, ret_typ, body);
            }
            _ => return Err(()),
        }
    }

    fn check_let_decl(
        &mut self,
        target: &LetDeclTarget,
        expr: &Expr,
        annotation: &Option<TypeAnnotation>,
    ) -> Result<(), ()> {
        let mut expr_typespec = self.check_expr(expr)?;
        if let Some(annotation) = annotation {
            let annotation_typespec = self.from_ast(&annotation.typ);
            self.check_assign(&expr_typespec, &annotation_typespec)?;
            expr_typespec = annotation_typespec;
        }
        match target {
            LetDeclTarget::Ident(ident) => {
                self.scope.declare_var(ident.name.clone(), expr_typespec);
                return Ok(());
            }
            LetDeclTarget::ArrDestruct(fields) => {
                match expr_typespec {
                    TypeSpec::Tuple(tup_fields) => {
                        for (i, name) in itertools::enumerate(fields) {
                            if let Some(src_typespec) = tup_fields.get(i) {
                                self.scope.declare_var(
                                    name.clone(),
                                    src_typespec.clone(),
                                );
                            } else {
                                return Err(());
                            }
                        }
                        return Ok(());
                    }
                    TypeSpec::Array(elt_typ) => {
                        for (i, name) in itertools::enumerate(fields) {
                            self.scope.declare_var(
                                name.clone(),
                                TypeSpec::union_of(itertools::cloned(&[
                                    TypeSpec::Null,
                                    (*elt_typ).clone(),
                                ])),
                            );
                        }
                        return Ok(());
                    }
                    _ => return Err(()),
                }
            }
            LetDeclTarget::ObjDestruct(fields) => {
                match expr_typespec {
                    TypeSpec::Object(obj_fields) => {
                        for destructure_item in fields {
                            match destructure_item {
                                ObjDestructItem::Name(ident) => {
                                    if let Some(src_typespec) =
                                        obj_fields.get(&ident.name)
                                    {
                                        self.scope.declare_var(
                                            ident.name.clone(),
                                            src_typespec.clone(),
                                        );
                                    } else {
                                        return Err(());
                                    }
                                }
                                ObjDestructItem::NameMap(key, ident) => {
                                    if let Some(src_typespec) =
                                        obj_fields.get(key)
                                    {
                                        self.scope.declare_var(
                                            ident.name.clone(),
                                            src_typespec.clone(),
                                        );
                                    } else {
                                        return Err(());
                                    }
                                }
                            }
                        }
                        return Ok(());
                    }
                    TypeSpec::Map(elt_type) => {
                        for destructure_item in fields {
                            let ident = match destructure_item {
                                ObjDestructItem::Name(ident) => ident,
                                ObjDestructItem::NameMap(_, ident) => ident,
                            };
                            self.scope.declare_var(
                                ident.name.clone(),
                                TypeSpec::union_of(itertools::cloned(&[
                                    TypeSpec::Null,
                                    (*elt_type).clone(),
                                ])),
                            );
                        }
                        return Ok(());
                    }
                    _ => return Err(()),
                }
            }
        }
    }

    fn check_fn_decl(
        &mut self,
        name: &Ident,
        args: &Vec<FnArg>,
        ret_typ: &Option<TypeAnnotation>,
        body: &Vec<Stmt>,
    ) -> Result<(), ()> {
        let arg_typespecs: Vec<TypeSpec> = args
            .iter()
            .map(|a| {
                if let Some(annotation) = a.typ.as_ref() {
                    self.from_ast(&annotation.typ)
                } else {
                    TypeSpec::Any
                }
            })
            .collect();
        let ret_typespec = ret_typ
            .as_ref()
            .map_or(TypeSpec::Any, |annotation| self.from_ast(&annotation.typ));
        let fn_typespec = TypeSpec::Func(
            arg_typespecs
                .iter()
                .map(|spec| {
                    ArgType {
                        t:        Box::new(spec.clone()),
                        variadic: false,
                    }
                })
                .collect(),
            Box::new(ret_typespec.clone()),
        );
        self.scope.declare_var(name.name.clone(), fn_typespec);
        self.open_fn_scope(ret_typespec.clone());
        args.iter().enumerate().for_each(|(i, arg)| {
            self.scope
                .declare_var(arg.name.clone(), arg_typespecs[i].clone())
        });
        for stmt in body {
            self.check_stmt(stmt)?;
        }
        self.close_scope();
        return Ok(());
    }

    fn check_expr(&mut self, expr: &Expr) -> Result<TypeSpec, ()> {
        match expr {
            Expr::LitInt { .. } => Ok(TypeSpec::Int),
            Expr::LitFlt { .. } => Ok(TypeSpec::Float),
            Expr::LitBool { .. } => Ok(TypeSpec::Bool),
            Expr::LitStr { .. } => Ok(TypeSpec::Str),
            Expr::LitNull { .. } => Ok(TypeSpec::Null),
            Expr::LitArr { elements, .. } => {
                Ok(TypeSpec::Tuple(itertools::process_results(
                    elements.iter().map(|e| self.check_expr(e)),
                    |t| t.collect(),
                )?))
            }
            Expr::Ident(ident) => Ok(self.scope.lookup_var(&ident.name)),
            Expr::Import { .. } => todo!("type checking of imported files"),
            Expr::LitObj { fields, .. } => {
                let mut type_fields = BTreeMap::<String, TypeSpec>::new();
                let mut field_types = Vec::<TypeSpec>::new();
                let mut can_obj = true;
                for field in fields {
                    let field_type = self.check_expr(field.val.as_ref())?;
                    field_types.push(field_type.clone());
                    if can_obj {
                        if let ObjKey::Static(ref name, ..) = field.key {
                            if type_fields
                                .insert(name.clone(), field_type)
                                .is_some()
                            {
                                return Err(()); // TODO
                            }
                        } else {
                            can_obj = false
                        }
                    }
                }
                if can_obj {
                    Ok(TypeSpec::Object(type_fields))
                } else {
                    Ok(TypeSpec::map_of(TypeSpec::coalesce(field_types)))
                }
            }
            _ => Err(()),
        }
    }

    fn resolve(&mut self, path: String) {}
}

#[cfg(test)]
mod test {
    extern crate proc_macro;
    use super::{
        BTreeMap,
        BTreeSet,
        TypeSpec,
    };
    use itertools;

    #[test]
    fn test_variant_ordering() {
        let variant_order: Vec<u8> = itertools::sorted(
            vec![
                TypeSpec::Float,
                TypeSpec::Tuple(vec![]),
                TypeSpec::Int,
                TypeSpec::Null,
                TypeSpec::Union(BTreeSet::new()),
                TypeSpec::Any,
                TypeSpec::Object(BTreeMap::new()),
                TypeSpec::Bool,
                TypeSpec::Func(vec![], Box::new(TypeSpec::Any)),
                TypeSpec::Str,
                TypeSpec::Map(Box::new(TypeSpec::Any)),
                TypeSpec::Array(Box::new(TypeSpec::Any)),
            ]
            .iter(),
        )
        .map(TypeSpec::variant)
        .collect();
        assert_eq!(variant_order, vec![
            0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b,
            0x0c,
        ]);
    }
}
