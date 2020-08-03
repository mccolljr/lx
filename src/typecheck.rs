#![allow(unused_imports, dead_code, unused_variables)]

use itertools;

use crate::ast::{
    Expr,
    Ident,
    LetDeclTarget,
    ObjKey,
    ObjLitField,
    ObjTypeField,
    Stmt,
    Type,
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
}

impl Scope {
    fn new() -> Rc<Self> {
        Rc::new(Scope {
            parent:     None,
            decls:      RefCell::new(BTreeMap::new()),
            type_decls: RefCell::new(BTreeMap::new()),
        })
    }

    fn extend(self: &Rc<Self>) -> Rc<Self> {
        Rc::new(Scope {
            parent:     Some(Rc::clone(self)),
            decls:      RefCell::new(BTreeMap::new()),
            type_decls: RefCell::new(BTreeMap::new()),
        })
    }

    fn declare_type(self: &Rc<Self>, name: String, spec: TypeSpec) {
        if self.type_decls.borrow().get(&name).is_some() {
            unreachable!("type checker encountered redeclared type");
        }
        self.type_decls.borrow_mut().insert(name, spec);
    }

    fn lookup_type(self: &Rc<Self>, name: &String) -> TypeSpec {
        return self
            .type_decls
            .borrow()
            .get(name)
            .expect("type checker encountered undeclared type")
            .clone();
    }

    fn declare_var(self: &Rc<Self>, name: String, spec: TypeSpec) {
        if self.decls.borrow().get(&name).is_some() {
            unreachable!("type checker encountered redeclared variable");
        }
        self.decls.borrow_mut().insert(name, spec);
    }

    fn lookup_var(self: &Rc<Self>, name: &String) -> TypeSpec {
        return self
            .type_decls
            .borrow()
            .get(name)
            .expect("type checker encountered undeclared variable")
            .clone();
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
                let expr_typ = self.check_expr(expr.as_ref())?;
                match target {
                    LetDeclTarget::Ident(ident) => {
                        if let Some(given_typ) = annotation {
                            self.scope.declare_var(
                                ident.name.clone(),
                                self.from_ast(&given_typ.typ),
                            );
                        } else {
                            self.scope.declare_var(
                                ident.name.clone(),
                                self.check_expr(expr.as_ref())?,
                            );
                        }
                        Ok(())
                    }
                    LetDeclTarget::ArrDestruct(fields) => {
                        match expr_typ {
                            TypeSpec::Tuple(tup_fields) => {
                                if fields.len() > tup_fields.len() {
                                    return Err(());
                                }
                            }
                            TypeSpec::Array(elt_typ) => {}
                            _ => return Err(()),
                        }
                    }
                    LetDeclTarget::ObjDestruct(fields) => {}
                }
            }
            _ => Err(()),
        }
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
            Expr::Import { .. } => unimplemented!(),
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

    // fn collect_return_types(
    //     &mut self,
    //     stmts: &Vec<Stmt>,
    // ) -> Result<BTreeSet<TypeSpec>, ()> {
    //     let encountered
    //     for stmt in stmts.iter() {
    //         match stmt {
    //             Stmt::If { head, tail } => {
    //                 for block in head {
    //                     self.collect_return_types(types, &block.body);
    //                 }
    //                 if tail.is_some() {
    //                     self.collect_return_types(
    //                         types,
    //                         &tail.as_ref().unwrap().body,
    //                     );
    //                 }
    //             }
    //             Stmt::Try {
    //                 body,
    //                 catch,
    //                 finally,
    //             } => {
    //                 self.collect_return_types(types, &body.body);
    //                 if catch.is_some() {
    //                     self.collect_return_types(
    //                         types,
    //                         &catch.as_ref().unwrap().body,
    //                     )?;
    //                 }
    //                 if finally.is_some() {
    //                     self.collect_return_types(
    //                         types,
    //                         &finally.as_ref().unwrap().body,
    //                     )?;
    //                 }
    //             }
    //             Stmt::While { body, .. } => {
    //                 self.collect_return_types(types, &body)?;
    //             }
    //             Stmt::ForIn { body, .. } => {
    //                 self.collect_return_types(types, &body)?;
    //             }
    //             Stmt::Return { expr, .. } | Stmt::Yield { expr, .. } => {
    //                 types.insert(self.check_expr(expr.as_ref())?);
    //             }
    //             Stmt::LetDecl { .. }
    //             | Stmt::Break { .. }
    //             | Stmt::Expr { .. }
    //             | Stmt::Throw { .. }
    //             | Stmt::FnDecl { .. }
    //             | Stmt::Assignment { .. }
    //             | Stmt::TypeDecl { .. } => {
    //                 // other statement types can't contain relevant returns
    //             }
    //         }
    //     }
    //     Ok(())
    // }
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
