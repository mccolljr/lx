#![allow(unused_imports, dead_code, unused_variables)]

use itertools;
use quick_error::quick_error;

use crate::ast::{
    AssignTarget,
    Expr,
    File,
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
use crate::error::{
    Error,
    TypeError,
};
use crate::runtime::builtin::builtins_types;
use crate::token::TokenType;
use crate::typing::Typing;

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
use std::fmt::{
    Display,
    Formatter,
    Result as FmtResult,
};
use std::hash::{
    Hash,
    Hasher,
};
use std::rc::Rc;

struct Scope {
    parent:     Option<Rc<Scope>>,
    decls:      RefCell<BTreeMap<String, Typing>>,
    type_decls: RefCell<BTreeMap<String, Typing>>,
    returns:    Option<Typing>,
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

    fn extend_return(self: &Rc<Self>, returns: Typing) -> Rc<Self> {
        Rc::new(Scope {
            parent:     Some(Rc::clone(self)),
            decls:      RefCell::new(BTreeMap::new()),
            type_decls: RefCell::new(BTreeMap::new()),
            returns:    Some(returns),
        })
    }

    fn declare_type(self: &Rc<Self>, name: String, spec: Typing) {
        if self.type_decls.borrow().get(&name).is_some() {
            unreachable!("type checker encountered redeclared type");
        }
        self.type_decls.borrow_mut().insert(name, spec);
    }

    fn lookup_type(self: &Rc<Self>, name: &String) -> Typing {
        if let Some(typespec) = self.type_decls.borrow().get(name) {
            return typespec.clone();
        }
        if let Some(parent) = self.parent.as_ref() {
            return parent.lookup_type(name);
        }
        unreachable!("type checker encountered undeclared type");
    }

    fn declare_var(self: &Rc<Self>, name: String, spec: Typing) {
        if self.decls.borrow().get(&name).is_some() {
            unreachable!("type checker encountered redeclared variable");
        }
        self.decls.borrow_mut().insert(name, spec);
    }

    fn lookup_var(self: &Rc<Self>, name: &String) -> Typing {
        if let Some(typespec) = self.decls.borrow().get(name) {
            return typespec.clone();
        }
        if let Some(parent) = self.parent.as_ref() {
            return parent.lookup_var(name);
        }
        unreachable!("type checker encountered undeclared variable");
    }

    fn return_type(self: &Rc<Self>) -> Option<Typing> {
        if self.returns.is_some() {
            return self.returns.clone();
        }
        if self.parent.is_some() {
            return self.parent.as_ref().unwrap().return_type();
        }
        return None;
    }
}

pub struct Checker {
    scope: Rc<Scope>,
}

impl Checker {
    pub fn check_file(f: &File) -> Result<(), TypeError> {
        let mut c = Checker::new();
        for (name, spec) in builtins_types() {
            c.scope.declare_var(name, spec);
        }
        for stmt in f.stmts.iter() {
            c.check_stmt(stmt)?;
        }
        Ok(())
    }

    fn new() -> Self {
        Checker {
            scope: Scope::new(),
        }
    }

    fn open_scope(&mut self) { self.scope = Scope::extend(&self.scope); }

    fn open_fn_scope(&mut self, returns: Typing) {
        self.scope = Scope::extend_return(&self.scope, returns);
    }

    fn close_scope(&mut self) {
        self.scope = self.scope.parent.as_ref().unwrap().clone()
    }

    fn from_ast(&mut self, src: &Type) -> Typing {
        match src {
            Type::Any { .. } => Typing::Any,
            Type::Int { .. } => Typing::Int,
            Type::Float { .. } => Typing::Float,
            Type::Bool { .. } => Typing::Bool,
            Type::Str { .. } => Typing::Str,
            Type::Null { .. } => Typing::Null,
            Type::Nullable { element, .. } => {
                Typing::union_of(vec![
                    Typing::Null,
                    Typing::coalesce(vec![self.from_ast(element.as_ref())]),
                ])
            }
            Type::Named { ident } => self.scope.lookup_type(&ident.name),
            Type::Array { element, .. } => {
                Typing::array_of(self.from_ast(element.as_ref()))
            }
            Type::Map { element, .. } => {
                Typing::map_of(self.from_ast(element.as_ref()))
            }
            Type::Object { fields, .. } => {
                Typing::Object(
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
                Typing::Tuple(fields.iter().map(|t| self.from_ast(t)).collect())
            }
            Type::Func { args, ret, .. } => {
                Typing::Func(
                    args.iter().map(|a| self.from_ast(a)).collect(),
                    false,
                    Box::new(self.from_ast(ret.as_ref())),
                )
            }
            Type::Union { alts, .. } => {
                Typing::union_of(alts.iter().map(|t| self.from_ast(t)))
            }
        }
    }

    fn check_assign(
        &mut self,
        src: &Typing,
        target: &Typing,
    ) -> Result<(), TypeError> {
        if src == target {
            // A type is always assignable to itself
            return Ok(());
        }

        if src == &Typing::Any || target == &Typing::Any {
            // Any is assignable to and from any other type
            // (used to escape type safety)
            return Ok(());
        }

        match (src, target) {
            (Typing::Object(src_fields), Typing::Map(target_elt_type)) => {
                for src_field_typ in src_fields.values() {
                    self.check_assign(src_field_typ, target_elt_type)?;
                }
                return Ok(());
            }
            (Typing::Object(src_fields), Typing::Object(target_fields)) => {
                for (target_name, target_type) in target_fields {
                    let src_type =
                        src_fields.get(target_name).unwrap_or(&Typing::Null);
                    self.check_assign(src_type, target_type)?;
                }
                return Ok(());
            }
            (Typing::Tuple(src_fields), Typing::Array(target_elt_type)) => {
                for src_field_typ in src_fields {
                    self.check_assign(src_field_typ, target_elt_type)?;
                }
                return Ok(());
            }
            (Typing::Tuple(src_fields), Typing::Tuple(target_fields)) => {
                for (i, target_type) in itertools::enumerate(target_fields) {
                    let src_type = src_fields.get(i).unwrap_or(&Typing::Null);
                    self.check_assign(src_type, target_type)?;
                }
                return Ok(());
            }
            (src_type, Typing::Union(alts)) => {
                for alt in alts {
                    if self.check_assign(src_type, alt).is_ok() {
                        return Ok(());
                    }
                }
                return Err(TypeError::InvalidAssign(
                    src.clone(),
                    target.clone(),
                ));
            }
            (
                Typing::Func(src_args, src_is_variadic, src_ret),
                Typing::Func(target_args, target_is_variadic, target_ret),
            ) => {
                if src_args.len() != target_args.len() {
                    return Err(TypeError::InvalidAssign(
                        src.clone(),
                        target.clone(),
                    ));
                }
                self.check_assign(src_ret, target_ret)?;
                for (i, target_arg) in itertools::enumerate(target_args) {
                    // We need to check that the target argument type
                    // is assignable to the source argument type, since
                    // the values passed in by the eventual caller will
                    // conform to the target argument type, not the
                    // source argument type
                    let src_arg = src_args.get(i).unwrap();
                    self.check_assign(&target_arg, &src_arg)?;
                }
                if src_is_variadic != target_is_variadic {
                    return Err(TypeError::InvalidAssign(
                        src.clone(),
                        target.clone(),
                    ));
                }
                return Ok(());
            }
            _ => {
                return Err(TypeError::InvalidAssign(
                    src.clone(),
                    target.clone(),
                ));
            }
        }
    }

    fn check_stmt(&mut self, stmt: &Stmt) -> Result<(), TypeError> {
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
            Stmt::Assignment { target, rhs, .. } => {
                let rhs_typespec = self.check_expr(rhs.as_ref())?;
                match target {
                    AssignTarget::Ident(ident) => {
                        let target_typespec =
                            self.scope.lookup_var(&ident.name);
                        self.check_assign(&rhs_typespec, &target_typespec)?;
                        self.scope.declare_var(
                            ident.name.clone(),
                            rhs_typespec.clone(),
                        );
                        Ok(())
                    }
                    AssignTarget::Index(collection, elt) => {
                        let collection_typespec =
                            self.check_expr(collection.as_ref())?;
                        let index_typespec = self.check_expr(elt.as_ref())?;
                        let target_typespec = self.check_index_expr(
                            &collection_typespec,
                            &index_typespec,
                        )?;
                        self.check_assign(&rhs_typespec, &target_typespec)
                    }
                    AssignTarget::Select(object, key) => {
                        let object_typespec =
                            self.check_expr(object.as_ref())?;
                        let target_typespec = self.check_selector_expr(
                            &object_typespec,
                            key.name.clone(),
                        )?;
                        self.check_assign(&rhs_typespec, &target_typespec)
                    }
                }
            }
            Stmt::If { head, tail } => {
                for if_block in head {
                    self.check_expr(if_block.cond.as_ref())?;
                    self.open_scope();
                    for stmt in if_block.body.iter() {
                        self.check_stmt(stmt)?;
                    }
                    self.close_scope();
                }
                if let Some(else_block) = tail {
                    self.open_scope();
                    for stmt in else_block.body.iter() {
                        self.check_stmt(stmt)?;
                    }
                    self.close_scope();
                }
                Ok(())
            }
            Stmt::Return { expr, .. } | Stmt::Yield { expr, .. } => {
                let maybe_ret_typespec = self.scope.return_type();
                if maybe_ret_typespec.is_none() {
                    unreachable!(
                        "return or yield encountered outside of function scope"
                    );
                }
                let ret_typespec = maybe_ret_typespec.unwrap();
                let expr_typespec = self.check_expr(expr.as_ref())?;
                self.check_assign(&expr_typespec, &ret_typespec)
            }
            Stmt::Expr { expr, .. } => {
                self.check_expr(expr.as_ref())?;
                Ok(())
            }
            Stmt::Try {
                body,
                catch,
                finally,
            } => {
                self.open_scope();
                for stmt in body.body.iter() {
                    self.check_stmt(stmt)?;
                }
                self.close_scope();
                if let Some(catch_block) = catch {
                    self.open_scope();
                    self.scope.declare_var(
                        catch_block.name.name.clone(),
                        Typing::Any,
                    );
                    for stmt in catch_block.body.iter() {
                        self.check_stmt(stmt)?;
                    }
                    self.close_scope();
                }
                if let Some(finally_block) = finally {
                    self.open_scope();
                    for stmt in finally_block.body.iter() {
                        self.check_stmt(stmt)?;
                    }
                    self.close_scope();
                }
                Ok(())
            }
            Stmt::While { cond, body, .. } => {
                self.check_expr(cond.as_ref())?;
                self.open_scope();
                for stmt in body.iter() {
                    self.check_stmt(stmt)?;
                }
                self.close_scope();
                Ok(())
            }
            Stmt::TypeDecl { name, typ, .. } => {
                let typespec = self.from_ast(typ.as_ref());
                self.scope.declare_type(name.name.clone(), typespec);
                Ok(())
            }
            Stmt::ForIn {
                ident, expr, body, ..
            } => {
                let expr_typespec = self.check_expr(expr.as_ref())?;
                let iter_typespec = self.check_iter_item(&expr_typespec)?;
                self.open_scope();
                self.scope.declare_var(ident.name.clone(), iter_typespec);
                for stmt in body.iter() {
                    self.check_stmt(stmt)?;
                }
                self.close_scope();
                Ok(())
            }
            Stmt::Throw { error, .. } => {
                self.check_expr(error.as_ref())?;
                Ok(())
            }
            Stmt::Break { .. } => {
                // break statements are pure control flow
                Ok(())
            }
        }
    }

    fn check_let_decl(
        &mut self,
        target: &LetDeclTarget,
        expr: &Expr,
        annotation: &Option<TypeAnnotation>,
    ) -> Result<(), TypeError> {
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
                match &expr_typespec {
                    Typing::Tuple(tup_fields) => {
                        for (i, name) in itertools::enumerate(fields) {
                            let tup_field = tup_fields.get(i);
                            if tup_field.is_none() {
                                return Err(TypeError::InvalidTupleField(
                                    i,
                                    expr_typespec,
                                ));
                            }
                            self.scope.declare_var(
                                name.clone(),
                                tup_field.unwrap().clone(),
                            );
                        }
                        return Ok(());
                    }
                    Typing::Array(elt_typ) => {
                        for (i, name) in itertools::enumerate(fields) {
                            self.scope.declare_var(
                                name.clone(),
                                Typing::union_of(itertools::cloned(&[
                                    Typing::Null,
                                    (**elt_typ).clone(),
                                ])),
                            );
                        }
                        return Ok(());
                    }
                    _ => {
                        return Err(TypeError::InvalidArrayDestruct(
                            expr_typespec,
                        ))
                    }
                }
            }
            LetDeclTarget::ObjDestruct(fields) => {
                match &expr_typespec {
                    Typing::Object(obj_fields) => {
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
                                        return Err(
                                            TypeError::InvalidObjectField(
                                                ident.name.clone(),
                                                expr_typespec,
                                            ),
                                        );
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
                                        return Err(
                                            TypeError::InvalidObjectField(
                                                ident.name.clone(),
                                                expr_typespec,
                                            ),
                                        );
                                    }
                                }
                            }
                        }
                        return Ok(());
                    }
                    Typing::Map(elt_type) => {
                        for destructure_item in fields {
                            let ident = match destructure_item {
                                ObjDestructItem::Name(ident) => ident,
                                ObjDestructItem::NameMap(_, ident) => ident,
                            };
                            self.scope.declare_var(
                                ident.name.clone(),
                                Typing::union_of(itertools::cloned(&[
                                    Typing::Null,
                                    (**elt_type).clone(),
                                ])),
                            );
                        }
                        return Ok(());
                    }
                    _ => {
                        return Err(TypeError::InvalidObjectDestruct(
                            expr_typespec,
                        ))
                    }
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
    ) -> Result<(), TypeError> {
        let (arg_typespecs, ret_typespec) =
            self.calculate_fn_type(args, ret_typ);
        // overall function typespec
        let fn_typespec = Typing::Func(
            arg_typespecs.clone(),
            // TODO: enable variadic functions in syntax - currently only OOB
            // functions are variadic
            false,
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

    fn calculate_fn_type(
        &mut self,
        args: &Vec<FnArg>,
        ret_typ: &Option<TypeAnnotation>,
    ) -> (Vec<Typing>, Typing) {
        // collect argument types
        let arg_typespecs: Vec<Typing> = args
            .iter()
            .map(|a| {
                if let Some(annotation) = a.typ.as_ref() {
                    self.from_ast(&annotation.typ)
                } else {
                    Typing::Any
                }
            })
            .collect();
        // determine return type
        let ret_typespec = ret_typ
            .as_ref()
            // TODO: collect the return types from the body
            .map_or(Typing::Any, |annotation| self.from_ast(&annotation.typ));
        return (arg_typespecs, ret_typespec);
    }

    fn check_expr(&mut self, expr: &Expr) -> Result<Typing, TypeError> {
        match expr {
            Expr::Import { .. } => {
                // TODO: type checking of imported files
                Ok(Typing::map_of(Typing::Any))
            }
            Expr::Typeof { .. } => {
                // typeof expressions always return a string
                Ok(Typing::Str)
            }
            Expr::LitInt { .. } => Ok(Typing::Int),
            Expr::LitFlt { .. } => Ok(Typing::Float),
            Expr::LitBool { .. } => Ok(Typing::Bool),
            Expr::LitStr { .. } => Ok(Typing::Str),
            Expr::LitNull { .. } => Ok(Typing::Null),
            Expr::Ident(ident) => Ok(self.scope.lookup_var(&ident.name)),
            Expr::LitFunc {
                args,
                ret_typ,
                body,
                ..
            } => {
                let (arg_typespecs, ret_typespec) =
                    self.calculate_fn_type(args, ret_typ);
                // overall function typespec
                let fn_typespec = Typing::Func(
                    arg_typespecs.clone(),
                    // TODO: enable variadic functions in syntax - currently
                    // only OOB functions are variadic
                    false,
                    Box::new(ret_typespec.clone()),
                );
                self.open_fn_scope(ret_typespec.clone());
                args.iter().enumerate().for_each(|(i, arg)| {
                    self.scope
                        .declare_var(arg.name.clone(), arg_typespecs[i].clone())
                });
                for stmt in body {
                    self.check_stmt(stmt)?;
                }
                self.close_scope();
                Ok(fn_typespec)
            }
            Expr::LitArr { elements, .. } => {
                Ok(Typing::Tuple(itertools::process_results(
                    elements.iter().map(|e| self.check_expr(e)),
                    |t| t.collect(),
                )?))
            }
            Expr::LitObj { fields, .. } => {
                let mut type_fields = BTreeMap::<String, Typing>::new();
                let mut field_types = Vec::<Typing>::new();
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
                                return Err(TypeError::Other(
                                    "duplicate key in object literal",
                                ));
                            }
                        } else {
                            can_obj = false
                        }
                    }
                }
                if can_obj {
                    Ok(Typing::Object(type_fields))
                } else {
                    Ok(Typing::map_of(Typing::coalesce(field_types)))
                }
            }
            Expr::Paren { expr, .. } => self.check_expr(expr.as_ref()),
            Expr::Ternary {
                cond, pass, fail, ..
            } => {
                let cond_typespec = self.check_expr(cond.as_ref())?;
                let pass_typespec = self.check_expr(pass.as_ref())?;
                let fail_typespec = self.check_expr(fail.as_ref())?;
                self.check_assign(&cond_typespec, &Typing::Bool)?;
                Ok(Typing::coalesce(vec![pass_typespec, fail_typespec]))
            }
            Expr::Binary {
                lhs, rhs, op_typ, ..
            } => {
                let lhs_spec = self.check_expr(lhs)?;
                let rhs_spec = self.check_expr(rhs)?;
                self.check_binary_expr(&lhs_spec, &rhs_spec, *op_typ)
            }
            Expr::Unary { expr, op_typ, .. } => {
                let expr_spec = self.check_expr(expr)?;
                self.check_unary_expr(&expr_spec, *op_typ)
            }
            Expr::Index { expr, index, .. } => {
                let expr_typespec = self.check_expr(expr.as_ref())?;
                let index_typespec = self.check_expr(index.as_ref())?;
                self.check_index_expr(&expr_typespec, &index_typespec)
            }
            Expr::Selector { expr, selector, .. } => {
                let expr_typespec = self.check_expr(expr.as_ref())?;
                self.check_selector_expr(&expr_typespec, selector.name.clone())
            }
            Expr::As { expr, t, .. } => {
                let expr_typespec = self.check_expr(expr.as_ref())?;
                let as_typespec = self.from_ast(t);
                if self.check_assign(&as_typespec, &expr_typespec).is_err() {
                    return Err(TypeError::InvalidAsRefinement(
                        expr_typespec,
                        as_typespec,
                    ));
                }
                return Ok(as_typespec);
            }
            Expr::Call { expr, args, .. } => {
                let expr_typespec = self.check_expr(expr.as_ref())?;
                if expr_typespec == Typing::Any {
                    return Ok(Typing::Any);
                }
                if let Typing::Func(
                    target_arg_typespecs,
                    target_is_variadic,
                    target_ret_typespec,
                ) = expr_typespec
                {
                    let have_argct = args.len();
                    let required_argct = if target_is_variadic {
                        target_arg_typespecs.len() - 1
                    } else {
                        target_arg_typespecs.len()
                    };

                    if args.len() < required_argct {
                        return Err(TypeError::InvalidCallArgct(
                            required_argct,
                            args.len(),
                        ));
                    }

                    if !target_is_variadic && args.len() > required_argct {
                        return Err(TypeError::InvalidCallArgct(
                            required_argct,
                            args.len(),
                        ));
                    }

                    let mut arg_typespecs =
                        Vec::<Typing>::with_capacity(args.len());
                    for arg in args.iter() {
                        arg_typespecs.push(self.check_expr(arg)?);
                    }

                    for i in 0..required_argct {
                        self.check_assign(
                            &arg_typespecs[i],
                            &target_arg_typespecs[i],
                        )?;
                    }

                    if target_is_variadic && have_argct > required_argct {
                        let vararg_typespec =
                            &target_arg_typespecs[required_argct];
                        for i in required_argct..have_argct {
                            self.check_assign(
                                &arg_typespecs[i],
                                vararg_typespec,
                            )?;
                        }
                    }

                    return Ok((*target_ret_typespec).clone());
                }
                return Err(TypeError::InvalidCallAtAll(expr_typespec));
            }
        }
    }

    fn check_op_add(
        &mut self,
        lhs: &Typing,
        rhs: &Typing,
    ) -> Result<Typing, TypeError> {
        if lhs == &Typing::Any || rhs == &Typing::Any {
            return Ok(Typing::Any);
        }
        match (lhs, rhs) {
            (Typing::Int, Typing::Int) => Ok(Typing::Int),
            (Typing::Int, Typing::Float) => Ok(Typing::Float),
            (Typing::Float, Typing::Int) => Ok(Typing::Float),
            (Typing::Float, Typing::Float) => Ok(Typing::Float),
            (Typing::Str, Typing::Str) => Ok(Typing::Str),
            (Typing::Array(arr_lhs), Typing::Array(arr_rhs)) => {
                Ok(Typing::array_of(Typing::coalesce(vec![
                    *arr_lhs.clone(),
                    *arr_rhs.clone(),
                ])))
            }
            (Typing::Array(arr_lhs), Typing::Tuple(tup_rhs)) => {
                Ok(Typing::array_of(Typing::coalesce(
                    tup_rhs
                        .iter()
                        .map(Clone::clone)
                        .chain(vec![*arr_lhs.clone()])
                        .collect(),
                )))
            }
            (Typing::Tuple(tup_lhs), Typing::Tuple(tup_rhs)) => {
                Ok(Typing::array_of(Typing::coalesce(
                    tup_lhs
                        .iter()
                        .map(Clone::clone)
                        .chain(tup_rhs.iter().map(Clone::clone))
                        .collect(),
                )))
            }
            _ => {
                return Err(TypeError::InvalidBinaryOp(
                    lhs.clone(),
                    rhs.clone(),
                    TokenType::OpAdd,
                ));
            }
        }
    }

    fn check_op_sub(
        &mut self,
        lhs: &Typing,
        rhs: &Typing,
    ) -> Result<Typing, TypeError> {
        if lhs == &Typing::Any || rhs == &Typing::Any {
            return Ok(Typing::Any);
        }
        match (lhs, rhs) {
            (Typing::Int, Typing::Int) => Ok(Typing::Int),
            (Typing::Int, Typing::Float) => Ok(Typing::Float),
            (Typing::Float, Typing::Int) => Ok(Typing::Float),
            (Typing::Float, Typing::Float) => Ok(Typing::Float),
            _ => {
                return Err(TypeError::InvalidBinaryOp(
                    lhs.clone(),
                    rhs.clone(),
                    TokenType::OpSub,
                ));
            }
        }
    }

    fn check_op_mul(
        &mut self,
        lhs: &Typing,
        rhs: &Typing,
    ) -> Result<Typing, TypeError> {
        if lhs == &Typing::Any || rhs == &Typing::Any {
            return Ok(Typing::Any);
        }
        match (lhs, rhs) {
            (Typing::Int, Typing::Int) => Ok(Typing::Int),
            (Typing::Int, Typing::Float) => Ok(Typing::Float),
            (Typing::Float, Typing::Int) => Ok(Typing::Float),
            (Typing::Float, Typing::Float) => Ok(Typing::Float),
            _ => {
                return Err(TypeError::InvalidBinaryOp(
                    lhs.clone(),
                    rhs.clone(),
                    TokenType::OpMul,
                ));
            }
        }
    }

    fn check_op_div(
        &mut self,
        lhs: &Typing,
        rhs: &Typing,
    ) -> Result<Typing, TypeError> {
        if lhs == &Typing::Any || rhs == &Typing::Any {
            return Ok(Typing::Any);
        }
        match (lhs, rhs) {
            (Typing::Int, Typing::Int) => Ok(Typing::Int),
            (Typing::Int, Typing::Float) => Ok(Typing::Float),
            (Typing::Float, Typing::Int) => Ok(Typing::Float),
            (Typing::Float, Typing::Float) => Ok(Typing::Float),
            _ => {
                return Err(TypeError::InvalidBinaryOp(
                    lhs.clone(),
                    rhs.clone(),
                    TokenType::OpDiv,
                ));
            }
        }
    }

    fn check_op_rem(
        &mut self,
        lhs: &Typing,
        rhs: &Typing,
    ) -> Result<Typing, TypeError> {
        if lhs == &Typing::Any || rhs == &Typing::Any {
            return Ok(Typing::Any);
        }
        match (lhs, rhs) {
            (Typing::Int, Typing::Int) => Ok(Typing::Int),
            (Typing::Int, Typing::Float) => Ok(Typing::Float),
            (Typing::Float, Typing::Int) => Ok(Typing::Float),
            (Typing::Float, Typing::Float) => Ok(Typing::Float),
            _ => {
                return Err(TypeError::InvalidBinaryOp(
                    lhs.clone(),
                    rhs.clone(),
                    TokenType::OpRem,
                ));
            }
        }
    }

    fn check_op_eq(
        &mut self,
        lhs: &Typing,
        rhs: &Typing,
    ) -> Result<Typing, TypeError> {
        // TODO: catch cases where types ensure result will always be false
        Ok(Typing::Bool)
    }

    fn check_op_neq(
        &mut self,
        lhs: &Typing,
        rhs: &Typing,
    ) -> Result<Typing, TypeError> {
        // TODO: catch cases where types ensure result will always be true
        Ok(Typing::Bool)
    }

    fn check_op_gt(
        &mut self,
        lhs: &Typing,
        rhs: &Typing,
    ) -> Result<Typing, TypeError> {
        match (lhs, rhs) {
            (Typing::Any, _)
            | (_, Typing::Any)
            | (Typing::Int, Typing::Int)
            | (Typing::Int, Typing::Float)
            | (Typing::Float, Typing::Int)
            | (Typing::Float, Typing::Float) => Ok(Typing::Bool),
            _ => {
                return Err(TypeError::InvalidBinaryOp(
                    lhs.clone(),
                    rhs.clone(),
                    TokenType::OpGt,
                ));
            }
        }
    }

    fn check_op_lt(
        &mut self,
        lhs: &Typing,
        rhs: &Typing,
    ) -> Result<Typing, TypeError> {
        match (lhs, rhs) {
            (Typing::Any, _)
            | (_, Typing::Any)
            | (Typing::Int, Typing::Int)
            | (Typing::Int, Typing::Float)
            | (Typing::Float, Typing::Int)
            | (Typing::Float, Typing::Float) => Ok(Typing::Bool),
            _ => {
                return Err(TypeError::InvalidBinaryOp(
                    lhs.clone(),
                    rhs.clone(),
                    TokenType::OpLt,
                ));
            }
        }
    }

    fn check_op_geq(
        &mut self,
        lhs: &Typing,
        rhs: &Typing,
    ) -> Result<Typing, TypeError> {
        match (lhs, rhs) {
            (Typing::Any, _)
            | (_, Typing::Any)
            | (Typing::Int, Typing::Int)
            | (Typing::Int, Typing::Float)
            | (Typing::Float, Typing::Int)
            | (Typing::Float, Typing::Float) => Ok(Typing::Bool),
            _ => {
                return Err(TypeError::InvalidBinaryOp(
                    lhs.clone(),
                    rhs.clone(),
                    TokenType::OpGeq,
                ));
            }
        }
    }

    fn check_op_leq(
        &mut self,
        lhs: &Typing,
        rhs: &Typing,
    ) -> Result<Typing, TypeError> {
        match (lhs, rhs) {
            (Typing::Any, _)
            | (_, Typing::Any)
            | (Typing::Int, Typing::Int)
            | (Typing::Int, Typing::Float)
            | (Typing::Float, Typing::Int)
            | (Typing::Float, Typing::Float) => Ok(Typing::Bool),
            _ => {
                return Err(TypeError::InvalidBinaryOp(
                    lhs.clone(),
                    rhs.clone(),
                    TokenType::OpLeq,
                ));
            }
        }
    }

    fn check_binary_expr(
        &mut self,
        lhs: &Typing,
        rhs: &Typing,
        op_typ: TokenType,
    ) -> Result<Typing, TypeError> {
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
            _ => {
                return Err(TypeError::InvalidBinaryOp(
                    lhs.clone(),
                    rhs.clone(),
                    op_typ,
                ));
            }
        }
    }

    fn check_unary_not(&mut self, _: &Typing) -> Result<Typing, TypeError> {
        // TODO: check for cases where type will always return true
        Ok(Typing::Bool)
    }

    fn check_unary_negative(
        &mut self,
        item: &Typing,
    ) -> Result<Typing, TypeError> {
        match item {
            Typing::Int => Ok(Typing::Int),
            Typing::Float => Ok(Typing::Float),
            _ => {
                return Err(TypeError::InvalidUnaryOp(
                    item.clone(),
                    TokenType::OpSub,
                ));
            }
        }
    }

    fn check_unary_expr(
        &mut self,
        item: &Typing,
        op_typ: TokenType,
    ) -> Result<Typing, TypeError> {
        match op_typ {
            TokenType::Bang => self.check_unary_not(item),
            TokenType::OpSub => self.check_unary_negative(item),
            _ => {
                return Err(TypeError::InvalidUnaryOp(item.clone(), op_typ));
            }
        }
    }

    fn check_index_expr(
        &mut self,
        expr: &Typing,
        index: &Typing,
    ) -> Result<Typing, TypeError> {
        match &expr {
            Typing::Tuple(fields) => {
                match index {
                    Typing::Int => {
                        Ok(Typing::coalesce(vec![
                            Typing::Null,
                            Typing::coalesce(
                                fields.iter().map(Clone::clone).collect(),
                            ),
                        ]))
                    }
                    _ => {
                        Err(TypeError::InvalidIndexOp(
                            expr.clone(),
                            index.clone(),
                        ))
                    }
                }
            }
            Typing::Array(elt_typespec) => {
                match index {
                    Typing::Any | Typing::Int => {
                        Ok(Typing::union_of(vec![
                            Typing::Null,
                            (**elt_typespec).clone(),
                        ]))
                    }
                    _ => {
                        Err(TypeError::InvalidIndexOp(
                            expr.clone(),
                            index.clone(),
                        ))
                    }
                }
            }
            Typing::Object(fields) => {
                Ok(Typing::coalesce(vec![
                    Typing::Null,
                    Typing::coalesce(
                        fields.iter().map(|(_, t)| t.clone()).collect(),
                    ),
                ]))
            }
            Typing::Map(elt_typespec) => {
                Ok(Typing::union_of(vec![
                    Typing::Null,
                    (**elt_typespec).clone(),
                ]))
            }
            _ => Err(TypeError::InvalidIndexAtAll(expr.clone())),
        }
    }

    fn check_selector_expr(
        &mut self,
        expr: &Typing,
        key: String,
    ) -> Result<Typing, TypeError> {
        match expr {
            Typing::Object(fields) => {
                if let Some(field_typespec) = fields.get(&key) {
                    return Ok(field_typespec.clone());
                }
                Err(TypeError::InvalidObjectField(key, expr.clone()))
            }
            Typing::Map(elt_typespec) => {
                Ok(Typing::union_of(vec![
                    Typing::Null,
                    (**elt_typespec).clone(),
                ]))
            }
            _ => Err(TypeError::InvalidObjectField(key, expr.clone())),
        }
    }

    fn check_iter_item(&mut self, expr: &Typing) -> Result<Typing, TypeError> {
        match expr {
            Typing::Any => Ok(Typing::Any),
            Typing::Array(elt_typespec) => Ok((**elt_typespec).clone()),
            Typing::Tuple(fields) => {
                Ok(Typing::coalesce(fields.iter().map(Clone::clone).collect()))
            }
            Typing::Map(elt_typespec) => {
                Ok(Typing::Tuple(vec![Typing::Str, (**elt_typespec).clone()]))
            }
            Typing::Object(fields) => {
                Ok(Typing::Tuple(vec![
                    Typing::Str,
                    Typing::coalesce(
                        fields.iter().map(|(_, v)| v.clone()).collect(),
                    ),
                ]))
            }
            _ => Err(TypeError::InvalidIteration(expr.clone())),
        }
    }
}

#[cfg(test)]
mod test {
    extern crate proc_macro;
    use super::{
        BTreeMap,
        BTreeSet,
        Typing,
    };
    use itertools;

    #[test]
    fn test_variant_ordering() {
        let variant_order: Vec<u8> = itertools::sorted(
            vec![
                Typing::Float,
                Typing::Tuple(vec![]),
                Typing::Int,
                Typing::Null,
                Typing::Union(BTreeSet::new()),
                Typing::Any,
                Typing::Object(BTreeMap::new()),
                Typing::Bool,
                Typing::Func(vec![], false, Box::new(Typing::Any)),
                Typing::Str,
                Typing::Map(Box::new(Typing::Any)),
                Typing::Array(Box::new(Typing::Any)),
            ]
            .iter(),
        )
        .map(Typing::variant)
        .collect();
        assert_eq!(variant_order, vec![
            0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b,
            0x0c,
        ]);
    }
}
