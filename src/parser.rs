use crate::ast::{
    CatchBlock,
    ElseBlock,
    Expr,
    FinallyBlock,
    FnArg,
    Ident,
    IfBlock,
    LetDeclTarget,
    Node,
    ObjDestructItem,
    ObjKey,
    ObjLitField,
    ObjTypeField,
    Stmt,
    TryBlock,
    Type,
    TypeAnnotation,
};
use crate::error::SyntaxError;
use crate::lexer::Lexer;
use crate::source::{
    Code,
    Pos,
};
use crate::token::{
    Token,
    TokenType,
};

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

struct ParseScope {
    parent:     Option<Rc<ParseScope>>,
    decls:      RefCell<HashMap<String, Pos>>,
    type_decls: RefCell<HashMap<String, Pos>>,
    captures:   RefCell<Vec<String>>,
}

impl ParseScope {
    fn create(parent: Option<Rc<ParseScope>>) -> Rc<Self> {
        Rc::from(ParseScope {
            parent,
            decls: RefCell::new(HashMap::new()),
            type_decls: RefCell::new(HashMap::new()),
            captures: RefCell::new(Vec::new()),
        })
    }

    fn get_local_decl(&self, name: &String) -> Option<Pos> {
        self.decls.borrow().get(name).map(|pos| *pos)
    }

    fn get_local_type_decl(&self, name: &String) -> Option<Pos> {
        self.type_decls.borrow().get(name).map(|pos| *pos)
    }

    fn declare(&self, name: String, at: Pos) -> Option<Pos> {
        if let Some(original) = self.get_local_decl(&name) {
            return Some(original);
        }
        self.decls.borrow_mut().insert(name.clone(), at);
        None
    }

    fn declare_type(&self, name: String, at: Pos) -> Option<Pos> {
        if let Some(original) = self.get_local_type_decl(&name) {
            return Some(original);
        }
        self.type_decls.borrow_mut().insert(name.clone(), at);
        None
    }

    fn utilize(&self, name: &String) -> bool {
        if self.get_local_decl(name).is_some() {
            // found in local scope
            return true;
        }

        if let Some(parent) = &self.parent {
            // if the name is found several scopes up, we need to capture it in
            // all of the intermediate scopes, too.
            if !parent.utilize(name) {
                return false;
            }
            // if we get here, it was found in or above the parent scope,
            // and we need to capture the name
            self.captures.borrow_mut().push(name.clone());
            return true;
        }

        return false;
    }

    fn utilize_type(&self, name: &String) -> bool {
        if self.get_local_type_decl(name).is_some() {
            // found in local scope
            return true;
        }

        if let Some(parent) = &self.parent {
            if parent.utilize_type(name) {
                return true;
            }
        }

        return false;
    }
}

pub struct Parser {
    lex:        Lexer,
    cur_t:      Token,
    peek_t:     Token,
    scope:      Option<Rc<ParseScope>>,
    func_depth: usize,
    loop_depth: usize,
}

impl Parser {
    pub fn parse_file(
        src: &Code,
        track_scope: bool,
        globals: Vec<String>,
    ) -> Result<Vec<Stmt>, SyntaxError> {
        let mut p = Parser::new(src, track_scope, globals);
        p.advance()?;
        p.parse_stmt_list(&[TokenType::EOF])
    }

    fn new(src: &Code, track_scope: bool, globals: Vec<String>) -> Self {
        Parser {
            lex:        Lexer::new(src),
            cur_t:      Token::new_meta(0, TokenType::EOF),
            peek_t:     Token::new_meta(0, TokenType::EOF),
            scope:      if track_scope {
                let global = ParseScope::create(None);
                for name in globals {
                    global
                        .declare(name, Pos::mark(0))
                        .expect_none("duplicate global");
                }
                Some(ParseScope::create(Some(global)))
            } else {
                None
            },
            func_depth: 0,
            loop_depth: 0,
        }
    }

    fn open_scope(&mut self) {
        if let Some(scope) = &self.scope {
            self.scope = Some(ParseScope::create(Some(Rc::clone(scope))));
        }
    }

    fn close_scope(&mut self) {
        if let Some(scope) = &self.scope {
            self.scope = Some(Rc::clone(scope.parent.as_ref().unwrap()));
        }
    }

    fn scope_declare(
        &mut self,
        name: String,
        pos: Pos,
    ) -> Result<(), SyntaxError> {
        if let Some(scope) = &self.scope {
            if let Some(orig_decl_pos) = scope.declare(name.clone(), pos) {
                return Err(SyntaxError::Redeclaration {
                    code: self.lex.src.clone(),
                    at: pos,
                    original: orig_decl_pos,
                    name,
                });
            }
        }
        Ok(())
    }

    fn scope_declare_type(
        &mut self,
        name: String,
        pos: Pos,
    ) -> Result<(), SyntaxError> {
        if let Some(scope) = &self.scope {
            if let Some(orig_decl_pos) = scope.declare_type(name.clone(), pos) {
                return Err(SyntaxError::TypeRedeclaration {
                    code: self.lex.src.clone(),
                    at: pos,
                    original: orig_decl_pos,
                    name,
                });
            }
        }
        Ok(())
    }

    fn scope_use(
        &mut self,
        name: &String,
        pos: Pos,
    ) -> Result<(), SyntaxError> {
        if let Some(scope) = &self.scope {
            if !scope.utilize(name) {
                return Err(SyntaxError::Undeclared {
                    code: self.lex.src.clone(),
                    at:   pos,
                    name: name.clone(),
                });
            }
        }
        Ok(())
    }

    fn scope_use_type(
        &mut self,
        name: &String,
        pos: Pos,
    ) -> Result<(), SyntaxError> {
        if let Some(scope) = &self.scope {
            if !scope.utilize_type(name) {
                return Err(SyntaxError::TypeUndeclared {
                    code: self.lex.src.clone(),
                    at:   pos,
                    name: name.clone(),
                });
            }
        }
        Ok(())
    }

    fn scope_is_closure(&mut self) -> bool {
        if let Some(scope) = &self.scope {
            return !scope.captures.borrow().is_empty();
        }
        // If we're not tracking scope, treat every scope as a closure.
        return true;
    }

    pub fn parse_stmt_list(
        &mut self,
        terminators: &[TokenType],
    ) -> Result<Vec<Stmt>, SyntaxError> {
        let mut stmts: Vec<Stmt> = Vec::with_capacity(3);
        while !terminators.contains(&self.peek_t.typ) {
            stmts.push(self.parse_stmt()?);
        }
        Ok(stmts)
    }

    fn parse_stmt(&mut self) -> Result<Stmt, SyntaxError> {
        match self.peek_t.typ {
            TokenType::KwLet => self.parse_let_stmt(),
            TokenType::KwFn => self.parse_fndef_stmt(),
            TokenType::KwIf => self.parse_if_stmt(),
            TokenType::KwWhile => self.parse_while_stmt(),
            TokenType::KwFor => self.parse_for_in_stmt(),
            TokenType::KwReturn => {
                if self.func_depth > 0 {
                    self.parse_return_stmt()
                } else {
                    Err(SyntaxError::NotAllowed {
                        code: self.lex.src.clone(),
                        at:   self.peek_t.pos,
                        what: String::from(
                            "return not allowed outside of function",
                        ),
                    })
                }
            }
            TokenType::KwYield => {
                if self.func_depth > 0 {
                    self.parse_yield_stmt()
                } else {
                    Err(SyntaxError::NotAllowed {
                        code: self.lex.src.clone(),
                        at:   self.peek_t.pos,
                        what: String::from(
                            "yield not allowed outside of function",
                        ),
                    })
                }
            }
            TokenType::KwBreak => {
                if self.loop_depth > 0 {
                    Ok(Stmt::Break {
                        kwbreak: self.expect(TokenType::KwBreak)?.pos,
                        semi:    self.expect(TokenType::Semi)?.pos,
                    })
                } else {
                    Err(SyntaxError::NotAllowed {
                        code: self.lex.src.clone(),
                        at:   self.peek_t.pos,
                        what: String::from("break not allowed outside of loop"),
                    })
                }
            }
            TokenType::KwThrow => self.parse_throw_stmt(),
            TokenType::KwTry => self.parse_try_stmt(),
            TokenType::KwType => self.parse_type_stmt(),
            _ => self.parse_expr_or_assignment_stmt(),
        }
    }

    fn parse_let_stmt(&mut self) -> Result<Stmt, SyntaxError> {
        self.expect(TokenType::KwLet)?;
        let kwlet = self.cur_t.pos;
        let (target, target_pos) = self.parse_let_target()?;
        let annotation = if self.peek_t.typ == TokenType::Colon {
            Some(self.parse_type_annotation()?)
        } else {
            None
        };
        let assign = self.expect(TokenType::Assign)?.pos;
        let expr = Box::new(self.parse_expr(0)?);
        let semi = self.expect(TokenType::Semi)?.pos;
        Ok(Stmt::LetDecl {
            kwlet,
            target,
            target_pos,
            annotation,
            assign,
            expr,
            semi,
        })
    }

    fn parse_let_target(
        &mut self,
    ) -> Result<(LetDeclTarget, Pos), SyntaxError> {
        match self.peek_t.typ {
            TokenType::Ident => {
                let Token { lit: name, pos, .. } =
                    self.expect(TokenType::Ident)?;
                self.scope_declare(name.clone(), pos)?;
                Ok((LetDeclTarget::Ident(Ident { name, pos }), pos))
            }
            TokenType::OSquare => {
                self.advance()?;
                let start = self.cur_t.pos.offset;
                let names = self.parse_trailing_comma_list(
                    TokenType::CSquare,
                    Parser::parse_array_destruct_item,
                )?;
                let end_pos = self.expect(TokenType::CSquare)?.pos;
                let end = end_pos.offset + end_pos.length;
                Ok((
                    LetDeclTarget::ArrDestruct(names),
                    Pos::span(start, end - start),
                ))
            }
            TokenType::OBrace => {
                self.advance()?;
                let start = self.cur_t.pos.offset;
                let items = self.parse_trailing_comma_list(
                    TokenType::CBrace,
                    Parser::parse_object_destruct_item,
                )?;
                let end_pos = self.expect(TokenType::CBrace)?.pos;
                let end = end_pos.offset + end_pos.length;
                Ok((
                    LetDeclTarget::ObjDestruct(items),
                    Pos::span(start, end - start),
                ))
            }
            _ => {
                Err(SyntaxError::Expected {
                    code:   self.lex.src.clone(),
                    at:     self.peek_t.pos,
                    wanted: "ident, array destructure, or object destructure"
                        .into(),
                    found:  format!("{}", self.peek_t.typ),
                })
            }
        }
    }

    fn parse_array_destruct_item(&mut self) -> Result<String, SyntaxError> {
        let Token { lit: name, pos, .. } = self.expect(TokenType::Ident)?;
        self.scope_declare(name.clone(), pos)?;
        Ok(name)
    }

    fn parse_object_destruct_item(
        &mut self,
    ) -> Result<ObjDestructItem, SyntaxError> {
        match self.peek_t.typ {
            TokenType::Ident => {
                // can be Name or NameMap
                let Token {
                    lit: name_or_key,
                    pos,
                    ..
                } = self.expect(TokenType::Ident)?;

                if self.peek_t.typ == TokenType::Colon {
                    self.advance()?;
                    let Token { pos, lit: name, .. } =
                        self.expect(TokenType::Ident)?;
                    self.scope_declare(name.clone(), pos)?;
                    return Ok(ObjDestructItem::NameMap(name_or_key, Ident {
                        name,
                        pos,
                    }));
                }
                self.scope_declare(name_or_key.clone(), pos)?;
                Ok(ObjDestructItem::Name(Ident {
                    name: name_or_key,
                    pos,
                }))
            }
            TokenType::LitString => {
                // can only be NameMap
                self.advance()?;
                let key = self.cur_t.lit.clone();
                self.expect(TokenType::Colon)?;
                let Token { pos, lit: name, .. } =
                    self.expect(TokenType::Ident)?;
                self.scope_declare(name.clone(), pos)?;
                Ok(ObjDestructItem::NameMap(key, Ident { name, pos }))
            }
            _ => {
                Err(SyntaxError::Expected {
                    code:   self.lex.src.clone(),
                    at:     self.peek_t.pos,
                    wanted: "object destrucruting item".into(),
                    found:  format!("{}", self.peek_t.typ),
                })
            }
        }
    }

    fn parse_fndef_stmt(&mut self) -> Result<Stmt, SyntaxError> {
        let kwfn = self.expect(TokenType::KwFn)?.pos;
        let name = self.parse_ident()?;
        self.scope_declare(name.name.clone(), name.pos)?;
        self.open_scope();
        let oparen = self.expect(TokenType::OParen)?.pos;
        let args = self.parse_trailing_comma_list(
            TokenType::CParen,
            Parser::parse_fnarg,
        )?;
        for arg in args.iter() {
            self.scope_declare(arg.name.clone(), arg.pos)?;
        }
        let cparen = self.expect(TokenType::CParen)?.pos;
        let ret_typ = if self.peek_t.typ == TokenType::Colon {
            Some(self.parse_type_annotation()?)
        } else {
            None
        };
        let obrace = self.expect(TokenType::OBrace)?.pos;
        self.func_depth += 1;
        let body = self.parse_stmt_list(&[TokenType::CBrace])?;
        self.func_depth -= 1;
        let cbrace = self.expect(TokenType::CBrace)?.pos;
        let stmt = Stmt::FnDecl {
            kwfn,
            name,
            oparen,
            args,
            cparen,
            ret_typ,
            obrace,
            body,
            cbrace,
            is_closure: self.scope_is_closure(),
        };
        self.close_scope();
        Ok(stmt)
    }

    fn parse_if_stmt(&mut self) -> Result<Stmt, SyntaxError> {
        let mut head = Vec::<IfBlock>::with_capacity(1);
        head.push(self.parse_if_block(true)?);
        while self.peek_t.typ == TokenType::KwElif {
            head.push(self.parse_if_block(false)?);
        }
        Ok(Stmt::If {
            head,
            tail: if self.peek_t.typ == TokenType::KwElse {
                Some(self.parse_else_block()?)
            } else {
                None
            },
        })
    }

    fn parse_if_block(&mut self, first: bool) -> Result<IfBlock, SyntaxError> {
        let kw_tok = self.expect(
            if first {
                TokenType::KwIf
            } else {
                TokenType::KwElif
            },
        )?;
        let cond = Box::new(self.parse_expr(0)?);
        let obrace = self.expect(TokenType::OBrace)?.pos;
        self.open_scope();
        let body = self.parse_stmt_list(&[TokenType::CBrace])?;
        self.close_scope();
        let cbrace = self.expect(TokenType::CBrace)?.pos;
        Ok(IfBlock {
            kw_typ: kw_tok.typ,
            kw_pos: kw_tok.pos,
            cond,
            obrace,
            body,
            cbrace,
        })
    }

    fn parse_else_block(&mut self) -> Result<ElseBlock, SyntaxError> {
        let kwelse = self.expect(TokenType::KwElse)?.pos;
        let obrace = self.expect(TokenType::OBrace)?.pos;
        self.open_scope();
        let body = self.parse_stmt_list(&[TokenType::CBrace])?;
        self.close_scope();
        let cbrace = self.expect(TokenType::CBrace)?.pos;
        Ok(ElseBlock {
            kwelse,
            obrace,
            body,
            cbrace,
        })
    }

    fn parse_while_stmt(&mut self) -> Result<Stmt, SyntaxError> {
        let kwwhile = self.expect(TokenType::KwWhile)?.pos;
        let cond = Box::new(self.parse_expr(0)?);
        let obrace = self.expect(TokenType::OBrace)?.pos;
        self.open_scope();
        self.loop_depth += 1;
        let body = self.parse_stmt_list(&[TokenType::CBrace])?;
        self.loop_depth -= 1;
        self.close_scope();
        let cbrace = self.expect(TokenType::CBrace)?.pos;
        Ok(Stmt::While {
            kwwhile,
            cond,
            obrace,
            body,
            cbrace,
        })
    }

    fn parse_for_in_stmt(&mut self) -> Result<Stmt, SyntaxError> {
        let kwfor = self.expect(TokenType::KwFor)?.pos;
        let ident = self.parse_ident()?;
        let kwin = self.expect(TokenType::KwIn)?.pos;
        let expr = Box::new(self.parse_expr(0)?);
        let obrace = self.expect(TokenType::OBrace)?.pos;
        self.open_scope();
        self.scope_declare(ident.name.clone(), ident.pos)?;
        self.loop_depth += 1;
        let body = self.parse_stmt_list(&[TokenType::CBrace])?;
        self.loop_depth -= 1;
        self.close_scope();
        let cbrace = self.expect(TokenType::CBrace)?.pos;
        Ok(Stmt::ForIn {
            kwfor,
            ident,
            kwin,
            expr,
            obrace,
            body,
            cbrace,
        })
    }

    fn parse_return_stmt(&mut self) -> Result<Stmt, SyntaxError> {
        Ok(Stmt::Return {
            kwreturn: self.expect(TokenType::KwReturn)?.pos,
            expr:     Box::new(self.parse_expr(0)?),
            semi:     self.expect(TokenType::Semi)?.pos,
        })
    }

    fn parse_yield_stmt(&mut self) -> Result<Stmt, SyntaxError> {
        Ok(Stmt::Yield {
            kwyield: self.expect(TokenType::KwYield)?.pos,
            expr:    Box::new(self.parse_expr(0)?),
            semi:    self.expect(TokenType::Semi)?.pos,
        })
    }

    fn parse_throw_stmt(&mut self) -> Result<Stmt, SyntaxError> {
        Ok(Stmt::Throw {
            kwthrow: self.expect(TokenType::KwThrow)?.pos,
            error:   Box::new(self.parse_expr(0)?),
            semi:    self.expect(TokenType::Semi)?.pos,
        })
    }

    fn parse_try_stmt(&mut self) -> Result<Stmt, SyntaxError> {
        let body: TryBlock = self.parse_try_block()?;
        let mut catch: Option<CatchBlock> = None;
        if self.peek_t.typ == TokenType::KwCatch {
            catch = Some(self.parse_catch_block()?);
        }
        let mut finally: Option<FinallyBlock> = None;
        if self.peek_t.typ == TokenType::KwFinally {
            finally = Some(self.parse_finally_block()?);
        }
        if catch.is_none() && finally.is_none() {
            return Err(SyntaxError::Expected {
                code:   self.lex.src.clone(),
                at:     self.peek_t.pos,
                wanted: "catch or finally".into(),
                found:  format!("{}", self.peek_t.typ),
            });
        }
        Ok(Stmt::Try {
            body,
            catch,
            finally,
        })
    }

    fn parse_try_block(&mut self) -> Result<TryBlock, SyntaxError> {
        self.open_scope();
        let try_block = TryBlock {
            kwtry:  self.expect(TokenType::KwTry)?.pos,
            obrace: self.expect(TokenType::OBrace)?.pos,
            body:   self.parse_stmt_list(&[TokenType::CBrace])?,
            cbrace: self.expect(TokenType::CBrace)?.pos,
        };
        self.close_scope();
        Ok(try_block)
    }

    fn parse_catch_block(&mut self) -> Result<CatchBlock, SyntaxError> {
        self.open_scope();
        let kwcatch = self.expect(TokenType::KwCatch)?.pos;
        let name = self.parse_ident()?;
        self.scope_declare(name.name.clone(), self.peek_t.pos)?;
        let obrace = self.expect(TokenType::OBrace)?.pos;
        let body = self.parse_stmt_list(&[TokenType::CBrace])?;
        let cbrace = self.expect(TokenType::CBrace)?.pos;
        self.close_scope();
        Ok(CatchBlock {
            kwcatch,
            name,
            obrace,
            body,
            cbrace,
        })
    }

    fn parse_finally_block(&mut self) -> Result<FinallyBlock, SyntaxError> {
        self.open_scope();
        let finally_block = FinallyBlock {
            kwfinally: self.expect(TokenType::KwFinally)?.pos,
            obrace:    self.expect(TokenType::OBrace)?.pos,
            body:      self.parse_stmt_list(&[TokenType::CBrace])?,
            cbrace:    self.expect(TokenType::CBrace)?.pos,
        };
        self.close_scope();
        Ok(finally_block)
    }

    fn parse_type_stmt(&mut self) -> Result<Stmt, SyntaxError> {
        let kwtype = self.expect(TokenType::KwType)?.pos;
        let name = self.parse_ident()?;
        self.scope_declare_type(name.name.clone(), name.pos)?;
        let assign = self.expect(TokenType::Assign)?.pos;
        let typ = Box::new(self.parse_type()?);
        let semi = self.expect(TokenType::Semi)?.pos;
        Ok(Stmt::TypeDecl {
            kwtype,
            name,
            assign,
            typ,
            semi,
        })
    }

    fn parse_expr_or_assignment_stmt(&mut self) -> Result<Stmt, SyntaxError> {
        let x = self.parse_expr(0)?;
        if self.peek_t.typ == TokenType::Assign {
            if let Some((target, target_pos)) = x.get_assign_target() {
                return Ok(Stmt::Assignment {
                    target,
                    target_pos,
                    assign: self.expect(TokenType::Assign)?.pos,
                    rhs: Box::new(self.parse_expr(0)?),
                    semi: self.expect(TokenType::Semi)?.pos,
                });
            }
            return Err(SyntaxError::InvalidAssignment {
                code:  self.lex.src.clone(),
                at:    x.pos(),
                found: format!("{}", x),
            });
        }
        Ok(Stmt::Expr {
            expr: Box::new(x),
            semi: self.expect(TokenType::Semi)?.pos,
        })
    }

    fn parse_expr_list(
        &mut self,
        terminator: TokenType,
    ) -> Result<Vec<Expr>, SyntaxError> {
        Ok(self.parse_trailing_comma_list(terminator, |s: &mut Self| {
            Parser::parse_expr(s, 0)
        })?)
    }

    fn parse_expr(
        &mut self,
        min_binding_power: i32,
    ) -> Result<Expr, SyntaxError> {
        let mut x: Expr = match self.peek_t.typ.prefix_binding_power() {
            Some((_, prefix_rbp)) => {
                self.advance()?;
                let op_pos = self.cur_t.pos;
                let op_typ = self.cur_t.typ;
                if op_typ == TokenType::KwTypeof {
                    Expr::Typeof {
                        kwtypeof: op_pos,
                        expr:     Box::new(self.parse_expr(prefix_rbp)?),
                    }
                } else {
                    Expr::Unary {
                        op_typ,
                        op_pos,
                        expr: Box::new(self.parse_expr(prefix_rbp)?),
                    }
                }
            }
            None => self.parse_primary_expr()?,
        };

        while let Some((infix_lbp, infix_rbp)) =
            self.peek_t.typ.infix_binding_power()
        {
            if infix_lbp < min_binding_power {
                break;
            }
            match self.peek_t.typ {
                TokenType::Question => {
                    x = self.parse_ternary(x)?;
                }
                _ => {
                    self.advance()?;
                    x = Expr::Binary {
                        lhs:    Box::new(x),
                        op_pos: self.cur_t.pos,
                        op_typ: self.cur_t.typ,
                        rhs:    Box::new(self.parse_expr(infix_rbp)?),
                    };
                }
            };
        }
        Ok(x)
    }

    fn parse_primary_expr(&mut self) -> Result<Expr, SyntaxError> {
        self.advance()?;
        let mut x = match self.cur_t.typ {
            TokenType::KwImport => {
                Expr::Import {
                    kwimport: self.cur_t.pos,
                    oparen:   self.expect(TokenType::OParen)?.pos,
                    name:     self.expect(TokenType::LitString)?.lit,
                    cparen:   self.expect(TokenType::CParen)?.pos,
                }
            }
            TokenType::KwNull => {
                Expr::LitNull {
                    pos: self.cur_t.pos,
                }
            }
            TokenType::LitInt => {
                Expr::LitInt {
                    pos: self.cur_t.pos,
                    val: self.cur_t.lit.parse().expect("illegal int value"),
                }
            }
            TokenType::LitFloat => {
                Expr::LitFlt {
                    pos: self.cur_t.pos,
                    val: self.cur_t.lit.parse().expect("illegal float value"),
                }
            }
            TokenType::KwTrue | TokenType::KwFalse => {
                Expr::LitBool {
                    pos: self.cur_t.pos,
                    val: self.cur_t.typ == TokenType::KwTrue,
                }
            }
            TokenType::LitString => {
                Expr::LitStr {
                    pos: self.cur_t.pos,
                    val: self.cur_t.lit.clone(),
                }
            }
            TokenType::KwFn => {
                self.open_scope();
                let kwfn = self.cur_t.pos;
                let oparen = self.expect(TokenType::OParen)?.pos;
                let args = self.parse_trailing_comma_list(
                    TokenType::CParen,
                    Parser::parse_fnarg,
                )?;
                for arg in args.iter() {
                    self.scope_declare(arg.name.clone(), arg.pos)?;
                }
                let cparen = self.expect(TokenType::CParen)?.pos;
                let ret_typ = if self.peek_t.typ == TokenType::Colon {
                    Some(self.parse_type_annotation()?)
                } else {
                    None
                };
                let obrace = self.expect(TokenType::OBrace)?.pos;
                self.func_depth += 1;
                let body = self.parse_stmt_list(&[TokenType::CBrace])?;
                self.func_depth -= 1;
                let cbrace = self.expect(TokenType::CBrace)?.pos;
                let expr = Expr::LitFunc {
                    kwfn,
                    oparen,
                    args,
                    cparen,
                    ret_typ,
                    obrace,
                    body,
                    cbrace,
                    is_closure: self.scope_is_closure(),
                };
                self.close_scope();
                expr
            }
            TokenType::Ident => {
                let pos = self.cur_t.pos;
                let name = self.cur_t.lit.clone();
                self.scope_use(&name, pos)?;
                Expr::Ident(Ident { pos, name })
            }
            TokenType::OParen => {
                Expr::Paren {
                    oparen: self.cur_t.pos,
                    expr:   Box::new(self.parse_expr(0)?),
                    cparen: self.expect(TokenType::CParen)?.pos,
                }
            }
            TokenType::OSquare => {
                Expr::LitArr {
                    osquare:  self.cur_t.pos,
                    elements: self.parse_expr_list(TokenType::CSquare)?,
                    csquare:  self.expect(TokenType::CSquare)?.pos,
                }
            }
            TokenType::OBrace => {
                Expr::LitObj {
                    obrace: self.cur_t.pos,
                    fields: self.parse_field_list()?,
                    cbrace: self.expect(TokenType::CBrace)?.pos,
                }
            }
            _ => {
                return Err(SyntaxError::Expected {
                    code:   self.lex.src.clone(),
                    at:     self.cur_t.pos,
                    wanted: "expression".into(),
                    found:  format!("{}", self.cur_t.typ),
                })
            }
        };

        loop {
            match self.peek_t.typ {
                TokenType::OParen => {
                    x = self.parse_call(x)?;
                }
                TokenType::OSquare => {
                    x = self.parse_index(x)?;
                }
                TokenType::Dot => {
                    x = self.parse_selector_expr(x)?;
                }
                _ => return Ok(x),
            }
        }
    }

    fn parse_call(&mut self, expr: Expr) -> Result<Expr, SyntaxError> {
        let oparen = self.expect(TokenType::OParen)?.pos;
        let args = self.parse_expr_list(TokenType::CParen)?;
        let cparen = self.expect(TokenType::CParen)?.pos;
        Ok(Expr::Call {
            expr: Box::from(expr),
            oparen,
            args,
            cparen,
        })
    }

    fn parse_fnarg(&mut self) -> Result<FnArg, SyntaxError> {
        self.expect(TokenType::Ident)?;
        Ok(FnArg {
            pos:  self.cur_t.pos,
            name: self.cur_t.lit.clone(),
            typ:  if self.peek_t.typ == TokenType::Colon {
                Some(self.parse_type_annotation()?)
            } else {
                None
            },
        })
    }

    fn parse_type_annotation(&mut self) -> Result<TypeAnnotation, SyntaxError> {
        Ok(TypeAnnotation {
            colon: self.expect(TokenType::Colon)?.pos,
            typ:   self.parse_type()?,
        })
    }

    fn parse_ternary(&mut self, cond: Expr) -> Result<Expr, SyntaxError> {
        let (_, rbp) = TokenType::Question.infix_binding_power().unwrap();
        Ok(Expr::Ternary {
            cond:     Box::new(cond),
            question: self.expect(TokenType::Question)?.pos,
            pass:     Box::new(self.parse_expr(rbp)?),
            colon:    self.expect(TokenType::Colon)?.pos,
            fail:     Box::new(self.parse_expr(0)?),
        })
    }

    fn parse_index(&mut self, expr: Expr) -> Result<Expr, SyntaxError> {
        Ok(Expr::Index {
            expr:    Box::from(expr),
            osquare: self.expect(TokenType::OSquare)?.pos,
            index:   Box::new(self.parse_expr(0)?),
            csquare: self.expect(TokenType::CSquare)?.pos,
        })
    }

    fn parse_selector_expr(&mut self, expr: Expr) -> Result<Expr, SyntaxError> {
        let dot = self.expect(TokenType::Dot)?.pos;
        let selector = self.parse_ident()?;
        Ok(Expr::Selector {
            expr: Box::from(expr),
            dot,
            selector,
        })
    }

    fn parse_field_list(&mut self) -> Result<Vec<ObjLitField>, SyntaxError> {
        Ok(self.parse_trailing_comma_list(
            TokenType::CBrace,
            Parser::parse_object_field,
        )?)
    }

    fn parse_object_field(&mut self) -> Result<ObjLitField, SyntaxError> {
        match self.peek_t.typ {
            TokenType::OParen => {
                self.advance()?;
                return Ok(ObjLitField {
                    key:   ObjKey::Dynamic(Box::new(Expr::Paren {
                        oparen: self.cur_t.pos,
                        expr:   Box::new(self.parse_expr(0)?),
                        cparen: self.expect(TokenType::CParen)?.pos,
                    })),
                    colon: self.expect(TokenType::Colon)?.pos,
                    val:   Box::new(self.parse_expr(0)?),
                });
            }
            TokenType::LitString | TokenType::Ident => {
                self.advance()?;
                return Ok(ObjLitField {
                    key:   ObjKey::Static(
                        self.cur_t.lit.clone(),
                        self.cur_t.pos,
                    ),
                    colon: self.expect(TokenType::Colon)?.pos,
                    val:   Box::new(self.parse_expr(0)?),
                });
            }
            _ => {
                return Err(SyntaxError::Expected {
                    code:   self.lex.src.clone(),
                    at:     self.peek_t.pos,
                    wanted: "field key".into(),
                    found:  format!("{}", self.peek_t.typ),
                })
            }
        }
    }

    fn parse_ident(&mut self) -> Result<Ident, SyntaxError> {
        let tok = self.expect(TokenType::Ident)?;
        Ok(Ident {
            name: tok.lit,
            pos:  tok.pos,
        })
    }

    fn parse_type_item(&mut self) -> Result<Type, SyntaxError> {
        match self.peek_t.typ {
            TokenType::KwAny => {
                return Ok(Type::Any {
                    kwany: self.expect(TokenType::KwAny)?.pos,
                })
            }
            TokenType::KwInt => {
                return Ok(Type::Int {
                    kwint: self.expect(TokenType::KwInt)?.pos,
                })
            }
            TokenType::KwFloat => {
                return Ok(Type::Float {
                    kwfloat: self.expect(TokenType::KwFloat)?.pos,
                })
            }
            TokenType::KwBool => {
                return Ok(Type::Bool {
                    kwbool: self.expect(TokenType::KwBool)?.pos,
                })
            }
            TokenType::KwStr => {
                return Ok(Type::Str {
                    kwstr: self.expect(TokenType::KwStr)?.pos,
                })
            }
            TokenType::KwNull => {
                return Ok(Type::Null {
                    kwnull: self.expect(TokenType::KwNull)?.pos,
                })
            }
            TokenType::Question => {
                // TODO: this technically allows "???SomeType", which is
                // redundant.
                return Ok(Type::Nullable {
                    question: self.expect(TokenType::Question)?.pos,
                    element:  Box::new(self.parse_type()?),
                });
            }
            TokenType::Ident => {
                let ident = self.parse_ident()?;
                self.scope_use_type(&ident.name, ident.pos)?;
                return Ok(Type::Named { ident });
            }
            TokenType::KwArray => {
                return Ok(Type::Array {
                    kwarray: self.expect(TokenType::KwArray)?.pos,
                    osquare: self.expect(TokenType::OSquare)?.pos,
                    element: Box::new(self.parse_type()?),
                    csquare: self.expect(TokenType::CSquare)?.pos,
                })
            }
            TokenType::KwMap => {
                return Ok(Type::Map {
                    kwmap:   self.expect(TokenType::KwMap)?.pos,
                    osquare: self.expect(TokenType::OSquare)?.pos,
                    element: Box::new(self.parse_type()?),
                    csquare: self.expect(TokenType::CSquare)?.pos,
                })
            }
            TokenType::KwFn => {
                Ok(Type::Func {
                    kwfn:   self.expect(TokenType::KwFn)?.pos,
                    oparen: self.expect(TokenType::OParen)?.pos,
                    args:   self.parse_trailing_comma_list(
                        TokenType::CParen,
                        Parser::parse_type,
                    )?,
                    cparen: self.expect(TokenType::CParen)?.pos,
                    colon:  self.expect(TokenType::Colon)?.pos,
                    ret:    Box::new(self.parse_type()?),
                })
            }
            TokenType::OSquare => {
                Ok(Type::Tuple {
                    osquare: self.expect(TokenType::OSquare)?.pos,
                    fields:  self.parse_trailing_comma_list(
                        TokenType::CSquare,
                        Parser::parse_type,
                    )?,
                    csquare: self.expect(TokenType::CSquare)?.pos,
                })
            }
            TokenType::OBrace => {
                Ok(Type::Object {
                    obrace: self.expect(TokenType::OBrace)?.pos,
                    fields: self.parse_trailing_comma_list(
                        TokenType::CBrace,
                        Parser::parse_object_type_field,
                    )?,
                    cbrace: self.expect(TokenType::CBrace)?.pos,
                })
            }
            _ => {
                return Err(SyntaxError::Expected {
                    code:   self.lex.src.clone(),
                    at:     self.cur_t.pos,
                    wanted: "type".into(),
                    found:  format!("{}", self.cur_t.typ),
                })
            }
        }
    }

    fn parse_type(&mut self) -> Result<Type, SyntaxError> {
        let mut items: Vec<Type> = vec![self.parse_type_item()?];
        while self.peek_t.typ == TokenType::Bar {
            self.expect(TokenType::Bar)?;
            items.push(self.parse_type_item()?);
        }
        if items.len() == 1 {
            return Ok(items.pop().unwrap());
        }
        return Ok(Type::Union { alts: items });
    }

    fn parse_object_type_field(&mut self) -> Result<ObjTypeField, SyntaxError> {
        Ok(ObjTypeField {
            key:   self.parse_ident()?,
            colon: self.expect(TokenType::Colon)?.pos,
            typ:   Box::new(self.parse_type()?),
        })
    }

    fn advance(&mut self) -> Result<(), SyntaxError> {
        let mut nex_t = self.lex.next()?;
        while nex_t.typ == TokenType::Comment {
            nex_t = self.lex.next()?;
        }
        self.cur_t = std::mem::replace(&mut self.peek_t, nex_t);
        Ok(())
    }

    fn expect(&mut self, expected: TokenType) -> Result<Token, SyntaxError> {
        self.advance()?;
        if self.cur_t.typ != expected {
            return Err(SyntaxError::Expected {
                code:   self.lex.src.clone(),
                at:     self.cur_t.pos,
                wanted: format!("{}", expected),
                found:  format!("{}", self.cur_t.typ),
            });
        }
        Ok(self.cur_t.clone())
    }

    fn parse_trailing_comma_list<T>(
        &mut self,
        terminator: TokenType,
        mut f: impl FnMut(&mut Self) -> Result<T, SyntaxError>,
    ) -> Result<Vec<T>, SyntaxError> {
        let mut list = Vec::<T>::new();
        while self.peek_t.typ != terminator {
            if list.len() > 0 {
                self.expect(TokenType::Comma)?;
            }
            if self.peek_t.typ == terminator {
                break;
            }
            list.push(f(self)?);
        }
        Ok(list)
    }
}

#[cfg(test)]
mod test {
    use crate::{
        ast::{
            Expr,
            Ident,
            ObjTypeField,
            Type,
        },
        token::TokenType,
    };

    macro_rules! pos {
        ($start:expr, $len:expr) => {
            crate::source::Pos::span($start, $len)
        };
        ($start:expr) => {
            crate::source::Pos::span($start, 1)
        };
        (mark $start:expr) => {
            crate::source::Pos::span($start, 0)
        };
    }

    macro_rules! string {
        ($str:expr) => {
            String::from($str)
        };
    }

    macro_rules! expr {
        ($x:expr) => {
            Box::new($x)
        };
    }

    macro_rules! args {
        ($($name:expr => $pos:expr),*) => {
            vec![$(crate::ast::FnArg {
                pos: $pos,
                name: string!($name),
                typ: None
            }),*]
        };
    }

    macro_rules! assert_expr {
        ($src:expr, $want:expr) => {{
            let src = crate::source::Code::from($src);
            let mut parser = crate::parser::Parser::new(&src, false, vec![]);
            parser.advance().expect("failed to parse first token");
            let got = parser.parse_expr(0).unwrap();
            assert_eq!($want, got);
        }};
    }

    macro_rules! assert_type {
        ($src:expr, $want:expr) => {{
            let src = crate::source::Code::from($src);
            let mut parser = crate::parser::Parser::new(&src, false, vec![]);
            parser.advance().expect("failed to parse first token");
            let got = parser.parse_type().unwrap();
            assert_eq!($want, got);
        }};
    }

    macro_rules! assert_expr_str {
        ($src:expr, $want:expr) => {{
            let src = crate::source::Code::from($src);
            let mut parser = crate::parser::Parser::new(&src, false, vec![]);
            parser.advance().expect("failed to parse first token");
            let got = format!("{}", parser.parse_expr(0).unwrap());
            assert_eq!($want, got);
        }};
        ($src:expr; ERROR) => {{
            let src = crate::source::Code::from($src);
            let mut parser = crate::parser::Parser::new(&src, false);
            assert!(parser.parse_expr(0).is_err());
        }};
    }

    #[test]
    fn test_operator_binding_power() {
        // selectors, indexes, and calls
        assert_expr_str!("a", "a");
        assert_expr_str!("a.b", "(a.b)");
        assert_expr_str!("a[b]", "(a[b])");
        assert_expr_str!("a.b.c", "((a.b).c)");
        assert_expr_str!("a[b][c]", "((a[b])[c])");
        assert_expr_str!("a.b[c]", "((a.b)[c])");
        assert_expr_str!("a[b].c", "((a[b]).c)");
        assert_expr_str!("a()[b]", "((a())[b])");
        assert_expr_str!("a().b", "((a()).b)");
        assert_expr_str!("a[b]()", "((a[b])())");
        assert_expr_str!("a.b()", "((a.b)())");
        assert_expr_str!("a.b().c", "(((a.b)()).c)");
        assert_expr_str!("a[b]().c", "(((a[b])()).c)");
        assert_expr_str!("a.b()[c]", "(((a.b)())[c])");
        assert_expr_str!("a()[b]()[c]", "((((a())[b])())[c])");
        assert_expr_str!("a().b().c", "((((a()).b)()).c)");
        assert_expr_str!("a()[b().c]", "((a())[((b()).c)])");
        assert_expr_str!("a[b().c]", "(a[((b()).c)])");

        // selectors, indexes, and calls as operand
        assert_expr_str!("a - 1", "(a - 1)");
        assert_expr_str!("a.b - 1", "((a.b) - 1)");
        assert_expr_str!("a[b] - 1", "((a[b]) - 1)");
        assert_expr_str!("a.b.c - 1", "(((a.b).c) - 1)");
        assert_expr_str!("a[b][c] - 1", "(((a[b])[c]) - 1)");
        assert_expr_str!("a.b[c] - 1", "(((a.b)[c]) - 1)");
        assert_expr_str!("a[b].c - 1", "(((a[b]).c) - 1)");
        assert_expr_str!("a()[b] - 1", "(((a())[b]) - 1)");
        assert_expr_str!("a().b - 1", "(((a()).b) - 1)");
        assert_expr_str!("a[b]() - 1", "(((a[b])()) - 1)");
        assert_expr_str!("a.b() - 1", "(((a.b)()) - 1)");
        assert_expr_str!("a.b().c - 1", "((((a.b)()).c) - 1)");
        assert_expr_str!("a[b]().c - 1", "((((a[b])()).c) - 1)");
        assert_expr_str!("a.b()[c] - 1", "((((a.b)())[c]) - 1)");
        assert_expr_str!("a()[b]()[c] - 1", "(((((a())[b])())[c]) - 1)");
        assert_expr_str!("a().b().c - 1", "(((((a()).b)()).c) - 1)");
        assert_expr_str!("a()[b().c] - 1", "(((a())[((b()).c)]) - 1)");
        assert_expr_str!("a[b().c] - 1", "((a[((b()).c)]) - 1)");

        // selectors, indexes, and calls with prefix
        assert_expr_str!("-a", "(-a)");
        assert_expr_str!("-a.b", "(-(a.b))");
        assert_expr_str!("-a[b]", "(-(a[b]))");
        assert_expr_str!("-a.b.c", "(-((a.b).c))");
        assert_expr_str!("-a[b][c]", "(-((a[b])[c]))");
        assert_expr_str!("-a.b[c]", "(-((a.b)[c]))");
        assert_expr_str!("-a[b].c", "(-((a[b]).c))");
        assert_expr_str!("-a()[b]", "(-((a())[b]))");
        assert_expr_str!("-a().b", "(-((a()).b))");
        assert_expr_str!("-a[b]()", "(-((a[b])()))");
        assert_expr_str!("-a.b()", "(-((a.b)()))");
        assert_expr_str!("-a.b().c", "(-(((a.b)()).c))");
        assert_expr_str!("-a[b]().c", "(-(((a[b])()).c))");
        assert_expr_str!("-a.b()[c]", "(-(((a.b)())[c]))");
        assert_expr_str!("-a()[b]()[c]", "(-((((a())[b])())[c]))");
        assert_expr_str!("-a().b().c", "(-((((a()).b)()).c))");
        assert_expr_str!("-a()[b().c]", "(-((a())[((b()).c)]))");
        assert_expr_str!("-a[b().c]", "(-(a[((b()).c)]))");

        // selectors, indexes, and calls with prefix as operand
        assert_expr_str!("-a + 1", "((-a) + 1)");
        assert_expr_str!("-a.b + 1", "((-(a.b)) + 1)");
        assert_expr_str!("-a[b] + 1", "((-(a[b])) + 1)");
        assert_expr_str!("-a.b.c + 1", "((-((a.b).c)) + 1)");
        assert_expr_str!("-a[b][c] + 1", "((-((a[b])[c])) + 1)");
        assert_expr_str!("-a.b[c] + 1", "((-((a.b)[c])) + 1)");
        assert_expr_str!("-a[b].c + 1", "((-((a[b]).c)) + 1)");
        assert_expr_str!("-a()[b] + 1", "((-((a())[b])) + 1)");
        assert_expr_str!("-a().b + 1", "((-((a()).b)) + 1)");
        assert_expr_str!("-a[b]() + 1", "((-((a[b])())) + 1)");
        assert_expr_str!("-a.b() + 1", "((-((a.b)())) + 1)");
        assert_expr_str!("-a.b().c + 1", "((-(((a.b)()).c)) + 1)");
        assert_expr_str!("-a[b]().c + 1", "((-(((a[b])()).c)) + 1)");
        assert_expr_str!("-a.b()[c] + 1", "((-(((a.b)())[c])) + 1)");
        assert_expr_str!("-a()[b]()[c] + 1", "((-((((a())[b])())[c])) + 1)");
        assert_expr_str!("-a().b().c + 1", "((-((((a()).b)()).c)) + 1)");
        assert_expr_str!("-a()[b().c] + 1", "((-((a())[((b()).c)])) + 1)");
        assert_expr_str!("-a[b().c] + 1", "((-(a[((b()).c)])) + 1)");

        // TODO: categorize
        assert_expr_str!("!true == false", "((!true) == false)");
        assert_expr_str!("!1-1 == false", "(((!1) - 1) == false)");
        assert_expr_str!("!-1 == false", "((!(-1)) == false)");
        assert_expr_str!("-!1 == false", "((-(!1)) == false)");
        assert_expr_str!("a ? b ? c : d : e", "(a ? (b ? c : d) : e)");
        assert_expr_str!(
            "a == a ? b == b ? c == c ? d : e : f : g",
            "((a == a) ? ((b == b) ? ((c == c) ? d : e) : f) : g)"
        );
        assert_expr_str!("- typeof a", "(-(typeof a))");
        assert_expr_str!("typeof a", "(typeof a)");
        assert_expr_str!("typeof -a", "(typeof (-a))");
        assert_expr_str!("typeof !a", "(typeof (!a))");
        assert_expr_str!("typeof a+b", "(typeof (a + b))");
        assert_expr_str!("typeof -a+b", "(typeof ((-a) + b))");
        assert_expr_str!(
            "- typeof a+b == 'int'",
            "((-(typeof (a + b))) == \"int\")"
        );
        assert_expr_str!(
            "typeof a+b == 'int'",
            "((typeof (a + b)) == \"int\")"
        );
        assert_expr_str!(
            "typeof a[b](c).d + e == typeof !q + f(g).h[i]",
            "((typeof ((((a[b])(c)).d) + e)) == (typeof ((!q) + \
             (((f(g)).h)[i]))))"
        );
    }

    #[test]
    fn test_types() {
        assert_type!("named", Type::Named {
            ident: Ident {
                pos:  pos!(0, 5),
                name: string!("named"),
            },
        });
        assert_type!("?named", Type::Nullable {
            question: pos!(0, 1),
            element:  Box::new(Type::Named {
                ident: Ident {
                    pos:  pos!(1, 5),
                    name: string!("named"),
                },
            }),
        });
        assert_type!("a|b|c", Type::Union {
            alts: vec![
                Type::Named {
                    ident: Ident {
                        pos:  pos!(0, 1),
                        name: string!("a"),
                    },
                },
                Type::Named {
                    ident: Ident {
                        pos:  pos!(2, 1),
                        name: string!("b"),
                    },
                },
                Type::Named {
                    ident: Ident {
                        pos:  pos!(4, 1),
                        name: string!("c"),
                    },
                }
            ],
        });
        assert_type!("[a, b]", Type::Tuple {
            osquare: pos!(0, 1),
            fields:  vec![
                Type::Named {
                    ident: Ident {
                        pos:  pos!(1, 1),
                        name: string!("a"),
                    },
                },
                Type::Named {
                    ident: Ident {
                        pos:  pos!(4, 1),
                        name: string!("b"),
                    },
                }
            ],
            csquare: pos!(5, 1),
        });
        assert_type!("{a: a, b: b}", Type::Object {
            obrace: pos!(0, 1),
            fields: vec![
                ObjTypeField {
                    key:   Ident {
                        pos:  pos!(1, 1),
                        name: string!("a"),
                    },
                    colon: pos!(2, 1),
                    typ:   Box::new(Type::Named {
                        ident: Ident {
                            pos:  pos!(4, 1),
                            name: string!("a"),
                        },
                    }),
                },
                ObjTypeField {
                    key:   Ident {
                        pos:  pos!(7, 1),
                        name: string!("b"),
                    },
                    colon: pos!(8, 1),
                    typ:   Box::new(Type::Named {
                        ident: Ident {
                            pos:  pos!(10, 1),
                            name: string!("b"),
                        },
                    }),
                }
            ],
            cbrace: pos!(11, 1),
        });
        assert_type!("array[a]", Type::Array {
            kwarray: pos!(0, 5),
            osquare: pos!(5, 1),
            element: Box::new(Type::Named {
                ident: Ident {
                    pos:  pos!(6, 1),
                    name: string!("a"),
                },
            }),
            csquare: pos!(7, 1),
        });
        assert_type!("map[a]", Type::Map {
            kwmap:   pos!(0, 3),
            osquare: pos!(3, 1),
            element: Box::new(Type::Named {
                ident: Ident {
                    pos:  pos!(4, 1),
                    name: string!("a"),
                },
            }),
            csquare: pos!(5, 1),
        });
    }

    #[test]
    fn test_expressions() {
        assert_expr!("1", Expr::LitInt {
            pos: pos!(0, 1),
            val: 1,
        });
        assert_expr!("1.5e10", Expr::LitFlt {
            pos: pos!(0, 6),
            val: 1.5e10,
        });
        assert_expr!("true", Expr::LitBool {
            pos: pos!(0, 4),
            val: true,
        });
        assert_expr!("false", Expr::LitBool {
            pos: pos!(0, 5),
            val: false,
        });
        assert_expr!("''", Expr::LitStr {
            pos: pos!(0, 2),
            val: string!(""),
        });
        assert_expr!("'abc'", Expr::LitStr {
            pos: pos!(0, 5),
            val: string!("abc"),
        });
        assert_expr!("'\\n\\t'", Expr::LitStr {
            pos: pos!(0, 6),
            val: string!("\n\t"),
        });
        assert_expr!("'\\''", Expr::LitStr {
            pos: pos!(0, 4),
            val: string!("'"),
        });
        assert_expr!("[]", Expr::LitArr {
            osquare:  pos!(0, 1),
            elements: vec![],
            csquare:  pos!(1, 1),
        });
        assert_expr!("[1]", Expr::LitArr {
            osquare:  pos!(0, 1),
            elements: vec![Expr::LitInt {
                pos: pos!(1, 1),
                val: 1,
            }],
            csquare:  pos!(2, 1),
        });
        assert_expr!("fn () {}", Expr::LitFunc {
            kwfn:       pos!(0, 2),
            oparen:     pos!(3),
            args:       args!(),
            cparen:     pos!(4),
            ret_typ:    None,
            obrace:     pos!(6),
            body:       vec![],
            cbrace:     pos!(7),
            is_closure: true, // always true in parse tests
        });
        assert_expr!("fn (a, b, c) {}", Expr::LitFunc {
            kwfn:       pos!(0, 2),
            oparen:     pos!(3),
            args:       args!(
                "a" => pos!(4),
                "b" => pos!(7),
                "c" => pos!(10)
            ),
            cparen:     pos!(11),
            ret_typ:    None,
            obrace:     pos!(13),
            body:       vec![],
            cbrace:     pos!(14),
            is_closure: true, // always true in parse tests
        });
        assert_expr!("a()", Expr::Call {
            expr:   expr!(Expr::Ident(Ident {
                pos:  pos!(0, 1),
                name: string!("a"),
            })),
            oparen: pos!(1, 1),
            args:   vec![],
            cparen: pos!(2, 1),
        });
        assert_expr!("(a)", Expr::Paren {
            oparen: pos!(0, 1),
            expr:   expr!(Expr::Ident(Ident {
                pos:  pos!(1, 1),
                name: string!("a"),
            })),
            cparen: pos!(2, 1),
        });
        assert_expr!("-1", Expr::Unary {
            op_pos: pos!(0, 1),
            op_typ: TokenType::OpSub,
            expr:   expr!(Expr::LitInt {
                pos: pos!(1),
                val: 1,
            }),
        });
        assert_expr!("1+1", Expr::Binary {
            lhs:    expr!(Expr::LitInt {
                pos: pos!(0),
                val: 1,
            }),
            op_pos: pos!(1, 1),
            op_typ: TokenType::OpAdd,
            rhs:    expr!(Expr::LitInt {
                pos: pos!(2),
                val: 1,
            }),
        });
        assert_expr!("true ? 0 : 1", Expr::Ternary {
            cond:     expr!(Expr::LitBool {
                pos: pos!(0, 4),
                val: true,
            }),
            question: pos!(5, 1),
            pass:     expr!(Expr::LitInt {
                pos: pos!(7, 1),
                val: 0,
            }),
            colon:    pos!(9, 1),
            fail:     expr!(Expr::LitInt {
                pos: pos!(11, 1),
                val: 1,
            }),
        });
        assert_expr!("true ? true ? 1 : 0 : 1", Expr::Ternary {
            cond:     expr!(Expr::LitBool {
                pos: pos!(0, 4),
                val: true,
            }),
            question: pos!(5),
            pass:     expr!(Expr::Ternary {
                cond:     expr!(Expr::LitBool {
                    pos: pos!(7, 4),
                    val: true,
                }),
                question: pos!(12),
                pass:     expr!(Expr::LitInt {
                    pos: pos!(14),
                    val: 1,
                }),
                colon:    pos!(16),
                fail:     expr!(Expr::LitInt {
                    pos: pos!(18),
                    val: 0,
                }),
            }),
            colon:    pos!(20),
            fail:     expr!(Expr::LitInt {
                pos: pos!(22),
                val: 1,
            }),
        });
        assert_expr!("true ? 1 : false ? 0 : 1", Expr::Ternary {
            cond:     expr!(Expr::LitBool {
                pos: pos!(0, 4),
                val: true,
            }),
            question: pos!(5),
            pass:     expr!(Expr::LitInt {
                pos: pos!(7),
                val: 1,
            }),
            colon:    pos!(9),
            fail:     expr!(Expr::Ternary {
                cond:     expr!(Expr::LitBool {
                    pos: pos!(11, 5),
                    val: false,
                }),
                question: pos!(17),
                pass:     expr!(Expr::LitInt {
                    pos: pos!(19),
                    val: 0,
                }),
                colon:    pos!(21),
                fail:     expr!(Expr::LitInt {
                    pos: pos!(23),
                    val: 1,
                }),
            }),
        })
    }
}
