use super::{
    errors::Error,
    lexer::Lexer,
    source::{
        Code,
        Pos,
    },
    token::{
        Token,
        TokenType,
    },
};
use crate::ast::{
    ElseBlock,
    Expr,
    FnArg,
    IfBlock,
    LetTarget,
    Node,
    ObjDestructItem,
    ObjField,
    ObjKey,
    Stmt,
};
use std::{
    cell::RefCell,
    collections::HashMap,
    rc::Rc,
};

struct ParseScope {
    parent:   Option<Rc<ParseScope>>,
    decls:    RefCell<HashMap<String, Pos>>,
    captures: RefCell<Vec<String>>,
}

impl ParseScope {
    fn global() -> Rc<Self> {
        let s = ParseScope::create(None);
        s.declare("print".into(), Pos::mark(0)).unwrap();
        s.declare("do".into(), Pos::mark(0)).unwrap();
        s
    }

    fn create(parent: Option<Rc<ParseScope>>) -> Rc<Self> {
        Rc::from(ParseScope {
            parent,
            decls: RefCell::new(HashMap::new()),
            captures: RefCell::new(Vec::new()),
        })
    }

    fn get_local_decl(&self, name: &String) -> Option<Pos> {
        self.decls.borrow().get(name).map(|pos| *pos)
    }

    fn declare(&self, name: String, at: Pos) -> Result<(), Error> {
        if let Some(original) = self.get_local_decl(&name) {
            return Err(Error::Redeclaration { at, original, name });
        }
        self.decls.borrow_mut().insert(name, at);
        Ok(())
    }

    fn utilize(&self, name: &String, pos: Pos) -> Result<(), Error> {
        if self.get_local_decl(name).is_some() {
            // found in local scope
            return Ok(());
        }

        if let Some(parent) = &self.parent {
            // if the name is found several scopes up, we need to capture it in
            // all of the intermediate scopes, too.
            parent.utilize(name, pos)?;
            // if we get here, it was found in or above the parent scope,
            // and we need to capture the name
            self.captures.borrow_mut().push(name.clone());
            return Ok(());
        }

        Err(Error::Undeclared {
            at:   pos,
            name: name.clone(),
        })
    }
}

pub struct Parser {
    lex:    Lexer,
    cur_t:  Token,
    peek_t: Token,
    scope:  Rc<ParseScope>,
}

impl Parser {
    pub fn new(src: &Code) -> Self {
        let mut p = Parser {
            lex:    Lexer::new(src),
            cur_t:  Token::new_meta(0, TokenType::EOF),
            peek_t: Token::new_meta(0, TokenType::EOF),
            scope:  ParseScope::create(Some(ParseScope::global())),
        };
        p.advance().expect("fatal error");
        p
    }

    pub fn parse_stmt_list(
        &mut self,
        terminators: Vec<TokenType>,
    ) -> Result<Vec<Stmt>, Error> {
        let mut stmts: Vec<Stmt> = Vec::with_capacity(3);
        while !terminators.contains(&self.peek_t.typ) {
            stmts.push(self.parse_stmt()?);
        }
        Ok(stmts)
    }

    fn parse_stmt(&mut self) -> Result<Stmt, Error> {
        match self.peek_t.typ {
            TokenType::KwLet => self.parse_let_stmt(),
            TokenType::KwFn => self.parse_fndef_stmt(),
            TokenType::KwIf => self.parse_if_stmt(),
            TokenType::KwReturn => self.parse_return_stmt(),
            TokenType::KwThrow => self.parse_throw_stmt(),
            _ => self.parse_expr_or_assignment_stmt(),
        }
    }

    fn parse_let_stmt(&mut self) -> Result<Stmt, Error> {
        self.expect(TokenType::KwLet)?;
        let kwlet = self.cur_t.pos;
        let (target, target_pos) = self.parse_let_target()?;
        let assign = self.expect(TokenType::Assign)?.pos;
        let expr = Box::new(self.parse_expr(0)?);
        let semi = self.expect(TokenType::Semi)?.pos;
        Ok(Stmt::Let {
            kwlet,
            target,
            target_pos,
            assign,
            expr,
            semi,
        })
    }

    fn parse_let_target(&mut self) -> Result<(LetTarget, Pos), Error> {
        match self.peek_t.typ {
            TokenType::Ident => {
                let Token { lit: name, pos, .. } =
                    self.expect(TokenType::Ident)?;
                self.scope.declare(name.clone(), pos)?;
                Ok((LetTarget::Ident(name), pos))
            }
            TokenType::OSquare => {
                self.advance()?;
                let start = self.cur_t.pos.offset;
                let mut names = Vec::<String>::new();
                while self.peek_t.typ != TokenType::CSquare {
                    if names.len() > 0 {
                        self.expect(TokenType::Comma)?;
                    }
                    let Token { lit: name, pos, .. } =
                        self.expect(TokenType::Ident)?;
                    self.scope.declare(name.clone(), pos)?;
                    names.push(name);
                }
                let end_pos = self.expect(TokenType::CSquare)?.pos;
                let end = end_pos.offset + end_pos.length;
                Ok((
                    LetTarget::ArrDestruct(names),
                    Pos::span(start, end - start),
                ))
            }
            TokenType::OBrace => {
                self.advance()?;
                let start = self.cur_t.pos.offset;
                let mut items = Vec::<ObjDestructItem>::new();
                while self.peek_t.typ != TokenType::CBrace {
                    if items.len() > 0 {
                        self.expect(TokenType::Comma)?;
                    }
                    items.push(self.parse_object_destruct_item()?);
                }
                let end_pos = self.expect(TokenType::CBrace)?.pos;
                let end = end_pos.offset + end_pos.length;
                Ok((
                    LetTarget::ObjDestruct(items),
                    Pos::span(start, end - start),
                ))
            }
            _ => {
                Err(Error::Expected {
                    at:     self.peek_t.pos,
                    wanted: "ident, array destructure, or object destructure"
                        .into(),
                    found:  format!("{}", self.peek_t.typ),
                })
            }
        }
    }

    fn parse_object_destruct_item(&mut self) -> Result<ObjDestructItem, Error> {
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
                    self.scope.declare(name.clone(), pos)?;
                    return Ok(ObjDestructItem::NameMap(name_or_key, name));
                }
                self.scope.declare(name_or_key.clone(), pos)?;
                Ok(ObjDestructItem::Name(name_or_key))
            }
            TokenType::LitString => {
                // can only be NameMap
                self.advance()?;
                let key = self.cur_t.lit.clone();
                self.expect(TokenType::Colon)?;
                let Token { pos, lit: name, .. } =
                    self.expect(TokenType::Ident)?;
                self.scope.declare(name.clone(), pos)?;
                Ok(ObjDestructItem::NameMap(key, name))
            }
            _ => {
                Err(Error::Expected {
                    at:     self.peek_t.pos,
                    wanted: "object destrucruting item".into(),
                    found:  format!("{}", self.peek_t.typ),
                })
            }
        }
    }

    fn parse_fndef_stmt(&mut self) -> Result<Stmt, Error> {
        let kwfn = self.expect(TokenType::KwFn)?.pos;
        let ident = self.expect(TokenType::Ident)?;
        let ident_name = ident.lit;
        let ident_pos = ident.pos;
        self.scope.declare(ident_name.clone(), ident_pos)?;
        self.scope = ParseScope::create(Some(self.scope.clone()));
        let oparen = self.expect(TokenType::OParen)?.pos;
        let args = self.parse_fnarg_list()?;
        for arg in args.iter() {
            self.scope.declare(arg.name.clone(), arg.pos)?;
        }
        let cparen = self.expect(TokenType::CParen)?.pos;
        let obrace = self.expect(TokenType::OBrace)?.pos;
        let body = self.parse_stmt_list(vec![TokenType::CBrace])?;
        let cbrace = self.expect(TokenType::CBrace)?.pos;
        let stmt = Stmt::FnDef {
            kwfn,
            ident_pos,
            ident_name,
            oparen,
            args,
            cparen,
            obrace,
            body,
            cbrace,
            is_closure: !self.scope.captures.borrow().is_empty(),
        };
        self.scope = self.scope.parent.clone().unwrap();
        Ok(stmt)
    }

    fn parse_if_stmt(&mut self) -> Result<Stmt, Error> {
        let mut head = Vec::<IfBlock>::with_capacity(1);
        head.push(IfBlock {
            kw_typ: TokenType::KwIf,
            kw_pos: self.expect(TokenType::KwIf)?.pos,
            cond:   Box::new(self.parse_expr(0)?),
            obrace: self.expect(TokenType::OBrace)?.pos,
            body:   self.parse_stmt_list(vec![TokenType::CBrace])?,
            cbrace: self.expect(TokenType::CBrace)?.pos,
        });
        while self.peek_t.typ == TokenType::KwElif {
            head.push(IfBlock {
                kw_typ: TokenType::KwElif,
                kw_pos: self.expect(TokenType::KwElif)?.pos,
                cond:   Box::new(self.parse_expr(0)?),
                obrace: self.expect(TokenType::OBrace)?.pos,
                body:   self.parse_stmt_list(vec![TokenType::CBrace])?,
                cbrace: self.expect(TokenType::CBrace)?.pos,
            });
        }
        Ok(Stmt::If {
            head,
            tail: if self.peek_t.typ == TokenType::KwElse {
                Some(ElseBlock {
                    kwelse: self.expect(TokenType::KwElse)?.pos,
                    obrace: self.expect(TokenType::OBrace)?.pos,
                    body:   self.parse_stmt_list(vec![TokenType::CBrace])?,
                    cbrace: self.expect(TokenType::CBrace)?.pos,
                })
            } else {
                None
            },
        })
    }

    fn parse_return_stmt(&mut self) -> Result<Stmt, Error> {
        let kwreturn = self.expect(TokenType::KwReturn)?.pos;
        Ok(Stmt::Return {
            kwreturn,
            expr: Box::new(self.parse_expr(0)?),
            semi: self.expect(TokenType::Semi)?.pos,
        })
    }

    fn parse_throw_stmt(&mut self) -> Result<Stmt, Error> {
        let kwthrow = self.expect(TokenType::KwThrow)?.pos;
        Ok(Stmt::Throw {
            kwthrow,
            error: Box::new(self.parse_expr(0)?),
            semi: self.expect(TokenType::Semi)?.pos,
        })
    }

    fn parse_expr_or_assignment_stmt(&mut self) -> Result<Stmt, Error> {
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
            return Err(Error::InvalidAssignment {
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
        terminators: Vec<TokenType>,
    ) -> Result<Vec<Expr>, Error> {
        let mut exprs: Vec<Expr> = Vec::with_capacity(3);
        while !terminators.contains(&self.peek_t.typ) {
            if exprs.len() > 0 {
                self.expect(TokenType::Comma)?;
            }
            exprs.push(self.parse_expr(0)?);
        }
        Ok(exprs)
    }

    fn parse_expr(&mut self, precedence: i32) -> Result<Expr, Error> {
        let mut x: Expr = match self.peek_t.typ {
            TokenType::Bang | TokenType::OpSub | TokenType::OpFeed => {
                self.advance()?;
                Expr::Unary {
                    op_pos: self.cur_t.pos,
                    op_typ: self.cur_t.typ,
                    expr:   Box::new(self.parse_primary_expr()?),
                }
            }
            _ => self.parse_primary_expr()?,
        };

        while self.peek_t.typ.is_operator() {
            let (lbp, rbp) = self.peek_t.typ.precedence();
            if lbp < precedence {
                break;
            }
            match self.peek_t.typ {
                TokenType::OParen => {
                    x = self.parse_call(x)?;
                }
                TokenType::Question => {
                    x = self.parse_ternary(x)?;
                }
                TokenType::OSquare => {
                    x = self.parse_index(x)?;
                }
                TokenType::Dot => {
                    x = self.parse_selector_expr(x)?;
                }
                _ => {
                    self.advance()?;
                    x = Expr::Binary {
                        lhs:    Box::new(x),
                        op_pos: self.cur_t.pos,
                        op_typ: self.cur_t.typ,
                        rhs:    Box::new(self.parse_expr(rbp)?),
                    };
                }
            }
        }
        Ok(x)
    }

    fn parse_primary_expr(&mut self) -> Result<Expr, Error> {
        self.advance()?;
        match self.cur_t.typ {
            TokenType::KwNull => {
                Ok(Expr::LitNull {
                    pos: self.cur_t.pos,
                })
            }
            TokenType::LitInt => {
                Ok(Expr::LitInt {
                    pos: self.cur_t.pos,
                    val: self.cur_t.lit.parse().expect("illegal int value"),
                })
            }
            TokenType::LitFloat => {
                Ok(Expr::LitFlt {
                    pos: self.cur_t.pos,
                    val: self.cur_t.lit.parse().expect("illegal float value"),
                })
            }
            TokenType::KwTrue | TokenType::KwFalse => {
                Ok(Expr::LitBool {
                    pos: self.cur_t.pos,
                    val: self.cur_t.typ == TokenType::KwTrue,
                })
            }
            TokenType::LitString => {
                Ok(Expr::LitStr {
                    pos: self.cur_t.pos,
                    val: self.cur_t.lit.clone(),
                })
            }
            TokenType::KwFn => {
                self.scope = ParseScope::create(Some(self.scope.clone()));
                let kwfn = self.cur_t.pos;
                let oparen = self.expect(TokenType::OParen)?.pos;
                let args = self.parse_fnarg_list()?;
                for arg in args.iter() {
                    self.scope.declare(arg.name.clone(), arg.pos)?;
                }
                let cparen = self.expect(TokenType::CParen)?.pos;
                let obrace = self.expect(TokenType::OBrace)?.pos;
                let body = self.parse_stmt_list(vec![TokenType::CBrace])?;
                let cbrace = self.expect(TokenType::CBrace)?.pos;
                let expr = Expr::LitFunc {
                    kwfn,
                    oparen,
                    args,
                    cparen,
                    obrace,
                    body,
                    cbrace,
                    is_closure: !self.scope.captures.borrow().is_empty(),
                };
                self.scope = self.scope.parent.clone().unwrap();
                Ok(expr)
            }
            TokenType::Ident => {
                let pos = self.cur_t.pos;
                let name = self.cur_t.lit.clone();
                self.scope.utilize(&name, pos)?;
                Ok(Expr::Ident { pos, name })
            }
            TokenType::OParen => {
                Ok(Expr::Paren {
                    oparen: self.cur_t.pos,
                    expr:   Box::new(self.parse_expr(0)?),
                    cparen: self.expect(TokenType::CParen)?.pos,
                })
            }
            TokenType::OSquare => {
                Ok(Expr::LitArr {
                    osquare:  self.cur_t.pos,
                    elements: self.parse_expr_list(vec![TokenType::CSquare])?,
                    csquare:  self.expect(TokenType::CSquare)?.pos,
                })
            }
            TokenType::OBrace => {
                Ok(Expr::LitObj {
                    obrace: self.cur_t.pos,
                    fields: self.parse_field_list()?,
                    cbrace: self.expect(TokenType::CBrace)?.pos,
                })
            }
            _ => {
                Err(Error::Expected {
                    at:     self.cur_t.pos,
                    wanted: "expression".into(),
                    found:  format!("{:?}", self.cur_t.typ),
                })
            }
        }
    }

    fn parse_call(&mut self, expr: Expr) -> Result<Expr, Error> {
        let oparen = self.expect(TokenType::OParen)?.pos;
        let args = self.parse_expr_list(vec![TokenType::CParen])?;
        let cparen = self.expect(TokenType::CParen)?.pos;
        Ok(Expr::Call {
            expr: Box::from(expr),
            oparen,
            args,
            cparen,
        })
    }

    fn parse_fnarg_list(&mut self) -> Result<Vec<FnArg>, Error> {
        let mut args = Vec::<FnArg>::with_capacity(1);
        loop {
            if self.peek_t.typ == TokenType::Ident {
                self.advance()?;
                args.push(FnArg {
                    pos:  self.cur_t.pos,
                    name: self.cur_t.lit.clone(),
                })
            }
            if args.len() > 0 && self.peek_t.typ == TokenType::Comma {
                self.advance()?;
                continue;
            }
            break;
        }
        Ok(args)
    }

    fn parse_ternary(&mut self, cond: Expr) -> Result<Expr, Error> {
        let (_, rbp) = TokenType::Question.precedence();
        Ok(Expr::Ternary {
            cond:     Box::new(cond),
            question: self.expect(TokenType::Question)?.pos,
            pass:     Box::new(self.parse_expr(rbp)?),
            colon:    self.expect(TokenType::Colon)?.pos,
            fail:     Box::new(self.parse_expr(0)?),
        })
    }

    fn parse_index(&mut self, expr: Expr) -> Result<Expr, Error> {
        Ok(Expr::Index {
            expr:    Box::from(expr),
            osquare: self.expect(TokenType::OSquare)?.pos,
            index:   Box::new(self.parse_expr(0)?),
            csquare: self.expect(TokenType::CSquare)?.pos,
        })
    }

    fn parse_selector_expr(&mut self, expr: Expr) -> Result<Expr, Error> {
        let dot = self.expect(TokenType::Dot)?.pos;
        let element_ident = self.expect(TokenType::Ident)?;
        Ok(Expr::Selector {
            expr: Box::from(expr),
            dot,
            elt_name: element_ident.lit,
            elt_pos: element_ident.pos,
        })
    }

    fn parse_field_list(&mut self) -> Result<Vec<ObjField>, Error> {
        let mut fields = Vec::<ObjField>::new();
        loop {
            match self.peek_t.typ {
                TokenType::CBrace => {
                    break;
                }
                TokenType::OParen => {
                    self.advance()?;
                    fields.push(ObjField {
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
                    fields.push(ObjField {
                        key:   ObjKey::Static(
                            self.cur_t.lit.clone(),
                            self.cur_t.pos,
                        ),
                        colon: self.expect(TokenType::Colon)?.pos,
                        val:   Box::new(self.parse_expr(0)?),
                    });
                }
                _ => {
                    return Err(Error::Expected {
                        at:     self.peek_t.pos,
                        wanted: "field key".into(),
                        found:  format!("{:?}", self.peek_t.typ),
                    })
                }
            }
            if self.peek_t.typ == TokenType::CBrace {
                break;
            }
            self.expect(TokenType::Comma)?;
        }
        Ok(fields)
    }

    fn advance(&mut self) -> Result<(), Error> {
        self.cur_t = std::mem::replace(&mut self.peek_t, self.lex.next()?);
        Ok(())
    }

    fn expect(&mut self, expected: TokenType) -> Result<Token, Error> {
        self.advance()?;
        if self.cur_t.typ != expected {
            return Err(Error::Expected {
                at:     self.cur_t.pos,
                wanted: format!("{:?}", expected),
                found:  format!("{:?}", self.cur_t.typ),
            });
        }
        Ok(self.cur_t.clone())
    }
}

#[cfg(test)]
mod test {
    use crate::{
        ast::Expr,
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
            }),*]
        };
    }

    macro_rules! assert_expr {
        ($src:expr, $want:expr) => {{
            let src = crate::source::Code::from($src);
            let mut parser = crate::parser::Parser::new(&src);
            let got = parser.parse_expr(0).unwrap();
            assert_eq!($want, got);
        }};
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
            obrace:     pos!(6),
            body:       vec![],
            cbrace:     pos!(7),
            is_closure: false,
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
            obrace:     pos!(13),
            body:       vec![],
            cbrace:     pos!(14),
            is_closure: false,
        });
        assert_expr!("a()", Expr::Call {
            expr:   expr!(Expr::Ident {
                pos:  pos!(0, 1),
                name: string!("a"),
            }),
            oparen: pos!(1, 1),
            args:   vec![],
            cparen: pos!(2, 1),
        });
        assert_expr!("(a)", Expr::Paren {
            oparen: pos!(0, 1),
            expr:   expr!(Expr::Ident {
                pos:  pos!(1, 1),
                name: string!("a"),
            }),
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
