use super::errors::Error;
use super::lexer::Lexer;
use super::source::Code;
use super::token::{Token, TokenType};
use crate::ast::{ElseBlock, Expr, FnArg, IfBlock, Node, ObjField, Stmt};

pub struct Parser {
    lex: Lexer,
    cur_t: Token,
    peek_t: Token,
}

impl Parser {
    pub fn new(src: &Code) -> Self {
        let mut p = Parser {
            lex: Lexer::new(src),
            cur_t: Token::new_meta(0, TokenType::EOF),
            peek_t: Token::new_meta(0, TokenType::EOF),
        };
        p.advance().expect("fatal error");
        p
    }

    pub fn parse_stmt_list(&mut self, terminators: Vec<TokenType>) -> Result<Vec<Stmt>, Error> {
        let mut stmts: Vec<Stmt> = Vec::with_capacity(3);
        while !terminators.contains(&self.peek_t.typ) {
            stmts.push(self.parse_stmt()?);
        }
        Ok(stmts)
    }

    fn parse_stmt(&mut self) -> Result<Stmt, Error> {
        match self.peek_t.typ {
            TokenType::KwLet => self.parse_let_stmt(),
            TokenType::KwIf => self.parse_if_stmt(),
            TokenType::KwReturn => self.parse_return_stmt(),
            TokenType::KwThrow => self.parse_throw_stmt(),
            _ => self.parse_expr_or_assignment_stmt(),
        }
    }

    fn parse_let_stmt(&mut self) -> Result<Stmt, Error> {
        self.expect(TokenType::KwLet)?;
        let kwlet = self.cur_t.pos;
        let ident = self.expect(TokenType::Ident)?;
        let ident_pos = ident.pos;
        let ident_name = ident.lit.clone();
        let assign = self.expect(TokenType::Assign)?.pos;
        let expr = Box::new(self.parse_expr(0)?);
        let semi = self.expect(TokenType::Semi)?.pos;
        Ok(Stmt::Let {
            kwlet,
            ident_pos,
            ident_name,
            assign,
            expr,
            semi,
        })
    }

    fn parse_if_stmt(&mut self) -> Result<Stmt, Error> {
        self.expect(TokenType::KwIf)?;
        let mut head = Vec::<IfBlock>::with_capacity(1);
        head.push(IfBlock {
            kw_typ: TokenType::KwIf,
            kw_pos: self.cur_t.pos,
            cond: Box::new(self.parse_expr(0)?),
            obrace: self.expect(TokenType::OBrace)?.pos,
            body: self.parse_stmt_list(vec![TokenType::CBrace])?,
            cbrace: self.expect(TokenType::CBrace)?.pos,
        });
        while self.peek_t.typ == TokenType::KwElif {
            self.advance()?;
            head.push(IfBlock {
                kw_typ: TokenType::KwElif,
                kw_pos: self.cur_t.pos,
                cond: Box::new(self.parse_expr(0)?),
                obrace: self.expect(TokenType::OBrace)?.pos,
                body: self.parse_stmt_list(vec![TokenType::CBrace])?,
                cbrace: self.expect(TokenType::CBrace)?.pos,
            });
        }
        Ok(Stmt::If {
            head,
            tail: if self.peek_t.typ == TokenType::KwElse {
                self.advance()?;
                Some(ElseBlock {
                    kwelse: self.cur_t.pos,
                    obrace: self.expect(TokenType::OBrace)?.pos,
                    body: self.parse_stmt_list(vec![TokenType::CBrace])?,
                    cbrace: self.expect(TokenType::CBrace)?.pos,
                })
            } else {
                None
            },
        })
    }

    fn parse_return_stmt(&mut self) -> Result<Stmt, Error> {
        let kw_return = self.expect(TokenType::KwReturn)?.pos;
        Ok(Stmt::Return {
            kw_return,
            expr: Box::new(self.parse_expr(0)?),
            semi: self.expect(TokenType::Semi)?.pos,
        })
    }

    fn parse_throw_stmt(&mut self) -> Result<Stmt, Error> {
        let kw_throw = self.expect(TokenType::KwThrow)?.pos;
        Ok(Stmt::Throw {
            kw_throw,
            error: Box::new(self.parse_expr(0)?),
            semi: self.expect(TokenType::Semi)?.pos,
        })
    }

    fn parse_expr_or_assignment_stmt(&mut self) -> Result<Stmt, Error> {
        let x = self.parse_expr(0)?;
        if self.peek_t.typ == TokenType::Assign {
            if !x.is_assignable() {
                return Err(Error::InvalidAssignment {
                    at: x.pos(),
                    found: format!("{}", x),
                });
            }
            return Ok(Stmt::Assignment {
                lhs: Box::new(x),
                assign: self.expect(TokenType::Assign)?.pos,
                rhs: Box::new(self.parse_expr(0)?),
                semi: self.expect(TokenType::Semi)?.pos,
            });
        }
        Ok(Stmt::Expr {
            expr: Box::new(x),
            semi: self.expect(TokenType::Semi)?.pos,
        })
    }

    fn parse_expr_list(&mut self, terminators: Vec<TokenType>) -> Result<Vec<Expr>, Error> {
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
                    expr: Box::new(self.parse_primary_expr()?),
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
                        lhs: Box::new(x),
                        op_pos: self.cur_t.pos,
                        op_typ: self.cur_t.typ,
                        rhs: Box::new(self.parse_expr(rbp)?),
                    };
                }
            }
        }
        Ok(x)
    }

    fn parse_primary_expr(&mut self) -> Result<Expr, Error> {
        self.advance()?;
        match self.cur_t.typ {
            TokenType::KwNull => Ok(Expr::LitNull {
                pos: self.cur_t.pos,
            }),
            TokenType::LitInt => Ok(Expr::LitInt {
                pos: self.cur_t.pos,
                val: self.cur_t.lit.parse().expect("illegal int value"),
            }),
            TokenType::LitFloat => Ok(Expr::LitFlt {
                pos: self.cur_t.pos,
                val: self.cur_t.lit.parse().expect("illegal float value"),
            }),
            TokenType::KwTrue | TokenType::KwFalse => Ok(Expr::LitBool {
                pos: self.cur_t.pos,
                val: self.cur_t.typ == TokenType::KwTrue,
            }),
            TokenType::LitString => Ok(Expr::LitStr {
                pos: self.cur_t.pos,
                val: self.cur_t.lit.clone(),
            }),
            TokenType::KwFn => Ok(Expr::LitFunc {
                kwfn: self.cur_t.pos,
                oparen: self.expect(TokenType::OParen)?.pos,
                args: self.parse_fnarg_list()?,
                cparen: self.expect(TokenType::CParen)?.pos,
                obrace: self.expect(TokenType::OBrace)?.pos,
                body: self.parse_stmt_list(vec![TokenType::CBrace])?,
                cbrace: self.expect(TokenType::CBrace)?.pos,
            }),
            TokenType::Ident => Ok(Expr::Ident {
                pos: self.cur_t.pos,
                name: self.cur_t.lit.clone(),
            }),
            TokenType::OParen => Ok(Expr::Paren {
                oparen: self.cur_t.pos,
                expr: Box::new(self.parse_expr(0)?),
                cparen: self.expect(TokenType::CParen)?.pos,
            }),
            TokenType::OSquare => Ok(Expr::LitArr {
                osquare: self.cur_t.pos,
                elements: self.parse_expr_list(vec![TokenType::CSquare])?,
                csquare: self.expect(TokenType::CSquare)?.pos,
            }),
            TokenType::OBrace => Ok(Expr::LitObj {
                obrace: self.cur_t.pos,
                fields: self.parse_field_list()?,
                cbrace: self.expect(TokenType::CBrace)?.pos,
            }),
            _ => Err(Error::Expected {
                at: self.cur_t.pos,
                wanted: "expression".into(),
                found: format!("{:?}", self.cur_t.typ),
            }),
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
                    pos: self.cur_t.pos,
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
            cond: Box::new(cond),
            question: self.expect(TokenType::Question)?.pos,
            pass: Box::new(self.parse_expr(rbp)?),
            colon: self.expect(TokenType::Colon)?.pos,
            fail: Box::new(self.parse_expr(0)?),
        })
    }

    fn parse_index(&mut self, expr: Expr) -> Result<Expr, Error> {
        Ok(Expr::Index {
            expr: Box::from(expr),
            osquare: self.expect(TokenType::OSquare)?.pos,
            index: Box::new(self.parse_expr(0)?),
            csquare: self.expect(TokenType::CSquare)?.pos,
        })
    }

    fn parse_selector_expr(&mut self, expr: Expr) -> Result<Expr, Error> {
        let dot = self.expect(TokenType::Dot)?.pos;
        let element_ident = self.expect(TokenType::Ident)?;
        Ok(Expr::Selector {
            expr: Box::from(expr),
            dot,
            element: Box::from(Expr::Ident {
                pos: element_ident.pos,
                name: element_ident.lit,
            }),
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
                        key: Box::new(Expr::Paren {
                            oparen: self.cur_t.pos,
                            expr: Box::new(self.parse_expr(0)?),
                            cparen: self.expect(TokenType::CParen)?.pos,
                        }),
                        colon: self.expect(TokenType::Colon)?.pos,
                        val: Box::new(self.parse_expr(0)?),
                    });
                }
                TokenType::LitString => {
                    self.advance()?;
                    fields.push(ObjField {
                        key: Box::new(Expr::LitStr {
                            pos: self.cur_t.pos,
                            val: self.cur_t.lit.clone(),
                        }),
                        colon: self.expect(TokenType::Colon)?.pos,
                        val: Box::new(self.parse_expr(0)?),
                    });
                }
                TokenType::Ident => {
                    self.advance()?;
                    fields.push(ObjField {
                        key: Box::new(Expr::Ident {
                            pos: self.cur_t.pos,
                            name: self.cur_t.lit.clone(),
                        }),
                        colon: self.expect(TokenType::Colon)?.pos,
                        val: Box::new(self.parse_expr(0)?),
                    });
                }
                _ => {
                    return Err(Error::Expected {
                        at: self.peek_t.pos,
                        wanted: "field key".into(),
                        found: format!("{:?}", self.peek_t.typ),
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
                at: self.cur_t.pos,
                wanted: format!("{:?}", expected),
                found: format!("{:?}", self.cur_t.typ),
            });
        }
        Ok(self.cur_t.clone())
    }
}

#[cfg(test)]
mod test {
    use crate::ast::Expr;
    use crate::token::TokenType;

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
        assert_expr!(
            "1",
            Expr::LitInt {
                pos: pos!(0, 1),
                val: 1
            }
        );
        assert_expr!(
            "1.5e10",
            Expr::LitFlt {
                pos: pos!(0, 6),
                val: 1.5e10
            }
        );
        assert_expr!(
            "true",
            Expr::LitBool {
                pos: pos!(0, 4),
                val: true
            }
        );
        assert_expr!(
            "false",
            Expr::LitBool {
                pos: pos!(0, 5),
                val: false
            }
        );
        assert_expr!(
            "''",
            Expr::LitStr {
                pos: pos!(0, 2),
                val: string!(""),
            }
        );
        assert_expr!(
            "'abc'",
            Expr::LitStr {
                pos: pos!(0, 5),
                val: string!("abc")
            }
        );
        assert_expr!(
            "'\\n\\t'",
            Expr::LitStr {
                pos: pos!(0, 6),
                val: string!("\n\t")
            }
        );
        assert_expr!(
            "'\\''",
            Expr::LitStr {
                pos: pos!(0, 4),
                val: string!("'")
            }
        );
        assert_expr!(
            "[]",
            Expr::LitArr {
                osquare: pos!(0, 1),
                elements: vec![],
                csquare: pos!(1, 1),
            }
        );
        assert_expr!(
            "[1]",
            Expr::LitArr {
                osquare: pos!(0, 1),
                elements: vec![Expr::LitInt {
                    pos: pos!(1, 1),
                    val: 1
                }],
                csquare: pos!(2, 1),
            }
        );
        assert_expr!(
            "fn () {}",
            Expr::LitFunc {
                kwfn: pos!(0, 2),
                oparen: pos!(3),
                args: args!(),
                cparen: pos!(4),
                obrace: pos!(6),
                body: vec![],
                cbrace: pos!(7),
            }
        );
        assert_expr!(
            "fn (a, b, c) {}",
            Expr::LitFunc {
                kwfn: pos!(0, 2),
                oparen: pos!(3),
                args: args!(
                    "a" => pos!(4),
                    "b" => pos!(7),
                    "c" => pos!(10)
                ),
                cparen: pos!(11),
                obrace: pos!(13),
                body: vec![],
                cbrace: pos!(14),
            }
        );
        assert_expr!(
            "a()",
            Expr::Call {
                expr: expr!(Expr::Ident {
                    pos: pos!(0, 1),
                    name: string!("a")
                }),
                oparen: pos!(1, 1),
                args: vec![],
                cparen: pos!(2, 1),
            }
        );
        assert_expr!(
            "(a)",
            Expr::Paren {
                oparen: pos!(0, 1),
                expr: expr!(Expr::Ident {
                    pos: pos!(1, 1),
                    name: string!("a")
                }),
                cparen: pos!(2, 1),
            }
        );
        assert_expr!(
            "-1",
            Expr::Unary {
                op_pos: pos!(0, 1),
                op_typ: TokenType::OpSub,
                expr: expr!(Expr::LitInt {
                    pos: pos!(1),
                    val: 1
                })
            }
        );
        assert_expr!(
            "1+1",
            Expr::Binary {
                lhs: expr!(Expr::LitInt {
                    pos: pos!(0),
                    val: 1
                }),
                op_pos: pos!(1, 1),
                op_typ: TokenType::OpAdd,
                rhs: expr!(Expr::LitInt {
                    pos: pos!(2),
                    val: 1
                })
            }
        );
        assert_expr!(
            "true ? 0 : 1",
            Expr::Ternary {
                cond: expr!(Expr::LitBool {
                    pos: pos!(0, 4),
                    val: true
                }),
                question: pos!(5, 1),
                pass: expr!(Expr::LitInt {
                    pos: pos!(7, 1),
                    val: 0
                }),
                colon: pos!(9, 1),
                fail: expr!(Expr::LitInt {
                    pos: pos!(11, 1),
                    val: 1
                })
            }
        );
        assert_expr!(
            "true ? true ? 1 : 0 : 1",
            Expr::Ternary {
                cond: expr!(Expr::LitBool {
                    pos: pos!(0, 4),
                    val: true
                }),
                question: pos!(5),
                pass: expr!(Expr::Ternary {
                    cond: expr!(Expr::LitBool {
                        pos: pos!(7, 4),
                        val: true
                    }),
                    question: pos!(12),
                    pass: expr!(Expr::LitInt {
                        pos: pos!(14),
                        val: 1
                    }),
                    colon: pos!(16),
                    fail: expr!(Expr::LitInt {
                        pos: pos!(18),
                        val: 0
                    }),
                }),
                colon: pos!(20),
                fail: expr!(Expr::LitInt {
                    pos: pos!(22),
                    val: 1
                }),
            }
        );
        assert_expr!(
            "true ? 1 : false ? 0 : 1",
            Expr::Ternary {
                cond: expr!(Expr::LitBool {
                    pos: pos!(0, 4),
                    val: true
                }),
                question: pos!(5),
                pass: expr!(Expr::LitInt {
                    pos: pos!(7),
                    val: 1
                }),
                colon: pos!(9),
                fail: expr!(Expr::Ternary {
                    cond: expr!(Expr::LitBool {
                        pos: pos!(11, 5),
                        val: false,
                    }),
                    question: pos!(17),
                    pass: expr!(Expr::LitInt {
                        pos: pos!(19),
                        val: 0
                    }),
                    colon: pos!(21),
                    fail: expr!(Expr::LitInt {
                        pos: pos!(23),
                        val: 1
                    }),
                }),
            }
        )
    }
}
