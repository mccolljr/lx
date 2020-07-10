use super::{
    errors::Error,
    source::{
        Code,
        Pos,
    },
    token::{
        Token,
        TokenType,
        TokenType::*,
    },
};
use std::iter::FromIterator;

pub struct Lexer {
    src:    Code,
    cur_i:  usize,
    cur_c:  char,
    peek_i: usize,
    peek_c: char,
}

impl Lexer {
    pub fn new(src: &Code) -> Self {
        Lexer {
            src:    src.clone(),
            cur_i:  0,
            cur_c:  '\0',
            peek_i: 0,
            peek_c: if src.len() > 0 { src[0] } else { '\0' },
        }
    }

    pub fn next(&mut self) -> Result<Token, Error> {
        self.advance();
        while self.cur_c.is_ascii_whitespace() {
            self.advance()
        }

        if self.at_eof() {
            return Ok(Token::new_meta(self.cur_i, EOF));
        }

        if self.cur_c.is_ascii_alphabetic() || self.cur_c == '_' {
            return self.lex_bool_ident_or_keyword();
        }

        if self.cur_c.is_ascii_digit() {
            return self.lex_number();
        }

        if ['\'', '"'].contains(&self.cur_c) {
            return self.lex_string();
        }

        match self.cur_c {
            '+' => self.lex_1(OpAdd),
            '-' => self.lex_1(OpSub),
            '*' => self.lex_1(OpMul),
            '/' => self.lex_1(OpDiv),
            '%' => self.lex_1(OpRem),
            '(' => self.lex_1(OParen),
            ')' => self.lex_1(CParen),
            '{' => self.lex_1(OBrace),
            '}' => self.lex_1(CBrace),
            '[' => self.lex_1(OSquare),
            ']' => self.lex_1(CSquare),
            ',' => self.lex_1(Comma),
            ';' => self.lex_1(Semi),
            '?' => self.lex_1(Question),
            ':' => self.lex_1(Colon),
            '.' => self.lex_1(Dot),
            '>' => self.lex_2(&[('=', OpGeq)], OpGt),
            '<' => self.lex_2(&[('=', OpLeq)], OpLt),
            '=' => self.lex_2(&[('=', OpEq)], Assign),
            '!' => self.lex_2(&[('=', OpNeq)], Bang),
            _ => {
                Err(Error::InvalidCharacter {
                    at: Pos::one(self.cur_i),
                    ch: self.cur_c,
                })
            }
        }
    }

    fn advance(&mut self) {
        if self.at_eof() {
            return;
        }
        self.cur_c = self.peek_c;
        self.cur_i = self.peek_i;
        self.peek_i += 1;
        self.peek_c = if self.peek_eof() {
            '\0'
        } else {
            self.src[self.peek_i]
        };
    }

    fn at_eof(&self) -> bool { self.cur_i >= self.src.len() }

    fn peek_eof(&self) -> bool { self.peek_i >= self.src.len() }

    #[inline]
    fn lex_1(&self, typ: TokenType) -> Result<Token, Error> {
        Ok(Token::new(
            Pos::one(self.cur_i),
            typ,
            self.src[self.cur_i..self.cur_i + 1]
                .iter()
                .collect::<String>(),
        ))
    }

    #[inline]
    fn lex_2(
        &mut self,
        opts: &[(char, TokenType)],
        fallback: TokenType,
    ) -> Result<Token, Error> {
        for (c, typ) in opts {
            if self.peek_c == *c {
                let start = self.cur_i;
                self.advance();
                return Ok(Token::new(
                    Pos::span(start, 2),
                    *typ,
                    self.src[start..start + 2].iter().collect::<String>(),
                ));
            }
        }
        self.lex_1(fallback)
    }

    fn lex_number(&mut self) -> Result<Token, Error> {
        let start = self.cur_i;
        let mut has_decimal = false;
        let mut has_exp = false;
        while !self.peek_eof()
            && (self.peek_c.is_ascii_digit()
                || ['.', 'e'].contains(&self.peek_c))
        {
            if self.peek_c == 'e' {
                if has_exp {
                    return Err(Error::UnexpectedCharacter {
                        at:      Pos::one(self.peek_i),
                        ch:      self.peek_c,
                        context: "numeric literal",
                    });
                }
                has_exp = true;
            }

            if self.peek_c == '.' {
                if has_exp || has_decimal {
                    return Err(Error::UnexpectedCharacter {
                        at:      Pos::one(self.peek_i),
                        ch:      self.peek_c,
                        context: "numeric literal",
                    });
                }
                has_decimal = true;
            }

            self.advance();
        }

        if has_decimal || has_exp {
            return Ok(Token::new(
                Pos::span(start, self.peek_i - start),
                LitFloat,
                self.src[start..self.peek_i].iter().collect::<String>(),
            ));
        }

        Ok(Token::new(
            Pos::span(start, self.peek_i - start),
            LitInt,
            self.src[start..self.peek_i].iter().collect::<String>(),
        ))
    }

    fn lex_bool_ident_or_keyword(&mut self) -> Result<Token, Error> {
        let start = self.cur_i;

        while !self.peek_eof()
            && (self.peek_c.is_ascii_alphanumeric() || self.peek_c == '_')
        {
            self.advance();
        }

        let lit = String::from_iter(&self.src[start..self.peek_i]);
        Ok(Token::new(
            Pos::span(start, self.peek_i - start),
            match lit.as_ref() {
                "let" => KwLet,
                "fn" => KwFn,
                "if" => KwIf,
                "elif" => KwElif,
                "else" => KwElse,
                "return" => KwReturn,
                "true" => KwTrue,
                "false" => KwFalse,
                "null" => KwNull,
                "throw" => KwThrow,
                _ => Ident,
            },
            lit,
        ))
    }

    fn lex_string(&mut self) -> Result<Token, Error> {
        let start = self.cur_i;
        let quote = self.cur_c;
        let mut lit = String::with_capacity(16);
        while !self.peek_eof() {
            if self.peek_c == quote {
                self.advance();
                return Ok(Token::new(
                    Pos::span(start, self.peek_i - start),
                    LitString,
                    lit,
                ));
            }

            if self.peek_c == '\\' {
                self.advance();
                self.lex_string_escape(start, quote, &mut lit)?;
                continue;
            }

            lit.push(self.peek_c);
            self.advance();
        }

        Err(Error::UnterminatedStringLiteral {
            at: Pos::mark(start),
        })
    }

    fn lex_string_escape(
        &mut self,
        start: usize,
        quote: char,
        lit: &mut String,
    ) -> Result<(), Error> {
        self.advance();
        if self.at_eof() {
            return Err(Error::UnterminatedStringLiteral {
                at: Pos::mark(start),
            });
        }

        if self.cur_c == quote {
            lit.push(quote);
            return Ok(());
        }

        match self.cur_c {
            '\\' => {
                lit.push(self.cur_c);
                Ok(())
            }
            'n' => {
                lit.push('\n');
                Ok(())
            }
            't' => {
                lit.push('\t');
                Ok(())
            }
            _ => {
                Err(Error::InvalidEscapeSequence {
                    at: Pos::one(self.cur_i),
                    ch: self.cur_c,
                })
            }
        }
    }
}

#[cfg(test)]
mod tests {
    macro_rules! assert_tokens {
        ($src:expr, $($typ:ident($start:expr,$len:expr,$lit:expr)),*) => {{
            use super::{Code, Pos, Token, TokenType, Lexer};
            let mut lex = Lexer::new(&Code::from($src));
            let want: Vec<Token> = vec![
                $(Token::new(Pos::span($start,$len), TokenType::$typ, $lit)),*
            ];
            let mut got: Vec<Token> = Vec::new();
            loop {
                let result = lex.next();
                if result.is_err() {
                    panic!(format!("{:?}", result.unwrap_err()));
                }
                let tok = result.unwrap();
                if tok.typ == TokenType::EOF {
                    break;
                }
                got.push(tok);
            }
            assert!(want == got, "\nwanted: {:?}\ngot:    {:?}\n", want, got);
        }};
    }

    #[test]
    fn test_lex_literals_ok() {
        assert_tokens!("1", LitInt(0, 1, "1"));
        assert_tokens!("12345", LitInt(0, 5, "12345"));

        assert_tokens!("1.5", LitFloat(0, 3, "1.5"));
        assert_tokens!("1e5", LitFloat(0, 3, "1e5"));
        assert_tokens!("1.5e5", LitFloat(0, 5, "1.5e5"));

        assert_tokens!("true", KwTrue(0, 4, "true"));
        assert_tokens!("false", KwFalse(0, 5, "false"));

        assert_tokens!("''", LitString(0, 2, ""));
        assert_tokens!("'\\''", LitString(0, 4, "'"));
        assert_tokens!("\"\"", LitString(0, 2, ""));
        assert_tokens!("\"\\\"\"", LitString(0, 4, "\""));
        assert_tokens!("'\\t\\n'", LitString(0, 6, "\t\n"));
    }

    #[test]
    fn test_lex_ident_ok() {
        assert_tokens!("abc", Ident(0, 3, "abc"));
        assert_tokens!("_bc", Ident(0, 3, "_bc"));
        assert_tokens!("ab_", Ident(0, 3, "ab_"));
        assert_tokens!("ab1", Ident(0, 3, "ab1"));
        assert_tokens!("_ _", Ident(0, 1, "_"), Ident(2, 1, "_"));
    }

    #[test]
    fn test_lex_op_ok() {
        assert_tokens!("+", OpAdd(0, 1, "+"));
        assert_tokens!("-", OpSub(0, 1, "-"));
        assert_tokens!("*", OpMul(0, 1, "*"));
        assert_tokens!("/", OpDiv(0, 1, "/"));
        assert_tokens!("%", OpRem(0, 1, "%"));
        assert_tokens!(">", OpGt(0, 1, ">"));
        assert_tokens!("<", OpLt(0, 1, "<"));
        assert_tokens!(">=", OpGeq(0, 2, ">="));
        assert_tokens!("<=", OpLeq(0, 2, "<="));
        assert_tokens!("==", OpEq(0, 2, "=="));
        assert_tokens!("!=", OpNeq(0, 2, "!="));

        assert_tokens!("!", Bang(0, 1, "!"));
        assert_tokens!("=", Assign(0, 1, "="));
        assert_tokens!("()", OParen(0, 1, "("), CParen(1, 1, ")"));
        assert_tokens!("{}", OBrace(0, 1, "{"), CBrace(1, 1, "}"));
        assert_tokens!(";", Semi(0, 1, ";"));
    }
}
