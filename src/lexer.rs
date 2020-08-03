use crate::error::SyntaxError;
use crate::source::{
    Code,
    Pos,
};
use crate::token::{
    Token,
    TokenType,
    TokenType::*,
};

use std::iter::FromIterator;

pub struct Lexer {
    pub(crate) src: Code,
    cur_i:          usize,
    cur_c:          char,
    peek_i:         usize,
    peek_c:         char,
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

    pub fn next(&mut self) -> Result<Token, SyntaxError> {
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
            '/' => self.lex_opdiv_or_comment(),
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
            '|' => self.lex_1(Bar),
            '>' => self.lex_2(&[('=', OpGeq)], OpGt),
            '<' => self.lex_2(&[('=', OpLeq)], OpLt),
            '=' => self.lex_2(&[('=', OpEq)], Assign),
            '!' => self.lex_2(&[('=', OpNeq)], Bang),
            _ => {
                Err(SyntaxError::InvalidCharacter {
                    code: self.src.clone(),
                    at:   Pos::one(self.cur_i),
                    ch:   self.cur_c,
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
    fn lex_1(&self, typ: TokenType) -> Result<Token, SyntaxError> {
        let pos = Pos::one(self.cur_i);
        Ok(Token::new(
            pos,
            typ,
            self.src[pos].iter().collect::<String>(),
        ))
    }

    #[inline]
    fn lex_2(
        &mut self,
        opts: &[(char, TokenType)],
        fallback: TokenType,
    ) -> Result<Token, SyntaxError> {
        for (c, typ) in opts {
            if self.peek_c == *c {
                let start = self.cur_i;
                self.advance();
                let pos = Pos::span(start, 2);
                return Ok(Token::new(
                    pos,
                    *typ,
                    self.src[pos].iter().collect::<String>(),
                ));
            }
        }
        self.lex_1(fallback)
    }

    fn lex_opdiv_or_comment(&mut self) -> Result<Token, SyntaxError> {
        match self.peek_c {
            '/' => {
                // line comment, read to end of line
                let start = self.cur_i;
                self.advance();
                let text_start = self.peek_i;
                while !self.peek_eof() && self.peek_c != '\n' {
                    self.advance();
                }
                let end = self.peek_i;
                Ok(Token::new(
                    Pos::span(start, end - start),
                    TokenType::Comment,
                    self.src[Pos::span(text_start, end - text_start)]
                        .iter()
                        .collect::<String>(),
                ))
            }
            '*' => {
                // block comment, read to end of block
                let start = self.cur_i;
                self.advance();
                let text_start = self.peek_i;
                loop {
                    if self.peek_eof() {
                        return Err(SyntaxError::UnterminatedBlockComment {
                            code: self.src.clone(),
                            at:   Pos::span(start, self.peek_i - start),
                        });
                    }
                    if self.peek_c == '*' {
                        self.advance();
                        let maybe_text_end = self.cur_i;
                        if self.peek_c == '/' {
                            self.advance();
                            let end = self.peek_i;
                            return Ok(Token::new(
                                Pos::span(start, end - start),
                                TokenType::Comment,
                                self.src[Pos::span(
                                    text_start,
                                    maybe_text_end - text_start,
                                )]
                                .iter()
                                .collect::<String>(),
                            ));
                        }
                    }
                    self.advance();
                }
            }
            _ => self.lex_1(TokenType::OpDiv),
        }
    }

    fn lex_number(&mut self) -> Result<Token, SyntaxError> {
        let start = self.cur_i;
        let mut has_decimal = false;
        let mut has_exp = false;
        while !self.peek_eof()
            && (self.peek_c.is_ascii_digit()
                || ['.', 'e'].contains(&self.peek_c))
        {
            if self.peek_c == 'e' {
                if has_exp {
                    return Err(SyntaxError::UnexpectedCharacter {
                        code:    self.src.clone(),
                        at:      Pos::one(self.peek_i),
                        ch:      self.peek_c,
                        context: "numeric literal",
                    });
                }
                has_exp = true;
                self.advance();
                // special case: allow a negative sign right after the e
                if self.peek_c == '-' {
                    self.advance();
                }
                continue;
            }

            if self.peek_c == '.' {
                if has_exp || has_decimal {
                    return Err(SyntaxError::UnexpectedCharacter {
                        code:    self.src.clone(),
                        at:      Pos::one(self.peek_i),
                        ch:      self.peek_c,
                        context: "numeric literal",
                    });
                }
                has_decimal = true;
                self.advance();
                continue;
            }

            self.advance();
        }

        let pos = Pos::span(start, self.peek_i - start);
        Ok(Token::new(
            pos,
            if has_decimal || has_exp {
                LitFloat
            } else {
                LitInt
            },
            String::from_iter(&self.src[pos]),
        ))
    }

    fn lex_bool_ident_or_keyword(&mut self) -> Result<Token, SyntaxError> {
        let start = self.cur_i;

        while !self.peek_eof()
            && (self.peek_c.is_ascii_alphanumeric() || self.peek_c == '_')
        {
            self.advance();
        }

        let pos = Pos::span(start, self.peek_i - start);
        let lit = String::from_iter(&self.src[pos]);
        Ok(Token::new(
            pos,
            match lit.as_ref() {
                // booleans
                "true" => KwTrue,
                "false" => KwFalse,
                // keywords
                "let" => KwLet,
                "fn" => KwFn,
                "if" => KwIf,
                "elif" => KwElif,
                "else" => KwElse,
                "return" => KwReturn,
                "null" => KwNull,
                "throw" => KwThrow,
                "while" => KwWhile,
                "break" => KwBreak,
                "for" => KwFor,
                "in" => KwIn,
                "yield" => KwYield,
                "import" => KwImport,
                "typeof" => KwTypeof,
                "try" => KwTry,
                "catch" => KwCatch,
                "finally" => KwFinally,
                "any" => KwAny,
                "int" => KwInt,
                "float" => KwFloat,
                "bool" => KwBool,
                "str" => KwStr,
                "map" => KwMap,
                "array" => KwArray,
                "type" => KwType,
                _ => Ident,
            },
            lit,
        ))
    }

    fn lex_string(&mut self) -> Result<Token, SyntaxError> {
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

        Err(SyntaxError::UnterminatedStringLiteral {
            code: self.src.clone(),
            at:   Pos::mark(start),
        })
    }

    fn lex_string_escape(
        &mut self,
        start: usize,
        quote: char,
        lit: &mut String,
    ) -> Result<(), SyntaxError> {
        self.advance();
        if self.at_eof() {
            return Err(SyntaxError::UnterminatedStringLiteral {
                code: self.src.clone(),
                at:   Pos::mark(start),
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
                Err(SyntaxError::InvalidEscapeSequence {
                    code: self.src.clone(),
                    at:   Pos::one(self.cur_i),
                    ch:   self.cur_c,
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
        assert_tokens!("1.5e-5", LitFloat(0, 6, "1.5e-5"));

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

    #[test]
    fn test_lex_comments() {
        assert_tokens!("//abc", Comment(0, 5, "abc"));
        assert_tokens!("/*abc\ndef*/", Comment(0, 11, "abc\ndef"));
    }
}
