use crate::source::Pos;
use std::fmt::{
    Display,
    Formatter,
    Result as FmtResult,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub pos: Pos,
    pub typ: TokenType,
    pub lit: String,
}

impl Token {
    pub fn new(pos: Pos, typ: TokenType, lit: impl Into<String>) -> Self {
        Token {
            pos,
            typ,
            lit: lit.into(),
        }
    }

    pub fn new_meta(loc: usize, typ: TokenType) -> Self {
        Token::new(Pos::mark(loc), typ, "")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    EOF,
    Ident,
    LitInt,
    LitFloat,
    LitString,

    Assign,
    Bang,
    Comma,
    Semi,
    Question,
    Colon,
    Dot,

    OParen,
    CParen,
    OBrace,
    CBrace,
    OSquare,
    CSquare,

    OpAdd,
    OpSub,
    OpMul,
    OpDiv,
    OpRem,
    OpEq,
    OpNeq,
    OpLeq,
    OpGeq,
    OpLt,
    OpGt,
    OpFeed,

    KwLet,
    KwFn,
    KwIf,
    KwElif,
    KwElse,
    KwTrue,
    KwFalse,
    KwReturn,
    KwNull,
    KwThrow,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        use TokenType::*;
        match self {
            EOF => write!(f, "end of file"),
            Ident => write!(f, "identifier"),
            LitInt => write!(f, "integer literal"),
            LitFloat => write!(f, "float literal"),
            LitString => write!(f, "string literal"),
            Assign => write!(f, "="),
            Bang => write!(f, "!"),
            Comma => write!(f, ","),
            Semi => write!(f, ";"),
            Question => write!(f, "?"),
            Colon => write!(f, ":"),
            Dot => write!(f, "."),
            OParen => write!(f, "("),
            CParen => write!(f, ")"),
            OBrace => write!(f, "{{"),
            CBrace => write!(f, "}}"),
            OSquare => write!(f, "["),
            CSquare => write!(f, "]"),
            OpAdd => write!(f, "+"),
            OpSub => write!(f, "-"),
            OpMul => write!(f, "*"),
            OpDiv => write!(f, "/"),
            OpRem => write!(f, "%"),
            OpEq => write!(f, "=="),
            OpNeq => write!(f, "!="),
            OpLeq => write!(f, "<="),
            OpGeq => write!(f, ">="),
            OpLt => write!(f, "<"),
            OpGt => write!(f, ">"),
            OpFeed => write!(f, "<<"),
            KwLet => write!(f, "let"),
            KwFn => write!(f, "fn"),
            KwIf => write!(f, "if"),
            KwElif => write!(f, "elif"),
            KwElse => write!(f, "else"),
            KwTrue => write!(f, "true"),
            KwFalse => write!(f, "false"),
            KwReturn => write!(f, "return"),
            KwNull => write!(f, "null"),
            KwThrow => write!(f, "throw"),
        }
    }
}

impl TokenType {
    pub fn is_operator(&self) -> bool {
        use TokenType::*;
        match self {
            OpAdd | OpSub | OpMul | OpDiv | OpRem | OpEq | OpNeq | OpLeq
            | OpGeq | OpLt | OpGt | OParen | Question | OpFeed | OSquare
            | Dot => true,
            _ => false,
        }
    }

    pub fn precedence(&self) -> (i32, i32) {
        use TokenType::*;
        match self {
            Question => (2, 2),
            OpEq | OpNeq => (6, 6),
            OpGt | OpLt | OpGeq | OpLeq => (7, 8),
            OpAdd | OpSub => (9, 10),
            OpDiv | OpRem => (12, 12),
            OpMul => (13, 14),
            OParen => (15, 16),
            OSquare | Dot => (17, 18),
            OpFeed => (19, 20),
            _ => (0, 0),
        }
    }
}
