use serde::{
    Deserialize,
    Serialize,
};

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
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
    Bar,

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
    KwWhile,
    KwBreak,
    KwFor,
    KwIn,
    KwYield,
    KwImport,
    KwTypeof,
    KwTry,
    KwCatch,
    KwFinally,
    KwAny,
    KwInt,
    KwFloat,
    KwBool,
    KwStr,
    KwMap,
    KwArray,
    KwType,
    KwAs,

    Comment,
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
            Bar => write!(f, "|"),
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
            KwWhile => write!(f, "while"),
            KwBreak => write!(f, "break"),
            KwFor => write!(f, "for"),
            KwIn => write!(f, "in"),
            KwYield => write!(f, "yield"),
            KwImport => write!(f, "import"),
            KwTypeof => write!(f, "typeof"),
            KwTry => write!(f, "try"),
            KwCatch => write!(f, "catch"),
            KwFinally => write!(f, "finally"),
            KwAny => write!(f, "any"),
            KwInt => write!(f, "int"),
            KwFloat => write!(f, "float"),
            KwBool => write!(f, "bool"),
            KwStr => write!(f, "str"),
            KwMap => write!(f, "map"),
            KwArray => write!(f, "array"),
            KwType => write!(f, "type"),
            KwAs => write!(f, "as"),
            Comment => write!(f, "comment"),
        }
    }
}

impl TokenType {
    pub fn infix_binding_power(&self) -> Option<(i32, i32)> {
        use TokenType::*;
        #[rustfmt::skip]
        return match self {
            Question                    => Some((20, 19)),
            OpEq | OpNeq                => Some((30, 29)),
            OpGt | OpLt | OpGeq | OpLeq => Some((39, 40)),
            OpAdd | OpSub               => Some((49, 50)),
            OpDiv | OpRem               => Some((59, 60)),
            OpMul                       => Some((69, 70)),
            KwAs                        => Some((79, 80)),
            _ => None,
        };
    }

    pub fn prefix_binding_power(&self) -> Option<((), i32)> {
        use TokenType::*;
        #[rustfmt::skip]
        return match self {
            KwTypeof      => Some(((), 31)),
            OpSub | Bang  => Some(((), 81)),
            _ => None,
        };
    }
}
