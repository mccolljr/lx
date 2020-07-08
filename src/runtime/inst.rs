use super::{
    super::token::TokenType,
    value::Value,
};
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Inst {
    Noop,
    PushStack(Value),
    PopStack(),
    LoadNamed(String),
    StoreNamed(String),
    DeclareNamed(String),
    BinaryOp(TokenType),
    UnaryOp(TokenType),
    Branch(usize, usize),
    Goto(usize),
    MakeClosure(Vec<String>, Rc<[Inst]>, Option<String>),
    InitCall,
    AddCallArg,
    FinishCall,
    Return,
    BuildObject,
    Index,
    IndexSet,
    Throw,
}
