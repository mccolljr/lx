use super::super::token::TokenType;
use super::value::Value;
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
    MakeClosure(usize, Rc<[Inst]>),
    InitCall,
    AddCallArg,
    FinishCall,
    Return,
    BuildObject,
    Index,
    IndexSet,
    Throw,
}
