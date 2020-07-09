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
    MakeFunc {
        args:       Vec<String>,
        insts:      Rc<[Inst]>,
        name:       Option<String>,
        is_closure: bool,
    },
    InitCall,
    AddCallArg,
    FinishCall,
    Return,
    BuildObject,
    BuildArray,
    Index,
    IndexSet,
    Throw,
}
