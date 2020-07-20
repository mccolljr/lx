use crate::ast::ObjDestructItem;
use crate::runtime::value::Value;
use crate::token::TokenType;

use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Inst {
    Illegal,
    StackPush(Value),
    StackPop,
    ScopeLoad(String),
    ScopeStore(String),
    ScopeDefine(String),
    DestructureArray(Rc<[String]>),
    DestructureObject(Rc<[ObjDestructItem]>),
    OperationBinary(TokenType),
    OperationUnary(TokenType),
    OperationIndexGet,
    OperationIndexSet,
    BranchIter(String, usize, usize),
    BranchConditional(usize, usize),
    BranchGoto(usize),
    BuildFunc {
        args:       Vec<String>,
        insts:      Rc<[Inst]>,
        name:       Option<Rc<str>>,
        is_closure: bool,
    },
    BuildObject,
    BuildArray,
    BuildIter,
    RunFrame {
        insts: Rc<[Inst]>,
    },
    RunLoopFrame {
        insts:    Rc<[Inst]>,
        on_break: usize,
    },
    RunIterFrame {
        insts:    Rc<[Inst]>,
        on_break: usize,
    },
    CallBegin,
    CallAppend,
    CallEnd,
    ControlReturn,
    ControlYield,
    ControlThrow,
    ControlBreak,
}
