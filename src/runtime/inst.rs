use crate::ast::ObjDestructItem;
use crate::runtime::frame::{
    CatchContext,
    FinallyContext,
};
use crate::runtime::value::Value;
use crate::token::TokenType;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Inst {
    Illegal,
    SysImport(String),
    SysTypeof,
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
    RunTryFrame {
        insts:   Rc<[Inst]>,
        catch:   Option<CatchContext>,
        finally: Option<FinallyContext>,
    },
    CallBegin,
    CallAppend,
    CallEnd,
    ControlReturn,
    ControlYield,
    ControlThrow,
    ControlBreak,
}
