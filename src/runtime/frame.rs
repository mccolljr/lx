use serde::{
    Deserialize,
    Serialize,
};

use super::inst::Inst;
use super::scope::Scope;
use super::value::Value;

use crate::error::Error;

use std::ops::Try;
use std::rc::Rc;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CatchContext(pub String, pub Rc<[Inst]>);

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FinallyContext(pub Rc<[Inst]>);

#[derive(Debug, Clone)]
pub struct Frame {
    pub insts:    Rc<[Inst]>,
    pub scope:    Rc<Scope>,
    pub start_ip: usize,
    pub catch:    Option<CatchContext>,
    pub finally:  Option<FinallyContext>,
}

impl Frame {
    pub fn new(insts: Rc<[Inst]>, scope: Rc<Scope>, start_ip: usize) -> Self {
        Frame {
            insts,
            scope,
            start_ip,
            catch: None,
            finally: None,
        }
    }

    pub fn new_with_catch(
        insts: Rc<[Inst]>,
        scope: Rc<Scope>,
        start_ip: usize,
        catch: Option<CatchContext>,
        finally: Option<FinallyContext>,
    ) -> Self {
        Frame {
            insts,
            scope,
            start_ip,
            catch,
            finally,
        }
    }
}

#[derive(Debug, Clone)]
/// FrameStatus is the exit status of a frame.
pub enum FrameStatus {
    /// Frame execution ended due to executing last instruction
    Ended,
    /// Frame ended due to `break` statement
    Broke,
    /// Frame ended due to `return` statement
    Returned(Value),
    /// Frame ended due to `yield` statement
    Yielded(Value),
    // Frame ended due to `thrown` exception
    Excepted(Error),
}

impl Try for FrameStatus {
    type Error = crate::error::Error;
    type Ok = Self;

    fn into_result(self) -> Result<Self::Ok, Self::Error> {
        match self {
            FrameStatus::Excepted(err) => Err(err),
            all_others => Ok(all_others),
        }
    }

    fn from_ok(status: Self::Ok) -> Self { status }

    fn from_error(err: Self::Error) -> Self { FrameStatus::Excepted(err) }
}
