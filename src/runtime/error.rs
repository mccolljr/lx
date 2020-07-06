#[derive(Debug)]
pub enum Error {
    InvalidOperation(String),
    RuntimeError(String),
    StackUnderflow,
    MalformedStackPanic,
}
