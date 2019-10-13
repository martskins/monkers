use super::object::Object;

pub type Result<T> = std::result::Result<T, EvalError>;

#[derive(Debug, PartialEq, Clone)]
pub enum EvalError {
    TypeMismatchError(Object, Object),
    UnknownIdentifier(String),
}
