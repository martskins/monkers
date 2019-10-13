use super::Object;

pub type Result<T> = std::result::Result<T, EvalError>;

#[derive(Debug, PartialEq, Clone)]
pub enum EvalError {
    TypeMismatchError(Object, Object),
}
