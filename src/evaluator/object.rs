use std::fmt::{self, Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Null,
    Integer(i64),
    Boolean(bool),
    ReturnValue(Box<Object>),
}

impl Object {
    pub(crate) fn is_truthy(&self) -> bool {
        match self {
            Object::Null => false,
            Object::Integer(0) => false,
            Object::Boolean(false) => false,
            _ => true,
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Object::Null => write!(f, "null"),
            Object::Integer(v) => write!(f, "{}", v),
            Object::Boolean(v) => write!(f, "{}", v),
            Object::ReturnValue(v) => write!(f, "{}", v),
        }
    }
}
