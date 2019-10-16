use super::Environment;
use crate::parser::{BlockStatement, Identifier};
use std::cell::RefCell;
use std::fmt::{self, Display, Formatter};
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Null,
    Integer(i64),
    Boolean(bool),
    Function {
        parameters: Vec<Identifier>,
        body: BlockStatement,
        env: Rc<RefCell<Environment>>,
    },
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
            Object::Function {
                parameters,
                body,
                env: _,
            } => write!(
                f,
                "fn ({}) {{\n\t{}\n}}",
                parameters
                    .iter()
                    .map(|x| format!("{}", x))
                    .collect::<Vec<String>>()
                    .join(","),
                body
            ),
        }
    }
}
