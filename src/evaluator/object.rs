use super::builtin::BuiltinFunction;
use super::result::{EvalError, Result};
use super::Environment;
use crate::parser::{BlockStatement, Identifier};
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::fmt::{self, Display, Formatter};
use std::hash::Hash;
use std::rc::Rc;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Rc<RefCell<Environment>>,
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "fn ({}) {{\n\t{}\n}}",
            self.parameters
                .iter()
                .map(|x| format!("{}", x))
                .collect::<Vec<String>>()
                .join(","),
            self.body
        )
    }
}

#[derive(Debug, Hash, Ord, PartialOrd, Eq, PartialEq, Clone)]
pub enum Hashable {
    Integer(i64),
    Boolean(bool),
    String(String),
}

impl Display for Hashable {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Hashable::String(v) => write!(f, "{}", v),
            Hashable::Integer(v) => write!(f, "{}", v),
            Hashable::Boolean(v) => write!(f, "{}", v),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Object {
    Null,
    BuiltinFunction(BuiltinFunction),
    Integer(i64),
    Boolean(bool),
    Hash(BTreeMap<Hashable, Object>),
    String(String),
    Function(Function),
    ReturnValue(Box<Object>),
    Array(Vec<Object>),
}

impl Object {
    pub(crate) fn is_truthy(&self) -> bool {
        match self {
            Object::Null => false,
            Object::Integer(0) => false,
            Object::String(s) => !s.is_empty(),
            Object::Boolean(false) => false,
            Object::Array(v) => !v.is_empty(),
            Object::Hash(v) => !v.is_empty(),
            _ => true,
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Object::Null => write!(f, "null"),
            Object::BuiltinFunction(v) => write!(f, "{}", v),
            Object::Integer(v) => write!(f, "{}", v),
            Object::Boolean(v) => write!(f, "{}", v),
            Object::Hash(v) => write!(
                f,
                "[{}]",
                v.iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect::<Vec<String>>()
                    .join(",")
            ),
            Object::Array(v) => write!(
                f,
                "{}",
                v.iter()
                    .map(|x| format!("{}", x))
                    .collect::<Vec<String>>()
                    .join(",")
            ),
            Object::String(v) => write!(f, "{}", v),
            Object::ReturnValue(v) => write!(f, "{}", v),
            Object::Function(v) => write!(f, "{}", v),
        }
    }
}
