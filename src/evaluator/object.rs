use super::result::{EvalError, Result};
use super::Environment;
use crate::parser::{BlockStatement, Identifier};
use std::cell::RefCell;
use std::fmt::{self, Display, Formatter};
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum BuiltinFunction {
    Len,
    First,
    Last,
    Tail,
    Head,
    Push,
}

impl BuiltinFunction {
    pub fn call(&self, args: &[Object]) -> Result<Object> {
        match self {
            BuiltinFunction::Push => {
                if args.len() == 2 {
                    match args.first().unwrap() {
                        Object::Array(s) => {
                            let mut out = s.clone();
                            out.push(args[1].clone());
                            Ok(Object::Array(out))
                        }
                        _ => Err(EvalError::UnsupportedArguments),
                    }
                } else {
                    Err(EvalError::UnsupportedArguments)
                }
            }
            BuiltinFunction::First => {
                if args.len() == 1 {
                    match args.first().unwrap() {
                        Object::Array(s) => Ok(s.first().cloned().unwrap_or(Object::Null)),
                        _ => Err(EvalError::UnsupportedArguments),
                    }
                } else {
                    Err(EvalError::UnsupportedArguments)
                }
            }
            BuiltinFunction::Last => {
                if args.len() == 1 {
                    match args.first().unwrap() {
                        Object::Array(s) => Ok(s.last().cloned().unwrap_or(Object::Null)),
                        _ => Err(EvalError::UnsupportedArguments),
                    }
                } else {
                    Err(EvalError::UnsupportedArguments)
                }
            }
            BuiltinFunction::Head => {
                if args.len() == 1 {
                    match args.first().unwrap() {
                        Object::Array(s) => Ok(Object::Array(if s.len() == 0 {
                            vec![]
                        } else {
                            s.split_at(s.len() - 1).0.to_vec()
                        })),
                        _ => Err(EvalError::UnsupportedArguments),
                    }
                } else {
                    Err(EvalError::UnsupportedArguments)
                }
            }
            BuiltinFunction::Tail => {
                if args.len() == 1 {
                    match args.first().unwrap() {
                        Object::Array(s) => Ok(Object::Array(if s.len() == 0 {
                            vec![]
                        } else {
                            s.split_at(1).1.to_vec()
                        })),
                        _ => Err(EvalError::UnsupportedArguments),
                    }
                } else {
                    Err(EvalError::UnsupportedArguments)
                }
            }
            BuiltinFunction::Len => {
                if args.len() == 1 {
                    match args.first().unwrap() {
                        Object::String(s) => Ok(Object::Integer(s.len() as i64)),
                        Object::Array(s) => Ok(Object::Integer(s.len() as i64)),
                        _ => Err(EvalError::UnsupportedArguments),
                    }
                } else {
                    Err(EvalError::UnsupportedArguments)
                }
            }
        }
    }
}

impl Display for BuiltinFunction {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            BuiltinFunction::Push => write!(f, "push"),
            BuiltinFunction::Len => write!(f, "len"),
            BuiltinFunction::First => write!(f, "first"),
            BuiltinFunction::Last => write!(f, "last"),
            BuiltinFunction::Head => write!(f, "head"),
            BuiltinFunction::Tail => write!(f, "tail"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Null,
    BuiltinFunction(BuiltinFunction),
    Integer(i64),
    Boolean(bool),
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
