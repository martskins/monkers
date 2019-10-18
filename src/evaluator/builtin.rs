use super::object::Object;
use super::result::{EvalError, Result};
use std::fmt::{self, Display, Formatter};

#[derive(Debug, Hash, Ord, PartialOrd, Eq, PartialEq, Clone)]
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
