use crate::lexer::Token;
use std::fmt::{self, Display, Formatter};

#[derive(Debug, Hash, Ord, PartialOrd, Eq, PartialEq, Clone)]
pub enum Operator {
    Minus,
    Add,
    Mul,
    Div,
    Gt,
    Lt,
    Eq,
    NotEq,
    Bang,
}

impl Display for Operator {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let v = match self {
            Operator::Minus => "-",
            Operator::Add => "+",
            Operator::Mul => "*",
            Operator::Div => "/",
            Operator::Gt => ">",
            Operator::Lt => "<",
            Operator::Eq => "==",
            Operator::NotEq => "!=",
            Operator::Bang => "!",
        };

        write!(f, "{}", v)
    }
}

impl From<&Token> for Operator {
    fn from(f: &Token) -> Self {
        match f {
            Token::Minus => Operator::Minus,
            Token::Plus => Operator::Add,
            Token::Bang => Operator::Bang,
            Token::EqEq => Operator::Eq,
            Token::NotEq => Operator::NotEq,
            Token::Slash => Operator::Div,
            Token::Asterisk => Operator::Mul,
            Token::GT => Operator::Gt,
            Token::LT => Operator::Lt,
            _ => unreachable!(),
        }
    }
}
