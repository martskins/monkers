use crate::lexer::Token;

#[derive(Debug, PartialEq)]
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
