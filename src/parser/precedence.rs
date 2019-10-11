use crate::lexer::Token;

#[derive(Debug, PartialOrd, PartialEq)]
pub(crate) enum Precedence {
    Lowest = 1,
    Equals = 2,
    LessGreater = 3,
    Sum = 4,
    Product = 5,
    Prefix = 6,
    Call = 7,
}

impl From<&Token> for Precedence {
    fn from(f: &Token) -> Self {
        match f {
            Token::EqEq => Precedence::Equals,
            Token::NotEq => Precedence::Equals,
            Token::LT => Precedence::LessGreater,
            Token::GT => Precedence::LessGreater,
            Token::Plus => Precedence::Sum,
            Token::Minus => Precedence::Sum,
            Token::Slash => Precedence::Product,
            Token::Asterisk => Precedence::Product,
            Token::LParen => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }
}
