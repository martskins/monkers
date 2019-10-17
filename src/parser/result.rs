use crate::lexer::Token;
use std::num::ParseIntError;

#[derive(Debug, PartialEq)]
pub enum ParseError {
    UnexpectedToken(Token),
    ParseIntError(ParseIntError),
    NoInfix(Token),
    NoPrefix(Token),
}

impl From<ParseIntError> for ParseError {
    fn from(f: ParseIntError) -> Self {
        ParseError::ParseIntError(f)
    }
}

pub type Result<T> = std::result::Result<T, ParseError>;
