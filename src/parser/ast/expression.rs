use super::operator::Operator;
use crate::lexer::Token;
use std::convert::TryFrom;

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
}

#[derive(Debug, PartialEq)]
pub struct Identifier {
    pub(crate) value: String,
}

#[derive(Debug, PartialEq)]
pub struct IntegerLiteral {
    pub(crate) value: i64,
}

#[derive(Debug, PartialEq)]
pub struct PrefixExpression {
    pub(crate) operator: Operator,
    pub(crate) right: Box<Expression>,
}

#[derive(Debug, PartialEq)]
pub struct InfixExpression {
    pub(crate) left: Box<Expression>,
    pub(crate) operator: Operator,
    pub(crate) right: Box<Expression>,
}
