use super::expression::*;

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

#[derive(Debug, PartialEq)]
pub struct LetStatement {
    pub(crate) name: Identifier,
    // pub(crate) value: Expression,
}

#[derive(Debug, PartialEq)]
pub struct ReturnStatement {
    // pub(crate) value: Expression,
}

#[derive(Debug, PartialEq)]
pub struct ExpressionStatement {
    pub(crate) value: Expression,
}
