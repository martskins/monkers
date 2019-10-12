use super::expression::*;

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
    Block(BlockStatement),
}

#[derive(Debug, PartialEq)]
pub struct LetStatement {
    pub(crate) name: Identifier,
    pub(crate) value: Expression,
}

impl From<LetStatement> for Statement {
    fn from(f: LetStatement) -> Self {
        Statement::Let(f)
    }
}

#[derive(Debug, PartialEq)]
pub struct ReturnStatement {
    pub(crate) value: Expression,
}

impl From<ReturnStatement> for Statement {
    fn from(f: ReturnStatement) -> Self {
        Statement::Return(f)
    }
}

#[derive(Debug, PartialEq)]
pub struct ExpressionStatement {
    pub(crate) value: Expression,
}

impl From<ExpressionStatement> for Statement {
    fn from(f: ExpressionStatement) -> Self {
        Statement::Expression(f)
    }
}

#[derive(Debug, PartialEq)]
pub struct BlockStatement {
    pub(crate) statements: Vec<Statement>,
}

impl From<BlockStatement> for Statement {
    fn from(f: BlockStatement) -> Self {
        Statement::Block(f)
    }
}
