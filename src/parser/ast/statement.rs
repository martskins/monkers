use super::expression::*;
use std::fmt::{self, Display, Formatter};

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
    Block(BlockStatement),
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Statement::Let(v) => write!(f, "{}", v),
            Statement::Return(v) => write!(f, "{}", v),
            Statement::Expression(v) => write!(f, "{}", v),
            Statement::Block(v) => write!(f, "{}", v),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct LetStatement {
    pub(crate) name: Identifier,
    pub(crate) value: Expression,
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "let {} = {};", self.name, self.value)
    }
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

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "return {};", self.value)
    }
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

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
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

impl Display for BlockStatement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        // write!(f, "{{ ")?;
        for statement in self.statements.iter() {
            write!(f, "{} ", statement)?;
        }
        // write!(f, "}} ")
        Ok(())
    }
}

impl From<BlockStatement> for Statement {
    fn from(f: BlockStatement) -> Self {
        Statement::Block(f)
    }
}
