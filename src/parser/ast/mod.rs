mod expression;
mod operator;
mod statement;

pub use expression::*;
pub use operator::*;
pub use statement::*;

#[derive(Debug, PartialEq)]
pub enum Node {
    Statement(Statement),
    Expression(Expression),
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub(crate) statements: Vec<Statement>,
}
