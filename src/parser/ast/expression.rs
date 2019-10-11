use super::operator::Operator;
use super::statement::BlockStatement;
use crate::lexer::Token;
use std::convert::TryFrom;

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    BooleanLiteral(BooleanLiteral),
    If(IfExpression),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Function(FunctionLiteral),
}

#[derive(Debug, PartialEq)]
pub struct Identifier {
    pub(crate) value: String,
}

impl From<Identifier> for Expression {
    fn from(f: Identifier) -> Self {
        Expression::Identifier(f)
    }
}

#[derive(Debug, PartialEq)]
pub struct IntegerLiteral {
    pub(crate) value: i64,
}

impl From<IntegerLiteral> for Expression {
    fn from(f: IntegerLiteral) -> Self {
        Expression::IntegerLiteral(f)
    }
}

#[derive(Debug, PartialEq)]
pub struct PrefixExpression {
    pub(crate) operator: Operator,
    pub(crate) right: Box<Expression>,
}

impl From<PrefixExpression> for Expression {
    fn from(f: PrefixExpression) -> Self {
        Expression::Prefix(f)
    }
}

#[derive(Debug, PartialEq)]
pub struct InfixExpression {
    pub(crate) left: Box<Expression>,
    pub(crate) operator: Operator,
    pub(crate) right: Box<Expression>,
}

impl From<InfixExpression> for Expression {
    fn from(f: InfixExpression) -> Self {
        Expression::Infix(f)
    }
}

#[derive(Debug, PartialEq)]
pub struct BooleanLiteral {
    pub(crate) value: bool,
}

impl From<BooleanLiteral> for Expression {
    fn from(f: BooleanLiteral) -> Self {
        Expression::BooleanLiteral(f)
    }
}

#[derive(Debug, PartialEq)]
pub struct IfExpression {
    pub(crate) condition: Box<Expression>,
    pub(crate) consequence: BlockStatement,
    pub(crate) alternative: Option<BlockStatement>,
}

impl From<IfExpression> for Expression {
    fn from(f: IfExpression) -> Self {
        Expression::If(f)
    }
}

#[derive(Debug, PartialEq)]
pub struct FunctionLiteral {
    pub(crate) parameters: Vec<Identifier>,
    pub(crate) body: BlockStatement,
}

impl From<FunctionLiteral> for Expression {
    fn from(f: FunctionLiteral) -> Self {
        Expression::Function(f)
    }
}
