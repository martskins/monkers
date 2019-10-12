use super::operator::Operator;
use super::statement::BlockStatement;
use std::fmt::{self, Display, Formatter};

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    BooleanLiteral(BooleanLiteral),
    If(IfExpression),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Function(FunctionLiteral),
    Call(CallExpression),
    Grouped(GroupedExpression),
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Expression::Identifier(v) => write!(f, "{}", v),
            Expression::IntegerLiteral(v) => write!(f, "{}", v),
            Expression::BooleanLiteral(v) => write!(f, "{}", v),
            Expression::If(v) => write!(f, "{}", v),
            Expression::Prefix(v) => write!(f, "{}", v),
            Expression::Infix(v) => write!(f, "{}", v),
            Expression::Function(v) => write!(f, "{}", v),
            Expression::Call(v) => write!(f, "{}", v),
            Expression::Grouped(v) => write!(f, "{}", v),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Identifier {
    pub(crate) value: String,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
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

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
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

impl PrefixExpression {
    pub fn new(operator: Operator, right: Expression) -> PrefixExpression {
        PrefixExpression {
            operator,
            right: Box::new(right),
        }
    }
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{} {}", self.operator, self.right)
    }
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

impl InfixExpression {
    pub fn new(left: Expression, operator: Operator, right: Expression) -> InfixExpression {
        InfixExpression {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{} {} {}", self.left, self.operator, self.right)
    }
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

impl Display for BooleanLiteral {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
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

impl Display for IfExpression {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "if ({}) {{\n\t{}\n}}", self.condition, self.consequence)?;
        if let Some(alt) = &self.alternative {
            write!(f, " else {{\n\t{}\n}}", alt)?;
        }

        Ok(())
    }
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

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "fn ({}) {{\n\t{}\n}}",
            self.parameters
                .iter()
                .map(|x| format!("{}", x))
                .collect::<Vec<String>>()
                .join(","),
            self.body
        )
    }
}

impl From<FunctionLiteral> for Expression {
    fn from(f: FunctionLiteral) -> Self {
        Expression::Function(f)
    }
}

#[derive(Debug, PartialEq)]
pub struct CallExpression {
    pub(crate) function: Box<Expression>,
    pub(crate) arguments: Vec<Expression>,
}

impl Display for CallExpression {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}({})",
            self.function,
            self.arguments
                .iter()
                .map(|x| format!("{}", x))
                .collect::<Vec<String>>()
                .join(",")
        )
    }
}

impl From<CallExpression> for Expression {
    fn from(f: CallExpression) -> Self {
        Expression::Call(f)
    }
}

#[derive(Debug, PartialEq)]
pub struct GroupedExpression {
    pub(crate) value: Box<Expression>,
}

impl Display for GroupedExpression {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "({})", self.value)
    }
}

impl From<GroupedExpression> for Expression {
    fn from(f: GroupedExpression) -> Self {
        Expression::Grouped(f)
    }
}
