#[derive(Debug)]
pub enum Precedence {
    Lowest = 1,
    Equals = 2,
    LessGreater = 3,
    Sum = 4,
    Product = 5,
    Prefix = 6,
    Call = 7,
}

#[derive(Debug, PartialEq)]
pub enum Node {
    Program(Program),
    // Statement(Statement),
    // Expression(ExpressionStatement),
}

#[derive(Debug, PartialEq)]
pub struct Identifier {
    // pub(crate) token: Token,
    pub(crate) value: String,
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub(crate) statements: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
}

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
