use crate::parser::*;
use std::fmt::{self, Display, Formatter};

// TODO: Figure out a way to use a static reference to Boolean instead of creating
// an instance of Object::Boolean every time we eval one.
// static TRUE: &'static Object = &Object::Boolean(true);
// static FALSE: &'static Object = &Object::Boolean(true);

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Null,
    Integer(i64),
    Boolean(bool),
    ReturnValue(Box<Object>),
}

impl Object {
    fn is_truthy(&self) -> bool {
        match self {
            Object::Null => false,
            Object::Integer(0) => false,
            Object::Boolean(false) => false,
            _ => true,
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Object::Null => write!(f, "null"),
            Object::Integer(v) => write!(f, "{}", v),
            Object::Boolean(v) => write!(f, "{}", v),
            Object::ReturnValue(v) => write!(f, "{}", v),
        }
    }
}

pub trait Node {
    fn eval(&self) -> Object;
}

impl Node for Program {
    fn eval(&self) -> Object {
        let mut result = Object::Null;
        for stmt in self.statements.iter() {
            result = stmt.eval();
            if let Object::ReturnValue(o) = result {
                return *o;
            }
        }

        result
    }
}

impl Node for Statement {
    fn eval(&self) -> Object {
        match self {
            //     Statement::Let(v) => v.eval(),
            Statement::Return(v) => Object::ReturnValue(Box::new(v.value.eval())),
            Statement::Expression(v) => v.value.eval(),
            Statement::Block(v) => v.eval(),
            v => unimplemented!("eval not implement for {:?}", v),
        }
    }
}

impl Node for BlockStatement {
    fn eval(&self) -> Object {
        let mut result = Object::Null;
        for stmt in self.statements.iter() {
            result = stmt.eval();
            if let Object::ReturnValue(_) = &result {
                return result.clone();
            }
        }

        result
    }
}

impl Node for Expression {
    fn eval(&self) -> Object {
        match self {
            Expression::IntegerLiteral(v) => Object::Integer(v.value),
            Expression::BooleanLiteral(v) => Object::Boolean(v.value),
            Expression::If(v) => {
                let condition = v.condition.eval();
                if condition.is_truthy() {
                    v.consequence.eval()
                } else {
                    if let Some(alt) = &v.alternative {
                        return alt.eval();
                    }

                    Object::Null
                }
            }
            Expression::Grouped(v) => v.value.eval(),
            Expression::Infix(v) => {
                let left = v.left.eval();
                let right = v.right.eval();
                eval_infix_expression(&v.operator, left, right)
            }
            Expression::Prefix(v) => {
                let right = v.right.eval();
                eval_prefix_expression(&v.operator, right)
            }
            v => unimplemented!("eval not implement for {:?}", v),
        }
    }
}

fn eval_infix_expression(operator: &Operator, left: Object, right: Object) -> Object {
    match operator {
        Operator::Eq => Object::Boolean(left == right),
        Operator::NotEq => Object::Boolean(left != right),
        Operator::Gt | Operator::Lt => match right {
            Object::Integer(r) => match left {
                Object::Integer(l) => {
                    let val = if operator == &Operator::Gt {
                        l > r
                    } else {
                        l < r
                    };
                    Object::Boolean(val)
                }
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        },
        Operator::Div => match right {
            Object::Integer(r) => match left {
                Object::Integer(l) => Object::Integer(l / r),
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        },
        Operator::Mul => match right {
            Object::Integer(r) => match left {
                Object::Integer(l) => Object::Integer(l * r),
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        },
        Operator::Minus => match right {
            Object::Integer(r) => match left {
                Object::Integer(l) => Object::Integer(l - r),
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        },
        Operator::Add => match right {
            Object::Integer(r) => match left {
                Object::Integer(l) => Object::Integer(l + r),
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    }
}

fn eval_prefix_expression(operator: &Operator, right: Object) -> Object {
    match operator {
        Operator::Bang => eval_bang_operator(right),
        Operator::Minus => eval_minus_prefix_operator(right),
        _ => unimplemented!(),
    }
}

fn eval_minus_prefix_operator(right: Object) -> Object {
    match right {
        Object::Integer(v) => Object::Integer(-v),
        _ => unimplemented!(),
    }
}

fn eval_bang_operator(right: Object) -> Object {
    match right {
        Object::Integer(0) => Object::Boolean(true),
        Object::Boolean(v) => Object::Boolean(!v),
        Object::Null => Object::Boolean(true),
        _ => Object::Boolean(false),
    }
}

#[cfg(test)]
mod test {
    use crate::evaluator::*;
    use crate::lexer::*;
    use crate::parser::*;

    #[test]
    fn test_eval_integer_expression() {
        let node = Expression::from(IntegerLiteral { value: 5 });
        let actual = node.eval();
        let expected = Object::Integer(5);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_eval_boolean_expression() {
        let node = Expression::from(BooleanLiteral { value: true });
        let actual = node.eval();
        let expected = Object::Boolean(true);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_minus_operator_on_number_returns_its_opposite_value() {
        let node = Expression::from(PrefixExpression::new(
            Operator::Minus,
            Expression::IntegerLiteral(IntegerLiteral { value: 42 }),
        ));
        let actual = node.eval();
        let expected = Object::Integer(-42);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_bang_operator_on_number_returns_false() {
        let node = Expression::from(PrefixExpression::new(
            Operator::Bang,
            Expression::IntegerLiteral(IntegerLiteral { value: 42 }),
        ));
        let actual = node.eval();
        let expected = Object::Boolean(false);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_bang_operator_on_true_returns_false() {
        let node = Expression::from(PrefixExpression::new(
            Operator::Bang,
            Expression::BooleanLiteral(BooleanLiteral { value: true }),
        ));
        let actual = node.eval();
        let expected = Object::Boolean(false);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_bang_operator_on_false_returns_true() {
        let node = Expression::from(PrefixExpression::new(
            Operator::Bang,
            Expression::BooleanLiteral(BooleanLiteral { value: false }),
        ));
        let actual = node.eval();
        let expected = Object::Boolean(true);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_double_bang_operator_on_boolean_leaves_it_unchanged() {
        let node = Expression::from(PrefixExpression::new(
            Operator::Bang,
            Expression::Prefix(PrefixExpression::new(
                Operator::Bang,
                Expression::BooleanLiteral(BooleanLiteral { value: false }),
            )),
        ));
        let actual = node.eval();
        let expected = Object::Boolean(false);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_eval_infix_greater_than_expression() {
        let node = Expression::from(InfixExpression::new(
            Expression::IntegerLiteral(IntegerLiteral { value: 7 }),
            Operator::Gt,
            Expression::IntegerLiteral(IntegerLiteral { value: 6 }),
        ));
        let actual = node.eval();
        let expected = Object::Boolean(true);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_eval_infix_less_than_expression() {
        let node = Expression::from(InfixExpression::new(
            Expression::IntegerLiteral(IntegerLiteral { value: 7 }),
            Operator::Lt,
            Expression::IntegerLiteral(IntegerLiteral { value: 6 }),
        ));
        let actual = node.eval();
        let expected = Object::Boolean(false);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_eval_infix_multiplication_expression() {
        let node = Expression::from(InfixExpression::new(
            Expression::IntegerLiteral(IntegerLiteral { value: 7 }),
            Operator::Mul,
            Expression::IntegerLiteral(IntegerLiteral { value: 6 }),
        ));
        let actual = node.eval();
        let expected = Object::Integer(42);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_eval_infix_division_expression() {
        let node = Expression::from(InfixExpression::new(
            Expression::IntegerLiteral(IntegerLiteral { value: 84 }),
            Operator::Div,
            Expression::IntegerLiteral(IntegerLiteral { value: 2 }),
        ));
        let actual = node.eval();
        let expected = Object::Integer(42);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_eval_infix_subtraction_expression() {
        let node = Expression::from(InfixExpression::new(
            Expression::IntegerLiteral(IntegerLiteral { value: 43 }),
            Operator::Minus,
            Expression::IntegerLiteral(IntegerLiteral { value: 1 }),
        ));
        let actual = node.eval();
        let expected = Object::Integer(42);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_eval_infix_addition_expression() {
        let node = Expression::from(InfixExpression::new(
            Expression::IntegerLiteral(IntegerLiteral { value: 1 }),
            Operator::Add,
            Expression::IntegerLiteral(IntegerLiteral { value: 41 }),
        ));
        let actual = node.eval();
        let expected = Object::Integer(42);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_eval_if_expression_with_else_and_non_trivial_condition() {
        let node = Expression::from(IfExpression::new(
            Expression::If(IfExpression::new(
                Expression::BooleanLiteral(BooleanLiteral { value: false }),
                BlockStatement {
                    statements: vec![Statement::Expression(ExpressionStatement {
                        value: Expression::BooleanLiteral(BooleanLiteral { value: true }),
                    })],
                },
                None,
            )),
            BlockStatement {
                statements: vec![Statement::Expression(ExpressionStatement {
                    value: Expression::IntegerLiteral(IntegerLiteral { value: 42 }),
                })],
            },
            Some(BlockStatement {
                statements: vec![Statement::Expression(ExpressionStatement {
                    value: Expression::IntegerLiteral(IntegerLiteral { value: 1 }),
                })],
            }),
        ));

        let actual = node.eval();
        let expected = Object::Integer(1);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_eval_if_expression_with_else() {
        let node = Expression::from(IfExpression::new(
            Expression::BooleanLiteral(BooleanLiteral { value: true }),
            BlockStatement {
                statements: vec![Statement::Expression(ExpressionStatement {
                    value: Expression::IntegerLiteral(IntegerLiteral { value: 42 }),
                })],
            },
            Some(BlockStatement {
                statements: vec![Statement::Expression(ExpressionStatement {
                    value: Expression::IntegerLiteral(IntegerLiteral { value: 1 }),
                })],
            }),
        ));
        let actual = node.eval();
        let expected = Object::Integer(42);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_eval_if_expression_without_else() {
        let node = Expression::from(IfExpression::new(
            Expression::BooleanLiteral(BooleanLiteral { value: true }),
            BlockStatement {
                statements: vec![Statement::Expression(ExpressionStatement {
                    value: Expression::IntegerLiteral(IntegerLiteral { value: 42 }),
                })],
            },
            None,
        ));
        let actual = node.eval();
        let expected = Object::Integer(42);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_eval_if_expression_evaluates_to_null_on_falsy_condition_with_no_else() {
        let node = Expression::from(IfExpression::new(
            Expression::BooleanLiteral(BooleanLiteral { value: false }),
            BlockStatement {
                statements: vec![Statement::Expression(ExpressionStatement {
                    value: Expression::IntegerLiteral(IntegerLiteral { value: 42 }),
                })],
            },
            None,
        ));
        let actual = node.eval();
        let expected = Object::Null;
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_nested_return_statements() {
        // if (10 < 12) {
        //      if (true) {
        //          return true;
        //      }
        //      return false;
        //  }

        let inner_if_stmt = Statement::Expression(ExpressionStatement {
            value: Expression::from(IfExpression::new(
                Expression::BooleanLiteral(BooleanLiteral { value: true }),
                BlockStatement {
                    statements: vec![Statement::Return(ReturnStatement {
                        value: Expression::from(BooleanLiteral { value: true }),
                    })],
                },
                None,
            )),
        });

        let outer_if_stmt = Statement::from(ExpressionStatement {
            value: Expression::from(IfExpression::new(
                Expression::Infix(InfixExpression::new(
                    Expression::from(IntegerLiteral { value: 10 }),
                    Operator::Lt,
                    Expression::from(IntegerLiteral { value: 12 }),
                )),
                BlockStatement {
                    statements: vec![
                        inner_if_stmt,
                        Statement::Return(ReturnStatement {
                            value: Expression::from(BooleanLiteral { value: false }),
                        }),
                    ],
                },
                None,
            )),
        });

        let actual = outer_if_stmt.eval();
        let expected = Object::ReturnValue(Box::new(Object::Boolean(true)));
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_return_statement_with_leading_statements() {
        let node = Statement::from(BlockStatement {
            statements: vec![
                Statement::Return(ReturnStatement {
                    value: Expression::BooleanLiteral(BooleanLiteral { value: true }),
                }),
                Statement::Return(ReturnStatement {
                    value: Expression::BooleanLiteral(BooleanLiteral { value: false }),
                }),
            ],
        });

        let actual = node.eval();
        let expected = Object::ReturnValue(Box::new(Object::Boolean(true)));
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_return_statement() {
        let node = Statement::from(ReturnStatement {
            value: Expression::BooleanLiteral(BooleanLiteral { value: false }),
        });

        let actual = node.eval();
        let expected = Object::ReturnValue(Box::new(Object::Boolean(false)));
        assert_eq!(expected, actual);
    }
}
