mod object;
mod result;

use crate::parser::*;
use object::Object;
use result::*;
use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};

// TODO: Figure out a way to use a static reference to Boolean instead of creating
// an instance of Object::Boolean every time we eval one.
// static TRUE: &'static Object = &Object::Boolean(true);
// static FALSE: &'static Object = &Object::Boolean(true);

pub struct Environment(HashMap<String, Object>);

impl Environment {
    pub fn new() -> Self {
        Environment(HashMap::new())
    }

    fn set(&mut self, name: &str, val: Object) {
        self.0.insert(name.to_owned(), val);
    }

    fn get(&mut self, name: &str) -> Option<&Object> {
        self.0.get(name)
    }
}

pub trait Node {
    fn eval(&self, env: &mut Environment) -> Result<Object>;
}

impl Node for Program {
    fn eval(&self, env: &mut Environment) -> Result<Object> {
        let mut result = Object::Null;
        for stmt in self.statements.iter() {
            result = stmt.eval(env)?;
            if let Object::ReturnValue(o) = result {
                return Ok(*o);
            }
        }

        Ok(result)
    }
}

impl Node for Statement {
    fn eval(&self, env: &mut Environment) -> Result<Object> {
        match self {
            Statement::Let(v) => {
                let val = v.value.eval(env)?;
                env.set(&v.name.value, val.clone());
                Ok(val)
            }
            Statement::Return(v) => Ok(Object::ReturnValue(Box::new(v.value.eval(env)?))),
            Statement::Expression(v) => v.value.eval(env),
            Statement::Block(v) => v.eval(env),
            v => unimplemented!("eval not implement for {:?}", v),
        }
    }
}

impl Node for BlockStatement {
    fn eval(&self, env: &mut Environment) -> Result<Object> {
        let mut result = Object::Null;
        for stmt in self.statements.iter() {
            result = stmt.eval(env)?;
            if let Object::ReturnValue(_) = &result {
                return Ok(result.clone());
            }
        }

        Ok(result)
    }
}

impl Node for Expression {
    fn eval(&self, env: &mut Environment) -> Result<Object> {
        match self {
            Expression::IntegerLiteral(v) => Ok(Object::Integer(v.value)),
            Expression::BooleanLiteral(v) => Ok(Object::Boolean(v.value)),
            Expression::If(v) => {
                let condition = v.condition.eval(env)?;
                if condition.is_truthy() {
                    v.consequence.eval(env)
                } else {
                    if let Some(alt) = &v.alternative {
                        return alt.eval(env);
                    }

                    Ok(Object::Null)
                }
            }
            Expression::Grouped(v) => v.value.eval(env),
            Expression::Infix(v) => {
                let left = v.left.eval(env)?;
                let right = v.right.eval(env)?;
                eval_infix_expression(&v.operator, left, right)
            }
            Expression::Prefix(v) => {
                let right = v.right.eval(env)?;
                eval_prefix_expression(&v.operator, right)
            }
            Expression::Identifier(v) => {
                let val = env.get(&v.value);
                if let None = val {
                    return Err(EvalError::UnknownIdentifier(v.value.clone()));
                }

                Ok(val.unwrap().clone())
            }
            v => unimplemented!("eval not implement for {:?}", v),
        }
    }
}

fn eval_infix_expression(operator: &Operator, left: Object, right: Object) -> Result<Object> {
    // TODO: Work this out so we don't end up with a gazillion lines comparing every possible
    // combination of left and right.
    match operator {
        Operator::Eq => eval_eq_infix_expression(left, right),
        Operator::NotEq => eval_neq_infix_expression(left, right),
        Operator::Gt => eval_gt_infix_expression(left, right),
        Operator::Lt => eval_lt_infix_expression(left, right),
        Operator::Div => eval_div_infix_expression(left, right),
        Operator::Mul => eval_mul_infix_expression(left, right),
        Operator::Minus => eval_minus_infix_expression(left, right),
        Operator::Add => eval_add_infix_expression(left, right),
        _ => unimplemented!(),
    }
}

fn eval_neq_infix_expression(left: Object, right: Object) -> Result<Object> {
    let res = match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Boolean(l != r),
        (Object::Boolean(l), Object::Boolean(r)) => Object::Boolean(l != r),
        (l, r) => return Err(EvalError::TypeMismatchError(l, r)),
    };

    Ok(res)
}

fn eval_eq_infix_expression(left: Object, right: Object) -> Result<Object> {
    let res = match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Boolean(l == r),
        (Object::Boolean(l), Object::Boolean(r)) => Object::Boolean(l == r),
        (l, r) => return Err(EvalError::TypeMismatchError(l, r)),
    };

    Ok(res)
}

fn eval_lt_infix_expression(left: Object, right: Object) -> Result<Object> {
    let res = match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Boolean(l < r),
        (l, r) => return Err(EvalError::TypeMismatchError(l, r)),
    };

    Ok(res)
}

fn eval_gt_infix_expression(left: Object, right: Object) -> Result<Object> {
    let res = match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Boolean(l > r),
        (l, r) => return Err(EvalError::TypeMismatchError(l, r)),
    };

    Ok(res)
}

fn eval_div_infix_expression(left: Object, right: Object) -> Result<Object> {
    let res = match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Integer(l / r),
        (l, r) => return Err(EvalError::TypeMismatchError(l, r)),
    };

    Ok(res)
}

fn eval_mul_infix_expression(left: Object, right: Object) -> Result<Object> {
    let res = match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Integer(l * r),
        (l, r) => return Err(EvalError::TypeMismatchError(l, r)),
    };

    Ok(res)
}

fn eval_minus_infix_expression(left: Object, right: Object) -> Result<Object> {
    let res = match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Integer(l - r),
        (l, r) => return Err(EvalError::TypeMismatchError(l, r)),
    };

    Ok(res)
}

fn eval_add_infix_expression(left: Object, right: Object) -> Result<Object> {
    let res = match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Integer(l + r),
        (l, r) => return Err(EvalError::TypeMismatchError(l, r)),
    };

    Ok(res)
}

fn eval_prefix_expression(operator: &Operator, right: Object) -> Result<Object> {
    match operator {
        Operator::Bang => eval_bang_operator(right),
        Operator::Minus => eval_minus_prefix_operator(right),
        _ => unimplemented!(),
    }
}

fn eval_minus_prefix_operator(right: Object) -> Result<Object> {
    let res = match right {
        Object::Integer(v) => Object::Integer(-v),
        _ => unimplemented!(),
    };

    Ok(res)
}

fn eval_bang_operator(right: Object) -> Result<Object> {
    let res = match right {
        Object::Integer(0) => Object::Boolean(true),
        Object::Boolean(v) => Object::Boolean(!v),
        Object::Null => Object::Boolean(true),
        _ => Object::Boolean(false),
    };

    Ok(res)
}

#[cfg(test)]
mod test {
    use crate::evaluator::*;
    use crate::lexer::*;
    use crate::parser::*;

    #[test]
    fn test_eval_integer_expression() {
        let mut env = Environment::new();
        let node = Expression::from(IntegerLiteral { value: 5 });
        let actual = node.eval(&mut env).unwrap();
        let expected = Object::Integer(5);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_eval_boolean_expression() {
        let mut env = Environment::new();
        let node = Expression::from(BooleanLiteral { value: true });
        let actual = node.eval(&mut env).unwrap();
        let expected = Object::Boolean(true);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_minus_operator_on_number_returns_its_opposite_value() {
        let mut env = Environment::new();
        let node = Expression::from(PrefixExpression::new(
            Operator::Minus,
            Expression::IntegerLiteral(IntegerLiteral { value: 42 }),
        ));
        let actual = node.eval(&mut env).unwrap();
        let expected = Object::Integer(-42);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_bang_operator_on_number_returns_false() {
        let mut env = Environment::new();
        let node = Expression::from(PrefixExpression::new(
            Operator::Bang,
            Expression::IntegerLiteral(IntegerLiteral { value: 42 }),
        ));
        let actual = node.eval(&mut env).unwrap();
        let expected = Object::Boolean(false);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_bang_operator_on_true_returns_false() {
        let mut env = Environment::new();
        let node = Expression::from(PrefixExpression::new(
            Operator::Bang,
            Expression::BooleanLiteral(BooleanLiteral { value: true }),
        ));
        let actual = node.eval(&mut env).unwrap();
        let expected = Object::Boolean(false);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_bang_operator_on_false_returns_true() {
        let mut env = Environment::new();
        let node = Expression::from(PrefixExpression::new(
            Operator::Bang,
            Expression::BooleanLiteral(BooleanLiteral { value: false }),
        ));
        let actual = node.eval(&mut env).unwrap();
        let expected = Object::Boolean(true);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_double_bang_operator_on_boolean_leaves_it_unchanged() {
        let mut env = Environment::new();
        let node = Expression::from(PrefixExpression::new(
            Operator::Bang,
            Expression::Prefix(PrefixExpression::new(
                Operator::Bang,
                Expression::BooleanLiteral(BooleanLiteral { value: false }),
            )),
        ));
        let actual = node.eval(&mut env).unwrap();
        let expected = Object::Boolean(false);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_eval_infix_greater_than_expression() {
        let mut env = Environment::new();
        let node = Expression::from(InfixExpression::new(
            Expression::IntegerLiteral(IntegerLiteral { value: 7 }),
            Operator::Gt,
            Expression::IntegerLiteral(IntegerLiteral { value: 6 }),
        ));
        let actual = node.eval(&mut env).unwrap();
        let expected = Object::Boolean(true);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_eval_infix_less_than_expression() {
        let mut env = Environment::new();
        let node = Expression::from(InfixExpression::new(
            Expression::IntegerLiteral(IntegerLiteral { value: 7 }),
            Operator::Lt,
            Expression::IntegerLiteral(IntegerLiteral { value: 6 }),
        ));
        let actual = node.eval(&mut env).unwrap();
        let expected = Object::Boolean(false);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_eval_infix_multiplication_expression() {
        let mut env = Environment::new();
        let node = Expression::from(InfixExpression::new(
            Expression::IntegerLiteral(IntegerLiteral { value: 7 }),
            Operator::Mul,
            Expression::IntegerLiteral(IntegerLiteral { value: 6 }),
        ));
        let actual = node.eval(&mut env).unwrap();
        let expected = Object::Integer(42);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_eval_infix_division_expression() {
        let mut env = Environment::new();
        let node = Expression::from(InfixExpression::new(
            Expression::IntegerLiteral(IntegerLiteral { value: 84 }),
            Operator::Div,
            Expression::IntegerLiteral(IntegerLiteral { value: 2 }),
        ));
        let actual = node.eval(&mut env).unwrap();
        let expected = Object::Integer(42);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_eval_infix_subtraction_expression() {
        let mut env = Environment::new();
        let node = Expression::from(InfixExpression::new(
            Expression::IntegerLiteral(IntegerLiteral { value: 43 }),
            Operator::Minus,
            Expression::IntegerLiteral(IntegerLiteral { value: 1 }),
        ));
        let actual = node.eval(&mut env).unwrap();
        let expected = Object::Integer(42);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_eval_infix_addition_expression() {
        let mut env = Environment::new();
        let node = Expression::from(InfixExpression::new(
            Expression::IntegerLiteral(IntegerLiteral { value: 1 }),
            Operator::Add,
            Expression::IntegerLiteral(IntegerLiteral { value: 41 }),
        ));
        let actual = node.eval(&mut env).unwrap();
        let expected = Object::Integer(42);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_eval_if_expression_with_else_and_non_trivial_condition() {
        let mut env = Environment::new();
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

        let actual = node.eval(&mut env).unwrap();
        let expected = Object::Integer(1);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_eval_if_expression_with_else() {
        let mut env = Environment::new();
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
        let actual = node.eval(&mut env).unwrap();
        let expected = Object::Integer(42);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_eval_if_expression_without_else() {
        let mut env = Environment::new();
        let node = Expression::from(IfExpression::new(
            Expression::BooleanLiteral(BooleanLiteral { value: true }),
            BlockStatement {
                statements: vec![Statement::Expression(ExpressionStatement {
                    value: Expression::IntegerLiteral(IntegerLiteral { value: 42 }),
                })],
            },
            None,
        ));
        let actual = node.eval(&mut env).unwrap();
        let expected = Object::Integer(42);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_eval_if_expression_evaluates_to_null_on_falsy_condition_with_no_else() {
        let mut env = Environment::new();
        let node = Expression::from(IfExpression::new(
            Expression::BooleanLiteral(BooleanLiteral { value: false }),
            BlockStatement {
                statements: vec![Statement::Expression(ExpressionStatement {
                    value: Expression::IntegerLiteral(IntegerLiteral { value: 42 }),
                })],
            },
            None,
        ));
        let actual = node.eval(&mut env).unwrap();
        let expected = Object::Null;
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_nested_return_statements() {
        let mut env = Environment::new();
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

        let actual = outer_if_stmt.eval(&mut env).unwrap();
        let expected = Object::ReturnValue(Box::new(Object::Boolean(true)));
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_return_statement_with_leading_statements() {
        let mut env = Environment::new();
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

        let actual = node.eval(&mut env).unwrap();
        let expected = Object::ReturnValue(Box::new(Object::Boolean(true)));
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_return_statement() {
        let mut env = Environment::new();
        let node = Statement::from(ReturnStatement {
            value: Expression::BooleanLiteral(BooleanLiteral { value: false }),
        });

        let actual = node.eval(&mut env).unwrap();
        let expected = Object::ReturnValue(Box::new(Object::Boolean(false)));
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_type_mismatch_error_inverse() {
        let mut env = Environment::new();
        let node = Expression::from(InfixExpression::new(
            Expression::IntegerLiteral(IntegerLiteral { value: 1 }),
            Operator::Add,
            Expression::BooleanLiteral(BooleanLiteral { value: true }),
        ));
        let actual = node.eval(&mut env);
        let expected = Err(EvalError::TypeMismatchError(
            Object::Integer(1),
            Object::Boolean(true),
        ));
        assert_eq!(expected, actual,);
    }

    #[test]
    fn test_type_mismatch_error() {
        let mut env = Environment::new();
        let node = Expression::from(InfixExpression::new(
            Expression::BooleanLiteral(BooleanLiteral { value: true }),
            Operator::Add,
            Expression::IntegerLiteral(IntegerLiteral { value: 1 }),
        ));
        let actual = node.eval(&mut env);
        let expected = Err(EvalError::TypeMismatchError(
            Object::Boolean(true),
            Object::Integer(1),
        ));
        assert_eq!(expected, actual,);
    }

    #[test]
    fn eval_simple_let_statement() {
        let mut env = Environment::new();
        let statements = vec![
            Statement::from(LetStatement {
                name: Identifier { value: "x".into() },
                value: Expression::from(IntegerLiteral { value: 5 }),
            }),
            Statement::from(ExpressionStatement {
                value: Expression::from(Identifier { value: "x".into() }),
            }),
        ];
        let node = Program { statements };
        let actual = node.eval(&mut env).unwrap();
        let expected = Object::Integer(5);
        assert_eq!(expected, actual,);
    }
}
