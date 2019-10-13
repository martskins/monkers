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
        (l, r) => return Err(EvalError::TypeMismatch(l, r)),
    };

    Ok(res)
}

fn eval_eq_infix_expression(left: Object, right: Object) -> Result<Object> {
    let res = match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Boolean(l == r),
        (Object::Boolean(l), Object::Boolean(r)) => Object::Boolean(l == r),
        (l, r) => return Err(EvalError::TypeMismatch(l, r)),
    };

    Ok(res)
}

fn eval_lt_infix_expression(left: Object, right: Object) -> Result<Object> {
    let res = match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Boolean(l < r),
        (l, r) => return Err(EvalError::TypeMismatch(l, r)),
    };

    Ok(res)
}

fn eval_gt_infix_expression(left: Object, right: Object) -> Result<Object> {
    let res = match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Boolean(l > r),
        (l, r) => return Err(EvalError::TypeMismatch(l, r)),
    };

    Ok(res)
}

fn eval_div_infix_expression(left: Object, right: Object) -> Result<Object> {
    let res = match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Integer(l / r),
        (l, r) => return Err(EvalError::TypeMismatch(l, r)),
    };

    Ok(res)
}

fn eval_mul_infix_expression(left: Object, right: Object) -> Result<Object> {
    let res = match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Integer(l * r),
        (l, r) => return Err(EvalError::TypeMismatch(l, r)),
    };

    Ok(res)
}

fn eval_minus_infix_expression(left: Object, right: Object) -> Result<Object> {
    let res = match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Integer(l - r),
        (l, r) => return Err(EvalError::TypeMismatch(l, r)),
    };

    Ok(res)
}

fn eval_add_infix_expression(left: Object, right: Object) -> Result<Object> {
    let res = match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Integer(l + r),
        (l, r) => return Err(EvalError::TypeMismatch(l, r)),
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

    macro_rules! should_err {
        ($input:expr, $expect:expr) => {
            let mut env = Environment::new();
            let mut parser = Parser::from($input);
            let program = parser.parse().unwrap();
            let actual = program.eval(&mut env);
            assert_eq!(Err($expect), actual);
        };
    }

    macro_rules! should_eval {
        ($input:expr, $expect:expr) => {
            let mut env = Environment::new();
            let mut parser = Parser::from($input);
            let program = parser.parse().unwrap();
            let actual = program.eval(&mut env).unwrap();
            assert_eq!($expect, actual);
        };
    }

    #[test]
    fn test_eval_integer_expression() {
        should_eval!("5;", Object::Integer(5));
        should_eval!("10 + 5;", Object::Integer(15));
    }

    #[test]
    fn test_eval_boolean_expression() {
        should_eval!("false;", Object::Boolean(false));
        should_eval!("true;", Object::Boolean(true));
        should_eval!("true != false;", Object::Boolean(true));
        should_eval!("5 < 3", Object::Boolean(false));
    }

    #[test]
    fn minus_operator() {
        should_eval!("-42;", Object::Integer(-42));
        should_eval!("--42;", Object::Integer(42));
    }

    #[test]
    fn bang_operator() {
        should_eval!("!5;", Object::Boolean(false));
        should_eval!("!true;", Object::Boolean(false));
        should_eval!("!false;", Object::Boolean(true));
        should_eval!("!!true;", Object::Boolean(true));
    }

    #[test]
    fn greater_than_expression() {
        should_eval!("5 > 3;", Object::Boolean(true));
        should_eval!("1 > 3;", Object::Boolean(false));
    }

    #[test]
    fn less_than_expression() {
        should_eval!("5 < 3;", Object::Boolean(false));
        should_eval!("1 < 3;", Object::Boolean(true));
    }

    #[test]
    fn multiplication_expression() {
        should_eval!("7 * 6;", Object::Integer(42));
        should_eval!("7 * -6;", Object::Integer(-42));
        should_eval!("-7 * -6;", Object::Integer(42));
        should_eval!("-7 * 6;", Object::Integer(-42));
    }

    #[test]
    fn division_expression() {
        should_eval!("36 / 6;", Object::Integer(6));
        should_eval!("36 / -6;", Object::Integer(-6));
        should_eval!("-36 / -6;", Object::Integer(6));
        should_eval!("-36 / 6;", Object::Integer(-6));
    }

    #[test]
    fn subtraction_expression() {
        should_eval!("6 - 6;", Object::Integer(0));
        should_eval!("20 - 6;", Object::Integer(14));
        should_eval!("20 - -6;", Object::Integer(26));
    }

    #[test]
    fn addition_expression() {
        should_eval!("-6 + -6;", Object::Integer(-12));
        should_eval!("20 + 6;", Object::Integer(26));
        should_eval!("20 + -6;", Object::Integer(14));
    }

    #[test]
    fn if_expression() {
        should_eval!("if (true) { 10 } else { 1 }", Object::Integer(10));
        should_eval!("if (true) { 10 }", Object::Integer(10));
        should_eval!("if (false) { 10 }", Object::Null);
        should_eval!(
            "if (true) { if (true) { 10 } } else { 1 } ",
            Object::Integer(10)
        );
        should_eval!(
            "if (true) { if (true) { return 10; } return 12; } else { 1 } ",
            Object::Integer(10)
        );
    }

    #[test]
    fn return_statement() {
        should_eval!("return 10; return 1;", Object::Integer(10));
        should_eval!("5; return 10;", Object::Integer(10));
        should_eval!("5; return 10; 15", Object::Integer(10));
    }

    #[test]
    fn type_mismatch_error() {
        should_err!(
            "5 + false",
            EvalError::TypeMismatch(Object::Integer(5), Object::Boolean(false))
        );
        should_err!(
            "5 / false",
            EvalError::TypeMismatch(Object::Integer(5), Object::Boolean(false))
        );
    }

    #[test]
    fn eval_simple_let_statement() {
        should_eval!("let a = 5; a;", Object::Integer(5));
        should_eval!("let a = 5; let b = a; b", Object::Integer(5));
    }
}
