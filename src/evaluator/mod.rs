mod builtin;
mod environment;
mod object;
mod result;

use crate::parser::*;
use builtin::BuiltinFunction;
pub use environment::Environment;
use object::{Function, Hashable, Object};
use result::*;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;

// const TRUE: Object = Object::Boolean(true);
// const FALSE: Object = Object::Boolean(false);

pub trait Node {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Object>;
}

impl Node for Program {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Object> {
        let mut result = Object::Null;
        for stmt in self.statements.iter() {
            result = stmt.eval(env.clone())?;
            if let Object::ReturnValue(o) = result {
                return Ok(*o);
            }
        }

        Ok(result)
    }
}

impl Node for Statement {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Object> {
        match self {
            Statement::Let(v) => {
                let val: Object = v.value.eval(env.clone())?;
                env.borrow_mut().set(&v.name.value, val.clone());
                Ok(val)
            }
            Statement::Return(v) => Ok(Object::ReturnValue(Box::new(v.value.eval(env)?))),
            Statement::Expression(v) => v.value.eval(env),
            Statement::Block(v) => v.eval(env),
        }
    }
}

impl Node for BlockStatement {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Object> {
        let mut result = Object::Null;
        for stmt in self.statements.iter() {
            result = stmt.eval(env.clone())?;
            if let Object::ReturnValue(_) = &result {
                return Ok(result.clone());
            }
        }

        Ok(result)
    }
}

impl Node for Expression {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Object> {
        match self {
            Expression::HashLiteral(v) => {
                let mut hash = BTreeMap::new();
                v.elements.iter().for_each(|(k, v)| {
                    let key = k.eval(env.clone());
                    let value = v.eval(env.clone());
                    match (key, value) {
                        (Ok(Object::String(key)), Ok(val)) => {
                            hash.insert(Hashable::String(key), val);
                        }
                        (Ok(Object::Integer(key)), Ok(val)) => {
                            hash.insert(Hashable::Integer(key), val);
                        }
                        (Ok(Object::Boolean(key)), Ok(val)) => {
                            hash.insert(Hashable::Boolean(key), val);
                        }
                        (Ok(k), _) => panic!("Unvalid value used for has key: {}", k),
                        (_, _) => unreachable!(),
                    }
                });

                Ok(Object::Hash(hash))
            }
            Expression::IntegerLiteral(v) => Ok(Object::Integer(v.value)),
            Expression::BooleanLiteral(v) => Ok(Object::Boolean(v.value)),
            Expression::StringLiteral(v) => Ok(Object::String(v.value.clone())),
            Expression::ArrayLiteral(v) => {
                let elements = eval_expressions(&v.elements, env)?;
                Ok(Object::Array(elements))
            }
            Expression::Index(v) => {
                let array = v.left.eval(env.clone())?;
                let index = v.index.eval(env)?;
                match (array, index) {
                    (Object::Array(array), Object::Integer(index)) => {
                        return Ok(array.get(index as usize).unwrap().clone())
                    }
                    (Object::Hash(hash), Object::String(index)) => {
                        return Ok(hash.get(&Hashable::String(index)).unwrap().clone())
                    }
                    (Object::Hash(hash), Object::Integer(index)) => {
                        return Ok(hash.get(&Hashable::Integer(index)).unwrap().clone())
                    }
                    (Object::Hash(hash), Object::Boolean(index)) => {
                        return Ok(hash.get(&Hashable::Boolean(index)).unwrap().clone())
                    }
                    (a, i) => unreachable!("Got {:?} and {:?}", a, i),
                }
            }
            Expression::If(v) => {
                let condition = v.condition.eval(env.clone())?;
                if condition.is_truthy() {
                    v.consequence.eval(env.clone())
                } else {
                    if let Some(alt) = &v.alternative {
                        return alt.eval(env);
                    }

                    Ok(Object::Null)
                }
            }
            Expression::Grouped(v) => v.value.eval(env),
            Expression::Infix(v) => {
                let left = v.left.eval(env.clone())?;
                let right = v.right.eval(env.clone())?;
                eval_infix_expression(&v.operator, left, right)
            }
            Expression::Prefix(v) => {
                let right = v.right.eval(env)?;
                eval_prefix_expression(&v.operator, right)
            }
            Expression::Identifier(v) => match v.value.as_str() {
                "puts" => Ok(Object::BuiltinFunction(BuiltinFunction::Puts)),
                "push" => Ok(Object::BuiltinFunction(BuiltinFunction::Push)),
                "len" => Ok(Object::BuiltinFunction(BuiltinFunction::Len)),
                "first" => Ok(Object::BuiltinFunction(BuiltinFunction::First)),
                "last" => Ok(Object::BuiltinFunction(BuiltinFunction::Last)),
                "head" => Ok(Object::BuiltinFunction(BuiltinFunction::Head)),
                "tail" => Ok(Object::BuiltinFunction(BuiltinFunction::Tail)),
                _ => {
                    let val = env.borrow().get(&v.value);
                    if val.is_none() {
                        return Err(EvalError::UnknownIdentifier(v.value.clone()));
                    }

                    Ok(val.unwrap().clone())
                }
            },
            Expression::Function(v) => {
                let res = Object::Function(Function {
                    parameters: v.parameters.clone(),
                    body: v.body.clone(),
                    env: env.clone(),
                });
                Ok(res)
            }
            Expression::Call(v) => eval_call_expression(v, env),
        }
    }
}

fn eval_call_expression(call: &CallExpression, env: Rc<RefCell<Environment>>) -> Result<Object> {
    let fun = call.function.eval(env.clone())?;
    let args = eval_expressions(&call.arguments, env.clone())?;

    match fun {
        Object::Function(v) => {
            let new_env = Rc::new(RefCell::new(Environment::from(v.env)));
            for (idx, arg) in v.parameters.iter().enumerate() {
                new_env.borrow_mut().set(&arg.value, args[idx].clone());
            }

            let value = v.body.eval(new_env)?;
            if let Object::ReturnValue(r) = value {
                return Ok(*r);
            }

            Ok(value)
        }
        Object::BuiltinFunction(v) => v.call(&args),
        _ => unreachable!(),
    }
}

fn eval_expressions(exprs: &[Expression], env: Rc<RefCell<Environment>>) -> Result<Vec<Object>> {
    let mut result = vec![];

    for expr in exprs {
        let val = expr.eval(env.clone())?;
        result.push(val);
    }
    Ok(result)
}

fn eval_infix_expression(operator: &Operator, left: Object, right: Object) -> Result<Object> {
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
        (Object::String(l), Object::String(r)) => Object::Boolean(l != r),
        (Object::Array(l), Object::Array(r)) => Object::Boolean(l != r),
        (l, r) => return Err(EvalError::TypeMismatch(l, r)),
    };

    Ok(res)
}

fn eval_eq_infix_expression(left: Object, right: Object) -> Result<Object> {
    let res = match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Boolean(l == r),
        (Object::Boolean(l), Object::Boolean(r)) => Object::Boolean(l == r),
        (Object::String(l), Object::String(r)) => Object::Boolean(l == r),
        (Object::Array(l), Object::Array(r)) => Object::Boolean(l == r),
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
        (Object::String(l), Object::String(r)) => Object::String(format!("{}{}", l, r)),
        (Object::Array(l), Object::Array(r)) => {
            let mut n = l.clone();
            for elem in r {
                n.push(elem);
            }
            Object::Array(n)
        }
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
        Object::String(s) => Object::Boolean(s.is_empty()),
        Object::Array(s) => Object::Boolean(s.is_empty()),
        Object::Hash(s) => Object::Boolean(s.is_empty()),
        Object::Null => Object::Boolean(true),
        _ => Object::Boolean(false),
    };

    Ok(res)
}

#[cfg(test)]
mod test {
    use crate::evaluator::*;
    use std::cell::RefCell;
    use std::iter::FromIterator;
    use std::rc::Rc;

    macro_rules! should_err {
        ($input:expr, $expect:expr) => {
            let env = Rc::new(RefCell::new(Environment::new()));
            let mut parser = Parser::from($input);
            let program = parser.parse().unwrap();
            let actual = program.eval(env);
            assert_eq!(actual, Err($expect));
        };
    }

    macro_rules! should_eval {
        ($input:expr, $expect:expr) => {
            let env = Rc::new(RefCell::new(Environment::new()));
            let mut parser = Parser::from($input);
            let program = parser.parse().unwrap();
            let actual = program.eval(env).unwrap();
            assert_eq!(actual, $expect);
        };
    }

    #[test]
    fn test_strings() {
        should_eval!("\"foobar\"", Object::String(String::from("foobar")));
        should_eval!("\"foo bar\"", Object::String(String::from("foo bar")));
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
        should_eval!("!\"\";", Object::Boolean(true));
        should_eval!("!\"asdasd\";", Object::Boolean(false));
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
    fn add_expression() {
        should_eval!("-6 + -6;", Object::Integer(-12));
        should_eval!("20 + 6;", Object::Integer(26));
        should_eval!("20 + -6;", Object::Integer(14));
        should_eval!(
            "\"hello \" + \"world\";",
            Object::String(String::from("hello world"))
        );
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
    fn let_statements() {
        should_eval!("let a = 5; a;", Object::Integer(5));
        should_eval!("let a = 5; let b = a; b", Object::Integer(5));
    }

    #[test]
    fn functions() {
        should_eval!(
            "let ident = fn(x) { return x; }; ident(5);",
            Object::Integer(5)
        );
        should_eval!(
            "let ident = fn(x) { return x; }; ident(5);",
            Object::Integer(5)
        );
        should_eval!(
            "let double = fn(x) { 2 * x; }; double(5);",
            Object::Integer(10)
        );
        should_eval!(
            "let add = fn(x, y) { x + y; }; add(5, 3);",
            Object::Integer(8)
        );
        should_eval!(
            "let sub = fn(x, y) { x - y; }; sub(5, 3);",
            Object::Integer(2)
        );
    }

    #[test]
    fn builtin_len() {
        should_eval!("len(\"\")", Object::Integer(0));
        should_eval!("len(\"hello\")", Object::Integer(5));
        should_eval!("len([])", Object::Integer(0));
        should_eval!("len([1, 2])", Object::Integer(2));

        should_err!("len(1)", EvalError::UnsupportedArguments);
    }

    #[test]
    fn builtin_first() {
        should_eval!("first([])", Object::Null);
        should_eval!("first([1, 2])", Object::Integer(1));

        should_err!("len(1)", EvalError::UnsupportedArguments);
    }

    #[test]
    fn builtin_last() {
        should_eval!("last([])", Object::Null);
        should_eval!("last([1, 2])", Object::Integer(2));
    }

    #[test]
    fn builtin_tail() {
        should_eval!("tail([])", Object::Array(vec![]));
        should_eval!("tail([1, 2])", Object::Array(vec![Object::Integer(2)]));
    }

    #[test]
    fn builtin_head() {
        should_eval!("head([])", Object::Array(vec![]));
        should_eval!("head([1, 2])", Object::Array(vec![Object::Integer(1)]));
    }

    #[test]
    fn builtin_push() {
        should_eval!("push([], 1)", Object::Array(vec![Object::Integer(1)]));
        should_eval!(
            "push([1, 2], 1)",
            Object::Array(vec![
                Object::Integer(1),
                Object::Integer(2),
                Object::Integer(1)
            ])
        );
    }

    #[test]
    fn arrays() {
        should_eval!("[1, 2][0]", Object::Integer(1));
        should_eval!("[1, 2 + 2][1]", Object::Integer(4));
        should_eval!("[1, 2][1]", Object::Integer(2));
        should_eval!(
            "let stuff = fn(x) { return [x, x + 1]; }; stuff(10)[1];",
            Object::Integer(11)
        );
    }

    #[test]
    fn hashes() {
        should_eval!(
            "{\"foo\": \"bar\"}",
            Object::Hash(BTreeMap::from_iter(
                vec![(Hashable::String("foo".into()), Object::String("bar".into()))].into_iter()
            ))
        );

        should_eval!(
            "{\"foo\" + \"bar\": 2 + 2}",
            Object::Hash(BTreeMap::from_iter(
                vec![(Hashable::String("foobar".into()), Object::Integer(4))].into_iter()
            ))
        );

        should_eval!(
            "{\"foo\": 2 + 2}",
            Object::Hash(BTreeMap::from_iter(
                vec![(Hashable::String("foo".into()), Object::Integer(4))].into_iter()
            ))
        );
    }
}
