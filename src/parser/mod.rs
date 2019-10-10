mod ast;
mod error;
mod precedence;
pub use ast::*;
pub use error::*;

use crate::lexer::{Keyword, Token};
use error::{ParseError, Result};
use precedence::Precedence;

type PrefixParseFn = Box<dyn Fn() -> Expression>;
type InfixParseFn = Box<dyn Fn(Expression) -> Expression>;

struct Parser {
    tokens: Vec<Token>,
    position: usize,
}

macro_rules! must {
    ($self:ident, $p:pat, $b:expr) => {{
        match $self.current_token() {
            $p => $b,
            _ => unreachable!(),
        }
    }};
}

macro_rules! expect {
    ($self:ident, $p:pat) => {{
        let t = $self.next_token();
        match t {
            $p => Ok(()),
            _ => Err(ParseError::UnexpectedToken(t.clone())),
        }
    }};

    ($self:ident, $p:pat, $s:ident) => {{
        let t = $self.next_token();
        if let $p = t {
            Ok($s)
        } else {
            Err(ParseError::UnexpectedToken(t.clone()))
        }
    }};
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        let mut parser = Parser {
            tokens,
            position: 0,
        };

        parser
    }

    pub fn parse(&mut self) -> Result<Program> {
        let mut program = Program { statements: vec![] };
        loop {
            let token = self.current_token();
            if token == &Token::EOF {
                break;
            }

            let stmt = self.parse_statement()?;
            program.statements.push(stmt);

            self.next();
        }

        Ok(program)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression> {
        let token = self.current_token().clone();
        let mut left_expr = self.prefix_parse_fn(token)?;
        while self.peek_token() != &Token::Semicolon
            && precedence < Precedence::from(self.peek_token())
        {
            let token = self.next_token().clone();
            left_expr = self.infix_parse_fn(token, left_expr)?;
        }

        Ok(left_expr)
    }

    fn prefix_parse_fn(&mut self, token: Token) -> Result<Expression> {
        match token {
            Token::Identifier(_) => self.parse_identifier(),
            Token::Number(_) => self.parse_integer_literal_expression(),
            Token::Minus | Token::Bang => self.parse_prefix_expression(),
            t => Err(ParseError::NoPrefix(t.clone())),
        }
    }

    fn infix_parse_fn(&mut self, token: Token, left: Expression) -> Result<Expression> {
        match token {
            Token::Plus
            | Token::Minus
            | Token::Slash
            | Token::Asterisk
            | Token::EqEq
            | Token::NotEq
            | Token::LT
            | Token::GT => self.parse_infix_expression(left),
            t => Err(ParseError::NoInfix(t.clone())),
        }
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression> {
        let precedence = Precedence::from(self.current_token());
        let operator = Operator::from(self.current_token());
        self.next();
        let right = self.parse_expression(precedence)?;
        let expr = Expression::Infix(InfixExpression {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        });
        Ok(expr)
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression> {
        let operator = Operator::from(self.current_token());

        self.next();
        let right = self.parse_expression(Precedence::Prefix)?;
        let right = Box::new(right);

        Ok(Expression::Prefix(PrefixExpression { operator, right }))
    }

    fn parse_identifier(&self) -> Result<Expression> {
        let result = must!(
            self,
            Token::Identifier(value),
            Expression::Identifier(Identifier {
                value: value.clone()
            })
        );

        Ok(result)
    }

    fn parse_integer_literal_expression(&self) -> Result<Expression> {
        let result = must!(
            self,
            Token::Number(value),
            Expression::IntegerLiteral(IntegerLiteral {
                value: value.parse()?,
            })
        );

        Ok(result)
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        let stmt = match self.current_token() {
            Token::Keyword(Keyword::Let) => Statement::Let(self.parse_let_statement()?),
            Token::Keyword(Keyword::Return) => Statement::Return(self.parse_return_statement()?),
            _ => Statement::Expression(self.parse_expression_statement()?),
            // t => Err(ParseError::UnexpectedToken(t.clone())),
        };

        Ok(stmt)
    }

    fn parse_expression_statement(&mut self) -> Result<ExpressionStatement> {
        let expr = self.parse_expression(Precedence::Lowest)?;
        expect!(self, Token::Semicolon);

        Ok(ExpressionStatement { value: expr })
    }

    fn parse_return_statement(&mut self) -> Result<ReturnStatement> {
        loop {
            let t = self.next_token();
            if let Token::Semicolon = t {
                break;
            }
        }

        Ok(ReturnStatement {})
    }

    fn parse_let_statement(&mut self) -> Result<LetStatement> {
        let identifier = expect!(self, Token::Identifier(s), s)?;
        let identifier = Identifier {
            value: identifier.clone(),
        };

        expect!(self, Token::Eq)?;

        loop {
            let t = self.next_token();
            if let Token::Semicolon = t {
                break;
            }
        }
        let stmt = LetStatement { name: identifier };

        Ok(stmt)
    }

    fn next_token(&mut self) -> &Token {
        self.next();
        self.current_token()
    }

    fn next(&mut self) {
        self.position += 1;
    }

    fn current_token(&self) -> &Token {
        &self.tokens[self.position]
    }

    fn peek_token(&self) -> &Token {
        &self.tokens[self.position + 1]
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::*;
    use crate::parser::*;

    fn parse(input: &str) -> Program {
        let mut lexer = Lexer::new(input.into());
        let tokens = lexer.lex();
        let mut parser = Parser::new(tokens);
        parser.parse().unwrap()
    }

    #[test]
    fn parse_identifier() {
        let parser = Parser::new(vec![Token::Identifier("someVar".into()), Token::Semicolon]);
        let actual = parser.parse_identifier().unwrap();
        let expect = Expression::Identifier(Identifier {
            value: "someVar".into(),
        });
        assert_eq!(expect, actual);
    }

    #[test]
    fn parse_prefix_expression_bang() {
        let mut parser = Parser::new(vec![
            Token::Bang,
            Token::Number("5".into()),
            Token::Semicolon,
        ]);
        let actual = parser.parse_prefix_expression().unwrap();
        let expect = Expression::Prefix(PrefixExpression {
            operator: Operator::Bang,
            right: Box::new(Expression::IntegerLiteral(IntegerLiteral { value: 5 })),
        });
        assert_eq!(expect, actual);
    }

    #[test]
    fn parse_prefix_expression_minus() {
        let mut parser = Parser::new(vec![
            Token::Minus,
            Token::Number("15".into()),
            Token::Semicolon,
        ]);
        let actual = parser.parse_prefix_expression().unwrap();
        let expect = Expression::Prefix(PrefixExpression {
            operator: Operator::Minus,
            right: Box::new(Expression::IntegerLiteral(IntegerLiteral { value: 15 })),
        });
        assert_eq!(expect, actual);
    }

    #[test]
    fn parse_infix_expression_star() {
        let input = "5 * 5;";
        let program = parse(input);
        let actual = &program.statements[0];
        let expect = Statement::Expression(ExpressionStatement {
            value: Expression::Infix(InfixExpression {
                left: Box::new(Expression::IntegerLiteral(IntegerLiteral { value: 5 })),
                operator: Operator::Mul,
                right: Box::new(Expression::IntegerLiteral(IntegerLiteral { value: 5 })),
            }),
        });
        assert_eq!(&expect, actual);
    }

    #[test]
    #[test]
    fn parse_infix_expression_minus() {
        let input = "5 - 5;";
        let program = parse(input);
        let actual = &program.statements[0];
        let expect = Statement::Expression(ExpressionStatement {
            value: Expression::Infix(InfixExpression {
                left: Box::new(Expression::IntegerLiteral(IntegerLiteral { value: 5 })),
                operator: Operator::Minus,
                right: Box::new(Expression::IntegerLiteral(IntegerLiteral { value: 5 })),
            }),
        });
        assert_eq!(&expect, actual);
    }

    #[test]
    fn parse_infix_expression_plus() {
        let input = "5 + 5;";
        let program = parse(input);
        let actual = &program.statements[0];
        let expect = Statement::Expression(ExpressionStatement {
            value: Expression::Infix(InfixExpression {
                left: Box::new(Expression::IntegerLiteral(IntegerLiteral { value: 5 })),
                operator: Operator::Add,
                right: Box::new(Expression::IntegerLiteral(IntegerLiteral { value: 5 })),
            }),
        });
        assert_eq!(&expect, actual);
    }

    #[test]
    fn parse_integer_literal_expression() {
        let parser = Parser::new(vec![Token::Number("5".into()), Token::Semicolon]);
        let actual = parser.parse_integer_literal_expression().unwrap();
        let expect = Expression::IntegerLiteral(IntegerLiteral { value: 5 });
        assert_eq!(expect, actual);
    }

    #[test]
    fn test_identifier_expressions() {
        let program = parse("foobar;");
        match &program.statements[0] {
            Statement::Expression(e) => match &e.value {
                Expression::Identifier(i) => assert_eq!("foobar".to_owned(), i.value),
                _ => panic!("test failed"),
            },
            _ => panic!("test failed"),
        }
    }

    #[test]
    fn test_expression_statements() {
        let program = parse("let myVar = anotherVar;");
        let expected = Program {
            statements: vec![Statement::Let(LetStatement {
                name: Identifier {
                    value: "myVar".into(),
                },
                // value: Expression::Identifier(Identifier {
                //     value: "anotherVar".into(),
                // }),
            })],
        };

        assert_eq!(expected, program);
    }

    #[test]
    fn test_return_statement() {
        let input = r#"
            return 5;
            return 10;
            return 838383;
        "#;
        let program = parse(input);

        assert_eq!(3, program.statements.len());

        match &program.statements[0] {
            Statement::Return(_) => {}
            _ => panic!("test failed"),
        }

        match &program.statements[1] {
            Statement::Return(_) => {}
            _ => panic!("test failed"),
        }

        match &program.statements[2] {
            Statement::Return(_) => {}
            _ => panic!("test failed"),
        }
    }

    #[test]
    fn test_let_statements() {
        let input = r#"
            let x = 5;
            let y = 10;
            let foobar = 838383;
        "#;
        let program = parse(input);

        assert_eq!(3, program.statements.len());

        match &program.statements[0] {
            Statement::Let(l) => assert_eq!("x", l.name.value),
            _ => panic!("test failed"),
        }

        match &program.statements[1] {
            Statement::Let(l) => assert_eq!("y", l.name.value),
            _ => panic!("test failed"),
        }

        match &program.statements[2] {
            Statement::Let(l) => assert_eq!("foobar", l.name.value),
            _ => panic!("test failed"),
        }
    }
}
