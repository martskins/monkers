mod ast;
mod precedence;
mod result;
pub use ast::*;
pub use result::*;

use crate::lexer::{Keyword, Token};
use precedence::Precedence;
use result::{ParseError, Result};

type PrefixParseFn = Box<dyn Fn() -> Expression>;
type InfixParseFn = Box<dyn Fn(Expression) -> Expression>;

struct Parser {
    tokens: Vec<Token>,
    position: usize,
}

macro_rules! expect {
    ($t:expr, $p:expr) => {{
        if $t != $p {
            eprintln!("Unexpected token {:?}", $t);
            return Err(ParseError::UnexpectedToken($t.clone()));
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
            Token::Identifier(_) => self.parse_identifier_expression(),
            Token::Number(_) => self.parse_integer_literal_expression(),
            Token::LParen => self.parse_grouped_expression(),
            Token::Keyword(k) => match k {
                Keyword::True | Keyword::False => self.parse_boolean_expression(),
                Keyword::If => self.parse_if_expression(),
                Keyword::Function => self.parse_function_expression(),
                t => Err(ParseError::NoPrefix(Token::Keyword(t.clone()))),
            },
            Token::Minus | Token::Bang => self.parse_prefix_expression(),
            t => Err(ParseError::NoPrefix(t.clone())),
        }
    }

    fn infix_parse_fn(&mut self, token: Token, left: Expression) -> Result<Expression> {
        match token {
            Token::LParen => self.parse_call_expression(left),
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

    fn parse_call_arguments(&mut self) -> Result<Vec<Expression>> {
        self.next(); // skip LParen

        let mut arguments = vec![];
        while self.current_token() != &Token::RParen && self.current_token() != &Token::EOF {
            if self.current_token() == &Token::Comma {
                self.next();
                continue;
            }

            let ident = self.parse_expression(Precedence::Lowest);
            if let Ok(ident) = ident {
                arguments.push(ident);
            }

            if self.peek_token() == &Token::RParen || self.peek_token() == &Token::EOF {
                break;
            }

            self.next();
        }

        Ok(arguments)
    }

    fn parse_call_expression(&mut self, function: Expression) -> Result<Expression> {
        let arguments = self.parse_call_arguments()?;

        Ok(Expression::from(CallExpression {
            function: Box::new(function),
            arguments,
        }))
    }

    fn parse_if_expression(&mut self) -> Result<Expression> {
        expect!(self.next_token(), &Token::LParen);
        self.next();
        let condition = self.parse_expression(Precedence::Lowest)?;
        expect!(self.next_token(), &Token::RParen);
        expect!(self.next_token(), &Token::LBrace);
        let consequence = self.parse_block_statement()?;
        expect!(self.next_token(), &Token::RBrace);
        let alternative = if self.peek_token() == &Token::Keyword(Keyword::Else) {
            self.next();
            let stmt = self.parse_block_statement()?;
            expect!(self.next_token(), &Token::RBrace);

            Some(stmt)
        } else {
            None
        };

        let expr = IfExpression {
            condition: Box::new(condition),
            consequence,
            alternative,
        };

        Ok(Expression::If(expr))
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Identifier>> {
        self.next(); // skip LParen

        let mut parameters = vec![];
        while self.current_token() != &Token::RParen && self.current_token() != &Token::EOF {
            println!("LOOP: {:?}", self.current_token());
            if self.current_token() == &Token::Comma {
                self.next();
                continue;
            }

            let ident = self.parse_identifier_literal();
            if let Ok(ident) = ident {
                parameters.push(ident);
            }

            if self.peek_token() == &Token::RParen || self.peek_token() == &Token::EOF {
                break;
            }

            self.next();
        }

        Ok(parameters)
    }

    fn parse_function_literal(&mut self) -> Result<FunctionLiteral> {
        expect!(self.next_token(), &Token::LParen);
        let parameters = self.parse_function_parameters()?;
        expect!(self.next_token(), &Token::RParen);

        expect!(self.next_token(), &Token::LBrace);
        let body = self.parse_block_statement()?;
        expect!(self.next_token(), &Token::RBrace);

        Ok(FunctionLiteral { body, parameters })
    }

    fn parse_function_expression(&mut self) -> Result<Expression> {
        let expr = self.parse_function_literal()?;
        Ok(Expression::Function(expr))
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatement> {
        println!("PBS: {:?}", self.current_token());
        self.next(); // skip LBrace
        println!("PBS: {:?}", self.current_token());

        let mut statements = vec![];
        while self.current_token() != &Token::RBrace && self.current_token() != &Token::EOF {
            let stmt = self.parse_statement();
            if let Ok(stmt) = stmt {
                statements.push(stmt);
            }

            if self.peek_token() == &Token::RBrace || self.peek_token() == &Token::EOF {
                break;
            }

            self.next();
        }

        println!("PBS OUT: {:?}", self.current_token());
        Ok(BlockStatement { statements })
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression> {
        self.next(); // skip paren
        let expr = self.parse_expression(Precedence::Lowest);
        expect!(self.next_token(), &Token::RParen);
        expr
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

    fn parse_boolean_expression(&self) -> Result<Expression> {
        let result = match self.current_token() {
            Token::Keyword(Keyword::True) => BooleanLiteral { value: true },
            Token::Keyword(Keyword::False) => BooleanLiteral { value: false },
            _ => unreachable!(),
        };

        Ok(Expression::BooleanLiteral(result))
    }

    fn parse_identifier_literal(&self) -> Result<Identifier> {
        let token = self.current_token();
        if let Token::Identifier(value) = token {
            Ok(Identifier {
                value: value.clone(),
            })
        } else {
            Err(ParseError::UnexpectedToken(token.clone()))
        }
    }

    fn parse_identifier_expression(&self) -> Result<Expression> {
        let ident = self.parse_identifier_literal()?;
        Ok(Expression::Identifier(ident))
    }

    fn parse_integer_literal_expression(&self) -> Result<Expression> {
        let token = self.current_token();
        if let Token::Number(value) = token {
            Ok(Expression::from(IntegerLiteral {
                value: value.parse()?,
            }))
        } else {
            Err(ParseError::UnexpectedToken(token.clone()))
        }
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        let stmt = match self.current_token() {
            Token::Keyword(Keyword::Let) => Statement::Let(self.parse_let_statement()?),
            Token::Keyword(Keyword::Return) => Statement::Return(self.parse_return_statement()?),
            _ => Statement::Expression(self.parse_expression_statement()?),
        };

        Ok(stmt)
    }

    fn parse_expression_statement(&mut self) -> Result<ExpressionStatement> {
        let expr = self.parse_expression(Precedence::Lowest)?;
        if self.peek_token() == &Token::Semicolon {
            self.next();
        }

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
        let token = self.next_token();
        if let Token::Identifier(identifier) = token {
            let identifier = Identifier {
                value: identifier.clone(),
            };

            expect!(self.next_token(), &Token::Eq);

            loop {
                let t = self.next_token();
                if let Token::Semicolon = t {
                    break;
                }
            }
            let stmt = LetStatement { name: identifier };

            Ok(stmt)
        } else {
            Err(ParseError::UnexpectedToken(token.clone()))
        }
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
    fn parse_call_expression() {
        let program = parse("add(1, 2 * 3, 4 + 5");
        let actual = program.statements.first().unwrap();
        let expect = Statement::from(ExpressionStatement {
            value: Expression::from(CallExpression {
                function: Box::new(Expression::from(Identifier {
                    value: "add".into(),
                })),
                arguments: vec![
                    Expression::from(IntegerLiteral { value: 1 }),
                    Expression::from(InfixExpression {
                        operator: Operator::Mul,
                        left: Box::new(Expression::from(IntegerLiteral { value: 2 })),
                        right: Box::new(Expression::from(IntegerLiteral { value: 3 })),
                    }),
                    Expression::from(InfixExpression {
                        operator: Operator::Add,
                        left: Box::new(Expression::from(IntegerLiteral { value: 4 })),
                        right: Box::new(Expression::from(IntegerLiteral { value: 5 })),
                    }),
                ],
            }),
        });

        assert_eq!(&expect, actual);
    }

    #[test]
    fn parse_function_literal() {
        let program = parse("fn (x, y) { x };");
        let actual = program.statements.first().unwrap();
        let expect = Statement::Expression(ExpressionStatement {
            value: Expression::Function(FunctionLiteral {
                parameters: vec![
                    Identifier { value: "x".into() },
                    Identifier { value: "y".into() },
                ],
                body: BlockStatement {
                    statements: vec![Statement::Expression(ExpressionStatement {
                        value: Expression::Identifier(Identifier { value: "x".into() }),
                    })],
                },
            }),
        });

        assert_eq!(&expect, actual);
    }

    #[test]
    fn parse_if_else_expression() {
        let program = parse("if (x < y) { x } else { y };");
        let actual = program.statements.first().unwrap();
        let expect = Statement::Expression(ExpressionStatement {
            value: Expression::If(IfExpression {
                condition: Box::new(Expression::Infix(InfixExpression {
                    left: Box::new(Expression::Identifier(Identifier { value: "x".into() })),
                    operator: Operator::Lt,
                    right: Box::new(Expression::Identifier(Identifier { value: "y".into() })),
                })),
                consequence: BlockStatement {
                    statements: vec![Statement::Expression(ExpressionStatement {
                        value: Expression::Identifier(Identifier { value: "x".into() }),
                    })],
                },
                alternative: Some(BlockStatement {
                    statements: vec![Statement::Expression(ExpressionStatement {
                        value: Expression::Identifier(Identifier { value: "y".into() }),
                    })],
                }),
            }),
        });

        assert_eq!(&expect, actual);
    }

    #[test]
    fn parse_if_expression() {
        let program = parse("if (x < y) { x };");
        let actual = program.statements.first().unwrap();
        let expect = Statement::Expression(ExpressionStatement {
            value: Expression::If(IfExpression {
                condition: Box::new(Expression::Infix(InfixExpression {
                    left: Box::new(Expression::Identifier(Identifier { value: "x".into() })),
                    operator: Operator::Lt,
                    right: Box::new(Expression::Identifier(Identifier { value: "y".into() })),
                })),
                consequence: BlockStatement {
                    statements: vec![Statement::Expression(ExpressionStatement {
                        value: Expression::Identifier(Identifier { value: "x".into() }),
                    })],
                },
                alternative: None,
            }),
        });

        assert_eq!(&expect, actual);
    }

    #[test]
    fn parse_grouped_expression() {
        let program = parse("-(5 + 5);");
        let actual = program.statements.first().unwrap();
        let expect = Statement::Expression(ExpressionStatement {
            value: Expression::Prefix(PrefixExpression {
                operator: Operator::Minus,
                right: Box::new(Expression::Infix(InfixExpression {
                    left: Box::new(Expression::IntegerLiteral(IntegerLiteral { value: 5 })),
                    operator: Operator::Add,
                    right: Box::new(Expression::IntegerLiteral(IntegerLiteral { value: 5 })),
                })),
            }),
        });

        assert_eq!(&expect, actual);
    }

    #[test]
    fn parse_identifier_expression() {
        let parser = Parser::new(vec![Token::Identifier("someVar".into()), Token::Semicolon]);
        let actual = parser.parse_identifier_expression().unwrap();
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
        let actual = program.statements.first().unwrap();
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
    fn parse_boolean_literal() {
        let test_cases = vec![("true;", true), ("false;", false)];
        for test_case in test_cases {
            let program = parse(test_case.0);
            let actual = program.statements.first().unwrap();
            let expect = Statement::Expression(ExpressionStatement {
                value: Expression::BooleanLiteral(BooleanLiteral { value: test_case.1 }),
            });
            assert_eq!(&expect, actual);
        }
    }

    #[test]
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
