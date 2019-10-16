mod ast;
mod precedence;
mod result;
pub use ast::*;
pub use result::*;

use crate::lexer::{Keyword, Lexer, Token};
use precedence::Precedence;
use result::{ParseError, Result};

pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
}

macro_rules! expect_token {
    ($t:expr, $p:expr) => {{
        if $t != $p {
            return Err(ParseError::UnexpectedToken($t.clone()));
        }
    }};
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        let parser = Parser {
            tokens,
            position: 0,
        };

        parser
    }

    pub fn from(input: &str) -> Parser {
        let mut lexer = Lexer::new(input);
        let tokens = lexer.lex();
        let parser = Parser {
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

        self.next(); // skip RParen
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
        expect_token!(self.next_token(), &Token::LParen);
        let condition = self.parse_expression(Precedence::Lowest)?;

        expect_token!(self.next_token(), &Token::LBrace);
        let consequence = self.parse_block_statement()?;
        expect_token!(self.next_token(), &Token::RBrace);

        let alternative = if self.peek_token() == &Token::Keyword(Keyword::Else) {
            self.next();
            self.next();
            let stmt = self.parse_block_statement()?;
            expect_token!(self.next_token(), &Token::RBrace);

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
        expect_token!(self.next_token(), &Token::LParen);
        let parameters = self.parse_function_parameters()?;
        expect_token!(self.next_token(), &Token::RParen);

        expect_token!(self.next_token(), &Token::LBrace);
        let body = self.parse_block_statement()?;
        expect_token!(self.next_token(), &Token::RBrace);

        Ok(FunctionLiteral { body, parameters })
    }

    fn parse_function_expression(&mut self) -> Result<Expression> {
        let expr = self.parse_function_literal()?;
        Ok(Expression::Function(expr))
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatement> {
        self.next(); // skip LBrace

        let mut statements = vec![];
        while self.current_token() != &Token::RBrace && self.current_token() != &Token::EOF {
            let stmt = self.parse_statement()?;
            statements.push(stmt);

            if self.peek_token() == &Token::RBrace || self.peek_token() == &Token::EOF {
                break;
            }

            self.next();
        }

        Ok(BlockStatement { statements })
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression> {
        self.next(); // skip paren

        let expr = self.parse_expression(Precedence::Lowest)?;
        expect_token!(self.next_token(), &Token::RParen);

        Ok(Expression::from(GroupedExpression {
            value: Box::new(expr),
        }))
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
        self.next(); // skip return token

        let value = self.parse_expression(Precedence::Lowest)?;
        expect_token!(self.next_token(), &Token::Semicolon);

        Ok(ReturnStatement { value })
    }

    fn parse_let_statement(&mut self) -> Result<LetStatement> {
        let token = self.next_token();
        if let Token::Identifier(identifier) = token {
            let name = Identifier {
                value: identifier.clone(),
            };

            expect_token!(self.next_token(), &Token::Eq);
            self.next();

            let value = self.parse_expression(Precedence::Lowest)?;
            expect_token!(self.next_token(), &Token::Semicolon);

            Ok(LetStatement { name, value })
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

    fn parser_from(input: &str) -> Parser {
        let mut lexer = Lexer::new(input.into());
        let tokens = lexer.lex();
        Parser::new(tokens)
    }

    fn program_from(input: &str) -> Program {
        let mut parser = parser_from(input);
        parser.parse().unwrap()
    }

    macro_rules! should_parse {
        ($input:expr, $expect:expr) => {
            let mut parser = Parser::from($input);
            let program = parser.parse().unwrap();
            assert_ne!(0, program.statements.len());
            assert_eq!($expect, program.statements[0]);
        };
    }

    #[test]
    fn parse_call_expression() {
        let mut parser = parser_from("add(1, 2 * 3, 4 + 5)");
        let actual = parser.parse_expression(Precedence::Lowest).unwrap();
        let expect = Expression::from(CallExpression {
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
        });

        assert_eq!(expect, actual);
    }

    #[test]
    fn parse_function_statement() {
        let expect = Statement::from(LetStatement {
            name: Identifier {
                value: "ident".into(),
            },
            value: Expression::from(FunctionLiteral {
                parameters: vec![Identifier { value: "x".into() }],
                body: BlockStatement {
                    statements: vec![Statement::from(ReturnStatement {
                        value: Expression::from(Identifier { value: "x".into() }),
                    })],
                },
            }),
        });
        should_parse!("let ident = fn(x) { return x; }; ident(2);", expect);
    }

    #[test]
    fn parse_function_literal() {
        let mut parser = parser_from("fn (x, y) { x };");
        let actual = parser.parse_function_literal().unwrap();
        let expect = FunctionLiteral {
            parameters: vec![
                Identifier { value: "x".into() },
                Identifier { value: "y".into() },
            ],
            body: BlockStatement {
                statements: vec![Statement::Expression(ExpressionStatement {
                    value: Expression::Identifier(Identifier { value: "x".into() }),
                })],
            },
        };

        assert_eq!(expect, actual);
    }

    #[test]
    fn parse_grouped_expression() {
        let mut parser = parser_from("-(5 + 5);");
        let actual = parser.parse_expression(Precedence::Lowest).unwrap();
        let expect = Expression::Prefix(PrefixExpression {
            operator: Operator::Minus,
            right: Box::new(Expression::Grouped(GroupedExpression {
                value: Box::new(Expression::Infix(InfixExpression {
                    left: Box::new(Expression::IntegerLiteral(IntegerLiteral { value: 5 })),
                    operator: Operator::Add,
                    right: Box::new(Expression::IntegerLiteral(IntegerLiteral { value: 5 })),
                })),
            })),
        });

        assert_eq!(expect, actual);
    }

    #[test]
    fn parse_identifier_expression() {
        let parser = parser_from("someVar;");
        let actual = parser.parse_identifier_expression().unwrap();
        let expect = Expression::Identifier(Identifier {
            value: "someVar".into(),
        });
        assert_eq!(expect, actual);
    }

    #[test]
    fn parse_prefix_expression_bang() {
        let mut parser = parser_from("!5;");
        let actual = parser.parse_prefix_expression().unwrap();
        let expect = Expression::Prefix(PrefixExpression {
            operator: Operator::Bang,
            right: Box::new(Expression::IntegerLiteral(IntegerLiteral { value: 5 })),
        });
        assert_eq!(expect, actual);
    }

    #[test]
    fn parse_prefix_expression_minus() {
        let mut parser = parser_from("-15;");
        let actual = parser.parse_prefix_expression().unwrap();
        let expect = Expression::Prefix(PrefixExpression {
            operator: Operator::Minus,
            right: Box::new(Expression::IntegerLiteral(IntegerLiteral { value: 15 })),
        });
        assert_eq!(expect, actual);
    }

    #[test]
    fn parse_infix_expression_star() {
        let mut parser = parser_from("5 * 5");
        let actual = parser.parse_expression(Precedence::Lowest).unwrap();
        let expect = Expression::Infix(InfixExpression {
            left: Box::new(Expression::IntegerLiteral(IntegerLiteral { value: 5 })),
            operator: Operator::Mul,
            right: Box::new(Expression::IntegerLiteral(IntegerLiteral { value: 5 })),
        });
        assert_eq!(expect, actual);
    }

    #[test]
    fn parse_infix_expression_minus() {
        let mut parser = parser_from("5 - 5");
        let actual = parser.parse_expression(Precedence::Lowest).unwrap();
        let expect = Expression::Infix(InfixExpression {
            left: Box::new(Expression::IntegerLiteral(IntegerLiteral { value: 5 })),
            operator: Operator::Minus,
            right: Box::new(Expression::IntegerLiteral(IntegerLiteral { value: 5 })),
        });
        assert_eq!(expect, actual);
    }

    #[test]
    fn parse_infix_expression_plus() {
        let mut parser = parser_from("5 + 5;");
        let actual = parser.parse_expression(Precedence::Lowest).unwrap();
        let expect = Expression::Infix(InfixExpression {
            left: Box::new(Expression::IntegerLiteral(IntegerLiteral { value: 5 })),
            operator: Operator::Add,
            right: Box::new(Expression::IntegerLiteral(IntegerLiteral { value: 5 })),
        });
        assert_eq!(expect, actual);
    }

    #[test]
    fn parse_if_expression() {
        let mut parser = parser_from("if (x > y) { x } else { y }");
        let actual = parser.parse_if_expression().unwrap();
        let expect = Expression::If(IfExpression {
            condition: Box::new(Expression::Grouped(GroupedExpression {
                value: Box::new(Expression::Infix(InfixExpression {
                    operator: Operator::Gt,
                    left: Box::new(Expression::from(Identifier { value: "x".into() })),
                    right: Box::new(Expression::from(Identifier { value: "y".into() })),
                })),
            })),
            consequence: BlockStatement {
                statements: vec![Statement::from(ExpressionStatement {
                    value: Expression::from(Identifier { value: "x".into() }),
                })],
            },
            alternative: Some(BlockStatement {
                statements: vec![Statement::from(ExpressionStatement {
                    value: Expression::from(Identifier { value: "y".into() }),
                })],
            }),
        });

        assert_eq!(expect, actual);
    }

    #[test]
    fn parse_integer_literal_expression() {
        let parser = parser_from("5;");
        let actual = parser.parse_integer_literal_expression().unwrap();
        let expect = Expression::IntegerLiteral(IntegerLiteral { value: 5 });
        assert_eq!(expect, actual);
    }

    #[test]
    fn parse_boolean_literal() {
        let test_cases = vec![("true;", true), ("false;", false)];
        for test_case in test_cases {
            let parser = parser_from(test_case.0);
            let actual = parser.parse_boolean_expression().unwrap();
            let expect = Expression::BooleanLiteral(BooleanLiteral { value: test_case.1 });
            assert_eq!(expect, actual);
        }
    }

    #[test]
    fn test_identifier_expressions() {
        let parser = parser_from("foobar;");
        let actual = parser.parse_identifier_expression().unwrap();
        let expect = Expression::from(Identifier {
            value: "foobar".into(),
        });
        assert_eq!(expect, actual);
    }

    #[test]
    fn test_expression_statements() {
        let mut parser = parser_from("let myVar = anotherVar;");
        let actual = parser.parse_statement().unwrap();
        let expected = Statement::Let(LetStatement {
            name: Identifier {
                value: "myVar".into(),
            },
            value: Expression::Identifier(Identifier {
                value: "anotherVar".into(),
            }),
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_return_statement() {
        let input = r#"
            return 5;
            return 10;
            return 838383;
        "#;
        let program = program_from(input);

        assert_eq!(3, program.statements.len());

        let expect = Statement::from(ReturnStatement {
            value: Expression::from(IntegerLiteral { value: 5 }),
        });
        assert_eq!(expect, program.statements[0]);

        let expect = Statement::from(ReturnStatement {
            value: Expression::from(IntegerLiteral { value: 10 }),
        });
        assert_eq!(expect, program.statements[1]);

        let expect = Statement::from(ReturnStatement {
            value: Expression::from(IntegerLiteral { value: 838383 }),
        });
        assert_eq!(expect, program.statements[2]);
    }

    #[test]
    fn parse_let_statements() {
        let input = r#"
            let x = 5;
            let y = 10;
            let foobar = 838383;
        "#;
        let program = program_from(input);

        assert_eq!(3, program.statements.len());

        let expect = Statement::from(LetStatement {
            name: Identifier { value: "x".into() },
            value: Expression::from(IntegerLiteral { value: 5 }),
        });
        assert_eq!(expect, program.statements[0]);

        let expect = Statement::from(LetStatement {
            name: Identifier { value: "y".into() },
            value: Expression::from(IntegerLiteral { value: 10 }),
        });
        assert_eq!(expect, program.statements[1]);

        let expect = Statement::from(LetStatement {
            name: Identifier {
                value: "foobar".into(),
            },
            value: Expression::from(IntegerLiteral { value: 838383 }),
        });
        assert_eq!(expect, program.statements[2]);
    }
}
