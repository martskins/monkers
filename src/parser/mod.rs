mod ast;
mod parsers;
pub use ast::*;
pub use parsers::*;

use crate::lexer::{Keyword, Token};

type PrefixParseFn = Box<dyn Fn() -> Expression>;
type InfixParseFn = Box<dyn Fn(Expression) -> Expression>;

struct Parser {
    tokens: Vec<Token>,
    position: usize,
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(Token),
}

type Result<T> = std::result::Result<T, ParseError>;

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

    fn prefix_parse_fn(&self, token: Token) -> Result<Expression> {
        match token {
            Token::Identifier(_) => Ok(self.parse_identifier()),
            _ => unimplemented!(),
        }
    }

    fn parse_identifier(&self) -> Expression {
        match self.current_token() {
            Token::Identifier(value) => Expression::Identifier(Identifier {
                value: value.clone(),
            }),
            _ => unreachable!(),
        }
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

    fn parse_statement(&mut self) -> Result<Statement> {
        match self.current_token() {
            Token::Keyword(Keyword::Let) => self.parse_let_statement(),
            Token::Keyword(Keyword::Return) => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
            // t => Err(ParseError::UnexpectedToken(t.clone())),
        }
    }

    fn parse_expression_statement(&mut self) -> Result<Statement> {
        let expr = self.parse_expression(Precedence::Lowest)?;
        expect!(self, Token::Semicolon);

        Ok(Statement::Expression(ExpressionStatement { value: expr }))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression> {
        let token = self.current_token().clone();
        self.prefix_parse_fn(token)
    }

    fn parse_return_statement(&mut self) -> Result<Statement> {
        loop {
            let t = self.next_token();
            if let Token::Semicolon = t {
                break;
            }
        }

        Ok(Statement::Return(ReturnStatement {}))
    }

    fn parse_let_statement(&mut self) -> Result<Statement> {
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

        Ok(Statement::Let(stmt))
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

    #[test]
    fn test_identifier_expressions() {
        let input = "foobar;";
        let mut lexer = Lexer::new(input.to_owned());
        let tokens = lexer.lex();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();

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
        let input = "let myVar = anotherVar;";
        let mut lexer = Lexer::new(input.to_owned());
        let tokens = lexer.lex();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();

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
        let mut lexer = Lexer::new(input.to_owned());
        let tokens = lexer.lex();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();

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
        let mut lexer = Lexer::new(input.to_owned());
        let tokens = lexer.lex();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();

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
