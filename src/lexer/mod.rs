mod token;
pub use token::*;

pub struct Lexer {
    position: usize,
    chars: Vec<char>,
}

impl Lexer {
    pub fn new(input: &str) -> Lexer {
        Lexer {
            position: 0,
            chars: input.chars().collect(),
        }
    }

    fn read(&mut self) -> &char {
        let c = self.chars.get(self.position).unwrap_or(&(0 as u8 as char));
        self.position += 1;
        c
    }

    fn back(&mut self) {
        self.position -= 1;
    }

    fn lex_string(&mut self) -> Token {
        let mut value = String::new();
        let mut c = self.read();
        while c != &'"' {
            value.push(c.clone());
            c = self.read();
        }

        Token::String(value)
    }

    fn lex_number(&mut self) -> Token {
        let mut value = String::new();
        let mut c = self.read();
        while c.is_numeric() {
            value.push(c.clone());
            c = self.read();
        }
        self.back();

        Token::Number(value)
    }

    fn lex_identifier(&mut self) -> Token {
        let mut value = String::new();
        let mut c = self.read();
        while c.is_alphabetic() {
            value.push(c.clone());
            c = self.read();
        }
        self.back();

        match value.as_str() {
            "let" => Token::Keyword(Keyword::Let),
            "fn" => Token::Keyword(Keyword::Function),
            "if" => Token::Keyword(Keyword::If),
            "else" => Token::Keyword(Keyword::Else),
            "return" => Token::Keyword(Keyword::Return),
            "true" => Token::Keyword(Keyword::True),
            "false" => Token::Keyword(Keyword::False),
            _ => Token::Identifier(value),
        }
    }

    pub fn next_token(&mut self) -> Token {
        if self.position == self.chars.len() {
            return Token::EOF;
        }

        match self.read() {
            '=' => {
                if self.read() == &'=' {
                    return Token::EqEq;
                } else {
                    self.back();
                    return Token::Eq;
                }
            }
            '!' => {
                if self.read() == &'=' {
                    return Token::NotEq;
                } else {
                    self.back();
                    return Token::Bang;
                }
            }
            '+' => Token::Plus,
            '-' => Token::Minus,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            '[' => Token::LBracket,
            ']' => Token::RBracket,
            ',' => Token::Comma,
            ';' => Token::Semicolon,
            '<' => Token::LT,
            '>' => Token::GT,
            '/' => Token::Slash,
            '*' => Token::Asterisk,
            ' ' | '\t' | '\n' | '\r' => self.next_token(),
            'a'..='z' | 'A'..='Z' | '_' => {
                self.back();
                self.lex_identifier()
            }
            '0'..='9' => {
                self.back();
                self.lex_number()
            }
            '"' => self.lex_string(),
            c => panic!("unrecognized character {}", c),
        }
    }

    pub fn lex(&mut self) -> Vec<Token> {
        let mut tokens = vec![];
        loop {
            let token = self.next_token();
            if let Token::EOF = token {
                break;
            }

            tokens.push(token);
        }

        tokens.push(Token::EOF);
        tokens
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::{Keyword, Lexer, Token};

    #[test]
    fn test_next_token() {
        let input = r#" let five = 5;
                        let ten = 10;
                        let add = fn(x, y) {
                        x + y;
                        };
                        let result = add(five, ten);

                        !-/*5;
                        5 < 10 > 5;

                        if (5 < 10) {
                        return true;
                        } else {
                        return false;
                        }

                        10 == 10;
                        10 != 9;

                        "foobar";
                        "foo bar";

                        [1, 2];
                        "#;

        let mut lexer = Lexer::new(input.into());
        assert_eq!(Token::Keyword(Keyword::Let), lexer.next_token());
        assert_eq!(Token::Identifier("five".into()), lexer.next_token());
        assert_eq!(Token::Eq, lexer.next_token());
        assert_eq!(Token::Number("5".into()), lexer.next_token());
        assert_eq!(Token::Semicolon, lexer.next_token());

        assert_eq!(Token::Keyword(Keyword::Let), lexer.next_token());
        assert_eq!(Token::Identifier("ten".into()), lexer.next_token());
        assert_eq!(Token::Eq, lexer.next_token());
        assert_eq!(Token::Number("10".into()), lexer.next_token());
        assert_eq!(Token::Semicolon, lexer.next_token());

        assert_eq!(Token::Keyword(Keyword::Let), lexer.next_token());
        assert_eq!(Token::Identifier("add".into()), lexer.next_token());
        assert_eq!(Token::Eq, lexer.next_token());
        assert_eq!(Token::Keyword(Keyword::Function), lexer.next_token());
        assert_eq!(Token::LParen, lexer.next_token());
        assert_eq!(Token::Identifier("x".into()), lexer.next_token());
        assert_eq!(Token::Comma, lexer.next_token());
        assert_eq!(Token::Identifier("y".into()), lexer.next_token());
        assert_eq!(Token::RParen, lexer.next_token());
        assert_eq!(Token::LBrace, lexer.next_token());
        assert_eq!(Token::Identifier("x".into()), lexer.next_token());
        assert_eq!(Token::Plus, lexer.next_token());
        assert_eq!(Token::Identifier("y".into()), lexer.next_token());
        assert_eq!(Token::Semicolon, lexer.next_token());
        assert_eq!(Token::RBrace, lexer.next_token());
        assert_eq!(Token::Semicolon, lexer.next_token());

        assert_eq!(Token::Keyword(Keyword::Let), lexer.next_token());
        assert_eq!(Token::Identifier("result".into()), lexer.next_token());
        assert_eq!(Token::Eq, lexer.next_token());
        assert_eq!(Token::Identifier("add".into()), lexer.next_token());
        assert_eq!(Token::LParen, lexer.next_token());
        assert_eq!(Token::Identifier("five".into()), lexer.next_token());
        assert_eq!(Token::Comma, lexer.next_token());
        assert_eq!(Token::Identifier("ten".into()), lexer.next_token());
        assert_eq!(Token::RParen, lexer.next_token());
        assert_eq!(Token::Semicolon, lexer.next_token());

        assert_eq!(Token::Bang, lexer.next_token());
        assert_eq!(Token::Minus, lexer.next_token());
        assert_eq!(Token::Slash, lexer.next_token());
        assert_eq!(Token::Asterisk, lexer.next_token());
        assert_eq!(Token::Number("5".into()), lexer.next_token());
        assert_eq!(Token::Semicolon, lexer.next_token());

        assert_eq!(Token::Number("5".into()), lexer.next_token());
        assert_eq!(Token::LT, lexer.next_token());
        assert_eq!(Token::Number("10".into()), lexer.next_token());
        assert_eq!(Token::GT, lexer.next_token());
        assert_eq!(Token::Number("5".into()), lexer.next_token());
        assert_eq!(Token::Semicolon, lexer.next_token());

        assert_eq!(Token::Keyword(Keyword::If), lexer.next_token());
        assert_eq!(Token::LParen, lexer.next_token());
        assert_eq!(Token::Number("5".into()), lexer.next_token());
        assert_eq!(Token::LT, lexer.next_token());
        assert_eq!(Token::Number("10".into()), lexer.next_token());
        assert_eq!(Token::RParen, lexer.next_token());
        assert_eq!(Token::LBrace, lexer.next_token());
        assert_eq!(Token::Keyword(Keyword::Return), lexer.next_token());
        assert_eq!(Token::Keyword(Keyword::True), lexer.next_token());
        assert_eq!(Token::Semicolon, lexer.next_token());
        assert_eq!(Token::RBrace, lexer.next_token());
        assert_eq!(Token::Keyword(Keyword::Else), lexer.next_token());
        assert_eq!(Token::LBrace, lexer.next_token());
        assert_eq!(Token::Keyword(Keyword::Return), lexer.next_token());
        assert_eq!(Token::Keyword(Keyword::False), lexer.next_token());
        assert_eq!(Token::Semicolon, lexer.next_token());
        assert_eq!(Token::RBrace, lexer.next_token());

        assert_eq!(Token::Number("10".into()), lexer.next_token());
        assert_eq!(Token::EqEq, lexer.next_token());
        assert_eq!(Token::Number("10".into()), lexer.next_token());
        assert_eq!(Token::Semicolon, lexer.next_token());

        assert_eq!(Token::Number("10".into()), lexer.next_token());
        assert_eq!(Token::NotEq, lexer.next_token());
        assert_eq!(Token::Number("9".into()), lexer.next_token());
        assert_eq!(Token::Semicolon, lexer.next_token());

        assert_eq!(Token::String("foobar".into()), lexer.next_token());
        assert_eq!(Token::Semicolon, lexer.next_token());
        assert_eq!(Token::String("foo bar".into()), lexer.next_token());
        assert_eq!(Token::Semicolon, lexer.next_token());

        assert_eq!(Token::LBracket, lexer.next_token());
        assert_eq!(Token::Number("1".into()), lexer.next_token());
        assert_eq!(Token::Comma, lexer.next_token());
        assert_eq!(Token::Number("2".into()), lexer.next_token());
        assert_eq!(Token::RBracket, lexer.next_token());
        assert_eq!(Token::Semicolon, lexer.next_token());
    }
}
