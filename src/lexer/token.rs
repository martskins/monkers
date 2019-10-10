#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum Token {
    Illegal,
    EOF,

    Eq,
    EqEq,
    NotEq,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Bang,
    LT,
    GT,

    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Identifier(String),
    Number(String),
    Keyword(Keyword),
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum Keyword {
    Function,
    Let,
    Else,
    If,
    Return,
    True,
    False,
}
