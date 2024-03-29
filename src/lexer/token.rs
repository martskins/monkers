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
    Colon,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Identifier(String),
    Number(String),
    String(String),
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
