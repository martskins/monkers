use crate::lexer::{Lexer, Token};
use std::io::{BufRead, Write};

static PROMPT: &[u8; 2] = &['>' as u8, '>' as u8];

pub fn start(mut input: impl BufRead, mut output: impl Write) -> Result<(), std::io::Error> {
    loop {
        output.write_all(PROMPT)?;
        output.flush()?;

        let mut line = String::new();
        input.read_line(&mut line)?;

        let mut lexer = Lexer::new(line);
        loop {
            let token = lexer.next_token();
            if let Token::EOF = token {
                break;
            }

            println!("{:?}", token);
        }
    }
}
