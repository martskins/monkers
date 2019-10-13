use crate::evaluator::{Environment, Node};
use crate::lexer::{Lexer, Token};
use crate::parser::{Parser, Program};
use std::io::{BufRead, Result, Write};

static PROMPT: &[u8; 2] = &['>' as u8, ' ' as u8];

pub fn start(mut input: impl BufRead, mut output: impl Write) -> Result<()> {
    let mut env = Environment::new();

    loop {
        output.write_all(PROMPT)?;
        output.flush()?;

        let mut line = String::new();
        input.read_line(&mut line)?;

        let mut lexer = Lexer::new(line);
        let tokens = lexer.lex();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        match program.eval(&mut env) {
            Ok(res) => println!("{}\n", res),
            Err(e) => eprintln!("{:?}\n", e),
        }
    }
}
