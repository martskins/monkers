mod lexer;
mod parser;
mod repl;

fn main() {
    let stdin = std::io::stdin();
    let stdout = std::io::stdout();
    let _ = repl::start(stdin.lock(), stdout.lock());
}
