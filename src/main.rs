#![allow(dead_code)]

mod lexer;
mod parser;
mod logging;

use parser::Parser;
use lexer::{Lexer, Error};

use rustyline;

use crate::logging::println_error;
extern crate term;

fn read_eval(line: &str) -> Result<(), Error> {
    let mut lxr = Lexer::new(line);
    lxr.initialize();
    let tokens = lxr.lex_tokens()?;
    if tokens.len() == 1 { return Ok(()); }

    let mut psr = Parser::new(tokens.clone());
    let ast = psr.parse_expression()?;

    println!("{}", ast);

    Ok(())
}

fn repl() -> rustyline::Result<()> {
    let mut rl = rustyline::DefaultEditor::new()?;
    // let mut stdout = term::stdout().unwrap();
    let mut stderr = term::stderr().unwrap();

    println!(
"    .----.
(\\./)     \\.  ....~
>' '<  (__.'\"\"\"
\" ` \" \"
===== Rat v0.1.0 ==
");

    loop {
        let readline = rl.readline("rat λ ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str()).unwrap();

                let read_eval_res = read_eval(&line);

                match read_eval_res {
                    Ok(_) => {}
                    Err(e) => { println!(); println_error("<interactive>".to_string(), e, &mut stderr); println!(); }
                };
            },
            Err(rustyline::error::ReadlineError::Interrupted) => {},
            Err(rustyline::error::ReadlineError::Eof) => {
                println!("🪤 Exiting Rat. Bye! 🐀");
                break
            },
            Err(err) => {
                println!("rustyline error: {:?}", err);
                break
            }
        }

    }

    Ok(())
}

fn main()  -> rustyline::Result<()> {
    repl()?;
    Ok(())
}
