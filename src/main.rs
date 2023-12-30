#![allow(dead_code)]

mod lexer;
mod parser;
mod logging;
mod data;

use std::collections::HashMap;

use parser::Parser;
use lexer::{Lexer, Error};

use rustyline;

use crate::{logging::{println_error, println_errors}, data::{Substitutable, Environment, Namespace}};
extern crate term;

fn read_eval(space: &mut Namespace, line: &str) -> Result<(), Error> {
    let mut lxr = Lexer::new(line);
    lxr.initialize();
    let tokens = lxr.lex_tokens()?;
    if tokens.len() == 1 { return Ok(()); }

    let mut psr = Parser::new(tokens.clone());
    let ast = psr.parse_line_repl()?;

    let mut val = match space.from_ast(ast) {
        Ok(v) => v,
        Err(e) => {
            let err = Error::from_token(&e.0, &tokens[e.1.0]);
            let err_pos1 = tokens[e.1.0].pos;
            let err_pos2 = tokens[e.1.1].pos;
            // println_error("<interactive>".to_string(), Error::from_token(&e.0, &tokens[e.1.0]), &mut term::stdout().unwrap());
            println_errors("<interactive>".to_string(), err, (err_pos1, err_pos2), &mut term::stdout().unwrap());
            return Ok(());
        }
    };

    let mut env = Environment::new();
    let ty = env.generate_type_var();

    match space.alg_m(&mut val, &mut env, &ty) {
        Ok(sub) => {
            // println!("{}", ty.apply(&sub));
            println!("{}", ty.apply(&sub).simplify(&mut HashMap::new()));
        }
        Err(e) => {
            let err = Error::from_token(&e.0, &tokens[e.1.0]);
            let err_pos1 = tokens[e.1.0].pos;
            let err_pos2 = tokens[e.1.1].pos;
            // println_error("<interactive>".to_string(), Error::from_token(&e.0, &tokens[e.1.0]), &mut term::stdout().unwrap());
            println_errors("<interactive>".to_string(), err, (err_pos1, err_pos2), &mut term::stdout().unwrap());
        }
    }

    println!("\n{}", val);


    Ok(())
}

fn repl() -> rustyline::Result<()> {
    let mut rl = rustyline::DefaultEditor::new()?;
    let mut stdout = term::stdout().unwrap();

    println!(
"    .----.
(\\./)     \\.  ....~
>' '<  (__.'\"\"\"
\" ` \" \"
===== Rat v0.0.1 ==
");

    let mut space = Namespace::new();

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str()).unwrap();

                let read_eval_res = read_eval(&mut space, &line);

                match read_eval_res {
                    Ok(_) => {}
                    Err(e) => { println_error("<interactive>".to_string(), e, &mut stdout); }
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
