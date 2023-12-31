#![allow(dead_code)]

mod lexer;
mod parser;
mod logging;
mod data;

mod interactive;

use std::env;
use std::fs;

use interactive::repl;

use lexer::Error;
use rustyline;

use crate::data::Environment;
use crate::data::Namespace;
use crate::lexer::Lexer;
use crate::logging::println_error;
use crate::logging::println_error_msg;
use crate::logging::println_errors;
use crate::parser::Parser;

extern crate term;


fn main()  -> rustyline::Result<()> {
    let args: Vec<String> = env::args().collect();

    match args.len() {
        1 => repl()?,

        2 => {
            let path = &args[1];

            let contents =
                match fs::read_to_string(path) {
                    Ok(o) => o,
                    Err(e) => {
                        println_error_msg(&format!("Could not read file `{}`: {}", path, e));
                        return Ok(())
                    },
            };

            let mut space = Namespace::new();

            let mut lxr = Lexer::new(&contents);
            lxr.initialize();
            let tokens = lxr.lex_tokens()
                .unwrap_or_else(|err| { println_error(path, err); std::process::exit(1) });

            if tokens.len() == 1 { return Ok(()); }

            let mut psr = Parser::new(tokens.clone());
            let asts = psr.parse_file()
                .unwrap_or_else(|err| { println_error(path, err); std::process::exit(1) });

            for ast in asts {
                let mut val = match space.from_ast(ast) {
                    Ok(v) => v,
                    Err(e) => {
                        let err = Error::from_token(&e.0, &tokens[e.1.0]);
                        let err_pos1 = tokens[e.1.0].pos;
                        let err_pos2 = tokens[e.1.1].pos;
                        println_errors(path, err, (err_pos1, err_pos2));
                        return Ok(());
                    }
                };

                let mut env = Environment::new();

                space.alg_w(&mut val, &mut env)
                    .unwrap_or_else(|e| {
                        let err = Error::from_token(&e.0, &tokens[e.1.0]);
                        let err_pos1 = tokens[e.1.0].pos;
                        let err_pos2 = tokens[e.1.1].pos;
                        println_errors(path, err, (err_pos1, err_pos2));
                        std::process::exit(1);
                    });
            }

            match space.get_decl("main") {
                Some(s) => println!("{}", s),
                None => println_error_msg("Could not find entry function `main`"),
            }
        },

        _ => println_error_msg("Invalid amount of arguments passed into Rat"),
    };

    Ok(())
}
