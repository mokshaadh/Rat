use crate::{logging::{println_errors, println_error}, lexer::{Error, Lexer}, data::{Namespace, Environment, Substitutable}, parser::Parser};

pub fn read_eval(space: &mut Namespace, file: &str, line: &str) -> Result<(), Error> {
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
            println_errors(file, err, (err_pos1, err_pos2));
            return Ok(());
        }
    };

    let mut env = Environment::new();

    match space.alg_w(&mut val, &mut env) {
        Ok((sub, ty)) => {
            println!("{}", ty.apply(&sub));
            // println!("{}", ty.apply(&sub).simplify(&mut HashMap::new()));
        }
        Err(e) => {
            let err = Error::from_token(&e.0, &tokens[e.1.0]);
            let err_pos1 = tokens[e.1.0].pos;
            let err_pos2 = tokens[e.1.1].pos;
            println_errors(file, err, (err_pos1, err_pos2));
        }
    }

    println!("\n{}", val);


    Ok(())
}

pub fn repl() -> rustyline::Result<()> {
    let mut rl = rustyline::DefaultEditor::new()?;

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

                let read_eval_res = read_eval(&mut space, "<interactive>", &line);

                match read_eval_res {
                    Ok(_) => {}
                    Err(e) => { println_error("<interactive>", e); }
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