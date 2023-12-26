use crate::lexer::{Token, Error};


pub fn println_token(file: String, tok: Token, underline_char: &str, underline_color: term::color::Color, output: &mut Box<dyn term::Terminal<Output = std::io::Stdout> + Send>) {
    let line_len = tok.pos.0.0.to_string().len();

    let line_len_space = " ".repeat(line_len);
    let start_space = " ".repeat(tok.pos.0.1 + line_len);
    let underline = underline_char.repeat(tok.pos.1.1 - tok.pos.0.1);

    output.fg(term::color::BLUE).unwrap();
    output.attr(term::Attr::Bold).unwrap();

    write!(output, "{}-->", line_len_space).unwrap();

    output.reset().unwrap();
    output.flush().unwrap();

    writeln!(output, " {}:{}:{}", file, tok.pos.0.0, tok.pos.0.1).unwrap();

    output.fg(term::color::BLUE).unwrap();
    output.attr(term::Attr::Bold).unwrap();

    writeln!(output, "{} |", line_len_space).unwrap();

    write!(output, "{} | ", tok.pos.0.0).unwrap();

    output.reset().unwrap();
    output.flush().unwrap();

    writeln!(output, "{}", tok.line).unwrap();

    output.fg(term::color::BLUE).unwrap();
    output.attr(term::Attr::Bold).unwrap();

    write!(output, "{} |{}", line_len_space, start_space).unwrap();

    output.fg(underline_color).unwrap();
    writeln!(output, "{}", underline).unwrap();

    output.reset().unwrap();
    output.flush().unwrap();
}

pub fn println_error(file: String, err: Error, stderr: &mut Box<dyn term::Terminal<Output = std::io::Stderr> + Send>) {
    let line_len = err.pos.0.0.to_string().len();

    let line_len_space = " ".repeat(line_len);
    let start_space = " ".repeat(err.pos.0.1 + line_len);
    let underline = "^".repeat(err.pos.1.1 - err.pos.0.1);

    stderr.fg(term::color::RED).unwrap();
    stderr.attr(term::Attr::Bold).unwrap();

    write!(stderr, "Error").unwrap();

    stderr.reset().unwrap();
    stderr.flush().unwrap();
    stderr.attr(term::Attr::Bold).unwrap();

    writeln!(stderr, ": {}", err.msg).unwrap();

    stderr.reset().unwrap();
    stderr.flush().unwrap();

    stderr.fg(term::color::BLUE).unwrap();
    stderr.attr(term::Attr::Bold).unwrap();

    write!(stderr, "{}-->", line_len_space).unwrap();

    stderr.reset().unwrap();
    stderr.flush().unwrap();

    writeln!(stderr, " {}:{}:{}", file, err.pos.0.0, err.pos.0.1).unwrap();

    stderr.fg(term::color::BLUE).unwrap();
    stderr.attr(term::Attr::Bold).unwrap();

    writeln!(stderr, "{} |", line_len_space).unwrap();

    write!(stderr, "{} | ", err.pos.0.0).unwrap();

    stderr.reset().unwrap();
    stderr.flush().unwrap();

    writeln!(stderr, "{}", err.line).unwrap();

    stderr.fg(term::color::BLUE).unwrap();
    stderr.attr(term::Attr::Bold).unwrap();

    write!(stderr, "{} |{}", line_len_space, start_space).unwrap();

    stderr.fg(term::color::RED).unwrap();
    writeln!(stderr, "{}", underline).unwrap();

    stderr.reset().unwrap();
    stderr.flush().unwrap();
}