use crate::lexer::{Token, Error};

fn println_stuff_around(file: String, line: &str, poses: (((usize, usize), (usize, usize)), ((usize, usize), (usize, usize))), underline_char: &str, underline_color: term::color::Color, output: &mut Box<dyn term::Terminal<Output = std::io::Stdout> + Send>) {
    let og_pos = poses.0;
    let last_pos = poses.1;

    let line_len = og_pos.0.0.to_string().len();

    let line_len_space = " ".repeat(line_len);
    let start_space = " ".repeat(og_pos.0.1 + line_len);
    let underline = underline_char.repeat(last_pos.1.1 - og_pos.0.1);

    output.fg(term::color::BLUE).unwrap();
    output.attr(term::Attr::Bold).unwrap();

    write!(output, "{}-->", line_len_space).unwrap();

    output.reset().unwrap();
    output.flush().unwrap();

    writeln!(output, " {}:{}:{}", file, og_pos.0.0, og_pos.0.1).unwrap();

    output.fg(term::color::BLUE).unwrap();
    output.attr(term::Attr::Bold).unwrap();

    writeln!(output, "{} |", line_len_space).unwrap();

    write!(output, "{} | ", og_pos.0.0).unwrap();

    output.reset().unwrap();
    output.flush().unwrap();

    writeln!(output, "{}", line).unwrap();

    output.fg(term::color::BLUE).unwrap();
    output.attr(term::Attr::Bold).unwrap();

    write!(output, "{} |{}", line_len_space, start_space).unwrap();

    output.fg(underline_color).unwrap();
    writeln!(output, "{}", underline).unwrap();

    output.reset().unwrap();
    output.flush().unwrap();
}


pub fn println_token(file: String, tok: Token, underline_char: &str, underline_color: term::color::Color, output: &mut Box<dyn term::Terminal<Output = std::io::Stdout> + Send>) {
    println_stuff_around(file, &tok.line, (tok.pos, tok.pos), underline_char, underline_color, output)
}

pub fn println_error(file: String, err: Error, output: &mut Box<dyn term::Terminal<Output = std::io::Stdout> + Send>) {
    output.fg(term::color::RED).unwrap();
    output.attr(term::Attr::Bold).unwrap();

    write!(output, "Error").unwrap();

    output.reset().unwrap();
    output.flush().unwrap();
    output.attr(term::Attr::Bold).unwrap();

    writeln!(output, ": {}", err.msg).unwrap();

    output.reset().unwrap();
    output.flush().unwrap();

    println_stuff_around(file, &err.line, (err.pos, err.pos), "^", term::color::RED, output)
}

pub fn println_errors(file: String, err: Error, poses: (((usize, usize), (usize, usize)), ((usize, usize), (usize, usize))), output: &mut Box<dyn term::Terminal<Output = std::io::Stdout> + Send>) {
    output.fg(term::color::RED).unwrap();
    output.attr(term::Attr::Bold).unwrap();

    write!(output, "Error").unwrap();

    output.reset().unwrap();
    output.flush().unwrap();
    output.attr(term::Attr::Bold).unwrap();

    writeln!(output, ": {}", err.msg).unwrap();

    output.reset().unwrap();
    output.flush().unwrap();

    println_stuff_around(file, &err.line, poses, "^", term::color::RED, output)
}