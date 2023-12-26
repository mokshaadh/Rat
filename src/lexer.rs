use std::fmt;

#[derive(Clone, PartialEq, Eq)]
pub enum TokenType {
    Lambda,
    Identifier(String),
    Dot,
    Operator(String),
    LParen, RParen,
    EoF,
}
impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenType::Lambda => write!(f, "λ"),
            TokenType::Identifier(id) => write!(f, "{}", id),
            TokenType::Dot => write!(f, "."),
            TokenType::Operator(op) => write!(f, "{}", op),
            TokenType::LParen => write!(f, "("),
            TokenType::RParen => write!(f, ")"),
            TokenType::EoF => write!(f, "EoF")
        }
    }
}

#[derive(Clone)]
pub struct Token {
    pub tok_type:   TokenType,
    pub line:       String,
    pub pos:        ((usize, usize), (usize, usize))
}
impl Token {
    pub fn new(tok_type: TokenType, line: String, pos: ((usize, usize), (usize, usize))) -> Self {
        Self { tok_type, line, pos }
    }
    pub fn is(&self, other_type: TokenType) -> bool {
        self.tok_type == other_type
    }
}

pub struct Error {
    pub msg:    String,
    pub line:   String,
    pub pos:    ((usize, usize), (usize, usize))
}
impl Error {
    pub fn new(msg: &str, line: String, pos: ((usize, usize), (usize, usize))) -> Self {
        Self { msg: msg.to_string(), line, pos }
    }
    pub fn from_token(msg: &str, other: &Token) -> Self {
        Self { msg: msg.to_string(), line: other.line.clone(), pos: other.pos }
    }
}

pub struct Lexer {
    last:   char,
    src:    String,

    line:   usize,
    col:    usize,
    next_is_nl: bool,

    curr_char_num:    usize,
}

impl Lexer{

    pub fn new(src: &str) -> Self {
        Self { last: '\0', src: src.to_string(), line: 0, col: 0, next_is_nl: false, curr_char_num: 0 }
    }

    fn get_pos(&self) -> (usize, usize) {
        (self.line, self.col)
    }

    fn get_line(&self) -> String {
        match self.src.lines().nth(self.line) {
            Some(l) => l.to_string(),
            None => "".to_string(),
        }
    }

    fn create_token(&self, tok_type: TokenType, start_pos: (usize, usize)) -> Token {
        Token::new(tok_type, self.get_line(), (start_pos, self.get_pos()))
    }
    fn create_error(&self, msg: &str, start_pos: (usize, usize)) -> Error {
        Error::new(msg, self.get_line(), (start_pos, self.get_pos()))
    }

    pub fn initialize(&mut self) {

        if self.next_is_nl {
            self.line += 1; self.col = 0; self.next_is_nl = false;
        }

        self.last =
            match self.src.chars().nth(0) {
                Some(c) =>
                    match c {
                        '\n' => { self.next_is_nl = true; c }
                        _ => { c }
                    }
                None => { '\0' }
            };
    }

    fn advance(&mut self) {
        self.curr_char_num += 1;
        let last_was_nl: bool;

        if self.next_is_nl {
            self.line += 1; self.col = 0; self.next_is_nl = false;
            last_was_nl = true;
        }
        else { last_was_nl = false; }

        self.last =
            match self.src.chars().nth(self.curr_char_num) {
                Some(c) =>
                    match c {
                        '\n' => { self.next_is_nl = true; c }
                        _ => { c }
                    }
                None => { '\0' }
            };

        if !last_was_nl {
            self.col += 1;
        }
    }

    fn lex_identifier(&mut self) -> Token {
        let start_pos = self.get_pos();

        let mut id = String::new();
        while self.last.is_alphanumeric() || self.last == '_' || self.last == '\'' {
            id.push(self.last);
            self.advance();
        }
        self.create_token(TokenType::Identifier(id), start_pos)
    }

    fn lex_token(&mut self) -> Result<Token, Error> {
        while self.last.is_whitespace() {
            self.advance();
        }

        let start_pos = self.get_pos();

        match self.last {
            // comments
            '#' => {
                while self.last != '\n' && self.last != '\0' { self.advance(); }
                self.lex_token()
            }

            '(' => { self.advance(); Ok(self.create_token(TokenType::LParen, start_pos)) }
            ')' => { self.advance(); Ok(self.create_token(TokenType::RParen, start_pos)) }

            // lambda expressions
            '\\' | 'λ' => { self.advance(); Ok(self.create_token(TokenType::Lambda, start_pos)) }

            '.' => { self.advance(); Ok(self.create_token(TokenType::Dot, start_pos)) }

            // operators
            '!' | '$' | '%' | '&' | '*' | '+' | '-' | '/' | '<' | '=' | '>' | '?' | '@' | '^' | '|' | '~' => {
                let mut op: String = self.last.to_string();
                self.advance();
                loop {
                    match self.last {
                        '!' | '$' | '%' | '&' | '*' | '+' | '-' | '/' | '<' | '=' | '>' | '?' | '@' | '^' | '|' | '~' => {
                            op.push(self.last); self.advance();
                        }
                        _ => break
                    }
                }
                Ok(self.create_token(TokenType::Operator(op), start_pos))
            }

            // identfiers
            c if c.is_alphabetic() => Ok(self.lex_identifier()),

            // EoF
            '\0' => { self.advance(); Ok(self.create_token(TokenType::EoF, start_pos)) }

            _ => {
                let ch = self.last;
                self.advance();
                Err(self.create_error(&format!("Unexpected character '{}'", ch), start_pos))
            }
        }
    }

    pub fn lex_tokens(&mut self) -> Result<Vec<Token>, Error> {
        let mut retr: Vec<Token> = Vec::new();
        loop {
            let temp_tok = self.lex_token()?;
            retr.push(temp_tok.clone());
            if temp_tok.tok_type == TokenType::EoF { break; }
        }
        Ok(retr)
    }

}