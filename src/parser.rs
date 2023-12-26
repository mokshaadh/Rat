use std::{rc::Rc, fmt::{self}};

use crate::lexer::{Token, Error, TokenType};

pub enum Ast {
    Identifier(usize, String),
    Lambda((usize, usize), Vec<String>, Rc<Ast>),
    FunctionApplication(Rc<Ast>, Vec<Rc<Ast>>),
}
impl fmt::Display for Ast {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ast::Identifier(_, name) => write!(f, "{}", name),
            Ast::Lambda(_, parameters, body) => {
                write!(f, "\\{} -> {}", parameters.join(" "), body)
            },
            Ast::FunctionApplication(func, args) => {
                let mut args_str = String::new();
                for (i, arg) in args.iter().enumerate() {
                    args_str.push_str(&format!("{}", arg));
                    if i != args.len() - 1 {args_str.push_str(" ")}
                }
                write!(f, "CALL {} <{}>", func, args_str)
            }
        }
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, index: 0 }
    }

    fn advance(&mut self) {
        self.index += 1;
    }

    fn prev_tok(&self, back: usize) -> &Token {
        &self.tokens[self.index - back]
    }
    fn curr_tok(&self) -> &Token {
        &self.tokens[self.index]
    }

    fn parse_atom(&mut self) -> Result<Rc<Ast>, Error> {
        match self.curr_tok().clone().tok_type {
            TokenType::Identifier(name) => {
                self.advance();
                Ok(Ast::Identifier(self.index, name).into())
            }

            TokenType::LParen => {
                self.advance();
                let retr = self.parse_expression()?;
                if self.curr_tok().is(TokenType::RParen) {
                    self.advance();
                    Ok(retr)
                }
                else {
                    Err(Error::from_token("Expected closing ')'", self.curr_tok()))
                }
            }

            TokenType::Lambda => {
                let start_idx = self.index;

                self.advance();
                let mut parameters = Vec::new();
                if let TokenType::Identifier(name) = self.curr_tok().clone().tok_type {
                    parameters.push(name);
                    self.advance();

                    while let TokenType::Identifier(name) = self.curr_tok().clone().tok_type {
                        parameters.push(name);
                        self.advance();
                    }

                    if self.curr_tok().is(TokenType::Dot)
                    || self.curr_tok().is(TokenType::Operator("->".to_string())) {
                        self.advance();
                        let body = self.parse_expression()?;
                        Ok(Ast::Lambda((start_idx, self.index), parameters, body).into())
                    }
                    else {
                        Err(Error::from_token("Expected '->' or '.' in lambda expression", self.curr_tok()))
                    }
                }
                else {
                    Err(Error::from_token("Expected variable name(s) in lambda expression", self.curr_tok()))
                }
            }
            TokenType::EoF => Err(Error::from_token("Unexpected end of file", self.curr_tok())),
            _ => Err(Error::from_token("Unexpected token", self.curr_tok()))
        }
    }

    pub fn parse_application(&mut self) -> Result<Rc<Ast>, Error> {
        let f = self.parse_atom()?;

        if let Ok(arg) = self.parse_atom() {
            let mut args = vec![arg];
            while let Ok(arg) = self.parse_atom() {
                args.push(arg);
            }
            Ok(Ast::FunctionApplication(f, args).into())
        }
        else {
            Ok(f)
        }
    }

    pub fn parse_expression(&mut self) -> Result<Rc<Ast>, Error> {
        self.parse_application()
    }
}