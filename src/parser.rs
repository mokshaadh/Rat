use std::{rc::Rc, fmt::{self}};

use crate::lexer::{Token, Error, TokenType};

#[derive(Clone)]
pub enum Ast {
    Identifier(usize, String),
    Number(usize, String),
    Lambda((usize, usize), Vec<String>, Rc<Ast>),
    FunctionApplication((usize, usize), Rc<Ast>, Vec<Rc<Ast>>),
    Let((usize, usize), String, Vec<String>, Rc<Ast>, Rc<Ast>),

    FunctionBinding((usize, usize), String, Vec<String>, Rc<Ast>),
    VariableBinding((usize, usize), String, Rc<Ast>),
}

impl fmt::Display for Ast {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ast::Identifier(_, name) => write!(f, "{}", name),
            Ast::Number(_, num) => write!(f, "{}", num),
            Ast::Lambda(_, parameters, body) => {
                write!(f, "λ{}. {}", parameters.join(" "), body)
            },
            Ast::FunctionApplication(_, func, args) => {
                let mut args_str = String::new();
                for (i, arg) in args.iter().enumerate() {
                    args_str.push_str(&format!("{}", arg));
                    if i != args.len() - 1 {args_str.push_str(" ")}
                }
                write!(f, "CALL {} <{}>", func, args_str)
            }
            Ast::Let(_, name, parameters, var_val, body) =>
                write!(f, "LET {} {} = {} IN {}", name, parameters.join(" "), var_val, body),

            Ast::FunctionBinding(_, name, parameters, body) => {
                write!(f, "FUNC {} {} = {}", name, parameters.join(" "), body)
            }
            Ast::VariableBinding(_, name, body) => {
                write!(f, "VAR {} = {}", name, body)
            }
        }
    }
}

#[derive(Clone)]
pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, index: 0}
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
                let start_idx = self.index;
                self.advance();
                Ok(Ast::Identifier(start_idx, name).into())
            }

            TokenType::Number(num) => {
                let start_idx = self.index;
                self.advance();
                Ok(Ast::Number(start_idx, num).into())
            }

            TokenType::LParen => {
                self.advance();
                let retr = self.parse_expression()?;
                if self.curr_tok().is(TokenType::RParen) {
                    self.advance();
                    Ok(retr)
                }
                else {
                    Err(Error::from_token("Expected closing `)`", self.curr_tok()))
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
                        Err(Error::from_token("Expected `->` or `.` in lambda expression", self.curr_tok()))
                    }
                }
                else {
                    Err(Error::from_token("Expected variable name(s) in lambda expression", self.prev_tok(1)))
                }
            }

            TokenType::Let => {
                let start_idx = self.index;
                self.advance();

                if let TokenType::Identifier(name) = self.curr_tok().clone().tok_type {
                    self.advance();

                    let mut parameters = Vec::new();

                    while let TokenType::Identifier(param) = self.curr_tok().clone().tok_type {
                        parameters.push(param);
                        self.advance();
                    }

                    if self.curr_tok().is(TokenType::Operator("=".to_string())) {
                        self.advance();

                        let var_val = self.parse_expression()?;

                        if self.curr_tok().is(TokenType::In) {
                            self.advance();
                            let body = self.parse_expression()?;

                            Ok(Ast::Let((start_idx, self.index - 1), name, parameters, var_val, body).into())
                        }
                        else {
                            Err(Error::from_token("Expected `in` in let expression", self.prev_tok(1)))
                        }
                    }
                    else {
                        Err(Error::from_token("Expected `=` in let expression", self.curr_tok()))
                    }
                }
                else {
                    Err(Error::from_token("Expected name in let expression", self.curr_tok()))
                }
            }

            TokenType::EoF => Err(Error::from_token("Unexpected end of file", self.curr_tok())),
            _ => Err(Error::from_token("Unexpected token", self.curr_tok()))
        }
    }

    fn parse_application(&mut self) -> Result<Rc<Ast>, Error> {
        let start_idx = self.index;

        let f = self.parse_atom()?;

        let restore = self.index;

        if let Ok(arg) = self.parse_atom() {
            let mut args = vec![arg];
            loop {
                let restore = self.index;
                if let Ok(arg) = self.parse_atom() {
                    args.push(arg);
                }
                else {
                    self.index = restore;
                    break;
                }
            }
            Ok(Ast::FunctionApplication((start_idx, self.index - 1), f, args).into())
        }
        else {
            self.index = restore;
            Ok(f)
        }
    }

    fn parse_expression(&mut self) -> Result<Rc<Ast>, Error> {
        let retr = self.parse_application();
        retr
    }

    fn parse_binding(&mut self) -> Result<Rc<Ast>, Error> {
        let start_idx = self.index;
        self.advance();

        if let TokenType::Identifier(name) = self.curr_tok().clone().tok_type {
            self.advance();
            // function
            if let TokenType::Identifier(param) = self.curr_tok().clone().tok_type {
                let mut parameters = vec![param];
                self.advance();

                while let TokenType::Identifier(param) = self.curr_tok().clone().tok_type {
                    parameters.push(param);
                    self.advance();
                }

                if self.curr_tok().is(TokenType::Operator("=".to_string())) {
                    self.advance();
                    let body = self.parse_expression()?;
                    Ok(Ast::FunctionBinding((start_idx, self.index - 1), name, parameters, body).into())
                }
                else {
                    Err(Error::from_token("Expected `=` in binding", self.curr_tok()))
                }
            }
            // variable
            else {
                if self.curr_tok().is(TokenType::Operator("=".to_string())) {
                    self.advance();
                    let body = self.parse_expression()?;
                    Ok(Ast::VariableBinding((start_idx, self.index - 1), name, body).into())
                }
                else {
                    Err(Error::from_token("Expected `=` in binding", self.curr_tok()))
                }
            }
        }
        else {
            Err(Error::from_token("Expected name in binding", self.curr_tok()))
        }
    }

    pub fn parse_line_repl(&mut self) -> Result<Rc<Ast>, Error> {
        let restore = self.index;

        if let TokenType::Let = self.curr_tok().tok_type {
            match self.parse_expression() {
                Ok(b) => Ok(b),
                Err(_) => { self.index = restore; self.parse_binding() }
            }
        }
        else {
            self.parse_expression()
        }
    }

    fn parse_declaration(&mut self) -> Result<Rc<Ast>, Error> {
        match self.curr_tok().tok_type {
            TokenType::Let => self.parse_binding(),
            _ => Err(Error::from_token("Expected declaration at top level of file", self.curr_tok()))
        }
    }

    pub fn parse_file(&mut self) -> Result<Vec<Rc<Ast>>, Error> {
        let mut retr = Vec::new();
        while !self.curr_tok().is(TokenType::EoF) {
            let decl = self.parse_declaration()?;
            retr.push(decl);
        }
        Ok(retr)
    }
}