//! Root parser
//!
//! Parses declarations at source code root level: functions, classes,
//! behaviors, and constants.

use noisy_float::prelude::*;

use std::{
    iter::{Iterator, Peekable},
    slice::Iter,
    mem::{discriminant, Discriminant},
};

use saha_lib::{
    errors::{Error, ParseError},
    source::FilePosition,
    types::SahaType
};

use saha_tokenizer::token::Token;

use ::{
    parse_table::ParseTable,
    parser::{TokenType, PR, ParsesTokens}
};

/// RootParser, which parses root level declarations from tokens. Means we use
/// this to parse constants, classes, behaviors, and functions, while not
/// touching the tokens inside function/method bodies.
pub struct RootParser<'a> {
    ctok: Option<&'a Token>,
    ptok: Option<&'a Token>,
    ntok: Option<&'a Token>,
    parse_table: &'a mut ParseTable,
    tokens: Peekable<Iter<'a, Token>>
}

impl<'a> ParsesTokens for RootParser<'a> {
    fn consume_next(&mut self, next_variants: Vec<&str>) -> PR<()> {
        let next_discriminants: Vec<TokenType> = next_variants.clone().iter().map(|a| -> TokenType {
            self.get_dummy_token_type(a)
        }).collect();

        self.ptok = self.ctok.clone();
        self.ctok = self.tokens.next();

        if self.ctok.is_none() {
            if self.ptok.is_some() {
                return Err(ParseError::new(
                    &format!("Unexpected end of token stream after `{}` token", self.ptok.unwrap()),
                    Some(self.ptok.unwrap().get_file_position())
                ));
            }

            return Err(ParseError::new("Unexpected end of token stream", Some(FilePosition::unknown())));
        }

        if !next_discriminants.contains(&discriminant(&self.ctok.unwrap().clone())) {
            let unexp = self.ctok.unwrap().clone();
            return self.unexpected(&unexp, next_variants);
        }

        let next = {
            self.tokens.peek()
        };

        if next.is_none() {
            self.ntok = None;
        } else {
            self.ntok = Some(next.unwrap().clone());
        }

        return Ok(());
    }

    fn consume_any(&mut self) -> PR<()> {
        self.ptok = self.ctok.clone();
        self.ctok = self.tokens.next();

        if self.ctok.is_none() {
            if self.ptok.is_some() {
                return Err(ParseError::new(
                    &format!("Unexpected end of token stream after `{}` token", self.ptok.unwrap()),
                    Some(self.ptok.unwrap().get_file_position())
                ));
            }

            return Err(ParseError::new("Unexpected end of token stream", Some(FilePosition::unknown())));
        }

        let next = self.tokens.peek();

        if next.is_none() {
            self.ntok = None;
        } else {
            self.ntok = Some(next.unwrap().clone());
        }

        return Ok(());
    }
}

impl<'a> RootParser<'a> {
    pub fn new(tokens: &'a Vec<Token>, parse_table: &'a mut ParseTable) -> RootParser<'a> {
        return RootParser {
            ctok: None,
            ptok: None,
            ntok: None,
            parse_table: parse_table,
            tokens: tokens.iter().peekable()
        };
    }

    /// Parse tokens.
    pub fn start_parse(&mut self) -> PR<()> {
        return self.parse_root();
    }

    /// Parse a root level declaration.
    fn parse_root(&mut self) -> PR<()> {
        self.consume_next(vec!["class", "behavior", "function", "const", "import", "eof"])?;

        match self.ctok.unwrap() {
            Token::KwFunction(..) => self.parse_function_declaration(),
            Token::KwClass(..) => self.parse_class_declaration(),
            Token::KwMethod(..) => self.parse_behavior_declaration(),
            Token::KwConstant(..) => self.parse_constant_declaration(),
            Token::Import(..) => Ok(()), // TODO validate how this should really be handled
            Token::Eof(..) => {
                if self.ntok.is_none() {
                    // final EOF, we can stop parsing now
                    return Ok(());
                }

                // parse next file root
                self.parse_root()
            },
            _ => unreachable!()
        }
    }

    /// Parse root level function declaration.
    fn parse_function_declaration(&mut self) -> PR<()> {
        self.consume_next(vec!["name"])?;

        let (fn_name_position, fn_alias, fn_source_name) = match self.ctok.unwrap() {
            Token::Name(f, a, n) => (f, a, n),
            _ => unreachable!()
        };

        self.consume_next(vec!["("])?;

        let mut fn_parameter_definitions: Vec<()> = Vec::new();

        match self.ntok.unwrap() {
            Token::ParensClose(..) => (),
            _ => self.parse_function_parameter_definitions(&mut fn_parameter_definitions)?
        };

        self.consume_next(vec![")"])?;

        let return_type: SahaType = self.parse_return_type()?;

        // If there was a return type we need to consume the body open curly here, otherwise it has
        // been consumed already
        match return_type {
            SahaType::Void => (),
            _ => self.consume_next(vec!["{"])?
        };

        let fn_body_tokens = self.parse_curly_block()?;

        // last curly open token was parsed in the fn call above

        return self.parse_root();
    }

    /// Parse function declaration parameter definitions.
    fn parse_function_parameter_definitions(&mut self, mut definitions: &mut Vec<()>) -> PR<()> {
        unimplemented!()
    }

    // Parse a function return type.
    fn parse_return_type(&mut self) -> PR<SahaType> {
        self.consume_next(vec!["name", "typestr", "typebool", "typeint", "typefloat", "{"])?;

        match self.ctok.unwrap() {
            Token::Name(_, alias, _) => Ok(SahaType::Name(alias.to_owned())),
            Token::TypeBoolean(..) => Ok(SahaType::Bool),
            Token::TypeString(..) => Ok(SahaType::Str),
            Token::TypeInteger(..) => Ok(SahaType::Int),
            Token::TypeFloat(..) => Ok(SahaType::Float),
            Token::CurlyOpen(..) => Ok(SahaType::Void),
            _ => unreachable!()
        }
    }

    /// Parse a curly brace delimited body, a function body for instance. Just
    /// read tokens into a new vec.
    fn parse_curly_block(&mut self) -> PR<Vec<Token>> {
        let mut block_tokens: Vec<Token> = Vec::new();
        let mut current_block_depth = 0;

        'bodyloop: loop {
            self.consume_any()?;

            let current = self.ctok.to_owned().unwrap();

            match current {
                Token::CurlyClose(..) => {
                    if current_block_depth == 0 {
                        // encountered the body closing curly brace
                        break 'bodyloop;
                    }

                    // internal curly, dedent
                    current_block_depth -= 1;
                },
                Token::CurlyOpen(..) => {
                    // indent block
                    current_block_depth += 1;
                },
                Token::Eof(..) => return Err(self.unexpected(current, vec!["any"]).err().unwrap()),
                _ => ()
            };

            block_tokens.push(current.to_owned());
        }

        // append a endofbody token

        block_tokens.push(Token::Eob);

        return Ok(block_tokens);
    }

    fn parse_class_declaration(&mut self) -> PR<()> {
        unimplemented!()
    }

    fn parse_behavior_declaration(&mut self) -> PR<()> {
        unimplemented!()
    }

    fn parse_constant_declaration(&mut self) -> PR<()> {
        unimplemented!()
    }
}
