//! Saha Parser
//!
//! The parser takes in tokens which have been parsed by the Saha tokenizer, and
//! generates a machine readable application structure from them.
//!
//! First we parse the application outline with class, function, behavior, and
//! constant definitions in place.
//!
//! Then we hop into each class method and function and parse the abstract
//! syntax tree for all of them.
//!
//! After parsing is done we have a ready to interpret application.

extern crate saha_lib;
extern crate saha_tokenizer;
extern crate noisy_float;

mod parse_table;
mod parser;

use saha_lib::errors::{ParseError};
use saha_tokenizer::token::Token;

use ::{
    parse_table::ParseTable,
    parser::{RootParser}
};

/// Parse a collection of tokens into a declaration table and ASTs.
pub fn parse_tokens(tokens: Vec<Token>) -> Result<(), ParseError> {
    let mut parse_table = ParseTable::new();
    let mut root_parser = RootParser::new(&tokens, &mut parse_table);

    root_parser.start_parse()?;

    return Ok(());
}