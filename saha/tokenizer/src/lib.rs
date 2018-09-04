//! Saha Tokenizer Library
//! 
//! Takes in a source file or a source string and tokenizes it. First we parse
//! lexemes, after which we parse tokens from the lexemes. During tokenization
//! all filepath includes (`use` statements) are inlined and aliased properly
//! to generate a flat token stream for the Saha parser to operate on.

extern crate saha_lib;
extern crate noisy_float;

mod lexemer;
pub mod token;

use std::path::PathBuf;
use saha_lib::errors::{Error, ParseError};
use ::{
    lexemer::lexemize_source_file,
    token::Token
};

pub fn tokenize_source_file(file: &PathBuf) -> Result<Vec<Token>, ParseError> {
    let lexemes = lexemize_source_file(&file);
    let tokens: Vec<Token> = Vec::new();

    return Ok(tokens);
}