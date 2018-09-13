//! Saha Tokenizer Library
//!
//! Takes in a source file or a source string and tokenizes it. First we parse
//! lexemes, after which we parse tokens from the lexemes. During tokenization
//! all filepath includes (`use` statements) are inlined and aliased properly
//! to generate a flat token stream for the Saha parser to operate on.

extern crate saha_lib;
extern crate noisy_float;

mod imports;
mod lexer;
mod tokenizer;
pub mod token;

use std::{
    collections::HashMap,
    path::PathBuf
};

use saha_lib::errors::ParseError;
use ::{
    lexer::lexemize_source_file,
    tokenizer::tokenize_lexemes,
    token::Token
};

pub fn tokenize(file: &PathBuf) -> Result<Vec<Token>, ParseError> {
    let mut lexemes = lexemize_source_file(file)?;
    let tokens: Vec<Token> = tokenize_lexemes(lexemes, file)?;

    return Ok(tokens);
}
