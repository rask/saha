//! Tokenizer
//!
//! The tokenizer takes in lexemes and turns them into machine readable tokens,
//! which are used to generate root-level declarations (functions, classes,
//! etc.) and function ASTs.

use std::path::PathBuf;

use saha_lib::{
    errors::ParseError,
    source::FilePosition
};

use ::{
    lexer::{lexemize_source_file, Lexeme},
    token::Token
};

/// Result type for tokenization.
type TokenizationResult = Result<Vec<Token>, ParseError>;

/// Tokenizer
///
/// Manages the tokenization of source files.
pub struct Tokenizer {
    lexemes: Vec<Lexeme>
}

impl Tokenizer {
    /// Get a new tokenizer for a source file.
    pub fn new(lexemes: Vec<Lexeme>) -> Tokenizer {
        return Tokenizer {
            lexemes: lexemes
        };
    }

    /// When the self instance is ready, we can tokenize.
    pub fn tokenize(&mut self) -> TokenizationResult {
        return Ok(Vec::new());
    }
}

/// Tokenize a Saha source file that has been lexemized.
pub fn tokenize_lexemes(lexemes: Vec<Lexeme>) -> TokenizationResult {
    let mut tokenizer = Tokenizer::new(lexemes);

    return tokenizer.tokenize();
}

#[cfg(test)]
mod tests {
    use super::*;

    fn testfilepos() -> FilePosition {
        return FilePosition {
            path: PathBuf::from("/unknown"),
            line: 0,
            column: 0
        };
    }

    #[test]
    fn test_tokenizer_tokenizes_basic_sources() {
        let lexemes = vec![
            Lexeme::Word(testfilepos(), "use".to_string()),
            Lexeme::Whitespace(testfilepos(), " ".to_string()),
            Lexeme::Word(testfilepos(), "foo.bar.baz".to_string()),
            Lexeme::Whitespace(testfilepos(), " ".to_string()),
            Lexeme::Word(testfilepos(), "as".to_string()),
            Lexeme::Whitespace(testfilepos(), " ".to_string()),
            Lexeme::Word(testfilepos(), "FoobarBaz".to_string()),
            Lexeme::Symbol(testfilepos(), ";".to_string()),
        ];

        let mut tokenizer = Tokenizer::new(lexemes);

        let tokens = tokenizer.tokenize();

        assert!(tokens.unwrap().pop().unwrap() == Token::KwUse(testfilepos()));
    }
}