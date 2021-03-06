//! Saha Tokenizer Library
//!
//! Takes in a source file or a source string and tokenizes it. First we parse
//! lexemes, after which we parse tokens from the lexemes. During tokenization
//! all filepath includes (`use` statements) are inlined and aliased properly
//! to generate a flat token stream for the Saha parser to operate on.

#![allow(clippy::needless_return, clippy::redundant_field_names)]

mod lexer;
mod tokenizer;

use std::{
    fs::File,
    io::Write,
    collections::HashMap,
    path::PathBuf
};

use tempfile;

use saha_lib::{
    errors::{Error, ParseError},
    source::{
        import::Import,
        token::{Token, ContainsImports}
    }
};

use crate::{
    tokenizer::tokenize_lexemes,
    lexer::{lexemize_source_file, lexemize_source_code, Lexeme}
};

/// Tokenize a Saha source file with imports.
pub fn tokenize(parsed_lexemes: Vec<Lexeme>, file: &PathBuf) -> Result<Vec<Token>, ParseError> {
    let mut tokenized_files: HashMap<PathBuf, Vec<Token>> = HashMap::new();
    let mut already_tokenized_paths: Vec<PathBuf> = Vec::new();
    let mut path_to_tokenize: PathBuf;

    let mut lexemes = parsed_lexemes;
    let mut tokens: Vec<Token> = tokenize_lexemes(lexemes, file, String::from("pkg"))?;

    already_tokenized_paths.push(file.to_owned());
    tokenized_files.insert(file.to_owned(), tokens.clone());

    // Loop while we have tokens and the tokens contain any filebased imports
    // to tokenize as well.
    'importloop: loop {
        // we pass in the files we have already tokenized to prevent infinite
        // looping
        let (contains, exists, module) = tokens.contains_file_imports(&already_tokenized_paths);

        if exists.is_some() {
            let existing = exists.unwrap();

            // we push existing paths again to the vector to make it appear
            // earlier in the parsing chain, to prevent missing definitions from
            // appearing in case the same file is required in multiple parts of
            // the codebase
            already_tokenized_paths.push(existing.to_owned());
        }

        if contains.is_none() {
            break 'importloop;
        }

        path_to_tokenize = contains.unwrap();

        if module.is_none() {
            return Err(ParseError::new(
                &format!("Invalid module pathname received for imported file `{:?}`", path_to_tokenize),
                None
            ));
        }

        lexemes = lexemize_source_file(&path_to_tokenize)?;
        let subtokens = tokenize_lexemes(lexemes, file, module.unwrap())?;

        already_tokenized_paths.push(path_to_tokenize.to_owned());
        tokenized_files.insert(path_to_tokenize, subtokens.clone());

        tokens = subtokens;
    }

    let mut flattened_paths: Vec<PathBuf> = Vec::new();
    let mut flattened_tokens: Vec<Token> = Vec::new();

    for p in already_tokenized_paths.iter().rev() {
        if flattened_paths.contains(p) {
            continue;
        }

        let mut tokens_to_append = tokenized_files[p].clone();

        // remove the now useless file import tokens
        tokens_to_append = tokens_to_append.into_iter().filter(|tok| {
            match tok {
                Token::Import(_, import) => {
                    match import {
                        Import::Pkg(..) => false,
                        _ => true
                    }
                },
                _ => true
            }
        }).collect();

        flattened_tokens.append(&mut tokens_to_append);
        flattened_paths.push(p.to_owned());
    }

    return Ok(flattened_tokens);
}

/// Tokenize a source file.
pub fn tokenize_file(file: &PathBuf) -> Result<Vec<Token>, ParseError> {
    let lexemes = lexemize_source_file(file)?;

    return tokenize(lexemes, file);
}

/// Allows tokenizing a raw source code string, by creating a tempfile for it.
///
/// This function is a helper for usecases where someone wants to just pipe in
/// short scripts through the process STDIN.
pub fn tokenize_raw_source_code(source: String) -> Result<Vec<Token>, ParseError> {
    let imaginary_path = PathBuf::from("saha://stdin");
    let lexemes = lexemize_source_code(source, &imaginary_path)?;

    return tokenize(lexemes, &imaginary_path);
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::env::current_dir;
    use saha_lib::source::files::FilePosition;

    fn get_test_sample_file(sample_path: &str) -> PathBuf {
        let mut path = current_dir().unwrap(); // TODO does this work
        path.push("tests/samples");
        path.push(sample_path);

        return path;
    }

    #[test]
    fn test_tokens_are_flattened_when_tokenizing_multiple_files() {
        let mainfile = get_test_sample_file("src/flatten_this.saha");
        let usefile = get_test_sample_file("src/flatten_from.saha");
        let usefile2 = get_test_sample_file("src/flatten_once_more.saha");

        fn fpos(path: &PathBuf, line: usize, col: i32) -> FilePosition {
            return FilePosition {
                path: path.to_owned(),
                line: line,
                column: col
            };
        }

        let expected: Vec<Token> = vec![
            // imported file qwerty
            Token::KwFunction(fpos(&usefile2, 1, 1)),
            Token::Name(fpos(&usefile2, 1, 10), "pkg.flatten_once_more.qwerty".to_string(), "qwerty".to_string()),
            Token::ParensOpen(fpos(&usefile2, 1, 16)),
            Token::ParensClose(fpos(&usefile2, 1, 17)),
            Token::CurlyOpen(fpos(&usefile2, 1, 19)),
            Token::CurlyClose(fpos(&usefile2, 1, 20)),
            Token::Eof(fpos(&usefile2, 3, 0)),

            // imported file foobar
            Token::KwFunction(fpos(&usefile, 3, 1)),
            Token::Name(fpos(&usefile, 3, 10), "pkg.flatten_from.foobar".to_string(), "foobar".to_string()),
            Token::ParensOpen(fpos(&usefile, 3, 16)),
            Token::ParensClose(fpos(&usefile, 3, 17)),
            Token::CurlyOpen(fpos(&usefile, 3, 19)),
            Token::CurlyClose(fpos(&usefile, 3, 20)),
            Token::Eof(fpos(&usefile, 5, 0)),

            // main file
            Token::KwFunction(fpos(&mainfile, 4, 1)),
            Token::Name(fpos(&mainfile, 4, 10), "pkg.main".to_string(), "main".to_string()),
            Token::ParensOpen(fpos(&mainfile, 4, 14)),
            Token::ParensClose(fpos(&mainfile, 4, 15)),
            Token::TypeInteger(fpos(&mainfile, 4, 17)),
            Token::CurlyOpen(fpos(&mainfile, 4, 21)),
            Token::KwReturn(fpos(&mainfile, 5, 5)),
            Token::IntegerValue(fpos(&mainfile, 5, 12), 0 as isize),
            Token::EndStatement(fpos(&mainfile, 5, 13)),
            Token::CurlyClose(fpos(&mainfile, 6, 1)),
            Token::Eof(fpos(&mainfile, 8, 0)),
        ];

        let tokenized = tokenize(&mainfile);

        assert_eq!(expected, tokenized.unwrap());
    }
}