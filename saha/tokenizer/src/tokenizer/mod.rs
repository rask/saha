//! Tokenizer
//!
//! The tokenizer takes in lexemes and turns them into machine readable tokens,
//! which are used to generate root-level declarations (functions, classes,
//! etc.) and function ASTs.

use noisy_float::prelude::*;

use std::path::PathBuf;

use saha_lib::{
    errors::{Error, ParseError},
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
        let mut tokens: Vec<Token> = Vec::new();

        let lexemes = self.lexemes.to_owned();
        let mut prev_lexeme: Option<Lexeme> = None;
        let mut prev_symbol: String = "a".to_string(); // default this to non-symbol so we don't break anything
        let mut prev_pos: Option<FilePosition> = None;

        for current_lexeme in lexemes {
            match current_lexeme {
                // we don't need these to make the code operable
                Lexeme::Comment(..) | Lexeme::Whitespace(..) | Lexeme::Newline(..) => {
                    prev_symbol = "a".to_string();

                    continue
                },
                Lexeme::Eof(ref f) => {
                    prev_symbol = "a".to_string();

                    tokens.push(Token::Eof(f.to_owned()));
                },
                Lexeme::String(ref f, ref string) => {
                    prev_symbol = "a".to_string();

                    let mut normalized_string = string.to_owned();

                    // remove leading and trailing quotes
                    normalized_string.remove(0);
                    normalized_string.pop();

                    tokens.push(Token::StringValue(f.to_owned(), normalized_string));
                },
                Lexeme::Number(ref f, ref number) => {
                    prev_symbol = "a".to_string();

                    if number.contains('.') {
                        // float number
                        let parsed = number.parse::<f64>();

                        match parsed {
                            Ok(flo) => {
                                let floatval = r64(flo);
                                tokens.push(Token::FloatValue(f.to_owned(), floatval));
                            },
                            Err(_) => {
                                return Err(ParseError::new("Invalid float value encountered", Some(f.to_owned())));
                            }
                        };
                    } else {
                        // integer number
                        let parsed = number.parse::<isize>();

                        match parsed {
                            Ok(intval) => {
                                tokens.push(Token::IntegerValue(f.to_owned(), intval));
                            },
                            Err(_) => {
                                return Err(ParseError::new("Invalid integer value encountered", Some(f.to_owned())));
                            }
                        };
                    }
                },
                Lexeme::Symbol(ref f, ref symbol) => {
                    let fp: FilePosition = f.to_owned();
                    let mut allow_prev_use = true;

                    let newtoken = match &symbol as &str {
                        // single symbols, e.g. not combined with other symbols anywhere
                        "'" => Token::SingleQuote(fp),
                        "(" => Token::ParensOpen(fp),
                        ")" => Token::ParensClose(fp),
                        "[" => Token::BraceOpen(fp),
                        "]" => Token::BraceClose(fp),
                        "{" => Token::CurlyOpen(fp),
                        "}" => Token::CurlyClose(fp),
                        "?" => Token::QuestionMark(fp),
                        "," => Token::Comma(fp),
                        ";" => Token::EndStatement(fp),
                        ":" => Token::Colon(fp),

                        // single ops
                        "-" => Token::OpSub(fp),
                        "+" => Token::OpAdd(fp),
                        "*" => Token::OpMul(fp),
                        "/" => Token::OpDiv(fp),
                        "!" => Token::Negation(fp),
                        "<" => Token::OpLt(fp),
                        ">" => Token::OpGt(fp),

                        // combinable symbols, we need to check back to see if we need to combine
                        "=" => {
                            if &prev_symbol == "=" {
                                allow_prev_use = false;
                                tokens.pop();
                                Token::OpEq(fp.shift_col(-1))
                            } else if &prev_symbol == "!" {
                                allow_prev_use = false;
                                tokens.pop();
                                Token::OpNeq(fp.shift_col(-1))
                            } else if &prev_symbol == "<" {
                                allow_prev_use = false;
                                tokens.pop();
                                Token::OpLte(fp.shift_col(-1))
                            } else if &prev_symbol == ">" {
                                allow_prev_use = false;
                                tokens.pop();
                                Token::OpGte(fp.shift_col(-1))
                            } else {
                                Token::Assign(fp)
                            }
                        },
                        "&" => {
                            if &prev_symbol == "&" {
                                allow_prev_use = false;
                                tokens.pop();
                                Token::OpAnd(fp.shift_col(-1))
                            } else {
                                Token::Ampersand(fp)
                            }
                        },
                        "|" => {
                            if &prev_symbol == "|" {
                                allow_prev_use = false;
                                tokens.pop();
                                Token::OpOr(fp.shift_col(-1))
                            } else {
                                Token::Pipe(fp)
                            }
                        },

                        // anything else
                        _ => {
                            return Err(ParseError::new(&format!("Unexpected symbol: `{}`", symbol), Some(fp)));
                        }
                    };

                    let prev_symb_ref = symbol.clone();

                    if allow_prev_use {
                        prev_symbol = prev_symb_ref;
                    } else {
                        prev_symbol = "a".to_string(); // init to something unusable when it comes to symbols
                    }

                    tokens.push(newtoken);
                },
                Lexeme::Word(ref f, ref word) => {
                    prev_symbol = "a".to_string();

                    let fp: FilePosition = f.to_owned();

                    let newtoken: Token = match &word as &str {
                        // boolean value
                        "true" => Token::BooleanValue(fp, true),
                        "false" => Token::BooleanValue(fp, false),

                        // type declarations
                        "str" => Token::TypeString(fp),
                        "bool" => Token::TypeBoolean(fp),
                        "int" => Token::TypeInteger(fp),
                        "float" => Token::TypeFloat(fp),
                        "list" => Token::TypeList(fp),
                        "dict" => Token::TypeDictionary(fp),

                        // keywords
                        "var" => Token::KwVar(fp),
                        "function" => Token::KwFunction(fp),
                        "method" => Token::KwMethod(fp),
                        "static" => Token::KwStatic(fp),
                        "public" => Token::KwPublic(fp),
                        "class" => Token::KwClass(fp),
                        "behavior" => Token::KwBehavior(fp),
                        "const" => Token::KwConstant(fp),
                        "prop" => Token::KwProperty(fp),
                        "for" => Token::KwFor(fp),
                        "in" => Token::KwIn(fp),
                        "loop" => Token::KwLoop(fp),
                        "if" => Token::KwIf(fp),
                        "elseif" => Token::KwElseif(fp),
                        "else" => Token::KwElse(fp),
                        "return" => Token::KwReturn(fp),
                        "try" => Token::KwTry(fp),
                        "catch" => Token::KwCatch(fp),
                        "raise" => Token::KwRaise(fp),
                        "new" => Token::KwNew(fp),
                        "use" => Token::KwUse(fp),
                        "as" => Token::KwAs(fp),
                        "implements" => Token::KwImplements(fp),
                        "continue" => Token::KwContinue(fp),
                        "break" => Token::KwBreak(fp),

                        // names, we store the word twice into this token, once to setup alias,
                        // second to store the source code representation
                        _ => Token::Name(f.to_owned(), word.to_owned(), word.to_owned())
                        //                             ^ alias          ^ source representation
                    };

                    tokens.push(newtoken);
                },
                _ => {
                    return Err(ParseError::new("Unknown lexeme encountered", prev_pos));
                }
            };

        }

        return Ok(tokens);
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
            Lexeme::Newline(testfilepos()),
            Lexeme::Word(testfilepos(), "foo".to_string()),
            Lexeme::Whitespace(testfilepos(), " ".to_string()),
            Lexeme::Symbol(testfilepos(), "&".to_string()),
            Lexeme::Symbol(testfilepos(), "&".to_string()),
            Lexeme::Whitespace(testfilepos(), " ".to_string()),
            Lexeme::Number(testfilepos(), "123".to_string()),
            Lexeme::Whitespace(testfilepos(), " ".to_string()),
            Lexeme::Symbol(testfilepos(), "*".to_string()),
            Lexeme::Whitespace(testfilepos(), " ".to_string()),
            Lexeme::String(testfilepos(), "\"asdf\"".to_string()),
            Lexeme::Symbol(testfilepos(), ";".to_string()),
        ];

        let expected = vec![
            Token::KwUse(testfilepos()),
            Token::Name(testfilepos(), "foo.bar.baz".to_string(), "foo.bar.baz".to_string()),
            Token::KwAs(testfilepos()),
            Token::Name(testfilepos(), "FoobarBaz".to_string(), "FoobarBaz".to_string()),
            Token::EndStatement(testfilepos()),
            Token::Name(testfilepos(), "foo".to_string(), "foo".to_string()),
            Token::OpAnd(testfilepos().shift_col(-1)),
            Token::IntegerValue(testfilepos(), 123 as isize),
            Token::OpMul(testfilepos()),
            Token::StringValue(testfilepos(), "asdf".to_string()),
            Token::EndStatement(testfilepos()),
        ];

        let mut tokenizer = Tokenizer::new(lexemes);

        let tokens = tokenizer.tokenize();
        let printtok = tokenizer.tokenize();

        assert!(tokens.unwrap() == expected);
    }
}