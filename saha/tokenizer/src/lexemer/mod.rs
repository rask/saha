//! Saha Lexemer
//! 
//! Parses a source file or string into primitive lexemes that are used to form
//! tokens.

use std::{
    fs::read_to_string,
    path::PathBuf
};

use saha_lib::{
    errors::{Error, ParseError},
    source::FilePosition
};

/// Lexeme
/// 
/// A lexeme is a token source primitive. A lexeme can be a single symbol, a
/// word, a string value, or maybe whitespace.
#[derive(Debug)]
pub enum Lexeme {
    /// If we encounter unparseable characters we create an `Unknown` lexeme from
    /// them.
    Unknown(FilePosition),

    /// Symbols, aka anything that is not whitespace or alphanumeric.
    Symbol(FilePosition, String),

    /// Continuous passages of `a-zA-Z0-9_.` characters.
    Word(FilePosition, String),

    /// Anything wrapped in `"` characters.
    String(FilePosition, String),

    /// Anything preceded by two slashes (`//`).
    Comment(FilePosition, String),

    /// Non-newline whitespace.
    Whitespace(FilePosition, String),

    /// Newlines as in `\n`.
    Newline(FilePosition),

    /// End of file marker for generic use.
    Eof(FilePosition),
}

impl Lexeme {
    pub fn get_file_position(&self) -> FilePosition {
        return match self {
            Lexeme::Unknown(f) => f.clone(),
            Lexeme::Symbol(f, ..) => f.clone(),
            Lexeme::Word(f, ..) => f.clone(),
            Lexeme::String(f, ..) => f.clone(),
            Lexeme::Comment(f, ..) => f.clone(),
            Lexeme::Whitespace(f, ..) => f.clone(),
            Lexeme::Newline(f, ..) => f.clone(),
            Lexeme::Eof(f) => f.clone(),
        };
    }
}

pub type LexemizationResult = Result<Vec<Lexeme>, ParseError>;

/// Lexemer takes in a source file and string, then parses Lexeme collections
/// from them.
struct Lexemer<'a> {
    source_file: &'a PathBuf,
    source_string: String,
}

impl<'a> Lexemer<'a> {
    /// Create a new Lexemer for a source file and string.
    pub fn new(source_file: &'a PathBuf, source_string: String) -> Lexemer {
        return Lexemer {
            source_file: source_file,
            source_string: source_string,
        };
    }

    /// Get the Lexemes for a source.
    pub fn get_lexemes(&mut self) -> LexemizationResult {
        return self.lexemize();
    }

    fn new_filepos(&self, line: isize, col: u32) -> FilePosition {
        return FilePosition {
            path: self.source_file.clone(),
            line: line,
            column: col
        };
    }

    /// Iterate over a string and collect Lexemes.
    fn lexemize(&mut self) -> LexemizationResult {
        let mut lexemes: Vec<Lexeme> = Vec::new();
        let mut current_character_type: &'static str;
        let mut char_buffer: Vec<char> = Vec::new();
        let mut comment_buffer: Vec<char> = Vec::new();
        let mut string_buffer: Vec<char> = Vec::new();

        let word_joiners = ['_', '.'];

        let mut current_line = 1;
        let mut current_column = 0;

        let mut comment_maybe_starts = false;
        let mut comment_can_start = true;
        let mut comment_is_open = false;
        let mut string_is_open = false;

        let mut string_begin_pos: Option<FilePosition> = None;

        for current_character in self.source_string.chars() {
            current_column += 1;

            if comment_maybe_starts == true {
                if current_character == '/' {
                    comment_is_open = true;
                    comment_maybe_starts = false;
                    comment_can_start = false;

                    continue;
                }
            }

            if comment_is_open == true {
                if current_character == '\n' {
                    comment_is_open = false;

                    // Hacky way to prepend comment markers
                    comment_buffer.insert(0, '/');
                    comment_buffer.insert(0, '/');

                    // Hacky way to skip adding comment markers as symbols
                    let comment_start = lexemes.pop();

                    let comment_start_column = match comment_start {
                        Some(pt) => pt.get_file_position().column.to_owned(),
                        None => 0,
                    };

                    lexemes.push(Lexeme::Comment(
                        self.new_filepos(current_line, comment_start_column),
                        comment_buffer.iter().collect()
                    ));

                    lexemes.push(Lexeme::Newline(self.new_filepos(current_line, current_column)));

                    comment_buffer.clear();

                    current_line += 1;
                    current_column = 0; // after newline
                    comment_can_start = true;
                } else {
                    comment_buffer.push(current_character);
                }

                continue;
            }

            if comment_can_start == true {
                if current_character == '/' {
                    comment_maybe_starts = true;
                }
            }

            if string_is_open == true {
                comment_can_start = false;
                comment_maybe_starts = false;
                let mut last_string_char: char;
                let mut buf = string_buffer.clone();

                last_string_char = *buf.last().unwrap_or(&'-');

                string_buffer.push(current_character);

                if current_character == '\n' {
                    if last_string_char != '\\' {
                        return Err(ParseError::new(
                            "Unterminated string encountered",
                            Some(self.new_filepos(current_line, 0)))
                        );
                    }

                    current_line += 1;
                    current_column = 0;
                }

                if current_character == '"' && last_string_char != '\\' {
                    string_is_open = false;
                    let cur_buffer_pos = string_begin_pos.clone().unwrap();

                    lexemes.push(Lexeme::String(
                        cur_buffer_pos,
                        string_buffer.iter().collect())
                    );

                    string_buffer.clear();
                }

                continue;
            }


            if current_character == '"' {
                // string delimiter
                string_is_open = true;

                string_begin_pos = Some(self.new_filepos(current_line, current_column));

                string_buffer.push(current_character);

                // This continue opens the string loop
                continue;
            } else if current_character == '\n' {
                current_character_type = "newline";
            } else if current_character.is_ascii_whitespace() {
                current_character_type = "whitespace";
            } else if current_character.is_ascii() == true
                && current_character.is_ascii_alphanumeric() == false
            {
                // symbols
                current_character_type = "symbol";
            } else {
                // everything else
                current_character_type = "word";
            }

            match current_character_type {
                "comment" | "string" => {},
                "word" => {
                    char_buffer.push(current_character);

                    comment_can_start = false;
                },
                "symbol" => {
                    if word_joiners.contains(&current_character) {
                        char_buffer.push(current_character);
                    } else {
                        if char_buffer.is_empty() {
                            comment_can_start = false;
                        } else {
                            // Word buffer contains something and we encountered non-joiner, flush
                            // the word buffer.
                            lexemes.push(Lexeme::Word(
                                self.new_filepos(current_line, char_buffer.len() as u32),
                                char_buffer.iter().collect()
                            ));
                        }

                        // Flush char word buffer after a symbol.
                        char_buffer.clear();

                        // Push the current symbol as well.
                        lexemes.push(Lexeme::Symbol(
                            self.new_filepos(current_line, current_column),
                            current_character.to_string()
                        ));
                    }

                    if current_character != '/' && comment_can_start == true {
                        comment_can_start = false;
                    }
                },
                "whitespace" => {
                    if char_buffer.is_empty() == false {
                        lexemes.push(Lexeme::Word(
                            self.new_filepos(current_line, char_buffer.len() as u32),
                            char_buffer.iter().collect()
                        ));

                        char_buffer.clear();
                    }

                    lexemes.push(Lexeme::Whitespace(
                        self.new_filepos(current_line, current_column),
                        current_character.to_string()
                    ));
                },
                "newline" => {
                    if char_buffer.is_empty() == false {
                        lexemes.push(Lexeme::Word(
                            self.new_filepos(current_line, char_buffer.len() as u32),
                            char_buffer.iter().collect()
                        ));

                        char_buffer.clear();
                    }

                    lexemes.push(Lexeme::Newline(self.new_filepos(current_line, current_column)));

                    current_line += 1;
                    current_column = 0;
                    comment_can_start = true;
                },
                _ => {
                    if char_buffer.is_empty() == false {
                        lexemes.push(Lexeme::Word(
                            self.new_filepos(current_line, char_buffer.len() as u32),
                            char_buffer.iter().collect()
                        ));

                        char_buffer.clear();
                    }

                    lexemes.push(Lexeme::Unknown(self.new_filepos(current_line, current_column)));

                    comment_can_start = false;
                }
            }
        }

        // In case we have a char buffer that is not empty by the end.
        if char_buffer.is_empty() == false {
            lexemes.push(Lexeme::Word(
                self.new_filepos(current_line, char_buffer.len() as u32),
                char_buffer.iter().collect()
            ));

            char_buffer.clear();
        }

        lexemes.push(Lexeme::Eof(self.new_filepos(current_line + 1, 0)));

        return Ok(lexemes);
    }
}

/// Lexemize a source file.
pub fn lexemize_source_file(file: &PathBuf) -> LexemizationResult {
    let null_pos = FilePosition {
        path: file.to_owned(),
        line: 0,
        column: 0
    };

    let source_string: String = match read_to_string(&file) {
        Ok(s) => s,
        Err(e) => return Err(ParseError::new(&format!("Could not read file: {:?}", e), Some(null_pos.clone()))),
    };

    if source_string.len() < 1 {
        return Err(ParseError::new(&format!("Could not read empty file: `{:?}`", file), Some(null_pos.clone())));
    }

    let mut lexemer = Lexemer::new(&file, source_string);

    return lexemer.get_lexemes();
}