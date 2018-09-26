//! Tokenizer
//!
//! The tokenizer takes in lexemes and turns them into machine readable tokens,
//! which are used to generate root-level declarations (functions, classes,
//! etc.) and function ASTs.

use noisy_float::prelude::*;

use std::{
    collections::HashMap,
    path::PathBuf
};

use saha_lib::{
    errors::{Error, ParseError},
    source::FilePosition
};

use ::{
    imports::Import,
    lexer::Lexeme,
    token::Token
};

/// Result type for tokenization.
type TokenizationResult = Result<Vec<Token>, ParseError>;

/// Tokenizer
///
/// Manages the tokenization of source files.
pub struct Tokenizer<'a> {
    main_file: &'a PathBuf,
    lexemes: Vec<Lexeme>,
    module: String
}

impl<'a> Tokenizer<'a> {
    /// Get a new tokenizer for a source file.
    pub fn new(lexemes: Vec<Lexeme>, main_file: &PathBuf, module_name: String) -> Tokenizer {
        return Tokenizer {
            main_file: main_file,
            lexemes: lexemes,
            module: module_name
        };
    }

    /// Parse import name and path.
    ///
    /// For `project.` imports, we try to find an actual file to load. For other types we just
    /// return none as the parser will use internal symbol routing to find the imported members.
    ///
    /// Assuming we are importing a name called
    ///
    /// ```saha
    /// // /my/project/src/main.saha
    /// use project.my_module.hello_world;
    /// ```
    ///
    /// We will look for the `hello_world` function or other member in the following path:
    ///
    /// ```text
    /// /my/project/src/my_module.saha
    /// ```
    fn parse_import_path_names(&self, namepos: &FilePosition, name: String) -> Result<(String, Option<PathBuf>), ParseError> {
        if name.contains('.') == false {
            return Err(ParseError::new("Invalid `use` member, must be namespaced", Some(namepos.to_owned())));
        }

        let mut name_parts: Vec<&str> = name.split('.').collect();
        let member = name_parts.pop().unwrap();

        if name.starts_with("project.") {
            // we are looking at a filepath based import
            let mut project_root = self.main_file.clone();
            project_root.pop(); // pop the main file of the project

            name_parts.remove(0); // remove the starting `project` to make it inferred instead

            let import_member_path = name_parts.join("/");

            project_root.push(import_member_path);
            project_root.set_extension("saha");

            if project_root.exists() == false {
                return Err(ParseError::new(&format!("Invalid `use`, file `{:?}` for `{}` does not exist", project_root, name), Some(namepos.to_owned())));
            }

            return Ok((member.to_string(), Some(project_root)));
        }

        // std and ext imports do not use filesystem paths but internal routing instead
        return Ok((member.to_string(), None));
    }

    /// Get the import type for an import name source string.
    fn get_import_type(&self, import_source_string: &String) -> &str {
        if import_source_string.starts_with("project.") {
            return "project";
        } else if import_source_string.starts_with("std.") {
            return "std";
        } else if import_source_string.starts_with("ext.") {
            return "ext";
        } else {
            return "";
        }
    }

    /// Parse use keywords to imports and alias names to imports.
    fn parse_imports(&self, tokens: Vec<Token>, main_file: &PathBuf) -> TokenizationResult {
        let mut iterable = tokens.iter().peekable();
        let mut parsed: Vec<Token> = Vec::new();

        // name -> imported member
        let mut names_to_alias: HashMap<String, String> = HashMap::new();

        loop {
            let current_token_maybe = iterable.next();

            if current_token_maybe.is_none() {
                break;
            }

            let current_token = current_token_maybe.unwrap();

            match current_token {
                Token::KwUse(use_pos) => {
                    let use_name_maybe = iterable.next();

                    if use_name_maybe.is_none() {
                        return Err(ParseError::new("Expected name after `use` keyword", Some(use_pos.to_owned())));
                    }

                    let use_name = use_name_maybe.unwrap();

                    match use_name {
                        Token::Name(pos, _, source) => {
                            // `source` contains the whole import name
                            let import_type = self.get_import_type(&source);
                            let import_name: String; // the member we're importing
                            let mut import_alias: String; // alias with `as` possibly
                            let import_path: Option<PathBuf>; // file path we're importing from

                            match import_type {
                                "project" | "vendor" | "std" | "ext" => (), // "ok"
                                _ => return Err(ParseError::new("Invalid import type encountered, expected `project`, `std`, or `ext`", Some(pos.to_owned())))
                            };

                            let (import_name, import_path) = self.parse_import_path_names(&pos, source.to_string())?;

                            // we need to peek to make sure we have either a bare import or an
                            // aliased import, or perhaps a parse error
                            let mut peeked_token: Option<Token>;

                            // do the following inside a block to keep reference lifetimes in tick
                            {
                                peeked_token = match iterable.peek().clone() {
                                    Some(t) => Some(t.to_owned().to_owned()),
                                    None => None
                                };
                            }

                            let import_definition: Import = match peeked_token {
                                Some(t) => match t {
                                    Token::EndStatement(..) => {
                                        iterable.next();

                                        names_to_alias.insert(import_name.clone(), source.to_owned());

                                        match import_type {
                                            "project" => Import::Project(source.to_owned(), import_name, import_path.unwrap()),
                                            "std" => Import::Std(source.to_owned(), import_name),
                                            "ext" => Import::Ext(source.to_owned(), import_name),
                                            _ => unreachable!()
                                        }
                                    }
                                    Token::KwAs(..) => {
                                        // parse import name aliasing
                                        let askw = iterable.next().unwrap(); // skip the `as`

                                        let aliased_name = iterable.next();

                                        if aliased_name.is_none() {
                                            return Err(ParseError::new("Expected name after `as`", Some(askw.get_file_position())));
                                        }

                                        import_alias = match aliased_name.unwrap() {
                                            Token::Name(_, _, alias_source) => alias_source.to_owned(),
                                            _ => return Err(ParseError::new("Expected name after `as`", Some(askw.get_file_position())))
                                        };

                                        names_to_alias.insert(import_alias.clone(), source.to_owned());

                                        // consume statement end
                                        let end = iterable.next();

                                        if end.is_none() {
                                            return Err(ParseError::new("Expected `;` after use statement alias", Some(aliased_name.unwrap().get_file_position())));
                                        }

                                        match end.unwrap() {
                                            Token::EndStatement(..) => {},
                                            _ => return Err(ParseError::new("Expected `;` after use statement alias", Some(aliased_name.unwrap().get_file_position())))
                                        };

                                        match import_type {
                                            "project" => Import::Project(source.to_owned(), import_alias, import_path.unwrap()),
                                            "std" => Import::Std(source.to_owned(), import_alias),
                                            "ext" => Import::Ext(source.to_owned(), import_alias),
                                            _ => unreachable!()
                                        }
                                    },
                                    _ => return Err(ParseError::new("Expected `as` or `;`", Some(t.get_file_position())))
                                },
                                _ => {
                                    return Err(ParseError::new("Expected `as` or `;`", Some(pos.to_owned())));
                                }
                            };

                            parsed.push(Token::Import(use_pos.to_owned(), import_definition));
                        },
                        _ => {
                            return Err(ParseError::new("Expected name after `use` keyword", Some(use_pos.to_owned())));
                        }
                    }
                },
                _ => parsed.push(current_token.to_owned())
            };
        }

        let mut alias_previous: Option<Token> = None;

        parsed = parsed.iter().map(|tok| {
            let new: Token = match tok {
                Token::Name(pos, _, source) => {
                    let copypos = pos.to_owned();
                    let copysource = source.to_owned();

                    if names_to_alias.contains_key(source) {
                        // this alters names to match imported module names
                        let alias_to: String = names_to_alias.get(source).unwrap().to_string();

                        Token::Name(copypos, alias_to, copysource)
                    } else {
                        // see if we need to prepend module name to declarations, so they are
                        // namespaced in the end result of tokens
                        match alias_previous {
                            Some(ref p) => {
                                match p {
                                    Token::KwFunction(ref f) |
                                    Token::KwBehavior(ref f) |
                                    Token::KwClass(ref f)  => {
                                        let alias_to = format!("{}.{}", self.module, source);

                                        Token::Name(copypos, alias_to, copysource)
                                    },
                                    _ => tok.to_owned()
                                }
                            },
                            _ => tok.to_owned()
                        }
                    }
                },
                _ => tok.to_owned()
            };

            alias_previous = Some(new.clone());

            new
        }).collect();

        return Ok(parsed);
    }

    /// When the self instance is ready, we can tokenize.
    pub fn tokenize(&mut self) -> TokenizationResult {
        let mut tokens: Vec<Token> = Vec::new();

        let lexemes = self.lexemes.to_owned();
        let mut prev_pos: Option<FilePosition> = None;
        let mut prev_symbol: String = "a".to_string(); // default this to non-symbol so we don't break anything

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
                        "!" => Token::UnOpNot(fp),
                        "<" => Token::OpLt(fp),
                        ">" => {
                            if &prev_symbol == "|" {
                                allow_prev_use = false;
                                tokens.pop();
                                Token::OpPipe(fp.shift_col(-1))
                            } else {
                                Token::OpGt(fp)
                            }
                        },

                        // combinable symbols, we need to check back to see if we need to combine
                        "=" => {
                            if ["=", "!", "<", ">"].contains(&prev_symbol.as_str()) {
                                allow_prev_use = false;
                                tokens.pop();
                            }

                            match &prev_symbol as &str {
                                "=" => Token::OpEq(fp.shift_col(-1)),
                                "!" => Token::OpNeq(fp.shift_col(-1)),
                                "<" => Token::OpLte(fp.shift_col(-1)),
                                ">" => Token::OpGte(fp.shift_col(-1)),
                                _ => Token::Assign(fp),
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

                        // keywords
                        "var" => Token::KwVar(fp),
                        "function" => Token::KwFunction(fp),
                        "method" => Token::KwMethod(fp),
                        "static" => Token::KwStatic(fp),
                        "pub" => Token::KwPublic(fp),
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
                        "finally" => Token::KwFinally(fp),
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

            prev_pos = Some(current_lexeme.get_file_position());
        }

        let imports_parsed = self.parse_imports(tokens, self.main_file)?;

        return Ok(imports_parsed);
    }
}

/// Tokenize a Saha source file that has been lexemized.
pub fn tokenize_lexemes(lexemes: Vec<Lexeme>, main_file: &PathBuf, module_name: String) -> TokenizationResult {
    let mut tokenizer = Tokenizer::new(lexemes, main_file, module_name);

    return tokenizer.tokenize();
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::env::current_dir;

    fn testfilepos() -> FilePosition {
        return FilePosition::unknown();
    }

    fn testmainpos() -> FilePosition {
        return FilePosition {
            path: get_test_main_file(),
            line: 0,
            column: 0
        };
    }

    fn get_test_sample_file(sample_path: &str) -> PathBuf {
        let mut path = current_dir().unwrap(); // TODO does this work
        path.push("tests/samples");
        path.push(sample_path);

        return path;
    }

    fn get_test_main_file() -> PathBuf {
        return get_test_sample_file("src/main.saha");
    }

    #[test]
    fn test_tokenizer_tokenizes_basic_sources() {
        let testpath: PathBuf = get_test_main_file();

        let lexemes = vec![
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

        let first_import = Import::Project("project.bar.hello_world".to_string(), "hello_world".to_string(), PathBuf::from(""));
        let second_import = Import::Std("std.bar.baz".to_string(), "FoobarBaz".to_string());

        let expected = vec![
            Token::Name(testfilepos(), "foo".to_string(), "foo".to_string()),
            Token::OpAnd(testfilepos().shift_col(-1)),
            Token::IntegerValue(testfilepos(), 123 as isize),
            Token::OpMul(testfilepos()),
            Token::StringValue(testfilepos(), "asdf".to_string()),
            Token::EndStatement(testfilepos()),
        ];

        let mut tokenizer = Tokenizer::new(lexemes, &testpath, String::new());

        let tokens = tokenizer.tokenize();
        let printtok = tokenizer.tokenize();

        assert_eq!(tokens.unwrap(), expected);
    }

    #[test]
    fn test_it_tokenizes_imports_properly() {
        let testpath: PathBuf = get_test_main_file();

        let proj_lexemes = vec![
            Lexeme::Word(testfilepos(), "use".to_string()),
            Lexeme::Whitespace(testfilepos(), " ".to_string()),
            Lexeme::Word(testfilepos(), "project.mymod.HelloWorld".to_string()),
            Lexeme::Symbol(testfilepos(), ";".to_string()),
        ];

        let std_lexemes = vec![
            Lexeme::Word(testfilepos(), "use".to_string()),
            Lexeme::Whitespace(testfilepos(), " ".to_string()),
            Lexeme::Word(testfilepos(), "std.coremod.Foobar".to_string()),
            Lexeme::Symbol(testfilepos(), ";".to_string()),
        ];

        let proj_aliased_lexemes = vec![
            Lexeme::Word(testfilepos(), "use".to_string()),
            Lexeme::Whitespace(testfilepos(), " ".to_string()),
            Lexeme::Word(testfilepos(), "project.mymod.HelloWorld".to_string()),
            Lexeme::Whitespace(testfilepos(), " ".to_string()),
            Lexeme::Word(testfilepos(), "as".to_string()),
            Lexeme::Whitespace(testfilepos(), " ".to_string()),
            Lexeme::Word(testfilepos(), "HiyaWorld".to_string()),
            Lexeme::Symbol(testfilepos(), ";".to_string()),
        ];

        let mut proj_tokenizer = Tokenizer::new(proj_lexemes, &testpath, String::from("project"));
        let mut std_tokenizer = Tokenizer::new(std_lexemes, &testpath, String::from("project"));
        let mut proj_alias_tokenizer = Tokenizer::new(proj_aliased_lexemes, &testpath, String::from("project"));

        let projtokens = proj_tokenizer.tokenize();
        let stdtokens = std_tokenizer.tokenize();
        let projaliastokens = proj_alias_tokenizer.tokenize();

        let expected_proj_import = Import::Project(
            "project.mymod.HelloWorld".to_string(),
            "HelloWorld".to_string(),
            get_test_sample_file("src/mymod.saha")
        );

        let expected_std_import = Import::Std(
            "std.coremod.Foobar".to_string(),
            "Foobar".to_string(),
        );

        let expected_projalias_import = Import::Project(
            "project.mymod.HelloWorld".to_string(),
            "HiyaWorld".to_string(),
            get_test_sample_file("src/mymod.saha")
        );

        match projtokens {
            Ok(mut tks) => {
                let tok = tks.remove(0);
                let expected = Token::Import(testfilepos(), expected_proj_import);
                assert_eq!(expected, tok);
            }
            Err(_) => unreachable!()
        };

        match stdtokens {
            Ok(mut tks) => {
                let tok = tks.remove(0);
                let expected = Token::Import(testfilepos(), expected_std_import);
                assert_eq!(expected, tok);
            }
            Err(_) => unreachable!()
        };

        match projaliastokens {
            Ok(mut tks) => {
                let tok = tks.remove(0);
                let expected = Token::Import(testfilepos(), expected_projalias_import);
                assert_eq!(expected, tok);
            },
            Err(_) => unreachable!()
        };
    }

    #[test]
    fn test_malformed_imports_fail() {
        let testpath: PathBuf = PathBuf::from("/unknown");

        let fail_lexemes = vec![
            Lexeme::Word(testfilepos(), "use".to_string()),
            Lexeme::Whitespace(testfilepos(), " ".to_string()),
            Lexeme::Word(testfilepos(), "nonamespacing".to_string()),
            Lexeme::Symbol(testfilepos(), ";".to_string()),
        ];

        let mut fail_tokenizer = Tokenizer::new(fail_lexemes, &testpath, String::from("project"));

        let fail_tokens = fail_tokenizer.tokenize();

        assert!(fail_tokens.is_err());
    }

    #[test]
    fn test_imports_are_aliased_into_sourcecode() {
        let testpath: PathBuf = get_test_main_file();

        let lexemepos = testmainpos();

        let lexemes = vec![
            Lexeme::Word(lexemepos.clone(), "use".to_string()),
            Lexeme::Whitespace(lexemepos.clone(), " ".to_string()),
            Lexeme::Word(lexemepos.clone(), "project.mymod.HelloWorld".to_string()),
            Lexeme::Symbol(lexemepos.clone(), ";".to_string()),
            Lexeme::Newline(lexemepos.clone()),
            Lexeme::Word(lexemepos.clone(), "use".to_string()),
            Lexeme::Whitespace(lexemepos.clone(), " ".to_string()),
            Lexeme::Word(lexemepos.clone(), "project.mymod.FooestOfBars".to_string()),
            Lexeme::Whitespace(lexemepos.clone(), " ".to_string()),
            Lexeme::Word(lexemepos.clone(), "as".to_string()),
            Lexeme::Whitespace(lexemepos.clone(), " ".to_string()),
            Lexeme::Word(lexemepos.clone(), "Foibar".to_string()),
            Lexeme::Symbol(lexemepos.clone(), ";".to_string()),
            Lexeme::Newline(lexemepos.clone()),
            Lexeme::Word(lexemepos.clone(), "function".to_string()),
            Lexeme::Whitespace(lexemepos.clone(), " ".to_string()),
            Lexeme::Word(lexemepos.clone(), "main".to_string()),
            Lexeme::Symbol(lexemepos.clone(), "(".to_string()),
            Lexeme::Symbol(lexemepos.clone(), ")".to_string()),
            Lexeme::Symbol(lexemepos.clone(), "{".to_string()),
            Lexeme::Newline(lexemepos.clone()),
            Lexeme::Word(lexemepos.clone(), "var".to_string()),
            Lexeme::Whitespace(lexemepos.clone(), " ".to_string()),
            Lexeme::Word(lexemepos.clone(), "HelloWorld".to_string()),
            Lexeme::Symbol(lexemepos.clone(), "'".to_string()),
            Lexeme::Word(lexemepos.clone(), "hw".to_string()),
            Lexeme::Symbol(lexemepos.clone(), ";".to_string()),
            Lexeme::Newline(lexemepos.clone()),
            Lexeme::Word(lexemepos.clone(), "var".to_string()),
            Lexeme::Whitespace(lexemepos.clone(), " ".to_string()),
            Lexeme::Word(lexemepos.clone(), "Foibar".to_string()),
            Lexeme::Symbol(lexemepos.clone(), "'".to_string()),
            Lexeme::Word(lexemepos.clone(), "foobar".to_string()),
            Lexeme::Symbol(lexemepos.clone(), ";".to_string()),
            Lexeme::Newline(lexemepos.clone()),
            Lexeme::Symbol(lexemepos.clone(), "}".to_string()),
        ];

        let import1 = Import::Project(
            "project.mymod.HelloWorld".to_string(),
            "HelloWorld".to_string(),
            get_test_sample_file("src/mymod.saha")
        );

        let import2 = Import::Project(
            "project.mymod.FooestOfBars".to_string(),
            "Foibar".to_string(),
            get_test_sample_file("src/mymod.saha")
        );

        let expected = vec![
            Token::Import(lexemepos.clone(), import1),
            Token::Import(lexemepos.clone(), import2),
            Token::KwFunction(lexemepos.clone()),
            Token::Name(lexemepos.clone(), "project.main".to_string(), "main".to_string()),
            Token::ParensOpen(lexemepos.clone()),
            Token::ParensClose(lexemepos.clone()),
            Token::CurlyOpen(lexemepos.clone()),
            Token::KwVar(lexemepos.clone()),
            Token::Name(lexemepos.clone(), "project.mymod.HelloWorld".to_string(), "HelloWorld".to_string()),
            Token::SingleQuote(lexemepos.clone()),
            Token::Name(lexemepos.clone(), "hw".to_string(), "hw".to_string()),
            Token::EndStatement(lexemepos.clone()),
            Token::KwVar(lexemepos.clone()),
            Token::Name(lexemepos.clone(), "project.mymod.FooestOfBars".to_string(), "Foibar".to_string()),
            Token::SingleQuote(lexemepos.clone()),
            Token::Name(lexemepos.clone(), "foobar".to_string(), "foobar".to_string()),
            Token::EndStatement(lexemepos.clone()),
            Token::CurlyClose(lexemepos.clone()),
        ];

        let mut tokenizer = Tokenizer::new(lexemes, &testpath, String::from("project"));

        let tokens = tokenizer.tokenize();

        assert_eq!(expected, tokens.unwrap());
    }

    #[test]
    fn test_implements_lists_are_aliased_properly() {
        let testpath: PathBuf = get_test_main_file();

        let lexemes = vec![
            Lexeme::Word(testfilepos(), "use".to_string()),
            Lexeme::Whitespace(testfilepos(), " ".to_string()),
            Lexeme::Word(testfilepos(), "project.mymod.HelloBehavior".to_string()),
            Lexeme::Symbol(testfilepos(), ";".to_string()),

            Lexeme::Newline(testfilepos()),
            Lexeme::Newline(testfilepos()),

            Lexeme::Word(testfilepos(), "class".to_string()),
            Lexeme::Whitespace(testfilepos(), " ".to_string()),
            Lexeme::Word(testfilepos(), "MyClass".to_string()),
            Lexeme::Whitespace(testfilepos(), " ".to_string()),
            Lexeme::Symbol(testfilepos(), "{".to_string()),
            Lexeme::Newline(testfilepos()),
            Lexeme::Whitespace(testfilepos(), "    ".to_string()),
            Lexeme::Word(testfilepos(), "implements".to_string()),
            Lexeme::Whitespace(testfilepos(), " ".to_string()),
            Lexeme::Word(testfilepos(), "HelloBehavior".to_string()),
            Lexeme::Symbol(testfilepos(), ";".to_string()),
            Lexeme::Newline(testfilepos()),
            Lexeme::Symbol(testfilepos(), "}".to_string())
        ];

        let import1 = Import::Project(
            "project.mymod.HelloBehavior".to_string(),
            "HelloBehavior".to_string(),
            get_test_sample_file("src/mymod.saha")
        );

        let mut tokenizer = Tokenizer::new(lexemes, &testpath, String::from("project"));

        let tokens = tokenizer.tokenize();

        let expected = vec![
            Token::Import(testfilepos(), import1),
            Token::KwClass(testfilepos()),
            Token::Name(testfilepos(), "project.MyClass".to_string(), "MyClass".to_string()),
            Token::CurlyOpen(testfilepos()),
            Token::KwImplements(testfilepos()),
            Token::Name(testfilepos(), "project.mymod.HelloBehavior".to_string(), "HelloBehavior".to_string()),
            Token::EndStatement(testfilepos()),
            Token::CurlyClose(testfilepos()),
        ];

        assert_eq!(expected, tokens.unwrap());
    }
}
