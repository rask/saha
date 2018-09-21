//! Root parser
//!
//! Parses declarations at source code root level: functions, classes,
//! behaviors, and constants.

use std::{
    collections::HashMap,
    iter::{Iterator, Peekable},
    slice::Iter,
    mem::discriminant,
};

use saha_lib::{
    errors::{
        Error,
        ParseError
    },
    source::FilePosition,
    types::{
        SahaType,
        Value,
        objects::{MemberVisibility},
        functions::{FunctionParameter, SahaFunctionParamDefs}
    }
};

use saha_tokenizer::token::Token;

use ::{
    parse_table::{
        ParseTable,
        FunctionDefinition,
        PropertyDefinition,
        ClassDefinition,
        BehaviorDefinition
    },
    parser::{
        TokenType,
        PR,
        ParsesTokens
    }
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
            Token::Name(f, a, n) => (f, a.to_string(), n.to_string()),
            _ => unreachable!()
        };

        self.consume_next(vec!["("])?;

        let fn_parameter_definitions: SahaFunctionParamDefs = match self.ntok.unwrap() {
            Token::ParensClose(..) => HashMap::new(),
            _ => self.parse_function_parameter_definitions()?
        };

        self.consume_next(vec![")"])?;

        let return_type: SahaType = self.parse_function_return_type()?;

        // If there was a return type we need to consume the body open curly here, otherwise it has
        // been consumed already
        match return_type {
            SahaType::Void => (),
            _ => self.consume_next(vec!["{"])?
        };

        let fn_body_tokens = self.parse_curly_block()?;

        // last curly open token was parsed in the parse fn call above

        let fn_definition = FunctionDefinition {
            name: fn_alias.to_owned(),
            source_name: fn_source_name,
            return_type: return_type,
            body_tokens: fn_body_tokens,
            parameters: fn_parameter_definitions,
            visibility: MemberVisibility::Public
        };

        self.parse_table.functions.insert(fn_alias, fn_definition);

        return self.parse_root();
    }

    /// Parse function declaration parameter definitions.
    fn parse_function_parameter_definitions(&mut self) -> PR<SahaFunctionParamDefs> {
        let mut param_defs: SahaFunctionParamDefs = HashMap::new();

        loop {
            self.consume_next(vec!["name"])?;

            let param_name = match self.ctok.unwrap() {
                Token::Name(_, _, name) => name.to_owned(),
                _ => unreachable!()
            };

            self.consume_next(vec!["'"])?;

            self.consume_next(vec!["typestring", "typeinteger", "typefloat", "typeboolean", "name"])?;

            let param_type = match self.ctok.unwrap() {
                Token::TypeBoolean(..) => SahaType::Bool,
                Token::TypeString(..) => SahaType::Str,
                Token::TypeInteger(..) => SahaType::Int,
                Token::TypeFloat(..) => SahaType::Float,
                Token::Name(_, n, _) => SahaType::Name(n.to_owned()),
                _ => unreachable!()
            };

            let paramdef: FunctionParameter;

            match self.ntok.unwrap() {
                Token::Assign(..) => {
                    self.consume_next(vec!["="])?;

                    // only primitive types can be set as param defaults
                    self.consume_next(vec!["stringval", "booleanval", "integerval", "floatval"])?;

                    let (def_pos, default) = match self.ctok.unwrap() {
                        Token::StringValue(fp, val) => (fp, Value::str(val.to_owned())),
                        Token::BooleanValue(fp, val) => (fp, Value::bool(val.to_owned())),
                        Token::IntegerValue(fp, val) => (fp, Value::int(val.to_owned())),
                        Token::FloatValue(fp, val) => (fp, Value::float(val.to_owned())),
                        _ => unreachable!()
                    };

                    if param_type != default.kind {
                        return Err(ParseError::new(
                            &format!(
                                "Parameter type mismatch for `{}`, expected `{}` but given default value is a `{}`",
                                param_name,
                                param_type.to_readable_string(),
                                default.kind.to_readable_string()
                            ),
                            Some(def_pos.to_owned())
                        ));
                    }

                    paramdef = FunctionParameter {
                        param_type: param_type,
                        default: default
                    };
                },
                _ => {
                    paramdef = FunctionParameter {
                        param_type: param_type,
                        default: Value::void()
                    };
                }
            };

            param_defs.insert(param_name, paramdef);

            match self.ntok.unwrap() {
                Token::ParensClose(..) => break,
                _ => {
                    self.consume_next(vec![","])?;

                    continue;
                }
            };
        }

        return Ok(param_defs);
    }

    /// Parse a function return type.
    fn parse_function_return_type(&mut self) -> PR<SahaType> {
        self.consume_next(vec!["name", "typestring", "typeboolean", "typeinteger", "typefloat", "{"])?;

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

    /// Parse constant declaration.
    fn parse_constant_declaration(&mut self) -> PR<()> {
        let (const_name, const_type) = self.parse_constant_name_declaration()?;



        self.consume_next(vec!["="])?;

        let const_val = self.parse_literal_value()?;

        if const_val.kind != const_type {
            return Err(ParseError::new(
                &format!(
                    "Constant type mismatch for `{}`, expected `{}`, received `{}`",
                    const_name,
                    const_type.to_readable_string(),
                    const_val.kind.to_readable_string()
                ),
                Some(self.ctok.unwrap().get_file_position())
            ));
        }

        self.parse_table.constants.insert(const_name, const_val);

        self.consume_next(vec![";"])?;

        return self.parse_root();
    }

    fn validate_constant_name(&mut self, const_name: String, name_pos: &FilePosition) -> PR<()> {
        let acceptable = [
            '_', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
            'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
            'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
        ];

        let is_valid: bool = const_name.chars().fold(true, |mut valid_char, c| {
            if valid_char == false {
                return valid_char;
            }

            valid_char = acceptable.contains(&c);

            return valid_char;
        });

        if is_valid == false {
            return Err(ParseError::new(
                "Invalid constant name, constants should be defined using `A-Z0-9` and unsderscores.",
                Some(name_pos.to_owned())
            ));
        }

        return Ok(());
    }

    /// Parse const name and type from `name'type`.
    fn parse_constant_name_declaration(&mut self) -> PR<(String, SahaType)> {
        self.consume_next(vec!["name"])?;

        let (const_name_pos, const_name) = match self.ctok.unwrap() {
            Token::Name(pos, _, source_name) => (pos, source_name), // use source name for consts, always
            _ => unreachable!()
        };

        self.validate_constant_name(const_name.to_owned(), const_name_pos)?;

        self.consume_next(vec!["'"])?;
        self.consume_next(vec!["str", "bool", "int", "float"])?; // consts dont accept names or refs

        let const_type = match self.ctok.unwrap() {
            Token::TypeBoolean(..) => SahaType::Bool,
            Token::TypeString(..) => SahaType::Str,
            Token::TypeInteger(..) => SahaType::Int,
            Token::TypeFloat(..) => SahaType::Float,
            _ => unreachable!()
        };

        return Ok((const_name.to_owned(), const_type));
    }

    /// Parse a literal non-name non-object value.
    fn parse_literal_value(&mut self) -> PR<Value> {
        self.consume_next(vec!["stringval", "integerval", "booleanval", "floatval"])?;

        let value = match self.ctok.unwrap() {
            Token::StringValue(_, val) => Value::str(val.to_owned()),
            Token::BooleanValue(_, val) => Value::bool(val.to_owned()),
            Token::IntegerValue(_, val) => Value::int(val.to_owned()),
            Token::FloatValue(_, val) => Value::float(val.to_owned()),
            _ => unreachable!()
        };

        return Ok(value);
    }

    /// Parse class declaration.
    fn parse_class_declaration(&mut self) -> PR<()> {
        self.consume_next(vec!["name"])?;

        let (cname_pos, cname, cname_source) = match self.ctok.unwrap() {
            Token::Name(pos, alias, sourcename) => (pos, alias, sourcename),
            _ => unreachable!()
        };

        self.consume_next(vec!["{"])?;

        let (property_definitions, method_definitions, implements) = self.parse_class_body()?;

        // closing `}` was parsed n body parsing function

        unimplemented!()
    }

    /// Parse the contents of a class declaration. Methods, props, implements.
    fn parse_class_body(&mut self)
        -> PR<(HashMap<String, PropertyDefinition>, HashMap<String, FunctionDefinition>, Vec<String>)> {
        let mut props: HashMap<String, PropertyDefinition> = HashMap::new();
        let mut methods: HashMap<String, FunctionDefinition> = HashMap::new();
        let mut implements: Vec<String> = Vec::new();

        loop {
            self.consume_next(vec!["method", "prop", "pub", "static", "implements", "}"])?;

            let mut member_visibility = MemberVisibility::Private;
            let mut member_is_static = false;

            match self.ctok.unwrap() {
                Token::CurlyClose(..) => break, // reached end of class body
                Token::KwPublic(..) => {
                    member_visibility = MemberVisibility::Public;

                    self.consume_next(vec!["static", "method", "property"])?;

                    match self.ctok.unwrap() {
                        Token::KwStatic(..) => {
                            member_is_static = true;

                            self.consume_next(vec!["method", "property"])?;

                            match self.ctok.unwrap() {
                                Token::KwMethod(..) => {
                                    let method = self.parse_class_method(member_visibility, member_is_static)?;

                                    methods.insert(method.name.to_owned(), method);
                                },
                                Token::KwProperty(..) => {
                                    let prop = self.parse_class_property(member_visibility, member_is_static)?;

                                    props.insert(prop.name.to_owned(), prop);
                                },
                                _ => unreachable!()
                            };
                        },
                        Token::KwMethod(..) => {
                            let method = self.parse_class_method(member_visibility, member_is_static)?;

                            methods.insert(method.name.to_owned(), method);
                        },
                        Token::KwProperty(..) => {
                            let prop = self.parse_class_property(member_visibility, member_is_static)?;

                            props.insert(prop.name.to_owned(), prop);
                        },
                        _ => unreachable!()
                    }
                },
                Token::KwStatic(..) => {
                    member_is_static = true;

                    self.consume_next(vec!["pub", "method", "property"])?;

                    match self.ctok.unwrap() {
                        Token::KwPublic(..) => {
                            member_visibility = MemberVisibility::Public;

                            self.consume_next(vec!["method", "property"])?;

                            match self.ctok.unwrap() {
                                Token::KwMethod(..) => {
                                    let method = self.parse_class_method(member_visibility, member_is_static)?;

                                    methods.insert(method.name.to_owned(), method);
                                },
                                Token::KwProperty(..) => {
                                    let prop = self.parse_class_property(member_visibility, member_is_static)?;

                                    props.insert(prop.name.to_owned(), prop);
                                },
                                _ => unreachable!()
                            };
                        },
                        Token::KwMethod(..) => {
                            let method = self.parse_class_method(member_visibility, member_is_static)?;

                            methods.insert(method.name.to_owned(), method);
                        },
                        Token::KwProperty(..) => {
                            let prop = self.parse_class_property(member_visibility, member_is_static)?;

                            props.insert(prop.name.to_owned(), prop);
                        },
                        _ => unreachable!()
                    };
                },
                Token::KwMethod(..) => {
                    let method = self.parse_class_method(member_visibility, member_is_static)?;

                    methods.insert(method.name.to_owned(), method);
                },
                Token::KwProperty(..) => {
                    let prop = self.parse_class_property(member_visibility, member_is_static)?;

                    props.insert(prop.name.to_owned(), prop);
                },
                Token::KwImplements(..) => {
                    let mut impl_list = self.parse_implements_list()?;

                    implements.append(&mut impl_list);
                },
                _ => unreachable!()
            };
        }

        return Ok((props, methods, implements));
    }

    /// Parse a class property.
    fn parse_class_property(&mut self, visibility: MemberVisibility, is_static: bool) -> PR<PropertyDefinition> {
        unimplemented!()
    }

    /// Parse a class method
    fn parse_class_method(&mut self, visibility: MemberVisibility, is_static: bool) -> PR<FunctionDefinition> {
        unimplemented!()
    }

    /// Parse a comma-separated list of names in an implements declaration.
    fn parse_implements_list(&mut self) -> PR<Vec<String>> {
        let mut impl_list: Vec<String> = Vec::new();

        loop {
            self.consume_next(vec!["name"])?;

            let impl_name = match self.ctok.unwrap() {
                Token::Name(_, alias, _) => alias,
                _ => unreachable!()
            };

            impl_list.push(impl_name.to_owned());

            self.consume_next(vec![",", ";"])?;

            match self.ctok.unwrap() {
                Token::EndStatement(..) => break,
                Token::Comma(..) => continue,
                _ => unreachable!()
            };
        }

        return Ok(impl_list);
    }

    /// Parse behavior declaration.
    fn parse_behavior_declaration(&mut self) -> PR<()> {
        unimplemented!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn testfilepos() -> FilePosition {
        return FilePosition::unknown();
    }

    #[test]
    fn test_it_parses_constants() {
        let tokens = vec![
            Token::KwConstant(testfilepos()),
            Token::Name(testfilepos(), "FOOBAR".to_string(), "FOOBAR".to_string()),
            Token::SingleQuote(testfilepos()),
            Token::TypeString(testfilepos()),
            Token::Assign(testfilepos()),
            Token::StringValue(testfilepos(), "FooestBar".to_string()),
            Token::EndStatement(testfilepos()),
            Token::Eof(testfilepos())
        ];

        let mut parse_table = ParseTable::new();

        {
            let mut parser = RootParser::new(&tokens, &mut parse_table);

            let res = parser.start_parse();

            if res.is_err() {
                eprintln!("{:?}", res.err().unwrap());
                panic!();
            }
        }

        let val: Value = parse_table.constants.clone().get("FOOBAR").unwrap().to_owned();

        assert_eq!(val, Value::str("FooestBar".to_string()));
    }

    #[test]
    fn test_constant_names_are_validated() {
        let tokens = vec![
            Token::KwConstant(testfilepos()),
            Token::Name(testfilepos(), "thishaslcchars".to_string(), "thishaslcchars".to_string()),
            Token::SingleQuote(testfilepos()),
            Token::TypeString(testfilepos()),
            Token::Assign(testfilepos()),
            Token::StringValue(testfilepos(), "FooestBar".to_string()),
            Token::EndStatement(testfilepos()),
            Token::Eof(testfilepos())
        ];

        let mut parse_table = ParseTable::new();

        let mut parser = RootParser::new(&tokens, &mut parse_table);

        let res = parser.start_parse();

        if res.is_ok() {
            eprintln!("No error encountered, meaning const name validation is broken");
            panic!();
        }

        let err_str = res.err().unwrap();

        assert!(err_str.get_message().contains("`A-Z0-9`"));
    }

    #[test]
    fn test_it_parses_function_declarations() {
        let tokens = vec![
            Token::KwFunction(testfilepos()),
            Token::Name(testfilepos(), "project.main".to_string(), "main".to_string()),
            Token::ParensOpen(testfilepos()),
            Token::ParensClose(testfilepos()),
            Token::TypeInteger(testfilepos()),
            Token::CurlyOpen(testfilepos()),
            Token::CurlyClose(testfilepos()),
            Token::Eof(testfilepos())
        ];

        let mut parse_table = ParseTable::new();

        {
            let mut parser = RootParser::new(&tokens, &mut parse_table);

            let res = parser.start_parse();

            if res.is_err() {
                eprintln!("{:?}", res.err().unwrap());
                panic!();
            }
        }

        let fndefinition = parse_table.functions.get("project.main").unwrap();

        assert_eq!(SahaType::Int, fndefinition.return_type);
        assert_eq!(1, fndefinition.body_tokens.len()); // should contain only an EOB
        assert_eq!(0, fndefinition.parameters.len());
        assert_eq!("main".to_string(), fndefinition.source_name);
        assert_eq!("project.main".to_string(), fndefinition.name);
    }

    #[test]
    fn test_function_parameters_are_parsed_properly() {
        let tokens = vec![
            Token::KwFunction(testfilepos()),
            Token::Name(testfilepos(), "project.main".to_string(), "main".to_string()),
            Token::ParensOpen(testfilepos()),

            // params
            Token::Name(testfilepos(), "foo".to_string(), "foo".to_string()),
            Token::SingleQuote(testfilepos()),
            Token::TypeString(testfilepos()),
            Token::Comma(testfilepos()),
            Token::Name(testfilepos(), "bar".to_string(), "bar".to_string()),
            Token::SingleQuote(testfilepos()),
            Token::TypeInteger(testfilepos()),
            Token::Assign(testfilepos()),
            Token::IntegerValue(testfilepos(), 123),

            Token::ParensClose(testfilepos()),
            Token::TypeInteger(testfilepos()),
            Token::CurlyOpen(testfilepos()),
            Token::CurlyClose(testfilepos()),
            Token::Eof(testfilepos())
        ];

        let mut parse_table = ParseTable::new();

        {
            let mut parser = RootParser::new(&tokens, &mut parse_table);

            let res = parser.start_parse();

            if res.is_err() {
                eprintln!("{:?}", res.err().unwrap());
                panic!();
            }
        }

        let fndefinition = parse_table.functions.get("project.main").unwrap();
        let params = fndefinition.parameters.clone();

        let foo_param = params.get("foo").unwrap();
        let bar_param = params.get("bar").unwrap();

        assert_eq!(SahaType::Str, foo_param.param_type);
        assert_eq!(SahaType::Void, foo_param.default.kind);

        assert_eq!(SahaType::Int, bar_param.param_type);
        assert_eq!(SahaType::Int, bar_param.default.kind);
        assert_eq!(123, bar_param.default.int.unwrap());
    }

    #[test]
    fn test_function_bodies_are_parsed_properly() {
        let tokens = vec![
            Token::KwFunction(testfilepos()),
            Token::Name(testfilepos(), "project.main".to_string(), "main".to_string()),
            Token::ParensOpen(testfilepos()),
            Token::ParensClose(testfilepos()),
            Token::TypeInteger(testfilepos()),
            Token::CurlyOpen(testfilepos()),

            Token::KwVar(testfilepos()),
            Token::Name(testfilepos(), "myvar".to_string(), "myvar".to_string()),
            Token::SingleQuote(testfilepos()),
            Token::TypeBoolean(testfilepos()),
            Token::EndStatement(testfilepos()),

            Token::KwLoop(testfilepos()),
            Token::CurlyOpen(testfilepos()),
            Token::Name(testfilepos(), "print".to_string(), "print".to_string()),
            Token::ParensOpen(testfilepos()),
            Token::Name(testfilepos(), "value".to_string(), "value".to_string()),
            Token::Assign(testfilepos()),
            Token::StringValue(testfilepos(), "hello".to_string()),
            Token::ParensClose(testfilepos()),
            Token::EndStatement(testfilepos()),
            Token::CurlyClose(testfilepos()),

            Token::KwReturn(testfilepos()),
            Token::IntegerValue(testfilepos(), 0),

            Token::CurlyClose(testfilepos()),
            Token::Eof(testfilepos())
        ];

        let mut parse_table = ParseTable::new();

        {
            let mut parser = RootParser::new(&tokens, &mut parse_table);

            let res = parser.start_parse();

            if res.is_err() {
                eprintln!("{:?}", res.err().unwrap());
                panic!();
            }
        }

        let fndefinition = parse_table.functions.get("project.main").unwrap();

        assert_eq!(18, fndefinition.body_tokens.len());
    }
}