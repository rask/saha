//! AST parser
//!
//! Parses AST from function/method bodies.

use std::{
    collections::HashMap,
    iter::{Iterator, Peekable},
    slice::Iter,
    mem::discriminant,
};

use saha_lib::{
    ast::*,
    errors::{
        Error,
        ParseError
    },
    source::{
        files::FilePosition,
        token::Token
    },
    types::{
        SahaType,
        Value,
        objects::{MemberVisibility},
        functions::{FunctionParameter, SahaFunctionParamDefs}
    }
};

use crate::{
    parser::{
        TokenType,
        PR,
        ParsesTokens
    }
};

/// AstParser, which parses functions and methods from tokens into ASTs.
pub struct AstParser<'a> {
    ctok: Option<&'a Token>,
    ptok: Option<&'a Token>,
    ntok: Option<&'a Token>,
    tokens: Peekable<Iter<'a, Token>>
}

impl<'a> ParsesTokens for AstParser<'a> {
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

impl<'a> AstParser<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> AstParser<'a> {
        return AstParser {
            ctok: None,
            ptok: None,
            ntok: None,
            tokens: tokens.iter().peekable()
        };
    }

    /// Start AST parsing.
    pub fn start_parse(&mut self) -> PR<Ast> {
        {
            match self.tokens.peek() {
                Some(tok) => self.ntok = Some(tok.to_owned()),
                None => return Err(ParseError::new(
                    "Invalid token stream, no tokens found",
                    Some(FilePosition::unknown())
                ))
            };
        }

        return Ok(Ast {
            entrypoint: self.parse_block(true)?
        });
    }

    /// Parse a curly brace block. `is_root` defines whether we are at a function body root or
    /// whether we are in an inner block, e.g. ifelse block.
    fn parse_block(&mut self, is_root: bool) -> PR<Box<Block>> {
        let block_open_pos: FilePosition;

        if is_root == false {
            // at root we have no curly bounds
            self.consume_next(vec!["{"])?;

            block_open_pos = match self.ctok.unwrap() {
                Token::CurlyOpen(f) => f.to_owned(),
                _ => unreachable!()
            };
        } else {
            block_open_pos = self.ntok.unwrap_or(&Token::Eob).get_file_position();
        }

        let statements = self.parse_statements()?;

        if is_root == false {
            // at root we have no curly bounds
            self.consume_next(vec!["}"])?;
        }

        return Ok(Box::new(Block {
            statements: statements,
            file_position: block_open_pos.to_owned()
        }));
    }

    /// Parse statements inside a block.
    fn parse_statements(&mut self) -> PR<Vec<Box<Statement>>> {
        let mut statements = Vec::new();

        loop {
            // determine which statements end with a `;` character
            let stmt_ends_in_eos = match self.ntok.unwrap() {
                Token::Name(..) | Token::KwVar(..) | Token::KwRaise(..) |
                Token::KwContinue(..) | Token::KwBreak(..) | Token::KwReturn(..) |
                Token::ParensOpen(..) => true,
                _ => false
            };

            let statement: Box<Statement> = match self.ntok.unwrap() {
                Token::Eob | Token::CurlyClose(..) => break,
                Token::KwVar(..) => self.parse_variable_declaration_statement()?,
                Token::KwIf(..) => unimplemented!(),
                Token::KwLoop(..) => unimplemented!(),
                Token::KwFor(..) => unimplemented!(),
                Token::KwReturn(..) => self.parse_return_statement()?,
                Token::KwRaise(..) => unimplemented!(),
                Token::KwTry(..) => unimplemented!(),
                Token::KwBreak(..) => unimplemented!(),
                Token::KwContinue(..) => unimplemented!(),
                _ => self.parse_expression_statement()?,
            };

            if stmt_ends_in_eos {
                self.consume_next(vec![";"])?;
            }

            statements.push(statement);
        }

        return Ok(statements);
    }

    /// Parse a variable declaration.
    fn parse_variable_declaration_statement(&mut self) -> PR<Box<Statement>> {
        self.consume_next(vec!["var"])?;

        let statement_pos = match self.ctok.unwrap() {
            Token::KwVar(f) => f,
            _ => unreachable!()
        };

        self.consume_next(vec!["name"])?;

        let (ident_pos, ident_val) = match self.ctok.unwrap() {
            Token::Name(f, _, n) => (f, n),
            _ => unreachable!()
        };

        let identifier = Identifier {
            file_position: ident_pos.to_owned(),
            identifier: ident_val.to_owned()
        };

        // variable type
        self.consume_next(vec!["'"])?;
        self.consume_next(vec!["name", "str", "int", "float", "bool"])?;

        let var_type = match self.ctok.unwrap() {
            Token::TypeBoolean(..) => SahaType::Bool,
            Token::TypeInteger(..) => SahaType::Int,
            Token::TypeFloat(..) => SahaType::Float,
            Token::TypeString(..) => SahaType::Str,
            Token::Name(_, alias, _) => SahaType::Name(alias.to_owned()),
            _ => unreachable!()
        };

        // if we don't have an assigned we return an uninited variable
        if let Token::EndStatement(..) = self.ntok.unwrap() {
            let stmt = Statement {
                file_position: statement_pos.to_owned(),
                kind: StatementKind::VarDeclaration(identifier, var_type, None),
            };

            return Ok(Box::new(stmt));
        }

        self.consume_next(vec!["="])?;

        let value_expr: Box<Expression> = self.parse_expression(0)?;

        let stmt = Statement {
            file_position: statement_pos.to_owned(),
            kind: StatementKind::VarDeclaration(identifier, var_type, Some(value_expr))
        };

        return Ok(Box::new(stmt));
    }

    /// Parse a statement which is a bare expression.
    fn parse_expression_statement(&mut self) -> PR<Box<Statement>> {
        let stmt = Statement {
            file_position: self.ntok.unwrap().get_file_position(),
            kind: StatementKind::Expression(self.parse_expression(0)?)
        };

        return Ok(Box::new(stmt));
    }

    /// Parse a return statement.
    fn parse_return_statement(&mut self) -> PR<Box<Statement>> {
        self.consume_next(vec!["return"])?;

        let return_pos = self.ctok.unwrap().get_file_position();

        // if we encounter a `;` right after the return keyword, we are returning void, otherwise
        // we should parse and expression
        //
        // we don't consume the `;`, as that is handled in statement parent parsing method from
        // where this method was called
        let return_expr: Box<Expression> = match self.ntok.unwrap() {
            Token::EndStatement(..) => Box::new(Expression {
                file_position: self.ntok.unwrap().get_file_position(),
                kind: ExpressionKind::LiteralValue(Value::void())
            }),
            _ => self.parse_expression(0)?
        };

        return Ok(Box::new(Statement {
            file_position: return_pos,
            kind: StatementKind::Return(return_expr)
        }));
    }

    /// Parse an expression.
    fn parse_expression(&mut self, minimum_op_precedence: i8) -> PR<Box<Expression>> {
        let expression = self.parse_primary()?;

        let ntok: &Token = self.ntok.unwrap_or(&Token::Eob);
        let next_precedence = ntok.get_precedence();

        if next_precedence < minimum_op_precedence {
            // non-operator or lesser precedence
            return Ok(expression);
        }

        return self.parse_binop_expression(expression, minimum_op_precedence);
    }

    /// Primaries are building blocks for expressions. We could parse these in the
    /// `parse_expression` method, but separating concerns makes it simpler to consume. Also helps
    /// with operator precedence parsing.
    fn parse_primary(&mut self) -> PR<Box<Expression>> {
        self.consume_next(vec![
            "(", "[", "{", "new", "-", "!",
            "name", "stringval", "integerval", "floatval", "booleanval"
        ])?;

        let primary: Box<Expression> = match self.ctok.unwrap() {
            Token::ParensOpen(..) => {
                let expr = self.parse_expression(0)?;

                self.consume_next(vec![")"])?;

                expr
            },
            Token::BraceOpen(..) => unimplemented!(),
            Token::CurlyOpen(..) => unimplemented!(),
            Token::UnOpNot(..)
            | Token::OpSub(..) => unimplemented!(),
            Token::StringValue(..)
            | Token::IntegerValue(..)
            | Token::FloatValue(..)
            | Token::BooleanValue(..) => self.parse_literal_value()?,
            Token::KwNew(..) => unimplemented!(),
            Token::Name(..) => {
                let identpath_expr = self.parse_ident_path()?;

                // see if we're working with a function call
                match self.ntok.unwrap() {
                    Token::ParensOpen(..) => self.parse_function_call(identpath_expr)?,
                    _ => identpath_expr
                }
            },
            _ => unreachable!()
        };

        return Ok(primary);
    }

    /// Parse a binary operation. First we parse the op and then the RHS expression. Then we check
    /// if we should parse another binop.
    fn parse_binop_expression(&mut self, lhs_expr: Box<Expression>, minimum_op_precedence: i8) -> PR<Box<Expression>> {
        self.consume_next(vec!["+", "-", "*", "/", "&&", "||", ">", "<", ">=", "<="])?;

        let op_token = self.ctok.unwrap();

        let binop = BinOp::from_token(op_token);

        if binop.is_err() {
            return Err(ParseError::new(
                &format!("Could not parse binary operation type from token `{}`", op_token),
                Some(op_token.get_file_position())
            ));
        }

        let binop = binop.ok().unwrap();

        let next_min_precedence = match binop.is_left_assoc {
            true => minimum_op_precedence + 1,
            false => minimum_op_precedence
        };

        let rhs_expression = self.parse_expression(next_min_precedence)?;

        let binop_expr = Box::new(Expression {
            file_position: lhs_expr.file_position.to_owned(),
            kind: ExpressionKind::BinaryOperation(lhs_expr, binop, rhs_expression)
        });

        let ntok: &Token = self.ntok.unwrap_or(&Token::Eob);
        let next_precedence = ntok.get_precedence();

        if next_precedence < minimum_op_precedence {
            // non-operator or lesser precedence
            return Ok(binop_expr);
        }

        // FIXME should the next predence be `minimum_op_precedence` or `next_min_precedence` here?
        return self.parse_binop_expression(binop_expr, next_min_precedence);
    }

    /// Parse a literal value token.
    fn parse_literal_value(&mut self) -> PR<Box<Expression>> {
        let fpos = self.ctok.unwrap().get_file_position();

        let value = match self.ctok.unwrap() {
            Token::StringValue(_, val) => Value::str(val.to_owned()),
            Token::BooleanValue(_, val) => Value::bool(*val),
            Token::IntegerValue(_, val) => Value::int(*val),
            Token::FloatValue(_, val) => Value::float(*val),
            _ => unreachable!()
        };

        let val_expr = Expression {
            file_position: fpos,
            kind: ExpressionKind::LiteralValue(value)
        };

        return Ok(Box::new(val_expr));
    }

    fn parse_ident_path(&mut self) -> PR<Box<Expression>> {
        let curtok = self.ctok.unwrap();
        let mut path_items: Vec<(AccessKind, Identifier)> = Vec::new();

        let root: Identifier = match curtok {
            Token::Name(pos, alias, _) => {
                Identifier {
                    file_position: pos.clone(),
                    identifier: alias.to_string()
                }
            },
            _ => return Err(ParseError::new(
                &format!("Unexpected `{}`, expected name", curtok), Some(curtok.get_file_position())
            ))
        };

        let mut next_is_access_token: bool = match self.ntok.unwrap() {
            Token::StaticAccess(..) | Token::ObjectAccess(..) => true,
            _ => false
        };

        while next_is_access_token {
            self.consume_next(vec!["->", "::"])?;

            let access_kind: AccessKind = match self.ctok.unwrap() {
                Token::ObjectAccess(..) => AccessKind::Instance,
                Token::StaticAccess(..) => AccessKind::Static,
                _ => unreachable!()
            };

            self.consume_next(vec!["name"])?;

            let item_ident: Identifier = match self.ctok.unwrap() {
                Token::Name(pos, alias, _) => {
                    Identifier {
                        file_position: pos.clone(),
                        identifier: alias.to_string()
                    }
                },
                _ => unreachable!()
            };

            path_items.push((access_kind, item_ident));
        }

        let path_expr = Expression {
            file_position: root.file_position.clone(),
            kind: ExpressionKind::IdentPath(root, path_items)
        };

        return Ok(Box::new(path_expr));
    }

    /// Parse a function call expression which is tied to a identifier path.
    fn parse_function_call(&mut self, ident_expr: Box<Expression>) -> PR<Box<Expression>> {
        self.consume_next(vec!["("])?;

        let call_pos = self.ctok.unwrap().get_file_position();

        let call_args: Box<Expression> = self.parse_callable_args()?;

        self.consume_next(vec![")"])?;

        let fn_call_expr = Expression {
            file_position: call_pos,
            kind: ExpressionKind::FunctionCall(ident_expr, call_args)
        };

        return Ok(Box::new(fn_call_expr));
    }

    /// Parse function call arguments that are wrapped in parentheses. Also used
    /// for new instance args.
    fn parse_callable_args(&mut self) -> PR<Box<Expression>> {
        let mut args: Vec<Box<Expression>> = Vec::new();
        let args_pos = self.ctok.unwrap().get_file_position();

        loop {
            match self.ntok.unwrap() {
                Token::ParensClose(..) => break,
                Token::Comma(..) => {
                    self.consume_next(vec![","])?;
                }
                _ => {
                    args.push(self.parse_callable_arg()?);
                }
            }
        }

        let args_expr = Expression {
            file_position: args_pos,
            kind: ExpressionKind::CallableArgs(args)
        };

        return Ok(Box::new(args_expr));
    }

    /// Parse a single function call argument.
    fn parse_callable_arg(&mut self) -> PR<Box<Expression>> {
        self.consume_next(vec!["name"])?;

        let argname: String = String::new();
        let argpos: FilePosition = FilePosition::unknown();

        let (argname, argpos) = match self.ctok.unwrap() {
            Token::Name(pos, _, name) => (name, pos),
            _ => unreachable!()
        };

        self.consume_next(vec!["="])?;

        let arg_value_expr = self.parse_expression(0)?;

        return Ok(Box::new(Expression {
            file_position: argpos.clone(),
            kind: ExpressionKind::CallableArg(Identifier {
                file_position: argpos.clone(),
                identifier: argname.clone()
            },
            arg_value_expr)
        }));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn testfilepos() -> FilePosition {
        return FilePosition::unknown();
    }

    #[test]
    fn test_empty_is_parsed_correctly() {
        let tokens = vec![
            Token::Eob
        ];

        let mut parser = AstParser::new(&tokens);

        let ast = parser.start_parse();

        if ast.is_err() {
            eprintln!("{:?}", ast.err().unwrap().get_message());
            panic!();
        }

        let ast = ast.ok().unwrap();

        assert_eq!(0, ast.entrypoint.statements.len());
    }

    #[test]
    fn test_variable_declarations_are_parsed_correctly() {
        let tokens = vec![
            Token::KwVar(testfilepos()),
            Token::Name(testfilepos(), "foo".to_string(), "foo".to_string()),
            Token::SingleQuote(testfilepos()),
            Token::TypeString(testfilepos()),
            Token::Assign(testfilepos()),
            Token::StringValue(testfilepos(), "bar".to_string()),
            Token::EndStatement(testfilepos()),
            Token::Eob
        ];

        let mut parser = AstParser::new(&tokens);

        let ast = parser.start_parse();

        if ast.is_err() {
            eprintln!("{:?}", ast.err().unwrap().get_message());
            panic!();
        }

        let ast = ast.ok().unwrap();
        let mut statements = ast.entrypoint.statements.clone();

        assert_eq!(1, statements.len());

        let decl: Box<Statement> = statements.pop().unwrap();

        match decl.kind {
            StatementKind::VarDeclaration(ref ident, ref vartype, ref value) => {
                assert_eq!(Identifier {
                    file_position: testfilepos(),
                    identifier: "foo".to_string()
                }, ident.to_owned());

                assert_eq!(SahaType::Str, vartype.to_owned());

                assert_eq!(Box::new(Expression {
                    file_position: testfilepos(),
                    kind: ExpressionKind::LiteralValue(Value::str("bar".to_string()))
                }), value.to_owned().unwrap())
            },
            _ => unreachable!()
        }
    }

    #[test]
    fn test_binops_are_parsed() {
        let tokens = vec![
            Token::ParensOpen(testfilepos()),
            Token::IntegerValue(testfilepos(), 1),
            Token::OpAdd(testfilepos()),
            Token::IntegerValue(testfilepos(), 1),
            Token::OpAdd(testfilepos()),
            Token::IntegerValue(testfilepos(), 2),
            Token::OpMul(testfilepos()),
            Token::IntegerValue(testfilepos(), 3),
            Token::OpSub(testfilepos()),
            Token::IntegerValue(testfilepos(), 1),
            Token::ParensClose(testfilepos()),
            Token::EndStatement(testfilepos()),
            Token::Eob
        ];

        // above is
        // (1 + 1 + 2 * 3 - 1);

        let mut parser = AstParser::new(&tokens);

        let ast = parser.start_parse();

        if ast.is_err() {
            eprintln!("{:?}", ast.err().unwrap().get_message());
            panic!();
        }

        let ast = ast.ok().unwrap();
        let mut statements = ast.entrypoint.statements.clone();

        assert_eq!(1, statements.len());

        let stmt = statements.pop().unwrap();

        // this will get messy
        let expected_expr = Box::new(Expression {
            file_position: testfilepos(),
            kind: ExpressionKind::BinaryOperation(
                Box::new(Expression {
                    file_position: testfilepos(),
                    kind: ExpressionKind::LiteralValue(Value::int(1))
                }),
                BinOp {
                    kind: BinOpKind::Add,
                    is_left_assoc: true,
                    file_position: testfilepos()
                },
                Box::new(Expression {
                    file_position: testfilepos(),
                    kind: ExpressionKind::BinaryOperation(
                        Box::new(Expression {
                            file_position: testfilepos(),
                            kind: ExpressionKind::LiteralValue(Value::int(1))
                        }),
                        BinOp {
                            kind: BinOpKind::Add,
                            is_left_assoc: true,
                            file_position: testfilepos()
                        },
                        Box::new(Expression {
                            file_position: testfilepos(),
                            kind: ExpressionKind::BinaryOperation(
                                Box::new(Expression {
                                    file_position: testfilepos(),
                                    kind: ExpressionKind::BinaryOperation(
                                        Box::new(Expression {
                                            file_position: testfilepos(),
                                            kind: ExpressionKind::LiteralValue(Value::int(2))
                                        }),
                                        BinOp {
                                            kind: BinOpKind::Mul,
                                            is_left_assoc: true,
                                            file_position: testfilepos()
                                        },
                                        Box::new(Expression {
                                            file_position: testfilepos(),
                                            kind: ExpressionKind::LiteralValue(Value::int(3))
                                        })
                                    )
                                }),
                                BinOp {
                                    file_position: testfilepos(),
                                    is_left_assoc: true,
                                    kind: BinOpKind::Sub
                                },
                                Box::new(Expression {
                                    file_position: testfilepos(),
                                    kind: ExpressionKind::LiteralValue(Value::int(1))
                                })
                            )
                        })
                    )
                })
            )
        });

        match stmt.kind {
            StatementKind::Expression(expr) => {
                assert_eq!(expected_expr, expr);
            },
            _ => panic!("Unexpected statement kind, expected an expression statement")
        };
    }

    #[test]
    fn test_left_assoc_ops_work_as_they_should() {
        let tokens = vec![
            Token::ParensOpen(testfilepos()),
            Token::IntegerValue(testfilepos(), 1),
            Token::OpAdd(testfilepos()),
            Token::IntegerValue(testfilepos(), 1),
            Token::OpAdd(testfilepos()),
            Token::IntegerValue(testfilepos(), 2),
            Token::OpSub(testfilepos()),
            Token::IntegerValue(testfilepos(), 3),
            Token::OpAdd(testfilepos()),
            Token::IntegerValue(testfilepos(), 1),
            Token::ParensClose(testfilepos()),
            Token::EndStatement(testfilepos()),
            Token::Eob
        ];

        // above is
        // (1 + 1 + 2 * 3 - 1);

        let mut parser = AstParser::new(&tokens);

        let ast = parser.start_parse();

        if ast.is_err() {
            eprintln!("{:?}", ast.err().unwrap().get_message());
            panic!();
        }

        let ast = ast.ok().unwrap();
        let mut statements = ast.entrypoint.statements.clone();

        assert_eq!(1, statements.len());

        let stmt = statements.pop().unwrap();

        // this will get messy
        let expected_expr = Box::new(Expression {
            file_position: testfilepos(),
            kind: ExpressionKind::BinaryOperation(
                Box::new(Expression {
                    file_position: testfilepos(),
                    kind: ExpressionKind::LiteralValue(Value::int(1))
                }),
                BinOp {
                    kind: BinOpKind::Add,
                    is_left_assoc: true,
                    file_position: testfilepos()
                },
                Box::new(Expression {
                    file_position: testfilepos(),
                    kind: ExpressionKind::BinaryOperation(
                        Box::new(Expression {
                            file_position: testfilepos(),
                            kind: ExpressionKind::LiteralValue(Value::int(1))
                        }),
                        BinOp {
                            kind: BinOpKind::Add,
                            is_left_assoc: true,
                            file_position: testfilepos()
                        },
                        Box::new(Expression {
                            file_position: testfilepos(),
                            kind: ExpressionKind::BinaryOperation(
                                Box::new(Expression {
                                    file_position: testfilepos(),
                                    kind: ExpressionKind::BinaryOperation(
                                        Box::new(Expression {
                                            file_position: testfilepos(),
                                            kind: ExpressionKind::LiteralValue(Value::int(2))
                                        }),
                                        BinOp {
                                            kind: BinOpKind::Sub,
                                            is_left_assoc: true,
                                            file_position: testfilepos()
                                        },
                                        Box::new(Expression {
                                            file_position: testfilepos(),
                                            kind: ExpressionKind::LiteralValue(Value::int(3))
                                        }),
                                    )
                                }),
                                BinOp {
                                    file_position: testfilepos(),
                                    is_left_assoc: true,
                                    kind: BinOpKind::Add
                                },
                                Box::new(Expression {
                                    file_position: testfilepos(),
                                    kind: ExpressionKind::LiteralValue(Value::int(1))
                                })
                            )
                        })
                    )
                })
            )
        });

        match stmt.kind {
            StatementKind::Expression(expr) => {
                assert_eq!(expected_expr, expr);
            },
            _ => panic!("Unexpected statement kind, expected an expression statement")
        };
    }
}