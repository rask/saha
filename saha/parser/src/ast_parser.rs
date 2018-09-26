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
    ast::*,
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
                Token::KwReturn(..) => unimplemented!(),
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

    /// Parse an expression.
    fn parse_expression(&mut self, minimum_op_precedence: i8) -> PR<Box<Expression>> {
        let mut expression = self.parse_primary()?;

        let ntok: &Token = self.ntok.unwrap_or(&Token::Eob);
        let next_precedence = ntok.get_precedence();

        this is broken, precedence and op parsing fails, run tests to see

        if next_precedence < minimum_op_precedence {
            // non-operator or lesser precedence
            return Ok(expression);
        }

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

        expression = Box::new(Expression {
            file_position: expression.file_position.to_owned(),
            kind: ExpressionKind::BinaryOperation(expression, binop, rhs_expression)
        });

        return Ok(expression);
    }

    /// Primaries are building blocks for expressions. We could parse these in the
    /// `parse_expression` method, but separating concerns makes it simpler to consume. Also helps
    /// with operator precedence parsing.
    fn parse_primary(&mut self) -> PR<Box<Expression>> {
        self.consume_next(vec![
            "(", "[", "{", "new", "-", "!",
            "name", "stringval", "integerval", "floatval", "booleanval"
        ])?;

        let mut parens_close_expected = false;

        let expression: Box<Expression> = match self.ctok.unwrap() {
            Token::ParensOpen(..) => {
                parens_close_expected = true;
                self.parse_expression(0)?
            },
            Token::BraceOpen(..) => unimplemented!(),
            Token::CurlyOpen(..) => unimplemented!(),
            Token::UnOpNot(..) => unimplemented!(),
            Token::OpSub(..) => unimplemented!(),
            Token::StringValue(..)
            | Token::IntegerValue(..)
            | Token::FloatValue(..)
            | Token::BooleanValue(..) => self.parse_literal_value()?,
            Token::KwNew(..) => unimplemented!(),
            _ => unreachable!()
        };

        if parens_close_expected {
            self.consume_next(vec![")"])?;
        }

        return Ok(expression);
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
            kind: ExpressionKind::ParensExpression(
                Box::new(Expression {
                    file_position: testfilepos(),
                    kind: ExpressionKind::BinaryOperation(
                        Box::new(Expression {
                            file_position: testfilepos(),
                            kind: ExpressionKind::BinaryOperation(
                                Box::new(Expression {
                                    file_position: testfilepos(),
                                    kind: ExpressionKind::LiteralValue(Value::int(1))
                                }),
                                BinOp {
                                    kind: BinOpKind::Add,
                                    file_position: testfilepos()
                                },
                                Box::new(Expression {
                                    file_position: testfilepos(),
                                    kind: ExpressionKind::LiteralValue(Value::int(1))
                                }),
                            )
                        }),
                        BinOp {
                            kind: BinOpKind::Add,
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
}