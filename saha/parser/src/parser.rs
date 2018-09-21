use noisy_float::prelude::*;

use std::{
    mem::{discriminant, Discriminant},
};

use saha_lib::{
    source::FilePosition,
    errors::{Error, ParseError}
};

use saha_tokenizer::{
    token::Token,
    imports::Import,
};

/// Parse result, either something or a parse error.
pub type PR<T> = Result<T, ParseError>;

pub type TokenType = Discriminant<Token>;

/// Anything that parses tokens from a vector of tokens.
pub trait ParsesTokens {
    /// Consume a token of wanted Token enum variant.
    fn consume_next(&mut self, next_variants: Vec<&str>) -> PR<()>;

    /// Consume any token.
    fn consume_any(&mut self) -> PR<()>;

    /// Return a parse error for an unexpected token.
    fn unexpected(&mut self, token: &Token, expected: Vec<&str>) -> PR<()> {
        return Err(ParseError::new(
            &format!("Unexpected token `{}`, expected one of `{:?}`", token, expected),
            Some(token.get_file_position())
        ));
    }

    /// Get a dummy token type for comparison purposes.
    fn get_dummy_token_type(&self, ttype: &str) -> TokenType {
        fn _f() -> FilePosition {
            return FilePosition::unknown();
        }

        fn _s() -> String {
            "".to_string()
        }

        let dummy_import = Import::Std("asdf.asdf".to_string(), "asdf".to_string());

        let ttok = match ttype {
            "import" => Token::Import(_f(), dummy_import),
            "name" => Token::Name(_f(), _s(), _s()),
            "curlyopen" | "{" => Token::CurlyOpen(_f()),
            "curlyclose" | "}" => Token::CurlyClose(_f()),
            "parensopen" | "(" => Token::ParensOpen(_f()),
            "parensclose" | ")" => Token::ParensClose(_f()),
            "braceopen" | "[" => Token::BraceOpen(_f()),
            "braceclose" | "]" => Token::BraceClose(_f()),
            "comma" | "," => Token::Comma(_f()),
            "colon" | ":" => Token::Colon(_f()),
            "endstatement" | ";" => Token::EndStatement(_f()),
            "assign" | "=" => Token::Assign(_f()),
            "objectaccess" | "->" => Token::ObjectAccess(_f()),
            "staticaccess" | "::" => Token::StaticAccess(_f()),
            "squote" | "'" => Token::SingleQuote(_f()),

            // values
            "stringval" => Token::StringValue(_f(), _s()),
            "integerval" => Token::IntegerValue(_f(), 1),
            "floatval" => Token::FloatValue(_f(), r64(0.0)),
            "booleanval" => Token::BooleanValue(_f(), true),

            // types
            "typestring" | "str" => Token::TypeString(_f()),
            "typeinteger" | "int" => Token::TypeInteger(_f()),
            "typefloat" | "float" => Token::TypeFloat(_f()),
            "typeboolean" | "bool" => Token::TypeBoolean(_f()),

            // ops
            "+" => Token::OpAdd(_f()),
            "-" => Token::OpSub(_f()),
            "*" => Token::OpMul(_f()),
            "/" => Token::OpDiv(_f()),
            ">" => Token::OpGt(_f()),
            "<" => Token::OpLt(_f()),
            ">=" => Token::OpGte(_f()),
            "<=" => Token::OpLte(_f()),
            "&&" => Token::OpAnd(_f()),
            "||" => Token::OpOr(_f()),
            "==" => Token::OpEq(_f()),
            "!=" => Token::OpNeq(_f()),

            // kw
            "var" => Token::KwVar(_f()),
            "as" => Token::KwAs(_f()),
            "class" => Token::KwClass(_f()),
            "behavior" => Token::KwBehavior(_f()),
            "use" => Token::KwUse(_f()),
            "prop" => Token::KwProperty(_f()),
            "const" => Token::KwConstant(_f()),
            "static" => Token::KwStatic(_f()),
            "return" => Token::KwReturn(_f()),
            "new" => Token::KwNew(_f()),
            "for" => Token::KwFor(_f()),
            "in" => Token::KwIn(_f()),
            "if" => Token::KwIf(_f()),
            "elseif" => Token::KwElseif(_f()),
            "else" => Token::KwElse(_f()),
            "implements" => Token::KwImplements(_f()),
            "function" => Token::KwFunction(_f()),
            "method" => Token::KwMethod(_f()),
            "pub" => Token::KwPublic(_f()),
            "break" => Token::KwBreak(_f()),
            "continue" => Token::KwContinue(_f()),
            "try" => Token::KwTry(_f()),
            "catch" => Token::KwCatch(_f()),
            "raise" => Token::KwRaise(_f()),
            "eob" => Token::Eob,
            _ => Token::Eof(_f())
        };

        return discriminant(&ttok);
    }
}
