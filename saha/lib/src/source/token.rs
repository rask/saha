//! Token
//!
//! Token is a language specific string of characters that are used to parse
//! together an abstract syntax tree. Tokens are parsed from lexemes.

use noisy_float::prelude::*;

use std::{
    path::PathBuf,
    fmt::{Display, Result as FmtResult, Formatter}
};

use crate::source::{
    files::FilePosition,
    import::Import
};

/// A single tokenized piece of Saha source code.
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    /// A name reference value. E.g. function names, variable names, class
    /// names, property names, etc. Contains an aliased version of said name,
    /// along with a pure source representation of said name as well.
    Name(FilePosition, String, String),
    //                 ^ alias ^ source repr

    /// `=` character.
    Assign(FilePosition),

    /// `->` sequence. Marks object instance access.
    ObjectAccess(FilePosition),

    /// `::` sequence. Marks object static access.
    StaticAccess(FilePosition),

    /// `,` character.
    Comma(FilePosition),

    /// `:` character.
    Colon(FilePosition),

    /// `?` character.
    QuestionMark(FilePosition),

    /// '&' character.
    Ampersand(FilePosition),

    /// '|' character.
    Pipe(FilePosition),

    /// `'` character.
    SingleQuote(FilePosition),

    /// `;` character.
    EndStatement(FilePosition),

    /// `(` character.
    ParensOpen(FilePosition),

    /// `)` character.
    ParensClose(FilePosition),

    /// `[` character.
    BraceOpen(FilePosition),

    /// `]` character.
    BraceClose(FilePosition),

    /// `{` character.
    CurlyOpen(FilePosition),

    /// `}` character.
    CurlyClose(FilePosition),

    /// Import with `use` keyword, denotes imports from other codebases.
    Import(FilePosition, Import),

    /// End of file.
    Eof(FilePosition),

    /// This is a magic token, "end of body" and is used to denote function body
    /// endings for instance.
    Eob,

    // LITERAL VALUES

    /// A literal string value which was quoted with `"` characters in source
    /// code.
    StringValue(FilePosition, String),

    /// Literal integer value. Internally uses `isize` to make use of most int
    /// space on each platform.
    IntegerValue(FilePosition, isize),

    /// Literal float value. Internally uses noisy_float implementation to cut
    /// some corners when it comes to representing floats properly.
    FloatValue(FilePosition, R64),

    /// Literal boolean value, meaning either `true` or `false` in source code.
    BooleanValue(FilePosition, bool),

    // TYPE DECLARATIONS

    /// `str` declaration.
    TypeString(FilePosition),

    /// `int` declaration.
    TypeInteger(FilePosition),

    /// `float` declaration.
    TypeFloat(FilePosition),

    /// `bool` declaration.
    TypeBoolean(FilePosition),

    // OPERATORS

    /// `!`
    UnOpNot(FilePosition),

    /// `+` character.
    OpAdd(FilePosition),

    /// `-` character.
    OpSub(FilePosition),

    /// `/` character.
    OpDiv(FilePosition),

    /// `*` character.
    OpMul(FilePosition),

    /// `==` characters.
    OpEq(FilePosition),

    /// `!=` characters.
    OpNeq(FilePosition),

    /// `>` character.
    OpGt(FilePosition),

    /// `>=` characters.
    OpGte(FilePosition),

    /// `<` character.
    OpLt(FilePosition),

    /// `<=` characters.
    OpLte(FilePosition),

    /// `&&` characters.
    OpAnd(FilePosition),

    /// `||` characters.
    OpOr(FilePosition),

    /// `|>` characters.
    OpPipe(FilePosition),

    // KEYWORDS

    /// `use` keyword.
    KwUse(FilePosition),

    /// `as` keyword.
    KwAs(FilePosition),

    /// `class` keyword.
    KwClass(FilePosition),

    /// `behavior` keyword.
    KwBehavior(FilePosition),

    /// `var` keyword.
    KwVar(FilePosition),

    /// `prop` keyword.
    KwProperty(FilePosition),

    /// `const` keyword.
    KwConstant(FilePosition),

    /// `static` keyword.
    KwStatic(FilePosition),

    /// `return` keyword.
    KwReturn(FilePosition),

    /// `new` keyword.
    KwNew(FilePosition),

    /// `for` keyword.
    KwFor(FilePosition),

    /// `in` keyword.
    KwIn(FilePosition),

    /// `loop` keyword.
    KwLoop(FilePosition),

    /// `if` keyword.
    KwIf(FilePosition),

    /// `elseif` keyword.
    KwElseif(FilePosition),

    /// `else` keyword.
    KwElse(FilePosition),

    /// `implements` keyword.
    KwImplements(FilePosition),

    /// `function` keyword.
    KwFunction(FilePosition),

    /// `method` keyword.
    KwMethod(FilePosition),

    /// `public` keyword.
    KwPublic(FilePosition),

    /// `continue` keyword.
    KwContinue(FilePosition),

    /// `break` keyword.
    KwBreak(FilePosition),
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        let variant: String = match self {
            Token::StringValue(_, _) => "Literal string".to_string(),
            Token::IntegerValue(_, _) => "Literal integer".to_string(),
            Token::FloatValue(_, _) => "Literal float".to_string(),
            Token::BooleanValue(_, b) => format!("Boolean [{:?}]", b),
            Token::Name(_, _, orig) => format!("Name [{:?}]", orig),
            Token::ObjectAccess(_) => "Object access".to_string(),
            Token::StaticAccess(_) => "Static access".to_string(),
            Token::Comma(_) => "Comma".to_string(),
            Token::Colon(_) => "Colon".to_string(),
            Token::QuestionMark(_) => "Question mark".to_string(),
            Token::SingleQuote(_) => "Single quote".to_string(),
            Token::Ampersand(_) => "Ampersand [&]".to_string(),
            Token::Pipe(_) => "Pipe [|]".to_string(),
            Token::EndStatement(_) => "Statement end [;]".to_string(),
            Token::ParensOpen(_) => "Parenthesis open".to_string(),
            Token::ParensClose(_) => "Parenthesis close".to_string(),
            Token::BraceOpen(_) => "Brace open".to_string(),
            Token::BraceClose(_) => "Brace close".to_string(),
            Token::CurlyOpen(_) => "Curly brace open".to_string(),
            Token::CurlyClose(_) => "Curly brace close".to_string(),
            Token::Import(_, _) => "Import definition".to_string(),
            Token::Assign(_) => "Assignment".to_string(),
            Token::Eof(_) => "EOF".to_string(),
            Token::Eob => "EOB".to_string(),

            Token::TypeString(_) => "Type declaration [string]".to_string(),
            Token::TypeInteger(_) => "Type declaration [integer]".to_string(),
            Token::TypeFloat(_) => "Type declaration [float]".to_string(),
            Token::TypeBoolean(_) => "Type declaration [boolean]".to_string(),

            Token::UnOpNot(_) => "Operator [!]".to_string(),
            Token::OpAdd(_) => "Operator [+]".to_string(),
            Token::OpSub(_) => "Operator [-]".to_string(),
            Token::OpDiv(_) => "Operator [/]".to_string(),
            Token::OpMul(_) => "Operator [*]".to_string(),
            Token::OpEq(_) => "Operator [==]".to_string(),
            Token::OpNeq(_) => "Operator [!=]".to_string(),
            Token::OpGt(_) => "Operator [>]".to_string(),
            Token::OpGte(_) => "Operator [>=]".to_string(),
            Token::OpLt(_) => "Operator [<]".to_string(),
            Token::OpLte(_) => "Operator [<=]".to_string(),
            Token::OpAnd(_) => "Operator [&&]".to_string(),
            Token::OpOr(_) => "Operator [||]".to_string(),
            Token::OpPipe(_) => "Operator [|>]".to_string(),

            Token::KwUse(_) => "Keyword [use]".to_string(),
            Token::KwAs(_) => "Keyword [as]".to_string(),
            Token::KwClass(_) => "Keyword [class]".to_string(),
            Token::KwBehavior(_) => "Keyword [behavior]".to_string(),
            Token::KwVar(_) => "Keyword [var]".to_string(),
            Token::KwProperty(_) => "Keyword [property]".to_string(),
            Token::KwConstant(_) => "Keyword [constant]".to_string(),
            Token::KwReturn(_) => "Keyword [return]".to_string(),
            Token::KwStatic(_) => "Keyword [static]".to_string(),
            Token::KwNew(_) => "Keyword [new]".to_string(),
            Token::KwFor(_) => "Keyword [for]".to_string(),
            Token::KwIn(_) => "Keyword [in]".to_string(),
            Token::KwLoop(_) => "Keyword [loop]".to_string(),
            Token::KwIf(_) => "Keyword [if]".to_string(),
            Token::KwElseif(_) => "Keyword [elseif]".to_string(),
            Token::KwElse(_) => "Keyword [else]".to_string(),
            Token::KwImplements(_) => "Keyword [implements]".to_string(),
            Token::KwFunction(_) => "Keyword [function]".to_string(),
            Token::KwMethod(_) => "Keyword [method]".to_string(),
            Token::KwPublic(_) => "Keyword [public]".to_string(),
            Token::KwBreak(_) => "Keyword [break]".to_string(),
            Token::KwContinue(_) => "Keyword [continue]".to_string(),
        };

        write!(f, "{:?}", variant)
    }
}

impl Token {
    /// Get operator precendence. Returns 0 for least priority, and negative value for non-ops.
    ///
    /// This function can also be used as a cheap hack to see if a token is in fact a binary
    /// operator.
    ///
    /// In precedence, expressions should be evaluated starting with the highest precedence. This
    /// means all "math" is done before comparisons, and comparisons precede "true/false" checks.
    pub fn get_precedence(&self) -> i8 {
        return match self {
            Token::OpAnd(..) | Token::OpOr(..) => 0,
            Token::OpEq(..) | Token::OpNeq(..) | Token::OpGt(..)
            | Token::OpGte(..) | Token::OpLt(..) | Token::OpLte(..) => 1,
            Token::OpAdd(..) | Token::OpSub(..) => 2,
            Token::OpDiv(..) | Token::OpMul(..) => 3,
            _ => -1
        };
    }

    /// Validate whether an operator is left or right associative. Returns error
    /// in case the token is not a binary operator.
    pub fn is_left_associative(&self) -> Result<bool, ()> {
        if self.get_precedence() < 0 {
            return Err(());
        }

        return Ok(true);
    }

    /// Get the source file position of a token.
    pub fn get_file_position(&self) -> FilePosition {
        return match self {
            Token::Name(f, ..) => f.clone(),
            Token::Assign(f, ..) => f.clone(),
            Token::ObjectAccess(f, ..) => f.clone(),
            Token::StaticAccess(f, ..) => f.clone(),
            Token::Comma(f, ..) => f.clone(),
            Token::Colon(f, ..) => f.clone(),
            Token::QuestionMark(f, ..) => f.clone(),
            Token::SingleQuote(f, ..) => f.clone(),
            Token::Ampersand(f, ..) => f.clone(),
            Token::Pipe(f, ..) => f.clone(),
            Token::EndStatement(f, ..) => f.clone(),
            Token::ParensOpen(f, ..) => f.clone(),
            Token::ParensClose(f, ..) => f.clone(),
            Token::BraceOpen(f, ..) => f.clone(),
            Token::BraceClose(f, ..) => f.clone(),
            Token::CurlyOpen(f, ..) => f.clone(),
            Token::CurlyClose(f, ..) => f.clone(),
            Token::Import(f, ..) => f.clone(),
            Token::Eof(f, ..) => f.clone(),
            Token::Eob => FilePosition::unknown(),
            Token::StringValue(f, ..) => f.clone(),
            Token::IntegerValue(f, ..) => f.clone(),
            Token::FloatValue(f, ..) => f.clone(),
            Token::BooleanValue(f, ..) => f.clone(),
            Token::TypeBoolean(f, ..) => f.clone(),
            Token::TypeFloat(f, ..) => f.clone(),
            Token::TypeInteger(f, ..) => f.clone(),
            Token::TypeString(f, ..) => f.clone(),
            Token::UnOpNot(f, ..) => f.clone(),
            Token::OpAdd(f, ..) => f.clone(),
            Token::OpSub(f, ..) => f.clone(),
            Token::OpDiv(f, ..) => f.clone(),
            Token::OpMul(f, ..) => f.clone(),
            Token::OpEq(f, ..) => f.clone(),
            Token::OpNeq(f, ..) => f.clone(),
            Token::OpGt(f, ..) => f.clone(),
            Token::OpGte(f, ..) => f.clone(),
            Token::OpLt(f, ..) => f.clone(),
            Token::OpLte(f, ..) => f.clone(),
            Token::OpAnd(f, ..) => f.clone(),
            Token::OpOr(f, ..) => f.clone(),
            Token::OpPipe(f, ..) => f.clone(),
            Token::KwUse(f, ..) => f.clone(),
            Token::KwAs(f, ..) => f.clone(),
            Token::KwClass(f, ..) => f.clone(),
            Token::KwBehavior(f, ..) => f.clone(),
            Token::KwVar(f, ..) => f.clone(),
            Token::KwProperty(f, ..) => f.clone(),
            Token::KwConstant(f, ..) => f.clone(),
            Token::KwStatic(f, ..) => f.clone(),
            Token::KwReturn(f, ..) => f.clone(),
            Token::KwNew(f, ..) => f.clone(),
            Token::KwFor(f, ..) => f.clone(),
            Token::KwIn(f, ..) => f.clone(),
            Token::KwLoop(f, ..) => f.clone(),
            Token::KwIf(f, ..) => f.clone(),
            Token::KwElseif(f, ..) => f.clone(),
            Token::KwElse(f, ..) => f.clone(),
            Token::KwImplements(f, ..) => f.clone(),
            Token::KwFunction(f, ..) => f.clone(),
            Token::KwMethod(f, ..) => f.clone(),
            Token::KwPublic(f, ..) => f.clone(),
            Token::KwContinue(f, ..) => f.clone(),
            Token::KwBreak(f, ..) => f.clone(),
        };
    }
}

/// Allows checking whether something contains file-based source imports.
pub trait ContainsImports {
    /// Check if a Token collection contains a file-based import. Return the
    /// file which can be imported or None.
    fn contains_file_imports(&self, ignore_list: &Vec<PathBuf>) -> (Option<PathBuf>, Option<PathBuf>, Option<String>);
}

impl ContainsImports for Vec<Token> {
    fn contains_file_imports(&self, ignore_list: &Vec<PathBuf>) -> (Option<PathBuf>, Option<PathBuf>, Option<String>) {
        for t in self {
            match t {
                Token::Import(_, import) => {
                    match import {
                        Import::Pkg(module, _, ref path) => {
                            let mut modparts: Vec<&str> = module.split('.').collect();
                            modparts.pop();
                            let modulepath = modparts.join(".");

                            if ignore_list.contains(&path) {
                                return (None, Some(path.to_owned()), None);
                            }

                            return (Some(path.to_owned()), None, Some(modulepath.to_owned()));
                        },
                        _ => return (None, None, None)
                    };
                },
                _ => return (None, None, None)
            };
        }

        return (None, None, None);
    }
}
