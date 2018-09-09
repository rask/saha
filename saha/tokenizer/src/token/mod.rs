//! Token
//!
//! Token is a language specific string of characters that are used to parse
//! together an abstract syntax tree. Tokens are parsed from lexemes.

use noisy_float::prelude::*;
use saha_lib::source::FilePosition;

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

    /// `!` character.
    Negation(FilePosition),

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

    /// Import with `use` keyword, denotes imports from other codebases. First
    /// String is the actual module and member, second string is an alias
    /// declared for the import using the `as` keyword.
    Import(FilePosition, String, String),

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

    /// `list` declaration.
    TypeList(FilePosition),

    /// `dict` declaration.
    TypeDictionary(FilePosition),

    // OPERATORS

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

    /// `try` keyword.
    KwTry(FilePosition),

    /// `catch` keyword.
    KwCatch(FilePosition),

    /// `raise` keyword.
    KwRaise(FilePosition),
}

impl Token {
    /// Get the source file position of a token.
    pub fn get_file_position(&self) -> FilePosition {
        return match self {
            Token::Name(f, ..) => f.clone(),
            Token::Assign(f, ..) => f.clone(),
            Token::ObjectAccess(f, ..) => f.clone(),
            Token::StaticAccess(f, ..) => f.clone(),
            Token::Comma(f, ..) => f.clone(),
            Token::Colon(f, ..) => f.clone(),
            Token::Negation(f, ..) => f.clone(),
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
            Token::TypeDictionary(f, ..) => f.clone(),
            Token::TypeFloat(f, ..) => f.clone(),
            Token::TypeInteger(f, ..) => f.clone(),
            Token::TypeList(f, ..) => f.clone(),
            Token::TypeString(f, ..) => f.clone(),
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
            Token::KwTry(f, ..) => f.clone(),
            Token::KwCatch(f, ..) => f.clone(),
            Token::KwRaise(f, ..) => f.clone(),
        };
    }
}