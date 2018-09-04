//! Token
//! 
//! Token is a language specific string of characters that are used to parse
//! together an abstract syntax tree. Tokens are parsed from lexemes.

use noisy_float::prelude::*;
use saha_lib::source::FilePosition;

/// A single tokenized piece of Saha source code.
pub enum Token {
    /// An inline comment.
    Comment(FilePosition, String),

    /// A name reference value. E.g. function names, variable names, class
    /// names, property names, etc. Contains and aliased version of said name,
    /// along with a pure source representation of said name as well.
    Name(FilePosition, String, String),

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
    
    /// Any unspecified character that does not fit anywhere else. Most probably
    /// leads to a parse error at some point during execution.
    Symbol(FilePosition, char),

    /// `\n`
    Newline(FilePosition),

    /// Any horizontal whitespace.
    Whitespace(FilePosition, String),

    /// Import with `use` keyword, denotes imports from other codebases. First
    /// String is the actual module and member, second string is an alias
    /// declared for the import using the `as` keyword.
    Use(FilePosition, String, String),

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