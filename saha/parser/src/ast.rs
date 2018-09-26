//! Saha AST definition

use std::{
    fmt::{
        Debug,
        Formatter as FmtFormatter,
        Result as FmtResult
    },
    collections::HashMap
};

use saha_lib::{
    source::FilePosition,
    types::{Value, SahaType}
};

use saha_tokenizer::token::Token;

/// AST. Contains a visitable tree of AST nodes that make the program magic
/// happen. A single entrypoint in the form of a block is the first thing any
/// AST visitor should visit.
pub struct Ast {
    pub entrypoint: Box<Block>
}

/// Any block content, e.g. function body, if block body, loop block body, etc.
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub file_position: FilePosition,
    pub statements: Vec<Box<Statement>>
}

/// A statement, e.g. anything which is inside a block and ends in `;`.
#[derive(Debug, Clone, PartialEq)]
pub struct Statement {
    pub file_position: FilePosition,
    pub kind: StatementKind
}

/// Various kinds of statements.
#[derive(Debug, Clone, PartialEq)]
pub enum StatementKind {
    /// Variable name, variable type, value to assign
    ///
    /// ```saha
    /// var my_var'str = "hello";
    /// ```
    VarDeclaration(Identifier, SahaType, Option<Box<Expression>>),

    /// A one-off expression statement. Example:
    ///
    /// ```saha
    /// (new FooBar())->helloWorld();
    /// ```
    Expression(Box<Expression>),

    /// If-clause. First is the condition, then the `true` block, after is a possible elseif
    /// statements, finally and optional else block.
    ///
    /// ```saha
    /// if (something) {
    ///     //
    /// } elseif (something_else) {
    ///     //
    /// } else {
    ///     //
    /// }
    /// ```
    If(Box<Expression>, Box<Block>, Vec<Box<Statement>>, Option<Box<Block>>),

    /// Loop block.
    ///
    /// ```saha
    /// loop {
    ///     //
    /// }
    /// ```
    Loop(Box<Block>),

    /// For block, first two are `k` and `v` of loop, followed with the iterable thing expression,
    /// and last is the block which is looped over.
    ///
    /// ```saha
    /// for (k, v) in my_list {
    ///     //
    /// }
    /// ```
    For(Identifier, Identifier, Box<Expression>, Box<Block>),

    /// Return statement.
    ///
    /// ```saha
    /// return foobar;
    /// ```
    Return(Box<Expression>),

    /// Try statements. First is the try block, then is a collection of catch
    /// blocks. Lastly there is a `finally` block which is optional.
    ///
    /// ```saha
    /// try {
    ///     //
    /// }
    /// ```
    Try(Box<Block>, Vec<Statement>, Option<Box<Block>>),

    /// Catch statements. Must be after a Try statement in source. Contains two
    /// identifiers: one for the error type, then the variable to use inside
    /// the block for the caught expection/error.
    ///
    /// ```saha
    /// ...
    /// } catch (Something e) {
    ///     //
    /// }
    /// ```
    Catch(Identifier, Identifier, Box<Block>),

    /// Break statement. Used in loops.
    Break(FilePosition),

    /// Continue statement. Used in loops.
    Continue(FilePosition),
}

/// Identifiers, e.g. var names.
#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub file_position: FilePosition,
    pub identifier: String,
}

/// Expressions.
#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub file_position: FilePosition,
    pub kind: ExpressionKind,
}

/// Stack of expressions, used for operator precedence parsing.
pub struct ExpressionStack {
    expressions: Vec<Box<Expression>>
}

/// Expression kinds.
#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionKind {
    /// Expression wrapped in parentheses.
    ParensExpression(Box<Expression>),

    /// Literal values in source.
    LiteralValue(Value),

    /// A single identifier, e.g. a variable name.
    Ident(Identifier),

    /// List declaration.
    ///
    /// ```saha
    /// [value, value, value, ...]
    /// ```
    ListDeclaration(Vec<Box<Expression>>),

    /// Dictionary declarations.
    ///
    /// ```saha
    /// {key: value, ...}
    /// ```
    DictionaryDeclaration(Vec<(Box<Expression>, Box<Expression>)>),

    /// Assignment. `name = expr`.
    AssignOperation(Box<Expression>, Box<Expression>),

    /// Pipe operation, similar to shell piping.
    ///
    /// ```saha
    /// variable = call_stuff() |> call_other_stuff();
    /// ```
    ///
    /// With piping we take the return value of left hand side and pass it as
    /// a arg to the right hand side. Requires that the functions return and
    /// accept correct types. The receiving function should accept only a single
    /// parameter, the name of which will be inferred.
    PipeOperation(Box<Expression>, Box<Expression>),

    /// Binary op. left -> op -> right, `expr + expr`, `expr * expr`.
    BinaryOperation(Box<Expression>, BinOp, Box<Expression>),

    /// Unary op. `! value`, `- expr`.
    UnaryOperation(UnaryOp, Box<Expression>),

    /// Function or method call. First is the function we're calling. Second
    /// are the args.
    FunctionCall(Box<Expression>, Box<Expression>),

    /// Collection of callable args.
    CallableArgs(Vec<Box<Expression>>),

    /// A single callable argument. Contains arg name and the value assigned to it.
    CallableArg(Identifier, Box<Expression>),

    /// Object access, first is the type, then the object we're accessing, then
    /// the property/method we're accessing. We can nest access expressions by
    /// inserting further object access expressions into the first field.
    ObjectAccess(Box<Expression>, AccessKind, Box<Expression>),

    /// Newup a class. First is the class name, second is the constructor args,
    /// which are alike function call args.
    NewInstance(Identifier, Box<Expression>)
}

/// Binary operation.
#[derive(Clone, PartialEq)]
pub struct BinOp {
    pub file_position: FilePosition,
    pub kind: BinOpKind,
    pub is_left_assoc: bool
}

impl BinOp {
    /// Create a BinOp from a token instance.
    pub fn from_token(token: &Token) -> Result<BinOp, ()> {
        let fpos = token.get_file_position();

        let op_kind: BinOpKind = match token {
            Token::OpAdd(..) => BinOpKind::Add,
            Token::OpSub(..) => BinOpKind::Sub,
            Token::OpMul(..) => BinOpKind::Mul,
            Token::OpDiv(..) => BinOpKind::Div,
            Token::OpAnd(..) => BinOpKind::And,
            Token::OpOr(..) => BinOpKind::Or,
            Token::OpLt(..) => BinOpKind::Lt,
            Token::OpLte(..) => BinOpKind::Lte,
            Token::OpGt(..) => BinOpKind::Gt,
            Token::OpGte(..) => BinOpKind::Gte,
            _ => return Err(())
        };

        return Ok(BinOp {
            file_position: fpos,
            kind: op_kind,
            is_left_assoc: true
        });
    }
}

impl Debug for BinOp {
    fn fmt(&self, f: &mut FmtFormatter) -> FmtResult {
        return write!(f, "BinOp::{:?}", self.kind);
    }
}

/// Binary operation kind.
#[derive(Debug, Clone, PartialEq)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Gt,
    Gte,
    Lt,
    Lte,
    And,
    Or,
}

/// Unary operation.
#[derive(Debug, Clone, PartialEq)]
pub struct UnaryOp {
    file_position: FilePosition,
    kind: UnaryOpKind
}

/// Unary operation kind.
#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOpKind {
    /// `!expr`
    Not,

    /// `-expr`
    Minus
}

/// Object access kinds. `->` for instance, `::` for static.
#[derive(Debug, Clone, PartialEq)]
pub enum AccessKind {
    /// `->`
    Instance,

    /// `::`
    Static
}