//! Saha Errors
//!
//! Defines the error types and related logic.
//!
//! Errors implementing the `Error` trait can and should be used in the language
//! interpreter and implementation to conform to the overall error system of
//! the language.

use std::fmt::Debug;

use crate::{
    source::files::FilePosition
};

/// Error trait
///
/// Implement this trait for all structs that are to be used as error types when
/// running Saha interpretation.
pub trait Error: Debug {
    /// Create a new error instance with a message text.
    fn new(message: &str, pos: Option<FilePosition>) -> Self;

    /// Get a short description of the error.
    fn get_message(&self) -> String;

    /// Get a defined name for the error (e.g. `RuntimeError`).
    fn get_name(&self) -> String;

    /// Get the file position where this error occured in the source code.
    fn get_file_position(&self) -> Option<FilePosition>;

    /// Return a human-friendly formatted version of this error.
    fn format(&self) -> String {
        if self.get_file_position().is_none() {
            return format!("Unexpected `{}`\n    {}", self.get_name(), self.get_message());
        }

        let pos = self.get_file_position().unwrap();

        return format!(
            "Unexpected `{}`\nat {:?}\n{}",
            self.get_name(),
            pos,
            self.get_message()
        );
    }
}

/// For parsing errors which may happen either during tokenization or during AST
/// parsing.
#[derive(Debug)]
pub struct ParseError {
    message: String,
    file_position: Option<FilePosition>
}

impl Error for ParseError {
    fn new(message: &str, pos: Option<FilePosition>) -> Self {
        return ParseError {
            message: message.to_owned(),
            file_position: pos.clone()
        };
    }

    fn get_message(&self) -> String {
        return self.message.to_owned();
    }

    fn get_name(&self) -> String {
        return "ParseError".to_string();
    }

    fn get_file_position(&self) -> Option<FilePosition> {
        return self.file_position.to_owned();
    }
}

/// Runtime errors are all the errors that can appear when the application is
/// being interpreted, and these contain the userland and core defined specific
/// errors such as `TypeError` and others.
#[derive(Debug)]
pub struct RuntimeError {
    message: String,
    file_position: Option<FilePosition>,
    saha_error_type: String,
}

impl Error for RuntimeError {
    fn new(message: &str, pos: Option<FilePosition>) -> Self {
        return RuntimeError {
            message: message.to_owned(),
            file_position: pos.clone(),
            saha_error_type: "RuntimeError".to_string(),
        };
    }

    fn get_message(&self) -> String {
        return self.message.to_owned();
    }

    fn get_name(&self) -> String {
        return self.saha_error_type.to_owned();
    }

    fn get_file_position(&self) -> Option<FilePosition> {
        return self.file_position.to_owned();
    }
}
