//! Saha Errors
//!
//! Defines the error types and related logic.
//!
//! Errors implementing the `Error` trait can and should be used in the language
//! interpreter and implementation to conform to the overall error system of
//! the language.

/// Error trait
///
/// Implement this trait for all structs that are to be used as error types when
/// running Saha interpretation.
pub trait Error {
    /// Create a new error instance with a message text.
    fn new(message: &str) -> Self;

    /// Get a short description of the error.
    fn get_message(&self) -> String;

    /// Get a defined name for the error (e.g. `RuntimeError`).
    fn get_name(&self) -> String;
}
