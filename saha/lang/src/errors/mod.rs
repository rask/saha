//! Interpreter error handling
//!
//! This module defined the root interpreter error handling setup, and provides
//! a few error types related to startup errors and similar.

use saha_lib::{
    source::FilePosition,
    errors::Error
};

pub struct StartupError {
    message: String
}

impl Error for StartupError {
    fn new(message: &str, _pos: Option<FilePosition>) -> Self {
        return StartupError {
            message: message.to_owned(),
        };
    }

    fn get_message(&self) -> String {
        return self.message.to_string();
    }

    fn get_name(&self) -> String {
        return "StartupError".to_owned();
    }

    fn get_file_position(&self) -> Option<FilePosition> {
        return None;
    }
}

pub type StartupResult = Result<(), StartupError>;
