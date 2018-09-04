//! Interpreter error handling
//!
//! This module defined the root interpreter error handling setup, and provides
//! a few error types related to startup errors and similar.

use saha_lib::errors::Error;

pub struct StartupError {
    name: String,
    message: String
}

impl Error for StartupError {
    fn new(message: &str) -> Self {
        return StartupError {
            name: "StartupError".to_owned(),
            message: message.to_owned()
        };
    }

    fn get_message(&self) -> String {
        return self.message.to_string();
    }

    fn get_name(&self) -> String {
        return self.name.to_string();
    }
}

pub type StartupResult = Result<(), StartupError>;
