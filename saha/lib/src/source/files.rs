//! Files
//!
//! Anything related to managing source code, e.g. token/lexeme positioning and
//! code file locations.

use std::{
    fmt::{
        Debug,
        Formatter as FmtFormatter,
        Result as FmtResult
    },
    path::PathBuf
};

/// Marks a specific position inside a source code file.
#[derive(Clone, PartialEq)]
pub struct FilePosition {
    pub path: PathBuf,
    pub line: usize,
    pub column: i32
}

impl FilePosition {
    /// Helper method for generating so called unknown file positions.
    ///
    /// This is used when we want to specify a file position for logic which has
    /// no file or file position. Often these are internal language errors that
    /// do not stem from userland errors.
    pub fn unknown() -> FilePosition {
        return FilePosition {
            path: PathBuf::from("/unknown"),
            line: 0,
            column: 0
        };
    }

    /// Shift the position's column value by an offset.
    pub fn shift_col(&self, shift: i32) -> FilePosition {
        return FilePosition {
            path: self.path.clone(),
            line: self.line.clone(),
            column: self.column + shift
        };
    }
}

impl Debug for FilePosition {
    fn fmt(&self, f: &mut FmtFormatter) -> FmtResult {
        let mut path_str = format!("{:?}", self.path);
        path_str.pop();
        path_str.remove(0);
        return write!(f, "{}:{}:{}", path_str, self.line, self.column);
    }
}