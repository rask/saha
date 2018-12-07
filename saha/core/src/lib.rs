//! Saha Core
//!
//! Contains the stdlib.

#![allow(clippy::needless_return, clippy::redundant_field_names)]

mod utils;
pub mod stdlib;

pub fn register_core() {
    stdlib::register_stdlib();
}