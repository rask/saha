//! Saha Core Library
//!
//! This library contains the building blocks for all things around the Saha
//! core and extensions.

#![crate_type = "rlib"]

extern crate uuid;
#[macro_use]
extern crate lazy_static;
extern crate noisy_float;

pub mod types;
pub mod errors;
pub mod source;
pub mod symbol_table;

use std::{
    sync::{Arc, Mutex}
};

use symbol_table::SymbolTable;

lazy_static! {
    pub static ref SAHA_SYMBOL_TABLE: Arc<Mutex<SymbolTable>> = Arc::new(Mutex::new(SymbolTable::new()));
}