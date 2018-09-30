mod utils;
pub mod stdlib;

pub fn register_core() {
    stdlib::register_stdlib();
}