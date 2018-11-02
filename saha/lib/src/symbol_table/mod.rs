//! Saha Symbol Table
//!
//! The Saha symbol table is the main symbol and reference storage when running
//! a Saha application. It is global and under mutex restrictions.
//!
//! All declarations and globals are stored in the symbol table.

use std::{
    collections::HashMap,
    sync::{Arc, Mutex}
};

use uuid::Uuid;

use crate::{
    errors::{Error, ParseError, RuntimeError},
    source::files::FilePosition,
    types::{
        Value,
        functions::{SahaCallable, SahaFunctionArguments},
        objects::{SahaObject, ClassDefinition, BehaviorDefinition}
    }
};

/// UUID as bytes
pub type InstRef = [u8; 16];

/// Symbol table, stores global parsed declarations and definitions, in addition
/// to references to things that should be available globally.
pub struct SymbolTable {
    /// Constants are global and static values which are defined and loaded
    /// before any application logic execution begins. Constants are defined
    /// with the `const` keyword and their names should contains only uppercase
    /// letters and underscores.
    pub constants: HashMap<String, Value>,

    /// Functions are top-level function declarations defined with the
    /// `function` keyword.
    pub functions: HashMap<String, Box<dyn SahaCallable>>,

    /// Behaviors are top-level behavior declarations. Behaviors are often
    /// called _interfaces_ in other languages.
    ///
    /// Behaviors define a public API which the implementors of each behavior
    /// must implement exactly as defined.
    pub behaviors: HashMap<String, BehaviorDefinition>,

    /// Class declarations. Each class has a name, properties, behavior
    /// implementations, and methods. Class methods can be private or public,
    /// instance or static.
    ///
    /// Methods are stored into a separate symbol table collection to separate
    /// instances (data) and logic that modifies instances.
    pub classes: HashMap<String, ClassDefinition>,

    /// Class methods. These are the same as functions, but the naming
    /// convention goes as such:
    ///
    /// ```txt
    /// fully.qualified.className#methodName
    /// ```
    ///
    /// Where the `#` separates the class name to which the method is tied to,
    /// and the method name which is defined for the method.
    ///
    /// Method callable itself stores information on whether the method is
    /// public or private, and whether it is instanced or static. Static methods
    /// receive no `self` parameter.
    pub methods: HashMap<String, Arc<Box<dyn SahaCallable>>>,

    /// Class instances (data) are stored here. They are behind an Arc and a
    /// Mutex to keep things consistent in case multiple points of an
    /// application want to modify or interact with the same instance at the
    /// same time.
    ///
    /// Instances contain only the data of a class instance, methods are stored
    /// centrally in another HashMap.
    pub instances: HashMap<InstRef, Arc<Mutex<Box<dyn SahaObject>>>>,
}

impl SymbolTable {
    /// Return a new and empty symbol table.
    pub fn new() -> SymbolTable {
        return SymbolTable {
            constants: HashMap::new(),
            functions: HashMap::new(),
            behaviors: HashMap::new(),
            classes: HashMap::new(),
            methods: HashMap::new(),
            instances: HashMap::new(),
        };
    }

    /// Set the symbol table constants collection.
    pub fn set_constants(&mut self, constants: HashMap<String, Value>) {
        self.constants = constants;
    }

    /// Add a new function/callable.
    pub fn add_function(&mut self, func: Box<dyn SahaCallable>) {
        let fn_name = func.get_name().clone();

        // FIXME prevent overrides
        self.functions.insert(fn_name, func);
    }

    /// Add a new method.
    pub fn add_method(&mut self, class_name: &String, method: &Box<dyn SahaCallable>) {
        let method_name = method.get_name().clone();
        let fq_method_name = format!("{}#{}", class_name, method_name);

        self.methods.insert(fq_method_name, Arc::new(method.clone()));
    }

    /// Insert a new object instance to the symbol table, and then return the
    /// instref value.
    pub fn create_object_instance(&mut self, class_name: String, args: SahaFunctionArguments, create_pos: &Option<FilePosition>) -> Result<Value, RuntimeError> {
        let def: Option<&ClassDefinition> = self.classes.get(&class_name);

        if def.is_none() {
            let err = RuntimeError::new(&format!("Cannot create instance of unknown class `{}`", class_name), create_pos.to_owned());

            return Err(err.with_type("TypeError"));
        }

        let def = def.unwrap();

        let instref = Self::get_new_uuid_bytes();

        let inst: Box<dyn SahaObject> = def.create_new_instance(instref, args, HashMap::new(), create_pos)?;

        self.instances.insert(instref, Arc::new(Mutex::new(inst)));

        return Ok(Value::obj(instref));
    }

    /// Get a new random UUID types type instance reference.
    fn get_new_uuid_bytes() -> InstRef {
        Uuid::new_v4().as_bytes().to_owned()
    }
}
