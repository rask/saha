//! Saha object types

use std::collections::HashMap;

use ::types::functions::SahaCallResult;

/// Instances of classes in saha, either core or userland.
pub trait SahaObject: Send {
    /// Get the class name for the object without module path.
    fn get_class_name(&self) -> String;

    /// The the fully qualified class name with namespace/module path.
    fn get_fully_qualified_class_name(&self) -> String;

    /// Get a list of behavior names that this object implements.
    fn get_implements(&self) -> Vec<String>;

    /// Call an object member, e.g. a method.
    fn call_member(&mut self) -> SahaCallResult;

    /// Access (get) a member property.
    fn access_property(&self) -> SahaCallResult;

    /// Mutate (set) a member property.
    fn mutate_property(&mut self) -> SahaCallResult;
}

/// A class definition, or a blueprint in other words. From these actual
/// class object instances are created.
pub struct ClassDefinition {
    name: String,
    fqname: String,
    properties: HashMap<String, ()>,
    methods: HashMap<String, ()>,
    implements: Vec<String>
}