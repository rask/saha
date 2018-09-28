//! Saha object types

use std::collections::HashMap;

use crate::{
    symbol_table::InstRef,
    types::{
        Value, SahaType,
        functions::{SahaCallable, SahaCallResult}
    }
};

/// Instances of classes in saha, either core or userland.
pub trait SahaObject: Send {
    /// Get the instance reference for this object.
    fn get_instance_ref(&self) -> InstRef;

    /// Get the class name for the object without module path.
    fn get_class_name(&self) -> String;

    /// The the fully qualified class name with namespace/module path.
    fn get_fully_qualified_class_name(&self) -> String;

    /// Get a list of behavior names that this object implements.
    fn get_implements(&self) -> Vec<String>;

    /// Call an object member, e.g. a method. We denote whether this is a static call, and we pass
    /// in an optional instance reference of the calling context (which is used to determine if this
    /// is a `self` call and allow access to private members).
    fn call_member(&mut self, static_access: bool, accessor_instref: Option<InstRef>) -> SahaCallResult;

    /// Access (get) a member property. Determine static access and `self` similarly to
    /// `call_member()`.
    fn access_property(&self, static_access: bool, accessor_instref: Option<InstRef>) -> SahaCallResult;

    /// Mutate (set) a member property. Determine static access and `self` similarly to
    /// `call_member()`.
    fn mutate_property(&mut self, static_access: bool, accessor_instref: Option<InstRef>) -> SahaCallResult;
}

pub struct Property {
    pub name: String,
    pub prop_type: SahaType,
    pub default: Value,
    pub is_static: bool,
    pub visibility: MemberVisibility
}

/// A class definition, or a blueprint in other words. From these actual
/// class object instances are created.
pub struct ClassDefinition {
    name: String,
    fqname: String,
    properties: HashMap<String, Property>,
    methods: HashMap<String, Box<SahaCallable>>,
    implements: Vec<String>
}

/// Class member visibility, e.g. public or private.
#[derive(Clone, Debug, PartialEq)]
pub enum MemberVisibility {
    Public,
    Private
}