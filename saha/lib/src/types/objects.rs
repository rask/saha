//! Saha object types

use std::collections::HashMap;

use crate::{
    symbol_table::InstRef,
    errors::{Error, RuntimeError},
    source::files::FilePosition,
    types::{
        Value, SahaType,
        functions::{ValidatesArgs, SahaCallable, SahaFunctionArguments, SahaCallResult}
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
    fn call_member(&mut self, member: String, args: SahaFunctionArguments, static_access: bool, accessor_instref: Option<InstRef>) -> SahaCallResult;

    /// Access (get) a member property. Determine static access and `self` similarly to
    /// `call_member()`.
    fn access_property(&self, prop: String, static_access: bool, accessor_instref: Option<InstRef>) -> SahaCallResult;

    /// Mutate (set) a member property. Determine static access and `self` similarly to
    /// `call_member()`.
    fn mutate_property(&mut self, prop: String, static_access: bool, accessor_instref: Option<InstRef>) -> SahaCallResult;

    /// Clone for boxed self.
    fn box_clone(&self) -> Box<dyn SahaObject>;
}

impl Clone for Box<dyn SahaObject> {
    fn clone(&self) -> Box<dyn SahaObject> {
        return self.box_clone();
    }
}

/// A class property.
#[derive(Clone)]
pub struct Property {
    pub name: String,
    pub prop_type: SahaType,
    pub default: Value,
    pub is_static: bool,
    pub visibility: MemberVisibility
}

pub type ObjProperties = HashMap<String, Property>;

impl ValidatesArgs for ObjProperties {
    fn validate_args(&self, args: &SahaFunctionArguments, call_pos: &Option<FilePosition>) -> Result<(), RuntimeError> {
        if self.len() == 0 && args.len() == 0 {
            // no args expected, and none given, so we're ok
            return Ok(());
        }

        if args.len() > self.len() {
            let err = RuntimeError::new("Too many arguments provided", call_pos.to_owned());

            return Err(err.with_type("InvalidArgumentError"));
        }

        for (pname, p) in self {
            if p.default.kind == SahaType::Void {
                // no default
                if args.contains_key(pname) == false {
                    let err = RuntimeError::new(&format!("The `{}` argument is required", pname), call_pos.to_owned());

                    return Err(err.with_type("InvalidArgumentError"));
                }
            }

            let propk = &p.prop_type;
            let argk = &args.get(pname).unwrap().kind;

            if propk != argk {
                let err = RuntimeError::new(
                    &format!(
                        "Invalid argument `{}`, expected `{}` but received `{}`",
                        pname,
                        propk.to_readable_string(),
                        argk.to_readable_string()),
                    call_pos.to_owned()
                );
            }
        }

        return Ok(());
    }

    fn validate_single_param_args(&self, args: &SahaFunctionArguments, call_pos: &Option<FilePosition>) -> Result<(), RuntimeError> {
        return Ok(());
    }
}

/// A class definition, or a blueprint in other words. From these actual
/// class object instances are created.
pub struct ClassDefinition {
    pub name: String,
    pub fqname: String,
    pub properties: ObjProperties,
    pub methods: HashMap<String, Box<dyn SahaCallable>>,
    pub implements: Vec<String>
}

impl ClassDefinition {
    /// Create a new instance object from this definition blueprint.
    pub fn create_new_instance(&self, inst_ref: InstRef, args: SahaFunctionArguments, create_pos: &Option<FilePosition>) -> Result<Box<dyn SahaObject>, RuntimeError> {
        self.properties.validate_args(&args, create_pos)?;

        let mut inst_props: ObjProperties = HashMap::new();
        let mut inst_methods: HashMap<String, Box<dyn SahaCallable>> = HashMap::new();

        for (pname, p) in &self.properties {
            if args.contains_key(pname) {
                let arg_prop: Property = Property {
                    name: p.name.clone(),
                    prop_type: p.prop_type.clone(),
                    default: args.get(pname).unwrap().clone(),
                    is_static: p.is_static,
                    visibility: p.visibility.clone()
                };

                inst_props.insert(pname.to_string(), arg_prop);
            } else {
                inst_props.insert(pname.to_string(), p.clone());
            }
        }

        for (mname, m) in &self.methods {
            // FIXME how to reference these without cloning them all over the place
            inst_methods.insert(mname.to_string(), m.box_clone());
        }

        return Ok(Box::new(UserInstance {
            class_name: self.name.clone(),
            fq_class_name: self.fqname.clone(),
            methods: inst_methods,
            properties: inst_props,
            implements: self.implements.clone(),
            inst_ref: inst_ref
        }));
    }
}

/// Behavior definition, name and the requires methods.
pub struct BehaviorDefinition {
    name: String,
    fqname: String,
    methods: HashMap<String, Box<SahaCallable>>
}

/// Class member visibility, e.g. public or private.
#[derive(Clone, Debug, PartialEq)]
pub enum MemberVisibility {
    Public,
    Private
}

/// User-defined class instances.
#[derive(Clone)]
pub struct UserInstance {
    class_name: String,
    fq_class_name: String,
    inst_ref: InstRef,
    implements: Vec<String>,
    methods: HashMap<String, Box<SahaCallable>>,
    properties: HashMap<String, Property>
}

impl SahaObject for UserInstance {
    fn get_instance_ref(&self) -> InstRef {
        return self.inst_ref;
    }

    fn get_class_name(&self) -> String {
        return self.class_name.clone();
    }

    fn get_fully_qualified_class_name(&self) -> String {
        return self.fq_class_name.clone();
    }

    fn get_implements(&self) -> Vec<String> {
        return self.implements.clone();
    }

    fn call_member(&mut self, member: String, args: SahaFunctionArguments, static_access: bool, accessor_instref: Option<InstRef>) -> SahaCallResult {
        unimplemented!()
    }

    fn access_property(&self, prop: String, static_access: bool, accessor_instref: Option<InstRef>) -> SahaCallResult {
        unimplemented!()
    }

    fn mutate_property(&mut self, prop: String, static_access: bool, accessor_instref: Option<InstRef>) -> SahaCallResult {
        unimplemented!()
    }

    fn box_clone(&self) -> Box<dyn SahaObject> {
        return Box::new(self.clone());
    }
}