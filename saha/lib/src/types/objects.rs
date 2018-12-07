//! Saha object types

use std::{
    collections::HashMap,
    sync::Arc
};

use crate::prelude::*;

/// Helper type for core class constructors.
pub type CoreConstructorFn = fn(instref: InstRef, args: &SahaFunctionArguments, param_types: &[Box<SahaType>], additional_data: &SahaFunctionArguments, create_pos: Option<FilePosition>) -> Result<Box<dyn SahaObject>, RuntimeError>;

/// A helper struct for constructing object member access, either method or
/// property.
#[derive(Clone, Copy)]
pub struct AccessParams<'a> {
    pub member_name: &'a str,
    pub is_static_access: bool,
    pub access_file_pos: &'a Option<FilePosition>,
    pub accessor_instref: &'a Option<InstRef>
}

/// Instances of classes in saha, either core or userland.
pub trait SahaObject: Send {
    /// Get the instance reference for this object.
    fn get_instance_ref(&self) -> InstRef;

    /// Is is a core class or a user class? Core classes get some runtime adjustments when it comes
    /// to accessing and method calls.
    fn is_core_defined(&self) -> bool;

    /// Get the class name for the object without module path.
    fn get_class_name(&self) -> String;

    /// The the fully qualified class name with namespace/module path.
    fn get_fully_qualified_class_name(&self) -> String;

    /// Get a list of behavior names that this object implements.
    fn get_implements(&self) -> Vec<String>;

    /// Get a full method name to use when getting a reference to a class
    /// method.
    fn get_full_method_name(&mut self, method_name: &str) -> String;

    /// Get a lockable Arc ref to a method.
    fn get_method_ref(&mut self, method_name: &str) -> Result<Arc<Box<dyn SahaCallable>>, RuntimeError>;

    /// Get type parameter definitions (i.e. generics definitions).
    fn get_type_params(&self) -> Vec<(char, Box<SahaType>)>;

    /// Get the object `Name` type value.
    fn get_named_type(&self) -> Box<SahaType>;

    /// Call an object member, e.g. a method. We denote whether this is a static call, and we pass
    /// in an optional instance reference of the calling context (which is used to determine if this
    /// is a `self` call and allow access to private members).
    fn call_member(&mut self, access: AccessParams, args: SahaFunctionArguments) -> SahaCallResult;

    /// Access (get) a member property. Determine static access and `self`
    /// similarly to `call_member()`.
    fn access_property(&self, access: AccessParams) -> SahaCallResult;

    /// Mutate (set) a member property. Determine static access and `self`
    /// similarly to `call_member()`.
    fn mutate_property(&mut self, access: AccessParams, new_value: Value) -> SahaCallResult;

    /// Clone for boxed self.
    fn box_clone(&self) -> Box<dyn SahaObject>;

    /// Convert iterable objects into a Rust iterator for data handling inside loops and similar
    /// constructs.
    fn into_iter(&self) -> Box<Iterator<Item = (Value, Value)>>;

    /// Load object data from an iterator. Only implemented for iterable type objects such as `List`
    /// and `Dict`.
    fn set_data_from_iter(&mut self, iterator: Box<Iterator<Item = (Value, Value)>>);
}

impl Clone for Box<dyn SahaObject> {
    fn clone(&self) -> Box<dyn SahaObject> {
        return self.box_clone();
    }
}

/// A class property.
#[derive(Clone, Debug)]
pub struct Property {
    pub name: String,
    pub prop_type: Box<SahaType>,
    pub default: Value,
    pub is_static: bool,
    pub visibility: MemberVisibility,
    pub value: Option<Value>
}

impl Property {
    /// Return the same property as a clone with a newly replaced value.
    fn with_value(&self, new_value: &Value) -> Property {
        let mut new = self.clone();

        new.value = Some(new_value.clone());

        return new;
    }
}

pub type ObjProperties = HashMap<String, Property>;

impl ValidatesArgs for ObjProperties {
    fn validate_args(&self, args: &SahaFunctionArguments, call_pos: &Option<FilePosition>) -> Result<SahaFunctionArguments, RuntimeError> {
        if self.len() == 0 && args.len() == 0 {
            // no args expected, and none given, so we're ok
            return Ok(args.clone());
        }

        if args.len() > self.len() {
            let err = RuntimeError::new("Too many arguments provided", call_pos.to_owned());

            return Err(err);
        }

        for (pname, p) in self {
            if *p.default.kind == SahaType::Void && args.contains_key(pname) == false {
                // no default and no arg for it given
                let err = RuntimeError::new(&format!("The `{}` argument is required", pname), call_pos.to_owned());

                return Err(err);
            } else if *p.default.kind != SahaType::Void && args.contains_key(pname) == false {
                // default exists and no args was provided, we're OK here
                continue;
            }

            let propk = p.prop_type.clone();
            let arg = &args.get(pname).unwrap();
            let argk = arg.kind.clone();

            match *argk {
                SahaType::Obj => {
                    let wanted_name = match *propk {
                        SahaType::Name(ref n, ..) => n.clone(),
                        _ => {
                            let err = RuntimeError::new(
                                &format!(
                                    "Invalid argument `{}`, expected `{}` but received `{}`",
                                    pname,
                                    propk.to_readable_string(),
                                    argk.to_readable_string()
                                ),
                                call_pos.to_owned()
                            );

                            return Err(err);
                        }
                    };

                    // get the object type+implements list for comparing
                    let mut inst_impl: Vec<String>;
                    let inst_fqname: String;
                    let inst_type: Box<SahaType>;

                    {
                        let st = crate::SAHA_SYMBOL_TABLE.lock().unwrap();
                        let inst_lockable = st.instances.get(&arg.obj.unwrap()).unwrap();
                        let inst = inst_lockable.lock().unwrap();

                        inst_impl = inst.get_implements();
                        inst_impl.push(inst.get_fully_qualified_class_name());
                        inst_fqname = inst.get_fully_qualified_class_name();
                        inst_type = inst.get_named_type();
                    }


                    // Compare class/behavior names, then types (as there could be different type params in use)
                    if inst_impl.contains(&wanted_name) == false || *propk != *inst_type {
                        let err = RuntimeError::new(
                            &format!(
                                "Invalid argument `{}`, expected `{}` but received `{}`",
                                pname,
                                propk.to_readable_string(),
                                inst_fqname
                            ),
                            call_pos.to_owned()
                        );

                        return Err(err);
                    }
                },
                _ => {
                    if *propk != *argk {
                        let err = RuntimeError::new(
                            &format!(
                                "Invalid argument `{}`, expected `{}` but received `{}`",
                                pname,
                                propk.to_readable_string(),
                                argk.to_readable_string()
                            ),
                            call_pos.to_owned()
                        );

                        return Err(err);
                    }
                }
            };
        }

        return Ok(args.clone());
    }

    fn validate_single_param_args(&self, args: &SahaFunctionArguments, _call_pos: &Option<FilePosition>) -> Result<SahaFunctionArguments, RuntimeError> {
        return Ok(args.clone());
    }
}

/// A class definition, or a blueprint in other words. From these actual
/// class object instances are created.
#[derive(Clone)]
pub struct ClassDefinition {
    pub name: String,
    pub fqname: String,
    pub properties: ObjProperties,
    pub implements: Vec<String>,
    pub type_params: Vec<(char, Box<SahaType>)>
}

impl ClassDefinition {
    /// Get parsed class properties, where type parameters have been normalized.
    fn get_parsed_properties(
        &self,
        type_params: &HashMap<char, Box<SahaType>>,
        create_pos: &Option<FilePosition>
    ) -> Result<ObjProperties, RuntimeError> {
        let mut new_props: ObjProperties = HashMap::new();

        for (pname, p) in &self.properties {
            let new_p = match *p.prop_type {
                SahaType::TypeParam(n) => {
                    let mut np = p.clone();

                    if type_params.contains_key(&n) == false {
                        return Err(
                            RuntimeError::new(
                                &format!("Class `{}` requires type parameter {} to be defined", self.fqname, n),
                                create_pos.clone()
                            )
                        );
                    }

                    np.prop_type = type_params.get(&n).unwrap().clone();

                    np
                },
                _ => p.clone()
            };

            new_props.insert(pname.clone(), new_p);
        }

        return Ok(new_props);
    }

    /// Create a new instance object from this definition blueprint.
    pub fn create_new_instance(
        &self,
        inst_ref: InstRef,
        args: SahaFunctionArguments,
        typeparams: &[Box<SahaType>],
        create_pos: &Option<FilePosition>
    ) -> Result<Box<dyn SahaObject>, RuntimeError> {
        if self.type_params.len() != typeparams.len() {
            return Err(
                RuntimeError::new(
                    &format!("Class `{}` expects {} type parameters, {} given", self.fqname, self.type_params.len(), typeparams.len()),
                    create_pos.clone()
                )
            );
        }

        let mut tyidx = 0;
        let mut received_typarams: HashMap<char, Box<SahaType>> = HashMap::new();

        for (c, _) in &self.type_params {
            received_typarams.insert(*c, typeparams[tyidx].clone());

            tyidx += 1;
        }

        let parsed_properties = self.get_parsed_properties(&received_typarams, create_pos)?;

        parsed_properties.validate_args(&args, create_pos)?;

        let mut inst_props: ObjProperties = HashMap::new();

        for (pname, p) in &parsed_properties {
            if args.contains_key(pname) {
                let arg_val: Value = args.get(pname).unwrap().clone();

                let arg_prop: Property = Property {
                    name: p.name.clone(),
                    prop_type: p.prop_type.clone(),
                    default: p.default.clone(),
                    is_static: p.is_static,
                    visibility: p.visibility.clone(),
                    value: Some(arg_val)
                };

                inst_props.insert(pname.to_string(), arg_prop);
            } else {
                let mut newprop = p.clone();
                newprop.value = Some(p.default.clone());

                inst_props.insert(pname.to_string(), newprop);
            }
        }

        return Ok(Box::new(UserInstance {
            class_name: self.name.clone(),
            fq_class_name: self.fqname.clone(),
            properties: inst_props,
            implements: self.implements.clone(),
            inst_ref: inst_ref,
            type_params: received_typarams.iter().map(|(k, v)| (k.clone(), v.clone())).collect()
        }));
    }
}

/// Behavior definition, name and the required methods (no actual callable, just
/// expected params and expected return type).
pub struct BehaviorDefinition {
    /// Plain behavior name.
    pub name: String,

    /// Fully qualified behavior name.
    pub fqname: String,

    /// Method collection. Keyed by method name, tuple contains param definitions for the method,
    /// and the return type for the method.
    pub methods: HashMap<String, (SahaFunctionParamDefs, Box<SahaType>)>
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
    properties: HashMap<String, Property>,
    type_params: Vec<(char, Box<SahaType>)>
}

impl SahaObject for UserInstance {
    fn get_instance_ref(&self) -> InstRef {
        return self.inst_ref;
    }

    fn is_core_defined(&self) -> bool {
        return false;
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

    fn get_full_method_name(&mut self, method_name: &str) -> String {
        return format!("{}#{}", self.get_fully_qualified_class_name(), method_name);
    }

    fn get_method_ref(&mut self, method_name: &str) -> Result<Arc<Box<dyn SahaCallable>>, RuntimeError> {
        let full_method_name = self.get_full_method_name(method_name);

        let st = crate::SAHA_SYMBOL_TABLE.lock().unwrap();

        if st.methods.contains_key(&full_method_name) == false {
            let err = RuntimeError::new(&format!("No method `{}` defined in class `{}`", method_name, self.get_fully_qualified_class_name()), None);

            return Err(err);
        }

        let method_callable: Arc<Box<dyn SahaCallable>> = st.methods.get(&full_method_name).unwrap().clone();

        // clone arc ref and return it
        return Ok(method_callable);
    }

    fn get_type_params(&self) -> Vec<(char, Box<SahaType>)> {
        return self.type_params.clone();
    }

    fn get_named_type(&self) -> Box<SahaType> {
        // we need this sorting hassle to keep the param type items in the exact correct order
        // every time, hashmaps suck at sorting
        let mut ty: Vec<(char, Box<SahaType>)> = self.type_params.clone().into_iter().map(|(k, t)| (k, t)).collect();
        ty.sort_by(|a, b| a.0.cmp(&b.0));

        return Box::new(SahaType::Name(
            self.fq_class_name.clone(),
            ty.iter().map(|(_, t)| t.clone()).collect()
        ));
    }

    fn call_member(&mut self, access: AccessParams, args: SahaFunctionArguments) -> SahaCallResult {
        let is_self_internal_call: bool;
        let member = access.member_name;
        let access_pos = access.access_file_pos;
        let static_access = access.is_static_access;
        let accessor_instref = access.accessor_instref;
        let tparams = self.get_type_params();

        match accessor_instref {
            Some(iref) => is_self_internal_call = self.get_instance_ref() == *iref,
            _ => is_self_internal_call = false
        };

        let member_callable = self.get_method_ref(member);

        if member_callable.is_err() {
            let err = RuntimeError::new(
                &member_callable.err().unwrap().get_message(),
                access_pos.to_owned()
            );

            return Err(err);
        }

        // callable result contains an arc and mutex which we need to lock and unwrap
        let member_callable = member_callable.ok().unwrap();
        let member_is_static = member_callable.is_static();
        let member_is_public = member_callable.is_public();
        let typeparammap: HashMap<_, _> = tparams.into_iter().collect();
        let member_ret_type = member_callable.get_return_type();

        let actual_return_type: Box<SahaType> = match *member_ret_type {
            SahaType::TypeParam(ty) => {
                let maybe_ty: Box<SahaType> = typeparammap.get(&ty).unwrap_or(&Box::new(SahaType::Void)).clone();

                match maybe_ty == Box::new(SahaType::Void) {
                    true => {
                        let err = RuntimeError::new(
                            &format!(
                                "Method `{}` on class `{}` expects a type parameter `{}`, but none was defined",
                                member,
                                self.get_fully_qualified_class_name(),
                                ty
                            ),
                            access_pos.to_owned()
                        );

                        return Err(err);
                    },
                    _ => maybe_ty.clone()
                }
            },
            _ => member_ret_type
        };

        if member_is_public == false && is_self_internal_call == false {
            let err = RuntimeError::new(
                &format!("Attempted to call private method `{}` on class `{}`", member, self.get_fully_qualified_class_name()),
                access_pos.to_owned()
            );

            return Err(err);
        }

        if member_is_static == true && static_access == false {
            let err = RuntimeError::new(
                &format!("Attempted to call static method `{}` unstatically on class `{}`", member, self.get_fully_qualified_class_name()),
                access_pos.to_owned()
            );

            return Err(err);
        }

        // clone here to prevent any accidental side effects
        let mut call_args: SahaFunctionArguments = args.clone();

        if member_is_static == false {
            // insert `self` to the call
            call_args.insert("self".to_string(), Value::obj(self.get_instance_ref()));
        }

        return member_callable.call(call_args, Some(actual_return_type), Vec::new(), access_pos.clone());
    }

    fn access_property(&self, access: AccessParams) -> SahaCallResult {
        let is_self_internal_access: bool;
        let prop = access.member_name;
        let static_access = access.is_static_access;
        let access_pos = access.access_file_pos;
        let accessor_instref = access.accessor_instref;
        let self_fqname = self.get_fully_qualified_class_name();

        match accessor_instref {
            Some(iref) => is_self_internal_access = self.get_instance_ref() == *iref,
            _ => is_self_internal_access = false
        };

        let member_prop = self.properties.get(prop);

        if member_prop.is_none() {
            let err = RuntimeError::new(
                &format!("Attempted to access undefined property `{}` on class `{}`", prop, self_fqname),
                access_pos.to_owned()
            );

            return Err(err);
        }

        let member_prop = member_prop.unwrap();
        let member_is_static = member_prop.is_static;

        let member_is_public = match member_prop.visibility {
            MemberVisibility::Private => false,
            _ => true
        };

        if member_is_public == false && is_self_internal_access == false {
            let err = RuntimeError::new(
                &format!("Attempted to access private property `{}` on class `{}`", prop, self_fqname),
                access_pos.to_owned()
            );

            return Err(err);
        }

        if member_is_static == true && static_access == false {
            let err = RuntimeError::new(
                &format!("Attempted to access static property `{}` unstatically on class `{}`", prop, self_fqname),
                access_pos.to_owned()
            );

            return Err(err);
        } else if member_is_static == false && static_access == true {
            let err = RuntimeError::new(
                &format!("Attempted to access instance property `{}` statically on class `{}`", prop, self_fqname),
                access_pos.to_owned()
            );

            return Err(err);
        }

        match &member_prop.value {
            Some(val) => Ok(val.clone()),
            None => {
                let err = RuntimeError::new(
                    &format!("Attempted to access uninitialized property `{}` unstatically on class `{}`", prop, self_fqname),
                    access_pos.to_owned()
                );

                return Err(err);
            }
        }
    }

    fn mutate_property(&mut self, access: AccessParams, new_value: Value) -> SahaCallResult {
        let is_self_internal_access: bool;
        let prop = access.member_name;
        let static_access = access.is_static_access;
        let access_pos = access.access_file_pos;
        let accessor_instref = access.accessor_instref;
        let self_fqname = self.get_fully_qualified_class_name();

        match accessor_instref {
            Some(iref) => is_self_internal_access = self.get_instance_ref() == *iref,
            _ => is_self_internal_access = false
        };

        let member_prop = self.properties.get(prop);

        if member_prop.is_none() {
            let err = RuntimeError::new(
                &format!("Attempted to mutate undefined property `{}` on class `{}`", prop, self_fqname),
                access_pos.to_owned()
            );

            return Err(err);
        }

        let member_prop = member_prop.unwrap();
        let member_is_static = member_prop.is_static;

        let member_is_public = match member_prop.visibility {
            MemberVisibility::Private => false,
            _ => true
        };

        if member_is_public == false && is_self_internal_access == false {
            let err = RuntimeError::new(
                &format!("Attempted to mutate private property `{}` on class `{}`", prop, self_fqname),
                access_pos.to_owned()
            );

            return Err(err);
        }

        if member_is_static == true && static_access == false {
            let err = RuntimeError::new(
                &format!("Attempted to mutate static property `{}` unstatically on class `{}`", prop, self_fqname),
                access_pos.to_owned()
            );

            return Err(err);
        } else if member_is_static == false && static_access == true {
            let err = RuntimeError::new(
                &format!("Attempted to mutate instance property `{}` statically on class `{}`", prop, self_fqname),
                access_pos.to_owned()
            );

            return Err(err);
        }

        if *member_prop.prop_type != *new_value.kind {
            let err = RuntimeError::new(
                &format!(
                    "Type mismatch when attempting to mutate property `{}` on class `{}`, expected `{:?}` but received `{:?}`",
                    prop,
                    self_fqname,
                    member_prop.prop_type.clone(),
                    new_value.kind.clone()
                ),
                access_pos.to_owned()
            );

            return Err(err);
        }

        let new_prop = member_prop.with_value(&new_value);

        self.properties.insert(member_prop.name.clone(), new_prop);

        return Ok(Value::void());
    }

    fn box_clone(&self) -> Box<dyn SahaObject> {
        return Box::new(self.clone());
    }

    fn into_iter(&self) -> Box<Iterator<Item = (Value, Value)>> {
        unimplemented!()
    }

    fn set_data_from_iter(&mut self, _iterator: Box<Iterator<Item = (Value, Value)>>) {
        unimplemented!()
    }
}
