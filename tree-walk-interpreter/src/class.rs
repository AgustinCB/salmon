use crate::function::LoxFunction;
use crate::value::Value;
use parser::types::{ProgramError, SourceCodeLocation, Statement, StatementType};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use crate::interpreter::Interpreter;

fn function_declaration_to_lox_funxtion<'a>(
    arguments: &[&'a str],
    body: Vec<&'a Statement<'a>>,
    location: &SourceCodeLocation<'a>,
    environments: &[Rc<RefCell<HashMap<&'a str, Value<'a>>>>],
) -> LoxFunction<'a> {
    LoxFunction {
        arguments: arguments.to_vec(),
        body,
        environments: environments.to_vec(),
        location: location.clone(),
    }
}

fn statement_list_to_function_hash_map<'a>(
    statements: &[&'a Statement<'a>],
    environments: &[Rc<RefCell<HashMap<&'a str, Value<'a>>>>],
) -> HashMap<&'a str, Rc<LoxFunction<'a>>> {
    let mut functions = HashMap::default();
    for s in statements {
        match &s.statement_type {
            StatementType::FunctionDeclaration {
                arguments,
                body,
                name,
            } => {
                functions.insert(
                    name.clone(),
                    Rc::new(function_declaration_to_lox_funxtion(
                        arguments,
                        body.iter().map(AsRef::as_ref).collect(),
                        &s.location,
                        &environments,
                    )),
                );
            }
            _ => panic!("Unexpected method"),
        }
    }
    functions
}

#[derive(Clone, Debug, PartialEq)]
pub struct LoxClass<'a> {
    methods: Rc<RefCell<HashMap<&'a str, Rc<LoxFunction<'a>>>>>,
    getters: Rc<RefCell<HashMap<&'a str, Rc<LoxFunction<'a>>>>>,
    setters: Rc<RefCell<HashMap<&'a str, Rc<LoxFunction<'a>>>>>,
    traits: Rc<RefCell<HashSet<&'a str>>>,
    pub superclass: Option<Rc<LoxClass<'a>>>,
    pub name: &'a str,
    pub static_instance: Rc<LoxObject<'a>>,
}

impl<'a> LoxClass<'a> {
    pub fn new(
        name: &'a str,
        static_method_list: &[&'a Statement<'a>],
        method_list: &[&'a Statement<'a>],
        getters: &[&'a Statement<'a>],
        setters: &[&'a Statement<'a>],
        superclass: Option<Rc<LoxClass<'a>>>,
        mut environments: Vec<Rc<RefCell<HashMap<&'a str, Value<'a>>>>>,
    ) -> LoxClass<'a> {
        environments.push(Rc::new(RefCell::new(HashMap::default())));
        let methods = Rc::new(RefCell::new(statement_list_to_function_hash_map(
            method_list,
            &environments,
        )));
        let getters = Rc::new(RefCell::new(statement_list_to_function_hash_map(
            getters,
            &environments,
        )));
        let setters = Rc::new(RefCell::new(statement_list_to_function_hash_map(
            setters,
            &environments,
        )));
        let mut static_methods = vec![];
        for ms in static_method_list {
            match &ms.statement_type {
                StatementType::FunctionDeclaration {
                    arguments,
                    body,
                    name,
                } => {
                    static_methods.push((
                        name.clone(),
                        Rc::new(function_declaration_to_lox_funxtion(
                            arguments,
                            body.iter().map(AsRef::as_ref).collect(),
                            &ms.location,
                            &environments,
                        )),
                    ));
                }
                _ => panic!("Unexpected method"),
            }
        }
        let static_instance =
            Rc::new(LoxObject::new_static(name, &static_methods, superclass.clone()));
        LoxClass {
            getters,
            methods,
            name,
            setters,
            static_instance,
            superclass,
            traits: Rc::new(RefCell::new(HashSet::new())),
        }
    }

    pub fn append_trait(&self, trait_name: &'a str) {
        self.traits.borrow_mut().insert(trait_name);
    }

    pub fn implements(&self, trait_name: &str) -> bool {
        self.traits.borrow().contains(trait_name)
    }

    pub fn append_methods(
        &self,
        method_list: &[&'a Statement<'a>],
        mut environments: Vec<Rc<RefCell<HashMap<&'a str, Value<'a>>>>>,
    ) {
        environments.push(Rc::new(RefCell::new(HashMap::default())));
        let methods = statement_list_to_function_hash_map(method_list, &environments);
        self.methods.borrow_mut().extend(methods);
    }

    pub fn append_static_methods(
        &self,
        method_list: &[&'a Statement<'a>],
        mut environments: Vec<Rc<RefCell<HashMap<&'a str, Value<'a>>>>>,
    ) {
        environments.push(Rc::new(RefCell::new(HashMap::default())));
        self.static_instance
            .append_methods(method_list, environments);
    }

    pub fn append_getters(
        &self,
        method_list: &[&'a Statement<'a>],
        mut environments: Vec<Rc<RefCell<HashMap<&'a str, Value<'a>>>>>,
    ) {
        environments.push(Rc::new(RefCell::new(HashMap::default())));
        let getters = statement_list_to_function_hash_map(method_list, &environments);
        self.getters.borrow_mut().extend(getters);
    }

    pub fn append_setters(
        &self,
        method_list: &[&'a Statement<'a>],
        mut environments: Vec<Rc<RefCell<HashMap<&'a str, Value<'a>>>>>,
    ) {
        environments.push(Rc::new(RefCell::new(HashMap::default())));
        let setters = statement_list_to_function_hash_map(method_list, &environments);
        self.setters.borrow_mut().extend(setters);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct LoxObject<'a> {
    properties: Rc<RefCell<HashMap<&'a str, Value<'a>>>>,
    getters: HashMap<&'a str, Rc<LoxFunction<'a>>>,
    setters: HashMap<&'a str, Rc<LoxFunction<'a>>>,
    pub superclass: Option<Rc<LoxObject<'a>>>,
    pub class_name: &'a str,
    pub traits: HashSet<&'a str>,
}

impl<'a> LoxObject<'a> {
    pub fn new(class: Rc<LoxClass<'a>>) -> Rc<LoxObject<'a>> {
        let properties = Rc::new(RefCell::new(HashMap::default()));
        let superclass = class
            .superclass
            .clone()
            .map(LoxObject::new);
        let mut object = LoxObject {
            class_name: class.name.clone(),
            getters: HashMap::default(),
            properties: properties.clone(),
            setters: HashMap::default(),
            superclass: superclass.clone(),
            traits: class.traits.borrow().clone(),
        };
        if let Some(obj) = &superclass {
            properties.borrow_mut().insert("super", Value::Object(obj.clone()));
        }
        for (name, f) in class.getters.borrow().iter() {
            object.getters.insert(name.clone(), f.clone());
        }
        for (name, f) in class.setters.borrow().iter() {
            object.setters.insert(name.clone(), f.clone());
        }
        let object = Rc::new(object);
        for (name, f) in class.methods.borrow().iter() {
            properties
                .borrow_mut()
                .insert(name.clone(), Value::Method(f.clone(), object.clone()));
        }
        object
    }

    fn new_static(
        class_name: &'a str,
        methods: &[(&'a str, Rc<LoxFunction<'a>>)],
        superclass: Option<Rc<LoxClass<'a>>>,
    ) -> LoxObject<'a> {
        let properties = Rc::new(RefCell::new(HashMap::default()));
        for (name, function) in methods {
            properties
                .borrow_mut()
                .insert(name.clone(), Value::Function(function.clone()));
        }
        let superclass = superclass
            .map(|c| {
                LoxObject::new_static(
                    c.name.clone(),
                    &c.methods
                        .borrow()
                        .clone()
                        .into_iter()
                        .collect::<Vec<(&'a str, Rc<LoxFunction>)>>(),
                    c.superclass.clone(),
                )
            })
            .map(Rc::new);
        LoxObject {
            getters: HashMap::new(),
            setters: HashMap::new(),
            traits: HashSet::new(),
            class_name,
            properties,
            superclass,
        }
    }

    pub fn init(
        &self,
        values: &[Value<'a>],
        interpreter: &'a Interpreter<'a>,
        location: &SourceCodeLocation<'a>,
    ) -> Result<(), ProgramError<'a>> {
        if let Some(s) = &self.superclass {
            s.init(values, interpreter, location)?;
        }
        if let Some(Value::Method(f, obj)) = self.properties.borrow().get("init") {
            let mut arguments: Vec<Value<'a>> = vec![Value::Object(obj.clone())];
            arguments.extend_from_slice(values);
            f.eval(&arguments, interpreter)?;
            Ok(())
        } else if values.len() != 0 {
            Err(ProgramError {
                message: format!(
                    "Wrong number of arguments: Received {}, expected {}",
                    values.len(),
                    0,
                ),
                location: location.clone(),
            })
        } else {
            Ok(())
        }
    }

    pub fn get_setter(&self, name: &str) -> Option<Rc<LoxFunction<'a>>> {
        self.setters.get(name).cloned()
    }

    pub fn get_getter(&self, name: &str) -> Option<Rc<LoxFunction<'a>>> {
        self.getters.get(name).cloned()
    }

    pub fn get(&self, name: &str) -> Option<Value<'a>> {
        let v = self.properties.borrow().get(name).cloned();
        if v.is_some() {
            v
        } else {
            self.superclass.as_ref().map(|s| s.get(name)).flatten()
        }
    }

    pub fn set(&self, name: &'a str, value: Value<'a>) {
        self.properties.borrow_mut().insert(name, value);
    }

    pub fn append_methods(
        &self,
        method_list: &[&'a Statement<'a>],
        environments: Vec<Rc<RefCell<HashMap<&'a str, Value<'a>>>>>,
    ) {
        let methods = statement_list_to_function_hash_map(method_list, &environments);
        self.properties
            .borrow_mut()
            .extend(methods.into_iter().map(|(s, f)| (s, Value::Function(f))));
    }
}
