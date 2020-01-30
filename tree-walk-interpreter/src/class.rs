use crate::function::LoxFunction;
use crate::value::Value;
use parser::types::{ProgramError, SourceCodeLocation, Statement, StatementType};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use crate::interpreter::Interpreter;

fn function_declaration_to_lox_funxtion<'a>(
    arguments: &[String],
    body: Vec<&'a Statement<'a>>,
    location: &SourceCodeLocation<'a>,
    environments: &[Rc<RefCell<HashMap<String, Value<'a>>>>],
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
    environments: &[Rc<RefCell<HashMap<String, Value<'a>>>>],
) -> HashMap<String, LoxFunction<'a>> {
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
                    function_declaration_to_lox_funxtion(
                        arguments,
                        body.iter().map(AsRef::as_ref).collect(),
                        &s.location,
                        &environments,
                    ),
                );
            }
            _ => panic!("Unexpected method"),
        }
    }
    functions
}

#[derive(Clone, Debug, PartialEq)]
pub struct LoxClass<'a> {
    methods: Rc<RefCell<HashMap<String, LoxFunction<'a>>>>,
    getters: Rc<RefCell<HashMap<String, LoxFunction<'a>>>>,
    setters: Rc<RefCell<HashMap<String, LoxFunction<'a>>>>,
    traits: Rc<RefCell<HashSet<String>>>,
    pub superclass: Option<Box<LoxClass<'a>>>,
    pub name: String,
    pub static_instance: LoxObject<'a>,
}

impl<'a> LoxClass<'a> {
    pub fn new(
        name: String,
        static_method_list: &[&'a Statement<'a>],
        method_list: &[&'a Statement<'a>],
        getters: &[&'a Statement<'a>],
        setters: &[&'a Statement<'a>],
        superclass: Option<LoxClass<'a>>,
        environments: Vec<Rc<RefCell<HashMap<String, Value<'a>>>>>,
    ) -> LoxClass<'a> {
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
                        function_declaration_to_lox_funxtion(
                            arguments,
                            body.iter().map(AsRef::as_ref).collect(),
                            &ms.location,
                            &environments,
                        ),
                    ));
                }
                _ => panic!("Unexpected method"),
            }
        }
        let static_instance =
            LoxObject::new_static(name.clone(), &static_methods, superclass.clone());
        LoxClass {
            getters,
            methods,
            name,
            setters,
            static_instance,
            superclass: superclass.map(Box::new),
            traits: Rc::new(RefCell::new(HashSet::new())),
        }
    }

    pub fn append_trait(&self, trait_name: String) {
        self.traits.borrow_mut().insert(trait_name);
    }

    pub fn implements(&self, trait_name: &str) -> bool {
        self.traits.borrow().contains(trait_name)
    }

    pub fn append_methods(
        &self,
        method_list: &[&'a Statement<'a>],
        environments: Vec<Rc<RefCell<HashMap<String, Value<'a>>>>>,
    ) {
        let methods = statement_list_to_function_hash_map(method_list, &environments);
        self.methods.borrow_mut().extend(methods);
    }

    pub fn append_static_methods(
        &self,
        method_list: &[&'a Statement<'a>],
        environments: Vec<Rc<RefCell<HashMap<String, Value<'a>>>>>,
    ) {
        self.static_instance
            .append_methods(method_list, environments);
    }

    pub fn append_getters(
        &self,
        method_list: &[&'a Statement<'a>],
        environments: Vec<Rc<RefCell<HashMap<String, Value<'a>>>>>,
    ) {
        let getters = statement_list_to_function_hash_map(method_list, &environments);
        self.getters.borrow_mut().extend(getters);
    }

    pub fn append_setters(
        &self,
        method_list: &[&'a Statement<'a>],
        environments: Vec<Rc<RefCell<HashMap<String, Value<'a>>>>>,
    ) {
        let setters = statement_list_to_function_hash_map(method_list, &environments);
        self.setters.borrow_mut().extend(setters);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct LoxObject<'a> {
    properties: Rc<RefCell<HashMap<String, Value<'a>>>>,
    getters: HashMap<String, LoxFunction<'a>>,
    setters: HashMap<String, LoxFunction<'a>>,
    pub superclass: Option<Box<LoxObject<'a>>>,
    pub class_name: String,
    pub traits: HashSet<String>,
}

impl<'a> LoxObject<'a> {
    pub fn new(class: LoxClass<'a>) -> LoxObject<'a> {
        let properties = Rc::new(RefCell::new(HashMap::default()));
        let superclass = class
            .superclass
            .map(|c| *c)
            .map(LoxObject::new)
            .map(Box::new);
        let mut object = LoxObject {
            class_name: class.name.clone(),
            getters: HashMap::default(),
            properties: properties.clone(),
            setters: HashMap::default(),
            superclass: superclass.clone(),
            traits: class.traits.borrow().clone(),
        };
        let mut variables = vec!["this"];
        let mut instances = vec![object.clone()];
        if let Some(box obj) = &superclass {
            variables.push("super");
            instances.push(obj.clone());
        }
        for (name, f) in class.methods.borrow().iter() {
            let mut f = f.clone();
            f.bind(&instances, &variables);
            properties
                .borrow_mut()
                .insert(name.clone(), Value::Function(f));
        }
        for (name, f) in class.getters.borrow().iter() {
            let mut f = f.clone();
            f.bind(&instances, &variables);
            object.getters.insert(name.clone(), f);
        }
        for (name, f) in class.setters.borrow().iter() {
            let mut f = f.clone();
            f.bind(&instances, &variables);
            object.setters.insert(name.clone(), f);
        }
        object
    }

    fn new_static(
        class_name: String,
        methods: &[(String, LoxFunction<'a>)],
        superclass: Option<LoxClass<'a>>,
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
                        .collect::<Vec<(String, LoxFunction)>>(),
                    c.superclass.map(|c| *c),
                )
            })
            .map(Box::new);
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
        if let Some(Value::Function(f)) = self.properties.borrow().get("init") {
            f.eval(values, interpreter)?;
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

    pub fn get_setter(&self, name: &str) -> Option<LoxFunction<'a>> {
        self.setters.get(name).cloned()
    }

    pub fn get_getter(&self, name: &str) -> Option<LoxFunction<'a>> {
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

    pub fn set(&mut self, name: String, value: Value<'a>) {
        self.properties.borrow_mut().insert(name, value);
    }

    pub fn append_methods(
        &self,
        method_list: &[&'a Statement<'a>],
        environments: Vec<Rc<RefCell<HashMap<String, Value<'a>>>>>,
    ) {
        let mut methods = statement_list_to_function_hash_map(method_list, &environments);
        let mut variables = vec!["this"];
        let mut instances = vec![self.clone()];
        if let Some(box obj) = &self.superclass {
            variables.push("super");
            instances.push(obj.clone());
        }
        for f in methods.values_mut() {
            f.bind(&instances, &variables);
        }
        self.properties
            .borrow_mut()
            .extend(methods.into_iter().map(|(s, f)| (s, Value::Function(f))));
    }
}
