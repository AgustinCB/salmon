use crate::function::LoxFunction;
use crate::value::{Value, LoxModule};
use parser::types::{ProgramError, SourceCodeLocation, Statement, StatementType};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

fn function_declaration_to_lox_funxtion(
    arguments: &[String],
    body: &[Box<Statement>],
    location: &SourceCodeLocation,
    environments: &[Rc<RefCell<HashMap<String, Value>>>],
) -> LoxFunction {
    LoxFunction {
        arguments: arguments.to_vec(),
        body: body.iter().map(|s| (**s).clone()).collect(),
        environments: environments.to_vec(),
        location: location.clone(),
    }
}

fn statement_list_to_function_hash_map(
    statements: &[&Statement],
    environments: &[Rc<RefCell<HashMap<String, Value>>>],
) -> HashMap<String, LoxFunction> {
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
                        body,
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
pub struct LoxClass {
    methods: Rc<RefCell<HashMap<String, LoxFunction>>>,
    getters: Rc<RefCell<HashMap<String, LoxFunction>>>,
    setters: Rc<RefCell<HashMap<String, LoxFunction>>>,
    superclass: Option<Box<LoxClass>>,
    pub name: String,
    pub static_instance: LoxObject,
}

impl LoxClass {
    pub fn new(
        name: String,
        static_method_list: &[&Statement],
        method_list: &[&Statement],
        getters: &[&Statement],
        setters: &[&Statement],
        superclass: Option<LoxClass>,
        environments: Vec<Rc<RefCell<HashMap<String, Value>>>>,
    ) -> LoxClass {
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
                            body,
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
        }
    }

    pub fn append_methods(
        &self,
        method_list: &[&Statement],
        environments: Vec<Rc<RefCell<HashMap<String, Value>>>>,
    ) {
        let methods = statement_list_to_function_hash_map(method_list, &environments);
        self.methods.borrow_mut().extend(methods);
    }

    pub fn append_static_methods(
        &self,
        method_list: &[&Statement],
        environments: Vec<Rc<RefCell<HashMap<String, Value>>>>,
    ) {
        self.static_instance
            .append_methods(method_list, environments);
    }

    pub fn append_getters(
        &self,
        method_list: &[&Statement],
        environments: Vec<Rc<RefCell<HashMap<String, Value>>>>,
    ) {
        let getters = statement_list_to_function_hash_map(method_list, &environments);
        self.getters.borrow_mut().extend(getters);
    }

    pub fn append_setters(
        &self,
        method_list: &[&Statement],
        environments: Vec<Rc<RefCell<HashMap<String, Value>>>>,
    ) {
        let setters = statement_list_to_function_hash_map(method_list, &environments);
        self.setters.borrow_mut().extend(setters);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct LoxObject {
    properties: Rc<RefCell<HashMap<String, Value>>>,
    getters: HashMap<String, LoxFunction>,
    setters: HashMap<String, LoxFunction>,
    superclass: Option<Box<LoxObject>>,
    pub class_name: String,
}

impl LoxObject {
    pub fn new(class: LoxClass) -> LoxObject {
        let properties = Rc::new(RefCell::new(HashMap::default()));
        let superclass = class
            .superclass
            .map(|c| *c)
            .map(LoxObject::new)
            .map(Box::new);
        let mut object = LoxObject {
            class_name: class.name,
            getters: HashMap::default(),
            properties: properties.clone(),
            setters: HashMap::default(),
            superclass: superclass.clone(),
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
        methods: &[(String, LoxFunction)],
        superclass: Option<LoxClass>,
    ) -> LoxObject {
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
            class_name,
            properties,
            superclass,
        }
    }

    pub fn init(
        &self,
        values: &[Value],
        locals: &HashMap<usize, usize>,
        imports: &HashMap<String, LoxModule>,
        location: &SourceCodeLocation,
    ) -> Result<(), ProgramError> {
        if let Some(s) = &self.superclass {
            s.init(values, locals, imports, location)?;
        }
        if let Some(Value::Function(f)) = self.properties.borrow().get("init") {
            f.eval(values, locals, imports)?;
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

    pub fn get_setter(&self, name: &str) -> Option<LoxFunction> {
        self.setters.get(name).cloned()
    }

    pub fn get_getter(&self, name: &str) -> Option<LoxFunction> {
        self.getters.get(name).cloned()
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        let v = self.properties.borrow().get(name).cloned();
        if v.is_some() {
            v
        } else {
            self.superclass.as_ref().map(|s| s.get(name)).flatten()
        }
    }

    pub fn set(&mut self, name: String, value: Value) {
        self.properties.borrow_mut().insert(name, value);
    }

    pub fn append_methods(
        &self,
        method_list: &[&Statement],
        environments: Vec<Rc<RefCell<HashMap<String, Value>>>>,
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
