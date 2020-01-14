use crate::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub struct State {
    pub return_value: Option<Box<Value>>,
    pub broke_loop: bool,
    pub loop_count: usize,
    pub in_function: bool,
    environments: Vec<Rc<RefCell<HashMap<String, Value>>>>,
}

impl State {
    pub fn new(environments: &[Rc<RefCell<HashMap<String, Value>>>]) -> State {
        if environments.is_empty() {
            panic!("Environments can't be empty");
        }
        State {
            return_value: None,
            broke_loop: false,
            in_function: false,
            loop_count: 0,
            environments: environments.to_vec(),
        }
    }

    pub fn assign_at(&self, id: usize, identifier: &str, value: &Value) {
        self.environments.get(id).iter().for_each(|e| {
            e.borrow_mut().insert(identifier.to_owned(), value.clone());
        });
    }

    pub fn get_at(&self, identifier: &str, id: usize) -> Option<Value> {
        self.environments
            .get(id)
            .map(|e| e.borrow().get(identifier).cloned())
            .flatten()
    }

    pub fn get_global(&self, identifier: &str) -> Option<Value> {
        self.environments[0].borrow().get(identifier).cloned()
    }

    pub fn push(&mut self) {
        self.environments
            .push(Rc::new(RefCell::new(HashMap::default())));
    }

    pub fn pop(&mut self) {
        self.environments.pop();
        if self.environments.is_empty() {
            panic!("Empty environments!");
        }
    }

    pub fn insert_top(&mut self, identifier: String, value: Value) {
        self.last().borrow_mut().insert(identifier, value);
    }

    pub fn insert(&mut self, identifier: String, value: Value) {
        for env in self.environments.iter().rev() {
            if env.borrow().contains_key(&identifier) {
                env.borrow_mut().insert(identifier, value);
                return;
            }
        }
        self.insert_top(identifier, value);
    }

    #[cfg(test)]
    pub fn find(&self, identifier: &str) -> Option<Value> {
        self.environments
            .iter()
            .rev()
            .map(|env| env.borrow().get(identifier).cloned())
            .find(|v| v.is_some())
            .flatten()
    }

    pub fn add_return_value(&mut self, v: Value) {
        self.return_value = Some(Box::new(v));
    }

    pub fn last(&self) -> &RefCell<HashMap<String, Value>> {
        self.environments.last().unwrap()
    }

    pub fn get_environments(&self) -> Vec<Rc<RefCell<HashMap<String, Value>>>> {
        self.environments.clone()
    }
}

impl Default for State {
    fn default() -> State {
        State {
            return_value: None,
            broke_loop: false,
            loop_count: 0,
            in_function: false,
            environments: vec![Rc::new(RefCell::new(HashMap::default()))],
        }
    }
}
