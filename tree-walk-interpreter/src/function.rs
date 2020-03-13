use ahash::{AHashMap as HashMap};
use crate::value::Value;
use parser::types::{ProgramError, SourceCodeLocation, Statement};
use std::cell::RefCell;
use std::fmt::{Debug, Error, Formatter};
use std::rc::Rc;
use crate::interpreter::Interpreter;

#[derive(Clone, PartialEq)]
pub struct LoxFunction<'a> {
    pub arguments: Vec<&'a str>,
    pub environments: Vec<Rc<RefCell<HashMap<&'a str, Value<'a>>>>>,
    pub body: Vec<&'a Statement<'a>>,
    pub location: SourceCodeLocation<'a>,
}

impl<'a> LoxFunction<'a> {
    pub fn eval(
        &self,
        values: &[Value<'a>],
        interpreter: &'a Interpreter<'a>,
    ) -> Result<Value<'a>, ProgramError<'a>> {
        if self.arguments.len() != values.len() {
            return Err(ProgramError {
                message: format!(
                    "Wrong number of arguments: Received {}, expected {}",
                    values.len(),
                    self.arguments.len(),
                ),
                location: self.location.clone(),
            });
        }
        let old_envs = interpreter.state.borrow().get_environments();
        interpreter.state.borrow_mut().environments = self.environments.clone();
        interpreter.state.borrow_mut().push();
        for (name, value) in self.arguments.iter().cloned().zip(values.iter().cloned()) {
            interpreter.state.borrow_mut().insert_top(name, value);
        }
        interpreter.state.borrow_mut().in_function = true;
        let mut value = Value::Nil;
        for st in self.body.iter() {
            interpreter.evaluate(st)?;
            if let Some(return_value) = &interpreter.state.borrow().return_value {
                value = (**return_value).clone();
                break;
            }
        }
        interpreter.state.borrow_mut().return_value = None;
        interpreter.state.borrow_mut().pop();
        interpreter.state.borrow_mut().in_function = false;
        interpreter.state.borrow_mut().environments = old_envs;
        Ok(value)
    }
}

impl<'a> Debug for LoxFunction<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        f.write_str(
            format!(
                "[Function: Arguments {:?} Body {:?} Location {:?} Env Size {:?}",
                self.arguments,
                self.body,
                self.location,
                self.environments.len()
            )
            .as_str(),
        )
    }
}
