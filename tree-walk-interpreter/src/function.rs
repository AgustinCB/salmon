use crate::class::LoxObject;
use crate::state::State;
use crate::value::Value;
use parser::types::{ProgramError, SourceCodeLocation, Statement};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Error, Formatter};
use std::rc::Rc;
use crate::interpreter::Interpreter;

#[derive(Clone, PartialEq)]
pub struct LoxFunction<'a> {
    pub arguments: Vec<String>,
    pub environments: Vec<Rc<RefCell<HashMap<String, Value<'a>>>>>,
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
                    self.arguments.len()
                ),
                location: self.location.clone(),
            });
        }
        let mut current_state = State::new(&self.environments);
        current_state.push();
        for (name, value) in self.arguments.iter().cloned().zip(values.iter().cloned()) {
            current_state.insert_top(name, value);
        }
        current_state.in_function = true;
        for st in self.body.iter() {
            current_state = interpreter.evaluate(current_state, st)?.0;
            if let Some(return_value) = &current_state.return_value {
                let value = (**return_value).clone();
                return Ok(value);
            }
        }
        Ok(Value::Nil)
    }

    pub(crate) fn bind(&mut self, instances: &[LoxObject<'a>], variables: &[&str]) {
        let mut new_scope = HashMap::default();
        for (variable, obj) in variables.into_iter().zip(instances.into_iter()) {
            new_scope.insert((*variable).to_owned(), Value::Object(obj.clone()));
        }
        self.environments.push(Rc::new(RefCell::new(new_scope)));
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
