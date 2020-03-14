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
        let prev_margin = interpreter.state.borrow().view_margin;
        {
            let mut s = interpreter.state.borrow_mut();
            s.environments.extend_from_slice(&self.environments);
            s.view_margin = s.environments.len() - self.environments.len();
            s.push();
            for (name, value) in self.arguments.iter().zip(values.iter().cloned()) {
                s.insert_top(name, value);
            }
            s.in_function = true;
        }
        let mut value = Value::Nil;
        for st in self.body.iter() {
            interpreter.evaluate(st)?;
            if let Some(box return_value) = &interpreter.state.borrow().return_value {
                value = return_value.clone();
                break;
            }
        }
        {
            let mut s = interpreter.state.borrow_mut();
            s.return_value = None;
            s.in_function = false;
            s.pop();
            let current_len = s.environments.len();
            s.view_margin = prev_margin;
            s.environments.resize_with(
                current_len - self.environments.len(),
                || unreachable!(),
            );
        }
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
