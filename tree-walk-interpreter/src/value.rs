use crate::class::{LoxClass, LoxObject};
use crate::function::LoxFunction;
use parser::types::{DataKeyword, FunctionHeader, Literal, ProgramError, SourceCodeLocation};
use std::cell::RefCell;
use std::convert::TryInto;
use std::fmt::{Display, Error, Formatter};
use std::ops::{Neg, Not};
use std::rc::Rc;
use crate::state::State;
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub struct LoxModule {
    pub state: State,
    pub locals: HashMap<usize, usize>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Nil,
    Uninitialized,
    Boolean {
        value: bool,
    },
    Number {
        value: f32,
    },
    String {
        value: String,
    },
    Function(LoxFunction),
    Class(LoxClass),
    Object(LoxObject),
    Trait {
        name: String,
        methods: Vec<FunctionHeader>,
        getters: Vec<FunctionHeader>,
        setters: Vec<FunctionHeader>,
        static_methods: Vec<FunctionHeader>,
    },
    Array {
        capacity: usize,
        elements: Rc<RefCell<Vec<Box<Value>>>>,
    },
    Module(LoxModule),
}

impl Value {
    pub fn is_number(&self) -> bool {
        match self {
            Value::Number { .. } => true,
            _ => false,
        }
    }

    pub fn is_class(&self) -> bool {
        match self {
            Value::Class { .. } => true,
            _ => false,
        }
    }

    pub fn is_trait(&self) -> bool {
        match self {
            Value::Trait { .. } => true,
            _ => false,
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Nil => false,
            Value::Uninitialized => false,
            Value::Boolean { value: false } => false,
            Value::Number { value } if *value == 0f32 => false,
            _ => true,
        }
    }
}

impl Neg for Value {
    type Output = Value;

    fn neg(self) -> Value {
        match self {
            Value::Number { value } => Value::Number { value: -value },
            _ => panic!("Only numbers can change sign"),
        }
    }
}

impl Not for Value {
    type Output = Value;

    fn not(self) -> Self::Output {
        match self {
            Value::Boolean { value } => Value::Boolean { value: !value },
            _ => Value::Boolean {
                value: !self.is_truthy(),
            },
        }
    }
}

pub enum ValueError {
    ExpectingDouble,
    ExpectingString,
}

impl ValueError {
    pub fn into_program_error(self, location: &SourceCodeLocation) -> ProgramError {
        ProgramError {
            location: location.clone(),
            message: self.to_string(),
        }
    }
}

impl ToString for ValueError {
    fn to_string(&self) -> String {
        match self {
            ValueError::ExpectingDouble => "Type error! Expecting a double!".to_owned(),
            ValueError::ExpectingString => "Type error! Expecting a string!".to_owned(),
        }
    }
}

impl TryInto<f32> for Value {
    type Error = ValueError;
    fn try_into(self) -> Result<f32, Self::Error> {
        match self {
            Value::Number { value } => Ok(value),
            _ => Err(ValueError::ExpectingDouble),
        }
    }
}

impl TryInto<String> for Value {
    type Error = ValueError;
    fn try_into(self) -> Result<String, Self::Error> {
        match self {
            Value::String { value } => Ok(value),
            _ => Err(ValueError::ExpectingString),
        }
    }
}

impl Into<Value> for &Literal {
    fn into(self) -> Value {
        match self {
            Literal::Number(value) => Value::Number { value: *value },
            Literal::QuotedString(value) => Value::String {
                value: value.clone(),
            },
            Literal::Keyword(DataKeyword::Nil) => Value::Nil,
            Literal::Keyword(DataKeyword::True) => Value::Boolean { value: true },
            Literal::Keyword(DataKeyword::False) => Value::Boolean { value: false },
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Value::Number { value } => f.write_str(value.to_string().as_str()),
            Value::String { value } => f.write_str(value.as_str()),
            Value::Boolean { value } => f.write_str(value.to_string().as_str()),
            Value::Uninitialized => f.write_str("Uninitialized"),
            Value::Nil => f.write_str("Nil"),
            Value::Function(lf) => f.write_str(format!("{:?}", *lf).as_str()),
            Value::Class(c) => f.write_str(format!("{}", c.name).as_str()),
            Value::Object(c) => f.write_str(format!("{} instance", c.class_name).as_str()),
            Value::Trait { name, .. } => f.write_str(name),
            Value::Array { elements, .. } => {
                f.write_str("[ ")?;
                for e in elements.borrow().iter() {
                    f.write_str(format!("{}, ", e).as_str())?;
                }
                f.write_str("]")
            }
            Value::Module(_) => f.write_str("[Module]"),
        }
    }
}
