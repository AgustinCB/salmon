use ahash::{AHashMap as HashMap};
use parser::types::{Pass, ProgramError, Statement, Expression, Literal, SourceCodeLocation, TokenType, DataKeyword, Type};
use smoked::instruction::{Instruction, InstructionType};

#[derive(Debug, PartialEq)]
pub enum ConstantValues<'a> {
    Literal(Literal<'a>),
    Function {
        arity: usize,
        ip: usize,
        name: &'a str,
        context_variables: &'a [&'a str],
    }
}

#[derive(Clone, Copy)]
pub enum BufferSelection {
    DryRun,
    Rom,
    Function,
}

pub struct Compiler<'a> {
    pub constants: Vec<ConstantValues<'a>>,
    buffer: Vec<Instruction>,
    function_instructions: Vec<Instruction>,
    functions: HashMap<&'a str, usize>,
    instructions: Vec<Instruction>,
    last_location: Option<SourceCodeLocation<'a>>,
    locals: HashMap<usize, usize>,
    pub locations: Vec<SourceCodeLocation<'a>>,
    scopes: Vec<HashMap<&'a str, usize>>,
    toggle_selection: BufferSelection,
}

impl<'a> Compiler<'a> {
    pub fn new(locals: HashMap<usize, usize>) -> Compiler<'a> {
        Compiler {
            buffer: vec![],
            constants: vec![],
            function_instructions: vec![],
            functions: HashMap::default(),
            instructions: vec![],
            last_location: None,
            locations: vec![],
            scopes: vec![HashMap::default()],
            toggle_selection: BufferSelection::Rom,
            locals,
        }
    }
}

impl<'a> Compiler<'a> {
    fn constant_from_literal(&mut self, value: ConstantValues<'a>) -> usize {
        match self.constants.iter().position(|i| i == &value) {
            Some(i) => i,
            None => {
                self.constants.push(value);
                self.constants.len() - 1
            }
        }
    }

    #[cfg(target_os = "macos")]
    fn get_write_syscall_number_constant(&mut self) -> usize {
        self.constant_from_literal(ConstantValues::Literal(Literal::Integer(4)))
    }

    #[cfg(not(target_os = "macos"))]
    fn get_write_syscall_number_constant(&mut self) -> usize {
        self.constant_from_literal(ConstantValues::Literal(Literal::Integer(1)))
    }

    fn toggle_selection(&mut self, selection: BufferSelection) {
        self.toggle_selection = selection;
    }

    fn add_instruction(&mut self, instruction: Instruction) {
        (match self.toggle_selection {
            BufferSelection::DryRun => &mut self.buffer,
            BufferSelection::Function => &mut self.function_instructions,
            BufferSelection::Rom => &mut self.instructions,
        }).push(instruction)
    }

    fn drain_buffer(&mut self) {
        self.instructions.extend_from_slice(&self.buffer);
        self.buffer.clear();
    }

    fn add_function<I: Iterator<Item=&'a Statement<'a>>>(
        &mut self,
        name: &'a str,
        arguments: &'a [&'a str],
        body: I,
        context_variables: &'a [&'a str],
    ) -> Result<usize, Vec<ProgramError<'a>>> {
        let constant = self.constant_from_literal(ConstantValues::Function {
            arity: arguments.len(),
            ip: self.function_instructions.len(),
            context_variables,
            name,
        });
        let previous = self.toggle_selection;
        let prev_functions_size = self.function_instructions.len();
        self.toggle_selection(BufferSelection::Function);
        let mut new_scope = HashMap::default();
        for (i, n) in context_variables.iter().cloned().enumerate() {
            new_scope.insert(n, i);
        }
        for (i, n) in arguments.iter().cloned().enumerate() {
            new_scope.insert(n, i+context_variables.len());
        }
        self.scopes.push(new_scope);
        for s in body {
            self.pass(s)?;
        }
        if prev_functions_size != self.function_instructions.len() &&
            self.function_instructions.last().map(|i| i.instruction_type != InstructionType::Return)
                .unwrap_or(true) {
            self.add_instruction(Instruction {
                instruction_type: InstructionType::Return,
                location: self.locations.len() -1,
            });
        }
        self.scopes.pop();
        self.toggle_selection(previous);
        Ok(constant)
    }

    fn add_set_instruction(&mut self, scope_id: usize, var_id: usize) {
        if scope_id == 0 {
            self.add_instruction(Instruction {
                instruction_type: InstructionType::SetGlobal(var_id),
                location: self.locations.len() - 1,
            });
        } else {
            self.add_instruction(Instruction {
                instruction_type: InstructionType::SetLocal(var_id),
                location: self.locations.len() - 1,
            });
        }
    }
}

impl<'a> Pass<'a, Vec<Instruction>> for Compiler<'a> {
    fn run(&mut self, ss: &'a [Statement<'a>]) -> Result<Vec<Instruction>, Vec<ProgramError<'a>>> {
        for s in ss {
            if self.last_location.clone().map(|l| l != s.location).unwrap_or(true) {
                self.last_location = Some(s.location.clone());
                self.locations.push(s.location.clone());
            }
            self.pass(s)?;
        }
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Return,
            location: self.locations.len() - 1,
        });
        let mut instructions = self.instructions.clone();
        for constant in self.constants.iter_mut() {
            if let ConstantValues::Function { ip, .. } = constant {
                *ip += self.instructions.len();
            }
        }
        instructions.extend_from_slice(&self.function_instructions);
        Ok(instructions)
    }

    fn pass_expression_statement(
        &mut self,
        expression: &'a Expression<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        self.pass_expression(expression)?;
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Pop,
            location: self.locations.len() - 1,
        });
        Ok(())
    }

    fn pass_expression_literal(&mut self, value: &'a Literal<'a>) -> Result<(), Vec<ProgramError<'a>>> {
        let constant_index = self.constant_from_literal(ConstantValues::Literal(value.clone()));
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Constant(constant_index),
            location: self.locations.len() - 1,
        });
        Ok(())
    }

    fn pass_uplift_function_variables(
        &mut self,
        name: &'a str,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        let constant = match self.constants.iter().position(|i| {
            return if let ConstantValues::Function { name: fname, context_variables, .. } = i {
                *fname == name && context_variables.len() > 0
            } else {
                false
            };
        }) {
            Some(i) => i,
            None => return Ok(()),
        };
        let context_variables = if let ConstantValues::Function { context_variables, .. } = &self.constants[constant] {
            *context_variables
        } else {
            return Err(vec![ProgramError {
                location: self.locations.last().unwrap().clone(),
                message: "Constant is not a function".to_string()
            }]);
        };
        let array_size = self.constant_from_literal(ConstantValues::Literal(Literal::Integer(context_variables.len() as _)));
        for context_variable in context_variables {
            let scope = (&self.scopes[1..]).iter().rev()
                .find(|scope| scope.get(context_variable).is_some())
                .map(|scope| *scope.get(context_variable).unwrap());
            if let Some(scope) = scope {
                self.add_instruction(Instruction {
                    instruction_type: InstructionType::Uplift(scope),
                    location: self.locations.len() - 1,
                });
            } else {
                return Err(vec![ProgramError {
                    location: self.locations.last().unwrap().clone(),
                    message: format!("Context variable {} was not found", context_variable),
                }]);
            }
        }
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Constant(array_size),
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::ArrayAlloc,
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::MultiArraySet,
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::AttachArray(constant),
            location: self.locations.len() - 1,
        });
        Ok(())
    }

    fn pass_variable_declaration(
        &mut self,
        identifier: &'a str,
        expression: &'a Option<Expression<'a>>,
        _statement: &'a Statement<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        let scope_id = self.scopes.len() - 1;
        let var_id = self.scopes[scope_id].len();
        self.scopes[scope_id].insert(identifier, var_id);
        if let Some(e) = expression {
            self.pass_expression(e)?;
        } else {
            let c0 = self.constant_from_literal(ConstantValues::Literal(Literal::Keyword(DataKeyword::Nil)));
            self.add_instruction(Instruction {
                instruction_type: InstructionType::Constant(c0),
                location: self.locations.len() - 1,
            });
        }
        self.add_set_instruction(scope_id, var_id);
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Pop,
            location: self.locations.len() - 1,
        });
        Ok(())
    }

    fn pass_function_declaration(
        &mut self,
        name: &'a str,
        arguments: &'a [&'a str],
        body: &'a [Box<Statement<'a>>],
        _statement: &'a Statement<'a>,
        context_variables: &'a [&'a str],
    ) -> Result<(), Vec<ProgramError<'a>>> {
        if self.scopes.len() == 1 {
            let constant = self.add_function(name, arguments, body.iter().map(|i| i.as_ref()), context_variables)?;
            self.add_instruction(Instruction {
                instruction_type: InstructionType::Constant(constant),
                location: self.locations.len() - 1,
            });
            let global_index = if let Some(global_index) = self.scopes[0].get(name) {
                *global_index
            } else {
                let global_index = self.scopes[0].len();
                self.scopes[0].insert(name, global_index);
                global_index
            };
            self.add_instruction(Instruction {
                instruction_type: InstructionType::SetGlobal(global_index),
                location: self.locations.len() - 1,
            });
            self.add_instruction(Instruction {
                instruction_type: InstructionType::Pop,
                location: self.locations.len() - 1,
            });
        }
        Ok(())
    }

    fn pass_print(&mut self, expression: &'a Expression<'a>) -> Result<(), Vec<ProgramError<'a>>> {
        let c0 = self.constant_from_literal(ConstantValues::Literal(Literal::Integer(1)));
        let syscall = self.get_write_syscall_number_constant();
        let c1 = self.constant_from_literal(ConstantValues::Literal(Literal::Integer(3)));
        let newline = self.constant_from_literal(ConstantValues::Literal(Literal::QuotedString("\n")));
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Constant(newline),
            location: self.locations.len() - 1,
        });
        self.pass_expression(expression)?;
        self.add_instruction(Instruction {
            instruction_type: InstructionType::ToStr,
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::StringConcat,
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Push,
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Strlen,
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Swap,
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Constant(c0),
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Constant(c1),
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Constant(syscall),
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Syscall,
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Pop,
            location: self.locations.len() - 1,
        });
        Ok(())
    }

    fn pass_return(&mut self, expression: &'a Option<Expression<'a>>) -> Result<(), Vec<ProgramError<'a>>> {
        if let Some(e) = expression {
            self.pass_expression(e)?;
        };
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Return,
            location: self.locations.len() - 1,
        });
        Ok(())
    }

    fn pass_while(
        &mut self,
        condition: &'a Expression<'a>,
        action: &'a Statement<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        let current_selection = self.toggle_selection;
        self.toggle_selection(BufferSelection::DryRun);
        self.pass_expression(condition)?;
        self.toggle_selection(current_selection);
        let condition_length = self.buffer.len() + 1;
        self.drain_buffer();
        self.toggle_selection(BufferSelection::DryRun);
        self.pass(action)?;
        self.toggle_selection(current_selection);
        let body_length = self.buffer.len() + 1;
        self.add_instruction(Instruction {
            instruction_type: InstructionType::JmpIfFalse(body_length),
            location: self.locations.len() - 1,
        });
        self.drain_buffer();
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Loop(condition_length + body_length),
            location: self.locations.len() - 1,
        });
        Ok(())
    }

    fn pass_checked_type(
        &mut self,
        value: &'a Expression<'a>,
        checked_type: &'a Type,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        self.pass_expression(value)?;
        let type_index = match checked_type {
            Type::Nil => 0,
            Type::Boolean => 1,
            Type::Integer => 2,
            Type::Float => 3,
            Type::String => 4,
            Type::Function => 5,
            Type::Array => 6,
            _ => {
                return Err(vec![ProgramError {
                    location: self.locations.last().unwrap().clone(),
                    message: format!("Type checking for {:?} not implemented yet", checked_type),
                }]);
            }
        };
        self.add_instruction(Instruction {
            instruction_type: InstructionType::CheckType(type_index),
            location: self.locations.len() - 1,
        });
        Ok(())
    }

    fn pass_variable_literal(
        &mut self,
        identifier: &'a str,
        expression: &'a Expression<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        if let Some(scope_id) = self.locals.get(&expression.id()).cloned() {
            let var_id = *self.scopes[scope_id].get(identifier).unwrap();
            if scope_id == 0 {
                self.add_instruction(Instruction {
                    instruction_type: InstructionType::GetGlobal(var_id),
                    location: self.locations.len() - 1,
                });
            } else {
                self.add_instruction(Instruction {
                    instruction_type: InstructionType::GetLocal(var_id),
                    location: self.locations.len() - 1,
                });
            }
            Ok(())
        } else {
            if let Some(constant) = self.functions.get(identifier).cloned() {
                self.add_instruction(Instruction {
                    instruction_type: InstructionType::Constant(constant),
                    location: self.locations.len() - 1,
                });
                Ok(())
            } else {
                Err(vec![ProgramError {
                    message: format!("Variable {} not declared", identifier),
                    location: self.locations.last().unwrap().clone(),
                }])
            }
        }
    }

    fn pass_variable_assignment(
        &mut self,
        identifier: &'a str,
        expression_value: &'a Expression<'a>,
        expression: &'a Expression<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        if let Some(scope_id) = self.locals.get(&expression.id()).cloned() {
            let var_id = match self.scopes[scope_id].get(identifier) {
                Some(id) => *id,
                None => {
                    let id = self.scopes[scope_id].len();
                    self.scopes[scope_id].insert(identifier, id);
                    id
                }
            };
            self.pass_expression(expression_value)?;
            self.add_set_instruction(scope_id, var_id);
            Ok(())
        } else {
            Err(vec![ProgramError {
                message: format!("Variable {} not declared", identifier),
                location: self.locations.last().unwrap().clone(),
            }])
        }
    }

    fn pass_binary(
        &mut self,
        left: &'a Expression<'a>,
        right: &'a Expression<'a>,
        operator: &'a TokenType<'a>
    ) -> Result<(), Vec<ProgramError<'a>>> {
        self.pass_expression(left)?;
        self.pass_expression(right)?;
        match operator {
            TokenType::Plus => self.add_instruction(Instruction {
                instruction_type: InstructionType::Plus,
                location: self.locations.len() - 1,
            }),
            TokenType::Minus => self.add_instruction(Instruction {
                instruction_type: InstructionType::Minus,
                location: self.locations.len() - 1,
            }),
            TokenType::Slash => self.add_instruction(Instruction {
                instruction_type: InstructionType::Div,
                location: self.locations.len() - 1,
            }),
            TokenType::Star => self.add_instruction(Instruction {
                instruction_type: InstructionType::Mult,
                location: self.locations.len() - 1,
            }),
            TokenType::Greater => self.add_instruction(Instruction {
                instruction_type: InstructionType::Greater,
                location: self.locations.len() - 1,
            }),
            TokenType::GreaterEqual => self.add_instruction(Instruction {
                instruction_type: InstructionType::GreaterEqual,
                location: self.locations.len() - 1,
            }),
            TokenType::Less => self.add_instruction(Instruction {
                instruction_type: InstructionType::Less,
                location: self.locations.len() - 1,
            }),
            TokenType::LessEqual => self.add_instruction(Instruction {
                instruction_type: InstructionType::LessEqual,
                location: self.locations.len() - 1,
            }),
            TokenType::EqualEqual => self.add_instruction(Instruction {
                instruction_type: InstructionType::Equal,
                location: self.locations.len() - 1,
            }),
            TokenType::BangEqual => self.add_instruction(Instruction {
                instruction_type: InstructionType::NotEqual,
                location: self.locations.len() - 1,
            }),
            TokenType::And => self.add_instruction(Instruction {
                instruction_type: InstructionType::And,
                location: self.locations.len() - 1,
            }),
            TokenType::Or => self.add_instruction(Instruction {
                instruction_type: InstructionType::Or,
                location: self.locations.len() - 1,
            }),
            t => Err(vec![ProgramError {
                message: format!("Invalid operator {:?}", t),
                location: self.locations.last().unwrap().clone(),
            }])?,
        }
        Ok(())
    }

    fn pass_call(
        &mut self,
        callee: &'a Expression<'a>,
        arguments: &'a [Box<Expression<'a>>],
        _expression_id: usize,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        arguments
            .iter()
            .map(|a| self.pass_expression(a))
            .collect::<Result<Vec<()>, Vec<ProgramError>>>()?;
        self.pass_expression(callee)?;
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Call,
            location: self.locations.len() - 1,
        });
        Ok(())
    }

    fn pass_conditional(
        &mut self,
        condition: &'a Expression<'a>,
        then_branch: &'a Expression<'a>,
        else_branch: &'a Expression<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        self.pass_expression(condition)?;
        let current_selection = self.toggle_selection;
        self.toggle_selection(BufferSelection::DryRun);
        self.pass_expression(then_branch)?;
        self.toggle_selection(current_selection);
        self.add_instruction(Instruction {
            instruction_type: InstructionType::JmpIfFalse(self.buffer.len() + 1),
            location: self.locations.len() - 1,
        });
        self.drain_buffer();
        self.toggle_selection(BufferSelection::DryRun);
        self.pass_expression(else_branch)?;
        self.toggle_selection(current_selection);
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Jmp(self.buffer.len()),
            location: self.locations.len() - 1,
        });
        self.drain_buffer();
        Ok(())
    }

    fn pass_unary(
        &mut self,
        operand: &'a Expression<'a>,
        operator: &'a TokenType<'a>
    ) -> Result<(), Vec<ProgramError<'a>>> {
        if let TokenType::Minus = operator {
            let constant = self.constant_from_literal(ConstantValues::Literal(Literal::Integer(0)));
            self.add_instruction(Instruction {
                instruction_type: InstructionType::Constant(constant),
                location: self.locations.len() - 1,
            });
        }
        self.pass_expression(operand)?;
        match operator {
            TokenType::Plus => self.add_instruction(Instruction {
                instruction_type: InstructionType::Abs,
                location: self.locations.len() - 1,
            }),
            TokenType::Minus => self.add_instruction(Instruction {
                instruction_type: InstructionType::Minus,
                location: self.locations.len() - 1,
            }),
            TokenType::Bang => self.add_instruction(Instruction {
                instruction_type: InstructionType::Not,
                location: self.locations.len() - 1,
            }),
            t => Err(vec![ProgramError {
                message: format!("Invalid unary operator {:?}", t),
                location: self.locations.last().unwrap().clone(),
            }])?,
        }
        Ok(())
    }

    fn pass_anonymous_function(
        &mut self,
        _arguments: &'a [&'a str],
        _body: &'a [Statement<'a>],
        _expression: &'a Expression<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        Ok(())
    }

    fn pass_repeated_element_array(&mut self, element: &'a Expression<'a>, length: &'a Expression<'a>) -> Result<(), Vec<ProgramError<'a>>> {
        self.pass_expression(element)?;
        self.pass_expression(length)?;
        self.add_instruction(Instruction {
            instruction_type: InstructionType::ArrayAlloc,
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::RepeatedArraySet,
            location: self.locations.len() - 1,
        });
        Ok(())
    }

    fn pass_array(&mut self, elements: &'a [Box<Expression<'a>>]) -> Result<(), Vec<ProgramError<'a>>> {
        for element in elements.iter().rev() {
            self.pass_expression(element)?;
        }
        let c = self.constant_from_literal(ConstantValues::Literal(Literal::Integer(elements.len() as _)));
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Constant(c),
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::ArrayAlloc,
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::MultiArraySet,
            location: self.locations.len() - 1,
        });
        Ok(())
    }

    fn pass_array_element(&mut self, array: &'a Expression<'a>, index: &'a Expression<'a>) -> Result<(), Vec<ProgramError<'a>>> {
        self.pass_expression(index)?;
        self.pass_expression(array)?;
        self.add_instruction(Instruction {
            instruction_type: InstructionType::ArrayGet,
            location: self.locations.len() - 1,
        });
        Ok(())
    }

    fn pass_array_element_set(&mut self, array: &'a Expression<'a>, index: &'a Expression<'a>, value: &'a Expression<'a>) -> Result<(), Vec<ProgramError<'a>>> {
        self.pass_expression(value)?;
        self.pass_expression(index)?;
        self.pass_expression(array)?;
        self.add_instruction(Instruction {
            instruction_type: InstructionType::ArraySet,
            location: self.locations.len() - 1,
        });
        Ok(())
    }
}
