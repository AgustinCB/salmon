use ahash::{AHashMap as HashMap};
use parser::types::{Pass, ProgramError, Statement, Expression, Literal, SourceCodeLocation, TokenType};
use smoked::instruction::{Instruction, InstructionType};

pub struct Compiler<'a> {
    pub constants: Vec<Literal<'a>>,
    buffer: Vec<Instruction>,
    dry_run: bool,
    instructions: Vec<Instruction>,
    last_location: Option<SourceCodeLocation<'a>>,
    locals: HashMap<usize, usize>,
    pub locations: Vec<SourceCodeLocation<'a>>,
    scopes: Vec<HashMap<&'a str, usize>>,
}

impl<'a> Compiler<'a> {
    pub fn new(locals: HashMap<usize, usize>) -> Compiler<'a> {
        Compiler {
            buffer: vec![],
            constants: vec![],
            dry_run: false,
            instructions: vec![],
            last_location: None,
            locations: vec![],
            scopes: vec![HashMap::default()],
            locals,
        }
    }
}

impl<'a> Compiler<'a> {
    fn constant_from_literal(&mut self, value: Literal<'a>) -> usize {
        match self.constants.iter().position(|i| i == &value) {
            Some(i) => i,
            None => {
                self.constants.push(value);
                self.constants.len() - 1
            }
        }
    }

    fn toggle_dry(&mut self) {
        self.dry_run = !self.dry_run;
    }

    fn add_instruction(&mut self, instruction: Instruction) {
        (if self.dry_run {
            &mut self.buffer
        } else {
           &mut self.instructions
        }).push(instruction);
    }

    fn drain_buffer(&mut self) {
        self.instructions.extend_from_slice(&self.buffer);
        self.buffer.clear();
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
        Ok(self.instructions.clone())
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
        let constant_index = self.constant_from_literal(value.clone());
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Constant(constant_index),
            location: self.locations.len() - 1,
        });
        Ok(())
    }

    fn pass_print(&mut self, expression: &'a Expression<'a>) -> Result<(), Vec<ProgramError<'a>>> {
        let c0 = self.constant_from_literal(Literal::Integer(1));
        let c1 = self.constant_from_literal(Literal::Integer(3));
        let newline = self.constant_from_literal(Literal::QuotedString("\n"));
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
            instruction_type: InstructionType::Constant(c0),
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

    fn pass_variable_literal(
        &mut self,
        identifier: &'a str,
        expression: &'a Expression<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        if let Some(scope_id) = self.locals.get(&expression.id()).cloned() {
            let var_id = *self.scopes[0].get(identifier).unwrap();
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
            Err(vec![ProgramError {
                message: format!("Variable {} not declared", identifier),
                location: self.locations.last().unwrap().clone(),
            }])
        }
    }

    fn pass_variable_assignment(
        &mut self,
        identifier: &'a str,
        expression_value: &'a Expression<'a>,
        expression: &'a Expression<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        if let Some(scope_id) = self.locals.get(&expression.id()).cloned() {
            let var_id = match self.scopes[0].get(identifier) {
                Some(id) => *id,
                None => {
                    let id = self.scopes[0].len();
                    self.scopes[0].insert(identifier, id);
                    id
                }
            };
            self.pass_expression(expression_value)?;
            self.add_instruction(Instruction {
                instruction_type: InstructionType::Push,
                location: self.locations.len() - 1,
            });
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

    fn pass_conditional(
        &mut self,
        condition: &'a Expression<'a>,
        then_branch: &'a Expression<'a>,
        else_branch: &'a Expression<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        self.pass_expression(condition)?;
        self.toggle_dry();
        self.pass_expression(then_branch)?;
        self.toggle_dry();
        self.add_instruction(Instruction {
            instruction_type: InstructionType::JmpIfFalse(self.buffer.len() + 1),
            location: self.locations.len() - 1,
        });
        self.drain_buffer();
        self.toggle_dry();
        self.pass_expression(else_branch)?;
        self.toggle_dry();
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
            let constant = self.constant_from_literal(Literal::Integer(0));
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
        let c = self.constant_from_literal(Literal::Integer(elements.len() as _));
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