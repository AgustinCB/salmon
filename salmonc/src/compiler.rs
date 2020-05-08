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
            locals,
        }
    }
}

impl<'a> Compiler<'a> {
    fn constant_from_literal(&mut self, value: &'a Literal<'a>) -> usize {
        match self.constants.iter().position(|i| i == value) {
            Some(i) => i,
            None => {
                self.constants.push(value.clone());
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

    fn pass_expression_literal(&mut self, value: &'a Literal<'a>) -> Result<(), Vec<ProgramError<'a>>> {
        let constant_index = self.constant_from_literal(value);
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Constant(constant_index),
            location: self.locations.len() - 1,
        });
        Ok(())
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
            let constant = self.constant_from_literal(&Literal::Integer(0));
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

    fn pass_array(&mut self, elements: &'a [Box<Expression<'a>>]) -> Result<(), Vec<ProgramError<'a>>> {
        for element in elements.iter().rev() {
            self.pass_expression(element)?;
        }
        let c = match self.constants.iter().position(|i| i == &Literal::Integer(elements.len() as _)) {
            Some(i) => i,
            None => {
                self.constants.push(Literal::Integer(elements.len() as _));
                self.constants.len() - 1
            }
        };
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