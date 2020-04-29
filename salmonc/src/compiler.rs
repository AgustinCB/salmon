use ahash::{AHashMap as HashMap};
use parser::types::{Pass, ProgramError, Statement, Expression, Literal, SourceCodeLocation, TokenType};
use smoked::instruction::{Instruction, InstructionType};

pub struct Compiler<'a> {
    pub constants: Vec<Literal<'a>>,
    instructions: Vec<Instruction>,
    last_location: Option<SourceCodeLocation<'a>>,
    locals: HashMap<usize, usize>,
    pub locations: Vec<SourceCodeLocation<'a>>,
}

impl<'a> Compiler<'a> {
    pub fn new(locals: HashMap<usize, usize>) -> Compiler<'a> {
        Compiler {
            constants: vec![],
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
        self.instructions.push(Instruction {
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
            TokenType::Plus => self.instructions.push(Instruction {
                instruction_type: InstructionType::Plus,
                location: self.locations.len() - 1,
            }),
            TokenType::Minus => self.instructions.push(Instruction {
                instruction_type: InstructionType::Minus,
                location: self.locations.len() - 1,
            }),
            TokenType::Slash => self.instructions.push(Instruction {
                instruction_type: InstructionType::Div,
                location: self.locations.len() - 1,
            }),
            TokenType::Star => self.instructions.push(Instruction {
                instruction_type: InstructionType::Mult,
                location: self.locations.len() - 1,
            }),
            TokenType::Greater => self.instructions.push(Instruction {
                instruction_type: InstructionType::Greater,
                location: self.locations.len() - 1,
            }),
            TokenType::GreaterEqual => self.instructions.push(Instruction {
                instruction_type: InstructionType::GreaterEqual,
                location: self.locations.len() - 1,
            }),
            TokenType::Less => self.instructions.push(Instruction {
                instruction_type: InstructionType::Less,
                location: self.locations.len() - 1,
            }),
            TokenType::LessEqual => self.instructions.push(Instruction {
                instruction_type: InstructionType::LessEqual,
                location: self.locations.len() - 1,
            }),
            TokenType::EqualEqual => self.instructions.push(Instruction {
                instruction_type: InstructionType::Equal,
                location: self.locations.len() - 1,
            }),
            TokenType::BangEqual => self.instructions.push(Instruction {
                instruction_type: InstructionType::NotEqual,
                location: self.locations.len() - 1,
            }),
            TokenType::And => self.instructions.push(Instruction {
                instruction_type: InstructionType::And,
                location: self.locations.len() - 1,
            }),
            TokenType::Or => self.instructions.push(Instruction {
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

    fn pass_unary(
        &mut self,
        operand: &'a Expression<'a>,
        operator: &'a TokenType<'a>
    ) -> Result<(), Vec<ProgramError<'a>>> {
        if let TokenType::Minus = operator {
            let constant = self.constant_from_literal(&Literal::Integer(0));
            self.instructions.push(Instruction {
                instruction_type: InstructionType::Constant(constant),
                location: self.locations.len() - 1,
            });
        }
        self.pass_expression(operand)?;
        match operator {
            TokenType::Plus => self.instructions.push(Instruction {
                instruction_type: InstructionType::Abs,
                location: self.locations.len() - 1,
            }),
            TokenType::Minus => self.instructions.push(Instruction {
                instruction_type: InstructionType::Minus,
                location: self.locations.len() - 1,
            }),
            TokenType::Bang => self.instructions.push(Instruction {
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
}