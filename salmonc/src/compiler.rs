use parser::types::{Pass, ProgramError, Statement};
use smoked::instruction::Instruction;
use std::collections::HashMap;

struct Compiler {
    locals: HashMap<usize, usize>,
}

impl<'a> Pass<'a, Vec<Instruction>> for Compiler {
    fn run(&mut self, ss: &'a [Statement<'a>]) -> Result<Vec<Instruction>, Vec<ProgramError<'a>>> {
        unimplemented!()
    }
}