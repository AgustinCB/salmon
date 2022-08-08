use parser::types::{Pass, ProgramError, Statement, Expression, Literal, SourceCodeLocation, TokenType, DataKeyword, Type, StatementType, ExpressionType, FunctionHeader};
use smoked::instruction::{Instruction, InstructionType};
use std::collections::{BTreeMap, HashMap, HashSet};
use crate::lambda_lifting::{variable_or_module_name, leak_reference};
use std::iter::FromIterator;

#[derive(Debug, PartialEq)]
pub enum ConstantValues<'a> {
    Literal(Literal<'a>),
    Function {
        arity: usize,
        ip: usize,
        name: &'a str,
        context_variables: &'a [&'a str],
    },
    Class {
        name: &'a str,
    }
}

#[derive(Clone, Copy)]
pub enum BufferSelection {
    DryRun,
    Rom,
    Function,
}

type CompilerResult<'a, R> = Result<R, Vec<ProgramError<'a>>>;

#[derive(Debug)]
pub struct ClassMembers<'a> {
    methods: HashMap<&'a str, &'a str>,
    getters: HashMap<&'a str, &'a str>,
    setters: HashMap<&'a str, &'a str>,
    static_methods: HashMap<&'a str, &'a str>,
}

pub struct TraitMembers<'a> {
    methods: &'a [FunctionHeader<'a>],
    getters: &'a [FunctionHeader<'a>],
    setters: &'a [FunctionHeader<'a>],
    static_methods: &'a [FunctionHeader<'a>],
}

impl<'a> ClassMembers<'a> {
    pub(crate) fn length(&self) -> usize {
        self.methods.len() + self.getters.len() + self.setters.len() + self.static_methods.len()
    }
    pub(crate) fn for_each_member<F: FnMut(&'a str, &'a str) -> ()>(&self, mut action: F) {
        for ms in vec![&self.methods, &self.getters, &self.setters, &self.static_methods] {
            for (key, value) in ms {
                action(key, value);
            }
        }
    }

    pub(crate) fn for_each_member_in_order<F: FnMut(&'a str, &'a str) -> ()>(&self, mut action: F) {
        let mut ordered_map = BTreeMap::default();
        for ms in vec![&self.methods, &self.getters, &self.setters, &self.static_methods] {
            for (key, value) in ms {
                ordered_map.insert(key, value);
            }
        }
        for (key, value) in ordered_map {
            action(*key, *value)
        }
    }
}

pub struct Compiler<'a> {
    pub constants: Vec<ConstantValues<'a>>,
    buffer: Vec<Instruction>,
    pub class_members: HashMap<&'a str, ClassMembers<'a>>,
    traits: HashMap<&'a str, TraitMembers<'a>>,
    function_instructions: Vec<Instruction>,
    instructions: Vec<Instruction>,
    locals: HashMap<usize, usize>,
    pub locations: Vec<SourceCodeLocation<'a>>,
    scopes: Vec<HashMap<&'a str, usize>>,
    toggle_selection: BufferSelection,
}

impl<'a> Compiler<'a> {
    pub fn new(locals: HashMap<usize, usize>) -> Compiler<'a> {
        Compiler {
            buffer: vec![],
            class_members: HashMap::default(),
            constants: vec![],
            function_instructions: vec![],
            instructions: vec![],
            locations: vec![],
            scopes: vec![HashMap::default()],
            toggle_selection: BufferSelection::Rom,
            traits: HashMap::default(),
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
        let buffer = match self.toggle_selection {
            BufferSelection::DryRun => panic!("WHAT YOU DOING"),
            BufferSelection::Function => &mut self.function_instructions,
            BufferSelection::Rom => &mut self.instructions,
        };
        buffer.extend_from_slice(&self.buffer);
        self.buffer.clear();
    }

    fn add_function<I: Iterator<Item=&'a Statement<'a>>>(
        &mut self,
        name: &'a str,
        arguments: &'a [&'a str],
        body: I,
        context_variables: &'a [&'a str],
    ) -> CompilerResult<'a, usize> {
        let constant = self.constant_from_literal(ConstantValues::Function {
            arity: arguments.len(),
            ip: self.function_instructions.len(),
            context_variables,
            name,
        });
        let previous = self.toggle_selection;
        let prev_functions_size = self.function_instructions.len(); self.toggle_selection(BufferSelection::Function);
        let mut new_scope = HashMap::default();
        for (i, n) in arguments.iter().cloned().enumerate() {
            new_scope.insert(n, i);
        }
        for (i, n) in context_variables.iter().cloned().enumerate() {
            new_scope.insert(n, i+arguments.len());
        }
        self.scopes.push(new_scope);
        for s in body {
            self.pass(s)?;
        }
        if prev_functions_size != self.function_instructions.len() &&
            self.function_instructions.last().map(|i| i.instruction_type != InstructionType::Return)
                .unwrap_or(true) {
            self.pass_return(&None)?;
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

    fn class_members_to_vec(&mut self, statements: &'a [Box<Statement<'a>>], prefix: Option<&'static str>) -> CompilerResult<'a, HashMap<&'a str, &'a str>> {
        let mut members = HashMap::default();
        for s in statements {
            if let StatementType::Expression { expression, .. } = &s.statement_type {
                if let ExpressionType::Binary {
                    right: box Expression {
                        expression_type: ExpressionType::VariableLiteral { identifier: name },
                        ..
                    },
                    operator: TokenType::Comma,
                    left: box Expression {
                        expression_type: ExpressionType::VariableLiteral { identifier: new_name },
                        ..
                    }
                } = &expression.expression_type {
                    let _ = self.constant_from_literal(ConstantValues::Literal(
                        Literal::QuotedString(*new_name)
                    ));
                    let storing_name = leak_reference(format!("{}{}", prefix.unwrap_or(""), name)).as_str();
                    let _ = self.constant_from_literal(ConstantValues::Literal(
                        Literal::QuotedString(*name)
                    ));
                    let _ = self.constant_from_literal(ConstantValues::Literal(
                        Literal::QuotedString(storing_name)
                    ));
                    members.insert(storing_name, *new_name);
                } else {
                    return Err(self.create_single_error(format!("Expected binary expression on class member, got {:?}", expression.expression_type)))
                }
            } else {
                return Err(self.create_single_error(format!("Expected expression statement on class member, got {:?}", s.statement_type)))
            }
        }
        Ok(members)
    }

    fn validate_trait_functions(
        &self,
        trait_functions: &'a [FunctionHeader<'a>],
        class_functions: &'a [Box<Statement<'a>>],
    ) -> CompilerResult<'a, ()> {
        let trait_functions_set: HashSet<&str> = HashSet::from_iter(
            trait_functions.iter()
                .map(|tf| tf.name)
        );
        let functions_in_implementation = HashSet::from_iter(
           class_functions.iter()
               .map(|s|
                   if let StatementType::Expression { expression } = &s.statement_type {
                       if let ExpressionType::Binary { right, ..} = &expression.expression_type {
                           if let ExpressionType::VariableLiteral { identifier } = &right.expression_type {
                               identifier.clone()
                           } else {
                               panic!("Variable Literal expected, got {:?}", right);
                           }
                       } else {
                           panic!("Binary expression expected, got {:?}", expression);
                       }
                   } else {
                       panic!("Expression statement expected, got {:?}", s);
                   }
               )
        );
        if trait_functions_set != functions_in_implementation {
            Err(self.create_single_error(format!(
                "Expected functions: {:?}, got {:?}", trait_functions_set, functions_in_implementation
            )))
        } else {
            Ok(())
        }
    }

    fn validate_trait_implementation(
        &self,
        trait_info: &TraitMembers<'a>,
        methods: &'a [Box<Statement<'a>>],
        getters: &'a [Box<Statement<'a>>],
        setters: &'a [Box<Statement<'a>>],
        static_methods: &'a [Box<Statement<'a>>],
    ) -> CompilerResult<'a, ()> {
        self.validate_trait_functions(trait_info.methods, methods)?;
        self.validate_trait_functions(trait_info.getters, getters)?;
        self.validate_trait_functions(trait_info.setters, setters)?;
        self.validate_trait_functions(trait_info.static_methods, static_methods)?;
        Ok(())
    }

    fn update_class(
        &mut self,
        class_name: &'a str,
        methods: &HashMap<&'a str, &'a str>,
    ) -> CompilerResult<'a, ()> {
        let class_global = self.scopes[0].get(class_name).unwrap().clone();
        let location = self.locations.len() - 1;
        for (method_name, global_name) in methods {
            let function_global = self.scopes[0].get(global_name).unwrap().clone();
            let method_name_constant = self.constant_from_literal(ConstantValues::Literal(Literal::QuotedString(method_name)));
            self.add_instruction(Instruction {
                instruction_type: InstructionType::GetGlobal(function_global),
                location,
            });
            self.add_instruction(Instruction {
                instruction_type: InstructionType::Constant(method_name_constant),
                location,
            });
            self.add_instruction(Instruction {
                instruction_type: InstructionType::GetGlobal(class_global),
                location,
            });
            self.add_instruction(Instruction {
                instruction_type: InstructionType::ObjectSet,
                location,
            });
            self.add_instruction(Instruction {
                instruction_type: InstructionType::Swap,
                location,
            });
            self.add_instruction(Instruction {
                instruction_type: InstructionType::Pop,
                location,
            });
            self.add_instruction(Instruction {
                instruction_type: InstructionType::SetGlobal(class_global),
                location,
            });
            self.add_instruction(Instruction {
                instruction_type: InstructionType::Pop,
                location,
            });
        }
        Ok(())
    }

    fn store_class_members(
        &mut self,
        class_name: &'a str,
        methods: &'a [Box<Statement<'a>>],
        getters: &'a [Box<Statement<'a>>],
        setters: &'a [Box<Statement<'a>>],
        static_methods: &'a [Box<Statement<'a>>],
    ) -> CompilerResult<'a, ()> {
        let methods = self.class_members_to_vec(methods, None)?;
        let getters = self.class_members_to_vec(getters, Some("@getter_"))?;
        let setters = self.class_members_to_vec(setters, Some("@setter_"))?;
        let static_methods = self.class_members_to_vec(static_methods, Some("@static_"))?;
        let members = ClassMembers { methods, getters, setters, static_methods };
        self.class_members.insert(class_name, members);
        Ok(())
    }

    fn create_single_error(&self, message: String) -> Vec<ProgramError<'a>> {
        vec![ProgramError {
            location: self.locations.last().unwrap().clone(),
            message
        }]
    }

    fn new_global(&mut self, name: &'a str) -> usize {
        if let Some(global_index) = self.scopes[0].get(name) {
            *global_index
        } else {
            let global_index = self.scopes[0].len();
            self.scopes[0].insert(name, global_index);
            global_index
        }
    }

    fn expression_or_nil(&mut self, expression: &'a Option<Expression<'a>>) -> CompilerResult<'a, ()> {
        if let Some(e) = expression {
            self.pass_expression(e)?;
        } else {
            let c0 = self.constant_from_literal(ConstantValues::Literal(Literal::Keyword(DataKeyword::Nil)));
            self.add_instruction(Instruction {
                instruction_type: InstructionType::Constant(c0),
                location: self.locations.len() - 1,
            });
        }
        Ok(())
    }

    fn save_trait_constants(&mut self, headers: &[FunctionHeader], prefix: Option<&'a str>) {
        for fh in headers.iter() {
            let storing_name = leak_reference(format!("{}{}", prefix.unwrap_or(""), fh.name)).as_str();
            self.constant_from_literal(ConstantValues::Literal(Literal::QuotedString(
                storing_name
            )));
        }
    }
}

impl<'a> Pass<'a, Vec<Instruction>> for Compiler<'a> {
    fn run(&mut self, ss: &'a [Statement<'a>]) -> Result<Vec<Instruction>, Vec<ProgramError<'a>>> {
        for s in ss {
            if let StatementType::VariableDeclaration { name, .. } = s.statement_type {
                let var_id = self.scopes[0].len();
                self.scopes[0].insert(name, var_id);
            }
        }
        for s in ss {
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

    fn before_pass(&mut self, s: &Statement<'a>) {
        if !self.locations.contains(&s.location) {
            self.locations.push(s.location.clone());
        }
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

    fn pass_uplift_class_variables(&mut self, name: &'a str) -> Result<(), Vec<ProgramError<'a>>> {
        let nil_constant = self.constant_from_literal(ConstantValues::Literal(Literal::Keyword(DataKeyword::Nil)));
        let class_constant = self.constant_from_literal(ConstantValues::Class { name });
        let members = self.class_members.get(name).ok_or(self.create_single_error(format!("Class {} not declared yet", name)))?;
        let mut names = vec![];
        members.for_each_member(|k, m| {
            names.push((k.clone(), m.clone()));
        });
        for (key, name) in names {
            let key_constant = self.constant_from_literal(ConstantValues::Literal(Literal::QuotedString(key)));
            self.pass_uplift_function_variables(name)?;
            self.add_instruction(Instruction {
                instruction_type: InstructionType::Duplicate,
                location: self.locations.len() - 1,
            });
            self.add_instruction(Instruction {
                instruction_type: InstructionType::JmpIfFalse(4),
                location: self.locations.len() - 1,
            });
            self.add_instruction(Instruction {
                instruction_type: InstructionType::Constant(key_constant),
                location: self.locations.len() - 1,
            });
            self.add_instruction(Instruction {
                instruction_type: InstructionType::Constant(class_constant),
                location: self.locations.len() - 1,
            });
            self.add_instruction(Instruction {
                instruction_type: InstructionType::ObjectSet,
                location: self.locations.len() - 1,
            });
            self.add_instruction(Instruction {
                instruction_type: InstructionType::Pop,
                location: self.locations.len() - 1,
            });
            self.add_instruction(Instruction {
                instruction_type: InstructionType::Pop,
                location: self.locations.len() - 1,
            });
        }
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Constant(nil_constant),
            location: self.locations.len() - 1,
        });
        Ok(())
    }

    fn pass_uplift_function_variables(
        &mut self,
        name: &'a str,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        let nil_constant = self.constant_from_literal(ConstantValues::Literal(Literal::Keyword(DataKeyword::Nil)));
        let global = match self.scopes[0].iter().filter_map(|(fname, value)| {
            if *fname == name {
                Some(value)
            } else {
                None
            }
        }).next() {
            Some(i) => *i,
            None => {
                self.add_instruction(Instruction {
                    instruction_type: InstructionType::Constant(nil_constant),
                    location: self.locations.len() - 1,
                });
                return Ok(())
            },
        };
        let constant = match self.constants.iter().position(|i| {
            return if let ConstantValues::Function { name: fname, context_variables, .. } = i {
                *fname == name && context_variables.len() > 0
            } else {
                false
            };
        }) {
            Some(i) => i,
            None => {
                self.add_instruction(Instruction {
                    instruction_type: InstructionType::Constant(nil_constant),
                    location: self.locations.len() - 1,
                });
                return Ok(())
            },
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
            let local_in_scope = (&self.scopes[0..]).iter().rev()
                .find(|scope| scope.get(context_variable).is_some())
                .map(|scope| *scope.get(context_variable).unwrap());
            if let Some(local) = local_in_scope {
                self.add_instruction(Instruction {
                    instruction_type: InstructionType::Uplift(local),
                    location: self.locations.len() - 1,
                });
            } else {
                return Err(vec![ProgramError {
                    location: self.locations.last().unwrap().clone(),
                    message: format!("Context variable {} was not found in function {}", context_variable, name),
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
            instruction_type: InstructionType::AttachArray(global),
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
        let var_id = if scope_id > 0 {
            self.scopes[scope_id].len()
        } else {
            self.scopes[scope_id][identifier]
        };
        if scope_id > 0 {
            self.scopes[scope_id].insert(identifier, var_id);
        }
        self.expression_or_nil(expression)?;
        self.add_set_instruction(scope_id, var_id);
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Pop,
            location: self.locations.len() - 1,
        });
        Ok(())
    }

    fn pass_class_declaration(
        &mut self,
        name: &'a str,
        properties: &'a [Box<Statement<'a>>],
        methods: &'a [Box<Statement<'a>>],
        static_methods: &'a [Box<Statement<'a>>],
        setters: &'a [Box<Statement<'a>>],
        getters: &'a [Box<Statement<'a>>],
        superclass: &'a Option<Expression<'a>>,
        _statement: &'a Statement<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        self.store_class_members(name, methods, getters, setters, static_methods)?;
        let constant = self.constant_from_literal(ConstantValues::Class {
            name,
        });
        let name_constant = self.constant_from_literal(ConstantValues::Literal(Literal::QuotedString(name)));
        let class_constant = self.constant_from_literal(
            ConstantValues::Literal(Literal::QuotedString("@class")),
        );
        let global_index = self.scopes[0].len();
        self.scopes[0].insert(name, global_index);
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Constant(name_constant),
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Constant(class_constant),
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Constant(constant),
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::AddTag,
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::AddTag,
            location: self.locations.len() - 1,
        });
        if let Some(e) = superclass {
            let super_constant = self.constant_from_literal(ConstantValues::Literal(Literal::QuotedString("super")));
            let class_constant = self.constant_from_literal(
                ConstantValues::Literal(Literal::QuotedString("@class")),
            );
            self.add_instruction(Instruction {
                instruction_type: InstructionType::Constant(class_constant),
                location: self.locations.len() - 1,
            });
            self.pass_expression(e)?;
            self.add_instruction(Instruction {
                instruction_type: InstructionType::RemoveTag,
                location: self.locations.len() - 1,
            });
            self.add_instruction(Instruction {
                instruction_type: InstructionType::Call,
                location: self.locations.len() - 1,
            });
            self.add_instruction(Instruction {
                instruction_type: InstructionType::Swap,
                location: self.locations.len() - 1,
            });
            self.add_instruction(Instruction {
                instruction_type: InstructionType::Constant(super_constant),
                location: self.locations.len() - 1,
            });
            self.add_instruction(Instruction {
                instruction_type: InstructionType::Swap,
                location: self.locations.len() - 1,
            });
            self.add_instruction(Instruction {
                instruction_type: InstructionType::ObjectSet,
                location: self.locations.len() - 1,
            });
            self.add_instruction(Instruction {
                instruction_type: InstructionType::Swap,
                location: self.locations.len() - 1,
            });
            self.add_instruction(Instruction {
                instruction_type: InstructionType::ObjectMerge,
                location: self.locations.len() - 1,
            });
        }
        for property in properties {
            if let StatementType::VariableDeclaration { expression, name} = &property.statement_type {
                self.locations.push(property.location.clone());
                self.expression_or_nil(expression)?;
                self.add_instruction(Instruction {
                    instruction_type: InstructionType::Swap,
                    location: self.locations.len() - 1,
                });
                let property_constant = self.constant_from_literal(ConstantValues::Literal(Literal::QuotedString(name)));
                self.add_instruction(Instruction {
                    instruction_type: InstructionType::Constant(property_constant),
                    location: self.locations.len() - 1,
                });
                self.add_instruction(Instruction {
                    instruction_type: InstructionType::Swap,
                    location: self.locations.len() - 1,
                });
                self.add_instruction(Instruction {
                    instruction_type: InstructionType::ObjectSet,
                    location: self.locations.len() - 1,
                });
                self.add_instruction(Instruction {
                    instruction_type: InstructionType::Swap,
                    location: self.locations.len() - 1,
                });
                self.add_instruction(Instruction {
                    instruction_type: InstructionType::Pop,
                    location: self.locations.len() - 1,
                });
            } else {
                Err(self.create_single_error("Expecting variable declaration statement".to_owned()))?;
            }
        }
        self.add_instruction(Instruction {
            instruction_type: InstructionType::SetGlobal(global_index),
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Pop,
            location: self.locations.len() - 1,
        });
        Ok(())
    }

    fn pass_trait_declaration(
        &mut self,
        name: &'a str,
        statement: &'a Statement<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        if let StatementType::TraitDeclaration {
            methods, getters, setters, static_methods, ..
        } = &statement.statement_type {
            self.save_trait_constants(methods, None);
            self.save_trait_constants(getters, Some("@getter_"));
            self.save_trait_constants(setters, Some("@setter_"));
            self.save_trait_constants(static_methods, Some("@static_"));
            self.traits.insert(name, TraitMembers {
                methods, getters, setters, static_methods,
            });
            let trait_constant = self.constant_from_literal(
                ConstantValues::Literal(Literal::QuotedString("@trait")),
            );
            let zero_constant = self.constant_from_literal(
                ConstantValues::Literal(Literal::Integer(0)),
            );
            let global_index = self.new_global(name);
            self.add_instruction(Instruction {
                instruction_type: InstructionType::Constant(trait_constant),
                location: self.locations.len() - 1,
            });
            self.add_instruction(Instruction {
                instruction_type: InstructionType::Constant(zero_constant),
                location: self.locations.len() - 1,
            });
            self.add_instruction(Instruction {
                instruction_type: InstructionType::ObjectAlloc,
                location: self.locations.len() - 1,
            });
            self.add_instruction(Instruction {
                instruction_type: InstructionType::AddTag,
                location: self.locations.len() - 1,
            });
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

    fn pass_trait_implementation(
        &mut self,
        class_name: &'a Expression<'a>,
        trait_name: &'a Expression<'a>,
        methods: &'a [Box<Statement<'a>>],
        static_methods: &'a [Box<Statement<'a>>],
        setters: &'a [Box<Statement<'a>>],
        getters: &'a [Box<Statement<'a>>],
        _statement: &'a Statement<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        if let Some(trait_info) = self.traits.get(variable_or_module_name(trait_name)?) {
            self.validate_trait_implementation(trait_info, methods, getters, setters, static_methods)?;
        } else {
            Err(self.create_single_error(format!("Trait does not exist: {:?}", trait_name)))?;
        }
        let new_methods = self.class_members_to_vec(methods, None)?;
        let new_getters = self.class_members_to_vec(getters, Some("@getter_"))?;
        let new_setters = self.class_members_to_vec(setters, Some("@setter_"))?;
        let new_static_methods = self.class_members_to_vec(static_methods, Some("@static_"))?;
        let class_name_value = variable_or_module_name(class_name)?;
        if let Some(class_members) = self.class_members.get_mut(class_name_value) {
            class_members.methods.extend(new_methods.iter());
            class_members.getters.extend(new_getters.iter());
            class_members.setters.extend(new_setters.iter());
            class_members.static_methods.extend(new_static_methods.iter());
            self.update_class(class_name_value, &new_methods)?;
            self.update_class(class_name_value, &new_getters)?;
            self.update_class(class_name_value, &new_setters)?;
            self.update_class(class_name_value, &new_static_methods)?;
            let class_name_value = variable_or_module_name(class_name)?;
            let trait_name_value = variable_or_module_name(trait_name)?;
            let global = *self.scopes[0].get(class_name_value).unwrap();
            let trait_constant = self.constant_from_literal(ConstantValues::Literal(Literal::QuotedString(
                trait_name_value
            )));
            self.add_instruction(Instruction {
                instruction_type: InstructionType::Constant(trait_constant),
                location: self.locations.len() - 1,
            });
            self.add_instruction(Instruction {
                instruction_type: InstructionType::GetGlobal(global),
                location: self.locations.len() - 1,
            });
            self.add_instruction(Instruction {
                instruction_type: InstructionType::AddTag,
                location: self.locations.len() - 1,
            });
            self.add_instruction(Instruction {
                instruction_type: InstructionType::SetGlobal(global),
                location: self.locations.len() - 1,
            });
            self.add_instruction(Instruction {
                instruction_type: InstructionType::Pop,
                location: self.locations.len() - 1,
            });
            Ok(())
        } else {
            Err(self.create_single_error(format!("class does not exist {:?}", class_name)))
        }
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
            let global_index = self.new_global(name);
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
        } else {
            let constant = self.constant_from_literal(ConstantValues::Literal(Literal::Keyword(DataKeyword::Nil)));
            self.add_instruction(Instruction {
                instruction_type: InstructionType::Constant(constant),
                location: self.locations.len() - 1,
            })
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
        if checked_type.is_object_type() {
            let tag_constant = match checked_type {
                Type::Class => {
                    self.constant_from_literal(
                        ConstantValues::Literal(Literal::QuotedString("@class"))
                    )
                },
                Type::Trait => {
                    self.constant_from_literal(
                        ConstantValues::Literal(Literal::QuotedString("@trait"))
                    )
                },
                Type::UserDefined(expression) => {
                    self.constant_from_literal(
                        ConstantValues::Literal(Literal::QuotedString(
                            variable_or_module_name(expression)?
                        ))
                    )
                },
                _ => panic!("Type is not object, as expected"),
            };
            self.add_instruction(Instruction {
                instruction_type: InstructionType::Constant(tag_constant),
                location: self.locations.len() - 1,
            });
            self.pass_expression(value)?;
            self.add_instruction(Instruction {
                instruction_type: InstructionType::CheckTag,
                location: self.locations.len() - 1,
            });
        } else {
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
        }
        Ok(())
    }

    fn pass_get(&mut self, callee: &'a Expression<'a>, property: &'a str) -> Result<(), Vec<ProgramError<'a>>> {
        let constant = self.constant_from_literal(ConstantValues::Literal(Literal::QuotedString(property)));
        let class_constant = self.constant_from_literal(
            ConstantValues::Literal(Literal::QuotedString("@class")),
        );
        let static_constant = self.constant_from_literal(ConstantValues::Literal(
            Literal::QuotedString(
                leak_reference(format!("{}{}", "@static_", property)).as_str()
            )
        ));
        let getter_constant = self.constant_from_literal(ConstantValues::Literal(
            Literal::QuotedString(
                leak_reference(format!("{}{}", "@getter_", property)).as_str()
            )
        ));
        self.pass_expression(callee)?;
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Duplicate,
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Constant(class_constant),
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Swap,
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::CheckTag,
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::JmpIfFalse(4),
            location: self.locations.len() - 1,
        });
        // Logic for static
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Constant(static_constant),
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Swap,
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::ObjectGet,
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Jmp(12),
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Constant(getter_constant),
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Swap,
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::ObjectHas,
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::JmpIfFalse(5),
            location: self.locations.len() - 1,
        });
        // logic for getter
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Constant(getter_constant),
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Swap,
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::ObjectGet,
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Call,
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Jmp(3),
            location: self.locations.len() - 1,
        });
        // logic for property
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Constant(constant),
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Swap,
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::ObjectGet,
            location: self.locations.len() - 1,
        });
        Ok(())
    }

    fn pass_set(
        &mut self,
        callee: &'a Expression<'a>,
        value: &'a Expression<'a>,
        property: &'a str,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        let property_constant = self.constant_from_literal(
            ConstantValues::Literal(Literal::QuotedString(property)),
        );
        let setter_constant = self.constant_from_literal(ConstantValues::Literal(
            Literal::QuotedString(
                leak_reference(format!("{}{}", "@setter_", property)).as_str()
            )
        ));
        self.pass_expression(value)?;
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Constant(setter_constant),
            location: self.locations.len() - 1,
        });
        self.pass_expression(callee)?;
        self.add_instruction(Instruction {
            instruction_type: InstructionType::ObjectHas,
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::JmpIfFalse(5),
            location: self.locations.len() - 1,
        });
        // setter logic
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Constant(setter_constant),
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Swap,
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::ObjectGet,
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Call,
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Jmp(4),
            location: self.locations.len() - 1,
        });
        // property logic
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Constant(property_constant),
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Swap,
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::ObjectSet,
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
            Err(vec![ProgramError {
                message: format!("Variable literal {} not declared", identifier),
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
                message: format!("Variable {} not declared on assignment", identifier),
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
            TokenType::Bar => {
                self.add_instruction(Instruction {
                    instruction_type: InstructionType::Swap,
                    location: self.locations.len() - 1,
                });
                self.add_instruction(Instruction {
                    instruction_type: InstructionType::Pop,
                    location: self.locations.len() - 1,
                });
            },
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
            instruction_type: InstructionType::Duplicate,
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::CheckType(7),
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::JmpIfFalse(19),
            location: self.locations.len() - 1,
        });
        let class_constant = self.constant_from_literal(
            ConstantValues::Literal(Literal::QuotedString("@class")),
        );
        let init_method = self.constant_from_literal(
            ConstantValues::Literal(Literal::QuotedString("init")),
        );
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Constant(init_method),
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Swap,
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Constant(class_constant),
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Swap,
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::RemoveTag,
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Call,
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::ObjectHas,
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::JmpIfFalse(12),
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Duplicate,
            location: self.locations.len() - 1,
        });
        let scope_id = self.scopes.len() - 1;
        let var_id = self.scopes[scope_id].len();
        let identifier = leak_reference(format!("@internal{}{}", scope_id, var_id));
        self.scopes[scope_id].insert(&identifier, var_id);
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Constant(init_method),
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Swap,
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::ObjectGet,
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Swap,
            location: self.locations.len() - 1,
        });
        self.add_set_instruction(scope_id, var_id);
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Pop,
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Call,
            location: self.locations.len() - 1,
        });
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Pop,
            location: self.locations.len() - 1,
        });
        if scope_id > 0 {
            self.add_instruction(Instruction {
                instruction_type: InstructionType::GetLocal(var_id),
                location: self.locations.len() - 1,
            });
        } else {
            self.add_instruction(Instruction {
                instruction_type: InstructionType::GetGlobal(var_id),
                location: self.locations.len() - 1,
            });
        }
        self.scopes[scope_id].remove(identifier.as_str());
        self.add_instruction(Instruction {
            instruction_type: InstructionType::Jmp(1),
            location: self.locations.len() - 1,
        });
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
