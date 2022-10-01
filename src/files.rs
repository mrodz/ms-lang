use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::{BufReader, Read};
use std::str::{FromStr, Lines};

use lazy_static::lazy_static;

use crate::files::command_types::{FunctionContext, GlobalFunctions};

use self::command_types::Command;

mod command_types {
    use super::{CompilerError, Line, Variable};
    use std::collections::HashMap;

    pub type VarMapping = HashMap<String, Variable>;
    pub type LoadedVars = Vec<Variable>;
    pub type CommandRet = Result<(), CompilerError>;
    pub type FunctionContext<'a> = (String, &'a Vec<Line>);
    pub type GlobalFunctions = HashMap<String, Vec<Line>>;
    pub type Command = fn(
        &Line,
        &mut VarMapping,
        &mut LoadedVars,
        &FunctionContext,
        &GlobalFunctions,
    ) -> CommandRet;
}

#[derive(Debug)]
pub struct Line {
    number: u32,
    command: String,
    arguments: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum Variable {
    Number(f32),
    String(String),
    Boolean(bool),
}

impl Variable {
    fn to_string(&self, append_type: bool) -> String {
        match self {
            Self::Number(n) => format!("{}{}", if append_type { "<number> " } else { "" }, n),
            Self::Boolean(b) => format!("{}{}", if append_type { "<bool> " } else { "" }, b),
            Self::String(s) => format!("{}{}", if append_type { "<str> " } else { "" }, s),
        }
    }
}

impl std::fmt::Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string(false))
    }
}

#[derive(Debug)]
pub struct CompilerError {
    message: String,
}

impl std::error::Error for CompilerError {}

impl std::fmt::Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Compiler Error: {}", self.message)
    }
}

impl CompilerError {
    fn new(message: String) -> Self {
        Self { message: message }
    }
}

lazy_static! {
    static ref COMMAND_MAP: HashMap<String, command_types::Command> = {
        let mut command_map = HashMap::<String, command_types::Command>::new();
        command_map.insert("SET".into(), commands::set as command_types::Command);
        command_map.insert("LOAD".into(), commands::load);
        command_map.insert("CALL".into(), commands::call);
        command_map.insert("CALLRM".into(), commands::call_d);
        command_map.insert("POPALL".into(), commands::pop_all);
        command_map.insert("DROPALL".into(), commands::drop_all);
        command_map.insert("DROP".into(), commands::drop);
        command_map.insert("REM".into(), commands::rem);
        command_map.insert("ADD".into(), commands::add);
        command_map.insert("SUB".into(), commands::sub);
        command_map.insert("MULT".into(), commands::mult);
        command_map.insert("DIV".into(), commands::div);
        command_map.insert("POW".into(), commands::pow);
        command_map.insert("ROOT".into(), commands::sqrt);
        command_map.insert("MOD".into(), commands::r#mod);
        command_map.insert("JMP".into(), commands::jmp);
		command_map.insert("IF".into(), commands::if_jmp);
		command_map.insert("CMP".into(), commands::cmp);
		command_map.insert("MOV".into(), commands::mov);

        command_map
    };
}

pub fn run_program(path: &String) {
    let mut variable_mapping = HashMap::<String, Variable>::new();
    let mut loaded_variables = Vec::<Variable>::new();

    fn build_program(path: &String) -> Result<GlobalFunctions, Box<dyn std::error::Error>> {
        let fio = read_file(path)?;
        let mapping = map_lines(fio.lines())?;
        Ok(mapping)
    }

    let mapping = match build_program(path) {
        Ok(m) => m,
        Err(e) => {
            println!("Error during compilation: {:?}", e);
            return;
        }
    };

    let traversed = traverse_lines(
        &mapping,
        &mapping,
        &mut variable_mapping,
        &mut loaded_variables,
    );
    if traversed.is_err() {
        println!(
            "Error during compilation: {:?}",
            traversed.err().unwrap().message
        );
        return;
    }
}

pub fn var_from_str(string: String) -> Variable {
    if let Ok(number) = string.parse::<f32>() {
        return Variable::Number(number);
    }

    if let Ok(boolean) = string.parse::<bool>() {
        return Variable::Boolean(boolean);
    }

    Variable::String(string)
}

mod commands {
    use super::Variable;
    use super::built_in_functions;
    use super::command_types::*;
    use super::execute_function;
    use super::var_from_str;
    use super::CompilerError;
    use super::Line;

    pub fn var_exists<'a>(name: &String, variables: &'a VarMapping) -> Result<&'a super::Variable, CompilerError> {
        match variables.get(name) {
			Some(v) => Ok(v),
			None => Err(CompilerError::new(format!("Variable {name} is not in scope.")))
		}
    }

    pub fn set(
        ctx: &Line,
        variables: &mut VarMapping,
        _: &mut LoadedVars,
        _function_context: &FunctionContext,
        _functions: &GlobalFunctions,
    ) -> CommandRet {
        if ctx.arguments.len() == 2 {
            let name = ctx.arguments.get(0).unwrap();
            if let Some('$') = name.chars().next() {
                let maybe_value = &*ctx.arguments.get(1).unwrap();
                let value = var_from_str(maybe_value.to_string());

                variables.insert(name.to_string(), value);
            } else {
                return Err(CompilerError::new(format!(
                    "Invalid 'SET' on line {}\r\n\tVariable name must start with '$'\r\n\tFound {}",
                    ctx.number, name
                )));
            }
        } else {
            return Err(CompilerError::new(
                format!(
                    "Invalid 'SET' on line {}\r\n\tSyntax --\r\n\tSET $VarName, VALUE",
                    ctx.number
                )
                .to_string(),
            ));
        }
        Ok(())
    }

    pub fn load(
        ctx: &Line,
        variables: &mut VarMapping,
        loaded_variables: &mut LoadedVars,
        _function_context: &FunctionContext,
        _functions: &GlobalFunctions,
    ) -> CommandRet {
        for var in ctx.arguments.as_slice() {
            if !variables.contains_key(var.as_str()) {
                return Err(CompilerError::new(format!(
                    "Variable '{}' has not been declared, but it is referenced on line {}",
                    var, ctx.number
                )));
            }

            let c = variables.get(var.as_str()).unwrap();

            loaded_variables.push(c.clone());
        }

        Ok(())
    }

    pub fn call(
        _ctx: &Line,
        _variables: &mut VarMapping,
        loaded_variables: &mut LoadedVars,
        _function_context: &FunctionContext,
        _functions: &GlobalFunctions,
    ) -> CommandRet {
        built_in_functions::print(loaded_variables);
        Ok(())
    }

    pub fn call_d(
        _ctx: &Line,
        _variables: &mut VarMapping,
        loaded_variables: &mut LoadedVars,
        _function_context: &FunctionContext,
        _functions: &GlobalFunctions,
    ) -> CommandRet {
        built_in_functions::print(loaded_variables);
        loaded_variables.clear();
        Ok(())
    }

    pub fn pop_all(
        _ctx: &Line,
        _variables: &mut VarMapping,
        loaded_variables: &mut LoadedVars,
        _function_context: &FunctionContext,
        _functions: &GlobalFunctions,
    ) -> CommandRet {
        loaded_variables.clear();
        Ok(())
    }

    pub fn drop_all(
        _ctx: &Line,
        variables: &mut VarMapping,
        _loaded_variables: &mut LoadedVars,
        _function_context: &FunctionContext,
        _functions: &GlobalFunctions,
    ) -> CommandRet {
        variables.clear();
        Ok(())
    }

    macro_rules! arithmetic {
      ($name:ident,$op:tt) => {
        pub fn $name(
        	ctx: &Line,
        	variables: &mut VarMapping,
        	_loaded_variables: &mut LoadedVars,
			_function_context: &FunctionContext,
			_functions: &GlobalFunctions
        ) -> CommandRet {
			if ctx.arguments.len() != 2 {
            	return Err(CompilerError::new(
                	format!(
                    	"Invalid '{}' on line {}\r\n\tSyntax --\r\n\tADD $VarName1, $VarName2",
                    	stringify!($name),
                    	ctx.number
                	).to_string(),
            	));
        	}

        	if let [name1, name2] = ctx.arguments.as_slice() {
            	let maybe1 = var_exists(&name1, variables);
            	let maybe2 = var_exists(&name2, variables);
            	let maybe2_is = maybe2.is_err();

            	if maybe1.is_ok() {
                	let n1 = match maybe1.unwrap() {
                    	super::Variable::Number(n1) => *n1,
                    	_ => {
                        	return Err(CompilerError::new(
                            	format!("Invalid data types on line {}: Expected <int>", ctx.number)
                                	.to_string(),
                        	))
                    	}
                	};

                	use std::str::FromStr;

	                let n2 = if !maybe2_is {
    	                match maybe2.unwrap() {
        	                super::Variable::Number(n2) => *n2,
            	            _ => {
                	            return Err(CompilerError::new(
                    	            format!(
                        	            "Invalid data types on line {}: Expected <int>",
                            	        ctx.number
                                	).to_string(),
                            	))
                        	}
                    	}
                	} else if let Ok(number) = f32::from_str(name2) {
                    	number
                	} else {
                  		println!("{:?}", maybe2);
						return Err(CompilerError::new(
                        	format!("Invalid data types on line {}: Expected <int>", ctx.number)
                            .to_string(),
                    	));
                	};

                	#[allow(mutable_borrow_reservation_conflict)]
                	variables.insert(name1.to_string(), super::Variable::Number($op(n1, n2)));
            	} else if maybe2_is {
                	return Err(CompilerError::new(format!("Invalid '{}' on line {}\r\n\tAdding {} to {}, but at least one of these names are not in scope.", stringify!($name), ctx.number, name2, name2).to_string()));
            	}
        	}

        	Ok(())
        }
      }
    }

    arithmetic!(add, (|n1, n2| n1 + n2));
    arithmetic!(sub, (|n1, n2| n1 - n2));
    arithmetic!(mult, (|n1, n2| n1 * n2));
    arithmetic!(div, (|n1, n2| n1 / n2));
    arithmetic!(pow, (|n1: f32, n2| n1.powf(n2)));
    arithmetic!(r#mod, (|n1, n2| n1 % n2));
    arithmetic!(sqrt, (|n1: f32, n2| n1.powf(1.0 / n2)));

    pub fn drop(
        ctx: &Line,
        variables: &mut VarMapping,
        _loaded_variables: &mut LoadedVars,
        _function_context: &FunctionContext,
		_functions: &GlobalFunctions
    ) -> CommandRet {
        if ctx.arguments.len() != 1 {
            return Err(CompilerError::new(
                format!(
                    "Invalid 'DROP' on line {}\r\n\tSyntax --\r\n\tDROP $VarName",
                    ctx.number
                )
                .to_string(),
            ));
        } else if let Some(_) = variables.get(ctx.arguments.get(0).unwrap()) {
            variables.remove(ctx.arguments.get(0).unwrap());
        } else {
            return Err(CompilerError::new(
                format!("Variable {} is not loaded.", ctx.arguments.get(0).unwrap()).to_string(),
            ));
        }
        variables.clear();
        Ok(())
    }

    pub fn rem(
        _ctx: &Line,
        _variables: &mut VarMapping,
        _loaded_variables: &mut LoadedVars,
        _function_context: &FunctionContext,
		_functions: &GlobalFunctions
    ) -> CommandRet {
        Ok(())
    }

	pub fn if_jmp(
		ctx: &Line,
        variables: &mut VarMapping,
        loaded_variables: &mut LoadedVars,
        _function_context: &FunctionContext,
		functions: &GlobalFunctions
	) -> CommandRet {
		if ctx.arguments.len() != 3 {
			return Err(CompilerError::new(
                format!(
                    "Invalid 'IF' on line {}\r\n\tSyntax --\r\n\tIF $BoolVarName, FunctionName1, FunctionName2",
                    ctx.number
                ).to_string(),
            ));
		}

		let predicate = var_exists(ctx.arguments.get(0).unwrap(), variables)?;

		if let Variable::Boolean(as_bool) = predicate {
			let _ = if *as_bool {
				execute_function(ctx.arguments.get(1).unwrap(), functions, variables, loaded_variables)
			} else {
				execute_function(ctx.arguments.get(2).unwrap(), functions, variables, loaded_variables)
			};
		} else {
			return Err(CompilerError::new(
				format!("Invalid data types on line {}: Expected <bool>", ctx.number)
					.to_string(),
			));
		}

		Ok(())
	}

	pub fn cmp(
		ctx: &Line,
        variables: &mut VarMapping,
        _loaded_variables: &mut LoadedVars,
        _function_context: &FunctionContext,
		_functions: &GlobalFunctions
	) -> CommandRet {
		if ctx.arguments.len() != 3 {
			return Err(CompilerError::new(
                format!(
                    "Invalid 'CMP' on line {}\r\n\tSyntax --\r\n\tCMP $DestinationVarName, $VarName1, $VarName2",
                    ctx.number
                ).to_string(),
            ));
		}

		let dest_name = ctx.arguments.get(0).unwrap();
		let _ = var_exists(dest_name, variables)?;
		let var1 = var_exists(ctx.arguments.get(1).unwrap(), variables)?;
		let var2 = var_exists(ctx.arguments.get(2).unwrap(), variables)?;

		if let (Variable::Number(n1), Variable::Number(n2)) = (var1, var2) {
			variables.insert(dest_name.to_string(), Variable::Boolean(n1 == n2));
			return Ok(())
		}

		if let (Variable::String(n1), Variable::String(n2)) = (var1, var2) {
			variables.insert(dest_name.to_string(), Variable::Boolean(n1 == n2));
			return Ok(())
		}

		if let (Variable::Boolean(n1), Variable::Boolean(n2)) = (var1, var2) {
			variables.insert(dest_name.to_string(), Variable::Boolean(n1 == n2));
			return Ok(())
		}

		Ok(())
	}

	pub fn mov(
		ctx: &Line,
        variables: &mut VarMapping,
        _loaded_variables: &mut LoadedVars,
        _function_context: &FunctionContext,
		_functions: &GlobalFunctions
	) -> CommandRet {
		if ctx.arguments.len() != 2 {
			return Err(CompilerError::new(
                format!(
                    "Invalid 'MOV' on line {}\r\n\tSyntax --\r\n\tMOV $VarName1, $VarName2",
                    ctx.number
                )
                .to_string(),
            ));
		}

		let var1_name = ctx.arguments.get(0).unwrap();
		let _ = var_exists(var1_name, variables)?;
		let var2 = var_exists(ctx.arguments.get(1).unwrap(), variables)?;
		
		variables.insert(var1_name.to_string(), var2.to_owned());
		
		Ok(())
	}

    pub fn jmp(
        ctx: &Line,
        variables: &mut VarMapping,
        loaded_variables: &mut LoadedVars,
        _function_context: &FunctionContext,
		functions: &GlobalFunctions
    ) -> CommandRet {
        if ctx.arguments.len() != 1 {
            return Err(CompilerError::new(
                format!(
                    "Invalid 'JMP' on line {}\r\n\tSyntax --\r\n\tJMP FunctionName",
                    ctx.number
                )
                .to_string(),
            ));
        }

		let _ = execute_function(&ctx.arguments[0], functions, variables, loaded_variables);

        Ok(())
    }
}

mod built_in_functions {
    use super::command_types::LoadedVars;

    pub fn print(loaded_variables: &mut LoadedVars) {
        let mut string_buf = String::new();

        for var in loaded_variables {
            string_buf.push_str(&var.to_string());
            string_buf.push(' ');
        }

        println!("{}", string_buf.trim_end())
    }
}

pub fn execute_function(
    name: &String,
    functions: &GlobalFunctions,
    variable_mapping: &mut HashMap<String, Variable>,
    loaded_variables: &mut Vec<Variable>,
) -> Result<(), CompilerError> {
    if let Some(lines) = functions.get(name) {
        for line in lines {
            let callable: Command = match COMMAND_MAP.get(line.command.as_str()) {
                Some(n) => *n,
                None => {
                    return Err(CompilerError::new(format!(
                        "Unknown command: {}",
                        line.command
                    )))
                }
            };

            let result = callable(
                &line,
                variable_mapping,
                loaded_variables,
                &(name.to_string(), lines),
                functions,
            );

            if result.is_err() {
                return Err(CompilerError::new(
                    format!("Error: {:?}", result.err().unwrap()).to_string(),
                ));
            }
        }
    }

    Ok(())
}

pub fn traverse_lines(
    lines: &GlobalFunctions,
    functions: &GlobalFunctions,
    // command_map: &HashMap<&str, Command>,
    variable_mapping: &mut HashMap<String, Variable>,
    loaded_variables: &mut Vec<Variable>,
) -> Result<(), CompilerError> {
    if !lines.contains_key("Main") {
        return Err(CompilerError::new("No Main function found.".into()));
    }

    let _ = execute_function(
        &"Main".to_string(),
        functions,
        variable_mapping,
        loaded_variables,
    );

    Ok(())
}

pub fn val_or_compiler_error<T, E>(
    result: Result<T, E>,
    message: String,
) -> Result<T, CompilerError> {
    match result {
        Ok(n) => Ok(n),
        Err(_) => Err(CompilerError::new(message)),
    }
}

pub fn next_token<T>(tokens: &mut T) -> Result<String, CompilerError>
where
    T: Iterator<Item = String>,
{
    match tokens.next() {
        Some(n) => Ok(n),
        None => Err(CompilerError::new("Early EOL".into())),
    }
}

pub fn split_string(string: &String) -> Result<Vec<String>, String> {
    let mut result = Vec::<String>::new();
    let mut buf = String::new();
    let mut in_quotes = false;
    let mut escaping = false;

    for char in string.chars() {
        if char.is_whitespace() && !in_quotes {
            if buf.len() != 0 {
                result.push(buf.to_string());
                buf.clear();
            }
            continue;
        }

        match char {
            '\\' => {
                if escaping {
                    buf.push(char);
                }
                escaping = !escaping;
                continue;
            }
            '"' => {
                if escaping {
                    buf.push(char);
                    escaping = false;
                    continue;
                }

                if in_quotes {
                    result.push(buf.to_string());
                    buf.clear();
                }

                in_quotes = !in_quotes;
                continue;
            }
            'n' => {
                if escaping {
                    buf.push('\n');
                    escaping = false;
                    continue;
                }
            }
            't' => {
                if escaping {
                    buf.push('\t');
                    escaping = false;
                    continue;
                }
            }
            _ => {
                if escaping {
                    return Err(format!("Unknown escape sequence: \\{}", char));
                }
            }
        }

        buf.push(char);
    }

    if in_quotes {
        Err("Found EOL while parsing string".to_string())
    } else {
        if buf.len() > 0 {
            result.push(buf.to_string());
        }
        drop(buf);
        Ok(result)
    }
}

pub fn map_lines(lines: Lines) -> Result<GlobalFunctions, Box<dyn std::error::Error>> {
    let mut result = Vec::new();
    let mut line_numbers = HashSet::<u32>::new();

    let mut init_fn_name: Option<String> = None;
    let mut functions = HashMap::new();

    for line in lines {
        if line.starts_with('~') && line.len() >= 2 {
            if let Some(init_fn_name) = init_fn_name {
                functions.insert(init_fn_name, result);
                result = Vec::new();
                // fn_lines.sort_by(|a, b| a.number.cmp(&b.number));
                // let m = fn_lines.into_boxed_slice();
                // functions.insert(init_fn_name, &m);
            }

            init_fn_name = Some(line[1..line.len()].to_string());
            continue;
        }

		if line.len() == 0 {
			continue;
		}

        let tokens = split_string(&line.to_string())?;

        let mut tokens = tokens.into_iter();

        let line_num = next_token(&mut tokens)?;
        let line_num_u32 = match u32::from_str(line_num.as_str()) {
            Ok(n) => n,
            Err(_) => {
                return Err(Box::new(CompilerError::new(
                    format!("Not a line number: ({})", line_num).to_owned(),
                )))
            }
        };

        if line_numbers.contains(&line_num_u32) {
            return Err(Box::new(CompilerError::new(
                format!("Line number {} declared more than once.", line_num_u32).to_owned(),
            )));
        } else {
            line_numbers.insert(line_num_u32);
        }

        let command = next_token(&mut tokens)?;

        let mut arguments = Vec::new();
        for argument in tokens {
            let mut argument = argument.to_string();
            if let Some(',') = argument.chars().next_back() {
                argument.pop();
            }

            arguments.push(argument);
        }

        result.push(Line {
            number: line_num_u32,
            command: command.to_string(),
            arguments: arguments,
        })
    }

    if let Some(fn_name) = init_fn_name {
        functions.insert(fn_name, result);
    }

    Ok(functions)
}

pub fn read_file(path: &String) -> Result<String, CompilerError> {
    let mut data = String::new();
    let f = val_or_compiler_error(
        File::open(path),
        format!("Could not open file '{}'", path).to_string(),
    )?;
    let mut br = BufReader::new(f);
    val_or_compiler_error(
        br.read_to_string(&mut data),
        format!("Could not read file '{}'", path).to_string(),
    )?;
    Ok(data)
}
