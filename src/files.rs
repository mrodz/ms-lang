use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::{BufReader, Read};
use std::str::{FromStr, Lines};

use self::command_types::Command;

mod command_types {
    use super::{CompilerError, Line, Variable};
    use std::collections::HashMap;

    pub type VarMapping = HashMap<String, Variable>;
    pub type LoadedVars = Vec<Variable>;
    pub type CommandRet = Result<(), CompilerError>;
    pub type Command = fn(&Line, &mut VarMapping, &mut LoadedVars) -> CommandRet;
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

pub fn run_program(path: &String) {
    fn build_program(path: &String) -> Result<Vec<Line>, Box<dyn std::error::Error>> {
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

    let traversed = traverse_lines(&mapping);
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

    // let chars = string.as_bytes();
    // let len = chars.len();
    // if chars[0] == b'"' && chars[len - 1] == b'"' {
    // return Some(Variable::String(string[1..len].to_string()));
    // }

    // println!("!!{}", string);

    Variable::String(string)
    // None
}

mod commands {
    use super::built_in_functions;
    use super::command_types::*;
    use super::var_from_str;
    use super::CompilerError;
    use super::Line;

    pub fn var_exists<'a>(name: &String, variables: &'a VarMapping) -> Option<&'a super::Variable> {
        // if variables.contains_key(&name) {
        // Some(variables.get(&name))
        variables.get(name)
        // } else {
        // None
        // }
    }

    pub fn set(ctx: &Line, variables: &mut VarMapping, _: &mut LoadedVars) -> CommandRet {
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
    ) -> CommandRet {
        built_in_functions::print(loaded_variables);
        Ok(())
    }

    pub fn call_d(
        _ctx: &Line,
        _variables: &mut VarMapping,
        loaded_variables: &mut LoadedVars,
    ) -> CommandRet {
        built_in_functions::print(loaded_variables);
        loaded_variables.clear();
        Ok(())
    }

    pub fn pop_all(
        _ctx: &Line,
        _variables: &mut VarMapping,
        loaded_variables: &mut LoadedVars,
    ) -> CommandRet {
        loaded_variables.clear();
        Ok(())
    }

    pub fn drop_all(
        _ctx: &Line,
        variables: &mut VarMapping,
        _loaded_variables: &mut LoadedVars,
    ) -> CommandRet {
        variables.clear();
        Ok(())
    }

    macro_rules! arithmetic {
      ($name:ident,$op:tt) => {
        pub fn $name(
          ctx: &Line,
          variables: &mut VarMapping,
          _loaded_variables: &mut LoadedVars
        ) -> CommandRet {
          if ctx.arguments.len() != 2 {
            return Err(CompilerError::new(
                format!(
                    "Invalid '{}' on line {}\r\n\tSyntax --\r\n\tADD $VarName1, $VarName2",
                    stringify!($name),
                    ctx.number
                )
                .to_string(),
            ));
        }

        if let [name1, name2] = ctx.arguments.as_slice() {
            let maybe1 = var_exists(&name1, variables);
            let maybe2 = var_exists(&name2, variables);
            let maybe2_is = maybe2.is_none();

            if maybe1.is_some() {
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

                let n2 = if maybe2.is_some() {
                    match maybe2.unwrap() {
                        super::Variable::Number(n2) => *n2,
                        _ => {
                            return Err(CompilerError::new(
                                format!(
                                    "Invalid data types on line {}: Expected <int>",
                                    ctx.number
                                )
                                .to_string(),
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
            // std::mem::drop(name2);
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
    ) -> CommandRet {
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

pub fn traverse_lines<'a>(lines: &Vec<Line>) -> Result<(), CompilerError> {
    let mut variable_mapping = HashMap::<String, Variable>::new();
    let mut loaded_variables = Vec::<Variable>::new();

    let mut command_map = HashMap::<&str, Command>::new();
    command_map.insert("SET", commands::set as command_types::Command);
    command_map.insert("LOAD", commands::load);
    command_map.insert("CALL", commands::call);
    command_map.insert("CALLRM", commands::call_d);
    command_map.insert("POPALL", commands::pop_all);
    command_map.insert("DROPALL", commands::drop_all);
    command_map.insert("DROP", commands::drop);
    command_map.insert("REM", commands::rem);
    command_map.insert("ADD", commands::add);
    command_map.insert("SUB", commands::sub);
    command_map.insert("MULT", commands::mult);
    command_map.insert("DIV", commands::div);
    command_map.insert("POW", commands::pow);
    command_map.insert("ROOT", commands::sqrt);
    command_map.insert("MOD", commands::r#mod);
  

    for line in lines {
        let callable = match command_map.get(line.command.as_str()) {
            Some(n) => *n,
            None => {
                return Err(CompilerError::new(format!(
                    "Unknown command: {}",
                    line.command
                )))
            }
        };

        let result = callable(&line, &mut variable_mapping, &mut loaded_variables);

        if result.is_err() {
            return Err(CompilerError::new(
                format!("Error: {:?}", result.err().unwrap()).to_string(),
            ));
        }
    }

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

pub fn map_lines(lines: Lines) -> Result<Vec<Line>, Box<dyn std::error::Error>> {
    let mut result = Vec::new();
    let mut line_numbers = HashSet::<u32>::new();

    for line in lines {
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

    result.sort_by(|a, b| a.number.cmp(&b.number));

    Ok(result)
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
