use lazy_static::lazy_static;

use std::collections::HashMap;
use std::fs::File;
use std::io::{BufReader, Read};
use std::str::Lines;

use self::command_types::{Command, GlobalFunctions};

mod command_types {
    use super::{Line, MountError, Variable};
    use std::collections::HashMap;

    pub type VarMapping = HashMap<String, Variable>;
    pub type LoadedVars = Vec<Variable>;
    pub type CommandRet = Result<(), MountError>;
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
    Dim(Vec<Variable>),
}

impl Variable {
    fn to_string(&self, append_type: bool) -> String {
        match self {
            Self::Number(n) => format!("{}", n),
            Self::Boolean(b) => format!("{}", b),
            Self::String(s) => format!(
                "{}{}{}",
                if append_type { "\"" } else { "" },
                s,
                if append_type { "\"" } else { "" }
            ),
            Self::Dim(d) => {
                let mut res = String::from("[");

                let mut c = 0;
                for var in d {
                    c += 1;
                    let temp = var.to_string(true);

                    res.push_str(temp.as_str());
                    res.push_str(", ");
                }

                let actual = (if c > 0 {
                    res.get(0..res.len() - 2).unwrap()
                } else {
                    res.as_str()
                })
                .to_owned()
                    + "]";

                actual
            }
        }
    }
}

impl std::fmt::Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string(false))
    }
}

#[derive(Debug)]
pub struct MountError {
    message: String,
}

impl std::error::Error for MountError {}

impl std::fmt::Display for MountError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Compiler Error: {}", self.message)
    }
}

impl MountError {
    fn new(message: String) -> Self {
        Self { message: message }
    }
}

lazy_static! {
    static ref COMMAND_MAP: HashMap<String, command_types::Command> = {
        let mut command_map = HashMap::<String, command_types::Command>::new();

        macro_rules! insert_command {
            ($name:ident, $func:ident) => {
                command_map.insert(
                    stringify!($name).to_ascii_uppercase(),
                    commands::$func as command_types::Command,
                );
            };
        }

        insert_command!(SET, set);
        insert_command!(DIM, dim);
        insert_command!(LOAD, load);
        insert_command!(SYSCALL, call);
        insert_command!(SYSCALLRM, call_d);
        insert_command!(POPALL, pop_all);
        insert_command!(DROPALL, drop_all);
        insert_command!(DROP, drop);
        insert_command!(REM, rem);
        insert_command!(ADD, add);
        insert_command!(SUB, sub);
        insert_command!(MULT, mult);
        insert_command!(DIV, div);
        insert_command!(POW, pow);
        insert_command!(ROOT, sqrt);
        insert_command!(MOD, r#mod);
        insert_command!(JMP, jmp);
        insert_command!(IF, if_jmp);
        insert_command!(EQU, equ);
        insert_command!(NEQ, neq);
        insert_command!(MOV, mov);
        insert_command!(DBG, dbg);
        insert_command!(AND, and);
        insert_command!(OR, or);
        insert_command!(XOR, xor);
        insert_command!(NOT, not);
        insert_command!(LT, lt);
        insert_command!(GT, gt);
        insert_command!(LTE, lte);
        insert_command!(GTE, gte);
        insert_command!(CAST_NUMBER, cast_number);
        insert_command!(NEGATE, negate);
        insert_command!(ARG, arg);
        insert_command!(AT, at);

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
            println!("Error during mount: {:?}", e);
            return;
        }
    };

    println!("Created block mapping\r\nExecuting...\r\n");

    let traversed = traverse_lines(
        &mapping,
        &mapping,
        &mut variable_mapping,
        &mut loaded_variables,
    );

    if traversed.is_err() {
        println!(
            "Error during runtime: {:?}",
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
    use super::built_in_functions;
    use super::command_types::*;
    use super::execute_function;
    use super::var_from_str;
    use super::Line;
    use super::MountError;
    use super::Variable;

    pub fn var_exists<'a>(
        name: &String,
        variables: &'a VarMapping,
    ) -> Result<&'a super::Variable, MountError> {
        match variables.get(name) {
            Some(v) => Ok(v),
            None => Err(MountError::new(format!("Variable {name} is not in scope."))),
        }
    }

    fn index_or_val(str: &String, variables: &mut VarMapping) -> Option<usize> {
        if str.starts_with('$') {
            dbg!("var");

            if let Some(Variable::Number(var)) = variables.get(str) {
                dbg!(str);
                Some(*var as usize)
            } else {
                dbg!("not number");
                None
            }
        } else if let Ok(index) = str.parse::<usize>() {
            dbg!("num");
            return Some(index)
        } else {
            dbg!("none");
            None
        }
    }

    pub fn at(
        ctx: &Line,
        variables: &mut VarMapping,
        _loaded_variables: &mut LoadedVars,
        _function_context: &FunctionContext,
        _functions: &GlobalFunctions,
    ) -> CommandRet {
        if let (Some(var), Some(index), Some(dest)) = (
            ctx.arguments.get(0),
            ctx.arguments.get(1),
            ctx.arguments.get(2),
        ) {
            if let Some(index) = index_or_val(&index, variables) {
                if !dest.starts_with("$") {
                    return Err(MountError::new(format!(
                    "Invalid 'ARG' on line {}\r\n\tVariable name must start with '$'\r\n\tFound {}",
                    ctx.number, dest
                  )));
                }

                if let Some(d) = variables.get(var) {
                    dbg!(&d);
                    if let Variable::Dim(d) = d {
                        match d.get(index) {
                            Some(v) => {
                                dbg!("in bounds", v, &dest);
                                let cloned = v.clone();
                                variables.insert(dest.to_string(), cloned);
                            }
                            None => {
                                return Err(MountError::new(format!(
                                    "Index out of bounds on line {}: {index}",
                                    ctx.number
                                )))
                            }
                        }

                        Ok(())
                    } else {
                        dbg!(var);
                        Err(MountError::new(
                            format!("Invalid data types on line {}: Expected <dim>, found {:?}", ctx.number, d)
                                .to_string(),
                        ))
                    }
                } else {
                    return Err(MountError::new(format!(
                        "Variable '{}' has not been declared, but it is referenced on line {}",
                        var, ctx.number
                    )));
                }
            } else {
                Err(MountError::new(
                    format!("Invalid data types on line {}: Expected <int>, found {index}", ctx.number)
                        .to_string(),
                ))
            }
        } else {
            Err(MountError::new(format!(
                "Invalid 'AT' on line {}\r\n\tSyntax --\r\n\tAT $DimName, index, $dest",
                ctx.number
            )))
        }
    }

    pub fn arg(
        ctx: &Line,
        variables: &mut VarMapping,
        loaded_variables: &mut LoadedVars,
        _function_context: &FunctionContext,
        _functions: &GlobalFunctions,
    ) -> CommandRet {
        if let (Some(index), Some(dest)) = (ctx.arguments.get(0), ctx.arguments.get(1)) {
            if let Ok(index) = index.parse::<usize>() {
                if !dest.starts_with("$") {
                    return Err(MountError::new(format!(
                    "Invalid 'ARG' on line {}\r\n\tVariable name must start with '$'\r\n\tFound {}",
                    ctx.number, dest
                  )));
                }

                match loaded_variables.get(index) {
                    Some(v) => {
                        variables.insert(dest.to_string(), v.clone());
                    }
                    None => {
                        return Err(MountError::new(format!(
                            "Index out of bounds on line {}: {index}",
                            ctx.number
                        )))
                    }
                };

                Ok(())
            } else {
                Err(MountError::new(format!(
                    "Invalid data types on line {}: Expected <int>",
                    ctx.number
                )))
            }
        } else {
            Err(MountError::new(format!(
                "Invalid 'ARG' on line {}\r\n\tSyntax --\r\n\tARG index, $dest",
                ctx.number
            )))
        }
    }

    pub fn cast_number(
        ctx: &Line,
        variables: &mut VarMapping,
        _loaded_variables: &mut LoadedVars,
        _function_context: &FunctionContext,
        _functions: &GlobalFunctions,
    ) -> CommandRet {
        if let Some(name) = ctx.arguments.get(0) {
            if let Some(var) = variables.get(name) {
                let result = match var {
                    Variable::Boolean(b) => Variable::Number(if *b { 1f32 } else { 0f32 }),
                    Variable::String(s) => {
                        let maybe_var = var_from_str(s.to_string());
                        if let Variable::Number(_) = maybe_var {
                            maybe_var
                        } else {
                            return Err(MountError::new(format!(
                    "Invalid 'CAST_N' on line {}\r\n\tVariable cannot be cast to a number\r\n\t`{}` cannot be coerced.",
                    ctx.number, name
                )));
                        }
                    }
                    Variable::Number(n) => Variable::Number(*n),
                    _ => return Err(MountError::new(format!(
                        "Invalid 'CAST_N' on line {}\r\n\tVariable cannot be cast to a number\r\n\t`{}` cannot be coerced.",
                        ctx.number, name
                    )))
                };

                variables.insert(name.to_string(), result);

                Ok(())
            } else {
                Err(MountError::new(format!(
                    "Variable '{}' has not been declared, but it is referenced on line {}",
                    name, ctx.number
                )))
            }
        } else {
            Err(MountError::new(format!(
                "Invalid 'CAST_N' on line {}\r\n\tSyntax --\r\n\tCAST_N $VarName, type",
                ctx.number
            )))
        }
    }

    pub fn negate(
        ctx: &Line,
        variables: &mut VarMapping,
        _: &mut LoadedVars,
        _function_context: &FunctionContext,
        _functions: &GlobalFunctions,
    ) -> CommandRet {
        if ctx.arguments.len() != 1 {
            return Err(MountError::new(format!(
                "Invalid 'NEGATE' on line {}\r\n\tSyntax --\r\n\tNEGATE $VarName",
                ctx.number
            )));
        }

        let name = ctx.arguments.get(0).unwrap();

        if let Some(var) = variables.get(name) {
            if let Variable::Number(n) = var {
                let inverse = -*n;
                variables.insert(name.to_string(), Variable::Number(inverse));

                Ok(())
            } else {
                return Err(MountError::new(
                    format!("Invalid data types on line {}: Expected <int>", ctx.number)
                        .to_string(),
                ));
            }
        } else {
            return Err(MountError::new(format!(
                "Variable '{}' has not been declared, but it is referenced on line {}",
                name, ctx.number
            )));
        }
    }

    pub fn dim(
        ctx: &Line,
        variables: &mut VarMapping,
        _: &mut LoadedVars,
        _function_context: &FunctionContext,
        _functions: &GlobalFunctions,
    ) -> CommandRet {
        // let args = &ctx.arguments;
        // let len = args.len();
        // dbg!((len < 1));
        if ctx.arguments.len() >= 1 {
            let name = ctx.arguments.get(0).unwrap();
            if let Some('$') = name.chars().next() {
                let mut vars = vec![];

                if ctx.arguments.len() > 1 {
                    for maybe_value in ctx.arguments.get(1..).unwrap() {
                        if maybe_value.starts_with("$") {
                            if let Some(var) = variables.get(maybe_value) {
                                vars.push(var.clone());
                            } else {
                                return Err(MountError::new(format!(
                                    "Variable '{}' has not been declared, but it is referenced on line {}",
                                    maybe_value, ctx.number
                                )));
                            }
                        } else {
                            let value = var_from_str(maybe_value.to_string());
                            vars.push(value);
                        }
                    }
                }

                variables.insert(name.to_string(), Variable::Dim(vars));
            } else {
                return Err(MountError::new(format!(
                    "Invalid 'DIM' on line {}\r\n\tVariable name must start with '$'\r\n\tFound {}",
                    ctx.number, name
                )));
            }
        } else {
            return Err(MountError::new(format!(
                "Invalid 'DIM' on line {}\r\n\tSyntax --\r\n\tDIM $VarName, VALUES...",
                ctx.number
            )));
        }
        Ok(())
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
                let maybe_value = ctx.arguments.get(1).unwrap();
                let value = var_from_str(maybe_value.to_string());

                variables.insert(name.to_string(), value);
            } else {
                return Err(MountError::new(format!(
                    "Invalid 'SET' on line {}\r\n\tVariable name must start with '$'\r\n\tFound {}",
                    ctx.number, name
                )));
            }
        } else {
            return Err(MountError::new(format!(
                "Invalid 'SET' on line {}\r\n\tSyntax --\r\n\tSET $VarName, VALUE",
                ctx.number
            )));
        }
        Ok(())
    }

    pub fn dbg(
        ctx: &Line,
        _variables: &mut VarMapping,
        _loaded_variables: &mut LoadedVars,
        _function_context: &FunctionContext,
        _functions: &GlobalFunctions,
    ) -> CommandRet {
        println!(
            "[DEBUG {}] {}",
            ctx.number,
            ctx.arguments.get(0).unwrap_or(&"".to_string())
        );

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
                return Err(MountError::new(format!(
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
        ctx: &Line,
        variables: &mut VarMapping,
        loaded_variables: &mut LoadedVars,
        _function_context: &FunctionContext,
        _functions: &GlobalFunctions,
    ) -> CommandRet {
        if ctx.arguments.len() != 1 && ctx.arguments.len() != 2 {
            return Err(MountError::new(format!(
                "Invalid 'SYSCALL' on line {}\r\n\tSyntax --\r\n\tSYSCALL NativeFunctionName, OptionalReturnAddress",
                ctx.number
            )));
        }

        let func_name = ctx.arguments[0].as_str();

        let result = (match func_name {
            "Print" => built_in_functions::print,
            "PrintLn" => built_in_functions::println,
            "StrCat" => built_in_functions::str_cat,
            "CharAt" => built_in_functions::char_at,
            "StrLen" => built_in_functions::str_len,
            "Input" => built_in_functions::input,
            "DimLen" => built_in_functions::dim_len,
            "DimAt" => built_in_functions::dim_at,
            _ => {
                return Err(MountError::new(format!(
                    "{} is not a native function.",
                    func_name
                )))
            }
        })(loaded_variables);

        if let (Ok(Some(result)), Some(name)) = (result, ctx.arguments.get(1)) {
            if !variables.contains_key(name) {
                return Err(MountError::new(format!(
                    "Variable '{}' has not been declared, but it is referenced on line {}",
                    name, ctx.number
                )));
            }

            variables.insert(name.to_string(), result);
        }

        Ok(())
    }

    pub fn call_d(
        ctx: &Line,
        variables: &mut VarMapping,
        loaded_variables: &mut LoadedVars,
        function_context: &FunctionContext,
        functions: &GlobalFunctions,
    ) -> CommandRet {
        call(
            ctx,
            variables,
            loaded_variables,
            function_context,
            functions,
        )?;

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
            	return Err(MountError::new(
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
                        	return Err(MountError::new(
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
                	            return Err(MountError::new(
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
						return Err(MountError::new(
                        	format!("Invalid data types on line {}: Expected <int>", ctx.number)
                            .to_string(),
                    	));
                	};

                	#[allow(mutable_borrow_reservation_conflict)]
                	variables.insert(name1.to_string(), super::Variable::Number($op(n1, n2)));
            	} else if maybe2_is {
                	return Err(MountError::new(format!("Invalid '{}' on line {}\r\n\tAdding {} to {}, but at least one of these names are not in scope.", stringify!($name), ctx.number, name2, name2).to_string()));
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
    arithmetic!(sqrt, (|n1, n2| (n1 as f32).powf(1.0 / n2)));

    pub fn drop(
        ctx: &Line,
        variables: &mut VarMapping,
        _loaded_variables: &mut LoadedVars,
        _function_context: &FunctionContext,
        _functions: &GlobalFunctions,
    ) -> CommandRet {
        if ctx.arguments.len() != 1 {
            return Err(MountError::new(format!(
                "Invalid 'DROP' on line {}\r\n\tSyntax --\r\n\tDROP $VarName",
                ctx.number
            )));
        }
        match variables.remove(ctx.arguments.get(0).unwrap()) {
            Some(_) => Ok(()),
            None => Err(MountError::new(
                format!("Variable {} is not loaded.", ctx.arguments.get(0).unwrap()).to_string(),
            )),
        }
    }

    macro_rules! bool_comparisons {
        ($name:ident, $op:tt) => {
            pub fn $name(
                ctx: &Line,
                variables: &mut VarMapping,
                _loaded_variables: &mut LoadedVars,
                _function_context: &FunctionContext,
                _functions: &GlobalFunctions,
            ) -> CommandRet {
                if let (Some(first), Some(second)) = (ctx.arguments.get(0), ctx.arguments.get(1)) {
                    if let (Some(var1), Some(var2)) = (variables.get(first), variables.get(second))
                    {
                        if let (Variable::Boolean(var1), Variable::Boolean(var2)) = (var1, var2) {
                            let one = *var1;
                            let two = *var2;

                            variables.insert(first.to_string(), Variable::Boolean(one $op two));
                            Ok(())
                        } else {
                            Err(MountError::new(format!(
                                "Invalid data types on line {}: Expected <boolean, boolean>",
                                ctx.number
                            )))
                        }
                    } else {
                        Err(MountError::new(format!(
                            "Encountered undefined variables on line {}",
                            ctx.number
                        )))
                    }
                } else {
                    Err(MountError::new(format!(
                        "Invalid '{}' on line {}\r\n\tSyntax --\r\n\tAND $VarName1, $VarName2",
                        stringify!($name),
                        ctx.number
                    )))
                }
            }
        };
    }

    bool_comparisons!(and, &&);
    bool_comparisons!(or, ||);
    bool_comparisons!(xor, ^);

    pub fn not(
        ctx: &Line,
        variables: &mut VarMapping,
        _loaded_variables: &mut LoadedVars,
        _function_context: &FunctionContext,
        _functions: &GlobalFunctions,
    ) -> CommandRet {
        if let Some(name) = ctx.arguments.get(0) {
            if let Some(var) = variables.get(name) {
                if let Variable::Boolean(var) = var {
                    let b = !var;
                    variables.insert(name.to_string(), Variable::Boolean(b));
                    Ok(())
                } else {
                    Err(MountError::new(
                        format!("Invalid data types on line {}: Expected <bool>", ctx.number)
                            .to_string(),
                    ))
                }
            } else {
                Err(MountError::new(format!(
                    "Variable '{}' has not been declared, but it is referenced on line {}",
                    name, ctx.number
                )))
            }
        } else {
            Err(MountError::new(format!(
                "Invalid 'NOT' on line {}\r\n\tSyntax --\r\n\tNOT $VarName",
                ctx.number
            )))
        }
    }

    pub fn rem(
        _ctx: &Line,
        _variables: &mut VarMapping,
        _loaded_variables: &mut LoadedVars,
        _function_context: &FunctionContext,
        _functions: &GlobalFunctions,
    ) -> CommandRet {
        Ok(())
    }

    pub fn if_jmp(
        ctx: &Line,
        variables: &mut VarMapping,
        loaded_variables: &mut LoadedVars,
        _function_context: &FunctionContext,
        functions: &GlobalFunctions,
    ) -> CommandRet {
        if ctx.arguments.len() != 3 {
            return Err(MountError::new(
                format!(
                    "Invalid 'IF' on line {}\r\n\tSyntax --\r\n\tIF $BoolVarName, FunctionName1, FunctionName2",
                    ctx.number
                ).to_string(),
            ));
        }

        let predicate = var_exists(ctx.arguments.get(0).unwrap(), variables)?;

        if let Variable::Boolean(as_bool) = predicate {
            let _ = if *as_bool {
                execute_function(
                    ctx.arguments.get(1).unwrap(),
                    functions,
                    variables,
                    loaded_variables,
                )?
            } else {
                execute_function(
                    ctx.arguments.get(2).unwrap(),
                    functions,
                    variables,
                    loaded_variables,
                )?
            };
        } else {
            return Err(MountError::new(
                format!("Invalid data types on line {}: Expected <bool>", ctx.number).to_string(),
            ));
        }

        Ok(())
    }

    macro_rules! comparison {
      ($name:ident, $op:tt) => {
     pub fn $name(
        ctx: &Line,
        variables: &mut VarMapping,
        _loaded_variables: &mut LoadedVars,
        _function_context: &FunctionContext,
        _functions: &GlobalFunctions,
    ) -> CommandRet {
        if ctx.arguments.len() != 3 {
            return Err(MountError::new(
                format!(
                    "Invalid '{}' on line {}\r\n\tSyntax --\r\n\t{} $DestinationVarName, $VarName1, $VarName2",
                    stringify!(name).to_ascii_uppercase(),
                    stringify!(name).to_ascii_uppercase(),
                    ctx.number
                ).to_string(),
            ));
        }

        let dest_name = ctx.arguments.get(0).unwrap();
        let _ = var_exists(dest_name, variables)?;
        let var1 = var_exists(ctx.arguments.get(1).unwrap(), variables)?;
        let var2 = var_exists(ctx.arguments.get(2).unwrap(), variables)?;

        if let (Variable::Number(n1), Variable::Number(n2)) = (var1, var2) {
          let res: bool = *n1 $op *n2;
            variables.insert(dest_name.to_string(), Variable::Boolean(res));
            Ok(())
        } else {
          Err(MountError::new(format!("Invalid data types on line {}: Expected <number, number>", ctx.number)))
        }
    }
      };
    }

    macro_rules! equality {
    ($name:ident, $op:tt) => {
      pub fn $name(
        ctx: &Line,
        variables: &mut VarMapping,
        _loaded_variables: &mut LoadedVars,
        _function_context: &FunctionContext,
        _functions: &GlobalFunctions,
    ) -> CommandRet {
        if ctx.arguments.len() != 3 {
            return Err(MountError::new(
                format!(
                    "Invalid '{}' on line {}\r\n\tSyntax --\r\n\t{} $DestinationVarName, $VarName1, $VarName2",
                    stringify!($name).to_ascii_uppercase(),
                    ctx.number,
                    stringify!($name).to_ascii_uppercase()
                )
            ));
        }

        let dest_name = ctx.arguments.get(0).unwrap();
        let _ = var_exists(dest_name, variables)?;
        let var1 = var_exists(ctx.arguments.get(1).unwrap(), variables)?;
        let var2 = var_exists(ctx.arguments.get(2).unwrap(), variables)?;

        if let (Variable::Number(n1), Variable::Number(n2)) = (var1, var2) {
          let res: bool = *n1 $op *n2;
            variables.insert(dest_name.to_string(), Variable::Boolean(res));
            return Ok(());
        }

        if let (Variable::Boolean(n1), Variable::Boolean(n2)) = (var1, var2) {
          let res: bool = *n1 $op *n2;
            variables.insert(dest_name.to_string(), Variable::Boolean(res));
            return Ok(());
        }

        if let (Variable::String(n1), Variable::String(n2)) = (var1, var2) {
            let res: bool = n1.to_string() $op n2.to_string();
            variables.insert(dest_name.to_string(), Variable::Boolean(res));
            return Ok(());
        }

        Ok(())
    }

    };
  }

    equality!(equ, ==);
    equality!(neq, !=);

    comparison!(gt, >);
    comparison!(lt, <);
    comparison!(gte, >=);
    comparison!(lte, <=);

    pub fn mov(
        ctx: &Line,
        variables: &mut VarMapping,
        _loaded_variables: &mut LoadedVars,
        _function_context: &FunctionContext,
        _functions: &GlobalFunctions,
    ) -> CommandRet {
        if ctx.arguments.len() != 2 {
            return Err(MountError::new(
                format!(
                    "Invalid 'MOV' on line {}\r\n\tSyntax --\r\n\tMOV $VarName1, $VarName2",
                    ctx.number
                )
                .to_string(),
            ));
        }

        let var1_name = ctx.arguments.get(0).unwrap();

        // let _ = var_exists(var1_name, variables)?;
        if !var1_name.starts_with('$') {
            return Err(MountError::new(format!(
                "Invalid 'MOV' on line {}\r\n\tVariable name must start with '$'\r\n\tFound {}",
                ctx.number, var1_name
            )));
        }

        let var2 = var_exists(ctx.arguments.get(1).unwrap(), variables)
            .unwrap()
            .to_owned();

        variables.insert(var1_name.to_string(), var2);

        Ok(())
    }

    pub fn jmp(
        ctx: &Line,
        variables: &mut VarMapping,
        loaded_variables: &mut LoadedVars,
        _function_context: &FunctionContext,
        functions: &GlobalFunctions,
    ) -> CommandRet {
        if ctx.arguments.len() != 1 {
            return Err(MountError::new(
                format!(
                    "Invalid 'JMP' on line {}\r\n\tSyntax --\r\n\tJMP FunctionName",
                    ctx.number
                )
                .to_string(),
            ));
        }

        let _ = execute_function(&ctx.arguments[0], functions, variables, loaded_variables)?;

        Ok(())
    }
}

mod built_in_functions {
    use super::{command_types::LoadedVars, MountError, Variable};

    pub fn input(_loaded_variables: &mut LoadedVars) -> Result<Option<Variable>, MountError> {
        use std::io::{stdin, stdout, Write};

        let _ = stdout().flush();

        let mut result = String::new();

        stdin().read_line(&mut result).expect("a string");

        if let Some('\n') = result.chars().next_back() {
            result.pop();
        }

        if let Some('\r') = result.chars().next_back() {
            result.pop();
        }

        Ok(Some(Variable::String(result)))
    }

    pub fn print(loaded_variables: &mut LoadedVars) -> Result<Option<Variable>, MountError> {
        let mut string_buf = String::new();

        for var in loaded_variables {
            // dbg!(&var.to_string());
            string_buf.push_str(&var.to_string());
            string_buf.push(' ');
        }

        let trimmed = string_buf.trim_end();

        print!("{}", trimmed);

        Ok(None)
    }

    pub fn println(loaded_variables: &mut LoadedVars) -> Result<Option<Variable>, MountError> {
        print(loaded_variables)?;
        println!();
        Ok(None)
    }

    pub fn str_cat(loaded_variables: &mut LoadedVars) -> Result<Option<Variable>, MountError> {
        let mut result: String = String::new();

        for var in loaded_variables {
            result.push_str(var.to_string().as_str());
        }

        Ok(Some(Variable::String(result)))
    }

    fn num_len(n: f32) -> usize {
        if n < 100000f32 {
            // 5 or less
            if n < 100f32 {
                // 1 or 2
                if n < 10f32 {
                    return 1;
                }
                return 2;
            } else {
                // 3 or 4 or 5
                if n < 1000f32 {
                    return 3;
                } else {
                    // 4 or 5
                    if n < 10000f32 {
                        return 4;
                    }
                    return 5;
                }
            }
        } else {
            // 6 or more
            if n < 10000000f32 {
                // 6 or 7
                if n < 1000000f32 {
                    return 6;
                }
                return 7;
            } else {
                // 8 to 10
                if n < 100000000f32 {
                    return 8;
                }
                // 9 or 10
                if n < 1000000000f32 {
                    return 9;
                }
                return 10;
            }
        }
    }

    pub fn str_len(loaded_variables: &mut LoadedVars) -> Result<Option<Variable>, MountError> {
        if let Some(name) = loaded_variables.get(0) {
            match name {
                Variable::String(str) => Ok(Some(Variable::Number(str.len() as f32))),
                Variable::Number(num) => Ok(Some(Variable::Number(num_len(*num) as f32))),
                Variable::Boolean(b) => Ok(Some(Variable::Number({
                    if *b {
                        4f32
                    } else {
                        5f32
                    }
                }))),
                _ => return Err(MountError::new(format!(
                    "Invalid function call\r\n\tVariable cannot be cast to a number\r\n\t`{:?}` cannot be coerced.",
                    name
                )))
            }
        } else {
            Err(MountError::new(format!(
                "Invalid function call\r\n\tExpected one loaded variable (<str> | <number> | <bool>)"
            )))
        }
    }

    pub fn dim_len(loaded_variables: &mut LoadedVars) -> Result<Option<Variable>, MountError> {
        if let Some(name) = loaded_variables.get(0) {
            match name {
                Variable::Dim(d) => Ok(Some(Variable::Number(d.len() as f32))),
                _ => return Err(MountError::new(format!(
                    "Invalid function call\r\n\tVariable is not a dim\r\n\t`{:?}` cannot be coerced.",
                    name
                )))
            }
        } else {
            Err(MountError::new(format!(
                "Invalid function call\r\n\tExpected one loaded variable (<dim>)"
            )))
        }
    }

    pub fn char_at(loaded_variables: &mut LoadedVars) -> Result<Option<Variable>, MountError> {
        if loaded_variables.len() != 2 {
            return Err(MountError::new(format!(
                "Invalid function call\r\n\tExpected two loaded variables (<str>, <num>)",
            )));
        }

        if let (Some(Variable::String(as_str)), Some(Variable::Number(index))) =
            (loaded_variables.get(0), loaded_variables.get(1))
        {
            if let Some(result) = as_str.chars().nth(*index as usize) {
                Ok(Some(Variable::String(result.to_string())))
            } else {
                Err(MountError::new(format!(
                    "Invalid function call\r\n\tCould not index into string at {}",
                    index
                )))
            }
        } else {
            Err(MountError::new(format!(
                "Invalid function call\r\n\tExpected two loaded variables (<str>, <num>)\r\n\tFound mismatched types",
            )))
        }
    }

    pub fn dim_at(loaded_variables: &mut LoadedVars) -> Result<Option<Variable>, MountError> {
        if loaded_variables.len() != 2 {
            return Err(MountError::new(format!(
                "Invalid function call\r\n\tExpected two loaded variables (<str>, <num>)",
            )));
        }

        if let (Some(Variable::Dim(as_dim)), Some(Variable::Number(index))) =
            (loaded_variables.get(0), loaded_variables.get(1))
        {
            if let Some(result) = as_dim.get(*index as usize) {
                Ok(Some(result.clone()))
            } else {
                Err(MountError::new(format!(
                    "Invalid function call\r\n\tCould not index into dim at {}",
                    index
                )))
            }
        } else {
            Err(MountError::new(format!(
                "Invalid function call\r\n\tExpected two loaded variables (<str>, <num>)\r\n\tFound mismatched types",
            )))
        }
    }
}

pub fn execute_function(
    name: &String,
    functions: &GlobalFunctions,
    variable_mapping: &mut HashMap<String, Variable>,
    loaded_variables: &mut Vec<Variable>,
) -> Result<(), MountError> {
    if let Some(lines) = functions.get(name) {
        for line in lines {
            let callable: Command = match COMMAND_MAP.get(line.command.as_str()) {
                Some(n) => *n,
                None => {
                    return Err(MountError::new(format!(
                        "Unknown command: {}",
                        line.command
                    )))
                }
            };

            callable(
                &line,
                variable_mapping,
                loaded_variables,
                &(name.to_string(), lines),
                functions,
            )?;

            // if result.is_err() {
            //     return Err(MountError::new(
            //         format!("Error: {:?}", result.err().unwrap()).to_string(),
            //     ));
            // }
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
) -> Result<(), MountError> {
    if !lines.contains_key("main") {
        return Err(MountError::new("No main function found.".into()));
    }

    let _ = execute_function(
        &"main".to_string(),
        functions,
        variable_mapping,
        loaded_variables,
    )?;

    Ok(())
}

pub fn val_or_compiler_error<T, E>(result: Result<T, E>, message: String) -> Result<T, MountError> {
    match result {
        Ok(n) => Ok(n),
        Err(_) => Err(MountError::new(message)),
    }
}

pub fn next_token<T>(tokens: &mut T) -> Result<String, MountError>
where
    T: Iterator<Item = String>,
{
    match tokens.next() {
        Some(n) => Ok(n),
        None => Err(MountError::new("Early EOL".into())),
    }
}

pub fn split_string(string: &String) -> Result<Vec<String>, String> {
    let mut result = Vec::<String>::new();
    let mut buf = String::new();
    let mut in_quotes = false;
    let mut escaping = false;

    for char in string.chars() {
        if !in_quotes {
            if char == ' ' || char == ',' {
                if buf.len() != 0 {
                    result.push(buf.to_string());
                    buf.clear();
                }
                continue;
            }
        }

        match char {
            '\\' => {
                if escaping {
                    buf.push(char);
                }
                escaping = !escaping;
                continue;
            }
            ',' => {
                if escaping {
                    buf.push(char);
                    escaping = !escaping;
                }
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

        // println!("$ {char}");
        buf.push(char);
    }

    if in_quotes {
        Err(("Found EOL while parsing string: ".to_owned() + string).to_string())
    } else {
        if buf.len() > 0 {
            result.push(buf.to_string());
        }
        Ok(result)
    }
}

pub fn map_lines(lines: Lines) -> Result<GlobalFunctions, Box<dyn std::error::Error>> {
    let mut result = Vec::new();
    let mut line_n = 0;
    // let mut line_numbers = HashSet::<u32>::new();

    let mut init_fn_name: Option<String> = None;
    let mut functions = HashMap::new();

    for line in lines {
        line_n += 1;
        if line.starts_with('~') && line.len() >= 2 {
            if let Some(init_fn_name) = init_fn_name {
                result.sort_by(|a: &Line, b: &Line| a.number.cmp(&b.number));
                functions.insert(init_fn_name, result);
                result = Vec::new();
            }

            init_fn_name = Some(line[1..line.len()].to_string());
            continue;
        }

        if line.len() == 0 {
            continue;
        }

        let tokens = split_string(&line.to_string())?;

        let mut tokens = tokens.into_iter();

        let command = next_token(&mut tokens)?;

        if !COMMAND_MAP.contains_key(&command) {
            return Err(Box::new(MountError::new(
                format!("Unknown command on line {}: {}", line_n, command).to_owned(),
            )));
        }

        // let mut arguments = Vec::new();
        // for argument in tokens {
        //     let argument = argument.to_string();
        //     // if let Some(',') = argument.chars().next_back() {
        //         // argument.pop();
        //     // }
        //     arguments.push(argument);
        // }

        result.push(Line {
            number: line_n - 1,
            command: command.to_string(),
            arguments: tokens.collect(),
        })
    }

    if let Some(fn_name) = init_fn_name {
        functions.insert(fn_name, result);
    }

    Ok(functions)
}

pub fn read_file(path: &String) -> Result<String, MountError> {
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
