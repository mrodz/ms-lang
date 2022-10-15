use lazy_static::lazy_static;
use pest::error::ErrorVariant;
use pest_consume::Error;
use pest_consume::Parser as ParserDerive;
use rand::prelude::*;
use std::collections::HashMap;
use std::fmt::Debug;
use std::fs::File;
use std::io::prelude::*;
use std::sync::Mutex;

type Result<T> = std::result::Result<T, Error<Rule>>;
type Node<'i> = pest_consume::Node<'i, Rule, ()>;



#[derive(ParserDerive)]
#[grammar = "grammar.pest"]
struct Parser;

#[derive(Debug)]
struct StackFrame {
    name: String,
    variables: HashMap<String, String>,
    depth: usize,
}

impl StackFrame {
    pub fn new(name: &str, inherit_from: usize) -> Self {
        Self {
            name: name.to_string(),
            variables: HashMap::new(),
            depth: inherit_from,
        }
    }

    /// # Add a variable to the current stack frame.
    pub fn add_var(&mut self, scope_name: String, compiled_name: String) {
        self.variables.insert(scope_name, compiled_name);
    }

    /// # Get the compiled name of a variable.
    /// This variable must be in scope, and the function will
    /// return the first instance it encounters. The most
    /// specific variable will take presedence.
    pub fn find_compiled_name(
        &self,
        scope_name: &String,
        stack: &Vec<StackFrame>,
    ) -> Option<String> {
        for i in (0..=self.depth).rev() {
            match stack.get(i).unwrap().variables.get(scope_name) {
                Some(name) => return Some(name.to_string()),
                None => continue,
            }
        }

        None
    }
}

lazy_static! {
    // static ref VARIABLE_MAPPING: Mutex<HashMap<String, String>> = Mutex::new(HashMap::new());
    static ref FUNCTION_MAPPING: Mutex<HashMap<String, usize>> = Mutex::new(HashMap::new());
    static ref GLOBAL_VARS: Mutex<Vec<String>> = Mutex::new(Vec::new());
    static ref LINE_NUMBER: Mutex<u32> = Mutex::new(0);
    static ref CALL_STACK: Mutex<Vec<StackFrame>> = Mutex::new(Vec::new());
}

static mut VAL_INIT_REF: String = String::new();

/// # Add a new stack frame, given a name.
fn push_frame(name: &str) {
    let mut stack = CALL_STACK.lock().unwrap();
    let len = stack.len();
    stack.push(StackFrame::new(name, len));
}

/// # Pop the top stack frame.
fn pop_frame() {
    CALL_STACK.lock().unwrap().pop();
}

/// # Register a variable to a scope.
/// Requires a name, which the language uses, and the compiled name,
/// which should be *unique* and differ from the reference name to avoid data collisions.
///
/// Currently does not check for unique compiled names, or whether
/// the name is already in the scope.
fn add_var_to_scope(name: String, compiled_name: String) {
    let mut stack = CALL_STACK.lock().unwrap();
    let i = stack.len() - 1;
    let top: &mut StackFrame = stack.get_mut(i).unwrap();

    top.add_var(name, compiled_name);

    drop(stack);
}

/// # Get the compiled name of a variable.
/// Will be `Some` is the variable is accessible from the current scope,
/// otherwise will be `None`.
fn get_var_from_scope(name: &String) -> Option<String> {
    let stack = CALL_STACK.lock().unwrap();

    let i = stack.len() - 1;
    let top: &StackFrame = stack.get(i).unwrap();

    let result = match top.find_compiled_name(name, &stack) {
        Some(str) => Some(str.to_string()),
        None => None,
    };

    drop(stack);

    result
}

macro_rules! gen_seed {
    () => {{
        let mut rng = thread_rng();
        rng.gen::<u32>()
    }};
}

/// # Evaluate a boolean expression
/// Will store the result in `destination`
pub(crate) fn eval_boolean(
    math: &str,
    destination: &String,
) -> Result<String> {
    let tt = crate::bools::parse(math);

    let mut res = Vec::<String>::new();

    fn f(res: &mut Vec<String>, bin_op: crate::bools::BoolExpr) -> () {
        use crate::bools::{BinOpKind, BoolExpr as Expr};

        let first_op = format!("$__MATH_TEMP__#{}", gen_seed!());

        match bin_op {
            Expr::BinOp(first, op, second) => {
                let cmd = match op {
                    BinOpKind::And => "AND",
                    BinOpKind::Or => "OR",
                    BinOpKind::Xor => "XOR",
                };

                f(res, *first);
                res.push(format!("MOV {first_op}, $__MATH_RESULT__"));

                let second_op = format!("$__MATH_TEMP__#{}", gen_seed!());

                f(res, *second);

                res.push(format!("MOV {second_op}, $__MATH_RESULT__"));

                res.push(format!("MOV $__MATH_RESULT__, {first_op}"));

                res.push(format!("{cmd} $__MATH_RESULT__, {second_op}"));

                res.push(format!("DROP {first_op}\r\nDROP {second_op}"));
            }
            Expr::UnOp(_, expr) => {
                f(res, *expr);

                res.push(format!("NOT $__MATH_RESULT__"));
            }
            Expr::Boolean(n) => {
                res.push(format!("SET $__MATH_RESULT__, {n}"));
            }
            Expr::Variable(name) => {
                let reference = get_var_from_scope(&name).unwrap();

                res.push(format!("MOV $__MATH_RESULT__, {}", reference));
            }
        }
    }

    f(&mut res, tt);
    res.push(format!("MOV {destination}, $__MATH_RESULT__"));

    let mut buf = String::new();

    for line in res {
        buf.push_str(&(line.to_owned() + "\r\n"))
    }

    Ok(buf)
}

/// # Evaluate a math expression
/// Will store the result in `destination`
pub(crate) fn eval_math(math: &str, destination: &String) -> Result<String> {
    let tt = crate::math::parse(math);

    let mut res = Vec::<String>::new();

    fn f(res: &mut Vec<String>, bin_op: crate::math::MathExpr) -> () {
        use crate::math::{BinOpKind, MathExpr};

        let first_op = format!("$__MATH_TEMP__#{}", gen_seed!());

        match bin_op {
            MathExpr::BinOp(first, op, second) => {
                let cmd = match op {
                    BinOpKind::Add => "ADD",
                    BinOpKind::Sub => "SUB",
                    BinOpKind::Mul => "MULT",
                    BinOpKind::Mod => "MOD",
                    BinOpKind::Div => "DIV",
                };

                f(res, *first);
                res.push(format!("MOV {first_op}, $__MATH_RESULT__"));

                let second_op = format!("$__MATH_TEMP__#{}", gen_seed!());

                f(res, *second);

                res.push(format!("MOV {second_op}, $__MATH_RESULT__"));

                res.push(format!("MOV $__MATH_RESULT__, {first_op}"));

                res.push(format!("{cmd} $__MATH_RESULT__, {second_op}"));

                res.push(format!("DROP {first_op}\r\nDROP {second_op}"));
            }
            MathExpr::UnOp(_, expr) => {
                f(res, *expr);

                res.push(format!("NEGATE $__MATH_RESULT__"));
            }
            MathExpr::Number(n) => {
                res.push(format!("SET $__MATH_RESULT__, {n}"));
            }
            MathExpr::Variable(name) => {
                let reference = get_var_from_scope(&name).unwrap();
                res.push(format!("MOV $__MATH_RESULT__, {}", reference));
            }
        }
    }

    f(&mut res, tt);
    res.push(format!("MOV {destination}, $__MATH_RESULT__"));

    let mut buf = String::new();

    for line in res {
        buf.push_str(&(line.to_owned() + "\r\n"))
    }

    Ok(buf)
}

/// # The Pest Language Parser
#[pest_consume::parser]
impl Parser {
    /// # Evaluate *any* "val" rule.
    /// Will store the result in the register stored at VAL_INIT_REF
    fn val(input: Node) -> Result<String> {
        let input = input.children().next().unwrap();

        let seed = gen_seed!();
        let new_name = format!("$__VAL_INIT__#{seed}");

        unsafe {
            VAL_INIT_REF = new_name.to_string();
        }

        let rule = input.as_rule();

        let ret = Ok(match rule {
            Rule::number
            | Rule::boolean
            | Rule::string_literal_double
            | Rule::string_literal_single => {
                let d = input.as_str().to_string();
                format!("SET {new_name}, {d}")
            }
            Rule::function_call => Self::function_call(input)?.to_string(),
            Rule::ident => {
                let mut buf = String::new();

                let other = input.as_str();

                buf = buf
                    + format!(
                        "MOV {new_name}, {}\r\n",
                        get_var_from_scope(&other.to_string()).unwrap()
                    )
                    .as_str();

                buf
            }
            Rule::inline_math => eval_math(&("(".to_owned() + input.as_str() + ")"), &new_name)?,
            Rule::group => eval_math(input.as_str(), &new_name)?,
            Rule::array => {
                let mut buf = String::new();
                let mut inits = vec![];

                let mut index: usize = 0;

                let seed = gen_seed!();
                let dest = format!("$__VAL_INIT__#{seed}");

                for var in input.children() {
                    let result = Self::val(var).unwrap();

                    let temp_id = format!("$__ARR_INIT_{index}@{seed}__");
                    inits.push(temp_id.to_string());
                    index += 1;

                    let c = unsafe { &VAL_INIT_REF };

                    buf.push_str(format!("{result}\r\nMOV {temp_id}, {c}\r\n",).as_str());
                }

                let mut array_init = format!("DIM {dest}, ");
                let mut cleanup = String::from("\r\n");

                unsafe {
                    VAL_INIT_REF = dest;
                }

                for init in inits {
                    array_init.push_str(&(init.to_owned() + ","));
                    cleanup.push_str(&("DROP ".to_owned() + init.as_str() + "\r\n"));
                }

                buf + array_init.as_str() + cleanup.as_str()
            }
            Rule::array_index => {
                let mut children = input.children();

                let ident = children.next().unwrap().as_str();

                let mut result = format!(
                    "MOV $__INDEXING_TEMP@{seed}__, {}\r\n",
                    get_var_from_scope(&ident.to_string()).unwrap()
                );

                for index in children {
                    let i_obj = index.children().next().unwrap();

                    match i_obj.as_rule() {
                        Rule::array_index_num => result.push_str(
                            format!(
                                "AT $__INDEXING_TEMP@{seed}__, {}, $__INDEXING_TEMP@{seed}__\r\n",
                                index.children().next().unwrap().as_str()
                            )
                            .as_str(),
                        ),
                        Rule::val => {
                            let v_init = Self::val(i_obj)?;

                            let dest = unsafe { &VAL_INIT_REF };

                            result.push_str(
                                format!(
                                    "{v_init}\r\nAT $__INDEXING_TEMP@{seed}__, {}, $__INDEXING_TEMP@{seed}__\r\n",
                                    dest
                                ).as_str(),
                            )
                        }
                        _ => unimplemented!(),
                    }
                }

                let res = format!("{result}MOV {new_name}, $__INDEXING_TEMP@{seed}__");
                unsafe { VAL_INIT_REF = new_name };
                res
            }
            Rule::boolean_group => eval_boolean(input.as_str(), &new_name)?,
            Rule::inline_boolean | Rule::boolean_prefix => eval_boolean(&("(".to_owned() + input.as_str() + ")"), &new_name)?,
            _ => panic!("undefined rule: {:?}", rule),
        });

        ret
    }

    /// # Variable declaration
    /// `var x = 10;`
    fn variable(input: Node) -> Result<String> {
        let mut parts = input.children();
        let name = parts.next().unwrap().as_str();
        let val = parts.next().unwrap();

        let seed = gen_seed!();

        let new_name = format!("${name}_{seed}");

        let init = Self::val(val)?; // evaluate the

        add_var_to_scope(name.to_string(), new_name.to_string());

        Ok(format!("{init}\r\nMOV {new_name}, {}", unsafe {
            &VAL_INIT_REF
        }))
    }

    /// # Native statement implementation
    /// Native statements allow for .MMM code injection.
    /// Allows references to local variables by their interpreted name,
    /// will search for their compiled name.
    fn native(input: Node) -> Result<String> {
        let code = input.children().next().unwrap().as_str();

        let split = match crate::files::split_string(&code.to_string()) {
            Ok(n) => n,
            Err(_) => {
                return Err(pest_consume::Error::new_from_span(
                    ErrorVariant::ParsingError {
                        positives: vec![Rule::native],
                        negatives: vec![],
                    },
                    input.as_span(),
                ))
            }
        };

        let mut buf: String = String::new();

        for token in split {
            if token.starts_with('$') {
                let interpreted_name = match token.get(1..token.len()) {
                    Some(name) => name,
                    None => {
                        return Err(pest_consume::Error::new_from_span(
                            ErrorVariant::ParsingError {
                                positives: vec![Rule::native],
                                negatives: vec![],
                            },
                            input.as_span(),
                        ))
                    }
                };

                if let Some(compiled_name) = get_var_from_scope(&interpreted_name.to_string()) {
                    buf.push_str(&(compiled_name.to_owned() + " "));
                } else {
                    return Err(pest_consume::Error::new_from_span(
                        ErrorVariant::ParsingError {
                            positives: vec![Rule::native],
                            negatives: vec![],
                        },
                        input.as_span(),
                    ));
                }
            } else {
                buf.push_str(&(token + " "));
            }
        }

        Ok(buf.trim_end().to_string())
    }

    /// # Variable reassignment statement.
    /// Makes use of the `MOV` command.
    /// `var a = 10; a = 20;`
    fn variable_reassign(input: Node) -> Result<String> {
        let mut children = input.children();

        let ident = children.next().unwrap().as_str();

        let val = children.next().unwrap();

        let val_init = Self::val(val)?;

        let val_init_dest = unsafe { &VAL_INIT_REF };

        let compiled_name = get_var_from_scope(&ident.to_string()).unwrap();

        Ok(format!(
            "{val_init}\r\nMOV {compiled_name}, {val_init_dest}"
        ))
    }

    /// # Function call implementation
    fn function_call(input: Node) -> Result<String> {
        let mut children = input.children();

        let ident = children.next().unwrap().as_str();

        let mut result = String::new();

        let mut val_inits = vec![];

        for param in children {
            let val = param.children().next().unwrap();
            let init = Self::val(val).unwrap();
            result.push_str(&(init.as_str().to_owned() + "\r\n"));
            let init_dest = unsafe { &VAL_INIT_REF };
            val_inits.push(init_dest);
        }

        Ok(format!(
            "{result}{}\r\nJMP {ident}",
            if val_inits.len() > 0 {
                let mut buf = String::from("\r\nLOAD ");
                for val_init in val_inits {
                    buf.push_str((val_init.to_owned() + ",").as_str());
                }
                buf
            } else {
                "".to_string()
            }
        ))
    }

    /// # Shortcut for variable reassignment
    /// Implementation for:
    /// - +=
    /// - -=
    /// - *=
    /// - /=
    /// - %=
    fn shorthand_assign(input: Node) -> Result<String> {
        let mut children = input.children();
        let ident = children.next().unwrap().as_str();
        let op = children.next().unwrap().as_str();
        let val = children.next().unwrap();

        let command = match op {
            "+=" => "ADD",
            "-=" => "SUB",
            "*=" => "MULT",
            "/=" => "DIV",
            "%=" => "MOD",
            _ => unreachable!(),
        };

        let val_init = Self::val(val)?;

        let compiled_name = get_var_from_scope(&ident.to_string()).unwrap();

        let val_dest = unsafe { &VAL_INIT_REF };

        Ok(format!(
            "{val_init}\r\n{command} {compiled_name}, {val_dest}"
        ))
    }

    /// # Function Bodies
    /// Iterate over each statement in a function, run/evauluate the code.
    fn function_body(input: Node) -> Result<Vec<String>> {
        let mut result = Vec::new();
        for statement in input.children() {
            for part in statement.children() {
                let rule = part.as_rule();
                match rule {
                    Rule::variable => result.push(Self::variable(part)? + "\r\n"),
                    Rule::native => result.push(Self::native(part)? + "\r\n"),
                    Rule::function_call => result.push(Self::function_call(part)? + "\r\n"),
                    Rule::variable_reassign => result.push(Self::variable_reassign(part)? + "\r\n"),
                    Rule::shorthand_assign => result.push(Self::shorthand_assign(part)? + "\r\n"),
                    _ => panic!("not implemented: {:?}", rule),
                }
            }
        }

        return Ok(result);
    }

    /// # Function declaration
    /// `func fizz() { }`
    fn function(input: Node) -> Result<String> {
        let mut result = String::new();

        let mut children = input.children();
        let ident = children.next().unwrap().as_str();

        let args = children.next().unwrap();

        push_frame(&("func::".to_owned() + ident + args.as_str()));
        result.push_str(format!("\r\n~{ident}\r\n").as_str());

        let mut c: usize = 0;
        for arg in args.children() {
            let seed = gen_seed!();

            let new_name = format!("${}_{seed}", arg.as_str());

            result.push_str(format!("ARG {c}, {}\r\n", &new_name).as_str());

            let interpreted = arg.as_str().to_string();

            add_var_to_scope(interpreted, new_name);
            c += 1;
        }

        let mut fn_map = FUNCTION_MAPPING.lock().unwrap();

        fn_map.insert(ident.to_string(), c + 1);

        result.push_str("POPALL\r\n");

        let body = Self::function_body(children.next().unwrap()).unwrap();

        // if it is the main function, load the constants.
        if ident == "main" {
            result.push_str(format!("JMP __GLOBALS__\r\n").as_str());
        }

        for line in body {
            // let mut n = LINE_NUMBER.lock().unwrap();
            // n.add_assign(10);

            result.push_str(format!("{}", line).as_str());
        }

        pop_frame();

        Ok(result + "\r\n")
    }

    /// # Top-level program declarations.
    /// Currently supports: functions, variables.
    fn declarations(input: Node) -> Result<String> {
        let mut result = String::new();

        for decl in input.children() {
            match decl.as_rule() {
                Rule::variable => GLOBAL_VARS
                    .lock()
                    .unwrap()
                    .push(Self::variable(decl)?.to_owned()),
                Rule::function => result.push_str(Self::function(decl)?.as_str()),
                _ => unreachable!(),
            }
        }

        Ok(result)
    }

    /// # Process the file.
    /// This is the "__GLOBALS__" stack frame.
    fn file(input: Node) -> Result<Vec<String>> {
        let mut result = Vec::<String>::new();

        push_frame("__GLOBALS__");

        for declaration in input.children() {
            match declaration.as_rule() {
                Rule::declarations => {
                    if let Ok(declaration) = Self::declarations(declaration) {
                        if declaration.len() != 0 {
                            result.push(declaration)
                        }
                    }
                }
                _ => continue,
            }
        }

        result.push("~__GLOBALS__\r\nSET $__MATH_RESULT__, 0".into());

        for var in GLOBAL_VARS.lock().unwrap().iter() {
            result.push(var.to_string())
        }

        Ok(result)
    }
}

/// # Compile a `.ms` file to a `.mmm` instruction file.
pub fn compile(path: &String) -> Result<()> {
    use crate::files::read_file;

    let fio = read_file(path).unwrap();

    let mut i = path.len();
    let chars = path.chars().rev();
    for char in chars {
        i -= 1;
        if char == '.' {
            break;
        }
    }

    let path_without_extension = path.get(0..i).expect("invalid path");

    let with_newline = &(fio.as_str().to_owned() + "\r\n");
    let inputs = <Parser as pest_consume::Parser>::parse(Rule::file, with_newline).unwrap();

    let input = inputs.single().unwrap();

    let c = Parser::file(input).unwrap();

    let mut buf = String::new();
    for l in c {
        buf += &(l + "\r\n");
    }

    let compiled_path = path_without_extension.to_owned() + ".mmm";

    let mut file = File::options()
        .write(true)
        .create(true)
        .truncate(true) // fixes bug where contents were not being fully overwritten.
        .open(compiled_path)
        .unwrap();

    println!("Compilation Successful, writing to file...");
    let _ = file.write_all(buf.as_bytes());
    println!("Done!\r\n");

    Ok(())
}
