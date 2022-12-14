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
use std::time::Instant;

use crate::files::Object;

type Result<T> = std::result::Result<T, Error<Rule>>;
type Node<'i> = pest_consume::Node<'i, Rule, ()>;

#[derive(ParserDerive)]
#[grammar = "grammar.pest"]
struct Parser;

#[derive(Debug)]
struct StackFrame {
    name: String,
    variables: HashMap<String, String>,
    functions: HashMap<String, String>,
    depth: usize,
    // popped: bool
}

impl StackFrame {
    pub fn new(name: &str, inherit_from: usize) -> Self {
        Self {
            name: name.to_string(),
            variables: HashMap::new(),
            functions: HashMap::new(),
            depth: inherit_from,
        }
    }

    /// # Add a variable to the current stack frame.
    pub fn add_var(&mut self, scope_name: String, compiled_name: String) {
        self.variables.insert(scope_name, compiled_name);
    }

    pub fn add_fn(&mut self, scope_name: String, compiled_name: String) {
        self.functions.insert(scope_name, compiled_name);
    }

    /// # Get the compiled name of a variable.
    /// This variable must be in scope, and the function will
    /// return the first instance it encounters. The most
    /// specific variable will take presedence.
    pub fn find_compiled_name(&self, scope_name: &String) -> Option<String> {
        for i in (0..=self.depth).rev() {
            match unsafe { CALL_STACK.get(i) }
                .unwrap()
                .variables
                .get(scope_name)
            {
                Some(name) => return Some(name.to_string()),
                None => continue,
            }
        }

        None
    }

    pub fn find_compiled_function(&self, scope_name: &String) -> Option<String> {
        for i in (0..=self.depth).rev() {
            let functions = &unsafe { CALL_STACK.get(i) }.unwrap().functions;
            match functions.get(scope_name) {
                Some(name) => return Some(name.to_string()),
                None => continue,
            }
        }

        None
    }
}

lazy_static! {
    static ref FUNCTION_MAPPING: Mutex<HashMap<String, usize>> = Mutex::new(HashMap::new());
}

static mut VAL_INIT_REF: String = String::new();
static mut CALL_STACK: Vec<StackFrame> = vec![];
static mut GLOBAL_VARS: Vec<String> = vec![];
// static mut LINE_NUMBER:

macro_rules! top_frame {
    () => {
        unsafe { CALL_STACK.get_mut(CALL_STACK.len() - 1) }.unwrap()
    };
}

/// # Add a new stack frame, given a name.
fn push_frame(name: &str) {
    unsafe { CALL_STACK.push(StackFrame::new(name, CALL_STACK.len())) }
}

/// # Pop the top stack frame.
fn pop_frame() {
    unsafe {
        CALL_STACK.pop();
    }
}

/// # Register a variable to a scope.
/// Requires a name, which the language uses, and the compiled name,
/// which should be *unique* and differ from the reference name to avoid data collisions.
///
/// Currently does not check for unique compiled names, or whether
/// the name is already in the scope.
fn add_var_to_scope(name: String, compiled_name: String) {
    let top = top_frame!();
    top.add_var(name, compiled_name);
}

fn add_fn_to_scope(name: String, compiled_name: String) {
    let top = top_frame!();
    top.add_fn(name, compiled_name);
}

/// # Get the compiled name of a variable.
/// Will be `Some` is the variable is accessible from the current scope,
/// otherwise will be `None`.
fn get_var_from_scope(name: &String) -> Option<String> {
    let top = top_frame!();

    let result = match top.find_compiled_name(name) {
        Some(str) => Some(str.to_string()),
        None => None,
    };

    result
}

fn get_fn_from_scope(name: &String) -> Option<String> {
    let top = top_frame!();

    let result = match top.find_compiled_function(name) {
        Some(str) => Some(str.to_string()),
        None => None,
    };

    result
}

macro_rules! gen_seed {
    () => {{
        let mut rng = thread_rng();
        rng.gen::<u32>()
    }};
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
            MathExpr::String(s) => {
                res.push(format!("SET $__MATH_RESULT__, {s}"));
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

fn gen_val_init(input: Node) -> Result<String> {
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
        Rule::function_call => {
            let function_call = Parser::function_call(input)?.to_string();
            unsafe { VAL_INIT_REF = new_name.to_string() };
            format!("{function_call}\r\nMOV {new_name}, $__RET__")
        }
        Rule::ident => {
            let mut buf = String::new();

            let other = input.as_str();

            buf = buf
                + format!(
                    "MOV {new_name}, {}\r\n",
                    get_var_from_scope(&other.to_string())
                        .expect(format!("{other} is not in scope.").as_str())
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
                let result = Parser::val(var).unwrap();

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
                        let v_init = Parser::val(i_obj)?;

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
        _ => panic!("undefined rule: {:?}", rule),
    });

    ret
}

/// # The Pest Language Parser
#[pest_consume::parser]
impl Parser {
    /// # Evaluate *any* "val" rule.
    /// Will store the result in the register stored at VAL_INIT_REF
    fn val(input: Node) -> Result<String> {
        gen_val_init(input)
    }

    /// # Variable declaration
    /// `var x = 10;`
    fn variable(input: Node) -> Result<String> {
        let mut parts = input.children();
        let name = parts.next().unwrap().as_str();
        let val = parts.next().unwrap();

        let new_name = format!("${}->var({name})", top_frame!().name);

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
            let init_dest = unsafe { &VAL_INIT_REF }.to_string();
            val_inits.push(init_dest);
        }

        Ok(format!(
            "{result}{}\r\nJMP {}",
            if val_inits.len() > 0 {
                let mut buf = String::from("\r\nLOAD ");
                for val_init in val_inits {
                    buf.push_str((val_init.to_owned() + ",").as_str());
                }
                buf
            } else {
                "".to_string()
            },
            {
                get_fn_from_scope(&ident.to_string()).unwrap()
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

    fn while_loop(input: Node) -> Result<(String, Vec<Option<String>>)> {
        let mut children = input.children();

        let _ = children.next();

        let condition_init = gen_val_init(input)?;

        let condition_init_dest = unsafe { &VAL_INIT_REF }.to_string();

        let mut appendices: Vec<Option<String>> = vec![];

        let seed = gen_seed!();
        let name1 = format!("{}::<while#{}>", top_frame!().name, seed);

        push_frame(&name1);
        let looping = Self::function_body(children.next().unwrap())?;
        pop_frame();

        appendices.push({
            let mut buf = format!("\r\n~{name1}::looping\r\n");
            for line in looping {
                buf.push_str((line + "\r\n").as_str())
            }
            buf.push_str(&("JMP ".to_owned() + &name1 + "\r\n"));
            Some(buf)
        });

        appendices.push(Some(format!("~{name1}\r\nIF {condition_init_dest}, {name1}::looping,")));

        Ok((format!(
            "{condition_init}\r\nJMP {name1}\r\n"
        ), appendices))
    }

    fn if_statement(input: Node) -> Result<(String, Vec<Option<String>>)> {
        let mut children = input.children();

        let _ = children.next();

        let condition_init = gen_val_init(input)?;

        let condition_init_dest = unsafe { &VAL_INIT_REF }.to_string();

        let mut appendices: Vec<Option<String>> = vec![];

        let seed = gen_seed!();
        let name1 = format!("{}::<if#{}>", top_frame!().name, seed);
        let name2 = format!("{}::<else#{}>", top_frame!().name, seed);

        push_frame(&name1);
        let if_true = Self::function_body(children.next().unwrap())?;
        pop_frame();

        let if_false: Option<String> = if let Some(else_block) = children.next() {
            push_frame(&name2);
            let r = match else_block.as_rule() {
                Rule::function_body => {
                    let mut buf = String::new();

                    let lines = Self::function_body(else_block)?;
                    for line in lines {
                        buf.push_str(&(line + "\r\n"));
                    }

                    Some(buf)
                }
                Rule::if_statement => {
                    let mut result = Self::if_statement(else_block)?;
                    appendices.append(&mut result.1);
                    Some(result.0)
                }
                _ => unreachable!(),
            };
            pop_frame();
            r
        } else {
            None
        };

        appendices.push({
            let mut buf = format!("\r\n~{name1}\r\n");
            for line in if_true {
                buf.push_str((line + "\r\n").as_str())
            }
            Some(buf)
        });

        appendices.push(if let Some(ref else_code) = if_false {
            Some(format!("\r\n~{}\r\n{else_code}", &name2))
        } else {
            None
        });

        Ok((
            format!(
                "{condition_init}\r\nIF {condition_init_dest}, {name1}, {}",
                if if_false.is_some() {
                    name2.to_string()
                } else {
                    "".to_string()
                }
            ),
            appendices,
        ))
    }

    fn return_statement(input: Node) -> Result<String> {
        let val = input.children().next().unwrap();

        let val_init = Self::val(val)?;

        Ok(format!("{val_init}\r\nMOV $__RET__, {}", unsafe {
            &VAL_INIT_REF
        }))
    }

    /// # Function Bodies
    /// Iterate over each statement in a function, run/evauluate the code.
    fn function_body(input: Node) -> Result<Vec<String>> {
        let mut result = Vec::new();

        let mut appendices = Vec::new();

        for statement in input.children() {
            for part in statement.children() {
                let rule = part.as_rule();
                match rule {
                    Rule::variable => result.push(Self::variable(part)? + "\r\n"),
                    Rule::native => result.push(Self::native(part)? + "\r\n"),
                    Rule::function_call => result.push(Self::function_call(part)? + "\r\n"),
                    Rule::variable_reassign => result.push(Self::variable_reassign(part)? + "\r\n"),
                    Rule::shorthand_assign => result.push(Self::shorthand_assign(part)? + "\r\n"),
                    Rule::return_statement => {
                        result.push(Self::return_statement(part)? + "\r\n");
                        return Ok(result);
                    }
                    Rule::if_statement => {
                        let mut if_statement = Self::if_statement(part)?;

                        result.push(if_statement.0 + "\r\n");

                        appendices.append(&mut if_statement.1);
                    }
                    Rule::while_loop => {
                        let mut while_loop = Self::while_loop(part)?;

                        result.push(while_loop.0 + "\r\n");

                        appendices.append(&mut while_loop.1);
                    }
                    _ => panic!("not implemented: {:?}", rule),
                }
            }
        }

        for appendice in appendices {
            if let Some(appendice) = appendice {
                result.push(appendice);
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

        let fn_ident = format!("func({ident})");
        result.push_str(format!("\r\n~{}->{fn_ident}\r\n", top_frame!().name).as_str());
        push_frame(fn_ident.as_str());

        let mut c: usize = 0;
        for arg in args.children() {
            let seed = gen_seed!();

            let new_name = format!("${}_{seed}", arg.as_str());

            result.push_str(format!("ARG {c}, {}\r\n", &new_name).as_str());

            let interpreted = arg.as_str().to_string();

            add_var_to_scope(interpreted, new_name);
            c += 1;
        }

        let mut lock = FUNCTION_MAPPING.lock().unwrap();
        lock.insert(ident.to_string(), c + 1);
        drop(lock);

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

    fn object(input: Node) -> Result<String> {
        let mut result: Vec<String> = vec![];

        let mut children = input.children();
        let ident = children.next().unwrap().as_str();

        let args = children.next().unwrap();

        // push_frame(format!("func::{ident}").as_str());
        // result.push_str(format!("\r\n~{ident}\r\n").as_str());
        result.push(format!("~{}->obj({ident})\r\n", top_frame!().name));
        push_frame(format!("{}->obj({ident})", top_frame!().name).as_str());

        let mut c: usize = 0;
        for arg in args.children() {
            let seed = gen_seed!();

            let new_name = format!("${}_{seed}", arg.as_str());

            result.push(format!("ARG {c}, {}", &new_name));

            let interpreted = arg.as_str().to_string();

            add_var_to_scope(interpreted, new_name);
            c += 1;
        }

        let mut lock = FUNCTION_MAPPING.lock().unwrap();
        lock.insert(ident.to_string(), c + 1);
        drop(lock);

        result.push("POPALL".to_owned());

        let mut variables = vec![];
        let mut members = vec![];

        let current_globals = unsafe { GLOBAL_VARS.to_owned() }.clone();

        unsafe {
            GLOBAL_VARS = vec![]
        }

        for child in children {
            let rule = child.children().next().unwrap();
            let ident = rule.children().next().unwrap();

            match rule.as_rule() {
                Rule::function | Rule::object => members.push(ident.as_str()),
                Rule::variable => variables.push(ident.as_str()),
                _ => {
                    panic!("{:?}", ident.as_rule())
                }
            }

            let body = Self::declarations(child).unwrap();



            result.push(body);
        }

        let new_globals = unsafe {
            let mut buf = String::new();
            for decl in &GLOBAL_VARS {
                buf.push_str(&(decl.to_owned() + "\r\n"));
            }

            let mut joint_list = String::new();

            for variable in variables {
                joint_list.push_str(format!("${}->var({variable}), ", top_frame!().name).as_str());
            }

            for member in members {
                joint_list.push_str(format!("{}->func({member}), ", top_frame!().name).as_str());
            }

            format!("{buf}\r\nNEWOBJ $__RET__, {joint_list}\r\n")
        };


        result.insert(1, new_globals);

        // let finished = format!("\r\n{result}\r\n", unsafe {
        //     let mut buf = String::new();
        //     for decl in &GLOBAL_VARS {
        //         buf.push_str(decl);
        //     }
        //     buf
        // });

        unsafe {
            GLOBAL_VARS = current_globals
        }


        pop_frame();


        Ok({
            let mut buf = String::new();

            for line in &result {
                buf.push_str(line);
            }

            buf
        })
    }

    /// # Top-level program declarations.
    /// Currently supports: functions, variables.
    fn declarations(input: Node) -> Result<String> {
        let mut result = String::new();

        for decl in input.children() {
            match decl.as_rule() {
                Rule::variable => {
                    unsafe { GLOBAL_VARS.push(Self::variable(decl)?.to_owned()) }
                }
                Rule::function => result.push_str(Self::function(decl)?.as_str()),
                Rule::object => result.push_str(Self::object(decl)?.as_str()),
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

        let cloned = input.children();

        for declaration in cloned {
            if let Rule::declarations = declaration.as_rule() {
                let decl = declaration.children().next().unwrap();
                let rule = decl.as_rule();
                match rule {
                    Rule::function | Rule::object => {
                        let ident = decl.children().next().unwrap().as_str();
                        let fn_ident = format!(
                            "{}->{}({ident})",
                            top_frame!().name,
                            match rule {
                                Rule::function => "func",
                                _ => "obj",
                            }
                        );

                        add_fn_to_scope(ident.to_string(), fn_ident);
                    }
                    Rule::variable => (),
                    _ => unreachable!(),
                }
            }
        }

        for declaration in input.children() {
            if let Rule::declarations = declaration.as_rule() {
                if let Ok(declaration) = Self::declarations(declaration) {
                    if declaration.len() != 0 {
                        result.push(declaration)
                    }
                }
            }
        }

        result.push("~__GLOBALS__\r\nSET $__MATH_RESULT__, 0".into());

        for var in unsafe { &GLOBAL_VARS } {
            result.push(var.to_string())
        }

        Ok(result)
    }
}

/// # Compile a `.ms` file to a `.mmm` instruction file.
pub fn compile(path: &String) -> Result<()> {
    use crate::files::read_file;

    let start = Instant::now();

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

    println!("Done! Compiled in {}ms\r\n", start.elapsed().as_millis());

    Ok(())
}
