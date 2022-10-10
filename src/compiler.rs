use lazy_static::lazy_static;
use pest::error::ErrorVariant;
use pest_consume::Error;
use pest_consume::Parser as ParserDerive;
use rand::prelude::*;
use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;
use std::sync::Mutex;

type Result<T> = std::result::Result<T, Error<Rule>>;
type Node<'i> = pest_consume::Node<'i, Rule, ()>;

#[derive(ParserDerive)]
#[grammar = "grammar.pest"]
struct Parser;

lazy_static! {
    static ref VARIABLE_MAPPING: Mutex<HashMap<String, String>> = Mutex::new(HashMap::new());
    static ref FUNCTION_MAPPING: Mutex<HashMap<String, Vec<String>>> = Mutex::new(HashMap::new());
    static ref GLOBAL_VARS: Mutex<Vec<String>> = Mutex::new(Vec::new());
    static ref LINE_NUMBER: Mutex<u32> = Mutex::new(0);
    static ref VAL_INIT_REF: Mutex<String> = Mutex::new(String::new());
}

macro_rules! gen_seed {
    () => {{
        let mut rng = thread_rng();
        rng.gen::<u32>()
    }};
}

pub(crate) fn eval_math(
    math: &str,
    vars: &HashMap<String, String>,
    destination: &String,
) -> Result<String> {
    let tt = crate::math::parse(math);

    let mut res = Vec::<String>::new();

    fn f(vars: &HashMap<String, String>, res: &mut Vec<String>, bin_op: crate::math::Expr) -> () {
        use crate::math::{BinOpKind, Expr};

        let first_op = format!("$__MATH_TEMP__#{}", gen_seed!());

        match bin_op {
            Expr::BinOp(first, op, second) => {
                let cmd = match op {
                    BinOpKind::Add => "ADD",
                    BinOpKind::Sub => "SUB",
                    BinOpKind::Mul => "MULT",
                    BinOpKind::Mod => "MOD",
                    BinOpKind::Div => "DIV",
                };

                res.push(format!("SET {first_op}, 0"));
                f(vars, res, *first);
                res.push(format!("MOV {first_op}, $__MATH_RESULT__"));

                let second_op = format!("$__MATH_TEMP__#{}", gen_seed!());

                res.push(format!("SET {second_op}, 0"));
                f(vars, res, *second);

                res.push(format!("MOV {second_op}, $__MATH_RESULT__"));

                res.push(format!("MOV $__MATH_RESULT__, {first_op}"));

                res.push(format!("{cmd} $__MATH_RESULT__, {second_op}"));

                res.push(format!("DROP {first_op}\r\nDROP {second_op}"));
            }
            Expr::UnOp(_, expr) => {
                f(vars, res, *expr);

                res.push(format!("NEGATE $__MATH_RESULT__"));
            }
            Expr::Number(n) => {
                res.push(format!(
                    "SET {first_op}, {n}\r\nMOV $__MATH_RESULT__, {first_op}\r\nDROP {first_op}"
                ));
            }
            Expr::Variable(name) => {
                dbg!("2");
                let reference = vars.get(&name).unwrap();

                println!("decl var {}", reference);
                res.push(format!("MOV $__MATH_RESULT__, {}", reference));
            }
        }
    }

    f(&vars, &mut res, tt);
    res.push(format!(
        "SET {destination}, 0\r\nMOV {destination}, $__MATH_RESULT__"
    ));

    let mut buf = String::new();

    for line in res {
        buf.push_str(&(line.to_owned() + "\r\n"))
    }

    Ok(buf)
}

#[pest_consume::parser]
impl Parser {
    fn function_call(input: Node) -> Result<String> {
        println!("{:?}", input);

        todo!("Add function calls");
    }

    fn val(input: Node) -> Result<String> {
        let input = input.children().next().unwrap();

        let seed = gen_seed!();
        let new_name = format!("$__VAL_INIT__#{seed}");

        let mut val_init = VAL_INIT_REF.lock().unwrap();

        *val_init = new_name.to_string();

        drop(val_init);

        let vars = VARIABLE_MAPPING.lock().unwrap();

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

                buf.push_str(format!("SET {new_name}, 0\r\n").as_str());

                buf = buf + format!("MOV {new_name}, {}", vars.get(other).unwrap()).as_str();

                buf
            }
            Rule::inline_math => {
                eval_math(&("(".to_owned() + input.as_str() + ")"), &vars, &new_name)?
            }
            Rule::group => eval_math(input.as_str(), &vars, &new_name)?,
            Rule::array => {
                let mut buf = String::new();
                let mut inits = vec![];

                let mut index: usize = 0;
                drop(vars);

                let seed = gen_seed!();
                let dest = format!("$__VAL_INIT__#{seed}");

                for var in input.children() {
                    let result = Self::val(var).unwrap();

                    let temp_id = format!("$__ARR_INIT_{index}@{seed}__");
                    inits.push(temp_id.to_string());
                    index += 1;

                    let c = VAL_INIT_REF.lock().unwrap();

                    buf.push_str(
                        format!("SET {temp_id}, 0\r\n{result}\r\nMOV {temp_id}, {c}\r\n",).as_str(),
                    );
                }

                let mut array_init = format!("DIM {dest}, ");
                let mut cleanup = String::from("\r\n");

                *VAL_INIT_REF.lock().unwrap() = dest;
                
                for init in inits {
                    array_init.push_str(&(init.to_owned() + ","));
                    cleanup.push_str(&("DROP ".to_owned() + init.as_str() + "\r\n"));
                }

                buf + array_init.as_str() + cleanup.as_str()
            }
            _ => panic!("undefined rule: {:?}", rule),
        });

        ret
    }

    fn variable(input: Node) -> Result<String> {
        let mut parts = input.children();
        let name = parts.next().unwrap().as_str();
        let val = parts.next().unwrap();

        let seed = gen_seed!();

        let new_name = format!("${name}_{seed}");

        // dbg!(&x);
        let init = Self::val(val)?;

        let mut vars = VARIABLE_MAPPING.lock().unwrap();
        // let val: String = Self::val(x);

        vars.insert(name.to_string(), new_name.to_string());

        Ok(format!(
            "{init}\r\nSET {new_name}, 0\r\nMOV {new_name}, {}",
            *VAL_INIT_REF.lock().unwrap()
        ))
    }

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

                if let Some(compiled_name) = VARIABLE_MAPPING.lock().unwrap().get(interpreted_name)
                {
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

    fn function_body(input: Node) -> Result<Vec<String>> {
        let mut result = Vec::new();
        for statement in input.children() {
            for part in statement.children() {
                let rule = part.as_rule();
                match rule {
                    Rule::variable => result.push(Self::variable(part)? + "\r\n"),

                    Rule::native => result.push(Self::native(part)? + "\r\n"),
                    _ => panic!("not implemented: {:?}", rule),
                }
            }
        }

        return Ok(result);
        // todo!("")
    }

    fn function(input: Node) -> Result<String> {
        // LINE_NUMBER.lock().unwrap().add_assign(10000);

        let mut result = String::new();

        let mut children = input.children();
        let ident = children.next().unwrap().as_str();
        let _args = children.next().unwrap().as_str();
        let body = Self::function_body(children.next().unwrap())?;

        result.push_str(format!("~{ident}\r\n").as_str());

        if ident == "main" {
            // let mut n = LINE_NUMBER.lock().unwrap();

            // n.add_assign(10);

            result.push_str(format!("JMP __GLOBALS__\r\n").as_str());
        }

        for line in body {
            // let mut n = LINE_NUMBER.lock().unwrap();
            // n.add_assign(10);

            result.push_str(format!("{}", line).as_str());
        }

        Ok(result + "\r\n")
    }

    fn declarations(input: Node) -> Result<String> {
        let mut result = String::new();

        // let mut top_level_vars = String::new();

        for decl in input.children() {
            match decl.as_rule() {
                Rule::variable => GLOBAL_VARS
                    .lock()
                    .unwrap()
                    .push(Self::variable(decl)?.to_owned()),
                // Rule::function => result.push_str(Self::function(variable)?.as_str()),
                Rule::function => result.push_str(Self::function(decl)?.as_str()),
                _ => panic!("other"),
            }
        }

        Ok(result)
    }

    fn file(input: Node) -> Result<Vec<String>> {
        let mut result = Vec::<String>::new();

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
            // let mut num = LINE_NUMBER.lock().unwrap();
            // num.add_assign(10);
            result.push(var.to_string())
        }

        Ok(result)
    }
}

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

    // println!("Compiled:");
    let mut buf = String::new();
    for l in c {
        // println!("{l}");
        buf += &(l + "\r\n");
    }

    let compiled_path = path_without_extension.to_owned() + ".mmm";

    let mut file = File::options()
        .write(true)
        .create(true)
        .truncate(true) // fixes bug where contents were not being fully overwritten.
        .open(compiled_path)
        .unwrap();

    let _ = file.write_all(buf.as_bytes());
    Ok(())
}
