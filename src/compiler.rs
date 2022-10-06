use lazy_static::lazy_static;
use pest_consume::Error;
use pest_consume::Parser as ParserDerive;
use rand::prelude::*;
use std::collections::HashMap;
use std::fs::{self, File, OpenOptions};
use std::io::prelude::*;
use std::ops::AddAssign;
use std::path::Path;
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

}

#[pest_consume::parser]
impl Parser {
    fn function_call(input: Node) -> Result<String> {
      println!("{:?}", input);

      todo!("Add function calls");

      Ok("!TODO".to_string())
    }


    fn variable(input: Node) -> Result<String> {
        let mut parts = input.children();
        let name = parts.next().unwrap().as_str();
        let val = parts.next().unwrap();

        LINE_NUMBER.lock().unwrap().add_assign(10);
        let line = LINE_NUMBER.lock().unwrap();

        let x = val.children().next().unwrap();

        let val: String = match x.as_rule() {
          Rule::number | Rule::boolean | Rule::string_literal_double | Rule::string_literal_single => {
            let d = x.as_str().to_string();

            d
          }
          Rule::function_call => Self::function_call(x)?.to_string(),
          _ => panic!("undefined")
        };

        let mut rng = thread_rng();
        let seed = rng.gen::<u32>();

        let new_name = format!("${name}_{seed}");
        VARIABLE_MAPPING.lock().unwrap().insert(name.to_string(), new_name.to_string());

        Ok(format!("{line} SET {new_name}, {val}"))
    }

    fn function_body(input: Node) -> Result<Vec<String>> {
      return Ok(vec!["REM".to_string()])
      // todo!("")
    }

    fn function(input: Node) -> Result<String> {
      LINE_NUMBER.lock().unwrap().add_assign(10000);

      let mut result = String::new();

      let mut children = input.children();
      let ident = children.next().unwrap().as_str();
      let args = children.next().unwrap().as_str();
      let body = Self::function_body(children.next().unwrap())?;

      result.push_str(format!("~{ident}\r\n").as_str());

      if ident == "main" { 
        let mut n = LINE_NUMBER.lock().unwrap();

        n.add_assign(10);

        result.push_str(format!("{n} JMP __GLOBALS__\r\n").as_str());
      }

      for line in body {
        let mut n = LINE_NUMBER.lock().unwrap();
        n.add_assign(10);
        
        result.push_str(format!("{n} {}", line).as_str());
      }

      Ok(result + "\r\n")
    }

    fn declarations(input: Node) -> Result<String> {
        let mut result = String::new();
        
        // let mut top_level_vars = String::new();

        for decl in input.children() {
            match decl.as_rule() {
              Rule::variable => GLOBAL_VARS.lock().unwrap().push(Self::variable(decl)?.to_owned()),
              // Rule::function => result.push_str(Self::function(variable)?.as_str()),
              Rule::function => result.push_str(Self::function(decl)?.as_str()),
              _ => panic!("other")
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
                },
                _ => continue
            }
        }

        
        result.push("~__GLOBALS__".into());
        
        for var in GLOBAL_VARS.lock().unwrap().iter() {
          result.push(var.to_owned())
        }

        Ok(result)
    }
}

pub fn compile(path: &String) -> Result<()> {
    use crate::files::read_file;

    let fio = read_file(path).unwrap();
    // println!("{}", fio);

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
    let inputs = Parser::parse(Rule::file, with_newline).unwrap();

    let input = inputs.single().unwrap();

    let c = Parser::file(input)?;

    println!("Compiled:");
    let mut buf = String::new();
    for l in c {
      println!("{l}");
      buf += &(l + "\r\n");
    }

    let compiled_path = path_without_extension.to_owned() + ".mmm";

    let mut file = File::options().write(true).create(true).open(compiled_path).unwrap();

    let _ = file.write_all(buf.as_bytes());
    Ok(())
}
