use lazy_static::lazy_static;
use pest_consume::Error;
use pest_consume::Parser as ParserDerive;
use rand::prelude::*;
use std::collections::HashMap;
use std::fs::{File};
use std::io::prelude::*;
use std::ops::AddAssign;
use std::sync::Mutex;

use crate::math::parse;

type Result<T> = std::result::Result<T, Error<Rule>>;
type Node<'i> = pest_consume::Node<'i, Rule, ()>;

#[derive(ParserDerive)]
#[grammar = "grammar.pest"]
struct Parser;

// #[derive(ParserDerive)]
// #[grammar = "grammar.pest"]
// struct TokenTreeParser;

lazy_static! {
    static ref VARIABLE_MAPPING: Mutex<HashMap<String, String>> = Mutex::new(HashMap::new());
    static ref FUNCTION_MAPPING: Mutex<HashMap<String, Vec<String>>> = Mutex::new(HashMap::new());
    static ref GLOBAL_VARS: Mutex<Vec<String>> = Mutex::new(Vec::new());
    static ref LINE_NUMBER: Mutex<u32> = Mutex::new(0);
}

macro_rules! gen_seed {
  () => {
    {
        let mut rng = thread_rng();
        rng.gen::<u32>()
    }
  }
}

// #[derive(Debug, Eq, PartialEq)]
// pub enum Expr {
//     BinOp(Box<Expr>, BinOpKind, Box<Expr>),
//     UnOp(UnOpKind, Box<Expr>),
//     Int(i32),
// }

// #[derive(Debug, Eq, PartialEq)]
// pub enum BinOpKind {
//     Add,
//     Sub,
//     Mul,
//     Div,
//     Pow,
//     Eq,
// }

// #[derive(Debug, Eq, PartialEq)]
// pub enum UnOpKind {
//     Not,
//     Neg,
//     Try,
// }

// use pratt::{Affix, Associativity, PrattParser, Precedence, Result as PrattResult};

// impl<'i, I> PrattParser<I> for Parser
// where
//     I: Iterator<Item = Pair<'i, Rule>>,
// {
//     type Error = pratt::NoError;
//     type Input = Pair<'i, Rule>;
//     type Output = Expr;

//     // Query information about an operator (Affix, Precedence, Associativity)
//     fn query(&mut self, tree: &Self::Input) -> PrattResult<Affix> {
//         let affix = match (tree.as_rule(), tree.as_str()) {
//             // (Rule::infix, "=") => Affix::Infix(Precedence(2), Associativity::Neither),
//             (Rule::infix, "+") => Affix::Infix(Precedence(3), Associativity::Left),
//             (Rule::infix, "-") => Affix::Infix(Precedence(3), Associativity::Left),
//             (Rule::infix, "*") => Affix::Infix(Precedence(4), Associativity::Left),
//             (Rule::infix, "/") => Affix::Infix(Precedence(4), Associativity::Left),
//             (Rule::infix, "%") => Affix::Infix(Precedence(4), Associativity::Left),
//             // (Rule::postfix, "?") => Affix::Postfix(Precedence(5)),
//             (Rule::prefix, "-") => Affix::Prefix(Precedence(6)),
//             (Rule::prefix, "!") => Affix::Prefix(Precedence(6)),
//             (Rule::infix, "^") => Affix::Infix(Precedence(7), Associativity::Right),
//             (Rule::group, _) => Affix::Nilfix,
//             (Rule::primary, _) => Affix::Nilfix,
//             (Rule::num, _) => Affix::Nilfix,
//             _ => unreachable!(),
//         };
//         Ok(affix)
//     }

//     // Construct a primary expression, e.g. a number
//     fn primary(&mut self, tree: Self::Input) -> PrattResult<Expr> {
//         let expr = match tree.as_rule() {
//             Rule::num => Expr::Int(tree.as_str().parse().unwrap()),
//             Rule::group => self.parse(&mut tree.into_inner()).unwrap(),
//             _ => unreachable!(),
//         };
//         Ok(expr)
//     }

//     // Construct a binary infix expression, e.g. 1+1
//     fn infix(&mut self, lhs: Expr, tree: Self::Input, rhs: Expr) -> PrattResult<Expr> {
//         let op = match tree.as_str() {
//             "+" => BinOpKind::Add,
//             "-" => BinOpKind::Sub,
//             "*" => BinOpKind::Mul,
//             "/" => BinOpKind::Div,
//             "^" => BinOpKind::Pow,
//             "=" => BinOpKind::Eq,
//             _ => unreachable!(),
//         };
//         Ok(Expr::BinOp(Box::new(lhs), op, Box::new(rhs)))
//     }

//     // Construct a unary prefix expression, e.g. !1
//     fn prefix(&mut self, tree: Self::Input, rhs: Expr) -> PrattResult<Expr> {
//         let op = match tree.as_str() {
//             "!" => UnOpKind::Not,
//             "-" => UnOpKind::Neg,
//             _ => unreachable!(),
//         };
//         Ok(Expr::UnOp(op, Box::new(rhs)))
//     }

//     // Construct a unary postfix expression, e.g. 1?
//     fn postfix(&mut self, _lhs: Expr, _tree: Self::Input) -> PrattResult<Expr> {
//         unimplemented!()
//         // let op = match tree.as_str() {
//         //     "?" => UnOpKind::Try,
//         //     _ => unreachable!(),
//         // };
//         // Ok(Expr::UnOp(op, Box::new(lhs)))
//     }
// }

#[pest_consume::parser]
impl Parser {
    fn function_call(input: Node) -> Result<String> {
        println!("{:?}", input);
        
        todo!("Add function calls");
    }

    fn primary(input: Node) -> Result<String> {
        println!("woo");
        // println!("node = {}", input.as_str());

        // let mut final_destination: Option<String> = if input.as_rule() == Rule::variable {
        //     Some(input.as_str().to_string())
        // } else {
        //   None
        // };

        // println!("fin = {}", final_destination.unwrap_or("no".to_string()));
      
        let mut result = Vec::<String>::new();

        let children = input.children();

        let mut first_operand = String::new();

        for child in children {
            match child.as_rule() {
              Rule::number => {
                first_operand = format!("$__MATH_TEMP__#{}", gen_seed!()); 
                result.push(format!("SET {first_operand}, {}", child.as_str()))
              },
            //   Rule::operation => {
            //     let mut children2 = child.children();
            //     let operator = children2.next().unwrap().as_str();
            //     let symbol = match operator {
            //       "+" => "ADD",
            //       "-" => "SUB",
            //       "*" => "MULT",
            //       "/" => "DIV",
            //       "%" => "MOD",
            //       _ => unreachable!()
            //     };

            //     let second = children2.next().unwrap();
            //     match second.as_rule() {
            //       Rule::number => {
            //         result.push(format!("SET $__MATH_RESULT__, {}", second.as_str()))
            //       }
            //       Rule::operation => {
            //         result.push(Self::math(second)?);
            //       }
            //       _ => unreachable!()
            //     }

            //     // println!("@r = {:#?}", second.as_rule());

            //     result.push(format!("{} {}, $__MATH_RESULT__\r\nMOV $__MATH_RESULT__, {}\r\n", symbol, first_operand, first_operand))
            //   },
              _ => {
                println!("{:?}", child.as_rule());
                unreachable!()
              }
            }
        }

      let mut buf = String::new();

      for line in result {
        buf.push_str(&(line.to_owned() + "\r\n"));
      }

      Ok(buf)
    }
  
    fn variable(input: Node) -> Result<String> {
        println!("varrrr!");
        let mut parts = input.children();
        let name = parts.next().unwrap().as_str();
        let val = parts.next().unwrap();

        let mut line = LINE_NUMBER.lock().unwrap();
        line.add_assign(10);
  
        let seed = gen_seed!();
        let x = val.children().next().unwrap();

        let new_name = format!("${name}_{seed}");

        let mut vars = VARIABLE_MAPPING.lock().unwrap();

        let rule = x.as_rule();
        let val: String = match rule {
            Rule::number
            | Rule::boolean
            | Rule::string_literal_double
            | Rule::string_literal_single => {
                let d = x.as_str().to_string();
                format!("SET {new_name}, {d}")
            }
          
            Rule::function_call => Self::function_call(x)?.to_string(),
            Rule::ident => {
                let mut buf = String::new();

                let other = x.as_str();
              
                buf.push_str(format!("SET {new_name}, 0\r\n").as_str());

                buf = buf + format!("MOV {new_name}, {}", vars.get(other).unwrap()).as_str();
              
                buf
            },
            // Rule::primary => {
            //   println!("maaath");
            //   format!("{}\r\nSET {new_name}, 0\r\nMOV {new_name}, $__MATH_RESULT__", Self::primary(x)?)
            // }
            Rule::group => {
                let tt = parse(x.as_str());
                println!("{:#?}", tt);
                // x.primary()
                
                "".into()
            }
            _ => panic!("undefined: {:?}", rule),
        };
      
        vars.insert(name.to_string(), new_name.to_string());

        Ok(format!("{val}"))
    }

    fn function_body(input: Node) -> Result<Vec<String>> {
        let mut result = Vec::new();
        for statement in input.children() {
            for part in statement.children() {
                let rule = part.as_rule();
                match rule {
                    Rule::variable => result.push(Self::variable(part)? + "\r\n"),
                    _ => panic!("not implemented"),
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
        .open(compiled_path).unwrap();

    let _ = file.write_all(buf.as_bytes());
    Ok(())
}
