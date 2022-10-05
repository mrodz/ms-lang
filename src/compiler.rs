use std::fs::File;
use std::io::prelude::*;
use pest_consume::Parser as ParserDerive;
use pest_consume::Error;

type Result<T> = std::result::Result<T, Error<Rule>>;
type Node<'i> = pest_consume::Node<'i, Rule, ()>;

#[derive(ParserDerive)]
#[grammar = "grammar.pest"]
struct Parser;

#[pest_consume::parser]
impl Parser {
    fn valid_ident(input: Node) -> Result<()> {
      println!("{:?}", input);

      Ok(())
    }

    fn file(input: Node) -> Result<Vec<String>> {
      println!("@ {:?}", input);
      
      // Ok(input);
      Ok(vec!["".to_string()])
    }
}

pub fn compile(path: &String) -> Result<()> {
  use crate::files::read_file;
  
  let fio = read_file(path).unwrap();
  println!("{}", fio);

  let mut i = path.len();
  let chars = path.chars().rev();
  for char in chars {
    i -= 1;
    if char == '.' {
      break;
    }
  }

  let path_without_extension = path.get(0..i).expect("invalid path");

  let inputs = Parser::parse(Rule::file, fio.as_str()).unwrap();

  let input = inputs.single().unwrap();

  let c = Parser::file(input);

  println!("{:?}", c);
  
  let compiled_path = path_without_extension.to_owned() + ".mmm";
  
  let mut file = File::create(compiled_path);
  Ok(())
}