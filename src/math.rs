/// Huge thanks to @segeljakt
/// https://github.com/segeljakt/pratt
///
/// His solution to parsing PEMDAS is wonderful.
use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct TokenTreeParser;

use pratt::{Affix, Associativity, PrattParser, Precedence, Result};

#[derive(Debug, PartialEq)]
pub enum MathExpr {
    BinOp(Box<MathExpr>, BinOpKind, Box<MathExpr>),
    UnOp(UnOpKind, Box<MathExpr>),
    Number(f32),
    Variable(String),
    String(String)
}

#[derive(Debug, Eq, PartialEq)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Eq, PartialEq)]
pub enum UnOpKind {
    // Not,
    Neg,
    // Try,
}

struct ExprParser;

impl<'i, I> PrattParser<I> for ExprParser
where
    I: Iterator<Item = Pair<'i, Rule>>,
{
    type Error = pratt::NoError;
    type Input = Pair<'i, Rule>;
    type Output = MathExpr;

    // Query information about an operator (Affix, Precedence, Associativity)
    fn query(&mut self, tree: &Self::Input) -> Result<Affix> {
        let rule = tree.as_rule();
        let affix = match (rule, tree.as_str()) {
            // (Rule::infix, "=") => Affix::Infix(Precedence(2), Associativity::Neither),
            (Rule::infix, "+") => Affix::Infix(Precedence(3), Associativity::Left),
            (Rule::infix, "-") => Affix::Infix(Precedence(3), Associativity::Left),
            (Rule::infix, "*") => Affix::Infix(Precedence(4), Associativity::Left),
            (Rule::infix, "/") => Affix::Infix(Precedence(4), Associativity::Left),
            (Rule::infix, "%") => Affix::Infix(Precedence(4), Associativity::Left),
            // (Rule::postfix, "?") => Affix::Postfix(Precedence(5)),
            (Rule::prefix, "-") => Affix::Prefix(Precedence(6)),
            (Rule::prefix, "!") => Affix::Prefix(Precedence(6)),
            // (Rule::infix, "^") => Affix::Infix(Precedence(7), Associativity::Right),
            (Rule::group, _) => Affix::Nilfix,
            (Rule::primary, _) => Affix::Nilfix,
            (Rule::number, _) 
            | (Rule::ident, _)
            | (Rule::string_literal_double, _) => Affix::Nilfix,
            // (Rule::ident, _) => Affix::Nilfix
            _ => unreachable!("no rule implemented for {:?}", rule),
        };
        Ok(affix)
    }

    // Construct a primary expression, e.g. a number
    fn primary(&mut self, tree: Self::Input) -> Result<MathExpr> {
        let rule = tree.as_rule();
        let expr = match rule {
            Rule::number => {
                let str = tree.as_str().trim();

                let str_f32 = str.parse().unwrap();
                MathExpr::Number(str_f32)
            }
            Rule::string_literal_double => {
                let str = tree.as_str().trim();
                MathExpr::String(str.to_string())
            }
            Rule::ident => {
                let str = tree.as_str().trim();
                MathExpr::Variable(str.to_string())
            }
            Rule::group => self.parse(&mut tree.into_inner()).unwrap(),
            _ => panic!("not implemented: {:?}", rule),
        };
        Ok(expr)
    }

    // Construct a binary infix expression, e.g. 1+1
    fn infix(&mut self, lhs: MathExpr, tree: Self::Input, rhs: MathExpr) -> Result<MathExpr> {
        let op = match tree.as_str() {
            "+" => BinOpKind::Add,
            "-" => BinOpKind::Sub,
            "*" => BinOpKind::Mul,
            "/" => BinOpKind::Div,
            "%" => BinOpKind::Mod,
            // "=" => BinOpKind::Eq,
            _ => unreachable!(),
        };
        Ok(MathExpr::BinOp(Box::new(lhs), op, Box::new(rhs)))
    }

    // Construct a unary prefix expression, e.g. !1
    fn prefix(&mut self, tree: Self::Input, rhs: MathExpr) -> Result<MathExpr> {
        let op = match tree.as_str() {
            // "!" => UnOpKind::Not,
            "-" => UnOpKind::Neg,
            _ => unreachable!(),
        };
        Ok(MathExpr::UnOp(op, Box::new(rhs)))
    }

    // Construct a unary postfix expression, e.g. 1?
    fn postfix(&mut self, _lhs: MathExpr, _tree: Self::Input) -> Result<MathExpr> {
        unimplemented!()
        // let op = match tree.as_str() {
        //     "?" => UnOpKind::Try,
        //     _ => unreachable!(),
        // };
        // Ok(MathExpr::UnOp(op, Box::new(lhs)))
    }
}

pub fn parse(input: &str) -> MathExpr {
    let tt = TokenTreeParser::parse(Rule::group, &input)
        .unwrap()
        .into_iter();
    ExprParser.parse(tt.into_iter()).unwrap()
}

// fn main() {
//     let mut args = std::env::args();
//     let _ = args.next();

//     let input = args.next().expect("Expected input string");
//     println!("Code: {}", input);

//     let tt = TokenTreeParser::parse(Rule::group, &input).unwrap_or_else(|e| panic!("{}", e));
//     println!("TokenTree: {:?}", tt);

//     let expr = ExprParser.parse(tt.into_iter()).unwrap();
//     println!("Expression: {:?}", expr);
// }

