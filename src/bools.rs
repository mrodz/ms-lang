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
pub enum BoolExpr {
    BinOp(Box<BoolExpr>, BinOpKind, Box<BoolExpr>),
    UnOp(UnOpKind, Box<BoolExpr>),
    Boolean(bool),
    Variable(String)
}

#[derive(Debug, Eq, PartialEq)]
pub enum BinOpKind {
    And,
    Or,
    Xor,
}

#[derive(Debug, Eq, PartialEq)]
pub enum UnOpKind {
    Not,
    // Neg,
    // Try,
}

struct ExprParser;

impl<'i, I> PrattParser<I> for ExprParser
where
    I: Iterator<Item = Pair<'i, Rule>>,
{
    type Error = pratt::NoError;
    type Input = Pair<'i, Rule>;
    type Output = BoolExpr;

    // Query information about an operator (Affix, Precedence, Associativity)
    fn query(&mut self, tree: &Self::Input) -> Result<Affix> {
        let rule = tree.as_rule();
        let affix = match (rule, tree.as_str()) {
            // (Rule::infix, "=") => Affix::Infix(Precedence(2), Associativity::Neither),
            (Rule::boolean_infix, "&&") => Affix::Infix(Precedence(4), Associativity::Left),
            (Rule::boolean_infix, "||") => Affix::Infix(Precedence(3), Associativity::Left),
            (Rule::boolean_infix, "^") => Affix::Infix(Precedence(3), Associativity::Left),
            // (Rule::infix, "/") => Affix::Infix(Precedence(4), Associativity::Left),
            // (Rule::infix, "%") => Affix::Infix(Precedence(4), Associativity::Left),
            // (Rule::postfix, "?") => Affix::Postfix(Precedence(5)),
            // (Rule::prefix, "-") => Affix::Prefix(Precedence(6)),
            (Rule::boolean_prefix, "!") => Affix::Prefix(Precedence(6)),
            // (Rule::infix, "^") => Affix::Infix(Precedence(7), Associativity::Right),
            (Rule::boolean_group, _) => Affix::Nilfix,
            (Rule::boolean_primary, _) => Affix::Nilfix,
            (Rule::boolean, _) 
            | (Rule::ident, _) => Affix::Nilfix,
            // (Rule::ident, _) => Affix::Nilfix
            _ => unreachable!("no rule implemented for {:?}", rule),
        };
        Ok(affix)
    }

    // Construct a primary expression, e.g. a number
    fn primary(&mut self, tree: Self::Input) -> Result<BoolExpr> {
        let rule = tree.as_rule();
        let expr = match rule {
            Rule::boolean => {
                let str = tree.as_str().trim();

                let str_f32 = str.parse().unwrap();
                BoolExpr::Boolean(str_f32)
            }
            Rule::ident => {
                let str = tree.as_str().trim();
                BoolExpr::Variable(str.to_string())
            }
            Rule::boolean_group => self.parse(&mut tree.into_inner()).unwrap(),
            _ => panic!("not implemented: {:?}", rule),
        };
        Ok(expr)
    }

    // Construct a binary infix expression, e.g. 1+1
    fn infix(&mut self, lhs: BoolExpr, tree: Self::Input, rhs: BoolExpr) -> Result<BoolExpr> {
        let op = match tree.as_str() {
            "&&" => BinOpKind::And,
            "||" => BinOpKind::Or,
            "^" => BinOpKind::Xor,
            _ => unreachable!(),
        };
        Ok(BoolExpr::BinOp(Box::new(lhs), op, Box::new(rhs)))
    }

    // Construct a unary prefix expression, e.g. !1
    fn prefix(&mut self, tree: Self::Input, rhs: BoolExpr) -> Result<BoolExpr> {
        let op = match tree.as_str() {
            "!" => UnOpKind::Not,
            _ => unreachable!(),
        };
        Ok(BoolExpr::UnOp(op, Box::new(rhs)))
    }

    fn postfix(&mut self, _lhs: BoolExpr, _tree: Self::Input) -> Result<BoolExpr> {
        unimplemented!()
    }
}

pub fn parse(input: &str) -> BoolExpr {
    let tt = TokenTreeParser::parse(Rule::boolean_group, &input)
        .unwrap()
        .into_iter();
    ExprParser.parse(tt.into_iter()).unwrap()
}