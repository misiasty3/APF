pub mod parser;
pub mod library;

use std::collections::{VecDeque, HashSet};
use library::{Expr, Val, Rule, Settings};

fn find_proof(eq1: Expr, eq2: Expr, rules: Vec<Rule>, set: Settings) -> Result<Vec<(Expr, Expr)>, &'static str> {
    if eq1 == eq2 {return Ok(vec![(eq1, eq2)])}

    let mut tried: HashSet<Expr> = HashSet::new();
    let mut pending: VecDeque<Expr> = VecDeque::new();
    pending.push_back(eq1);

    while !pending.is_empty() {
        pending.pop_front();
    }
    
    return Err("Checked all possibilities, answer couldn't be found.")
}


fn main() {
    println!("{}", Expr::from_str("(x) + (2 - y)"));
}
