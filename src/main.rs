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
    let power_rule = Rule::new(Expr::from_str("x^2"), Expr::from_str("x*x"));
    let mut new_expresions = vec![];
    let mut subject = Expr::from_str("((5^2)^2) * ((x*y)^2)");
    println!("subject: {subject}");
    println!("{} => {}\n", power_rule.pattern, power_rule.substitute);

    subject.create_variations(&power_rule, &mut new_expresions);
    for i in new_expresions {
        println!("{i}");
    }
}
