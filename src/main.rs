pub mod parser;
pub mod library;

use std::collections::{VecDeque, HashMap};
use library::{Expr, Rule, ExSide};
use std::rc::Rc;

type Proof = Result<Vec<(Expr, Expr)>, &'static str>;

fn find_proof(eq1: Expr, eq2: Expr, rules: &Vec<Rule>) -> Proof {
    if eq1 == eq2 {return Ok(vec![(eq1, eq2)])}

    let mut pending: VecDeque<(Rc<Expr>, Option<Rc<Expr>>, ExSide)> = VecDeque::new();
    let mut tried: HashMap<Rc<Expr>, (Option<Rc<Expr>>, ExSide)> = HashMap::new();

    let left = Rc::new(eq1);
    let right = Rc::new(eq2);

    pending.push_back((Rc::clone(&right), None, ExSide::Right));
    pending.push_back((Rc::clone(&left), None, ExSide::Left));

    tried.insert(left, (None, ExSide::Left));
    tried.insert(right, (None, ExSide::Right));

    while let Some((next, _, side)) = pending.pop_front() {
        let mut inside = (*next).clone();
        let mut new_variations = vec![];
        for rule in rules {
            inside.create_variations(&rule, &mut new_variations);
        }
        for variation in new_variations {
            match tried.get(&variation) {
                Some((_, x)) => if *x != side {
                    let mut ret: Vec<(Expr, Expr)> = vec![];
                    let get_parent = |x: &Expr| -> Expr {
                        //let foo = tried.get(x).as_ref().unwrap().0.as_ref().unwrap().as_ref();
                        let foo = &tried.get(x).as_ref().unwrap().0;
                        match foo {
                            Some(y) => (**y).clone(),
                            None => x.clone(),
                        }
                    };
                    let parent = get_parent(&variation);
                    ret.push((variation.clone(), variation));
                    ret.push((next.as_ref().clone(), parent));
                    loop {
                        let last_ref = &ret[ret.len()-1];
                        let new_parent1 = get_parent(&last_ref.0);
                        let new_parent2 = get_parent(&last_ref.1);

                        if ret[ret.len()-1].0 == new_parent1 && ret[ret.len()-1].1 == new_parent2 {
                            break;
                        } else {
                            ret.push((new_parent1, new_parent2));
                        }
                    }
                    return Ok(ret);
                },
                None => {
                    let new = Rc::new(variation);
                    pending.push_back((Rc::clone(&new), Some(Rc::clone(&next)), side));
                    tried.insert(new, (Some(Rc::clone(&next)), side));
                }
            }
        }
    }
    
    return Err("Checked all possibilities, answer couldn't be found.");
}

fn main() {
    let communitive_addition = Rule::new(Expr::from_str("x+y"), Expr::from_str("y+x"));
    let assosiative_addition = Rule::new(Expr::from_str("(a+b)+c"), Expr::from_str("a+(b+c)"));
    let communitive_multiplication = Rule::new(Expr::from_str("x*y"), Expr::from_str("y*x"));
    let assosiative_multiplication = Rule::new(Expr::from_str("a*(b+c)"), Expr::from_str("(a*b)+(a*c)"));
    let square_power = Rule::new(Expr::from_str("x^2"), Expr::from_str("x*x"));
    let square_power_rev = Rule::new(Expr::from_str("x*x"), Expr::from_str("x^2"));
    let double = Rule::new(Expr::from_str("2*x"), Expr::from_str("x+x"));
    let double_rev = Rule::new(Expr::from_str("x+x"), Expr::from_str("2*x"));
    let rules = vec![communitive_addition, assosiative_addition, communitive_multiplication, assosiative_multiplication, square_power, square_power_rev, double, double_rev];

    let eq1 = Expr::from_str("(a+b)^2");
    let eq2 = Expr::from_str("((a^2) + (2*(a*b))) + (b^2)");
    let proof = find_proof(eq1, eq2, &rules);
    match proof {
        Ok(x) => {
            for pair in x.iter().rev() {
                println!("{}        ==        {}", pair.0, pair.1);
            }
        }
        Err(y) => println!("{}", y)
    }
}
