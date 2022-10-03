use std::collections::{VecDeque, HashSet, HashMap};
use std::hash::{Hash, Hasher};
use std::fmt;

#[derive(Debug, Clone)]
#[repr(u8)]
pub enum Expr {
    VAL(Val),
    ABS(Box<Expr>),
//  SIN(Box<Expr>),
//  COS(Box<Expr>),
//  TAN(Box<Expr>),
    ADD(Box<Expr>, Box<Expr>),
    SUB(Box<Expr>, Box<Expr>),
    MUL(Box<Expr>, Box<Expr>),
    DIV(Box<Expr>, Box<Expr>),
    PWR(Box<Expr>, Box<Expr>),
//  ROT(Box<Expr>, Box<Expr>),
//  LOG(Box<Expr>, Box<Expr>),
//  LIM(Box<Expr>, Box<Expr>, Box<Expr>),
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum Val {
    INT(i64),
    VAR(String),
    PI,
    MPi,
}

pub struct Rule {
    pub pattern: Expr,
    pub substitute: Expr,
}

pub struct Proof {
    p: Result<Vec<(Expr, Expr)>, &'static str>
}

pub struct Settings {
    max_equation_length_in_bytes: usize,
    max_equations_tried: usize,
    stop_on_first_proof: bool,
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Expr::VAL(a), Expr::VAL(b)) => a == b,
            (Expr::ABS(a), Expr::ABS(b)) => a == b,

            (Expr::ADD(a, b), Expr::ADD(c, d)) |
            (Expr::SUB(a, b), Expr::SUB(c, d)) |
            (Expr::MUL(a, b), Expr::MUL(c, d)) |
            (Expr::DIV(a, b), Expr::DIV(c, d)) |
            (Expr::PWR(a, b), Expr::PWR(c, d)) => (a == c) && (b == d),

            _ => false,
        }
    }
}

impl Hash for Expr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Expr::VAL(a)    => {state.write_u8(0); a.hash(state);},
            Expr::ABS(a)    => {state.write_u8(1); a.hash(state);},
            Expr::ADD(a, b) => {state.write_u8(2); a.hash(state); b.hash(state)},
            Expr::SUB(a, b) => {state.write_u8(3); a.hash(state); b.hash(state)},
            Expr::MUL(a, b) => {state.write_u8(4); a.hash(state); b.hash(state)},
            Expr::DIV(a, b) => {state.write_u8(5); a.hash(state); b.hash(state)},
            Expr::PWR(a, b) => {state.write_u8(6); a.hash(state); b.hash(state)},
        }
    }
}


impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::VAL(a)    => write!(f, "{}", a),
            Expr::ABS(a)    => write!(f, "abs({})", a),
            Expr::ADD(a, b) => write!(f, "({} + {})", a, b),
            Expr::SUB(a, b) => write!(f, "({} - {})", a, b),
            Expr::MUL(a, b) => write!(f, "({} * {})", a, b),
            Expr::DIV(a, b) => write!(f, "({} / {})", a, b),
            Expr::PWR(a, b) => write!(f, "({} ^ {})", a, b),
        }
    }
}
            
impl Expr {
    pub fn create_variations(&mut self, rule: &Rule, new_variations: &mut Vec<Expr>) {
        unsafe {
            let ptr: *mut Expr = self;
            let sl = &mut *ptr;
            sl.apply_rule_all(&*ptr, rule, new_variations);
        }
    }
    fn apply_rule_all(&mut self, origin: &Expr, rule: &Rule, new_variations: &mut Vec<Expr>) {
        fn apply_rule<'a>(ex: &'a Expr, pattern: &'a Expr, bindings: &mut HashMap<&'a String, &'a Expr>) -> bool {
            match (ex, pattern) {
                (Expr::ABS(a), Expr::ABS(b)) => apply_rule(a, b, bindings),
                (Expr::ADD(a, b), Expr::ADD(c, d)) |
                (Expr::SUB(a, b), Expr::SUB(c, d)) |
                (Expr::MUL(a, b), Expr::MUL(c, d)) |
                (Expr::DIV(a, b), Expr::DIV(c, d)) |
                (Expr::PWR(a, b), Expr::PWR(c, d)) => {
                    apply_rule(a, c, bindings) &&
                    apply_rule(b, d, bindings)
                },
                (a, Expr::VAL(Val::VAR(name))) => 
                    match bindings.get(name) {
                        Some(x) => a == *x,
                        None => {bindings.insert(name, a); true},
                    }
                (Expr::VAL(a), Expr::VAL(b)) => a == b,
                _ => false,
            }
        }

        match self {
            Expr::VAL(_) => {},
            Expr::ABS(a) => a.apply_rule_all(origin, rule, new_variations),
            Expr::ADD(a, b) |
            Expr::SUB(a, b) |
            Expr::MUL(a, b) |
            Expr::DIV(a, b) |
            Expr::PWR(a, b) => {
                a.apply_rule_all(origin, rule, new_variations);
                b.apply_rule_all(origin, rule, new_variations);
            },
        }

        let mut bindings = HashMap::new();
        if apply_rule(&self, &rule.pattern, &mut bindings) {
            let cln = self.clone();
            *self = Expr::from_rule(&rule.substitute, &mut bindings);
            new_variations.push(origin.clone());
            *self = cln;
        }
    }
    pub fn from_rule(substitute: &Expr, bindings: &mut HashMap<&String, &Expr>) -> Expr {
        match substitute {
            Expr::ABS(a) => Expr::from_rule(a, bindings),
            Expr::ADD(a, b) => Expr::ADD(Box::new(Expr::from_rule(a, bindings)), Box::new(Expr::from_rule(b, bindings))),
            Expr::SUB(a, b) => Expr::SUB(Box::new(Expr::from_rule(a, bindings)), Box::new(Expr::from_rule(b, bindings))),
            Expr::MUL(a, b) => Expr::MUL(Box::new(Expr::from_rule(a, bindings)), Box::new(Expr::from_rule(b, bindings))),
            Expr::DIV(a, b) => Expr::DIV(Box::new(Expr::from_rule(a, bindings)), Box::new(Expr::from_rule(b, bindings))),
            Expr::PWR(a, b) => Expr::PWR(Box::new(Expr::from_rule(a, bindings)), Box::new(Expr::from_rule(b, bindings))),
            Expr::VAL(Val::VAR(name)) => (*bindings.get(&name).expect(format!("no binding \"{}\" in HashMap {:?}", name, bindings).as_str())).clone(),
            Expr::VAL(a) => Expr::VAL(a.clone()),
        }
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Val::INT(a) => write!(f, "{}", a),
            Val::VAR(a) => write!(f, "{}", a),
            _   => todo!(),
        }
    }
}

impl Rule {
    pub fn new(p: Expr, s: Expr) -> Self {
        Self {pattern: p, substitute: s}
    }
}

impl Default for Settings {
    fn default() -> Self {
        Settings {
            max_equation_length_in_bytes: 1024,
            max_equations_tried: 10000,
            stop_on_first_proof: true,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::library::{Expr, Val};
    use std::collections::hash_map::DefaultHasher;
    use std::hash::Hasher;
    use std::hash::Hash;

    #[test]
    #[allow(non_snake_case)]
    fn PartialEq_end_Hash() {
        let mut eq1;
        let mut eq2;
        
        // 2 == 2
        eq1 = Expr::VAL(Val::INT(2));
        eq2 = Expr::VAL(Val::INT(2));
        assert_eq!(eq1, eq2);
        assert_eq!(calculate_hash(&eq1), calculate_hash(&eq2));

        // 2 != 5
        eq1 = Expr::VAL(Val::INT(2));
        eq2 = Expr::VAL(Val::INT(5));
        assert_ne!(eq1, eq2);
        assert_ne!(calculate_hash(&eq1), calculate_hash(&eq2));

        // 2+5 == 2+5
        eq1 = Expr::ADD(
            Box::new(Expr::VAL(Val::INT(2))),
            Box::new(Expr::VAL(Val::INT(5))));
        eq2 = Expr::ADD(
            Box::new(Expr::VAL(Val::INT(2))),
            Box::new(Expr::VAL(Val::INT(5))));
        assert_eq!(eq1, eq2);
        assert_eq!(calculate_hash(&eq1), calculate_hash(&eq2));

       // 2-5 != 5-2
        eq1 = Expr::SUB(
            Box::new(Expr::VAL(Val::INT(2))),
            Box::new(Expr::VAL(Val::INT(5))));
        eq2 = Expr::SUB(
            Box::new(Expr::VAL(Val::INT(5))),
            Box::new(Expr::VAL(Val::INT(2))));
        assert_ne!(eq1, eq2);
        assert_ne!(calculate_hash(&eq1), calculate_hash(&eq2));

        // (5*2) + (3+4) == (5*2) + (3+4)
        eq1 = Expr::ADD(
            Box::new(Expr::MUL(
                    Box::new(Expr::VAL(Val::INT(5))),
                    Box::new(Expr::VAL(Val::INT(2))))),
            Box::new(Expr::ADD(
                    Box::new(Expr::VAL(Val::INT(3))),
                    Box::new(Expr::VAL(Val::INT(4))))));
        eq2 = Expr::ADD(
            Box::new(Expr::MUL(
                    Box::new(Expr::VAL(Val::INT(5))),
                    Box::new(Expr::VAL(Val::INT(2))))),
            Box::new(Expr::ADD(
                    Box::new(Expr::VAL(Val::INT(3))),
                    Box::new(Expr::VAL(Val::INT(4))))));
        assert_eq!(eq1, eq2);
        assert_eq!(calculate_hash(&eq1), calculate_hash(&eq2));
    }

    fn calculate_hash<T: Hash>(t: &T) -> u64 {
        let mut s = DefaultHasher::new();
        t.hash(&mut s);
        s.finish()
    }
}
