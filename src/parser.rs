use std::iter::Peekable;
use std::collections::HashMap;
use crate::library::{Expr, Val};

// NOTE: Apparently I am very bad a creating parsers, so this etire file should be refactored

impl Expr {
    pub fn from_iter<I: Iterator<Item = (usize, char)>>(parse: &mut Peekable<I>) -> Expr {
        fn get_field<I: Iterator<Item = (usize, char)>>(parse: &mut Peekable<I>) -> Expr {
            match parse.peek().unwrap() {
                (_, '(') => {parse.next(); let foo = Expr::from_iter(parse); parse.next(); return foo},
                v    => {
                    if v.1.is_numeric() {
                        let mut num: i64 = 0;
                        while let Some((_, x)) = parse.next_if(|y| y.1.is_numeric()) {
                            num = num*10 + x.to_digit(10).unwrap() as i64;
                        }
                        return Expr::VAL(Val::INT(num))
                    } else if v.1.is_alphabetic() {
                        let mut text = "".to_string();
                        while let Some((_, x)) = parse.next_if(|y| y.1.is_alphabetic()) {
                            text.push(x)
                        }
                        match parse.peek() {
                            Some((_, '(')) => {
                                parse.next();
                                let foo = match text.as_str() {
                                    "abs" => Expr::ABS(Box::new(Expr::from_iter(parse))),
                                    _ => panic!(),
                                };
                                parse.next();
                                return foo;},
                            _   => return Expr::VAL(Val::VAR(text)),
                        }
                    } else {
                        panic!("Unexpected character '{}' at {}", v.1, v.0 + 1);
                    }
                }
            };
        }

        let field1 = get_field(parse);
        match parse.peek() {
            Some((_, ')')) => {parse.next();},
            _   => (),
        }
        return match parse.next() {
            Some((_, '+'))  => Expr::ADD(Box::new(field1), Box::new(get_field(parse))),
            Some((_, '-'))  => Expr::SUB(Box::new(field1), Box::new(get_field(parse))),
            Some((_, '*'))  => Expr::MUL(Box::new(field1), Box::new(get_field(parse))),
            Some((_, '/'))  => Expr::DIV(Box::new(field1), Box::new(get_field(parse))),
            Some((_, '^'))  => Expr::PWR(Box::new(field1), Box::new(get_field(parse))),
            //Some((_, ')'))  => parse.next(),
            None        => field1,
            v           => panic!("Unexpected character '{}' at {}", v.unwrap().1, v.unwrap().0 + 1),
        }
    }

    pub fn from_string(x: String) -> Expr {
        let mut iter = x.chars().filter(|x| !x.is_whitespace()).enumerate().peekable();
        let foo = Expr::from_iter(&mut iter);
        let next = iter.peek();
        assert_eq!(next, None, "Unexpected token found at {}", next.unwrap().0 + 1);
        foo
    }

    pub fn from_str(x: &str) -> Expr {
        let mut iter = x.chars().filter(|x| !x.is_whitespace()).enumerate().peekable();
        let foo = Expr::from_iter(&mut iter);
        let next = iter.peek();
        assert_eq!(next, None, "Unexpected token found at {}", next.unwrap().0 + 1);
        foo
    }

    pub fn from_iter_and_bindings<I: Iterator<Item = (usize, char)>>(parse: &mut Peekable<I>, bindings: &HashMap<String, &Expr>) -> Expr {
        fn get_field<I: Iterator<Item = (usize, char)>>(parse: &mut Peekable<I>, bindings: &HashMap<String, &Expr>) -> Expr {
            match parse.peek().unwrap() {
                (_, '(') => {parse.next(); let foo = Expr::from_iter_and_bindings(parse, bindings); parse.next(); return foo},
                v    => {
                    if v.1.is_numeric() {
                        let mut num: i64 = 0;
                        while let Some((_, x)) = parse.next_if(|y| y.1.is_numeric()) {
                            num = num*10 + x.to_digit(10).unwrap() as i64;
                        }
                        return Expr::VAL(Val::INT(num))
                    } else if v.1.is_alphabetic() {
                        let mut text = "".to_string();
                        while let Some((_, x)) = parse.next_if(|y| y.1.is_alphabetic()) {
                            text.push(x)
                        }
                        match parse.peek() {
                            Some((_, '(')) => {
                                parse.next();
                                return match text.as_str() {
                                    "abs" => Expr::ABS(Box::new(Expr::from_iter_and_bindings(parse, bindings))),
                                    _ => panic!(),
                                }},
                            _   => {
                                let binding = bindings.get(&text);
                                match binding {
                                    Some(a) => return (*a).clone(),
                                    _   => panic!("Rule cant be applied! No binding for varible \"{}\"", text),
                                }
                            }
                        }
                    } else {
                        panic!("Unexpected character '{}' at {}", v.1, v.0 + 1);
                    }
                }
            };
        }

        let field1 = get_field(parse, bindings);
        //if parse.peek() == Some(&')') {parse.next();}
        return match parse.next() {
            Some((_, '+'))   => Expr::ADD(Box::new(field1), Box::new(get_field(parse, bindings))),
            Some((_, '-'))   => Expr::SUB(Box::new(field1), Box::new(get_field(parse, bindings))),
            Some((_, '*'))   => Expr::MUL(Box::new(field1), Box::new(get_field(parse, bindings))),
            Some((_, '/'))   => Expr::DIV(Box::new(field1), Box::new(get_field(parse, bindings))),
            Some((_, '^'))   => Expr::PWR(Box::new(field1), Box::new(get_field(parse, bindings))),
            Some((_, ')'))   => field1,
            None        => field1,
            v           => panic!("Unexpected character '{}' at {}", v.unwrap().1, v.unwrap().0 + 1),
        }
    }

    pub fn from_string_and_bindings(x: String, bindings: &HashMap<String, &Expr>) -> Expr {
        let mut iter = x.chars().filter(|x| !x.is_whitespace()).enumerate().peekable();
        let foo = Expr::from_iter_and_bindings(&mut iter, bindings);
        let next = iter.peek();
        assert_eq!(next, None, "Unexpected token found at {}", next.unwrap().0 + 1);
        foo
    }

    pub fn from_str_and_bindings(x: &str, bindings: &HashMap<String, &Expr>) -> Expr {
        let mut iter = x.chars().filter(|x| !x.is_whitespace()).enumerate().peekable();
        let foo = Expr::from_iter_and_bindings(&mut iter, bindings);
        let next = iter.peek();
        assert_eq!(next, None, "Unexpected token found at {}", next.unwrap().0 + 1);
        foo
    }
}


mod tests {
    use crate::library::{Expr, Val};
    use std::collections::HashMap;

    #[test]
    fn expr_from_str() {
        let mut x;
        let mut y;

        x = Expr::from_str("123");
        y = Expr::VAL(Val::INT(123));
        assert_eq!(x, y);

        x = Expr::from_str("27+15");
        y = Expr::ADD(Box::new(Expr::VAL(Val::INT(27))), Box::new(Expr::VAL(Val::INT(15))));
        assert_eq!(x, y);

        x = Expr::from_str("(27 * 15) - (13 / 1123)");
        y = Expr::SUB(  Box::new(Expr::MUL( Box::new(Expr::VAL(Val::INT(27))),
                                            Box::new(Expr::VAL(Val::INT(15))))),
                        Box::new(Expr::DIV( Box::new(Expr::VAL(Val::INT(13))),
                                            Box::new(Expr::VAL(Val::INT(1123))))));
        assert_eq!(x, y);

        x = Expr::from_str("115^((1112/3)+(22*33))");
        y = Expr::PWR(  Box::new(Expr::VAL(Val::INT(115))),
                        Box::new(Expr::ADD( Box::new(Expr::DIV( Box::new(Expr::VAL(Val::INT(1112))),
                                                                Box::new(Expr::VAL(Val::INT(3))))),
                                            Box::new(Expr::MUL( Box::new(Expr::VAL(Val::INT(22))),
                                                                Box::new(Expr::VAL(Val::INT(33))))))));
        assert_eq!(x, y);

        x = Expr::from_str("abs(x) + (2 - y)"); //i guess
        y = Expr::ADD(  Box::new(Expr::ABS( Box::new(Expr::VAL(Val::VAR("x".to_string()))))),
                        Box::new(Expr::SUB( Box::new(Expr::VAL(Val::INT(2))),
                                            Box::new(Expr::VAL(Val::VAR("y".to_string()))))));
        assert_eq!(x, y);
    }

    #[test]
    fn expr_from_str_and_bindings() {
        let mut bindings;
        let mut x;
        let mut y;

        let binding1 = Expr::from_str("1111*(975+1)");
        bindings = HashMap::from([("a".to_string(), &binding1)]);
        x = Expr::from_str_and_bindings("5*a", &bindings);
        y = Expr::from_str("5*(1111*(975+1))");
        assert_eq!(x, y);

        let binding1 = Expr::from_str("1111*10");
        let binding2 = Expr::from_str("z*(y+2)");
        let binding3 = Expr::from_str("5");
        bindings = HashMap::from([
            ("a".to_string(), &binding1),
            ("b".to_string(), &binding2),
            ("c".to_string(), &binding3),
        ]);
        x = Expr::from_str_and_bindings("(abs(a)^c)+(c-b)", &bindings);
        y = Expr::from_str("(abs(1111*10)^5)+(5-(z*(y+2)))");
        assert_eq!(x, y);
    }
}
