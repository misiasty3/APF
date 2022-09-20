use std::iter::Peekable;
use crate::library::{Expr, Val};

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
            Some((_, ')')) |
            None => return field1,
            _   => (),
        }
        return match parse.next() {
            Some((_, '+'))  => Expr::ADD(Box::new(field1), Box::new(get_field(parse))),
            Some((_, '-'))  => Expr::SUB(Box::new(field1), Box::new(get_field(parse))),
            Some((_, '*'))  => Expr::MUL(Box::new(field1), Box::new(get_field(parse))),
            Some((_, '/'))  => Expr::DIV(Box::new(field1), Box::new(get_field(parse))),
            Some((_, '^'))  => Expr::PWR(Box::new(field1), Box::new(get_field(parse))),
            v           => panic!("Unexpected character '{}' at {}", v.unwrap().1, v.unwrap().0 + 1),
        }
    }

    pub fn from_str(x: &str) -> Expr {
        let mut iter = x.chars().filter(|x| !x.is_whitespace()).enumerate().peekable();
        let foo = Expr::from_iter(&mut iter);
        let next = iter.peek();
        assert_eq!(next, None, "Unexpected token found at {}", next.unwrap().0 + 1);
        foo
    }
}

mod tests {
    use crate::library::{Expr, Val};

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
}
