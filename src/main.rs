mod lexer;
use lexer::Lexer;
mod expr;
use expr::Expr;
mod parse;
use parse::parse_expr;

fn is_value(expr: &Expr) -> bool {
    match expr {
        Expr::Epsilon => true,
        Expr::Boolean(_) => true,
        Expr::Number(_) => true,
        Expr::Variable(_) => true,
        Expr::Fun(_,_) => true,
        Expr::Quote(_,_) => true,
        _ => false
    }
}

fn eval_one_step(expr: Expr) -> Expr {
    if is_value(&expr) {
        expr
    } else {
        match expr {
            Expr::Plus(e1, e2) => {
                if !is_value(&*e1) {
                    let e3 = eval_one_step(*e1);
                    Expr::Plus(Box::new(e3), e2)
                } else if !is_value(&*e2) {
                    let e3 = eval_one_step(*e2);
                    Expr::Plus(e1, Box::new(e3))
                } else {
                    match (*e1, *e2) {
                        (Expr::Number(i1), Expr::Number(i2)) => Expr::Number(i1+i2),
                        _ => unreachable!()
                    }
                }
            },
            Expr::Times(e1, e2) => {
                if !is_value(&*e1) {
                    let e3 = eval_one_step(*e1);
                    Expr::Plus(Box::new(e3), e2)
                } else if !is_value(&*e2) {
                    let e3 = eval_one_step(*e2);
                    Expr::Plus(e1, Box::new(e3))
                } else {
                    match (*e1, *e2) {
                        (Expr::Number(i1), Expr::Number(i2)) => Expr::Number(i1*i2),
                        _ => unreachable!()
                    }
                }
            },
            Expr::Minus(e1, e2) => {
                if !is_value(&*e1) {
                    let e3 = eval_one_step(*e1);
                    Expr::Plus(Box::new(e3), e2)
                } else if !is_value(&*e2) {
                    let e3 = eval_one_step(*e2);
                    Expr::Plus(e1, Box::new(e3))
                } else {
                    match (*e1, *e2) {
                        (Expr::Number(i1), Expr::Number(i2)) => Expr::Number(i1-i2),
                        _ => unreachable!()
                    }
                }
            },
            Expr::Divides(e1, e2) => {
                if !is_value(&*e1) {
                    let e3 = eval_one_step(*e1);
                    Expr::Plus(Box::new(e3), e2)
                } else if !is_value(&*e2) {
                    let e3 = eval_one_step(*e2);
                    Expr::Plus(e1, Box::new(e3))
                } else {
                    match (*e1, *e2) {
                        (Expr::Number(i1), Expr::Number(i2)) => Expr::Number(i1/i2),
                        _ => unreachable!()
                    }
                }
            },
            _ => unreachable!()
        }
    }
}

fn test_with_input_string(vs: Vec<String>) {
    for s in vs {
        let mut lexer = Lexer::new(s);
        let r = parse_expr(&mut lexer);
        println!("{:?}", r);
        match r {
            Ok(e) => {
                let mut e2 = e;
                println!("{:?}", e2);
                while !is_value(&e2) {
                    e2 = eval_one_step(e2);
                    println!("{:?}", e2);
                }
            },
            Err(_) => ()
        }
        println!("========================");
    }
}

fn main() {
    let inputs = vec![
        String::from("1123 + 345"),
        String::from("1123 + 345 * 43 - 10"),
        String::from("x + 1123 * 345 * (99 + (fun x -> x) y)"),
        String::from("true"),
        String::from("epsilon"),
        String::from("if true then 1 + 3 else x"),
        String::from("fun x -> x + 2"),
        String::from("(quote a x) + 1"),
        String::from("(unquote a (x + y + z)) + 1"),
        String::from("(fun a -> (quote a 1 + 2)) epsilon true")];
    test_with_input_string(inputs)
}
