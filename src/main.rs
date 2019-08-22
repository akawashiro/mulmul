mod lexer;
use lexer::Lexer;
mod expr;
use expr::Expr;
use expr::Variable;
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
            Expr::App(e1, e2) => {
                if !is_value(&*e2) {
                    Expr::App(e1, Box::new(eval_one_step(*e2)))
                } else if !is_value(&*e1) {
                    Expr::App(Box::new(eval_one_step(*e1)), e2)
                } else {
                    match *e1 {
                        Expr::Fun(v, e3) => {
                            subst_expr(&v, *e2, *e3)
                        },
                        _ => unreachable!()
                    }
                }
            },
            Expr::Epsilon | Expr::Boolean(_) | Expr::Number(_) | Expr::Variable(_) | Expr::Fun(_,_) | Expr::Quote(_,_) => unreachable!(),
            _ => unreachable!()
        }
    }
}

fn subst_expr(var: &Variable, arg: Expr, expr: Expr) -> Expr {
    match expr {
        Expr::Variable(v) => {
            if v == *var {
                arg
            } else {
                Expr::Variable(v)
            }
        },
        Expr::Epsilon | Expr::Boolean(_) | Expr::Number(_) => expr,
        Expr::Plus(e1,e2) => {
            Expr::Plus(Box::new(subst_expr(var, arg.clone(), *e1)), Box::new(subst_expr(var, arg, *e2)))
        },
        Expr::Minus(e1,e2) => {
            Expr::Minus(Box::new(subst_expr(var, arg.clone(), *e1)), Box::new(subst_expr(var, arg, *e2)))
        },
        Expr::Times(e1,e2) => {
            Expr::Times(Box::new(subst_expr(var, arg.clone(), *e1)), Box::new(subst_expr(var, arg, *e2)))
        },
        Expr::Divides(e1,e2) => {
            Expr::Divides(Box::new(subst_expr(var, arg.clone(), *e1)), Box::new(subst_expr(var, arg, *e2)))
        },
        Expr::If(c,e1,e2) => {
            Expr::If(Box::new(subst_expr(var, arg.clone(), *c)), Box::new(subst_expr(var, arg.clone(), *e1)), Box::new(subst_expr(var, arg, *e2)))
        },
        Expr::Fun(v, e) => {
            if v == *var {
                Expr::Fun(v, e)
            } else {
                Expr::Fun(v, Box::new(subst_expr(var, arg, *e)))
            }
        },
        Expr::App(e1, e2) => {
            Expr::App(Box::new(subst_expr(var, arg.clone(), *e1)), Box::new(subst_expr(var, arg.clone(), *e2)))
        },
        Expr::Quote(v, e) => {
            if v == *var {
                if arg == Expr::Epsilon {
                    subst_expr(&v, Expr::Epsilon, *e)
                } else {
                    Expr::Quote(v.clone(), Box::new(subst_expr(&v, arg, *e)))
                }
            } else {
                Expr::Quote(v, Box::new(subst_expr(var, arg, *e)))
            }
        },
        Expr::UnQuote(v, e) => {
            if v == *var {
                Expr::UnQuote(v, e)
            } else {
                Expr::UnQuote(v, Box::new(subst_expr(var, arg, *e)))
            }
        }
    }
}

fn test_with_input_string(vs: Vec<String>) {
    for s in vs {
        let mut lexer = Lexer::new(s);
        let r = parse_expr(&mut lexer);
        match r {
            Ok(e) => {
                let mut e2 = e;
                println!("{:?}", e2.show());
                while !is_value(&e2) {
                    e2 = eval_one_step(e2);
                    println!("{:?}", e2.show());
                }
            },
            Err(_) => ()
        }
        println!("========================");
    }
}

fn is_stage(expr: Expr) -> bool {
    match expr {
        Expr::Epsilon => true,
        _ => false
    }
}

fn main() {
    let inputs = vec![
        String::from("1123 + 345"),
        String::from("1123 + 345 * 43 - 10"),
        String::from("(fun x -> x + 10) ((fun a -> (quote a (20 + 1))) epsilon)"),
        // String::from("x + 1123 * 345 * (99 + (fun x -> x) y)"),
        // String::from("true"),
        // String::from("epsilon"),
        // String::from("if true then 1 + 3 else x"),
        // String::from("fun x -> x + 2"),
        // String::from("(quote a x) + 1"),
        // String::from("(unquote a (x + y + z)) + 1"),
        String::from("(fun a -> (quote a 1 + 2)) epsilon true")];
    test_with_input_string(inputs)
}
