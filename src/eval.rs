use crate::expr::Expr;
use crate::expr::Variable;

pub fn is_value(expr: &Expr) -> bool {
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

pub fn eval_one_step(expr: Expr) -> Expr {
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
            Expr::If(c, e1, e2) => {
                if !is_value(&*c) {
                    Expr::If(Box::new(eval_one_step(*c)), e1, e2)
                } else {
                    match *c {
                        Expr::Boolean(true) => *e1,
                        Expr::Boolean(false) => *e2,
                        _ => unreachable!()
                    }
                }
            },
            Expr::Let(v, e1, e2) => {
                if !is_value(&*e1) {
                    Expr::Let(v, Box::new(eval_one_step(*e1)), e2)
                } else {
                    subst_expr(&v, *e1, *e2)
                }
            },
            Expr::UnQuote(v, e) => {
                if !is_value(&*e) {
                    Expr::UnQuote(v, Box::new(eval_one_step(*e)))
                } else {
                    match *e {
                        Expr::Quote(v2, e2) => {
                            if v == v2 {
                                *e2
                            } else {
                                unreachable!()
                            }
                        },
                        _ => unreachable!()
                    }
                }
            },
            Expr::Epsilon | Expr::Boolean(_) | Expr::Number(_) | Expr::Variable(_) | Expr::Fun(_,_) | Expr::Quote(_,_) => unreachable!(),
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
        Expr::Let(v, e1, e2) => {
            if v ==  *var {
                Expr::Let(v, e1, e2)
            } else {
                Expr::Let(v, Box::new(subst_expr(var, arg.clone(), *e1)), Box::new(subst_expr(var, arg, *e2)))
            }
        }
    }
}

// fn is_stage(expr: Expr) -> bool {
//     match expr {
//         Expr::Epsilon => true,
//         _ => false
//     }
// }

