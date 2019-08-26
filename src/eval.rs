use crate::expr::Expr;
use crate::expr::Op;
use crate::expr::Variable;
use std::collections::HashMap;

struct Env {
    binding: HashMap<Variable, Expr>
}

impl Env {
    fn find(&self, var: &Variable) -> Result<Expr, String> {
        match self.binding.get(var) {
            Some(e) => Ok(*e),
            None => Err(format!("Variable {} is not in the environment.", var))
        }
    }

    fn add(&self, var: Variable, expr: Expr) {
        self.binding.insert(var, expr);
    }
}

struct Evaluator {
    env: Env,
    expr: Expr
}

pub impl Evaluator {
   pub fn eval_one_step(&mut self) {
       if !is_value(&self.expr) {
        match self.expr {
            Expr::BinOp(o, e1, e2) => {
                if !is_value(&*e1) {
                    let e3 = eval_one_step(*e1);
                    self.expr = Expr::BinOp(o, Box::new(e3), e2)
                } else if !is_value(&*e2) {
                    let e3 = eval_one_step(*e2);
                    self.expr = Expr::BinOp(o, e1, Box::new(e3))
                } else {
                    match (o, *e1, *e2) {
                        (Op::Plus, Expr::Number(i1), Expr::Number(i2)) => self.expr = Expr::Number(i1+i2),
                        (Op::Minus, Expr::Number(i1), Expr::Number(i2)) => self.expr = Expr::Number(i1-i2),
                        (Op::Times, Expr::Number(i1), Expr::Number(i2)) => self.expr = Expr::Number(i1*i2),
                        (Op::Divides, Expr::Number(i1), Expr::Number(i2)) => self.expr = Expr::Number(i1/i2),
                        (Op::Lt, Expr::Number(i1), Expr::Number(i2)) => self.expr = Expr::Boolean(i1<i2),
                        (Op::Lte, Expr::Number(i1), Expr::Number(i2)) => self.expr = Expr::Boolean(i1<=i2),
                        (Op::Gt, Expr::Number(i1), Expr::Number(i2)) => self.expr = Expr::Boolean(i1>i2),
                        (Op::Gte, Expr::Number(i1), Expr::Number(i2)) => self.expr = Expr::Boolean(i1>=i2),
                        (Op::Equal, Expr::Number(i1), Expr::Number(i2)) => self.expr = Expr::Boolean(i1==i2),
                        (Op::And, Expr::Boolean(b1), Expr::Boolean(b2)) => self.expr = Expr::Boolean(b1&&b2),
                        (Op::Or, Expr::Boolean(b1), Expr::Boolean(b2)) => self.expr = Expr::Boolean(b1||b2),
                        (Op::Cons, e1, e2) => {
                            if !is_value(&e1) {
                                self.expr = Expr::BinOp(Op::Cons, Box::new(eval_one_step(e1)), Box::new(e2))
                            } else if !is_value(&e2) {
                                self.expr = Expr::BinOp(Op::Cons, Box::new(e1), Box::new(eval_one_step(e2)))
                            } else {
                                unreachable!()
                            }
                        },
                        _ => unreachable!()
                    }
                }
            },
            Expr::App(e1, e2) => {
                if !is_value(&*e2) {
                    self.expr = Expr::App(e1, Box::new(eval_one_step(*e2)))
                } else if !is_value(&*e1) {
                    self.expr = Expr::App(Box::new(eval_one_step(*e1)), e2)
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
                    self.expr = Expr::If(Box::new(eval_one_step(*c)), e1, e2)
                } else {
                    match *c {
                        Expr::Boolean(true) => self.expr = *e1,
                        Expr::Boolean(false) => self.expr = *e2,
                        _ => unreachable!()
                    }
                }
            },
            Expr::Let(v, e1, e2) => {
                if !is_value(&*e1) {
                    self.expr = Expr::Let(v, Box::new(eval_one_step(*e1)), e2)
                } else {
                    subst_expr(&v, *e1, *e2)
                }
            },
            Expr::UnQuote(v, e) => {
                if !is_value(&*e) {
                    self.expr = Expr::UnQuote(v, Box::new(eval_one_step(*e)))
                } else {
                    match *e {
                        Expr::Quote(v2, e2) => {
                            if v == v2 {
                                self.expr = *e2
                            } else {
                                unreachable!()
                            }
                        },
                        _ => unreachable!()
                    }
                }
            },
            Expr::Tuple(es) => {
                let mut f = false;
                let mut es2 = Vec::new();
                for e in es {
                    if !f && !is_value(&e) {
                        f = true;
                        es2.push(Box::new(eval_one_step(*e)))
                    } else {
                        es2.push(Box::new(*e))
                    }
                }
                self.expr = Expr::Tuple(es2)
            },
            Expr::Epsilon | Expr::Boolean(_) | Expr::Number(_) | Expr::Variable(_) | Expr::Fun(_,_) | Expr::Quote(_,_) | Expr::Nil => unreachable!(),
        }
       }
   }
}

pub fn is_value(expr: &Expr) -> bool {
    match expr {
        Expr::Epsilon => true,
        Expr::Nil => true,
        Expr::Boolean(_) => true,
        Expr::Number(_) => true,
        Expr::Variable(_) => true,
        Expr::Fun(_,_) => true,
        Expr::Quote(_,_) => true,
        Expr::Tuple(es) => {
            for e in es {
                if !is_value(e) {
                    return false
                }
            }
            true
        }
        Expr::BinOp(Op::Cons, e1, e2) => is_value(e1) && is_value(e2),
        _ => false
    }
}

pub fn eval_one_step(env: Env, expr: Expr) -> (Env, Expr) {
    if is_value(&expr) {
        (env, expr)
    } else {
        match expr {
            Expr::BinOp(o, e1, e2) => {
                if !is_value(&*e1) {
                    let (env2, e3) = eval_one_step(env, *e1);
                    (env2, Expr::BinOp(o, Box::new(e3), e2))
                } else if !is_value(&*e2) {
                    let (env2, e3) = eval_one_step(env, *e2);
                    (env2, Expr::BinOp(o, e1, Box::new(e3)))
                } else {
                    match (o, *e1, *e2) {
                        (Op::Plus, Expr::Number(i1), Expr::Number(i2)) => (env, Expr::Number(i1+i2)), 
                        (Op::Minus, Expr::Number(i1), Expr::Number(i2)) => (env, Expr::Number(i1-i2)),
                        (Op::Times, Expr::Number(i1), Expr::Number(i2)) => (env, Expr::Number(i1*i2)),
                        (Op::Divides, Expr::Number(i1), Expr::Number(i2)) => (env, Expr::Number(i1/i2)),
                        (Op::Lt, Expr::Number(i1), Expr::Number(i2)) => (env, Expr::Boolean(i1<i2)),
                        (Op::Lte, Expr::Number(i1), Expr::Number(i2)) => (env, Expr::Boolean(i1<=i2)),
                        (Op::Gt, Expr::Number(i1), Expr::Number(i2)) => (env, Expr::Boolean(i1>i2)),
                        (Op::Gte, Expr::Number(i1), Expr::Number(i2)) => (env, Expr::Boolean(i1>=i2)),
                        (Op::Equal, Expr::Number(i1), Expr::Number(i2)) => (env, Expr::Boolean(i1==i2)),
                        (Op::And, Expr::Boolean(b1), Expr::Boolean(b2)) => (env, Expr::Boolean(b1&&b2)),
                        (Op::Or, Expr::Boolean(b1), Expr::Boolean(b2)) => (env, Expr::Boolean(b1||b2)),
                        (Op::Cons, e1, e2) => {
                            if !is_value(&e1) {
                                let (env2, e3) = eval_one_step(env, e1);
                                (env2, Expr::BinOp(Op::Cons, Box::new(e3), Box::new(e2)))
                            } else if !is_value(&e2) {
                                let (env2, e3) = eval_one_step(env, e2);
                                (env2, Expr::BinOp(Op::Cons, Box::new(e1), Box::new(e3)))
                            } else {
                                unreachable!()
                            }
                        },
                        _ => unreachable!()
                    }
                }
            },
            Expr::App(e1, e2) => {
                if !is_value(&*e2) {
                    let (env2, e3) = eval_one_step(env, *e2);
                    (env2, Expr::App(e1, Box::new(e3)))
                } else if !is_value(&*e1) {
                    let (env2, e3) = eval_one_step(env, *e1);
                    (env2, Expr::App(Box::new(e3), e2))
                } else {
                    match *e1 {
                        Expr::Fun(pat, e3) => {
                            let bs = make_binding(pat, e3);
                            for (v,e) in bs {
                                env.insert(v,e)
                            }
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
            Expr::Tuple(es) => {
                let mut f = false;
                let mut es2 = Vec::new();
                for e in es {
                    if !f && !is_value(&e) {
                        f = true;
                        es2.push(Box::new(eval_one_step(*e)))
                    } else {
                        es2.push(Box::new(*e))
                    }
                }
                Expr::Tuple(es2)
            },
            Expr::Epsilon | Expr::Boolean(_) | Expr::Number(_) | Expr::Variable(_) | Expr::Fun(_,_) | Expr::Quote(_,_) | Expr::Nil => unreachable!(),
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
        Expr::Epsilon | Expr::Boolean(_) | Expr::Number(_) | Expr::Nil => expr,
        Expr::BinOp(o, e1, e2) => {
            Expr::BinOp(o, Box::new(subst_expr(var, arg.clone(), *e1)), Box::new(subst_expr(var, arg, *e2)))
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
        },
        Expr::Let(v, e1, e2) => {
            if v ==  *var {
                Expr::Let(v, e1, e2)
            } else {
                Expr::Let(v, Box::new(subst_expr(var, arg.clone(), *e1)), Box::new(subst_expr(var, arg, *e2)))
            }
        },
        Expr::Tuple(es) => {
            Expr::Tuple(es.iter().map(|e| Box::new(subst_expr(var, arg.clone(), (**e).clone()))).collect())
        }
    }
}

// fn is_stage(expr: Expr) -> bool {
//     match expr {
//         Expr::Epsilon => true,
//         _ => false
//     }
// }

