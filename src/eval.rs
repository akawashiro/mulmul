use crate::expr::Expr;
use crate::expr::Op;
use crate::expr::Pattern;
use crate::expr::Variable;
use std::collections::HashMap;

pub fn is_value(expr: &Expr) -> bool {
    match expr {
        Expr::Epsilon => true,
        Expr::Nil => true,
        Expr::Boolean(_) => true,
        Expr::Number(_) => true,
        Expr::Fun(_, _) => true,
        Expr::SFun(_, _) => true,
        Expr::Quote(_, _) => true,
        Expr::Tuple(es) => {
            for e in es {
                if !is_value(e) {
                    return false;
                }
            }
            true
        }
        Expr::BinOp(Op::Cons, e1, e2) => is_value(e1) && is_value(e2),
        _ => false,
    }
}

pub fn eval_one_step(expr: Expr) -> Expr {
    if is_value(&expr) {
        expr
    } else {
        match expr {
            Expr::BinOp(o, e1, e2) => {
                if !is_value(&*e1) {
                    Expr::BinOp(o, Box::new(eval_one_step(*e1)), e2)
                } else if !is_value(&*e2) {
                    Expr::BinOp(o, e1, Box::new(eval_one_step(*e2)))
                } else {
                    match (o, *e1, *e2) {
                        (Op::Plus, Expr::Number(i1), Expr::Number(i2)) => Expr::Number(i1 + i2),
                        (Op::Minus, Expr::Number(i1), Expr::Number(i2)) => Expr::Number(i1 - i2),
                        (Op::Times, Expr::Number(i1), Expr::Number(i2)) => Expr::Number(i1 * i2),
                        (Op::Divides, Expr::Number(i1), Expr::Number(i2)) => Expr::Number(i1 / i2),
                        (Op::Lt, Expr::Number(i1), Expr::Number(i2)) => Expr::Boolean(i1 < i2),
                        (Op::Lte, Expr::Number(i1), Expr::Number(i2)) => Expr::Boolean(i1 <= i2),
                        (Op::Gt, Expr::Number(i1), Expr::Number(i2)) => Expr::Boolean(i1 > i2),
                        (Op::Gte, Expr::Number(i1), Expr::Number(i2)) => Expr::Boolean(i1 >= i2),
                        (Op::Equal, Expr::Number(i1), Expr::Number(i2)) => Expr::Boolean(i1 == i2),
                        (Op::And, Expr::Boolean(b1), Expr::Boolean(b2)) => Expr::Boolean(b1 && b2),
                        (Op::Or, Expr::Boolean(b1), Expr::Boolean(b2)) => Expr::Boolean(b1 || b2),
                        (Op::Cons, e1, e2) => {
                            if !is_value(&e1) {
                                Expr::BinOp(Op::Cons, Box::new(eval_one_step(e1)), Box::new(e2))
                            } else if !is_value(&e2) {
                                Expr::BinOp(Op::Cons, Box::new(e1), Box::new(eval_one_step(e2)))
                            } else {
                                unreachable!()
                            }
                        }
                        _ => unreachable!(),
                    }
                }
            }
            Expr::App(e1, e2) => {
                if !is_value(&*e2) {
                    Expr::App(e1, Box::new(eval_one_step(*e2)))
                } else if !is_value(&*e1) {
                    Expr::App(Box::new(eval_one_step(*e1)), e2)
                } else {
                    match *e1 {
                        Expr::Fun(pat, e3) => match make_binding(&pat, &e2) {
                            Ok(bs) => subst_expr(bs, *e3),
                            Err(_) => unreachable!(),
                        },
                        Expr::SFun(v, e3) => {
                            let mut b = HashMap::new();
                            b.insert(v, *e2);
                            subst_expr(b, *e3)
                        },
                        _ => unreachable!(),
                    }
                }
            }
            Expr::If(c, e1, e2) => {
                if !is_value(&*c) {
                    Expr::If(Box::new(eval_one_step(*c)), e1, e2)
                } else {
                    match *c {
                        Expr::Boolean(true) => *e1,
                        Expr::Boolean(false) => *e2,
                        _ => unreachable!(),
                    }
                }
            }
            Expr::Let(pat, e1, e2) => {
                if !is_value(&*e1) {
                    Expr::Let(pat, Box::new(eval_one_step(*e1)), e2)
                } else {
                    match make_binding(&pat, &e1) {
                        Ok(bs) => subst_expr(bs, *e2),
                        Err(_) => unreachable!(),
                    }
                }
            }
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
                        }
                        _ => unreachable!(),
                    }
                }
            }
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
            }
            Expr::Match(expr, ms) => {
                if !is_value(&*expr) {
                    Expr::Match(Box::new(eval_one_step(*expr)), ms)
                } else {
                    for m in ms {
                        let (p, e) = *m;
                        match make_binding(&p, &expr) {
                            Ok(b) => return subst_expr(b, e),
                            Err(_) => (),
                        }
                    }
                    unreachable!()
                }
            }
            Expr::Epsilon
            | Expr::Boolean(_)
            | Expr::Number(_)
            | Expr::Variable(_)
            | Expr::Fun(_, _)
            | Expr::SFun(_, _)
            | Expr::Quote(_, _)
            | Expr::Nil => unreachable!(),
        }
    }
}

fn subst_expr(bind: HashMap<Variable, Expr>, expr: Expr) -> Expr {
    match expr {
        Expr::Variable(v) => match bind.get(&v) {
            Some(e) => e.clone(),
            None => Expr::Variable(v),
        },
        Expr::Epsilon | Expr::Boolean(_) | Expr::Number(_) | Expr::Nil => expr,
        Expr::BinOp(o, e1, e2) => Expr::BinOp(
            o,
            Box::new(subst_expr(bind.clone(), *e1)),
            Box::new(subst_expr(bind, *e2)),
        ),
        Expr::If(c, e1, e2) => Expr::If(
            Box::new(subst_expr(bind.clone(), *c)),
            Box::new(subst_expr(bind.clone(), *e1)),
            Box::new(subst_expr(bind, *e2)),
        ),
        Expr::Fun(pat, e) => {
            let mut b = bind.clone();
            for v in get_variables(&pat) {
                b.remove(&v);
            }
            Expr::Fun(pat, Box::new(subst_expr(b, *e)))
        }
        Expr::SFun(v, e) => {
            let mut b = bind.clone();
            b.remove(&v);
            Expr::SFun(v, Box::new(subst_expr(b, *e)))
        }
        Expr::App(e1, e2) => Expr::App(
            Box::new(subst_expr(bind.clone(), *e1)),
            Box::new(subst_expr(bind, *e2)),
        ),
        Expr::Quote(v, e) => match bind.get(&v) {
            Some(Expr::Epsilon) => subst_expr(bind, *e),
            Some(Expr::Variable(v2)) => Expr::Quote(v2.clone(), Box::new(subst_expr(bind, *e))),
            _ => {
                let mut b = bind.clone();
                b.remove(&v);
                Expr::Quote(v.clone(), Box::new(subst_expr(bind, *e)))
            }
        },
        Expr::UnQuote(v, e) => match bind.get(&v) {
            Some(Expr::Epsilon) => subst_expr(bind, *e),
            Some(Expr::Variable(v2)) => Expr::UnQuote(v2.clone(), Box::new(subst_expr(bind, *e))),
            _ => {
                let mut b = bind.clone();
                b.remove(&v);
                Expr::UnQuote(v.clone(), Box::new(subst_expr(bind, *e)))
            }
        },
        Expr::Let(pat, e1, e2) => {
            let mut b = bind.clone();
            for v in get_variables(&pat) {
                b.remove(&v);
            }
            Expr::Let(
                pat,
                Box::new(subst_expr(b, *e1)),
                Box::new(subst_expr(bind, *e2)),
            )
        }
        Expr::Tuple(es) => Expr::Tuple(
            es.iter()
                .map(|e| Box::new(subst_expr(bind.clone(), (**e).clone())))
                .collect(),
        ),
        Expr::Match(expr, ms) => {
            let expr2 = subst_expr(bind.clone(), *expr);
            let mut ms2 = Vec::new();
            for m in ms {
                let (p, e) = *m;
                let vs = get_variables(&p);
                let mut b = bind.clone();
                for v in vs {
                    b.remove(&v);
                }
                ms2.push(Box::new((p, subst_expr(b, e))));
            }
            Expr::Match(Box::new(expr2), ms2)
        }
    }
}

fn get_variables(pat: &Pattern) -> Vec<Variable> {
    match pat {
        Pattern::Variable(v) => vec![v.clone()],
        Pattern::Tuple(ps) => {
            let mut r = Vec::new();
            for p in ps.iter() {
                let mut v = get_variables(p);
                r.append(&mut v)
            }
            r
        }
        Pattern::Cons(p1, p2) => {
            let mut v1 = get_variables(p1);
            let mut v2 = get_variables(p2);
            v1.append(&mut v2);
            v1
        }
        Pattern::Nil => Vec::new(),
    }
}

fn make_binding(pat: &Pattern, exp: &Expr) -> Result<HashMap<Variable, Expr>, String> {
    match (pat, exp) {
        (Pattern::Cons(p1, p2), Expr::BinOp(Op::Cons, e1, e2)) => {
            let mut h1 = make_binding(p1, e1)?;
            let h2 = make_binding(p2, e2)?;
            h1.extend(h2);
            Ok(h1)
        }
        (Pattern::Tuple(ps), Expr::Tuple(es)) if ps.len() == es.len() => {
            let mut h = HashMap::new();
            for (p, e) in ps.iter().zip(es) {
                let h2 = make_binding(&p, &e)?;
                h.extend(h2);
            }
            Ok(h)
        }
        (Pattern::Nil, Expr::Nil) => Ok(HashMap::new()),
        (Pattern::Variable(v), e) => {
            let mut h = HashMap::new();
            h.insert(v.clone(), e.clone());
            Ok(h)
        }
        _ => Err(format!(
            "Pattern {} and expression {} cannot be matched.",
            pat, exp
        )),
    }
}
