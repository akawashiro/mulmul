use crate::expr::Expr;
use crate::expr::Op;
use crate::expr::Stage;
use crate::expr::Type;
use crate::expr::Variable;
use std::collections::HashMap;

pub fn get_type(expr: &Expr) -> Result<Type, String> {
    match expr {
        Expr::Number(_) => Ok(Type::Int),
        Expr::Boolean(_) => Ok(Type::Bool),
        _ => Ok(Type::TVar(Variable("undefined".to_string()))),
    }
}

pub fn gen_tvar(nt: &mut u32) -> Type {
    *nt += 1;
    Type::TVar(Variable(format!("a{}", nt)))
}

pub fn get_subst_from_expr(expr: &Expr) -> Vec<(Type, Type)> {
    let t = Type::TVar(Variable("a0".to_string()));
    let s = Stage("".to_string());
    let mut nt = 1;
    let mut res = Vec::new();
    let mut env = HashMap::new();
    get_subst(expr, t, s, &mut nt, &mut res, &mut env);
    res
}

pub fn get_subst(
    expr: &Expr,
    t: Type,
    s: Stage,
    nt: &mut u32,
    res: &mut Vec<(Type, Type)>,
    env: &mut HashMap<Variable, (Type, Stage)>,
) {
    match expr {
        Expr::Number(_) => res.push((t, Type::Int)),
        Expr::Boolean(_) => res.push((t, Type::Bool)),
        Expr::Nil => res.push((t, Type::List(Box::new(gen_tvar(nt))))),
        Expr::Variable(v) => match env.get(&v) {
            Some((t2, s2)) => res.push((t, t2.clone())),
            None => {
                let t2 = gen_tvar(nt);
                env.insert(v.clone(), (t2.clone(), s));
                res.push((t, Type::List(Box::new(t2))))
            }
        },
        Expr::BinOp(o, e1, e2) => {
            use Op::*;
            match o {
                Plus | Minus | Times | Divides => {
                    get_subst(e1, Type::Int, s.clone(), nt, res, env);
                    get_subst(e2, Type::Int, s, nt, res, env);
                    res.push((t, Type::Int))
                }
                Lt | Lte | Gt | Gte | Equal => {
                    get_subst(e1, Type::Int, s.clone(), nt, res, env);
                    get_subst(e2, Type::Int, s, nt, res, env);
                    res.push((t, Type::Bool))
                }
                And | Or => {
                    get_subst(e1, Type::Bool, s.clone(), nt, res, env);
                    get_subst(e2, Type::Bool, s, nt, res, env);
                    res.push((t, Type::Bool))
                }
                Cons => {
                    let t2 = Type::List(Box::new(gen_tvar(nt)));
                    get_subst(e1, t2.clone(), s.clone(), nt, res, env);
                    get_subst(e2, t2, s, nt, res, env);
                }
            }
        }
        Expr::If(e1, e2, e3) => {
            get_subst(e1, Type::Bool, s.clone(), nt, res, env);
            get_subst(e2, t.clone(), s.clone(), nt, res, env);
            get_subst(e3, t, s, nt, res, env);
        }
        Expr::Quote(sv, e) => {
            let t2 = gen_tvar(nt);
            res.push((t, Type::Code(sv.clone(), Box::new(t2.clone()))));
            get_subst(e, t2, s, nt, res, env)
        }
        Expr::UnQuote(sv, e) => {
            let t2 = gen_tvar(nt);
            res.push((t2.clone(), Type::Code(sv.clone(), Box::new(t.clone()))));
            get_subst(e, Type::Code(sv.clone(), Box::new(t)), s, nt, res, env)
        }
        Expr::Tuple(es) => {
            let mut ts = Vec::new();
            for e in es {
                let t2 = gen_tvar(nt);
                ts.push(Box::new(t2.clone()));
                get_subst(e, t2, s.clone(), nt, res, env);
            }
            res.push((t, Type::Tuple(ts)))
        }
        _ => (),
    }
}
