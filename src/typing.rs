use crate::expr::Expr;
use crate::expr::Op;
use crate::expr::Type;
use crate::expr::Variable;
use std::collections::HashMap;

pub fn get_type(expr: &Expr) -> Result<Type, String> {
    match expr {
        Expr::Number(_) => Ok(Type::Int),
        Expr::Boolean(_) => Ok(Type::Bool),
        Expr::Epsilon => Ok(Type::Stage),
        _ => Ok(Type::TVar(Variable {
            name: Box::new("undefined".to_string()),
        })),
    }
}

pub fn gen_tvar(nt: &mut u32) -> Type {
    *nt += 1;
    Type::TVar(Variable {
        name: Box::new(format!("a{}", nt)),
    })
}

pub fn get_subst(
    expr: Expr,
    t: Type,
    nt: &mut u32,
    res: &mut Vec<(Type, Type)>,
    env: &mut HashMap<Variable, Type>,
) {
    match expr {
        Expr::Number(_) => res.push((t, Type::Int)),
        Expr::Boolean(_) => res.push((t, Type::Int)),
        Expr::Epsilon => res.push((t, Type::Stage)),
        Expr::Nil => res.push((t, Type::List(Box::new(gen_tvar(nt))))),
        Expr::Variable(v) => match env.get(&v) {
            Some(s) => res.push((t, s.clone())),
            None => {
                let s = gen_tvar(nt);
                env.insert(v.clone(), s.clone());
                res.push((t, Type::List(Box::new(s))))
            }
        },
        Expr::BinOp(o, e1, e2) => {
            use Op::*;
            match o {
                Plus | Minus | Times | Divides => {
                    get_subst(*e1, Type::Int, nt, res, env);
                    get_subst(*e2, Type::Int, nt, res, env);
                    res.push((t, Type::Int))
                }
                Lt | Lte | Gt | Gte | Equal => {
                    get_subst(*e1, Type::Int, nt, res, env);
                    get_subst(*e2, Type::Int, nt, res, env);
                    res.push((t, Type::Bool))
                }
                And | Or => {
                    get_subst(*e1, Type::Bool, nt, res, env);
                    get_subst(*e2, Type::Bool, nt, res, env);
                    res.push((t, Type::Bool))
                }
                Cons => {
                    let s = Type::List(Box::new(gen_tvar(nt)));
                    get_subst(*e1, s.clone(), nt, res, env);
                    get_subst(*e2, s, nt, res, env);
                }
            }
        } // _ => ()
    }
}
