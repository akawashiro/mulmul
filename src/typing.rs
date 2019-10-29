use crate::expr::Pattern;
use crate::expr::Expr;
use crate::expr::Op;
use crate::expr::Stage;
use crate::expr::StageElement;
use crate::expr::StageElement::StageVariable;
use crate::expr::Type;
use crate::expr::Variable;
use std::collections::HashMap;
use std::collections::VecDeque;

pub fn get_type(expr: &Expr) -> Result<Type, String> {
    let c = get_constraints_from_expr(expr);
    println!("constraints = ");
    show_constraints(&c);
    match solve_constraints(c) {
        Ok((ts, ss)) => {
            println!("solutions = ");
            show_solutions(ts, ss);
        }
        Err(s) => {
            println!("{}", s);
        }
    }
    match expr {
        Expr::Number(_) => Ok(Type::Int),
        Expr::Boolean(_) => Ok(Type::Bool),
        _ => Ok(Type::TVar(Variable("undefined".to_string()))),
    }
}

type Constraints = Vec<((Type, Stage), (Type, Stage))>;
type TypeSolution = HashMap<Type, Type>;
type StageSolution = HashMap<StageElement, Stage>;

fn solve_constraints(constraints: Constraints) -> Result<(TypeSolution, StageSolution), String> {
    let mut tcstr = VecDeque::new();
    let mut scstr = VecDeque::new();
    let mut tsub = TypeSolution::new();
    let mut ssub = StageSolution::new();
    for ((t1, s1), (t2, s2)) in constraints {
        tcstr.push_back((t1, t2));
        scstr.push_back((s1, s2));
    }
    loop {
        match tcstr.pop_back() {
            Some((t1, t2)) => {
                use Type::*;
                if t1 == t2 {
                    continue;
                }
                match (t1, t2) {
                    (TVar(v), t22) => {
                        let _ = tsub.insert(Type::TVar(v), t22);
                        ()
                    }
                    (Fun(t11, t12), Fun(t21, t22)) => {
                        tcstr.push_back((*t11, *t21));
                        tcstr.push_back((*t12, *t22))
                    }
                    (List(t11), List(t21)) => tcstr.push_back((*t11, *t21)),
                    (Code(s1, t11), Code(s2, t21)) => {
                        scstr.push_back((s1, s2));
                        tcstr.push_back((*t11, *t21))
                    }
                    (Tuple(ts1), Tuple(ts2)) => {
                        if ts1.len() == ts2.len() {
                            for i in 0..ts1.len() {
                                tcstr.push_back((*ts1[i].clone(), *ts2[i].clone()))
                            }
                        } else {
                            return Err("Lenths of two tuples are not same.".to_string());
                        }
                    }
                    _ => (),
                };
            }
            None => break,
        }
    }
    Ok((tsub, ssub))
}

fn show_solutions(tsol: HashMap<Type, Type>, ssol: HashMap<StageElement, Stage>) {
    for (t1, t2) in tsol {
        println!("{} -> {}", t1, t2);
    }
    for (s1, s2) in ssol {
        println!("{} -> {}", s1, s2);
    }
}

fn gen_tvar(nt: &mut u32) -> Type {
    *nt += 1;
    Type::TVar(Variable(format!("a{}", nt)))
}

fn gen_svar(ns: &mut u32) -> Stage {
    *ns += 1;
    Stage(vec![StageVariable(format!("s{}", ns))])
}

fn get_constraints_from_expr(expr: &Expr) -> Vec<((Type, Stage), (Type, Stage))> {
    let t = Type::TVar(Variable("a0".to_string()));
    let s = Stage(Vec::new());
    let mut nt = 1;
    let mut ns = 1;
    let mut res = Vec::new();
    let mut env = HashMap::new();
    get_constraints(expr, t, s, &mut nt, &mut ns, &mut res, &mut env);
    res
}

fn show_constraints(subst: &Vec<((Type, Stage), (Type, Stage))>) {
    for s in subst {
        let ((t1, s1), (t2, s2)) = s;
        println!("{}, {} = {}, {}", t1, s1, t2, s2);
    }
}

fn get_type_of_pattern(pat: &Pattern, nt: &mut u32) -> (Type, Vec<(Variable, Type)>) {
    use Pattern::*;
    match pat {
        Variable(v) => {
            let t = gen_tvar(nt);
            (t.clone(), vec!((v.clone(),t)))
        }
        Tuple(ps) => {
            let mut ts = Vec::new();
            let mut env = Vec::new();
            for p in ps {
               let (t,mut e) = get_type_of_pattern(p, nt);
               ts.push(Box::new(t));
               env.append(&mut e);
            }
            (Type::Tuple(ts), env)
        }
        _ => unreachable!()
    }
}

fn get_constraints(
    expr: &Expr,
    t: Type,
    s: Stage,
    nt: &mut u32,
    ns: &mut u32,
    res: &mut Vec<((Type, Stage), (Type, Stage))>,
    env: &mut HashMap<Variable, (Type, Stage)>,
) {
    match expr {
        Expr::Number(_) => res.push(((t, s.clone()), (Type::Int, s))),
        Expr::Boolean(_) => res.push(((t, s.clone()), (Type::Bool, s))),
        Expr::Nil => res.push(((t, s.clone()), (Type::List(Box::new(gen_tvar(nt))), s))),
        Expr::Variable(v) => match env.get(&v) {
            Some((t2, s2)) => res.push(((t, s), (t2.clone(), s2.clone()))),
            None => {
                let t2 = gen_tvar(nt);
                env.insert(v.clone(), (t2.clone(), s.clone()));
                res.push(((t, s.clone()), (Type::List(Box::new(t2)), s)))
            }
        },
        Expr::BinOp(o, e1, e2) => {
            use Op::*;
            match o {
                Plus | Minus | Times | Divides => {
                    get_constraints(e1, Type::Int, s.clone(), nt, ns, res, env);
                    get_constraints(e2, Type::Int, s.clone(), nt, ns, res, env);
                    res.push(((t, s.clone()), (Type::Int, s)))
                }
                Lt | Lte | Gt | Gte | Equal => {
                    get_constraints(e1, Type::Int, s.clone(), nt, ns, res, env);
                    get_constraints(e2, Type::Int, s.clone(), nt, ns, res, env);
                    res.push(((t, s.clone()), (Type::Bool, s)))
                }
                And | Or => {
                    get_constraints(e1, Type::Bool, s.clone(), nt, ns, res, env);
                    get_constraints(e2, Type::Bool, s.clone(), nt, ns, res, env);
                    res.push(((t, s.clone()), (Type::Bool, s)))
                }
                Cons => {
                    let t2 = Type::List(Box::new(gen_tvar(nt)));
                    get_constraints(e1, t2.clone(), s.clone(), nt, ns, res, env);
                    get_constraints(e2, t2, s, nt, ns, res, env);
                }
            }
        }
        Expr::If(e1, e2, e3) => {
            get_constraints(e1, Type::Bool, s.clone(), nt, ns, res, env);
            get_constraints(e2, t.clone(), s.clone(), nt, ns, res, env);
            get_constraints(e3, t, s, nt, ns, res, env);
        }
        Expr::Quote(sv, e) => {
            let t2 = gen_tvar(nt);
            let mut v = s.0.clone();
            v.append(&mut sv.0.clone());
            res.push((
                (t, s.clone()),
                (Type::Code(sv.clone(), Box::new(t2.clone())), s),
            ));
            get_constraints(e, t2, Stage(v), nt, ns, res, env)
        }
        Expr::UnQuote(sv, e) => {
            let t2 = gen_tvar(nt);
            let s2 = gen_svar(ns);
            let mut s3 = s2.0.clone();
            s3.append(&mut sv.0.clone());
            res.push(((t.clone(), s), (t2, Stage(s3))));
            get_constraints(e, Type::Code(sv.clone(), Box::new(t)), s2, nt, ns, res, env)
        }
        Expr::Tuple(es) => {
            let mut ts = Vec::new();
            for e in es {
                let t2 = gen_tvar(nt);
                ts.push(Box::new(t2.clone()));
                get_constraints(e, t2, s.clone(), nt, ns, res, env);
            }
            res.push(((t, s.clone()), (Type::Tuple(ts), s)))
        }
        // In the original paper, sv must not appears anywhere else in the program. But
        // in this implementation, `sfun` makes scope. So the restriction is meeted
        // automatically.
        Expr::SFun(sv, e) => {
            let t2 = gen_tvar(nt);
            res.push((
                (t, s.clone()),
                (Type::SFun(sv.clone(), Box::new(t2.clone())), s.clone()),
            ));
            get_constraints(e, t2, s, nt, ns, res, env)
        }
        Expr::App(e1, e2) => {
            if **e2 == Expr::Stage(Stage(Vec::new())) {
                let s2 = gen_svar(ns);
                get_constraints(e1, Type::SFun(s2, Box::new(t)), s, nt, ns, res, env)
            } else {
                let t2 = gen_tvar(nt);
                get_constraints(
                    e1,
                    Type::Fun(Box::new(t2.clone()), Box::new(t)),
                    s.clone(),
                    nt,
                    ns,
                    res,
                    env,
                );
                get_constraints(e2, t2, s, nt, ns, res, env);
            }
        }
        _ => (),
    }
}
