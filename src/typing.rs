use crate::expr::Expr;
use crate::expr::Op;
use crate::expr::Stage;
use crate::expr::StageElement;
use crate::expr::StageElement::StageVariable;
use crate::expr::Type;
use crate::expr::Variable;
use std::collections::HashMap;

pub fn get_type(expr: &Expr) -> Result<Type, String> {
    let c = get_constraints_from_expr(expr);
    println!("constraints = ");
    show_constraints(&c);
    let (ts, ss) = solve_constraints(c);
    println!("solutions = ");
    show_solutions(ts, ss);
    match expr {
        Expr::Number(_) => Ok(Type::Int),
        Expr::Boolean(_) => Ok(Type::Bool),
        _ => Ok(Type::TVar(Variable("undefined".to_string()))),
    }
}

type Constraints = Vec<((Type, Stage), (Type, Stage))>;
type TypeSolution = HashMap<Type, Type>;
type StageSolution = HashMap<StageElement, Stage>;

fn solve_constraints(constraints: Constraints) -> (TypeSolution, StageSolution) {
    let mut tcstr = Vec::new();
    let mut scstr = Vec::new();
    let mut tsub = HashMap::new();
    let mut ssub = HashMap::new();
    for ((t1, s1), (t2, s2)) in constraints {
        tcstr.push((t1, t2));
        scstr.push((s1, s2));
    }
    while tcstr.len() > 0 {}
    (tsub, ssub)
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
            println!("{}", s);
            let t2 = gen_tvar(nt);
            let mut v = s.0.clone();
            v.append(&mut sv.0.clone());
            println!("{:?}", sv);
            println!("{:?}", v);
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
        _ => (),
    }
}
