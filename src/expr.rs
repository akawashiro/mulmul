use std::fmt;
#[derive(Debug, Clone, Eq, Hash)]
pub struct Variable(pub String);

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl PartialEq for Variable {
    fn eq(&self, other: &Self) -> bool {
        *self.0 == *other.0
    }
}

#[derive(Debug, Clone, Eq, Hash)]
pub enum StageElement {
    StageConst(String),
    StageVariable(String),
}

impl PartialEq for StageElement {
    fn eq(&self, other: &Self) -> bool {
        use StageElement::*;
        match (self, other) {
            (StageConst(s1), StageConst(s2)) if s1 == s2 => true,
            (StageVariable(s1), StageVariable(s2)) if s1 == s2 => true,
            _ => false,
        }
    }
}

impl fmt::Display for StageElement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use StageElement::*;
        match self {
            StageConst(s) => write!(f, "{}", s),
            StageVariable(s) => write!(f, "'{}", s),
        }
    }
}

#[derive(Debug, Clone, Eq, Hash)]
pub struct Stage(pub Vec<StageElement>);

impl fmt::Display for Stage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0.len() == 0 {
            write!(f, "epsilon")
        } else {
            let mut o = String::from("");
            for s in self.0.clone() {
                if o == String::from("") {
                    o = format!("{}", s);
                } else {
                    o = format!("{}, {}", o, s);
                }
            }
            write!(f, "{}", o)
        }
    }
}

impl PartialEq for Stage {
    fn eq(&self, other: &Self) -> bool {
        *self.0 == *other.0
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Op {
    Plus,
    Minus,
    Times,
    Divides,
    Lt,
    Lte,
    Gt,
    Gte,
    Equal,
    And,
    Or,
    Cons,
}

impl PartialEq for Op {
    fn eq(&self, other: &Self) -> bool {
        use Op::*;
        match (self, other) {
            (Plus, Plus) => true,
            (Minus, Minus) => true,
            (Times, Times) => true,
            (Divides, Divides) => true,
            (Lt, Lt) => true,
            (Lte, Lte) => true,
            (Gt, Gt) => true,
            (Gte, Gte) => true,
            (Equal, Equal) => true,
            (And, And) => true,
            (Or, Or) => true,
            _ => false,
        }
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Op::*;
        match self {
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Times => write!(f, "*"),
            Divides => write!(f, "/"),
            Lt => write!(f, "<"),
            Lte => write!(f, "<="),
            Gt => write!(f, ">"),
            Gte => write!(f, ">="),
            Equal => write!(f, "=="),
            And => write!(f, "&&"),
            Or => write!(f, "||"),
            Cons => write!(f, "::"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Variable(Variable),
    Tuple(Vec<Box<Pattern>>),
    Cons(Box<Pattern>, Box<Pattern>),
    Nil,
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Pattern::*;
        match self {
            Variable(v) => write!(f, "{}", v),
            Tuple(ps) => {
                let mut r = String::from("(");
                for (i, p) in ps.iter().enumerate() {
                    if i == 0 {
                        r = format!("{}{}", r, p)
                    } else {
                        r = format!("{},{}", r, p)
                    }
                }
                r = format!("{})", r);
                write!(f, "{}", r)
            }
            Cons(p1, p2) => write!(f, "{} :: {}", p1, p2),
            Nil => write!(f, "[]"),
        }
    }
}

impl PartialEq for Pattern {
    fn eq(&self, other: &Self) -> bool {
        use Pattern::*;
        match (self, other) {
            (Variable(v1), Variable(v2)) if v1 == v2 => true,
            (Tuple(ps1), Tuple(ps2)) if ps1 == ps2 => true,
            (Cons(p1, p2), Cons(p3, p4)) if p1 == p3 && p2 == p4 => true,
            (Nil, Nil) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Eq, Hash)]
pub enum Type {
    Bool,
    Int,
    Unit,
    TVar(Variable),
    Tuple(Vec<Box<Type>>),
    List(Box<Type>),
    Fun(Box<Type>, Box<Type>),
    Code(Stage, Box<Type>),
    SFun(Stage, Box<Type>),
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        use Type::*;
        match (self, other) {
            (Bool, Bool) => true,
            (Int, Int) => true,
            (Unit, Unit) => true,
            (TVar(v1), TVar(v2)) if v1 == v2 => true,
            (Tuple(ts1), Tuple(ts2)) => {
                if ts1.len() != ts2.len() {
                    false
                } else {
                    for i in 0..ts1.len() {
                        if ts1[i] != ts2[i] {
                            return false;
                        }
                    }
                    true
                }
            }
            (List(t1), List(t2)) if t1 == t2 => true,
            (Fun(t11, t12), Fun(t21, t22)) if t11 == t12 && t21 == t22 => true,
            (Code(s1, t1), Code(s2, t2)) if s1 == s2 && t1 == t2 => true,
            (SFun(s1, t1), SFun(s2, t2)) if s1 == s2 && t1 == t2 => true,
            _ => false,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Type::*;
        match self {
            Bool => write!(f, "bool"),
            Int => write!(f, "int"),
            Unit => write!(f, "()"),
            TVar(v) => write!(f, "{}", v),
            List(t) => write!(f, "{} list", t),
            Tuple(ts) => {
                let mut s = String::new();
                for (i, t) in ts.iter().enumerate() {
                    if i == 0 {
                        s = format!("{}", t);
                    } else {
                        s = format!("{} * {}", s, t);
                    }
                }
                write!(f, "{}", s)
            }
            Fun(t1, t2) => write!(f, "({} -> {})", t1, t2),
            Code(v, t) => write!(f, "(code {} {})", v, t),
            SFun(s, t) => write!(f, "(sfun {} {})", s, t),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Nil,
    Boolean(bool),
    Number(i64),
    Variable(Variable),
    Stage(Stage),
    BinOp(Op, Box<Expr>, Box<Expr>),
    Fun(Pattern, Box<Expr>),
    SFun(Stage, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Let(Pattern, Box<Expr>, Box<Expr>),
    Quote(Stage, Box<Expr>),
    UnQuote(Stage, Box<Expr>),
    Tuple(Vec<Box<Expr>>),
    Match(Box<Expr>, Vec<Box<(Pattern, Expr)>>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Expr::*;
        match self {
            Nil => write!(f, "[]"),
            Boolean(b) => {
                if *b {
                    write!(f, "true")
                } else {
                    write!(f, "false")
                }
            }
            Number(i) => write!(f, "{}", i),
            Variable(v) => write!(f, "{}", v),
            Stage(s) => write!(f, "{}", s),
            BinOp(o, e1, e2) => write!(f, "({} {} {})", e1, o, e2),
            Fun(p, e) => write!(f, "(fun {} -> {})", p, e),
            SFun(v, e) => write!(f, "(sfun {} -> {})", v, e),
            App(e1, e2) => write!(f, "({} {})", e1, e2),
            If(e1, e2, e3) => write!(f, "if {} then {} else {}", e1, e2, e3),
            Let(v, e1, e2) => write!(f, "let {} = {} in {}", v, e1, e2),
            Quote(v, e) => write!(f, "(quote {} {})", v, e),
            UnQuote(v, e) => write!(f, "(unquote {} {})", v, e),
            Tuple(es) => {
                let mut r = String::from("(");
                for (i, e) in es.iter().enumerate() {
                    if i == 0 {
                        r = format!("{}{}", r, e)
                    } else {
                        r = format!("{},{}", r, e)
                    }
                }
                r = format!("{})", r);
                write!(f, "{}", r)
            }
            Match(exp, ms) => {
                let mut r = format!("match {} with", exp);
                for (i, b) in ms.iter().enumerate() {
                    let (p, e) = &**b;
                    if i == 0 {
                        r = format!("{} {} ->  {}", r, p, e)
                    } else {
                        r = format!("{} | {} ->  {}", r, p, e)
                    }
                }
                write!(f, "{}", r)
            }
        }
    }
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        use Expr::*;
        match (self, other) {
            (Boolean(b1), Boolean(b2)) if b1 == b2 => true,
            (Number(i1), Number(i2)) if i1 == i2 => true,
            (Variable(v1), Variable(v2)) if v1 == v2 => true,
            (BinOp(o1, e1, e2), BinOp(o2, e3, e4)) if o1 == o2 && e1 == e3 && e2 == e4 => true,
            (Fun(v1, e1), Fun(v2, e2)) if v1 == v2 && e1 == e2 => true,
            (SFun(v1, e1), SFun(v2, e2)) if v1 == v2 && e1 == e2 => true,
            (App(e1, e2), App(e3, e4)) if e1 == e3 && e2 == e4 => true,
            (If(e1, e2, e3), If(e4, e5, e6)) if e1 == e4 && e2 == e5 && e3 == e6 => true,
            (Let(e1, e2, e3), Let(e4, e5, e6)) if e1 == e4 && e2 == e5 && e3 == e6 => true,
            (Quote(v1, e1), Quote(v2, e2)) if v1 == v2 && e1 == e2 => true,
            (UnQuote(v1, e1), UnQuote(v2, e2)) if v1 == v2 && e1 == e2 => true,
            (Tuple(es1), Tuple(es2)) if es1 == es2 => true,
            _ => false,
        }
    }
}
