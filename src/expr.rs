#[derive(Debug, Clone)]
pub struct Variable {
    pub name: Box<String>
}

impl Variable {
    fn show(&self) -> String {
        (*self.name).clone()
    }
}

impl PartialEq for Variable{
    fn eq(&self, other: &Self) -> bool {
        *self.name == *other.name
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Epsilon,
    Boolean(bool),
    Number(i64),
    Variable(Variable),
    Plus(Box<Expr>, Box<Expr>),
    Minus(Box<Expr>, Box<Expr>),
    Times(Box<Expr>, Box<Expr>),
    Divides(Box<Expr>, Box<Expr>),
    Fun(Variable, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Quote(Variable, Box<Expr>),
    UnQuote(Variable, Box<Expr>)
}

impl Expr {
    pub fn show(&self) -> String {
        use Expr::*;
        match self {
            Epsilon => String::from("epsilon"),
            Boolean(b) => {
                if *b {
                    String::from("true")
                } else {
                    String::from("false")
                }
            },
            Number(i) => i.to_string(),
            Variable(v) => v.show(),
            Plus(e1,e2) => format!("({}+{})",e1.show(),e2.show()),
            Minus(e1,e2) => format!("({}-{})",e1.show(),e2.show()),
            Times(e1,e2) => format!("({}*{})",e1.show(),e2.show()),
            Divides(e1,e2) => format!("({}/{})",e1.show(),e2.show()),
            Fun(v,e) => format!("(fun {} -> {})", v.show(), e.show()),
            App(e1, e2) => format!("({} {})",e1.show(),e2.show()),
            If(e1, e2, e3) => format!("if {} then {} else {}",e1.show(),e2.show(),e3.show()),
            Quote(v,e) => format!("(quote {} {})", v.show(), e.show()),
            UnQuote(v,e) => format!("(unquote {} {})", v.show(), e.show()),
        }
    }
}

impl PartialEq for Expr{
    fn eq(&self, other: &Self) -> bool {
        use Expr::*;
        match (self, other) {
            (Epsilon, Epsilon) => true,
            (Boolean(b1), Boolean(b2)) if b1 == b2 => true,
            (Number(i1), Number(i2)) if i1 == i2 => true,
            (Plus(e1, e2), Plus(e3, e4)) if e1 == e3 && e2 == e4 => true,
            (Minus(e1, e2), Minus(e3, e4)) if e1 == e3 && e2 == e4 => true,
            (Divides(e1, e2), Divides(e3, e4)) if e1 == e3 && e2 == e4 => true,
            (Times(e1, e2), Times(e3, e4)) if e1 == e3 && e2 == e4 => true,
            (Fun(v1,e1), Fun(v2,e2)) if v1 == v2 && e1 == e2 => true,
            (App(e1,e2), App(e3,e4)) if e1 == e3 && e2 == e4 => true,
            (If(e1,e2,e3), If(e4,e5,e6)) if e1 == e4 && e2 == e5 && e3 == e6 => true,
            (Quote(v1,e1), Quote(v2,e2)) if v1 == v2 && e1 == e2 => true,
            (UnQuote(v1,e1), UnQuote(v2,e2)) if v1 == v2 && e1 == e2 => true,
            _ => false
        }
    }
}
