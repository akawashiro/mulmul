#[derive(Debug, Clone)]
pub struct Variable(pub Box<String>);

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

