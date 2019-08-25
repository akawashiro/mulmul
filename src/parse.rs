use crate::lexer::Lexer;
use crate::expr::Expr;
use crate::expr::Op;
use crate::expr::Variable;

macro_rules! boxfn {
    ($x: expr) => { Box::new($x) }
}

fn combine_or (p1: Box<dyn Fn(&mut Lexer) -> Result<Expr, String>>, p2: Box<dyn Fn(&mut Lexer) -> Result<Expr, String>>) -> Box<dyn Fn(&mut Lexer) -> Result<Expr, String>> {
    Box::new(move |lexer: &mut Lexer| -> Result<Expr, String>{
        let r = p1(lexer);
        match r {
            Ok(e) => Ok(e),
            Err(_) => p2(lexer)
        }
    })
}

macro_rules! or {
    ($base:expr) => { $base };
    ($head:expr, $($rest:expr),+) => { combine_or($head, or!($($rest),+)) };
}

fn parse_boolean_constant(lexer: &mut Lexer) -> Result<Expr, String> {
    let t = lexer.peek();
    match &*t {
        "true" => {
            lexer.getone();
            Ok(Expr::Boolean(true))
        },
        "false" => {
            lexer.getone();
            Ok(Expr::Boolean(false))
        },
        _ => {
            let e = format!("This token {} cannot interpret as a boolean.", &*t);
            Err(e.to_string())
        }
    }
}

fn parse_list_expr(lexer: &mut Lexer) -> Result<Expr, String> {
    let head = parse_boolean_expr(lexer)?;
    if lexer.is_end() {
        return Ok(head)
    }
    let o = lexer.peek();
    if o == "::" {
        lexer.getone();
        let rest = parse_list_expr(lexer)?;
        Ok(Expr::BinOp(Op::Cons, Box::new(head), Box::new(rest)))
    } else {
        Ok(head)
    }
}

fn parse_boolean_expr(lexer: &mut Lexer) -> Result<Expr, String> {
    let head = parse_boolean_term_expr(lexer)?;
    let mut ret = head;
    loop {
        if lexer.is_end() {
            return Ok(ret)
        }
        let o = lexer.peek();
        if o == "&&" {
            lexer.getone();
            let e = parse_boolean_term_expr(lexer)?;
            ret = Expr::BinOp(Op::And, Box::new(ret), Box::new(e))
        } else if o == "||" {
            lexer.getone();
            let e = parse_boolean_term_expr(lexer)?;
            ret = Expr::BinOp(Op::Or, Box::new(ret), Box::new(e))
        } else {
            return Ok(ret)
        }
    }
}

fn parse_boolean_term_expr(lexer: &mut Lexer) -> Result<Expr, String> {
    let e1 = parse_numerical_expr(lexer)?;
    if lexer.is_end() {
        return Ok(e1)
    }
    let t = lexer.peek();
    let ops = vec!["<", ">", "<=", ">=", "=="];
    if ops.contains(&&*t) {
        lexer.getone();
        let e2 = parse_numerical_expr(lexer)?;
        match &*t {
            "<" => Ok(Expr::BinOp(Op::Lt, Box::new(e1), Box::new(e2))),
            "<=" => Ok(Expr::BinOp(Op::Lte, Box::new(e1), Box::new(e2))),
            ">" => Ok(Expr::BinOp(Op::Gt, Box::new(e1), Box::new(e2))),
            ">=" => Ok(Expr::BinOp(Op::Gte, Box::new(e1), Box::new(e2))),
            "==" => Ok(Expr::BinOp(Op::Equal, Box::new(e1), Box::new(e2))),
            _ => Err(format!("This token {} is not a operator in parse_boolean_term_expr", t))
        }
    } else {
        Ok(e1)
    }
}

fn parse_number(lexer: &mut Lexer) -> Result<Expr, String> {
    let t = lexer.peek();
    match t.parse::<i64>(){
        Ok(n) => {
            lexer.getone();
            Ok(Expr::Number(n))
        },
        Err(_) => Err("This token is not a number.".to_string())
    }
}

fn parse_paren_expr(lexer: &mut Lexer) -> Result<Expr, String> {
    let t = lexer.peek();
    match &*t {
        "(" => {
            lexer.getone();
            let e = parse_inside_tuple_expr(lexer)?;
            let t = lexer.peek();
            match &*t {
                ")" => {
                    lexer.getone();
                    Ok(e)
                },
                _ => Err("parse_paren_expr failed.".to_string())
            }
        },
        _ => Err("parse_paren_expr failed.".to_string())
    }
}

fn parse_variable(lexer: &mut Lexer) -> Result<Variable, String> {
    let keyword = vec!["if", "then", "else", "fun", "epsilon", "quote", "unquote", "true", "false", "let", "in"];
    let t = lexer.peek();
    if keyword.contains(&&*t) || ! t.chars().nth(0).unwrap().is_alphabetic() {
        Err("parse_variable failed".to_string())
    } else {
        lexer.getone();
        Ok(Variable {name: Box::new(t)})
    }
}

fn parse_variable_expr(lexer: &mut Lexer) -> Result<Expr, String> {
    let v = parse_variable(lexer)?;
        Ok(Expr::Variable(v))
}

fn parse_factor(lexer: &mut Lexer) -> Result<Expr, String> {
    if lexer.is_end() {
        Err("parse_factor failed.".to_string())
    } else {
        or!(boxfn!(parse_variable_expr), 
            boxfn!(parse_number), 
            boxfn!(parse_paren_expr),
            boxfn!(parse_epsilon),
            boxfn!(parse_nil),
            boxfn!(parse_boolean_constant))(lexer)
    }
}

fn parse_term(lexer: &mut Lexer) -> Result<Expr, String> {
    let head = parse_app(lexer)?;
    let mut ret = head;
    loop {
        if lexer.is_end() {
            return Ok(ret)
        }
        let o = lexer.peek();
        if o == "*" {
            lexer.getone();
            let e = parse_app(lexer)?;
            ret = Expr::BinOp(Op::Times, Box::new(ret), Box::new(e))
        } else if o == "/" {
            lexer.getone();
            let e = parse_app(lexer)?;
            ret = Expr::BinOp(Op::Divides, Box::new(ret), Box::new(e))
        } else {
            return Ok(ret)
        }
    }
}

fn parse_numerical_expr(lexer: &mut Lexer) -> Result<Expr, String> {
    let head = parse_term(lexer)?;
    let mut ret = head;
    loop {
        if lexer.is_end() {
            return Ok(ret)
        }
        let o = lexer.peek();
        if o == "+" {
            lexer.getone();
            let e = parse_term(lexer)?;
            ret = Expr::BinOp(Op::Plus, Box::new(ret), Box::new(e))
        } else if o == "-" {
            lexer.getone();
            let e = parse_term(lexer)?;
            ret = Expr::BinOp(Op::Minus, Box::new(ret), Box::new(e))
        } else {
            return Ok(ret)
        }
    }
}

fn parse_inside_tuple_expr(lexer: &mut Lexer) -> Result<Expr, String> {
    let head = parse_expr(lexer)?;
    let mut es = vec![Box::new(head.clone())];
    loop {
        if lexer.is_end() {
            return Ok(head)
        }
        let o = lexer.peek();
        if o == "," {
            lexer.getone();
            let e = parse_expr(lexer)?;
            es.push(Box::new(e))
        } else {
            if es.len() > 1 {
                return Ok(Expr::Tuple(es))
            } else {
                return Ok(head)
            }
        }
    }
}

fn parse_string(s: String) -> Box<dyn Fn(&mut Lexer) -> Result<String, String>>{
    Box::new(move |lexer: &mut Lexer| -> Result<String, String>{
        let t = lexer.peek();
        if s == t {
            lexer.getone();
            Ok(s.clone())
        } else{
            Err("parse_string(s = ".to_string() + &s + ") failed.")
        }
    })
}

fn parse_nil(lexer: &mut Lexer) -> Result<Expr, String> {
    parse_string("[]".to_string())(lexer)?;
    Ok(Expr::Nil)
}

fn parse_epsilon(lexer: &mut Lexer) -> Result<Expr, String> {
    parse_string("epsilon".to_string())(lexer)?;
    Ok(Expr::Epsilon)
}

fn parse_if (lexer: &mut Lexer) -> Result<Expr, String> {
    parse_string("if".to_string())(lexer)?;
    let cond = parse_expr(lexer)?;
    parse_string("then".to_string())(lexer)?;
    let e1 = parse_expr(lexer)?;
    parse_string("else".to_string())(lexer)?;
    let e2 = parse_expr(lexer)?;
    Ok(Expr::If(Box::new(cond), Box::new(e1), Box::new(e2)))
}

fn parse_fun (lexer: &mut Lexer) -> Result<Expr, String> {
    parse_string("fun".to_string())(lexer)?;
    let var = parse_variable(lexer)?;
    parse_string("->".to_string())(lexer)?;
    let e = parse_expr(lexer)?;
    Ok(Expr::Fun(var, Box::new(e)))
}

fn parse_let (lexer: &mut Lexer) -> Result<Expr, String> {
    parse_string("let".to_string())(lexer)?;
    let var = parse_variable(lexer)?;
    parse_string("=".to_string())(lexer)?;
    let e1 = parse_expr(lexer)?;
    parse_string("in".to_string())(lexer)?;
    let e2 = parse_expr(lexer)?;
    Ok(Expr::Let(var, Box::new(e1), Box::new(e2)))
}

fn parse_quote (lexer: &mut Lexer) -> Result<Expr, String> {
    parse_string("quote".to_string())(lexer)?;
    let var = parse_variable(lexer)?;
    let e = parse_expr(lexer)?;
    Ok(Expr::Quote(var, Box::new(e)))
}

fn parse_unquote (lexer: &mut Lexer) -> Result<Expr, String> {
    parse_string("unquote".to_string())(lexer)?;
    let var = parse_variable(lexer)?;
    let e = parse_expr(lexer)?;
    Ok(Expr::UnQuote(var, Box::new(e)))
}

fn parse_app(lexer: &mut Lexer) -> Result<Expr, String> {
    let e1 = parse_factor(lexer)?;
    match parse_factor(lexer) {
        Ok(e2) => {
            let mut ret = Expr::App(Box::new(e1), Box::new(e2));
            loop {
                match parse_factor(lexer) {
                    Ok(e3) => {
                        ret = Expr::App(Box::new(ret), Box::new(e3));
                    },
                    Err(_) => return Ok(ret)
                }
            }
        },
        Err(_) => Ok(e1) 
    }
}

pub fn parse_expr(lexer: &mut Lexer) -> Result<Expr, String> {
    or!(boxfn!(parse_quote), 
        boxfn!(parse_unquote),
        boxfn!(parse_fun),
        boxfn!(parse_let),
        boxfn!(parse_if),
        boxfn!(parse_list_expr))(lexer)
}


