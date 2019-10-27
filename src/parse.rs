use crate::expr::Expr;
use crate::expr::Op;
use crate::expr::Pattern;
use crate::expr::Stage;
use crate::expr::Variable;
use crate::lexer::Lexer;

macro_rules! boxfn {
    ($x: expr) => {
        Box::new($x)
    };
}

fn combinator_or<T: 'static>(
    p1: Box<dyn Fn(&mut Lexer) -> Result<T, String>>,
    p2: Box<dyn Fn(&mut Lexer) -> Result<T, String>>,
) -> Box<dyn Fn(&mut Lexer) -> Result<T, String>> {
    Box::new(move |lexer: &mut Lexer| -> Result<T, String> {
        let r = p1(lexer);
        match r {
            Ok(e) => Ok(e),
            Err(_) => p2(lexer),
        }
    })
}

macro_rules! or {
    ($base:expr) => { $base };
    ($head:expr, $($rest:expr),+) => { combinator_or($head, or!($($rest),+)) };
}

fn combinator_parens<T: 'static>(
    p: Box<dyn Fn(&mut Lexer) -> Result<T, String>>,
) -> Box<dyn Fn(&mut Lexer) -> Result<T, String>> {
    Box::new(move |lexer: &mut Lexer| -> Result<T, String> {
        let t = lexer.peek();
        match &*t {
            "(" => {
                lexer.getone();
                let e = p(lexer)?;
                let t = lexer.peek();
                match &*t {
                    ")" => {
                        lexer.getone();
                        Ok(e)
                    }
                    _ => Err(format!(
                        "combinator_parens failed. The last token is {}.",
                        t
                    )),
                }
            }
            _ => Err(format!(
                "combinator_parens failed. The first token is {}.",
                t
            )),
        }
    })
}

fn parse_pattern(lexer: &mut Lexer) -> Result<Pattern, String> {
    parse_list_pattern(lexer)
}

fn parse_list_pattern(lexer: &mut Lexer) -> Result<Pattern, String> {
    let head = parse_tuple_pattern(lexer)?;
    if lexer.is_end() {
        return Ok(head);
    }
    let o = lexer.peek();
    if o == "::" {
        lexer.getone();
        let rest = parse_list_pattern(lexer)?;
        Ok(Pattern::Cons(Box::new(head), Box::new(rest)))
    } else {
        Ok(head)
    }
}

fn parse_tuple_pattern(lexer: &mut Lexer) -> Result<Pattern, String> {
    or!(
        combinator_parens(boxfn!(parse_inside_tuple_pattern)),
        boxfn!(parse_variable_pattern),
        boxfn!(parse_nil_pattern)
    )(lexer)
}

fn parse_nil_pattern(lexer: &mut Lexer) -> Result<Pattern, String> {
    parse_string("[]".to_string())(lexer)?;
    Ok(Pattern::Nil)
}

fn parse_inside_tuple_pattern(lexer: &mut Lexer) -> Result<Pattern, String> {
    let head = parse_pattern(lexer)?;
    let mut es = vec![Box::new(head.clone())];
    loop {
        if lexer.is_end() {
            return Ok(head);
        }
        let o = lexer.peek();
        if o == "," {
            lexer.getone();
            let e = parse_pattern(lexer)?;
            es.push(Box::new(e))
        } else {
            if es.len() > 1 {
                return Ok(Pattern::Tuple(es));
            } else {
                return Ok(head);
            }
        }
    }
}

fn parse_boolean_constant(lexer: &mut Lexer) -> Result<Expr, String> {
    let t = lexer.peek();
    match &*t {
        "true" => {
            lexer.getone();
            Ok(Expr::Boolean(true))
        }
        "false" => {
            lexer.getone();
            Ok(Expr::Boolean(false))
        }
        _ => {
            let e = format!("This token {} cannot interpret as a boolean.", &*t);
            Err(e.to_string())
        }
    }
}

fn parse_list_expr(lexer: &mut Lexer) -> Result<Expr, String> {
    let head = parse_boolean_expr(lexer)?;
    if lexer.is_end() {
        return Ok(head);
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
            return Ok(ret);
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
            return Ok(ret);
        }
    }
}

fn parse_boolean_term_expr(lexer: &mut Lexer) -> Result<Expr, String> {
    let e1 = parse_numerical_expr(lexer)?;
    if lexer.is_end() {
        return Ok(e1);
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
            _ => Err(format!(
                "This token {} is not a operator in parse_boolean_term_expr",
                t
            )),
        }
    } else {
        Ok(e1)
    }
}

fn parse_number(lexer: &mut Lexer) -> Result<Expr, String> {
    let t = lexer.peek();
    match t.parse::<i64>() {
        Ok(n) => {
            lexer.getone();
            Ok(Expr::Number(n))
        }
        Err(_) => Err("This token is not a number.".to_string()),
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
                }
                _ => Err("parse_paren_expr failed.".to_string()),
            }
        }
        _ => Err("parse_paren_expr failed.".to_string()),
    }
}

fn parse_variable(lexer: &mut Lexer) -> Result<Variable, String> {
    let keyword = vec![
        "if", "then", "else", "fun", "epsilon", "quote", "unquote", "true", "false", "let", "in",
        "match", "with", "sfun",
    ];
    let t = lexer.peek();
    if keyword.contains(&&*t) || !t.chars().nth(0).unwrap().is_alphabetic() {
        Err("parse_variable failed".to_string())
    } else {
        lexer.getone();
        Ok(Variable(t))
    }
}

fn parse_stage(lexer: &mut Lexer) -> Result<Stage, String> {
    let keyword = vec![
        "if", "then", "else", "fun", "epsilon", "quote", "unquote", "true", "false", "let", "in",
        "match", "with", "sfun",
    ];
    let t = lexer.peek();
    if keyword.contains(&&*t) || !t.chars().nth(0).unwrap().is_alphabetic() {
        Err("parse_stage failed".to_string())
    } else {
        lexer.getone();
        let mut v = Vec::new();
        if t != "epsilon" {
            v.push(t);
        }
        Ok(Stage(v))
    }
}

fn parse_variable_pattern(lexer: &mut Lexer) -> Result<Pattern, String> {
    let v = parse_variable(lexer)?;
    Ok(Pattern::Variable(v))
}

fn parse_variable_expr(lexer: &mut Lexer) -> Result<Expr, String> {
    let v = parse_variable(lexer)?;
    Ok(Expr::Variable(v))
}

fn parse_factor(lexer: &mut Lexer) -> Result<Expr, String> {
    if lexer.is_end() {
        Err("parse_factor failed.".to_string())
    } else {
        or!(
            boxfn!(parse_variable_expr),
            boxfn!(parse_number),
            boxfn!(parse_paren_expr),
            boxfn!(parse_epsilon),
            boxfn!(parse_nil),
            boxfn!(parse_boolean_constant)
        )(lexer)
    }
}

fn parse_term(lexer: &mut Lexer) -> Result<Expr, String> {
    let head = parse_app(lexer)?;
    let mut ret = head;
    loop {
        if lexer.is_end() {
            return Ok(ret);
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
            return Ok(ret);
        }
    }
}

fn parse_numerical_expr(lexer: &mut Lexer) -> Result<Expr, String> {
    let head = parse_term(lexer)?;
    let mut ret = head;
    loop {
        if lexer.is_end() {
            return Ok(ret);
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
            return Ok(ret);
        }
    }
}

fn parse_inside_tuple_expr(lexer: &mut Lexer) -> Result<Expr, String> {
    let head = parse_expr(lexer)?;
    let mut es = vec![Box::new(head.clone())];
    loop {
        if lexer.is_end() {
            return Ok(head);
        }
        let o = lexer.peek();
        if o == "," {
            lexer.getone();
            let e = parse_expr(lexer)?;
            es.push(Box::new(e))
        } else {
            if es.len() > 1 {
                return Ok(Expr::Tuple(es));
            } else {
                return Ok(head);
            }
        }
    }
}

fn parse_string(s: String) -> Box<dyn Fn(&mut Lexer) -> Result<String, String>> {
    Box::new(move |lexer: &mut Lexer| -> Result<String, String> {
        let t = lexer.peek();
        if s == t {
            lexer.getone();
            Ok(s.clone())
        } else {
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
    Ok(Expr::Stage(Stage(Vec::new())))
}

fn parse_match(lexer: &mut Lexer) -> Result<Expr, String> {
    parse_string("match".to_string())(lexer)?;
    let expr = parse_expr(lexer)?;
    parse_string("with".to_string())(lexer)?;
    let ms = parse_inside_match(lexer)?;
    Ok(Expr::Match(Box::new(expr), ms))
}

fn parse_inside_match(lexer: &mut Lexer) -> Result<Vec<Box<(Pattern, Expr)>>, String> {
    let hp = parse_pattern(lexer)?;
    parse_string("->".to_string())(lexer)?;
    let he = parse_expr(lexer)?;
    let mut ms = Vec::new();
    ms.push(Box::new((hp, he)));
    loop {
        if lexer.is_end() {
            return Ok(ms);
        }
        let o = lexer.peek();
        if o == "|" {
            lexer.getone();
            let p = parse_pattern(lexer)?;
            parse_string("->".to_string())(lexer)?;
            let e = parse_expr(lexer)?;
            ms.push(Box::new((p, e)));
        } else {
            return Ok(ms);
        }
    }
}

fn parse_if(lexer: &mut Lexer) -> Result<Expr, String> {
    parse_string("if".to_string())(lexer)?;
    let cond = parse_expr(lexer)?;
    parse_string("then".to_string())(lexer)?;
    let e1 = parse_expr(lexer)?;
    parse_string("else".to_string())(lexer)?;
    let e2 = parse_expr(lexer)?;
    Ok(Expr::If(Box::new(cond), Box::new(e1), Box::new(e2)))
}

fn parse_fun(lexer: &mut Lexer) -> Result<Expr, String> {
    parse_string("fun".to_string())(lexer)?;
    let p = parse_pattern(lexer)?;
    parse_string("->".to_string())(lexer)?;
    let e = parse_expr(lexer)?;
    Ok(Expr::Fun(p, Box::new(e)))
}

fn parse_sfun(lexer: &mut Lexer) -> Result<Expr, String> {
    parse_string("sfun".to_string())(lexer)?;
    let s = parse_stage(lexer)?;
    parse_string("->".to_string())(lexer)?;
    let e = parse_expr(lexer)?;
    Ok(Expr::SFun(s, Box::new(e)))
}

fn parse_let(lexer: &mut Lexer) -> Result<Expr, String> {
    parse_string("let".to_string())(lexer)?;
    let p = parse_pattern(lexer)?;
    parse_string("=".to_string())(lexer)?;
    let e1 = parse_expr(lexer)?;
    parse_string("in".to_string())(lexer)?;
    let e2 = parse_expr(lexer)?;
    Ok(Expr::Let(p, Box::new(e1), Box::new(e2)))
}

fn parse_quote(lexer: &mut Lexer) -> Result<Expr, String> {
    parse_string("quote".to_string())(lexer)?;
    let s = parse_stage(lexer)?;
    let e = parse_expr(lexer)?;
    Ok(Expr::Quote(s, Box::new(e)))
}

fn parse_unquote(lexer: &mut Lexer) -> Result<Expr, String> {
    parse_string("unquote".to_string())(lexer)?;
    let s = parse_stage(lexer)?;
    let e = parse_expr(lexer)?;
    Ok(Expr::UnQuote(s, Box::new(e)))
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
                    }
                    Err(_) => return Ok(ret),
                }
            }
        }
        Err(_) => Ok(e1),
    }
}

pub fn parse_expr(lexer: &mut Lexer) -> Result<Expr, String> {
    or!(
        boxfn!(parse_quote),
        boxfn!(parse_unquote),
        boxfn!(parse_fun),
        boxfn!(parse_sfun),
        boxfn!(parse_let),
        boxfn!(parse_if),
        boxfn!(parse_match),
        boxfn!(parse_list_expr)
    )(lexer)
}
