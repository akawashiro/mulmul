mod lexer;
use lexer::Lexer;

#[derive(Debug)]
struct Variable(Box<String>);

#[derive(Debug)]
enum Expr {
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

macro_rules! boxfn {
    ($x: expr) => { Box::new($x) }
}

fn parse_boolean(lexer: &mut Lexer) -> Result<Expr, String> {
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
        _ => Err("This token cannot interpret as a boolean.".to_string())
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
            match parse_expr(lexer) {
                Ok(e) => {
                    let t = lexer.peek();
                    match &*t {
                        ")" => {
                            lexer.getone();
                            Ok(e)
                        },
                        _ => Err("parse_paren_expr failed.".to_string())
                    }
                },
                Err(_) => Err("parse_paren_expr failed.".to_string())
            }
        },
        _ => Err("parse_paren_expr failed.".to_string())
    }
}

fn parse_variable(lexer: &mut Lexer) -> Result<Variable, String> {
    let keyword = vec!["if", "then", "else", "fun", "epsilon", "quote", "unquote", "true", "false"];
    let t = lexer.peek();
    if keyword.contains(&&*t) || ! t.chars().nth(0).unwrap().is_alphabetic() {
        Err("parse_variable failed".to_string())
    } else {
        lexer.getone();
        Ok(Variable(Box::new(t)))
    }
}

fn parse_variable_expr(lexer: &mut Lexer) -> Result<Expr, String> {
    match parse_variable(lexer) {
        Ok(v) => Ok(Expr::Variable(v)),
        Err(_) => Err("parse_variable_expr failed.".to_string())
    }
}

fn parse_factor(lexer: &mut Lexer) -> Result<Expr, String> {
    combine_or(boxfn!(parse_variable_expr), combine_or(boxfn!(parse_number), boxfn!(parse_paren_expr)))(lexer)
}

fn parse_term(lexer: &mut Lexer) -> Result<Expr, String> {
    match parse_factor(lexer) {
        Ok(head) => {
            let mut ret = head;
            loop {
                if lexer.is_end() {
                    return Ok(ret)
                }
                let o = lexer.peek();
                if o == "*" {
                    lexer.getone();
                    match parse_factor(lexer) {
                        Ok(e) => {
                            ret = Expr::Times(Box::new(ret), Box::new(e))
                        },
                        Err(_) => return Err("parse_term function failed.".to_string())
                    }
                } else if o == "/" {
                    lexer.getone();
                    match parse_factor(lexer) {
                        Ok(e) => {
                            ret = Expr::Divides(Box::new(ret), Box::new(e))
                        },
                        Err(_) => return Err("parse_term function failed.".to_string())
                    }
                } else {
                    return Ok(ret)
                }
            }
        },
        Err(_) => Err("parse_term function failed.".to_string())
    }
}

fn parse_numerical_expr(lexer: &mut Lexer) -> Result<Expr, String> {
    match parse_term(lexer) {
        Ok(head) => {
            let mut ret = head;
            loop {
                if lexer.is_end() {
                    return Ok(ret)
                }
                let o = lexer.peek();
                if o == "+" {
                    lexer.getone();
                    match parse_term(lexer) {
                        Ok(e) => {
                            ret = Expr::Plus(Box::new(ret), Box::new(e))
                        },
                        Err(_) => return Err("parse_numerical_expr function failed.".to_string())
                    }
                } else if o == "-" {
                    lexer.getone();
                    match parse_term(lexer) {
                        Ok(e) => {
                            ret = Expr::Minus(Box::new(ret), Box::new(e))
                        },
                        Err(_) => return Err("parse_numerical_expr function failed.".to_string())
                    }
                } else {
                    return Ok(ret)
                }
            }
        },
        Err(_) => Err("parse_numerical_expr function failed.".to_string())
    }
}

fn parse_string(s: String) -> Box<Fn(&mut Lexer) -> Result<String, String>>{
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

fn parse_epsilon(lexer: &mut Lexer) -> Result<Expr, String> {
    match parse_string("epsilon".to_string())(lexer) {
        Ok(s) => Ok(Expr::Epsilon),
        Err(_) => Err("parse_epsilon failed.".to_string())
    }
}

fn parse_if (lexer: &mut Lexer) -> Result<Expr, String> {
    match parse_string("if".to_string())(lexer) {
        Ok(_) => {
            match parse_expr(lexer) {
                Ok(cond) => {
                    match parse_string("then".to_string())(lexer) {
                        Ok(_) => {
                            match parse_expr(lexer) {
                                Ok(e1) => {
                                    match parse_string("else".to_string())(lexer) {
                                        Ok(_) => {
                                            match parse_expr(lexer) {
                                                Ok(e2) => Ok(Expr::If(Box::new(cond), Box::new(e1), Box::new(e2))),
                                                Err(_) => Err("parse_if failed.".to_string())
                                            }
                                        },
                                        Err(_) => Err("parse_if failed.".to_string())
                                    }
                                },
                                Err(_) => Err("parse_if failed.".to_string())
                            }
                        },
                        Err(_) => Err("parse_if failed.".to_string())
                    }
                },
                Err(_) => Err("parse_if failed.".to_string())
            }
        },
    Err(_) => Err("parse_if failed.".to_string())
    }
}

fn parse_fun (lexer: &mut Lexer) -> Result<Expr, String> {
    match parse_string("fun".to_string())(lexer) {
        Ok(_) => {
            match parse_variable(lexer) {
                Ok(var) => {
                    match parse_string("->".to_string())(lexer) {
                        Ok(_) => {
                            match parse_expr(lexer) {
                                Ok(e) => {
                                    Ok(Expr::Fun(var, Box::new(e)))
                               },
                                Err(_) => Err("parse_fun failed.".to_string())
                            }
                        },
                        Err(_) => Err("parse_fun failed.".to_string())
                    }
                },
                Err(_) => Err("parse_fun failed.".to_string())
            }
        },
    Err(_) => Err("parse_fun failed.".to_string())
    }
}

fn parse_quote (lexer: &mut Lexer) -> Result<Expr, String> {
    match parse_string("quote".to_string())(lexer) {
        Ok(_) => {
            match parse_variable(lexer) {
                Ok(var) => {
                    match parse_expr(lexer) {
                        Ok(e) => {
                            Ok(Expr::Quote(var, Box::new(e)))
                        },
                        Err(_) => Err("parse_quote failed.".to_string())
                    }
                },
                Err(_) => Err("parse_quote failed.".to_string())
            }
        },
        Err(_) => Err("parse_quote failed.".to_string())
    }
}

fn parse_unquote (lexer: &mut Lexer) -> Result<Expr, String> {
    match parse_string("unquote".to_string())(lexer) {
        Ok(_) => {
            match parse_variable(lexer) {
                Ok(var) => {
                    match parse_expr(lexer) {
                        Ok(e) => {
                            Ok(Expr::UnQuote(var, Box::new(e)))
                        },
                        Err(_) => Err("parse_unquote failed.".to_string())
                    }
                },
                Err(_) => Err("parse_unquote failed.".to_string())
            }
        },
        Err(_) => Err("parse_unquote failed.".to_string())
    }
}

fn combine_or(p1: Box<Fn(&mut Lexer) -> Result<Expr, String>>, p2: Box<Fn(&mut Lexer) -> Result<Expr, String>>) -> Box<Fn(&mut Lexer) -> Result<Expr, String>> {
    Box::new(move |lexer: &mut Lexer| -> Result<Expr, String>{
        let r = p1(lexer);
        match r {
            Ok(e) => Ok(e),
            Err(_) => p2(lexer)
        }
    })
}

fn parse_expr(lexer: &mut Lexer) -> Result<Expr, String> {
    combine_or(boxfn!(parse_quote), 
               combine_or(boxfn!(parse_unquote),
               combine_or(boxfn!(parse_fun),
               combine_or(boxfn!(parse_if),
               combine_or(boxfn!(parse_epsilon),
               combine_or(boxfn!(parse_numerical_expr),
               boxfn!(parse_boolean)))))))(lexer)
}

fn main() {
    let mut lexer = Lexer::new("x + 1123 * 345 / (34 - 123)".to_string());
    let mut r = parse_expr(&mut lexer);
    println!("{:?}", r);

    lexer = Lexer::new("true".to_string());
    r = parse_expr(&mut lexer);
    println!("{:?}", r);

    lexer = Lexer::new("epsilon".to_string());
    r = parse_expr(&mut lexer);
    println!("{:?}", r);

    lexer = Lexer::new("if true then 1 + 3 else x".to_string());
    r = parse_expr(&mut lexer);
    println!("{:?}", r);

    lexer = Lexer::new("fun x -> x + 2".to_string());
    r = parse_expr(&mut lexer);
    println!("{:?}", r);

    lexer = Lexer::new("(quote a x) + 1".to_string());
    r = parse_expr(&mut lexer);
    println!("{:?}", r);

    lexer = Lexer::new("(unquote a (x + y + z)) + 1".to_string());
    r = parse_expr(&mut lexer);
    println!("{:?}", r);
}
