#[derive(Debug)]
struct Variable(Box<str>);

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
    Fun(Variable, Box<Expr>, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Quote(Variable, Box<Expr>),
    UnQuote(Variable, Box<Expr>)
}

fn tokenize(input_s: String) -> Vec<String> {
    let input : Vec<char> = input_s.chars().collect();
    let mut ret = Vec::new();
    let mut pos : usize = 0;
    let mut tok : String = String::new();
    let ops = "+*/-()";
    let spaces = " \n\t";
    loop {
        if pos >= input.len() {
            break
        }
        if input[pos] == '(' && pos < input.len()-1 && input[pos+1] == '*' {
            pos += 2;
            ret.push("(*".to_string());
        } else if input[pos] == '*' && pos < input.len()-1 && input[pos+1] == ')' {
            pos += 2;
            ret.push("*)".to_string());
        } else if ops.contains(input[pos]) {
            if "" != tok {
                ret.push(tok);
                tok = String::new();
            }	
            ret.push(input[pos].to_string());
            pos+=1;
        } else if spaces.contains(input[pos]) {
            if "" != tok {
                ret.push(tok);
                tok = String::new();
            }	
            pos+=1;
        }else {
            tok.push(input[pos]);
            pos+=1;
        }
    }
    if "" != tok {
        ret.push(tok);
    }	
    ret
}

struct Lexer {
    tokens: Vec<String>,
    position: usize
}

impl Lexer {
    fn new(input: String) -> Lexer {
        let tok = tokenize(input);
        println!("{:?}", tok);
        Lexer { tokens: tok, position: 0 }
    }

    fn peek(&mut self) -> String {
        self.tokens[self.position].clone()
    }

    fn getone(&mut self) -> String {
        let r = self.tokens[self.position].clone();
        self.position += 1;
        r
    }
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

fn parse_factor(lexer: &mut Lexer) -> Result<Expr, String> {
    combine_or(parse_number, parse_paren_expr)(lexer)
}

fn parse_term(lexer: &mut Lexer) -> Result<Expr, String> {
    match parse_factor(lexer) {
        Ok(head) => {
            let mut ret = head;
            loop {
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

fn combine_or(p1: fn(&mut Lexer) -> Result<Expr, String>, p2: fn(&mut Lexer) -> Result<Expr, String>) -> Box<Fn(&mut Lexer) -> Result<Expr, String>> {
    Box::new(move |lexer: &mut Lexer| -> Result<Expr, String>{
        let r = p1(lexer);
        match r {
            Ok(e) => Ok(e),
            Err(_) => p2(lexer)
        }
    })
}

fn parse_expr(lexer: &mut Lexer) -> Result<Expr, String> {
    combine_or(parse_numerical_expr, parse_boolean)(lexer)
}

fn main() {
    let mut lexer = Lexer::new("90 + 1123 * 345 / (34 - 123) true".to_string());
    let r1 = parse_expr(&mut lexer);
    let r2 = parse_expr(&mut lexer);

    println!("{:?}", r1);
    println!("{:?}", r2);
}
