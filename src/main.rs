mod lexer;
use lexer::Lexer;
mod expr;
mod parse;
use parse::parse_expr;
mod eval;
use eval::is_value;
use eval::eval_one_step;
use std::io::{self, Write};

fn test_with_input_string(vs: Vec<String>) {
    for s in vs {
        let mut lexer = Lexer::new(s);
        let r = parse_expr(&mut lexer);
        match r {
            Ok(e) => {
                let mut e2 = e;
                println!("{}", e2);
                while !is_value(&e2) {
                    e2 = eval_one_step(e2);
                    println!("{}", e2);
                }
            },
            Err(_) => ()
        }
        println!("========================");
    }
}

fn repl() {
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let mut s = String::new();
        std::io::stdin().read_line(&mut s).ok();

        let mut lexer = Lexer::new(s);
        let r = parse_expr(&mut lexer);
        match r {
            Ok(e) => {
                let mut e2 = e;
                println!("{}", e2);
                while !is_value(&e2) {
                    e2 = eval_one_step(e2);
                    println!("{}", e2);
                }
            },
            Err(e) => println!("{:?}", e)
        }

    }
}

fn main() {
    // let inputs = vec![
    //     String::from("1123 + 345"),
    //     String::from("1123 + 345 * 43 - 10"),
    //     String::from("(fun x -> x + 10) ((fun a -> (quote a (20 + 1))) epsilon)"),
    //     // String::from("x + 1123 * 345 * (99 + (fun x -> x) y)"),
    //     // String::from("true"),
    //     // String::from("epsilon"),
    //     // String::from("if true then 1 + 3 else x"),
    //     // String::from("fun x -> x + 2"),
    //     // String::from("(quote a x) + 1"),
    //     // String::from("(unquote a (x + y + z)) + 1"),
    //     String::from("(fun a -> (quote a 1 + 2)) epsilon true")];
    // test_with_input_string(inputs);
    repl();
}
