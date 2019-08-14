extern crate combine;

use combine::{Parser, many1};
use combine::char::digit;

#[derive(Debug)]
enum Exp {
    EInt { value: i32 }
}

fn parse_exp(input: &str) -> Result<(Exp, &str), combine::ParseError<&str>>{
    let mut parser = many1(digit()).map(|string: String| string.parse::<i32>().unwrap());
    match parser.parse(input){
        Ok((result, rest)) => Ok((Exp::EInt{ value: result }, rest)),
        Err(err) => Err(err)
    }
}

fn main() {
    let mut parser = many1::<Vec<_>, _>(digit());
    println!("{:?}", parser.parse("123"));
    println!("{:?}", parser.parse("123ABC"));
    println!("{:?}", parser.parse("ABC123"));
    println!("{:?}", parse_exp("123ABC"));
}
