extern crate combine;
extern crate combine_language;

use combine::char::{string, letter, alpha_num};
use combine::{Parser, satisfy, Stream, StreamOnce, ParseResult, parser, chainl1, choice, attempt, ParseError};
use combine_language::{LanguageEnv, LanguageDef, Identifier};

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

fn language_env<'a, I>() -> LanguageEnv<'a, I> where I: Stream<Item=char>, I::Error: ParseError<I::Item, I::Range, I::Position>, I: 'a {
    LanguageEnv::new(LanguageDef {
        ident: Identifier {
            start: letter(),
            rest: alpha_num(),
            reserved: ["if", "then", "else", "fun", "epsilon", "true", "false"].iter()
                                                                 .map(|x| (*x).into())
                                                                 .collect(),
        },
        op: Identifier {
            start: satisfy(|c| "+-*/".chars().any(|x| x == c)),
            rest: satisfy(|c| "+-*/".chars().any(|x| x == c)),
            reserved: ["+", "-", "*", "/"].iter().map(|x| (*x).into()).collect()
        },
        comment_start: string("/*").map(|_| ()),
        comment_end: string("*/").map(|_| ()),
        comment_line: string("//").map(|_| ()),
    })
}

fn parse_factor<'a, I>(input: &mut I) -> ParseResult<Box<Expr>, I> where I: Stream<Item=char> , I::Error: ParseError<I::Item, I::Range, I::Position>, I: 'a {
    let env = language_env();
    let number = env.integer().map(|x| Box::new(Expr::Number(x)));
    let parenthesized = env.parens(parser(parse_expr));
    number.or(parenthesized).parse_stream(input)
}

fn parse_term<'a, I>(input: &mut I) -> ParseResult<Box<Expr>, I> where I: Stream<Item=char> , I::Error: ParseError<I::Item, I::Range, I::Position>, I: 'a {
    let env = language_env();
    let op = env.reserved_op("*").or(env.reserved_op("/"))
        .map(|op| move |lhs, rhs| {
        if op == "*" {
            Box::new(Expr::Times(lhs, rhs))
        } else if op == "/" {
            Box::new(Expr::Divides(lhs, rhs))
        } else { unreachable!() }
    });
    chainl1(parser(parse_factor), op)
        .parse_stream(input)
}

fn parse_numeral_expr<'a, I>(input: &mut I) -> ParseResult<Box<Expr>, I> where I: Stream<Item=char> , I::Error: ParseError<I::Item, I::Range, I::Position>, I: 'a {
    let env = language_env();
    let op = env.reserved_op("+").or(env.reserved_op("-"))
        .map(|op| move |lhs, rhs| {
        if op == "+" {
            Box::new(Expr::Plus(lhs, rhs))
        } else if op == "-" {
            Box::new(Expr::Minus(lhs, rhs))
        } else { unreachable!() }
    });
    chainl1(parser(parse_term), op)
        .parse_stream(input)
}

fn parse_boolean<'a, I>(input: &mut I) -> ParseResult<Box<Expr>, I> where I: Stream<Item=char> , I::Error: ParseError<I::Item, I::Range, I::Position>, I: 'a{
    let env = language_env();
    let mut boolean = env.reserved("true").or(env.reserved("false"));
    match boolean.parse_stream(input) {
        Ok((r, i)) => if r == "true" {
            Ok((Box::new(Expr::Boolean(true)),i))
        } else if r == "false" {
            Ok((Box::new(Expr::Boolean(false)),i))
        } else { unreachable!() }
        Err(e) => Err(e)
    }
}

fn parse_expr<'a, I>(input: &mut I) -> ParseResult<Box<Expr>, I> where I: Stream<Item=char> , I::Error: ParseError<I::Item, I::Range, I::Position>, I: 'a {
    // let mut p = choice([attempt(parse_boolean), attempt(parse_numeral_expr)]);
    let mut p = choice([parse_boolean, parse_numeral_expr]);
    p.parse_stream(input)
}

fn main() {
    println!("{:?}", parse_expr(&mut "123 + 456 * 78 ABC".chars().collect()));
    // println!("{:?}", parse_expr("false 123"));
}
