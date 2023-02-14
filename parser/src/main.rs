use nom::{
    branch::alt,
    character::{complete::one_of, streaming::char},
    error::ErrorKind,
    multi::{many0, many1},
    IResult,
};
use rustyline::Editor;

#[derive(Debug)]
enum Expr {
    Num(u64),
    Add(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
}

fn parse_num(c: &str) -> IResult<&str, Expr> {
    let (c1, v) = many1(one_of("0123456789"))(c)?;
    let var: String = v.into_iter().collect();

    if let Ok(n) = var.parse() {
        Ok((c1, Expr::Num(n)))
    } else {
        let err = nom::error::Error::new(c, ErrorKind::Fail);
        Err(nom::Err::Failure(err))
    }
}

fn parse_op(c: &str) -> IResult<&str, Expr> {
    let (c, op) = one_of("+*")(c)?;
    let (c, e1) = parse_expr(c)?;
    let (c, e2) = parse_expr(c)?;

    if op == '+' {
        Ok((c, Expr::Add(Box::new(e1), Box::new(e2))))
    } else {
        Ok((c, Expr::Mul(Box::new(e1), Box::new(e2))))
    }
}

fn parse_expr(c: &str) -> IResult<&str, Expr> {
    let (c, _) = many0(char(' '))(c)?;

    let result = alt((parse_num, parse_op))(c)?;
    Ok(result)
}

fn parse(c: &str) -> Option<Expr> {
    match parse_expr(c) {
        Ok((_, e)) => {
            println!("AST: {:?}", e);
            Some(e)
        }
        Err(e) => {
            println!("{e}");
            None
        }
    }
}

fn eval(e: &Expr) -> u64 {
    match e {
        Expr::Num(n) => *n,
        Expr::Add(e1, e2) => eval(e1) + eval(e2),
        Expr::Mul(e1, e2) => eval(e1) * eval(e2),
    }
}

fn main() {
    let mut rl = Editor::<()>::new().unwrap();
    while let Ok(readline) = rl.readline(">> ") {
        if let Some(e) = parse(&readline) {
            println!("result: {}", eval(&e));
        }
    }
}
