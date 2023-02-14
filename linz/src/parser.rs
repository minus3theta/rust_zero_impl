use std::fmt;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, char, multispace0, multispace1},
    error::VerboseError,
    sequence::delimited,
    IResult,
};

#[derive(Debug)]
pub enum Expr {
    Let(LetExpr),
    If(IfExpr),
    Split(SplitExpr),
    Free(FreeExpr),
    App(AppExpr),
    Var(String),
    QVal(QValExpr),
}

#[derive(Debug)]
pub struct AppExpr {
    pub expr1: Box<Expr>,
    pub expr2: Box<Expr>,
}

#[derive(Debug)]
pub struct IfExpr {
    pub cond_expr: Box<Expr>,
    pub then_expr: Box<Expr>,
    pub else_expr: Box<Expr>,
}

#[derive(Debug)]
pub struct SplitExpr {
    pub expr: Box<Expr>,
    pub left: String,
    pub right: String,
    pub body: Box<Expr>,
}

#[derive(Debug)]
pub struct LetExpr {
    pub var: String,
    pub ty: TypeExpr,
    pub expr1: Box<Expr>,
    pub expr2: Box<Expr>,
}

#[derive(Debug)]
pub enum ValExpr {
    Bool(bool),
    Pair(Box<Expr>, Box<Expr>),
    Fun(FnExpr),
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum Qual {
    Lin,
    Un,
}

#[derive(Debug)]
pub struct QValExpr {
    pub qual: Qual,
    pub val: ValExpr,
}

#[derive(Debug)]
pub struct FnExpr {
    pub var: String,
    pub ty: TypeExpr,
    pub expr: Box<Expr>,
}

#[derive(Debug)]
pub struct FreeExpr {
    pub var: String,
    pub expr: Box<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypeExpr {
    pub qual: Qual,
    pub prim: PrimType,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum PrimType {
    Bool,
    Pair(Box<TypeExpr>, Box<TypeExpr>),
    Arrow(Box<TypeExpr>, Box<TypeExpr>),
}

impl fmt::Display for TypeExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.qual {
            Qual::Lin => write!(f, "lin {}", self.prim),
            Qual::Un => write!(f, "un {}", self.prim),
        }
    }
}

impl fmt::Display for PrimType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PrimType::Bool => write!(f, "bool"),
            PrimType::Pair(t1, t2) => write!(f, "({t1} * {t2})"),
            PrimType::Arrow(t1, t2) => write!(f, "({t1} -> {t2})"),
        }
    }
}

pub fn parse_expr(i: &str) -> IResult<&str, Expr, VerboseError<&str>> {
    let (i, _) = multispace0(i)?;
    let (i, val) = alt((alpha1, tag("(")))(i)?;

    match val {
        "let" => parse_let(i),
        "if" => parse_if(i),
        "split" => parse_split(i),
        "free" => parse_free(i),
        "lin" => parse_qval(Qual::Lin, i),
        "un" => parse_qval(Qual::Un, i),
        "(" => parse_app(i),
        _ => Ok((i, Expr::Var(val.to_string()))),
    }
}

fn parse_qval(q: Qual, i: &str) -> IResult<&str, Expr, VerboseError<&str>> {
    let (i, _) = multispace1(i)?;
    let (i, v) = parse_val(i)?;

    Ok((i, Expr::QVal(QValExpr { qual: q, val: v })))
}

fn parse_val(i: &str) -> IResult<&str, ValExpr, VerboseError<&str>> {
    let (i, val) = alt((tag("fn"), tag("true"), tag("false"), tag("<")))(i)?;
    match val {
        "fn" => parse_fn(i),
        "true" => Ok((i, ValExpr::Bool(true))),
        "false" => Ok((i, ValExpr::Bool(false))),
        "<" => parse_pair(i),
        _ => unreachable!(),
    }
}

fn parse_fn(i: &str) -> IResult<&str, ValExpr, VerboseError<&str>> {
    let (i, _) = multispace1(i)?;
    let (i, var) = parse_var(i)?;

    let (i, _) = multispace0(i)?;
    let (i, _) = char(':')(i)?;
    let (i, _) = multispace0(i)?;

    let (i, ty) = parse_type(i)?;
    let (i, _) = multispace0(i)?;

    let (i, expr) = delimited(
        char('{'),
        delimited(multispace0, parse_expr, multispace0),
        char('}'),
    )(i)?;

    Ok((
        i,
        ValExpr::Fun(FnExpr {
            var,
            ty,
            expr: Box::new(expr),
        }),
    ))
}

fn parse_var(i: &str) -> IResult<&str, String, VerboseError<&str>> {
    let (i, v) = alpha1(i)?;
    Ok((i, v.to_string()))
}

fn parse_type(i: &str) -> IResult<&str, TypeExpr, VerboseError<&str>> {
    let (i, q) = parse_qual(i)?;
    let (i, _) = multispace1(i)?;
    let (i, val) = alt((tag("bool"), tag("(")))(i)?;
    if val == "bool" {
        Ok((
            i,
            TypeExpr {
                qual: q,
                prim: PrimType::Bool,
            },
        ))
    } else {
        let (i, _) = multispace0(i)?;
        let (i, t1) = parse_type(i)?;
        let (i, _) = multispace0(i)?;

        let (i, op) = alt((tag("*"), tag("->")))(i)?;

        let (i, _) = multispace0(i)?;
        let (i, t2) = parse_type(i)?;
        let (i, _) = multispace0(i)?;

        let (i, _) = char(')')(i)?;

        Ok((
            i,
            TypeExpr {
                qual: q,
                prim: if op == "*" {
                    PrimType::Pair(Box::new(t1), Box::new(t2))
                } else {
                    PrimType::Arrow(Box::new(t1), Box::new(t2))
                },
            },
        ))
    }
}

fn parse_qual(i: &str) -> IResult<&str, Qual, VerboseError<&str>> {
    let (i, val) = alt((tag("lin"), tag("un")))(i)?;
    if val == "lin" {
        Ok((i, Qual::Lin))
    } else {
        Ok((i, Qual::Un))
    }
}

fn parse_if(i: &str) -> IResult<&str, Expr, VerboseError<&str>> {
    let (i, _) = multispace1(i)?;
    let (i, cond_expr) = parse_expr(i)?;
    let (i, _) = multispace0(i)?;

    let (i, then_expr) = delimited(
        char('{'),
        delimited(multispace0, parse_expr, multispace0),
        char('}'),
    )(i)?;

    let (i, _) = multispace0(i)?;
    let (i, _) = tag("else")(i)?;
    let (i, _) = multispace0(i)?;

    let (i, else_expr) = delimited(
        char('{'),
        delimited(multispace0, parse_expr, multispace0),
        char('}'),
    )(i)?;

    Ok((
        i,
        Expr::If(IfExpr {
            cond_expr: Box::new(cond_expr),
            then_expr: Box::new(then_expr),
            else_expr: Box::new(else_expr),
        }),
    ))
}

fn parse_split(i: &str) -> IResult<&str, Expr, VerboseError<&str>> {
    let (i, _) = multispace1(i)?;
    let (i, expr) = parse_expr(i)?;
    let (i, _) = multispace0(i)?;

    let (i, _) = tag("as")(i)?;
    let (i, _) = multispace1(i)?;

    let (i, left) = parse_var(i)?;
    let (i, _) = multispace0(i)?;
    let (i, _) = tag(",")(i)?;
    let (i, _) = multispace0(i)?;
    let (i, right) = parse_var(i)?;

    let (i, _) = multispace0(i)?;
    let (i, body) = delimited(
        char('{'),
        delimited(multispace0, parse_expr, multispace0),
        char('}'),
    )(i)?;

    Ok((
        i,
        Expr::Split(SplitExpr {
            expr: Box::new(expr),
            left,
            right,
            body: Box::new(body),
        }),
    ))
}

fn parse_free(i: &str) -> IResult<&str, Expr, VerboseError<&str>> {
    let (i, _) = multispace1(i)?;
    let (i, var) = parse_var(i)?;

    let (i, _) = multispace0(i)?;
    let (i, _) = tag(";")(i)?;

    let (i, _) = multispace0(i)?;
    let (i, expr) = parse_expr(i)?;

    Ok((
        i,
        Expr::Free(FreeExpr {
            var,
            expr: Box::new(expr),
        }),
    ))
}

fn parse_app(i: &str) -> IResult<&str, Expr, VerboseError<&str>> {
    let (i, _) = multispace0(i)?;
    let (i, expr1) = parse_expr(i)?;

    let (i, _) = multispace1(i)?;
    let (i, expr2) = parse_expr(i)?;

    let (i, _) = multispace0(i)?;
    let (i, _) = tag(")")(i)?;

    Ok((
        i,
        Expr::App(AppExpr {
            expr1: Box::new(expr1),
            expr2: Box::new(expr2),
        }),
    ))
}

fn parse_let(i: &str) -> IResult<&str, Expr, VerboseError<&str>> {
    let (i, _) = multispace1(i)?;
    let (i, var) = parse_var(i)?;

    let (i, _) = multispace0(i)?;
    let (i, _) = tag(":")(i)?;

    let (i, _) = multispace0(i)?;
    let (i, ty) = parse_type(i)?;

    let (i, _) = multispace0(i)?;
    let (i, _) = tag("=")(i)?;

    let (i, _) = multispace0(i)?;
    let (i, expr1) = parse_expr(i)?;

    let (i, _) = multispace0(i)?;
    let (i, _) = tag(";")(i)?;

    let (i, _) = multispace0(i)?;
    let (i, expr2) = parse_expr(i)?;

    Ok((
        i,
        Expr::Let(LetExpr {
            var,
            ty,
            expr1: Box::new(expr1),
            expr2: Box::new(expr2),
        }),
    ))
}

fn parse_pair(i: &str) -> IResult<&str, ValExpr, VerboseError<&str>> {
    let (i, _) = multispace0(i)?;
    let (i, left) = parse_expr(i)?;

    let (i, _) = multispace0(i)?;
    let (i, _) = tag(",")(i)?;

    let (i, _) = multispace0(i)?;
    let (i, right) = parse_expr(i)?;

    let (i, _) = multispace0(i)?;
    let (i, _) = tag(">")(i)?;

    Ok((i, ValExpr::Pair(Box::new(left), Box::new(right))))
}
