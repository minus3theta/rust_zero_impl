mod helper;
mod parser;
mod typing;

use anyhow::bail;
use nom::error::convert_error;
use std::{env, fs};

fn main() -> anyhow::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("以下のようにファイル名を指定して実行してください\ncargo run codes/ex1.lin");
        bail!("引数が不足");
    }

    let content = fs::read_to_string(&args[1])?;

    let ast = parser::parse_expr(&content);
    println!("AST:\n{ast:#?}\n");
    match ast {
        Ok((_, expr)) => {
            let mut ctx = typing::TypeEnv::new();
            println!("式:\n{content}");

            let a = typing::typing(&expr, &mut ctx, 0)?;
            println!("の型は\n{a}\nです。");
        }
        Err(nom::Err::Error(e)) => {
            let msg = convert_error(content.as_str(), e);
            eprintln!("パースエラー:\n{msg}");
            bail!(msg);
        }
        _ => (),
    }

    Ok(())
}
