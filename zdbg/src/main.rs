mod dbg;

use std::env;

use anyhow::bail;
use rustyline::{error::ReadlineError, Editor};

use dbg::{State, ZDbg};

fn main() -> anyhow::Result<()> {
    let args: Vec<_> = env::args().collect();
    if args.len() < 2 {
        bail!("引数が必要です\n例 : {} 実行ファイル [引数*]", args[0]);
    }

    run_dbg(&args[1])?;
    Ok(())
}

fn run_dbg(filename: &str) -> anyhow::Result<()> {
    let debugger = ZDbg::new(filename.to_string());
    let mut state = State::NotRunning(debugger);
    let mut rl = Editor::<()>::new()?;

    loop {
        match rl.readline("zdbg > ") {
            Ok(line) => {
                let trimmed = line.trim();
                let cmd: Vec<_> = trimmed.split(' ').filter(|c| !c.is_empty()).collect();
                state = match state {
                    State::Running(r) => r.do_cmd(&cmd)?,
                    State::NotRunning(n) => n.do_cmd(&cmd)?,
                    _ => break,
                };
                if let State::Exit = state {
                    break;
                }
                rl.add_history_entry(line);
            }
            Err(ReadlineError::Interrupted) => eprintln!("<<終了はCtrl+d>>"),
            _ => {
                if let State::Running(r) = state {
                    r.do_cmd(&["exit"])?;
                };
                break;
            }
        }
    }
    Ok(())
}
