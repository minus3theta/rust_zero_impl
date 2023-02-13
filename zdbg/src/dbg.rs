use std::{
    ffi::{c_void, CString},
    marker::PhantomData,
};

use anyhow::bail;
use nix::{
    libc::user_regs_struct,
    sys::{
        self,
        personality::{self, Persona},
        ptrace,
        wait::{waitpid, WaitStatus},
    },
    unistd::{execvp, fork, ForkResult, Pid},
};

pub struct DbgInfo {
    pid: Pid,
    brk_addr: Option<*mut c_void>,
    brk_val: i64,
    filename: String,
}

pub struct ZDbg<T> {
    info: Box<DbgInfo>,
    _state: std::marker::PhantomData<T>,
}

pub enum Running {}
pub enum NotRunning {}

pub enum State {
    Running(ZDbg<Running>),
    NotRunning(ZDbg<NotRunning>),
    Exit,
}

impl<T> ZDbg<T> {
    fn set_break_addr(&mut self, cmd: &[&str]) -> bool {
        if let Some(brk_addr) = self.info.brk_addr {
            eprintln!("<<ブレークポイントは設定済みです : Addr = {:p}>>", brk_addr);
            false
        } else if let Some(addr) = get_break_addr(cmd) {
            self.info.brk_addr = Some(addr);
            true
        } else {
            false
        }
    }

    fn do_cmd_common(&self, cmd: &[&str]) {
        match cmd[0] {
            "help" | "h" => do_help(),
            _ => (),
        }
    }
}

impl ZDbg<NotRunning> {
    pub fn new(filename: String) -> Self {
        Self {
            info: Box::new(DbgInfo {
                pid: Pid::from_raw(0),
                brk_addr: None,
                brk_val: 0,
                filename,
            }),
            _state: PhantomData,
        }
    }

    pub fn do_cmd(mut self, cmd: &[&str]) -> anyhow::Result<State> {
        if cmd.is_empty() {
            return Ok(State::NotRunning(self));
        }

        match cmd[0] {
            "run" | "r" => return self.do_run(cmd),
            "break" | "b" => {
                self.do_break(cmd);
            }
            "exit" => return Ok(State::Exit),
            "continue" | "c" | "stepi" | "s" | "registers" | "regs" => {
                eprintln!("<<ターゲットを実行していません。runで実行してください>>");
            }
            _ => self.do_cmd_common(cmd),
        }

        Ok(State::NotRunning(self))
    }

    fn do_break(&mut self, cmd: &[&str]) -> bool {
        self.set_break_addr(cmd)
    }

    fn do_run(mut self, cmd: &[&str]) -> anyhow::Result<State> {
        let args: Vec<CString> = cmd.iter().map(|s| CString::new(*s).unwrap()).collect();

        match unsafe { fork()? } {
            ForkResult::Child => {
                let p = personality::get().unwrap();
                personality::set(p | Persona::ADDR_NO_RANDOMIZE).unwrap();
                ptrace::traceme().unwrap();

                execvp(&CString::new(self.info.filename.as_str()).unwrap(), &args).unwrap();
                unreachable!();
            }
            ForkResult::Parent { child, .. } => match waitpid(child, None)? {
                sys::wait::WaitStatus::Stopped(..) => {
                    println!("<<子プロセスの実行に成功しました : PID = {child}>>");
                    self.info.pid = child;
                    let mut dbg = ZDbg::<Running> {
                        info: self.info,
                        _state: PhantomData,
                    };
                    dbg.set_break()?;
                    dbg.do_continue()
                }
                sys::wait::WaitStatus::Exited(..) | sys::wait::WaitStatus::Signaled(..) => {
                    bail!("子プロセスの実行に失敗しました")
                }
                _ => bail!("子プロセスが不正な状態です"),
            },
        }
    }
}

impl ZDbg<Running> {
    pub fn do_cmd(mut self, cmd: &[&str]) -> anyhow::Result<State> {
        if cmd.is_empty() {
            return Ok(State::Running(self));
        }

        match cmd[0] {
            "break" | "b" => self.do_break(cmd)?,
            "continue" | "c" => return self.do_continue(),
            "registers" | "regs" => {
                let regs = ptrace::getregs(self.info.pid)?;
                print_regs(&regs);
            }
            "stepi" | "s" => return self.do_stepi(),
            "run" | "r" => eprintln!("<<すでに実行中です>>"),
            "exit" => {
                self.do_exit()?;
                return Ok(State::Exit);
            }
            _ => self.do_cmd_common(cmd),
        }

        Ok(State::Running(self))
    }

    fn do_exit(self) -> anyhow::Result<()> {
        loop {
            ptrace::kill(self.info.pid)?;
            match waitpid(self.info.pid, None)? {
                WaitStatus::Exited(..) | WaitStatus::Signaled(..) => return Ok(()),
                _ => (),
            }
        }
    }

    fn do_break(&mut self, cmd: &[&str]) -> anyhow::Result<()> {
        if self.set_break_addr(cmd) {
            self.set_break()?;
        }
        Ok(())
    }

    fn set_break(&mut self) -> anyhow::Result<()> {
        let addr = if let Some(addr) = self.info.brk_addr {
            addr
        } else {
            return Ok(());
        };

        let val = match ptrace::read(self.info.pid, addr) {
            Ok(val) => val,
            Err(e) => {
                eprintln!("<<ptrace::readに失敗 : {e}, addr = {:p}>>", addr);
                return Ok(());
            }
        };

        fn print_val(addr: usize, val: i64) {
            print!("{addr:x}:");
            for n in (0..8).map(|n| ((val >> (n * 8)) & 0xff) as u8) {
                print!(" {n:x}");
            }
        }

        println!("<<以下のようにメモリを書き換えます>>");
        print!("<<before: ");
        print_val(addr as usize, val);
        println!(">>");

        let val_int3 = (val & !0xff) | 0xcc;
        print!("<<after: ");
        print_val(addr as usize, val_int3);
        println!(">>");

        match unsafe { ptrace::write(self.info.pid, addr, val_int3 as *mut c_void) } {
            Ok(_) => {
                self.info.brk_addr = Some(addr);
                self.info.brk_val = val;
            }
            Err(e) => {
                eprintln!("<<ptrace::writeに失敗 : {e}, addr = {addr:p}>>");
            }
        }

        Ok(())
    }

    fn do_continue(self) -> anyhow::Result<State> {
        match self.step_and_break()? {
            State::Running(r) => {
                ptrace::cont(r.info.pid, None)?;
                r.wait_child()
            }
            n => Ok(n),
        }
    }

    fn step_and_break(mut self) -> anyhow::Result<State> {
        let regs = ptrace::getregs(self.info.pid)?;
        if Some((regs.rip) as *mut c_void) == self.info.brk_addr {
            ptrace::step(self.info.pid, None)?;
            match waitpid(self.info.pid, None)? {
                WaitStatus::Exited(..) | WaitStatus::Signaled(..) => {
                    println!("<<子プロセスが終了しました>>");
                    return Ok(State::NotRunning(ZDbg {
                        info: self.info,
                        _state: PhantomData,
                    }));
                }
                _ => (),
            }
            self.set_break()?;
        }

        Ok(State::Running(self))
    }

    fn wait_child(self) -> anyhow::Result<State> {
        match waitpid(self.info.pid, None)? {
            WaitStatus::Exited(..) | WaitStatus::Signaled(..) => {
                println!("<<子プロセスが終了しました>>");
                Ok(State::NotRunning(ZDbg {
                    info: self.info,
                    _state: PhantomData,
                }))
            }
            WaitStatus::Stopped(..) => {
                let mut regs = ptrace::getregs(self.info.pid)?;
                if Some((regs.rip - 1) as *mut c_void) == self.info.brk_addr {
                    unsafe {
                        ptrace::write(
                            self.info.pid,
                            self.info.brk_addr.unwrap(),
                            self.info.brk_val as *mut c_void,
                        )?
                    };

                    regs.rip -= 1;
                    ptrace::setregs(self.info.pid, regs)?;
                }
                println!("<<子プロセスが停止しました : PC = {:#x}>>", regs.rip);

                Ok(State::Running(self))
            }
            _ => bail!("waitpidの返り値が不正です"),
        }
    }

    fn do_stepi(self) -> anyhow::Result<State> {
        let regs = ptrace::getregs(self.info.pid)?;
        if Some((regs.rip) as *mut c_void) == self.info.brk_addr {
            unsafe {
                ptrace::write(
                    self.info.pid,
                    self.info.brk_addr.unwrap(),
                    self.info.brk_val as *mut c_void,
                )?
            };
            self.step_and_break()
        } else {
            ptrace::step(self.info.pid, None)?;
            self.wait_child()
        }
    }
}

fn do_help() {
    println!(
        r#"コマンド一覧 (括弧内は省略記法)
break 0x8000 : ブレークポイントを0x8000番地に設定 (b 0x8000)
run          : プログラムを実行 (r)
continue     : プログラムを再開 (c)
stepi        : 機械語レベルで1ステップ実行 (s)
registers    : レジスタを表示 (regs)
exit         : 終了
help         : このヘルプを表示 (h)"#
    );
}

fn print_regs(regs: &user_regs_struct) {
    println!(
        r#"RIP: {:#016x}, RSP: {:#016x}, RBP: {:#016x}
RAX: {:#016x}, RBX: {:#016x}, RCX: {:#016x}
RDX: {:#016x}, RSI: {:#016x}, RDI: {:#016x}
 R8: {:#016x},  R9: {:#016x}, R10: {:#016x}
R11: {:#016x}, R12: {:#016x}, R13: {:#016x}
R14: {:#016x}, R15: {:#016x}"#,
        regs.rip,
        regs.rsp,
        regs.rbp,
        regs.rax,
        regs.rbx,
        regs.rcx,
        regs.rdx,
        regs.rsi,
        regs.rdi,
        regs.r8,
        regs.r9,
        regs.r10,
        regs.r11,
        regs.r12,
        regs.r13,
        regs.r14,
        regs.r15,
    );
}

fn get_break_addr(cmd: &[&str]) -> Option<*mut c_void> {
    if cmd.len() < 2 {
        eprintln!("<<アドレスを指定してください\n例 : break 0x8000>>");
        return None;
    }

    let addr_str = cmd[1];
    if &addr_str[0..2] != "0x" {
        eprintln!("<<アドレスは16進数でのみ指定可能です\n例 : break 0x8000>>");
        return None;
    }

    let addr = match usize::from_str_radix(&addr_str[2..], 16) {
        Ok(addr) => addr,
        Err(e) => {
            eprintln!("<<アドレス変換エラー : {e}>>");
            return None;
        }
    } as *mut c_void;

    Some(addr)
}
