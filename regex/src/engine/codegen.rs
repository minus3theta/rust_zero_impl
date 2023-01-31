use super::{parser::AST, Instruction};
use crate::helper::safe_add;

#[derive(thiserror::Error, Debug)]
pub enum CodeGenError {
    #[error("PCOverFlow")]
    PCOverFlow,
    #[error("FailStar")]
    FailStar,
    #[error("FailOr")]
    FailOr,
    #[error("FailQuestion")]
    FailQuestion,
}

#[derive(Default, Debug)]
struct Generator {
    pc: usize,
    insts: Vec<Instruction>,
}

impl Generator {
    fn inc_pc(&mut self) -> Result<(), CodeGenError> {
        safe_add(&mut self.pc, &1, || CodeGenError::PCOverFlow)
    }

    fn gen_expr(&mut self, ast: &AST) -> Result<(), CodeGenError> {
        match ast {
            AST::Char(c) => {
                self.insts.push(Instruction::Char(*c));
                self.inc_pc()?;
            }
            AST::Plus(e) => {
                let return_addr = self.pc;

                self.gen_expr(e)?;

                self.inc_pc()?;
                self.insts.push(Instruction::Split(return_addr, self.pc));
            }
            AST::Star(e) => match e.as_ref() {
                AST::Star(e2) => self.gen_expr(e2)?,
                AST::Seq(e2) if e2.len() == 1 => {
                    if let Some(e3 @ AST::Star(_)) = e2.get(0) {
                        self.gen_expr(e3)?
                    } else {
                        self.gen_star(e)?
                    }
                }
                _ => self.gen_star(e)?,
            },
            AST::Question(e) => {
                let split_addr = self.pc;
                self.inc_pc()?;
                self.insts.push(Instruction::Split(self.pc, 0));

                self.gen_expr(e)?;

                if let Some(Instruction::Split(_, l2)) = self.insts.get_mut(split_addr) {
                    *l2 = self.pc;
                } else {
                    return Err(CodeGenError::FailQuestion);
                }
            }
            AST::Or(e1, e2) => {
                let split_addr = self.pc;
                self.inc_pc()?;
                let split = Instruction::Split(self.pc, 0);
                self.insts.push(split);

                self.gen_expr(e1)?;

                let jmp_addr = self.pc;
                self.insts.push(Instruction::Jump(0));

                self.inc_pc()?;
                if let Some(Instruction::Split(_, l2)) = self.insts.get_mut(split_addr) {
                    *l2 = self.pc;
                } else {
                    return Err(CodeGenError::FailOr);
                }

                self.gen_expr(e2)?;

                if let Some(Instruction::Jump(l3)) = self.insts.get_mut(jmp_addr) {
                    *l3 = self.pc;
                } else {
                    return Err(CodeGenError::FailOr);
                }
            }
            AST::Seq(exprs) => {
                for e in exprs {
                    self.gen_expr(e)?;
                }
            }
        }

        Ok(())
    }

    fn gen_star(&mut self, e: &AST) -> Result<(), CodeGenError> {
        let split_addr = self.pc;
        self.insts.push(Instruction::Split(self.pc, 0));
        self.inc_pc()?;

        self.gen_expr(e)?;

        self.insts.push(Instruction::Jump(split_addr));
        self.inc_pc()?;

        if let Some(Instruction::Split(_, l2)) = self.insts.get_mut(split_addr) {
            *l2 = self.pc;
            Ok(())
        } else {
            Err(CodeGenError::FailStar)
        }
    }

    fn gen_code(&mut self, ast: &AST) -> Result<(), CodeGenError> {
        self.gen_expr(ast)?;
        self.inc_pc()?;
        self.insts.push(Instruction::Match);
        Ok(())
    }
}

pub fn get_code(ast: &AST) -> Result<Vec<Instruction>, CodeGenError> {
    let mut generator = Generator::default();
    generator.gen_code(ast)?;
    Ok(generator.insts)
}
