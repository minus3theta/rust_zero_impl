use super::Instruction;
use crate::helper::safe_add;

#[derive(thiserror::Error, Debug)]
pub enum EvalError {
    #[error("PCOverFlow")]
    PCOverFlow,
    #[error("SPOverFlow")]
    SPOverFlow,
    #[error("InvalidPC")]
    InvalidPC,
    #[error("InvalidContext")]
    InvalidContext,
}

fn eval_depth(
    inst: &[Instruction],
    line: &[char],
    mut pc: usize,
    mut sp: usize,
) -> Result<bool, EvalError> {
    loop {
        let next = if let Some(i) = inst.get(pc) {
            i
        } else {
            return Err(EvalError::InvalidPC);
        };

        match next {
            Instruction::Char(c) => {
                if let Some(sp_c) = line.get(sp) {
                    if c == sp_c {
                        safe_add(&mut pc, &1, || EvalError::PCOverFlow)?;
                        safe_add(&mut sp, &1, || EvalError::SPOverFlow)?;
                    } else {
                        return Ok(false);
                    }
                } else {
                    return Ok(false);
                }
            }
            Instruction::Match => return Ok(true),
            Instruction::Jump(addr) => pc = *addr,
            Instruction::Split(addr1, addr2) => {
                return Ok(
                    eval_depth(inst, line, *addr1, sp)? || eval_depth(inst, line, *addr2, sp)?
                );
            }
        }
    }
}
