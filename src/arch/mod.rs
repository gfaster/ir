use std::io;

use crate::instr::{MachineInstruction, Instruction};

mod x64;

pub type TargetReg = x64::r::Reg;

pub trait InstrPrinter {
    fn emit_instr(writer: &mut impl io::Write, instr: &MachineInstruction)
        -> Result<(), io::Error>;
}

pub trait SelectInstr {
    fn select(instr: &Instruction) -> Vec<MachineInstruction>;
}

pub struct RegBankInfo {
    pub transfer_cost: &'static [u16],
    pub name: &'static str,
}
