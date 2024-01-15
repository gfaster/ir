use std::io;

use crate::instr::MachineInstruction;

mod x64;

pub type TargetReg = x64::r::Reg;

pub trait InstrPrinter {
    fn emit_instr(writer: &mut impl io::Write, instr: &MachineInstruction) -> Result<(), io::Error>;
}
