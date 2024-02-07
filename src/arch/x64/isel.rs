use crate::instr::{OpInner, Instruction, MachineInstruction};

pub struct X64Selector;
impl crate::arch::SelectInstr for X64Selector {
    fn select(instr: &Instruction) -> Vec<MachineInstruction> {
        match &instr.inner {
            OpInner::Call { id, args } => todo!(),
            OpInner::Alloc { ty } => todo!(),
            OpInner::Assign { val } => todo!(),
            OpInner::Load { ptr } => todo!(),
            OpInner::Block {  } => todo!(),
            OpInner::Br { check, success, fail } => todo!(),
            OpInner::MachInstr { ins } => todo!(),
            OpInner::IrInstr { prop, args } => todo!(),
            OpInner::Jmp { target } => todo!(),
            OpInner::Store { dst, val } => todo!(),
            OpInner::Return { val } => todo!(),
        }
    }
}

