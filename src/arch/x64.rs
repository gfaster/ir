#![allow(non_camel_case_types)]
use macros::new_instr;

use crate::{
    instr::{ArgCnt, BasicInstrProp, MachineInstrProp},
    regstate::PhysRegUse, ty::{MachineType, MachineLocationType},
};

use crate::ty::Type as Ty;

use super::SelectInstr;
mod isel;
pub mod r;

const II64: MachineType = MachineType {
    ty: Ty::i64(),
    loc: MachineLocationType::Imm,
};
const MI64: MachineType = MachineType {
    ty: Ty::i64(),
    loc: MachineLocationType::Reg,
};
const DISP: MachineType = MachineType {
    ty: Ty::displacement(),
    loc: MachineLocationType::Imm,
};
const FLAG: MachineType = MachineType {
    ty: Ty::i1(),
    loc: MachineLocationType::Reg,
};

const BASIC_TEMPLATE: BasicInstrProp = BasicInstrProp {
    op_cnt: 0,
    res_cnt: 0,
    mnemonic: "[TEMPLATE]",
    is_branch: false,
    is_terminator: false,
    is_block_header: false,
    is_commutative: false,
    has_side_effects: true,
    may_read_memory: true,
    may_write_memory: true,
    is_barrier: false,
    operand_relative_type_constraints: &[],
    simulation: None,
};

const BASIC_TEMPLATE_SIMPLE: BasicInstrProp = BasicInstrProp {
    op_cnt: 0,
    res_cnt: 0,
    mnemonic: "[TEMPLATE]",
    is_branch: false,
    is_terminator: false,
    is_block_header: false,
    is_commutative: false,
    has_side_effects: false,
    may_read_memory: false,
    may_write_memory: false,
    is_barrier: false,
    operand_relative_type_constraints: &[],
    simulation: None,
};

const OP_BIN_RR_R: MachineInstrProp = MachineInstrProp {
    basic: BasicInstrProp {
        op_cnt: 2,
        res_cnt: 1,
        is_branch: false,
        is_terminator: false,
        is_commutative: false,
        ..BASIC_TEMPLATE
    },
    ref_regs: &[],
    operand_use: &[PhysRegUse::UseDef, PhysRegUse::UseDef, PhysRegUse::Use],
    op_eq_constraints: &[0, 0, 1],
    op_ty: &[MI64, MI64, MI64]
};

const OP_BIN_RR_F: MachineInstrProp = MachineInstrProp {
    basic: BasicInstrProp {
        op_cnt: 2,
        res_cnt: 1,
        ..BASIC_TEMPLATE_SIMPLE
    },
    ref_regs: &[(r::Reg::Eflags.to_mach(), PhysRegUse::Def)],
    operand_use: &[PhysRegUse::Use, PhysRegUse::Use],
    op_eq_constraints: &[0, 1, 2],
    op_ty: &[MI64, MI64, FLAG]
};

const OP_BIN_RR_FR: MachineInstrProp = MachineInstrProp {
    basic: BasicInstrProp {
        op_cnt: 2,
        res_cnt: 2,
        operand_relative_type_constraints: &[0, 1, 1, 1],
        ..BASIC_TEMPLATE_SIMPLE
    },
    operand_use: &[PhysRegUse::UseDef, PhysRegUse::UseDef, PhysRegUse::Use],
    op_eq_constraints: &[0, 0, 1],
    ref_regs: &[(r::Reg::Eflags.to_mach(), PhysRegUse::Def)],
    op_ty: &[MI64, MI64, MI64, FLAG]
};

const OP_BIN_RI_FR: MachineInstrProp = MachineInstrProp {
    op_ty: &[MI64, MI64, II64, FLAG],
    ..OP_BIN_RR_FR
};

/// Binary Reg, Mem(read-only) -> Reg + Flags
///
/// ex:
/// ```x86asm
/// add r9, qword ptr [0x28 + rsp + 4 * rcx]
const OP_BIN_RM_RF: MachineInstrProp = MachineInstrProp {
    basic: BasicInstrProp {
        op_cnt: 2,
        res_cnt: 1,
        is_branch: false,
        is_terminator: false,
        is_commutative: false,
        may_read_memory: true,
        ..BASIC_TEMPLATE
    },
    ref_regs: &[(r::Reg::Eflags.to_mach(), PhysRegUse::Def)],
    operand_use: &[PhysRegUse::UseDef, PhysRegUse::UseDef, PhysRegUse::Use],
    op_eq_constraints: &[0, 0, 1, 2, 3, 4],
    op_ty: &[]
};

new_instr!{
    let is_commutative = true, has_side_effects = false => {
        ADD = "add a:MI64 b:MI64";
    };
}

const OP_BIN_RM_RF_: MachineInstrProp = MachineInstrProp {
    ..ADD
};


macro_rules! def_instr {
    ($base:expr => $name:ident $(:)? ) => {
        const _: MachineInstrProp = $base;
        compile_error!(": $mnemonic");
    };
    ($base:expr => ) => {
        const _: MachineInstrProp = $base;
        compile_error!("missing: $name: $mnemonic");
    };
    ($base:expr) => {
        const _: MachineInstrProp = $base;
        compile_error!("missing: => $name: $mnemonic");
    };
    () => {
        compile_error!("missing: $base => $name: $mnemonic");
    };
    ($base:expr => $name:ident: $mnemonic:literal) => {
        pub const $name: MachineInstrProp = MachineInstrProp {
            basic: BasicInstrProp {
                mnemonic: $mnemonic,
                ..$base.basic
            },
            ..$base
        };
    };
}

def_instr!(OP_BIN_RI_FR => I_ADD_RI: "add");
def_instr!(OP_BIN_RR_FR => I_ADD_RR: "add");

#[cfg(test)]
mod tests {
    use macros::new_instr;

    #[test]
    fn test0() {
        dbg!(super::ADD.basic.is_commutative);
        panic!();
    }
}
