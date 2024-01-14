#![allow(non_camel_case_types)]
use crate::instr::{MachineInstrProp, BasicInstrProp, ArgCnt};

pub mod r {
    use crate::reg::MachineReg;

    macro_rules! machine_regs {
        ($($name:ident: $mne:literal, $val:literal);*$(;)?) => {
            #[derive(Debug, Clone, Copy, PartialEq, Eq)]
            #[repr(u16)]
            pub enum Reg { $($name = $val),*}
            impl Reg {
                pub const fn from_idx(loc: u16) -> Self {
                    match loc {
                        $($val => Self::$name,)*
                        _ => panic!("invalid register idx")
                    }
                }
                pub const fn idx(self) -> u16 {
                    self as u16
                }
                pub const fn name(self) -> &'static str {
                    match self {
                        $(Self::$name => $mne),*
                    }
                }
            }
        };
    }
    machine_regs!{
        Rax: "rax", 0;
        Rcx: "rcx", 1;
        Rdx: "rdx", 2;
        Rsi: "rsi", 3;
        Rdi: "rdi", 4;
        Rsp: "rsp", 5;
        Rbp: "rbp", 6;
        R8:  "r8",  7;
        R9:  "r9",  8;
        R10: "r10", 9;
        R11: "r11", 10;
        R12: "r12", 11;
        R13: "r13", 12;
        R14: "r14", 13;
        R15: "r15", 14;
        Eflags: "[[EFLAGS]]", 15;
    }

    impl Reg {
        pub const fn gprs() -> &'static [Self] {
            &[
                Self::Rax,
                Self::Rcx,
                Self::Rdx,
                Self::Rsi,
                Self::Rdi,
                Self::R8,
                Self::R9,
                Self::R10,
                Self::R11,
                Self::R12,
                Self::R13,
                Self::R14,
                Self::R15,
            ]
        }

        pub const fn to_mach(self) -> MachineReg {
            MachineReg::from_idx(self as u16)
        }

        pub const fn from_mach(mr: MachineReg) -> Self {
            Self::from_idx(mr.idx())
        }
    }
}

const BASIC_TEMPLATE: BasicInstrProp = BasicInstrProp {
    arg_cnt: ArgCnt::Unknown,
    arg_classes: None,
    res_cnt: 0,
    mnemonic: "[TEMPLATE]",
    is_branch: false,
    is_terminator: false,
    is_block_header: false,
    is_commutative: false,
    has_side_effects: true,
    may_read_memory: true,
    may_write_memory: true,
};

const OP_RR_R: MachineInstrProp = MachineInstrProp {
    basic: BasicInstrProp {
        arg_cnt: ArgCnt::Known(2),
        res_cnt: 1,
        is_branch: false,
        is_terminator: false,
        is_commutative: false,
        ..BASIC_TEMPLATE
    },
    set_regs: &[],
    read_regs: &[],
    set_operands: &[0],
    read_operands: &[1, 2],
    operand_relative_type_constraints: &[0, 0, 0],
};

const BIN_RR_F: MachineInstrProp = MachineInstrProp {
    basic: BasicInstrProp {
        arg_cnt: ArgCnt::Known(2),
        res_cnt: 1,
        is_branch: false,
        is_terminator: false,
        is_commutative: false,
        ..BASIC_TEMPLATE
    },
    set_regs: &[r::Reg::Eflags.to_mach()],
    read_regs: &[],
    set_operands: &[0],
    read_operands: &[1, 2],
    operand_relative_type_constraints: &[0, 1, 1],
};

const BIN_RR_FR: MachineInstrProp = MachineInstrProp {
    basic: BasicInstrProp {
        arg_cnt: ArgCnt::Known(2),
        res_cnt: 2,
        is_branch: false,
        is_terminator: false,
        is_commutative: false,
        ..BASIC_TEMPLATE
    },
    set_regs: &[r::Reg::Eflags.to_mach()],
    read_regs: &[],
    set_operands: &[0, 1],
    read_operands: &[2, 3],
    operand_relative_type_constraints: &[0, 1, 1, 1],
};
