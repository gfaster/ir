use crate::{
    arch::RegBankInfo,
    reg::{MachineReg, RegBank},
};

macro_rules! machine_regs {
    (@decl $($name:ident: $val:literal);*) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        #[repr(u16)]
        pub enum Reg { $($name = $val),*}
    };
    (@name $($name:ident: $mne:literal);*) => {
        pub const fn name(self) -> &'static str {
            match self {
                $(Self::$name => $mne),*
            }
        }
    };
    (@from_idx $($name:ident: $val:literal);*) => {
        pub const fn from_idx(loc: u16) -> Self {
            match loc {
                $($val => Self::$name,)*
                _ => panic!("invalid register idx")
            }
        }
    };
    (@from_name $($name:ident: $mne:literal);*) => {
        pub fn from_name(name: &str) -> Self {
            match name {
                $($mne => Self::$name,)*
                _ => panic!("invalid register name {name:?}")
            }
        }
    };
    ($($name:ident: $mne:literal, $val:literal);*$(;)?) => {
        machine_regs!(@decl $($name: $val);*);
        impl Reg {
            machine_regs!(@name $($name: $mne);*);
            machine_regs!(@from_idx $($name: $val);*);
            machine_regs!(@from_name $($name: $mne);*);
            pub const fn idx(self) -> u16 {
                self as u16
            }
        }
    };
}
machine_regs! {
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
    pub const fn bank(&self) -> RegBank {
        match self {
            Reg::Eflags => RegBank(1),
            _ => RegBank(0),
        }
    }

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

    pub const fn can_mov_to(&self, other: &Self) -> bool {
        self.bank().0 == other.bank().0
    }

    /// whether this register can be used as a mov src
    pub const fn can_mov_from(&self) -> bool {
        match self {
            Self::Eflags => false,
            _ => true,
        }
    }

    pub const fn may_spill(&self) -> bool {
        self.can_mov_from()
    }

    pub const fn to_mach(&self) -> MachineReg {
        MachineReg::from_idx(*self as u16)
    }

    pub const fn from_mach(mr: MachineReg) -> Self {
        Self::from_idx(mr.idx())
    }
}

pub const REG_BANK_INFO: [RegBankInfo; 2] = [
    RegBankInfo {
        transfer_cost: &[0, u16::MAX],
        name: "gpr",
    },
    RegBankInfo {
        transfer_cost: &[0, u16::MAX],
        name: "flags",
    },
];

