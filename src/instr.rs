use crate::reg::{Binding, MachineReg};


/// properties of a instruction
pub struct InstrProp {
    pub op_cnt: u8,
    /// assembly mnemonic
    pub mnemonic: &'static str,
    pub is_branch: bool,
    pub is_terminator: bool,

    /// if this instruction can be safely re-ordered with identical instructions
    pub is_commutative: bool,

    /// constant registers written to by this instruction (e.g. `EFLAGS`)
    ///
    /// corresponds 1:1 to the first operands in IR representation
    pub set_regs: &'static [MachineReg],

    /// constant registers read by this instruction (e.g. `EFLAGS`)
    ///
    /// is mutually exclusive with read operands
    pub read_regs: &'static [MachineReg],

    /// Operand indices written to by this instruction
    ///
    /// For example, take the following `RISC-V` instruction:
    /// ```riscv
    /// add r1, r2, r3
    /// ```
    /// then `set_operands` would be `&[0]` since only the first register is changed
    pub set_operands: &'static [u8],

    /// operand indices read by this instruction
    ///
    /// For example, take the following RISC-V instruction:
    /// ```riscv
    /// add r1, r2, r3
    /// ```
    /// then `read_operands` would be `&[1, 2]` since only the 2nd and 3rd operands are read.
    pub read_operands: &'static [u8],

    /// Groups of operands that must be the same type.
    ///
    /// For example, take the following RISC-V instruction:
    /// ```riscv
    /// add r1, r2, r3
    /// ```
    /// then `operand_relative_type_constraints` would be `&[0, 0, 0]` since all operands have to
    /// be the same type.
    pub operand_relative_type_constraints: &'static [u8],
}

impl InstrProp {
    pub const fn assert_makes_sense(&self) {
        let p = self;
        assert!(p.op_cnt as usize >= p.set_regs.len());
        assert!(p.op_cnt as usize >= p.read_operands.len());
        assert!(p.op_cnt as usize >= p.set_operands.len());
        assert!(p.op_cnt as usize == p.operand_relative_type_constraints.len());
        assert!(!p.is_branch || !p.is_commutative);
        assert!(!p.is_terminator || !p.is_commutative);
        assert!(!p.is_branch || p.is_terminator);
    }
}

#[derive(Debug, Clone, Copy)]
pub struct DebugInfo {
}

pub struct Instruction {
    /// the type of instruction this is, and all of its static properties
    op: &'static InstrProp,
    operands: Vec<Binding>,
    dbg_info: Option<DebugInfo>,
}

impl std::ops::Deref for Instruction {
    type Target = &'static InstrProp;

    fn deref(&self) -> &Self::Target {
        &self.op
    }
}

impl Instruction {
    pub fn new(op: &'static InstrProp) -> Self {
        Self {
            op,
            operands: Vec::new(),
            dbg_info: None,
        }
    }

    pub fn push_op(&mut self, op: Binding) -> &mut Self {
        assert!(self.operands.len() < self.op.op_cnt as usize);
        self.operands.push(op);
        self
    }

    pub fn push_ops(&mut self, op: impl IntoIterator<Item = Binding>) -> &mut Self {
        self.operands.extend(op);
        assert!(self.operands.len() <= self.op.op_cnt as usize);
        self
    }

    pub fn is_valid(&self) -> bool {
        self.operands.len() == self.op.op_cnt as usize
    }

    pub fn read_registers(&self) -> impl Iterator<Item = Binding> + '_ {
        debug_assert!(self.is_valid());
        self.op.read_operands.iter().map(|&r| self.operands[r as usize])
            .chain(self.op.read_regs.iter().map(|&r| r.into()))
    }

    pub fn set_registers(&self) -> impl Iterator<Item = Binding> + '_ {
        debug_assert!(self.is_valid());
        self.op.set_operands.iter().map(|&r| self.operands[r as usize])
            .chain(self.op.set_regs.iter().map(|&r| r.into()))
    }

    pub fn read_phys_regs(&self) -> impl Iterator<Item = MachineReg> + '_ {
        self.read_registers().filter_map(|r| r.as_machine().copied())
    }
}

impl std::fmt::Debug for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Self as std::fmt::Display>::fmt(self, f)
    }
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut it = self.set_registers().peekable();
        while let Some(r) = it.next() {
            write!(f, "%{r}")?;
            if it.peek().is_some() {
                write!(f, ", ")?;
            }
        }
        write!(f, " = {} ", self.op.mnemonic)?;
        let mut it = self.read_registers().peekable();
        while let Some(r) = it.next() {
            write!(f, "%{r}")?;
            if it.peek().is_some() {
                write!(f, ", ")?;
            }
        }
        Ok(())
    }
}
