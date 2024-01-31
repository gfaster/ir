use crate::{regstate::PhysRegUse, reg::Immediate};
use std::{rc::{Rc, Weak}, cell::Cell, fmt::Debug};

use crate::{reg::{InstrArg, MachineReg, BlockId, Binding}, Id, vec_map::{VecMap, VecSet}, Val, attr::BindAttributes, ty::Type};

/// Properties of a instruction. Note that equality is only checked by pointer, which should be
/// fine since they should only ever be defined as constants.
pub struct MachineInstrProp {
    pub basic: BasicInstrProp,

    /// constant registers (implicitly) referenced this instruction (e.g. `EFLAGS`)
    pub ref_regs: &'static [(MachineReg, PhysRegUse)],

    /// how this instruction interacts with operands
    pub operand_use: &'static [PhysRegUse],

    /// Operand identity classes
    ///
    /// For example, take the following x86 instruction:
    /// ```x86asm
    /// add r1, r2
    /// // represented like:
    /// // r1 = add r1, r2
    /// ```
    /// This is represented as a 3-operand instruction, but the destination operand and the first
    /// source operand are in the same register. In this case, `op_eq_constraints` would be 
    /// `&[0, 0, 1]`.
    pub op_eq_constraints: &'static [u8],
}

impl std::cmp::PartialEq for MachineInstrProp {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}
impl std::cmp::Eq for MachineInstrProp {}

impl MachineInstrProp {
    pub const fn assert_makes_sense(&self) {
        let p = self;
        p.basic.assert_makes_sense();
        assert!(p.basic.total_ops() as usize == p.operand_use.len());
        assert!(p.basic.total_ops() as usize == p.op_eq_constraints.len());
    }

    pub const fn implicit_op_cnt(&self) -> usize {
        self.ref_regs.len()
    }
}

impl BasicInstrProp {
    pub const fn total_ops(&self) -> u8 {
        self.res_cnt + self.op_cnt
    }
    pub const fn assert_makes_sense(&self) {
        let p = self;
        assert!(!p.is_branch || p.is_terminator);
    }
}


pub enum ArgCnt {
    Unknown,
    Variable,
    Known(u8)
}

#[derive(Debug)]
pub enum ArgClass {
    /// list of defined bindings
    ArgList,
    /// Any binding / register
    Binding,
    /// Label binding + arguments
    Target,
}

/// Basic properties of an instruction.
///
/// `Eq` is implemented for these, but is done by ptr compare.
#[derive(Debug)]
pub struct BasicInstrProp {
    /// argument count, total number of operands needs to add `res_cnt`
    pub op_cnt: u8,

    /// output (defined registers) count. For now I want to force this to be 1.
    pub res_cnt: u8,

    /// assembly mnemonic
    pub mnemonic: &'static str,

    /// whether this instruction is a branch, implies `is_terminator`
    pub is_branch: bool,

    /// whether this instruction is a terminator, implied by `is_branch`
    pub is_terminator: bool,

    /// whether this instruction can start a block
    pub is_block_header: bool,

    /// if this the operands of this instruction can be safely reordered
    ///
    /// For example: integer addition is probably commutative, but subtraction isn't
    pub is_commutative: bool,

    /// if this instruction can be safely re-ordered with identical instructions. Any instruction
    /// that may cause an exception (eg division, function calls) will have this set. I may want to think about
    /// moving this to a function since live ranges and metadata could change this.
    pub has_side_effects: bool,

    pub may_read_memory: bool,
    pub may_write_memory: bool,

    /// can other instructions move past around it
    pub is_barrier: bool,

    /// Groups of operands that must be the same type.
    ///
    /// For example, take the following RISC-V instruction:
    /// ```riscv
    /// add r1, r2, r3
    /// ```
    /// then `operand_relative_type_constraints` would be `&[0, 0, 0]` since all operands have to
    /// be the same type.
    ///
    /// Also note that this only enforces matching types, and won't ensure two types are different
    pub operand_relative_type_constraints: &'static [u8],

    /// simulation of the instruction. Should be none if there are side effects.
    pub simulation: Option<fn(u64, u64) -> Option<u64>>
}

impl std::cmp::PartialEq for BasicInstrProp {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}
impl std::cmp::Eq for BasicInstrProp {}

#[derive(Clone, PartialEq, Eq)]
pub enum ArgList {
    None,
    Unary(InstrArg),
    Binary(InstrArg, InstrArg),
    Many(Vec<InstrArg>),
}

impl ArgList {
    fn new() -> Self {
        Self::None
    }
    #[must_use]
    fn len(&self) -> usize {
        match self {
            Self::None => 0,
            Self::Unary(_) => 1,
            Self::Binary(_, _) => 2,
            Self::Many(v) => v.len(),
        }
    }
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
    #[must_use]
    fn get(&self, idx: usize) -> Option<InstrArg> {
        match self {
            Self::None => None,
            Self::Unary(op1) => [*op1].get(idx).copied(),
            Self::Binary(op1, op2) => [*op1, *op2].get(idx).copied(),
            Self::Many(v) => v.get(idx).copied(),
        }
    }
    fn push(&mut self, bind: InstrArg) {
        match self {
            Self::None => *self = Self::Unary(bind),
            Self::Unary(op1) => *self = Self::Binary(*op1, bind),
            Self::Binary(op1, op2) => *self = Self::Many(vec![*op1, *op2, bind]),
            Self::Many(v) => v.push(bind),
        }
    }

    fn iter(&self) -> ArgListIter {
        ArgListIter { list: self, idx: 0 }
    }

    fn bindings(&self) -> impl Iterator<Item = Binding> + '_ {
        self.iter().filter_map(|a| a.as_binding())
    }
}

impl Debug for ArgList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.iter()).finish()
    }
}

impl std::fmt::Display for ArgList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut it = self.iter().peekable();
        while let Some(next) = it.next() {
            write!(f, "{next}")?;
            if it.peek().is_some() {
                write!(f, ", ")?;
            }
        }
        Ok(())
    }
}

impl std::ops::Index<usize> for ArgList {
    type Output = InstrArg;

    fn index(&self, index: usize) -> &Self::Output {
        match self {
            Self::None => None,
            Self::Unary(op1) if index == 0 => Some(op1),
            Self::Binary(op1, _) if index == 0 => Some(op1),
            Self::Binary(_, op2) if index == 1 => Some(op2),
            Self::Many(v) => v.get(index),
            _ => None,
        }.expect("index is out of bounds")
    }
}

impl<'a> From<&'a [InstrArg]> for ArgList {
    fn from(value: &'a [InstrArg]) -> Self {
        let mut ret = Self::new();
        for x in value {
            ret.push(*x);
        }
        ret
    }
}

impl<const L: usize> From<[InstrArg; L]> for ArgList {
    fn from(value: [InstrArg; L]) -> Self {
        match value.len() {
            0 => Self::None,
            1 => Self::Unary(value[0]),
            2 => Self::Binary(value[0], value[1]),
            _ => Self::Many(Vec::from(value))
        }
    }
}

impl FromIterator<InstrArg> for ArgList {
    fn from_iter<T: IntoIterator<Item = InstrArg>>(iter: T) -> Self {
        let mut ret = Self::new();
        for item in iter {
            ret.push(item)
        }
        ret
    }
}

impl<'a> FromIterator<&'a InstrArg> for ArgList {
    fn from_iter<T: IntoIterator<Item = &'a InstrArg>>(iter: T) -> Self {
        let mut ret = Self::new();
        for &item in iter {
            ret.push(item)
        }
        ret
    }
}

impl<'a> IntoIterator for &'a ArgList {
    type Item = InstrArg;

    type IntoIter = ArgListIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

pub struct ArgListIter<'a>{
    list: &'a ArgList,
    idx: usize,
}

impl<'a> Iterator for ArgListIter<'a> {
    type Item = InstrArg;

    fn next(&mut self) -> Option<Self::Item> {
        let ret = self.list.get(self.idx)?;
        self.idx += 1;
        Some(ret)
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum BindList {
    None,
    Unary(Binding),
    Binary(Binding, Binding),
    Many(Vec<Binding>),
}

impl BindList {
    fn new() -> Self {
        Self::None
    }
    #[must_use]
    fn len(&self) -> usize {
        match self {
            Self::None => 0,
            Self::Unary(_) => 1,
            Self::Binary(_, _) => 2,
            Self::Many(v) => v.len(),
        }
    }
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
    #[must_use]
    fn get(&self, idx: usize) -> Option<Binding> {
        match self {
            Self::None => None,
            Self::Unary(op1) => [*op1].get(idx).copied(),
            Self::Binary(op1, op2) => [*op1, *op2].get(idx).copied(),
            Self::Many(v) => v.get(idx).copied(),
        }
    }
    fn push(&mut self, bind: Binding) {
        match self {
            Self::None => *self = Self::Unary(bind),
            Self::Unary(op1) => *self = Self::Binary(*op1, bind),
            Self::Binary(op1, op2) => *self = Self::Many(vec![*op1, *op2, bind]),
            Self::Many(v) => v.push(bind),
        }
    }

    fn iter(&self) -> BindListIter {
        BindListIter { list: self, idx: 0 }
    }
}

impl Debug for BindList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.iter()).finish()
    }
}

impl std::fmt::Display for BindList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut it = self.iter().peekable();
        while let Some(next) = it.next() {
            write!(f, "{next}")?;
            if it.peek().is_some() {
                write!(f, ", ")?;
            }
        }
        Ok(())
    }
}


impl<'a> From<&'a [Binding]> for BindList {
    fn from(value: &'a [Binding]) -> Self {
        let mut ret = Self::new();
        for x in value {
            ret.push(*x);
        }
        ret
    }
}

impl<const L: usize> From<[Binding; L]> for BindList {
    fn from(value: [Binding; L]) -> Self {
        match value.len() {
            0 => Self::None,
            1 => Self::Unary(value[0]),
            2 => Self::Binary(value[0], value[1]),
            _ => Self::Many(Vec::from(value))
        }
    }
}

impl FromIterator<Binding> for BindList {
    fn from_iter<T: IntoIterator<Item = Binding>>(iter: T) -> Self {
        let mut ret = Self::new();
        for item in iter {
            ret.push(item)
        }
        ret
    }
}

impl<'a> FromIterator<&'a Binding> for BindList {
    fn from_iter<T: IntoIterator<Item = &'a Binding>>(iter: T) -> Self {
        let mut ret = Self::new();
        for &item in iter {
            ret.push(item)
        }
        ret
    }
}

impl<'a> IntoIterator for &'a BindList {
    type Item = Binding;

    type IntoIter = BindListIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

pub struct BindListIter<'a>{
    list: &'a BindList,
    idx: usize,
}

impl<'a> Iterator for BindListIter<'a> {
    type Item = Binding;

    fn next(&mut self) -> Option<Self::Item> {
        let ret = self.list.get(self.idx)?;
        self.idx += 1;
        Some(ret)
    }
}


#[derive(Debug, Clone, Copy)]
pub struct DebugInfo {
}

#[derive(Clone)]
pub struct MachineInstruction {
    /// the type of instruction this is, and all of its static properties
    props: &'static MachineInstrProp,
    operands: ArgList,
}

impl MachineInstruction {
    pub fn new(op: &'static MachineInstrProp) -> Self {
        Self {
            props: op,
            operands: ArgList::new(),
        }
    }

    pub fn phys_reg_use(&self, mach: MachineReg) -> Option<PhysRegUse> {
        self.phys_reg_use_iter().find(|(m, _)| *m == mach).map(|(m, u)| u)
    }

    pub fn phys_reg_use_iter(&self) -> impl Iterator<Item = (MachineReg, PhysRegUse)> + '_ {
        self.op_use_iter().chain(self.implicit_op_use_iter()).filter_map(|(b, u)| b.as_machine().map(|b| (b, u)))
    }

    fn op_use_iter(&self) -> impl Iterator<Item = (Binding, PhysRegUse)> + '_ {
        self.operands.bindings().zip(self.props.operand_use.iter().copied())
    }

    fn implicit_op_use_iter(&self) -> impl Iterator<Item = (Binding, PhysRegUse)> + '_ {
        self.props.ref_regs.iter().map(|&(r, u)| (r.into(), u))
    }

    pub fn read_registers(&self) -> impl Iterator<Item = Binding> + '_ {
        self.op_use_iter().chain(self.implicit_op_use_iter()).filter(|(_, u)| u.is_read()).map(|(r, _)| r)
    }

    pub fn def_registers(&self) -> impl Iterator<Item = Binding> + '_ {
        self.op_use_iter().chain(self.implicit_op_use_iter()).filter(|(_, u)| u.is_defined()).map(|(r, _)| r)
    }

    pub fn read_phys_regs(&self) -> impl Iterator<Item = MachineReg> + '_ {
        self.read_registers().filter_map(|r| r.as_machine())
    }
}

impl std::fmt::Debug for MachineInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Self as std::fmt::Display>::fmt(self, f)
    }
}

impl std::fmt::Display for MachineInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut it = self.def_registers().peekable();
        while let Some(r) = it.next() {
            write!(f, "%{r}")?;
            if it.peek().is_some() {
                write!(f, ", ")?;
            }
        }
        write!(f, " = {} ", self.props.basic.mnemonic)?;
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

#[derive(Debug, Clone)]
pub struct Target {
    pub id: BlockId,
    pub args: ArgList,
}

impl Target {
    fn read_registers(&self) -> impl Iterator<Item = Binding> + '_ {
        std::iter::once(self.id.into()).chain(self.args.bindings())
    }
}

impl std::fmt::Display for Target {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self { id, args } = self;
        write!(f, "label {id} ({args})")
    }
}

/// instruction id, currently not used
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct InstrId(usize);
impl InstrId {
    fn new() -> Self {
        static CNT: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
        InstrId(CNT.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
    }
}

/// an [`Instruction`] without the linked-list
#[derive(Debug, Clone)]
pub struct InstructionTemplate {
    pub dbg_info: Option<DebugInfo>,
    pub inner: OpInner,
}

impl std::ops::Deref for InstructionTemplate {
    type Target = OpInner;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl InstructionTemplate {
    /// for now, all IR instruction are binary operations with one output
    pub fn from_binary_op(res: Binding, op: impl AsRef<str>, ty: Type, lhs: InstrArg, rhs: InstrArg ) -> Option<Self> {
        let &prop = crate::ir::instruction_map().get(op.as_ref())?;
        let args: ArgList = [lhs, rhs].into();
        Some(InstructionTemplate {
            dbg_info: None,
            inner: OpInner::IrInstr { 
                attr: BindAttributes::new(ty),
                prop,
                res,
                args,
            }
        })
    }
}

impl From<InstructionTemplate> for Instruction {
    fn from(value: InstructionTemplate) -> Self {
        Self {
            // id: InstructionId::new(),
            dbg_info: value.dbg_info,
            inner: value.inner,
        }
    }
}


#[derive(Debug, Clone)]
pub struct Instruction {
    /// attributes of defined binding
    dbg_info: Option<DebugInfo>,
    inner: OpInner,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AllocationType {
    Stack,
    Heap
}

#[derive(Debug, Clone)]
pub enum OpInner {
    /// treated as a binary instruction (target, arguments)
    Call {
        id: Id,
        args: ArgList,
    },
    Alloc {
        attr: BindAttributes,
        loc: Binding,
        ty: AllocationType
    },
    /// direct assignment of a literal, or just another binding
    Assign {
        attr: BindAttributes,
        loc: Binding,
        val: Val
    },
    Load {
        attr: BindAttributes,
        loc: Binding,
        ptr: InstrArg,
    },
    Block {
        id: BlockId,
        args: BindList,
    },
    Br {
        check: InstrArg,
        success: Target,
        fail: Target,
    },
    MachInstr {
        attr: BindAttributes,
        ins: MachineInstruction,
    },
    IrInstr {
        attr: BindAttributes,
        prop: &'static BasicInstrProp,
        args: ArgList,
        res: Binding
    },
    Jmp {
        target: Target,
    },
    Store {
        dst: InstrArg,
        val: InstrArg,
    },
    Return {
        val: InstrArg
    }
}

impl Instruction {
    const fn basic_props(&self) -> &'static BasicInstrProp {
        // conservative instruction properties
        const TEMPLATE: BasicInstrProp = BasicInstrProp {
            op_cnt: 0,
            res_cnt: 1,
            mnemonic: "[[TEMPLATE]]",
            is_branch: false,
            is_terminator: false,
            is_block_header: false,
            is_commutative: false,
            has_side_effects: true,
            may_read_memory: true,
            may_write_memory: true,
            is_barrier: false,
            operand_relative_type_constraints: &[0],
            simulation: None,
        };
        match &self.inner {
            OpInner::Call { .. } => &BasicInstrProp { 
                op_cnt: 1,
                mnemonic: "call",
                is_branch: false, // TODO: stop lying
                is_terminator: false, // TODO: stop lying
                operand_relative_type_constraints: &[0, 1],
                is_barrier: true,
                ..TEMPLATE
            },
            OpInner::Load { .. } => &BasicInstrProp {
                op_cnt: 1,
                mnemonic: "load",
                has_side_effects: false,
                may_read_memory: true,
                operand_relative_type_constraints: &[0, 1],
                ..TEMPLATE
            },
            OpInner::Block { .. } => &BasicInstrProp {
                op_cnt: 1,
                res_cnt: 0,
                mnemonic: "label",
                is_block_header: true,
                has_side_effects: false,
                may_read_memory: false,
                may_write_memory: false,
                operand_relative_type_constraints: &[0, 1],
                is_barrier: true,
                ..TEMPLATE
            },
            OpInner::Br { .. } => &BasicInstrProp {
                op_cnt: 3,
                res_cnt: 0,
                mnemonic: "br",
                is_branch: true,
                is_terminator: true,
                is_commutative: false,
                has_side_effects: false,
                may_read_memory: false,
                may_write_memory: false,
                operand_relative_type_constraints: &[0, 1, 1],
                is_barrier: true,
                ..TEMPLATE
            },
            OpInner::MachInstr { ins, .. } => &ins.props.basic,
            OpInner::Jmp { .. } => &BasicInstrProp{
                op_cnt: 1,
                res_cnt: 0,
                mnemonic: "br",
                is_branch: true,
                is_terminator: true,
                is_commutative: false,
                has_side_effects: false,
                may_read_memory: false,
                may_write_memory: false,
                is_barrier: true,
                ..TEMPLATE
            },
            OpInner::Store { .. } => &BasicInstrProp {
                op_cnt: 1,
                mnemonic: "store",
                has_side_effects: false,
                may_read_memory: false,
                may_write_memory: true,
                operand_relative_type_constraints: &[0, 1],
                ..TEMPLATE
            },
            OpInner::Assign { .. } => &BasicInstrProp {
                op_cnt: 1,
                res_cnt: 1,
                mnemonic: "br",
                is_commutative: false,
                has_side_effects: false,
                may_read_memory: false,
                may_write_memory: false,
                operand_relative_type_constraints: &[0, 0],
                ..TEMPLATE
            },
            OpInner::IrInstr { prop, .. } => prop,
            OpInner::Alloc { .. } => &BasicInstrProp {
                op_cnt: 1,
                res_cnt: 1,
                mnemonic: "br",
                is_commutative: false,
                has_side_effects: false,
                may_read_memory: false,
                may_write_memory: true,
                operand_relative_type_constraints: &[0, 1],
                ..TEMPLATE
            },
            OpInner::Return { .. } => &BasicInstrProp {
                op_cnt: 1,
                res_cnt: 0,
                mnemonic: "ret",
                is_branch: false,
                is_terminator: true,
                is_block_header: false,
                has_side_effects: true,
                may_read_memory: false,
                may_write_memory: false,
                operand_relative_type_constraints: &[0],
                is_barrier: true,
                ..TEMPLATE
            },
        }
    }

    pub fn is_term(&self) -> bool {
        self.basic_props().is_terminator
    }

    pub fn is_branch(&self) -> bool {
        self.basic_props().is_branch
    }

    pub fn is_block_header(&self) -> bool {
        self.basic_props().is_block_header
    }

    pub fn mnemonic(&self) -> &'static str {
        self.basic_props().mnemonic
    }

    pub fn sim(&self) -> Option<fn(u64, u64) -> Option<u64>> {
        self.basic_props().simulation
    }

    pub fn is_barrier(&self) -> bool {
        self.basic_props().is_barrier
    }

    pub fn is_movable(&self) -> bool {
        let props = self.basic_props();
        !props.has_side_effects && !props.is_block_header && !props.is_branch && !props.is_terminator && !props.is_barrier
    }

    pub fn has_side_effects(&self) -> bool {
        self.basic_props().has_side_effects
    }

    pub fn as_block_header_id(&self) -> Option<BlockId> {
        self.inner.as_block_id()
    }

    pub fn make_assignment(ty: Type, bind: Binding, val: InstrArg) -> Self {
        Self {
            dbg_info: None,
            inner: OpInner::Assign { attr: BindAttributes::new(ty), loc: bind, val: Val::Binding(val) },
        }
    }

    /// gets the simple case of a constant assignment.
    ///
    /// e.g. `%i = 5`
    pub fn as_const_assignment(&self) -> Option<Immediate> {
        let OpInner::Assign { val, .. } = self.inner else { return None };
        let Val::Binding(val) = val else { return None };
        val.as_imm()
    }

    pub fn get_def(&self, idx: usize) -> Option<Binding> {
        if idx != 0 {
            return None;
        }
        match &self.inner {
            OpInner::Call { id, args } => None,
            OpInner::Alloc { attr, loc, ty } => Some(*loc),
            OpInner::Assign { loc, .. } => Some(*loc),
            OpInner::Load { attr, loc, ptr } => Some(*loc),
            OpInner::Block { id, args } => Some(id.into()),
            OpInner::Br { check, success, fail } => None,
            OpInner::MachInstr { attr, ins } => ins.def_registers().nth(idx),
            OpInner::IrInstr { attr, prop, args, res } => Some(*res),
            OpInner::Jmp { target } => None,
            OpInner::Store { dst, val } => None,
            OpInner::Return { val } => None,
        }
    }

    pub fn defined_bindings(&self) -> impl Iterator<Item = Binding> + '_ {
        (0..).flat_map(|i| self.get_def(i))
    }

    /// returns self as a binary expression, if applicable
    pub fn as_binary(&self) -> Option<(Binding, InstrArg, InstrArg)> {
        let OpInner::IrInstr {res, args, ..} = &self.inner else { return None };
        Some((*res, args[0], args[1]))
    }

    pub fn read_bindings(&self) -> impl Iterator<Item = Binding> + '_ {
        (0..).flat_map(|i| self.get_arg(i)).flat_map(|a| a.as_binding())
    }

    /// get the input arg at idx
    pub fn get_arg(&self, idx: usize) -> Option<InstrArg> {
        match &self.inner {
            OpInner::Call { id, args } => None,
            OpInner::Alloc { attr, loc, ty } => [InstrArg::from(loc)].get(idx).copied(),
            OpInner::Assign { val, .. } => [val.as_arg()].get(idx).copied(),
            OpInner::Load { attr, loc, ptr } => [*ptr].get(idx).copied(),
            OpInner::Block { id, args } => None,
            OpInner::Br { check, success, fail } if idx == 0 => Some(*check),
            OpInner::Br { check, success, fail } if idx == 1 => Some(success.id.into()),
            OpInner::Br { check, success, fail } if idx < 2 + success.args.len() => Some(success.args[idx - 2]),
            OpInner::Br { check, success, fail } if idx == 2 + success.args.len() => Some(fail.id.into()),
            OpInner::Br { check, success, fail } => fail.args.get(idx - success.args.len() - 3),
            OpInner::MachInstr { attr, ins } => ins.op_use_iter().filter(|(_, u)| u.is_read()).map(|(i, _)| i.into()).nth(idx),
            OpInner::IrInstr { attr, prop, args, res } => args.get(idx),
            OpInner::Jmp { target } if idx == 0 => Some(target.id.into()),
            OpInner::Jmp { target } => target.args.get(idx - 1),
            OpInner::Store { dst, val } => [*dst, *val].get(idx).copied(),
            OpInner::Return { val } => [*val].get(idx).copied(),
        }
    }

    pub fn get_attr(&self) -> Option<&BindAttributes> {
        match &self.inner {
            OpInner::Call { .. } => None,
            OpInner::Alloc { attr, .. } => Some(attr),
            OpInner::Assign { attr, .. } => Some(attr),
            OpInner::Load { attr, .. } => Some(attr),
            OpInner::Block { .. } => None,
            OpInner::Br { .. } => None,
            OpInner::MachInstr { attr, .. } => Some(attr),
            OpInner::IrInstr { attr, .. } => Some(attr),
            OpInner::Jmp { .. } => None,
            OpInner::Store { .. } => None,
            OpInner::Return { .. } => None,
        }
    }

    pub fn arg_iter(&self) -> impl Iterator<Item = InstrArg> + '_ {
        (0..).flat_map(|i| self.get_arg(i))
    }

    pub fn jump_dsts(&self) -> impl Iterator<Item = Binding> + '_ {
        let ret: Box<dyn Iterator<Item = Binding>> = match &self.inner {
            OpInner::Jmp { target } => Box::new([target.id].into_iter().map(Into::into)),
            OpInner::Br { success, fail, ..  } => Box::new([success.id, fail.id].into_iter().map(Into::into)),
            _ => Box::new([].into_iter())
        };
        ret
    }

    /// whether two instructions (assumed to be adjacent) may be swapped. 
    ///
    /// Note that this is not necessarily transitive.
    pub fn may_commute(fst: &Self, snd: &Self) -> bool {
        (!fst.has_side_effects() || !snd.has_side_effects()) &&
        (!fst.is_block_header() && !snd.is_block_header()) &&
        (!fst.is_term() && !snd.is_term()) && {
            todo!("check usedef chain")
        }
    }

}

impl OpInner {
    fn as_mach_instr(&self) -> Option<&MachineInstruction> {
        if let Self::MachInstr { ins, .. } = self {
            Some(ins)
        } else {
            None
        }
    }

    fn as_block_id(&self) -> Option<BlockId> {
        if let Self::Block { id, .. } = self {
            Some(*id)
        } else {
            None
        }
    }
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.inner {
            OpInner::Call { id, args } => write!(f, "call @{id} ({args})"),
            OpInner::Alloc { loc, ty, attr } => write!(f, "{loc} = alloca"),
            OpInner::Assign { loc, val, .. } => write!(f, "{loc} = {val}"),
            OpInner::Load { loc, ptr, attr } => write!(f, "{loc} = load ptr {ptr}"),
            OpInner::Block { id, args } => write!(f, "label {id} ({args}):"),
            OpInner::Br { check, success, fail } => write!(f, "br {check}, {success}, {fail}"),
            OpInner::MachInstr { ins, attr } => write!(f, "{ins}"),
            OpInner::IrInstr { prop, args, res, attr } => write!(f, "{res} = {prop} {args}", prop = prop.mnemonic),
            OpInner::Jmp { target } => write!(f, "br {target}"),
            OpInner::Store { dst, val } => write!(f, "store {dst}, {val}"),
            OpInner::Return { val } => write!(f, "ret {val}"),
        }
    }
}
