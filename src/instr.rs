use std::{rc::{Rc, Weak}, cell::Cell, fmt::Debug};

use crate::{reg::{InstrArg, MachineReg, BlockId, SSAState, PhysRegUse, Binding}, Id, vec_map::{VecMap, VecSet}, Val};

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
    pub fn from_binary_op(res: Binding, op: impl AsRef<str>, lhs: InstrArg, rhs: InstrArg ) -> Option<Self> {
        let &prop = crate::ir::instruction_map().get(op.as_ref())?;
        let args: ArgList = [lhs, rhs].into();
        Some(InstructionTemplate {
            dbg_info: None,
            inner: OpInner::IrInstr { 
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

/// Universally unique instruction id. Used for equality of instructions.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct InstructionId(usize);

impl InstructionId {
    fn new() -> Self {
        use std::sync::atomic;
        static CNT: atomic::AtomicUsize = atomic::AtomicUsize::new(0);
        Self(CNT.fetch_add(1, atomic::Ordering::Relaxed))
    }
}

pub struct Instruction {
    // id: InstructionId,
    dbg_info: Option<DebugInfo>,
    inner: OpInner,
}

// impl std::cmp::PartialEq for Instruction {
//     fn eq(&self, other: &Self) -> bool {
//         self.id == other.id
//     }
// }
//
// impl std::cmp::Eq for Instruction {}

impl std::fmt::Debug for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct(std::any::type_name::<Self>())
            .field("dbg_info", &self.dbg_info)
            .field("inner", &self.inner)
            .finish()
    }
}

impl Clone for Instruction {
    fn clone(&self) -> Self {
        Instruction { 
            // id: InstructionId::new(),
            dbg_info: self.dbg_info.clone(), 
            inner: self.inner.clone()
        }
    }
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
        loc: Binding,
        ty: AllocationType
    },
    /// direct assignment of a literal, or just another binding
    Assign {
        loc: Binding,
        val: Val
    },
    Load {
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
        ins: MachineInstruction,
    },
    IrInstr {
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
    fn basic_props(&self) -> &'static BasicInstrProp {
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
            operand_relative_type_constraints: &[0],
        };
        match &self.inner {
            OpInner::Call { id, args } => &BasicInstrProp { 
                op_cnt: 1,
                mnemonic: "call",
                is_branch: false, // TODO: stop lying
                is_terminator: false, // TODO: stop lying
                operand_relative_type_constraints: &[0, 1],
                ..TEMPLATE
            },
            OpInner::Load { loc, ptr } => &BasicInstrProp {
                op_cnt: 1,
                mnemonic: "load",
                has_side_effects: false,
                may_read_memory: true,
                operand_relative_type_constraints: &[0, 1],
                ..TEMPLATE
            },
            OpInner::Block { id, args } => &BasicInstrProp {
                op_cnt: 1,
                res_cnt: 0,
                mnemonic: "label",
                is_block_header: true,
                has_side_effects: false,
                may_read_memory: false,
                may_write_memory: false,
                operand_relative_type_constraints: &[0, 1],
                ..TEMPLATE
            },
            OpInner::Br { check, success, fail } => &BasicInstrProp {
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
                ..TEMPLATE
            },
            OpInner::MachInstr { ins } => &ins.props.basic,
            OpInner::Jmp { target } => &BasicInstrProp{
                op_cnt: 1,
                res_cnt: 0,
                mnemonic: "br",
                is_branch: true,
                is_terminator: true,
                is_commutative: false,
                has_side_effects: false,
                may_read_memory: false,
                may_write_memory: false,
                ..TEMPLATE
            },
            OpInner::Store { dst, val: src } => &BasicInstrProp {
                op_cnt: 1,
                mnemonic: "store",
                has_side_effects: false,
                may_read_memory: false,
                may_write_memory: true,
                operand_relative_type_constraints: &[0, 1],
                ..TEMPLATE
            },
            OpInner::Assign { loc, val } => &BasicInstrProp {
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
            OpInner::Alloc { loc, ty } => &BasicInstrProp {
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
            OpInner::Return { val } => &BasicInstrProp {
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

    pub fn has_side_effects(&self) -> bool {
        self.basic_props().has_side_effects
    }

    pub fn as_block_header_id(&self) -> Option<BlockId> {
        self.inner.as_block_id()
    }

    pub fn defined_bindings(&self) -> impl Iterator<Item = Binding> + '_ {
        let ret: Box<dyn Iterator<Item = Binding>> = match &self.inner {
            OpInner::Call { id, args } => Box::new([].into_iter()),
            OpInner::Load { loc, ptr } => Box::new([*loc].into_iter()),
            OpInner::Block { id, args } => Box::new([(*id).into()].into_iter().chain(args.iter())),
            OpInner::Br { check, success, fail } => Box::new([].into_iter()),
            OpInner::MachInstr { ins } => Box::new(ins.def_registers()),
            OpInner::Jmp { target } => Box::new([].into_iter()),
            OpInner::Store { dst, val: src } => Box::new([].into_iter()),
            OpInner::Assign { loc, .. } => Box::new([*loc].into_iter()),
            OpInner::IrInstr { prop, res, args } => {
                assert_eq!(prop.op_cnt, 2);
                assert_eq!(prop.res_cnt, 1);
                assert_eq!(args.len(), 2, "incomplete defintion");
                Box::new([*res].into_iter())
            },
            OpInner::Alloc { loc, ty } =>  Box::new([*loc].into_iter()),
            OpInner::Return { val } => Box::new([].into_iter()),
        };
        ret
    }

    pub fn read_bindings(&self) -> impl Iterator<Item = Binding> + '_ {
        let ret: Box<dyn Iterator<Item = Binding>> = match &self.inner {
            OpInner::Call { id, args } => Box::new(args.bindings()),
            OpInner::Load { loc, ptr } => Box::new(ptr.as_binding().into_iter()),
            OpInner::Block { id, args } => Box::new([].into_iter()),
            OpInner::Br { check, success, fail } => 
                Box::new(check.as_binding().into_iter()
                    .chain(success.read_registers())
                    .chain(fail.read_registers())),
            OpInner::MachInstr { ins } => Box::new(ins.read_registers()),
            OpInner::Jmp { target } => Box::new(target.read_registers()),
            OpInner::Store { dst, val: src } => Box::new([*dst, *src].into_iter().filter_map(|a| a.as_binding())),
            OpInner::Assign { val, .. } => Box::new(val.as_binding().into_iter()),
            OpInner::IrInstr { prop, args, res } => {
                assert_eq!(prop.op_cnt, 2);
                assert_eq!(prop.res_cnt, 1);
                assert_eq!(args.len(), 2, "incomplete defintion");
                Box::new([args.get(1), args.get(2)].into_iter().flatten().filter_map(|a| a.as_binding()))
            },
            OpInner::Alloc { .. } => Box::new([].into_iter()),
            OpInner::Return { val } => Box::new(val.as_binding().into_iter()),
        };
        ret
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
        if let Self::MachInstr { ins } = self {
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
            OpInner::Alloc { loc, ty } => write!(f, "{loc} = alloca"),
            OpInner::Assign { loc, val } => write!(f, "{loc} = {val}"),
            OpInner::Load { loc, ptr } => write!(f, "{loc} = load ptr {ptr}"),
            OpInner::Block { id, args } => write!(f, "label {id} ({args}):"),
            OpInner::Br { check, success, fail } => write!(f, "br {check}, {success}, {fail}"),
            OpInner::MachInstr { ins } => write!(f, "{ins}"),
            OpInner::IrInstr { prop, args, res } => write!(f, "{res} = {prop} {args}", prop = prop.mnemonic),
            OpInner::Jmp { target } => write!(f, "br {target}"),
            OpInner::Store { dst, val } => write!(f, "store {dst}, {val}"),
            OpInner::Return { val } => write!(f, "ret {val}"),
        }
    }
}
