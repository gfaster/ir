use std::{rc::{Rc, Weak}, cell::Cell};

use crate::{reg::{Binding, MachineReg, BlockId, SSAState, PhysRegUse}, Id, vec_map::{VecMap, VecSet}, Val};

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InstrInputs {
    None,
    Unary(Binding),
    Binary(Binding, Binding),
    Many(Vec<Binding>),
}

impl InstrInputs {
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

    fn iter(&self) -> InstrInputsIter {
        InstrInputsIter { list: self, idx: 0 }
    }
}

impl<'a> From<&'a [Binding]> for InstrInputs {
    fn from(value: &'a [Binding]) -> Self {
        let mut ret = Self::new();
        for x in value {
            ret.push(*x);
        }
        ret
    }
}

impl<const L: usize> From<[Binding; L]> for InstrInputs {
    fn from(value: [Binding; L]) -> Self {
        match value.len() {
            0 => Self::None,
            1 => Self::Unary(value[0]),
            2 => Self::Binary(value[0], value[1]),
            _ => Self::Many(Vec::from(value))
        }
    }
}

impl FromIterator<Binding> for InstrInputs {
    fn from_iter<T: IntoIterator<Item = Binding>>(iter: T) -> Self {
        let mut ret = Self::new();
        for item in iter {
            ret.push(item)
        }
        ret
    }
}

impl<'a> FromIterator<&'a Binding> for InstrInputs {
    fn from_iter<T: IntoIterator<Item = &'a Binding>>(iter: T) -> Self {
        let mut ret = Self::new();
        for &item in iter {
            ret.push(item)
        }
        ret
    }
}

impl<'a> IntoIterator for &'a InstrInputs {
    type Item = Binding;

    type IntoIter = InstrInputsIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

struct InstrInputsIter<'a>{
    list: &'a InstrInputs,
    idx: usize,
}

impl<'a> Iterator for InstrInputsIter<'a> {
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
    operands: InstrInputs,
}

impl MachineInstruction {
    pub fn new(op: &'static MachineInstrProp) -> Self {
        Self {
            props: op,
            operands: InstrInputs::new(),
        }
    }

    fn op_use_iter(&self) -> impl Iterator<Item = (Binding, PhysRegUse)> + '_ {
        self.operands.iter().zip(self.props.operand_use.iter().copied())
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
        self.read_registers().filter_map(|r| r.as_machine().copied())
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
    pub id: Binding,
    pub args: InstrInputs,
}

impl Target {
    fn read_registers(&self) -> impl Iterator<Item = Binding> + '_ {
        std::iter::once(self.id).chain(&self.args)
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

impl InstructionTemplate {
    /// for now, all IR instruction are binary operations with one output
    pub fn from_binary_op(res: Binding, op: impl AsRef<str>, lhs: Binding, rhs: Binding ) -> Option<Self> {
        let &prop = crate::ir::instruction_map().get(op.as_ref())?;
        let args: InstrInputs = [res, lhs, rhs].into();
        Some(InstructionTemplate {
            dbg_info: None,
            inner: OpInner::IrInstr { 
                prop,
                ops: args
            }
        })
    }
}

impl From<InstructionTemplate> for Instruction {
    fn from(value: InstructionTemplate) -> Self {
        Self {
            id: InstructionId::new(),
            next: Cell::new(None),
            prev: Cell::new(None),
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
    id: InstructionId,
    next: Cell<Option<Rc<Instruction>>>,
    prev: Cell<Option<Weak<Instruction>>>,
    dbg_info: Option<DebugInfo>,
    inner: OpInner,
}

impl std::cmp::PartialEq for Instruction {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl std::cmp::Eq for Instruction {}

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
            id: InstructionId::new(),
            next: Cell::new(None), 
            prev: Cell::new(None), 
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
        args: InstrInputs,
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
        ptr: Binding,
    },
    Block {
        id: BlockId,
        args: InstrInputs,
    },
    Br {
        check: Binding,
        success: Target,
        fail: Target,
    },
    MachInstr {
        ins: MachineInstruction,
    },
    IrInstr {
        prop: &'static BasicInstrProp,
        ops: InstrInputs
    },
    Jmp {
        target: Target,
    },
    Store {
        dst: Binding,
        src: Binding,
    },
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
        match self.inner {
            OpInner::Call { id, args } => &BasicInstrProp { 
                op_cnt: 1,
                mnemonic: "call",
                is_branch: true,
                is_terminator: true,
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
            OpInner::Store { dst, src } => &BasicInstrProp {
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

    pub fn defined_bindings(&self) -> impl Iterator<Item = Binding> + '_ {
        let ret: Box<dyn Iterator<Item = Binding>> = match &self.inner {
            OpInner::Call { id, args } => Box::new([].into_iter()),
            OpInner::Load { loc, ptr } => Box::new([*loc].into_iter()),
            OpInner::Block { id, args } => Box::new([(*id).into()].into_iter().chain(args.iter())),
            OpInner::Br { check, success, fail } => Box::new([].into_iter()),
            OpInner::MachInstr { ins } => Box::new(ins.def_registers()),
            OpInner::Jmp { target } => Box::new([].into_iter()),
            OpInner::Store { dst, src } => Box::new([].into_iter()),
            OpInner::Assign { loc, .. } => Box::new([*loc].into_iter()),
            OpInner::IrInstr { prop, ops } => {
                assert_eq!(prop.op_cnt, 2);
                assert_eq!(prop.res_cnt, 1);
                assert_eq!(ops.len(), 3, "incomplete defintion");
                Box::new(ops.get(0).into_iter())
            },
            OpInner::Alloc { loc, ty } =>  Box::new([*loc].into_iter()),
        };
        ret
    }

    pub fn read_bindings(&self) -> impl Iterator<Item = Binding> + '_ {
        let ret: Box<dyn Iterator<Item = Binding>> = match &self.inner {
            OpInner::Call { id, args } => Box::new(args.into_iter()),
            OpInner::Load { loc, ptr } => Box::new([*ptr].into_iter()),
            OpInner::Block { id, args } => Box::new([].into_iter()),
            OpInner::Br { check, success, fail } => 
                Box::new([*check].into_iter()
                    .chain(success.read_registers())
                    .chain(fail.read_registers())),
            OpInner::MachInstr { ins } => Box::new(ins.read_registers()),
            OpInner::Jmp { target } => Box::new(target.read_registers()),
            OpInner::Store { dst, src } => Box::new([*dst, *src].into_iter()),
            OpInner::Assign { val, .. } => Box::new(val.as_binding().into_iter()),
            OpInner::IrInstr { prop, ops } => {
                assert_eq!(prop.op_cnt, 2);
                assert_eq!(prop.res_cnt, 1);
                assert_eq!(ops.len(), 3, "incomplete defintion");
                Box::new([ops.get(1), ops.get(2)].into_iter().flatten())
            },
            OpInner::Alloc { .. } => Box::new([].into_iter()),
        };
        ret
    }

    pub fn jump_dsts(&self) -> impl Iterator<Item = Binding> + '_ {
        let ret: Box<dyn Iterator<Item = Binding>> = match &self.inner {
            OpInner::Jmp { target } => Box::new([target.id].into_iter()),
            OpInner::Br { success, fail, ..  } => Box::new([success.id, fail.id].into_iter()),
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

impl Instruction {
    fn detach(&self) -> (Option<Rc<Self>>, Option<Rc<Self>>) {
        let prev = self.prev.take().and_then(|ptr| ptr.upgrade());
        let next = self.next.take();
        if let Some(ref prev) = prev {
            prev.next.set(next)
        }
        if let Some(ref next) = next {
            next.prev.set(Some(Rc::downgrade(next)))
        }
        (prev, next)
    }

    fn is_detached(&self) -> bool {
        self.take_prev().is_none() && self.take_next().is_none()
    }

    /// returns true if both previous and next instructions are properly linked
    fn link_is_valid(self: &Rc<Self>) -> bool {
        if let Some(prev) = self.take_prev() {
            let Some(prev_next) = prev.take_next() else { return false };
            if !Rc::ptr_eq(self, &prev_next) {
                return false;
            }
        };
        if let Some(next) = self.take_next() {
            let Some(next_prev) = next.take_prev() else { return false };
            if !Rc::ptr_eq(&self, &next_prev) {
                return false;
            }
        };
        true
    }

    fn is_valid_instr(&self) -> bool {
        let has_prev = self.take_prev().is_some();
        let has_next = self.take_next().is_some();
        let is_header = self.is_block_header();
        let is_term = self.is_term();

        if !has_prev && !is_header {
            return false;
        }
        if has_prev && is_header {
            return false;
        }
        if !has_next && !is_term {
            return false;
        }
        if has_next && is_term {
            return false;
        }
        true
    }

    fn take_prev(&self) -> Option<Rc<Instruction>> {
        let ret = self.prev.take().and_then(|p| p.upgrade());
        if let Some(ref prev) = ret {
            self.prev.set(Some(Rc::downgrade(prev)))
        }
        ret
    }

    fn take_next(&self) -> Option<Rc<Instruction>> {
        let ret = self.next.take();
        if let Some(ref next) = ret {
            self.next.set(Some(Rc::clone(next)));
        }
        ret
    }

    fn has_prev(&self) -> bool {
        let prev = self.prev.take();
        let ret = prev.is_some();
        self.prev.set(prev);
        ret
    }

    fn has_next(&self) -> bool {
        let next = self.next.take();
        let ret = next.is_some();
        self.next.set(next);
        ret
    }

    fn insert_before(self: &Rc<Self>, instr: Rc<Instruction>) {
        debug_assert!(!instr.is_term());
        debug_assert!(instr.is_detached());
        debug_assert!(self.link_is_valid());
        let prev = self.take_prev();
        if let Some(prev) = prev {
            debug_assert!(!self.is_block_header());
            prev.next.set(Some(Rc::clone(&instr)));
            instr.prev.set(Some(Rc::downgrade(&prev)));
        }
        instr.next.set(Some(Rc::clone(self)));
        self.prev.set(Some(Rc::downgrade(&instr)));
    }

    fn insert_after(self: &Rc<Self>, instr: Rc<Instruction>) {
        debug_assert!(!instr.is_term());
        debug_assert!(!self.is_term());
        debug_assert!(instr.is_detached());
        debug_assert!(self.link_is_valid());
        let next = self.take_next();
        if let Some(next) = next {
            debug_assert!(!self.is_block_header());
            next.prev.set(Some(Rc::downgrade(&instr)));
            instr.next.set(Some(next));
        }
        instr.prev.set(Some(Rc::downgrade(self)));
        self.next.set(Some(instr));
    }

    fn append_unfinished(self: &Rc<Self>, instr: Rc<Instruction>) {
        instr.prev.set(Some(Rc::downgrade(self)));
        self.next.set(Some(instr));
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

pub struct BasicBlock {
    id: BlockId,
    pred: VecSet<usize>,
    head: Rc<Instruction>,
    tail: Rc<Instruction>,
    seq: VecSet<usize>,
}

impl BasicBlock {
    fn is_valid(&self) -> bool {
        let mut instr = Some(&self.head);
        while let Some(i) = instr {
            if !i.is_valid_instr() || !i.link_is_valid() {
                return false
            }
            if Rc::ptr_eq(i, &self.tail) && !i.is_term() {
                return false
            }
        }
        true
    }

    fn split_function(instrs: impl IntoIterator<Item = InstructionTemplate>) -> Result<Vec<BasicBlock>, ()> {
        let mut it = instrs.into_iter().map(|t| t.into());
        let mut blocks: VecMap<BlockId, BasicBlock> = VecMap::new();
        loop {
            let Some(head): Option<Instruction> = it.next() else { break };
            let head = Rc::new(head);
            let Some(id) = head.inner.as_block_id() else { return Err(()) };
            let mut curr = Rc::clone(&head);
            while let Some(next) = it.next() {
                let next = Rc::new(next);
                curr.append_unfinished(Rc::clone(&next));
                curr = next;
                if curr.is_term() {
                    break;
                }
            }
            let repl = blocks.insert(id, BasicBlock {
                id,
                pred: VecSet::new(),
                head,
                tail: curr,
                seq: VecSet::new(),
            });
            debug_assert!(repl.is_none(), "duplicate definition");
        }
        let ids: Vec<_> = blocks.keys().copied().collect();
        for id in ids {
            let seq_ids: Vec<_> = blocks[&id].tail.jump_dsts().map(|x| x.as_label()
                .expect("Non-label jump targets are not supported")).collect();
            let mut seq_idxs = Vec::new();
            let idx = blocks.get_index(&id).unwrap();
            for &seq in &seq_ids {
                blocks[seq].pred.insert(idx);
                let Some(seq) = blocks.get_index(seq) else {
                    panic!("block {seq} was defined in a different function");
                };
                seq_idxs.push(seq);
            }
            blocks[&id].seq.extend(seq_idxs.iter().copied());
        }

        Ok(blocks.into_value_vec())
    }

    fn iter(&self) -> BlockInstrIter {
        BlockInstrIter { curr: Some(Rc::clone(&self.head)) }
    }
}

struct BlockInstrIter {
    curr: Option<Rc<Instruction>>
}

impl Iterator for BlockInstrIter {
    type Item = Rc<Instruction>;

    fn next(&mut self) -> Option<Self::Item> {
        let ret = self.curr.take()?;
        self.curr = ret.take_next();
        Some(ret)
    }
}

pub struct Function {
    name: Box<str>,
    blocks: Vec<BasicBlock>,
    reg_state: SSAState,
}

impl Function {
    pub fn from_iter(name: impl Into<Box<str>> + ?Sized, instrs: impl IntoIterator<Item = InstructionTemplate>) -> Self {
        let blocks = BasicBlock::split_function(instrs).expect("valid function");
        Function { name: name.into(), blocks, reg_state: Default::default() }
    }

    fn build_defuse(&mut self) {

    }
}
