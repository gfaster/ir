use std::{rc::Rc, collections::BTreeMap};

use crate::{IdTy, vec_map::VecMap, Instruction};


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Binding(BindTy);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct MachineReg(u16);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Virtual(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct BlockId(IdTy);


impl Binding {
    pub fn as_machine(&self) -> Option<&MachineReg> {
        self.0.as_machine()
    }

    pub fn as_virtual(&self) -> Option<&IdTy> {
        self.0.as_virtual()
    }

    pub fn as_label(&self) -> Option<&BlockId> {
        self.0.as_label()
    }

    pub fn is_machine(&self) -> bool {
        self.0.is_machine()
    }

    pub fn is_virtual(&self) -> bool {
        self.0.is_virtual()
    }

    pub const fn from_mach(phys: MachineReg) -> Self {
        Binding(BindTy::Machine(phys))
    }

    pub fn new_virtual() -> Self {
        static LABEL_COUNTER: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
        let id = LABEL_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        Binding(BindTy::Virtual(id))
    }

    pub fn new_block() -> Self {
        BlockId::new().into()
    }
}

impl From<MachineReg> for Binding {
    fn from(v: MachineReg) -> Self {
        Binding(BindTy::Machine(v))
    }
}

impl From<BlockId> for Binding {
    fn from(v: BlockId) -> Self {
        Binding(BindTy::Label(v))
    }
}

impl From<Virtual> for Binding {
    fn from(v: Virtual) -> Self {
        Binding(BindTy::Virtual(v))
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum BindTy {
    /// virtual register: infinite 
    Virtual(Virtual),

    /// machine register: corresponds with a machine register (e.g. `rax`)
    Machine(MachineReg),

    Label(BlockId),
}

impl MachineReg {
    pub const fn from_idx(phys: u16) -> Self {
        Self(phys)
    }

    pub const fn idx(self) -> u16 {
        self.0
    }
}

impl std::fmt::Display for MachineReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        crate::arch::TargetReg::from_mach(*self).name().fmt(f)
    }
}

impl std::fmt::Display for BlockId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl BlockId {
    pub fn new() -> BlockId {
        static LABEL_COUNTER: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
        BlockId(LABEL_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
    }
}


impl BindTy {
    /// Returns `true` if the bind ty is [`Virtual`].
    ///
    /// [`Virtual`]: BindTy::Virtual
    #[must_use]
    fn is_virtual(&self) -> bool {
        matches!(self, Self::Virtual(..))
    }

    /// Returns `true` if the bind ty is [`Machine`].
    ///
    /// [`Machine`]: BindTy::Machine
    #[must_use]
    fn is_machine(&self) -> bool {
        matches!(self, Self::Machine(..))
    }

    fn as_machine(&self) -> Option<&MachineReg> {
        if let Self::Machine(v) = self {
            Some(v)
        } else {
            None
        }
    }

    fn as_virtual(&self) -> Option<&IdTy> {
        if let Self::Virtual(v) = self {
            Some(v)
        } else {
            None
        }
    }

    fn as_label(&self) -> Option<&BlockId> {
        if let Self::Label(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

impl std::fmt::Debug for BindTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Self as std::fmt::Display>::fmt(&self, f)
    }
}

impl std::fmt::Display for BindTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BindTy::Virtual(v) => write!(f, "%v_{v}"),
            BindTy::Machine(m) => write!(f, "%{m}"),
            BindTy::Label(l) => write!(f, "@{l}"),
        }
    }
}

impl std::fmt::Display for Binding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

enum RegDefineState {
    DefinedBy(Rc<Instruction>),

    /// this register is undefined and any use of it is undefined behavior
    Undefined,

    /// This register seems to have multiple defintions (it's probably a machine register)
    NonSSA,

    // /// It's unkown whether this register is defined. If this shows up it's probably a bug
    // Unknown
}

struct RegState {
    defined_by: RegDefineState,
    used_by: Vec<Rc<Instruction>>
}

#[derive(Debug, Default)]
pub struct BankState {
    regs: BTreeMap<Binding, RegState>,
}

impl BankState {
    pub fn add_definition(&mut self, instr: &Rc<Instruction>) {
        todo!()
    }
}
