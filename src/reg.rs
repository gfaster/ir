use std::{rc::Rc, collections::BTreeMap, fmt::Display, sync::Mutex};

use crate::{IdTy, vec_map::VecMap, Instruction, ty::Type};


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct InstrArg (ArgTy);

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Binding (BindTy);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct MachineReg(u16);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Virtual(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Immediate(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct BlockId(IdTy);

pub mod bind_names {
    use super::{Binding, BindTy};
    use std::{sync::{Mutex, RwLock, Arc}, collections::BTreeMap, fmt::Display};

    static BIND_NAMES: RwLock<BTreeMap<Binding, Arc<str>>> = RwLock::new(BTreeMap::new());
    pub fn register_binding(bind: Binding, name: impl Into<Arc<str>>) {
        BIND_NAMES.write().unwrap().insert(bind, name.into());
    }

    pub fn register_many<I, T>(bank: I)
    where 
        I: IntoIterator<Item = (Binding, T)>,
        T: Into<Arc<str>>
    {
        BIND_NAMES.write().unwrap().extend(bank.into_iter().map(|(k, v)| (k, v.into())))
    }

    pub struct BindDisplay(BindDispInner);
    enum BindDispInner {
        Named(Arc<str>),
        Unknown(Binding)
    }

    impl Display for BindDisplay {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match &self.0 {
                BindDispInner::Named(s) => s.fmt(f),
                BindDispInner::Unknown(Binding(BindTy::Virtual(v))) => {
                    write!(f, "%{}", v.0)
                },
                BindDispInner::Unknown(Binding(BindTy::Machine(m))) => {
                    m.fmt(f)
                },
                BindDispInner::Unknown(Binding(BindTy::Block(b))) => {
                    write!(f, "{}", b.0)
                },
            }
        }
    }

    pub fn bind_name(bind: Binding) -> impl Display {
        if let Some(name) = BIND_NAMES.read().unwrap().get(&bind) {
            BindDisplay(BindDispInner::Named(Arc::clone(name)))
        } else {
            BindDisplay(BindDispInner::Unknown(bind))
        }
    }
}


impl InstrArg {
    pub fn as_binding(&self) -> Option<Binding> {
        self.0.as_binding()
    }

    pub fn as_machine(&self) -> Option<MachineReg> {
        self.0.as_binding()?.as_machine()
    }

    pub fn as_virtual(&self) -> Option<Virtual> {
        self.0.as_binding()?.as_virtual()
    }

    pub fn as_label(&self) -> Option<BlockId> {
        self.0.as_binding()?.as_label()
    }

    pub fn as_imm(&self) -> Option<Immediate> {
        self.0.as_imm()
    }

    pub fn is_machine(&self) -> bool {
        self.0.as_binding().is_some_and(|b| b.is_machine())
    }

    pub fn is_virtual(&self) -> bool {
        self.0.as_binding().is_some_and(|b| b.is_virtual())
    }

    pub fn is_imm(&self) -> bool {
        self.as_imm().is_some()
    }

    pub fn new_virtual() -> Self {
        Virtual::new().into()
    }

    pub fn new_block() -> Self {
        BlockId::new().into()
    }
}

impl Binding {
    pub fn as_machine(&self) -> Option<MachineReg> {
        self.0.as_machine()
    }

    pub fn as_virtual(&self) -> Option<Virtual> {
        self.0.as_virtual()
    }

    pub fn as_label(&self) -> Option<BlockId> {
        self.0.as_label()
    }

    pub fn is_machine(&self) -> bool {
        self.0.is_machine()
    }

    pub fn is_virtual(&self) -> bool {
        self.0.is_virtual()
    }

    pub fn is_label(&self) -> bool {
        self.0.is_label()
    }

    pub fn new_virtual() -> Self {
        Virtual::new().into()
    }
}

impl Virtual {
    fn new() -> Self {
        static CNT: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
        let id = CNT.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        Virtual(id)
    }
}

impl From<Binding> for InstrArg {
    fn from(v: Binding) -> Self {
        InstrArg (ArgTy::Binding(v))
    }
}

impl From<MachineReg> for InstrArg {
    fn from(v: MachineReg) -> Self {
        let b: Binding = v.into();
        InstrArg (ArgTy::Binding(b))
    }
}

impl From<BlockId> for InstrArg {
    fn from(v: BlockId) -> Self {
        let b: Binding = v.into();
        InstrArg (ArgTy::Binding(b))
    }
}

impl From<Virtual> for InstrArg {
    fn from(v: Virtual) -> Self {
        let b: Binding = v.into();
        InstrArg (ArgTy::Binding(b))
    }
}

impl From<Immediate> for InstrArg {
    fn from(v: Immediate) -> Self {
        InstrArg ( ArgTy::Imm(v))
    }
}

impl From<&Binding> for InstrArg {
    fn from(&v: &Binding) -> Self {
        InstrArg (ArgTy::Binding(v))
    }
}

impl From<&MachineReg> for InstrArg {
    fn from(&v: &MachineReg) -> Self {
        let b: Binding = v.into();
        InstrArg (ArgTy::Binding(b))
    }
}

impl From<&BlockId> for InstrArg {
    fn from(&v: &BlockId) -> Self {
        let b: Binding = v.into();
        InstrArg (ArgTy::Binding(b))
    }
}

impl From<&Virtual> for InstrArg {
    fn from(&v: &Virtual) -> Self {
        let b: Binding = v.into();
        InstrArg (ArgTy::Binding(b))
    }
}

impl From<&Immediate> for InstrArg {
    fn from(&v: &Immediate) -> Self {
        InstrArg ( ArgTy::Imm(v))
    }
}

impl From<MachineReg> for Binding {
    fn from(v: MachineReg) -> Self {
        Binding (BindTy::Machine(v))
    }
}

impl From<Virtual> for Binding {
    fn from(v: Virtual) -> Self {
        Binding ( BindTy::Virtual(v))
    }
}

impl From<BlockId> for Binding {
    fn from(v: BlockId) -> Self {
        Binding ( BindTy::Block(v))
    }
}

impl From<&MachineReg> for Binding {
    fn from(&v: &MachineReg) -> Self {
        v.into()
    }
}

impl From<&Virtual> for Binding {
    fn from(&v: &Virtual) -> Self {
        v.into()
    }
}

impl From<&BlockId> for Binding {
    fn from(&v: &BlockId) -> Self {
        Binding ( BindTy::Block(v))
    }
}

impl std::fmt::Debug for Binding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            BindTy::Virtual(v) => Display::fmt(&v, f),
            BindTy::Machine(m) => Display::fmt(&m, f),
            BindTy::Block(b) => Display::fmt(&b, f),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum ArgTy {
    Binding(Binding),
    Imm(Immediate),
}

impl ArgTy {
    fn as_binding(&self) -> Option<Binding> {
        if let Self::Binding(v) = self {
            Some(*v)
        } else {
            None
        }
    }

    fn as_imm(&self) -> Option<Immediate> {
        if let Self::Imm(v) = self {
            Some(*v)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum BindTy {
    /// virtual register: infinite 
    Virtual(Virtual),

    /// Machine register: corresponds with a machine register (e.g. `rax`)
    Machine (MachineReg),

    Block(BlockId),
}

impl BindTy {
    /// Returns `true` if the lval ty is [`Virtual`].
    ///
    /// [`Virtual`]: LvalTy::Virtual
    #[must_use]
    fn is_virtual(&self) -> bool {
        matches!(self, Self::Virtual(..))
    }

    fn as_virtual(&self) -> Option<Virtual> {
        if let Self::Virtual(v) = self {
            Some(*v)
        } else {
            None
        }
    }

    /// Returns `true` if the lval ty is [`Machine`].
    ///
    /// [`Machine`]: LvalTy::Machine
    #[must_use]
    fn is_machine(&self) -> bool {
        matches!(self, Self::Machine(..))
    }

    fn as_machine(&self) -> Option<MachineReg> {
        if let Self::Machine(v) = self {
            Some(*v)
        } else {
            None
        }
    }

    /// Returns `true` if the bind ty is [`Block`].
    ///
    /// [`Block`]: BindTy::Block
    #[must_use]
    fn is_label(&self) -> bool {
        matches!(self, Self::Block(..))
    }

    fn as_label(&self) -> Option<BlockId> {
        if let Self::Block(v) = self {
            Some(*v)
        } else {
            None
        }
    }
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
        let b: Binding = self.into();
        let name = bind_names::bind_name(b);
        write!(f, "{name}")
    }
}

impl std::fmt::Display for Virtual {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let b: Binding = self.into();
        let name = bind_names::bind_name(b);
        write!(f, "{name}")
    }
}

impl std::fmt::Display for Immediate {
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


impl std::fmt::Display for InstrArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            ArgTy::Binding(b) => b.fmt(f),
            ArgTy::Imm(i) => i.fmt(f),
        }
    }
}

impl std::fmt::Display for Binding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            BindTy::Virtual(v) => v.fmt(f),
            BindTy::Machine(m) => m.fmt(f),
            BindTy::Block(b) => b.fmt(f),
        }
    }
}

