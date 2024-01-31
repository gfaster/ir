use std::{collections::BTreeMap, fmt::Display, rc::Rc, sync::Mutex};

use crate::{ty::Type, vec_map::VecMap, IdTy, Instruction};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct InstrArg(InstrArgTy);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct MachineArg(MachArgTy);

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Binding(BindTy);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct MachineReg(u16);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct IrBinding(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Virtual(usize, RegBank);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct RegBank(pub u8);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Immediate(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct BlockId(IdTy);

pub mod bind_names {
    use super::{BindTy, Binding};
    use std::{
        collections::BTreeMap,
        fmt::Display,
        sync::{Arc, Mutex, RwLock},
    };

    static BIND_NAMES: RwLock<BTreeMap<Binding, Arc<str>>> = RwLock::new(BTreeMap::new());
    pub fn register_binding(bind: Binding, name: impl Into<Arc<str>>) {
        BIND_NAMES.write().unwrap().insert(bind, name.into());
    }

    pub fn register_many<I, T>(bank: I)
    where
        I: IntoIterator<Item = (Binding, T)>,
        T: Into<Arc<str>>,
    {
        BIND_NAMES
            .write()
            .unwrap()
            .extend(bank.into_iter().map(|(k, v)| (k, v.into())))
    }

    pub struct BindDisplay(BindDispInner);
    enum BindDispInner {
        Named(Arc<str>),
        Unknown(Binding),
    }

    impl Display for BindDisplay {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match &self.0 {
                BindDispInner::Named(s) => s.fmt(f),
                BindDispInner::Unknown(Binding(BindTy::IrBinding(v))) => {
                    write!(f, "{}", v.0)
                }
                BindDispInner::Unknown(Binding(BindTy::Virtual(v))) => {
                    write!(f, "{}", v.0)
                }
                BindDispInner::Unknown(Binding(BindTy::Machine(m))) => {
                    write!(f, "{m}")
                }
                BindDispInner::Unknown(Binding(BindTy::Block(b))) => {
                    write!(f, "{}", b.0)
                }
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

    pub fn as_ir_bind(&self) -> Option<IrBinding> {
        self.0.as_binding()?.as_ir_bind()
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

    pub fn is_ir(&self) -> bool {
        self.0.as_binding().is_some_and(|b| b.is_ir())
    }

    pub fn is_imm(&self) -> bool {
        self.as_imm().is_some()
    }

    pub fn new_ir_bind() -> Self {
        IrBinding::new().into()
    }

    pub fn new_block() -> Self {
        BlockId::new().into()
    }
}

impl MachineArg {
    pub fn as_machine(&self) -> Option<MachineReg> {
        self.0.as_machine()
    }

    pub fn as_label(&self) -> Option<BlockId> {
        self.0.as_label()
    }

    pub fn as_imm(&self) -> Option<Immediate> {
        self.0.as_imm()
    }

    pub fn is_machine(&self) -> bool {
        self.0.as_machine().is_some()
    }

    pub fn is_imm(&self) -> bool {
        self.as_imm().is_some()
    }
}

impl Binding {
    pub fn as_machine(&self) -> Option<MachineReg> {
        self.0.as_machine()
    }

    pub fn as_ir_bind(&self) -> Option<IrBinding> {
        self.0.as_ir_bind()
    }

    pub fn as_label(&self) -> Option<BlockId> {
        self.0.as_label()
    }

    pub fn is_machine(&self) -> bool {
        self.0.is_machine()
    }

    pub fn is_ir(&self) -> bool {
        self.0.is_ir_bind()
    }

    pub fn is_label(&self) -> bool {
        self.0.is_label()
    }

    pub fn new_ir_bind() -> Self {
        IrBinding::new().into()
    }
}

mod sealed {
    use super::*;
    static CNT: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
    impl Virtual {
        pub fn new(bank: RegBank) -> Self {
            let id = CNT.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            Virtual(id, bank)
        }
    }

    impl IrBinding {
        pub fn new() -> Self {
            let id = CNT.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            IrBinding(id)
        }
    }
}

impl RegBank {
    const fn new(id: u8) -> Self {
        Self(id)
    }

    const fn as_idx(self) -> usize {
        self.0 as usize
    }
}

impl From<Binding> for InstrArg {
    fn from(v: Binding) -> Self {
        InstrArg(InstrArgTy::Binding(v))
    }
}

impl From<MachineReg> for InstrArg {
    fn from(v: MachineReg) -> Self {
        let b: Binding = v.into();
        InstrArg(InstrArgTy::Binding(b))
    }
}

impl From<BlockId> for InstrArg {
    fn from(v: BlockId) -> Self {
        let b: Binding = v.into();
        InstrArg(InstrArgTy::Binding(b))
    }
}

impl From<IrBinding> for InstrArg {
    fn from(v: IrBinding) -> Self {
        let b: Binding = v.into();
        InstrArg(InstrArgTy::Binding(b))
    }
}

impl From<Immediate> for InstrArg {
    fn from(v: Immediate) -> Self {
        InstrArg(InstrArgTy::Imm(v))
    }
}

impl From<Virtual> for InstrArg {
    fn from(v: Virtual) -> Self {
        InstrArg(InstrArgTy::Binding(Binding(BindTy::Virtual(v))))
    }
}

impl From<MachineArg> for InstrArg {
    fn from(v: MachineArg) -> Self {
        match v.0 {
            MachArgTy::Virtual(v) => v.into(),
            MachArgTy::Machine(m) => m.into(),
            MachArgTy::Imm(i) => i.into(),
            MachArgTy::Block(b) => b.into(),
        }
    }
}

impl From<&Binding> for InstrArg {
    fn from(&v: &Binding) -> Self {
        InstrArg(InstrArgTy::Binding(v))
    }
}

impl From<&MachineReg> for InstrArg {
    fn from(&v: &MachineReg) -> Self {
        let b: Binding = v.into();
        InstrArg(InstrArgTy::Binding(b))
    }
}

impl From<&BlockId> for InstrArg {
    fn from(&v: &BlockId) -> Self {
        let b: Binding = v.into();
        InstrArg(InstrArgTy::Binding(b))
    }
}

impl From<&IrBinding> for InstrArg {
    fn from(&v: &IrBinding) -> Self {
        let b: Binding = v.into();
        InstrArg(InstrArgTy::Binding(b))
    }
}

impl From<&Immediate> for InstrArg {
    fn from(&v: &Immediate) -> Self {
        InstrArg(InstrArgTy::Imm(v))
    }
}

impl From<&Virtual> for InstrArg {
    fn from(&v: &Virtual) -> Self {
        InstrArg(InstrArgTy::Binding(Binding(BindTy::Virtual(v))))
    }
}

impl From<&MachineArg> for InstrArg {
    fn from(&v: &MachineArg) -> Self {
        v.into()
    }
}

impl From<Virtual> for Binding {
    fn from(v: Virtual) -> Self {
        Binding(BindTy::Virtual(v))
    }
}

impl From<MachineReg> for Binding {
    fn from(v: MachineReg) -> Self {
        Binding(BindTy::Machine(v))
    }
}

impl From<IrBinding> for Binding {
    fn from(v: IrBinding) -> Self {
        Binding(BindTy::IrBinding(v))
    }
}

impl From<BlockId> for Binding {
    fn from(v: BlockId) -> Self {
        Binding(BindTy::Block(v))
    }
}

impl From<&Virtual> for Binding {
    fn from(&v: &Virtual) -> Self {
        Binding(BindTy::Virtual(v))
    }
}

impl From<&MachineReg> for Binding {
    fn from(&v: &MachineReg) -> Self {
        v.into()
    }
}

impl From<&IrBinding> for Binding {
    fn from(&v: &IrBinding) -> Self {
        v.into()
    }
}

impl From<&BlockId> for Binding {
    fn from(&v: &BlockId) -> Self {
        Binding(BindTy::Block(v))
    }
}

impl std::fmt::Debug for Binding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            BindTy::IrBinding(i) => Display::fmt(&i, f),
            BindTy::Machine(m) => Display::fmt(&m, f),
            BindTy::Block(b) => Display::fmt(&b, f),
            BindTy::Virtual(v) => Display::fmt(&v, f),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum InstrArgTy {
    Binding(Binding),
    Imm(Immediate),
}

impl InstrArgTy {
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
    /// generic register: infinite and flexible
    IrBinding(IrBinding),

    /// virtual register: has an infinite ID, but corresponds with a set of registers
    Virtual(Virtual),

    /// Machine register: corresponds with a machine register (e.g. `rax`)
    Machine(MachineReg),

    Block(BlockId),
}

impl BindTy {
    #[must_use]
    fn is_ir_bind(&self) -> bool {
        matches!(self, Self::IrBinding(..))
    }

    fn as_ir_bind(&self) -> Option<IrBinding> {
        if let Self::IrBinding(v) = self {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum MachArgTy {
    /// generic register: infinite, but constrained to a register bank
    Virtual(Virtual),

    /// Machine register: corresponds with a machine register (e.g. `rax`)
    Machine(MachineReg),

    /// Immediate, possibly needs to be truncated
    Imm(Immediate),

    Block(BlockId),
}

impl MachArgTy {
    fn as_imm(&self) -> Option<Immediate> {
        if let Self::Imm(v) = self {
            Some(*v)
        } else {
            None
        }
    }

    fn as_machine(&self) -> Option<MachineReg> {
        if let Self::Machine(v) = self {
            Some(*v)
        } else {
            None
        }
    }

    fn as_virtual(&self) -> Option<Virtual> {
        if let Self::Virtual(v) = self {
            Some(*v)
        } else {
            None
        }
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

impl std::fmt::Display for IrBinding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let b: Binding = self.into();
        let name = bind_names::bind_name(b);
        write!(f, "%{name}")
    }
}

impl std::fmt::Display for Virtual {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let b: Binding = self.into();
        let name = bind_names::bind_name(b);
        write!(f, "%{name}")
    }
}

impl std::fmt::Display for Immediate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl BlockId {
    pub fn new() -> BlockId {
        static LABEL_COUNTER: std::sync::atomic::AtomicUsize =
            std::sync::atomic::AtomicUsize::new(0);
        BlockId(LABEL_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
    }
}

impl std::fmt::Display for InstrArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            InstrArgTy::Binding(b) => b.fmt(f),
            InstrArgTy::Imm(i) => i.fmt(f),
        }
    }
}

impl std::fmt::Display for Binding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            BindTy::IrBinding(v) => v.fmt(f),
            BindTy::Machine(m) => m.fmt(f),
            BindTy::Block(b) => b.fmt(f),
            BindTy::Virtual(v) => v.fmt(f),
        }
    }
}
