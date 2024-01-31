use crate::attr::BindAttributes;
use crate::list::List;
use crate::list::Ref as ListRef;
use crate::list::ThinRef;
use crate::reg::Binding;
use crate::reg::IrBinding;
use crate::reg::MachineReg;
use crate::vec_map::VecSet;
use crate::warn_once;
use crate::InstrArg;
use crate::Instruction;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug, Clone, Copy)]
pub enum PhysRegUse {
    Clobbered,
    UseClobber,
    Use,
    Def,
    UseDef,
}

impl PhysRegUse {
    pub const fn is_read(self) -> bool {
        matches!(self, Self::Use | Self::UseDef | Self::UseClobber)
    }

    pub const fn is_clobbered(self) -> bool {
        matches!(self, Self::Clobbered | Self::UseClobber)
    }

    pub const fn is_defined(self) -> bool {
        matches!(self, Self::Def | Self::UseDef)
    }
}

pub struct PtrMeta {}

pub enum PhysRegDefinednessState {
    /// Garbage data
    Clobbered,

    /// holds value of the contained virtual register
    Virtual(IrBinding),
}

/// physical register state, expected to mutate during codegen
pub struct PhysRegState {
    defined: PhysRegDefinednessState,
}
