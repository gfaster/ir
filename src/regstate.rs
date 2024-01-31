use crate::warn_once;
use crate::attr::BindAttributes;
use crate::list::List;
use crate::list::ThinRef;
use crate::reg::Binding;
use crate::reg::MachineReg;
use crate::reg::IrBinding;
use crate::InstrArg;
use crate::Instruction;
use crate::vec_map::VecSet;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::ops::Deref;
use std::rc::Rc;
use crate::list::Ref as ListRef;

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

// Getting rid of this because it's easier if they're well behaved
// #[derive(Debug)]
// enum RegDefineState {
//     Defined(Rc<Instruction>),
//
//     /// this register is undefined and any use of it is undefined behavior
//     Undefined,
//
//     /// This register seems to have multiple defintions. It's probably an error if this appears.
//     NonSSA,
// }

#[derive(Debug)]
struct SSARegState {
    defined_by: ThinRef<Instruction>,
    used_by: VecSet<ThinRef<Instruction>>,
    attrs: BindAttributes,
}

impl SSARegState {
    fn is_unused(&self) -> bool {
        self.used_by.is_empty()
    }
}

#[derive(Debug, Default)]
pub struct SSAState {
    regs: BTreeMap<Binding, SSARegState>,

    // must come after regs so regs is dropped first
    /// kept in an Rc so we can iterate over instructions and mutate regstate
    list: Rc<List<Instruction>>,

    // addresses of contained instructions for checking safety
    instrs: BTreeSet<usize>,
}

impl FromIterator<Instruction> for SSAState {
    fn from_iter<T: IntoIterator<Item = Instruction>>(iter: T) -> Self {
        let mut regs = BTreeMap::new();
        let mut instrs = BTreeSet::new();
        let list = Rc::new(List::new());
        for instr in iter {
            let instr = list.push_back(instr);
            for def in instr.defined_bindings() {
                if def.is_machine() {
                    panic!("I don't know how I'm going to handle machine regs here")
                }
                regs.entry(def).and_modify(|_| panic!("double definition of {def}")).or_insert(SSARegState {
                    defined_by: instr.into(),
                    used_by: VecSet::new(),
                    attrs: BindAttributes::new(crate::ty::Type::i64())
                });
                warn_once!("TODO: specify type here");
            }
            for used in instr.read_bindings() {
                let Some(state) = regs.get_mut(&used) else {panic!("use of {used} without def")};
                state.used_by.insert(instr.into());
            }
            instrs.insert(&*instr as *const Instruction as usize);
        }
        SSAState { regs, list, instrs }
    }
}

impl SSAState {
    pub fn machine_reg_use(&self, instr: &Instruction, reg: MachineReg) -> Option<&Instruction> {
        assert!(self.is_instr_in_state(instr));

        // Safety: instr is in list
        unsafe { self.machine_reg_use_unchecked(instr, reg) }
    }

    pub unsafe fn machine_reg_use_unchecked(&self, instr: &Instruction, reg: MachineReg) -> Option<&Instruction> {
        let instr = ThinRef::from_element_ref(instr).promote(&self.list);
        for instr in instr.make_iter() {
            if instr.is_branch() {
                /// TODO: maintain physical regs through branches
                return None;
            }
            // let Some( machine ) = instr.
        }
        todo!()
    }

    /// Entry point for machine optimizations
    pub fn optimize(&mut self) {
        let list = self.list.clone();
        todo!()
    }

    pub fn bind_attrs(&self, bind: Binding) -> &BindAttributes {
        let state = self.regs.get(&bind).unwrap_or_else(|| panic!("bind {bind} was never defined"));
        &state.attrs
    }

    pub fn defining_instr(&self, bind: Binding) -> impl Deref<Target = Instruction> + '_ {
        let state = self.regs.get(&bind).unwrap_or_else(|| panic!("bind {bind} was never defined"));
        // Safety: thinref was created in list
        unsafe {
            state.defined_by.promote(&self.list)
        }
    }

    fn is_instr_in_state(&self, instr: &Instruction) -> bool {
        self.instrs.contains(&(instr as *const _ as usize))
    }

    pub fn update_bind_attrs(&mut self, bind: Binding, attr: BindAttributes) {
        let state = self.regs.get_mut(&bind).unwrap_or_else(|| panic!("bind {bind} was never defined"));
        debug_assert_eq!(state.attrs.ty(), attr.ty(), "attribute type can't be different");
        state.attrs = attr
    }

    pub fn replace_instr(&mut self, old: &Instruction, new: Instruction) {
        assert!(self.is_instr_in_state(old));
        unsafe { self.replace_instr_unchecked(old, new) }
    }

    /// Safety: caller must uphold that old is in the list
    pub unsafe fn replace_instr_unchecked(&mut self, old: &Instruction, new: Instruction) {
        // Safety: caller must uphold that old is in the list
        let old = unsafe { ThinRef::from_element_ref(old).promote(&self.list) };
        let new = old.replace(new);

        for def in old.defined_bindings() {
            let state = self.regs.get_mut(&def).expect("old binding definitions are recorded");
            state.defined_by = new.into();
        }
        for used in old.read_bindings() {
            let Some(state) = self.regs.get_mut(&used) else {continue};
            state.used_by.remove(&old.into());
        }
        for used in new.read_bindings() {
            let state = self.regs.get_mut(&used).expect("definitions for new binding usage exists");
            state.used_by.insert(new.into());
        }
    }
    
    fn can_remove_instruction(&self, instr: &Instruction) -> bool {
        let no_uses = instr.defined_bindings().map(|b| {
            self.regs[&b].is_unused()
        }).all(std::convert::identity);
        no_uses && !instr.is_movable()
    }

    /// get rid of unused instructions
    pub fn trim(&mut self) -> usize {
        let mut additional: VecSet<ThinRef<_>> = VecSet::new();
        let mut cnt = 0;
        // first linear pass, checking every instruction
        for instr in &*self.list {
            if self.can_remove_instruction(&instr) {
                additional.remove(&instr.into());
                cnt += 1;
                instr.detach();
                for used in instr.read_bindings() {
                    let state = self.regs.get_mut(&used).expect("definitions for binding usage exists");
                    state.used_by.remove(&instr.into());
                    let definer = unsafe { state.defined_by.promote(&self.list) };
                    if self.can_remove_instruction(&definer) {
                        additional.insert(definer.into());
                    }
                }
            }
        }

        // second pass with more intelligence
        while let Some(instr) = additional.pop() {
            let instr = unsafe { instr.promote(&self.list) };
            debug_assert!(self.can_remove_instruction(&instr));
            instr.detach();
            cnt += 1;

            for used in instr.read_bindings() {
                let state = self.regs.get_mut(&used).expect("definitions for binding usage exists");
                state.used_by.remove(&instr.into());
                let definer = unsafe { state.defined_by.promote(&self.list) };
                if self.can_remove_instruction(&definer) {
                    additional.insert(definer.into());
                }
            }
        }
        cnt
    }
}

pub struct PtrMeta {

}

pub enum PhysRegDefinednessState {
    /// Garbage data
    Clobbered,

    /// holds value of the contained virtual register
    Virtual(IrBinding),
}

/// physical register state, expected to mutate during codegen
pub struct PhysRegState {
    defined: PhysRegDefinednessState
}
