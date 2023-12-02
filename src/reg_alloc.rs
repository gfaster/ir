use crate::Loc;
use crate::Reg;
use crate::{AsmOp, CallType, OpTarget};

use std::collections::BTreeMap;

#[derive(Clone, Copy)]
pub enum VarLoc {
    Stack(usize),
    Reg { reg: u8, stack_slot: Option<usize> },
    Uninit,
}

#[derive(Clone)]
pub struct RegAlloc {
    vars: BTreeMap<Loc, VarLoc>,
    regs: Vec<Option<Loc>>,
    times: Vec<usize>,
    age: usize,
    max_stack: usize,
    free_stack: Vec<usize>,
    opqueue: Vec<AsmOp>,
}

impl RegAlloc {
    pub fn new(reg_cnt: usize) -> Self {
        Self {
            vars: BTreeMap::new(),
            regs: vec![None; reg_cnt],
            times: vec![0; reg_cnt],
            age: 0,
            max_stack: 0,
            free_stack: Vec::new(),
            opqueue: Vec::new(),
        }
    }

    fn free_reg(&self) -> Option<usize> {
        if self.regs.iter().position(|x| x == &None).is_none() {
            eprintln!("{:?} has no free slots", self.regs);
        }
        self.regs.iter().position(|x| x == &None)
    }

    pub fn touch_reg(&mut self, reg: u8) {
        self.age += 1;
        self.times[reg as usize] = self.age
    }

    fn reserve_stack(&mut self) -> usize {
        if let Some(free) = self.free_stack.pop() {
            return free;
        }
        let ret = self.max_stack;
        self.max_stack += 8;
        ret
    }

    /// frees register
    fn evict_reg(&mut self, reg: u8) {
        let idx = reg as usize;
        if let Some(var) = self.regs[idx] {
            let Some(entry) = self.vars.get(&var) else {
                panic!("register var is not defined");
            };
            let VarLoc::Reg { reg: r, stack_slot } = entry else {
                panic!("register var is in register but recorded as in the stack");
            };
            if let Some(free_reg) = self.free_reg() {
                self.regs[free_reg] = Some(var);
                self.vars.insert(
                    var,
                    VarLoc::Reg {
                        reg: free_reg as u8,
                        stack_slot: *stack_slot,
                    },
                );
                self.opqueue.push(AsmOp::Mov(
                    OpTarget::Reg(free_reg as u8),
                    OpTarget::Reg(reg),
                ));
                self.opqueue.push(AsmOp::Comment(format!(
                    " ^^^ L_{var} from {reg:?} to free",
                    reg = Reg::from_idx(reg)
                )));
            } else {
                let slot = if let Some(slot) = *stack_slot {
                    slot
                } else {
                    if let Some(free) = self.free_stack.pop() {
                        free
                    } else {
                        let ret = self.max_stack;
                        self.max_stack += 8;
                        ret
                    }
                };
                let r = *r;
                self.vars.insert(var, VarLoc::Stack(slot));
                self.opqueue
                    .push(AsmOp::Mov(OpTarget::Stack(slot), OpTarget::Reg(r)));
                self.opqueue
                    .push(AsmOp::Comment(format!(" ^^^ L_{var} to stack")));
            }
        }
        self.regs[reg as usize] = None;
    }

    fn oldest(&self) -> u8 {
        self.times
            .iter()
            .enumerate()
            .min_by_key(|&(_i, &a)| a)
            .expect("nonempty array")
            .0 as u8
    }

    fn oldest_protected(&self, protected: &[u8]) -> u8 {
        self.times
            .iter()
            .enumerate()
            .filter(|&(i, _)| !protected.contains(&(i as u8)))
            .min_by_key(|&(_i, &a)| a)
            .expect("nonempty array and protected leaves room")
            .0 as u8
    }

    fn evict_oldest(&mut self) -> u8 {
        let oldest = self.oldest();
        self.evict_reg(oldest);
        oldest
    }

    fn evict_oldest_protected(&mut self, protected: &[u8]) -> u8 {
        let oldest = self.oldest_protected(protected);
        self.evict_reg(oldest as u8);
        oldest
    }

    fn move_to_specific_reg(&mut self, var: Loc, target: u8) {
        self.touch_reg(target);
        if let Some(curr) = self.vars.get_mut(&var) {
            match curr {
                VarLoc::Stack(slot) => {
                    let slot = *slot;
                    self.evict_reg(target);
                    assert_eq!(self.regs[target as usize], None);
                    self.regs[target as usize] = Some(var);
                    let curr = self.vars.get_mut(&var).expect("var declared");
                    *curr = VarLoc::Reg {
                        reg: target,
                        stack_slot: Some(slot),
                    };
                    self.opqueue
                        .push(AsmOp::Mov(OpTarget::Reg(target), OpTarget::Stack(slot)));
                    self.opqueue
                        .push(AsmOp::Comment(format!(" ^^^ L_{var} := *(rsp + {slot})")));
                }
                VarLoc::Reg { reg, .. } => {
                    if *reg == target {
                        // already in the right spot, don't need to do anything
                        return;
                    }
                    let prev = *reg;
                    self.evict_reg(target);
                    let Some(VarLoc::Reg { reg, .. }) = self.vars.get_mut(&var) else {
                        panic!("reg was removed from tree during eviction");
                    };
                    assert_eq!(prev, *reg, "register changed during eviction");
                    let src = *reg;
                    *reg = target;
                    assert_eq!(self.regs[target as usize], None);
                    self.regs[target as usize] = Some(var);
                    self.regs[src as usize] = None;
                    self.opqueue
                        .push(AsmOp::Mov(OpTarget::Reg(target), OpTarget::Reg(src)));
                    self.opqueue.push(AsmOp::Comment(format!(
                        " ^^^ L_{var} := {reg:?}",
                        reg = Reg::from_idx(src)
                    )));
                }
                VarLoc::Uninit => todo!(),
            };
        } else {
            panic!("cannot move uninitialized var {var} to register {target:?}");
        }
    }

    fn move_to_reg_protected(&mut self, var: Loc, protected: &[u8]) -> u8 {
        if let Some(curr) = self.vars.get_mut(&var) {
            match curr {
                VarLoc::Stack(slot) => {
                    let slot = *slot;
                    let reg = self.evict_oldest_protected(protected);
                    self.regs[reg as usize] = Some(var);
                    self.touch_reg(reg);
                    let curr = self.vars.get_mut(&var).expect("var declared");
                    *curr = VarLoc::Reg {
                        reg,
                        stack_slot: Some(slot),
                    };
                    self.opqueue
                        .push(AsmOp::Mov(OpTarget::Reg(reg), OpTarget::Stack(slot)));
                    self.opqueue
                        .push(AsmOp::Comment(format!(" ^^^ L_{var} := *(rsp + {slot})")));
                    reg
                }
                VarLoc::Reg { reg, .. } => {
                    let reg = *reg;
                    assert!(!protected.contains(&reg));
                    self.touch_reg(reg);
                    reg
                }
                VarLoc::Uninit => todo!(),
            }
        } else {
            let reg = self.evict_oldest_protected(protected);
            self.regs[reg as usize] = Some(var);
            self.touch_reg(reg);
            self.vars.insert(
                var,
                VarLoc::Reg {
                    reg,
                    stack_slot: None,
                },
            );
            reg as u8
        }
    }

    pub fn move_to_reg(&mut self, var: Loc) -> u8 {
        self.move_to_reg_protected(var, &[])
    }

    /// redefines var to be in reg. implicitly frees any var currently in reg. does not generate
    /// assembly
    pub fn force_reg(&mut self, var: Loc, reg: u8) {
        if let Some(to_free) = self.regs[reg as usize] {
            self.vars.remove(&to_free);
        }
        // let entry = self.vars.get_mut(&var).expect("var is nonexistent");
        let Some(entry) = self.vars.get_mut(&var) else { return };
        match entry {
            VarLoc::Stack(slot) => *entry = VarLoc::Reg { reg, stack_slot: Some(*slot) },
            VarLoc::Reg { reg: old_reg, stack_slot } => *old_reg = reg,
            VarLoc::Uninit => unimplemented!(),
        };
    }

    pub(crate) fn setup_call(&mut self, vars: &[Loc], call_type: CallType) -> Vec<AsmOp> {
        eprintln!("Setting up {call_type:?} with arguments {vars:?}");
        if let Some(args_regs) = call_type.arg_regs_idx() {
            eprintln!("\thas a specific calling convention");
            let arg_regs = &args_regs[..vars.len()];
            for (&var, &target) in vars.iter().zip(args_regs) {
                eprintln!("\tloading {target:?} <- {var:?}");
                self.move_to_specific_reg(var, target);
            }
        } else {
            eprintln!("\tHas no particular calling convention - moving everything to regs");
            let mut occupied = Vec::with_capacity(vars.len());
            for &var in vars {
                let reg = self.move_to_reg_protected(var, &occupied);
                occupied.push(reg);
                eprintln!("\tloading {reg:?} <- {var:?}");
            }
        }
        self.take_queue()
    }

    pub(crate) fn take_queue(&mut self) -> Vec<AsmOp> {
        // eprintln!("exporting assembly: {:?}", self.opqueue);
        std::mem::take(&mut self.opqueue)
    }

    fn var_location(&self, var: Loc) -> OpTarget {
        match self.vars[&var] {
            VarLoc::Stack(stack) => OpTarget::Stack(stack),
            VarLoc::Reg { reg, stack_slot } => OpTarget::Reg(reg),
            VarLoc::Uninit => todo!(),
        }
    }

    pub fn var_reg(&self, var: Loc) -> u8 {
        let reg = self
            .regs
            .iter()
            .position(|&v| v == Some(var))
            .expect("var_reg should only be called on vars in registers");
        reg as u8
    }

    pub fn free(&mut self, var: Loc) {
        let Some(&o_var) = self.vars.get(&var) else { return; };
        match o_var {
            VarLoc::Stack(s) => {
                self.free_stack.push(s);
            }
            VarLoc::Reg { reg, stack_slot } => {
                if let Some(slot) = stack_slot {
                    self.free_stack.push(slot);
                }
                self.regs[reg as usize] = None;
                self.times[reg as usize] = 0;
            }
            VarLoc::Uninit => todo!(),
        };
        self.opqueue
            .push(AsmOp::Comment(format!("( free L_{var} )")));
        self.vars.remove(&var);
    }

    pub fn stack_needed(&self) -> usize {
        self.max_stack
    }

    pub(crate) fn var_loc(&self, var: usize) -> OpTarget {
        self.vars.get(&var).expect("variable exists").clone().into()
    }
}

struct FixedPoint {
    time: usize,
    var: usize,
    reg: usize,
}

/// does graph coloring for register allocation.
/// need to build an interference graph
struct ColoringBook {
    max_reg: usize,
    fixed_points: Vec<FixedPoint>,
}
