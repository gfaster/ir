use crate::Loc;
use crate::Op;
use crate::Reg;
use crate::{AsmOp, CallType, OpTarget};

use std::collections::BTreeMap;
use std::collections::VecDeque;
use std::fmt::Debug;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum VarLoc {
    Stack { slot: usize },
    Reg { reg: u8 },
    Uninit,
}

impl Debug for VarLoc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VarLoc::Stack { slot } => write!(f, "stack location {slot}"),
            VarLoc::Reg { reg } => write!(f, "register {reg:?}", reg = Reg::from_idx(*reg)),
            VarLoc::Uninit => write!(f, "uninitialized var"),
        }
    }
}

#[derive(Debug)]
pub(crate) enum RegAllocOp {
    ReserveReg(u8),
    MakeRedundantVal(Loc),
    MakeRedundantReg(u8),
    SetupCall(CallType, Vec<Loc>),
    FreeReg(u8),
}

// #[derive(Clone, Copy, PartialEq, Eq)]
// enum RegEntry {
//     /// the entry can be overwritten and reading from it is undefined behavior
//     Garbage,
//     /// the entry is nonprimary and can be overwritten.
//     Aliases(Loc),
//     /// the entry is the only location for this variable
//     Occupied(Loc),
// }


#[derive(Clone)]
pub struct RegAlloc {
    vars: BTreeMap<Loc, Vec<VarLoc>>,
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

    fn is_var_init(&self, var: Loc) -> bool {
        self.vars.get(&var).is_some_and(|v| !v.is_empty())
    }

    pub fn touch_reg(&mut self, reg: u8) {
        // eprintln!("touching {reg:?}", reg = Reg::from_idx(reg));
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

    /// simply removes the entry in vars of the contents of reg without touching anything else
    fn remove_reg_entry(&mut self, reg: u8) {
        let Some(var) = self.regs[reg as usize] else { return };
        let Some(entry) = self.vars.get_mut(&var) else { return };
        let Some(pos) = entry.iter().position(|v| VarLoc::Reg { reg } == *v) else { return };
        // self.opqueue
        //     .push(AsmOp::Comment(format!("deleted reg entry for var L_{var}")));
        entry.swap_remove(pos);
    }

    fn backup_reg(&mut self, reg: u8) {
        if let Some(val) = self.regs[reg as usize] {
            self.backup_val(val)
        }
    }

    fn backup_val(&mut self, var: Loc) {
        let Some(entry) = self.vars.get(&var) else {
            panic!("var {var} is not defined and so can't be backed up");
        };

        if entry.len() > 1 {
            // redundancy exists!
            return;
        }

        let source = entry[0];

        if let Some(free_reg) = self.free_reg() {
            self.touch_reg(free_reg as u8);
            self.regs[free_reg] = Some(var);
            let new_loc = VarLoc::Reg {
                reg: free_reg as u8,
            };
            let entry = self.vars.entry(var).or_default().push(new_loc);
            self.opqueue.push(AsmOp::Mov(
                OpTarget::Reg(free_reg as u8),
                source.into(),
            ));
            self.opqueue.push(AsmOp::Comment(format!(
                " ^^^ L_{var} from {reg:?} to free for backup",
                reg = source
            )));
        } else {
            // make sure this var exists on the stack
            let entry = self.vars.entry(var).or_default();
            if entry.iter().find(|x| matches!(x, VarLoc::Stack {..})).is_none() {
                let slot = {
                    if let Some(free) = self.free_stack.pop() {
                        free
                    } else {
                        let ret = self.max_stack;
                        self.max_stack += 8;
                        ret
                    }
                };
                entry.push(VarLoc::Stack { slot });
                self.opqueue
                    .push(AsmOp::Mov(OpTarget::Stack(slot), source.into()));
                self.opqueue
                    .push(AsmOp::Comment(format!(" ^^^ L_{var} to stack for backup")));
            }
        }
    }

    /// frees register, making sure the current value is preserved
    fn evict_reg(&mut self, reg: u8) {
        let idx = reg as usize;
        if let Some(var) = self.regs[idx] {
            self.opqueue.push(AsmOp::Comment(format!("evicting var L_{var} from {reg:?}", reg = Reg::from_idx(reg))));
            let Some(entry) = self.vars.get(&var) else {
                panic!("entry for var {var} does not exist, but exists in reg {reg}");
            };
            self.backup_reg(reg);
        }
        self.remove_reg_entry(reg);
        self.regs[reg as usize] = None;
    }

    pub fn clobber_reg(&mut self, reg: u8) {
        if let Some(var) = self.regs[reg as usize] {
            self.opqueue.push(AsmOp::Comment(format!("{reg:?} was clobbered", reg = Reg::from_idx(reg))));
        }
        self.remove_reg_entry(reg);
        self.regs[reg as usize] = None;
    }

    fn oldest(&self) -> u8 {
        self.oldest_protected(&[])
    }

    fn oldest_protected(&self, protected: &[u8]) -> u8 {
        let ret = self.times
            .iter()
            .enumerate()
            .filter(|&(i, _)| !protected.contains(&(i as u8)))
            .min_by_key(|&(_i, &a)| a)
            .expect("nonempty array and protected leaves room")
            .0 as u8;
        // eprintln!("out of {times:?}, {ret} ({reg:?}) is the oldest", times = self.times, reg = Reg::from_idx(ret));
        ret
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

    /// find the fastest place we can copy var from
    fn fastest_source(&self, var: Loc) -> VarLoc {
        let Some(entry) = self.vars.get(&var) else {
            panic!("cannot source uninitialized var {var}");
        };
        *entry.iter().reduce(|l, r| {
            match (l, r) {
                (_, VarLoc::Reg { reg }) => r,
                (VarLoc::Reg { reg }, _) => l,
                (VarLoc::Uninit, _) | (_, VarLoc::Uninit) => todo!(),
                _ => l
            }
        }).expect("source exists")
    }

    fn move_to_specific_reg(&mut self, var: Loc, target: u8) {
        // self.opqueue.push(AsmOp::Comment(format!("moving var L_{var} to {reg:?}", reg = Reg::from_idx(target))));
        // self.opqueue.push(AsmOp::Comment(format!("> current: {:?}", self.vars.get(&var))));
        self.touch_reg(target);
        if self.regs[target as usize] == Some(var) {
            self.opqueue.push(AsmOp::Comment(format!(" L_{var} is already in {reg:?}", reg = Reg::from_idx(target))));
            return;
        };
        self.evict_reg(target);
        if self.is_var_init(var) {
            let source = self.fastest_source(var);
            self.vars.entry(var).or_default().push(VarLoc::Reg { reg: target });
            self.opqueue
                .push(AsmOp::Mov(OpTarget::Reg(target), source.into()));
            self.opqueue
                .push(AsmOp::Comment(format!(" ^^^ L_{var}")));
        } else {
            self.vars.entry(var).or_default().push(VarLoc::Reg { reg: target });
            self.opqueue
                .push(AsmOp::Comment(format!("uninitialized L_{var} to {reg:?}", reg = Reg::from_idx(target))));
        }
        self.regs[target as usize] = Some(var);
    }

    fn move_to_reg_protected(&mut self, var: Loc, protected: &[u8]) -> u8 {
        if let Some(reg) = self.var_reg(var) {
            debug_assert!(!protected.contains(&reg));
            self.opqueue
                .push(AsmOp::Comment(format!(" L_{var} already in registers")));
            return reg;
        }
        if !self.is_var_init(var) {
            let target = self.evict_oldest_protected(protected);
            self.touch_reg(target);
            self.vars.entry(var).or_default().push(VarLoc::Reg { reg: target });
            self.regs[target as usize] = Some(var);
            return target
        }
        let source = self.fastest_source(var).into();
        let target = self.evict_oldest_protected(protected);
        self.touch_reg(target);
        self.opqueue
            .push(AsmOp::Mov(OpTarget::Reg(target), source));
        self.opqueue
            .push(AsmOp::Comment(format!(" ^^^ L_{var}")));
        self.regs[target as usize] = Some(var);
        target
    }

    pub fn move_to_reg(&mut self, var: Loc) -> u8 {
        self.move_to_reg_protected(var, &[])
    }

    /// (re)defines var to be in reg. implicitly frees any var currently in reg. does not generate
    /// assembly
    pub fn force_reg(&mut self, var: Loc, reg: u8) {
        self.remove_reg_entry(reg);
        self.touch_reg(reg);
        let v = self.vars.entry(var).or_default();
        if !v.contains(&VarLoc::Reg { reg }) {
            v.push(VarLoc::Reg { reg });
        }
        self.regs[reg as usize] = Some(var);
    }

    pub(crate) fn setup_call(&mut self, vars: &[Loc], call_type: CallType) -> Vec<AsmOp> {
        eprintln!("Setting up {call_type:?} with arguments {vars:?}");
        if let Some(args_regs) = call_type.arg_regs_idx() {
            // eprintln!("\thas a specific calling convention");
            let arg_regs = &args_regs[..vars.len()];
            for (&var, &target) in vars.iter().zip(args_regs) {
                eprintln!("\tloading {reg:?} <- {var:?}", reg = Reg::from_idx(target));
                self.move_to_specific_reg(var, target);
            }
            for (&clobber) in call_type.clobers_idx() {
                self.backup_reg(clobber);
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

    pub fn var_reg(&self, var: Loc) -> Option<u8> {
        self
            .regs
            .iter()
            .position(|&v| v == Some(var))
            .map(|i| i as u8)
    }

    pub fn free(&mut self, var: Loc) {
        let Some(vars) = self.vars.get_mut(&var) else { return; };
        for o_var in std::mem::take(vars) {
            match o_var {
                VarLoc::Stack { slot: s } => {
                    self.free_stack.push(s);
                }
                VarLoc::Reg { reg } => {
                    self.regs[reg as usize] = None;
                    self.times[reg as usize] = 0;
                }
                VarLoc::Uninit => todo!(),
            };
        }
        self.opqueue
            .push(AsmOp::Comment(format!("( free L_{var} )")));
        self.vars.remove(&var);
    }

    pub fn stack_needed(&self) -> usize {
        self.max_stack
    }

    pub(crate) fn apply_op(&mut self, op: &RegAllocOp) {
        todo!()
    }

    pub(crate) fn organize_block(block: Vec<Op>, args: &[Loc]) -> Vec<AsmOp> {
        let mut alloc = Self::new(Reg::count());
        let mut last_occurrence = Vec::new();
        for (i, op) in block.iter().enumerate() {
            for var in op.vars_referenced() {
                if var >= last_occurrence.len() {
                    last_occurrence.resize(var + 1, usize::MAX);
                }
                last_occurrence[var] = i;
            }
            if let Op::Block { .. } = op {
                if i != 0 {
                    panic!("Cannot have block in another block")
                }
            }
            if op.is_branch() {
                assert_eq!(i + 1, block.len(), "can only branch at end of block")
            }
        }
        let mut ret = VecDeque::with_capacity(block.len() * 2);

        for (&arg, &reg) in args.iter().zip(CallType::Block.arg_regs_idx().expect("block has calling conv")) {
            alloc.force_reg(arg, reg)
        }

        for (i, op) in block.into_iter().enumerate() {
            for &clobbered in op.regs_clobbered() {
                alloc.backup_reg(clobbered);
            }
            ret.extend(op.to_asm(&mut alloc));
            for &clobbered in op.regs_clobbered() {
                alloc.clobber_reg(clobbered);
            }
            for vref in op.vars_referenced() {
                if last_occurrence[vref] <= i {
                    alloc.free(vref);
                }
            }
        }
        ret.extend(alloc.take_queue());

        if alloc.stack_needed() > 0 {
            ret.push_front(AsmOp::Mov(
                OpTarget::Reg(Reg::Rbp.idx()),
                OpTarget::Reg(Reg::Rsp.idx()),
            ));
            ret.push_front(AsmOp::Push(Reg::Rbp));
            ret.push_back(AsmOp::Pop(Reg::Rbp));
        }
        ret.into()
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
