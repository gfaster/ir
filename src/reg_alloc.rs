use crate::new_block_id;
use crate::Ctx;
use crate::Loc;
use crate::LocId;
use crate::Op;
use crate::OpInner;
use crate::Reg;
use crate::VarArray;
use crate::VarSet;
use crate::{AsmOp, CallType, OpTarget};

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::VecDeque;
use std::fmt::Debug;
use std::rc::Rc;

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
            VarLoc::Reg { reg } => write!(f, "{reg:?}", reg = Reg::from_idx(*reg).name()),
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

#[derive(Default, Clone)]
struct VarData {
    /// whether the variable will be preserved if it will be clobbered by a function call. Doesn't
    /// affect other preservation
    allow_clobber: bool,

    /// when moving variables to registers, prefer moving entires in here to the associated
    /// registers
    preferred_location: Option<u8>,
    locs: Vec<VarLoc>,
}

#[derive(Clone)]
pub struct RegAlloc {
    vars: BTreeMap<LocId, VarData>,
    regs: Vec<Option<LocId>>,
    times: Vec<usize>,
    age: usize,
    max_stack: usize,
    free_stack: Vec<usize>,
    opqueue: Vec<AsmOp>,

    /// map of variables allocated on the stack from the var id to (off, size)
    stack_allocs: BTreeMap<LocId, (usize, usize)>,
    vars_full: VarSet,

    /// whether each register has something that wants to be there
    soft_reserved: Vec<u32>,
}

impl RegAlloc {
    pub fn new(reg_cnt: usize, vars: VarSet) -> Self {
        Self {
            vars: BTreeMap::new(),
            regs: vec![None; reg_cnt],
            soft_reserved: vec![0; reg_cnt],
            times: vec![0; reg_cnt],
            age: 0,
            max_stack: 0,
            free_stack: Vec::new(),
            stack_allocs: BTreeMap::new(),
            opqueue: Vec::new(),
            vars_full: vars,
        }
    }

    fn var_name(&self, var: LocId) -> &str {
        if let Some(name) = self.vars_full.get(&var).and_then(|v| v.name.as_deref()) {
            return name;
        }
        "[[unknown var]]"
    }

    /// finds a free register. If a preferred location for var exists, then put it there. Also
    /// tries to respect other soft reserved registers
    fn free_reg(&self, var: LocId) -> Option<usize> {
        // if self.regs.iter().position(|x| x == &None).is_none() {
        //     eprintln!("{:?} has no free slots", self.regs);
        // }
        if let Some(prefer) = self.vars[&var].preferred_location {
            if self.regs[prefer as usize] == None {
                return Some(prefer as usize);
            }
        }
        self.regs
            .iter()
            .zip(&self.soft_reserved)
            .position(|(&content, &resv)| content.is_none() && resv == 0)
            .or_else(|| self.regs.iter().position(|&x| x == None))
    }

    fn is_var_init(&self, var: LocId) -> bool {
        self.vars.get(&var).is_some_and(|v| !v.locs.is_empty())
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
        let Some(var) = self.regs[reg as usize] else {
            return;
        };
        let Some(entry) = self.vars.get_mut(&var) else {
            return;
        };
        let Some(pos) = entry.locs.iter().position(|v| VarLoc::Reg { reg } == *v) else {
            return;
        };
        // self.opqueue
        //     .push(AsmOp::Comment(format!("deleted reg entry for var L_{var}")));
        entry.locs.swap_remove(pos);
    }

    fn backup_reg(&mut self, reg: u8) {
        if let Some(val) = self.regs[reg as usize] {
            self.backup_var(val)
        }
    }

    fn backup_var(&mut self, var: LocId) {
        let Some(entry) = self.vars.get(&var) else {
            panic!("var {var} is not defined and so can't be backed up");
        };

        if entry.locs.len() > 1 {
            // redundancy exists!
            return;
        }

        let source = entry.locs[0];

        if let Some(free_reg) = self.free_reg(var) {
            self.touch_reg(free_reg as u8);
            self.regs[free_reg] = Some(var);
            let new_loc = VarLoc::Reg {
                reg: free_reg as u8,
            };
            let entry = self.vars.entry(var).or_default().locs.push(new_loc);
            self.opqueue
                .push(AsmOp::Mov(OpTarget::Reg(free_reg as u8), source.into()));
            self.opqueue.push(AsmOp::Comment(format!(
                " ^^^ {var} from {reg:?} to unused location for backup",
                var = self.var_name(var),
                reg = source
            )));
        } else {
            // make sure this var exists on the stack
            let entry = self.vars.entry(var).or_default();
            if entry
                .locs
                .iter()
                .find(|x| matches!(x, VarLoc::Stack { .. }))
                .is_none()
            {
                let slot = {
                    if let Some(free) = self.free_stack.pop() {
                        free
                    } else {
                        let ret = self.max_stack;
                        self.max_stack += 8;
                        ret
                    }
                };
                entry.locs.push(VarLoc::Stack { slot });
                self.opqueue
                    .push(AsmOp::Mov(OpTarget::Stack(slot), source.into()));
                self.opqueue.push(AsmOp::Comment(format!(
                    " ^^^ {var} to stack for backup",
                    var = self.var_name(var)
                )));
            }
        }
    }

    /// frees register, making sure the current value is preserved
    fn evict_reg(&mut self, reg: u8) {
        let idx = reg as usize;
        if let Some(var) = self.regs[idx] {
            // self.opqueue.push(AsmOp::Comment(format!("evicting var {var} from {reg:?}",
            //     reg = Reg::from_idx(reg),
            //     var = self.var_name(var))
            // ));
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
            self.opqueue.push(AsmOp::Comment(format!(
                "{reg:?} was clobbered",
                reg = Reg::from_idx(reg).name()
            )));
        }
        self.remove_reg_entry(reg);
        self.regs[reg as usize] = None;
    }

    fn oldest(&self, var: LocId) -> u8 {
        self.oldest_protected(&[], var)
    }

    fn oldest_protected(&self, protected: &[u8], var: LocId) -> u8 {
        const TIME_LEWAY: usize = 5;
        let (min_time, &min_reg) = self
            .times
            .iter()
            .enumerate()
            .filter(|&(i, _)| !protected.contains(&(i as u8)))
            .min_by_key(|&(_i, &a)| a)
            .expect("nonempty array and protected leaves room");
        if let Some(prefer) = self.vars.get(&var).and_then(|v| v.preferred_location) {
            if self.times[prefer as usize] <= min_time + TIME_LEWAY {
                return prefer;
            }
        }
        if self.soft_reserved[min_reg] == 0 {
            return min_reg as u8;
        }
        self.times
            .iter()
            .zip(&self.soft_reserved)
            .position(|(&born, &resv)| born <= min_time + TIME_LEWAY && resv == 0)
            .map_or(min_reg as u8, |p| p as u8)
        // eprintln!("out of {times:?}, {ret} ({reg:?}) is the oldest", times = self.times, reg = Reg::from_idx(ret));
    }

    fn evict_oldest(&mut self, var: LocId) -> u8 {
        let oldest = self.oldest_protected(&[], var);
        self.evict_reg(oldest);
        oldest
    }

    fn evict_oldest_protected(&mut self, protected: &[u8], var: LocId) -> u8 {
        let oldest = self.oldest_protected(protected, var);
        self.evict_reg(oldest as u8);
        oldest
    }

    /// find the fastest place we can copy var from
    fn fastest_source(&self, var: LocId) -> VarLoc {
        let Some(entry) = self.vars.get(&var) else {
            panic!("cannot source uninitialized var {var}");
        };
        *entry
            .locs
            .iter()
            .reduce(|l, r| match (l, r) {
                (_, VarLoc::Reg { reg }) => r,
                (VarLoc::Reg { reg }, _) => l,
                (VarLoc::Uninit, _) | (_, VarLoc::Uninit) => todo!(),
                _ => l,
            })
            .expect("source exists")
    }

    fn move_to_specific_reg(&mut self, var: LocId, target: u8) {
        // self.opqueue.push(AsmOp::Comment(format!("moving var L_{var} to {reg:?}", reg = Reg::from_idx(target))));
        // self.opqueue.push(AsmOp::Comment(format!("> current: {:?}", self.vars.get(&var))));
        self.touch_reg(target);
        if self.regs[target as usize] == Some(var) {
            // self.opqueue.push(AsmOp::Comment(format!(" {var} is already in {reg:?}",
            //     reg = Reg::from_idx(target),
            //     var = self.var_name(var)
            // )));
            return;
        };
        self.evict_reg(target);
        if self.is_var_init(var) {
            let source = self.fastest_source(var);
            self.vars
                .entry(var)
                .or_default()
                .locs
                .push(VarLoc::Reg { reg: target });
            self.opqueue
                .push(AsmOp::Mov(OpTarget::Reg(target), source.into()));
            self.opqueue.push(AsmOp::Comment(format!(" ^^^ {var}",)));
        } else {
            self.vars
                .entry(var)
                .or_default()
                .locs
                .push(VarLoc::Reg { reg: target });
            self.opqueue.push(AsmOp::Comment(format!(
                "uninitialized {var} to {reg:?}",
                reg = Reg::from_idx(target).name(),
            )));
        }
        self.regs[target as usize] = Some(var);
    }

    fn move_to_reg_protected(&mut self, var: &Loc, protected: &[u8]) -> u8 {
        let varid = var.id;
        if let Some(reg) = self.var_reg(varid) {
            debug_assert!(!protected.contains(&reg));
            // self.opqueue
            //     .push(AsmOp::Comment(format!(" {var} already in registers", var = self.var_name(var))));
            return reg;
        }
        if !self.is_var_init(varid) {
            let target = self.evict_oldest_protected(protected, varid);
            self.touch_reg(target);
            self.vars
                .entry(varid)
                .or_default()
                .locs
                .push(VarLoc::Reg { reg: target });
            self.regs[target as usize] = Some(varid);
            return target;
        }
        let source = self.fastest_source(varid).into();
        let target = self.evict_oldest_protected(protected, varid);
        self.touch_reg(target);
        self.opqueue.push(AsmOp::Mov(OpTarget::Reg(target), source));
        self.opqueue.push(AsmOp::Comment(format!(" ^^^ {var}",)));
        self.regs[target as usize] = Some(varid);
        target
    }

    pub fn move_to_reg(&mut self, var: &Loc) -> u8 {
        self.move_to_reg_protected(var, &[])
    }

    /// (re)defines var to be in reg. implicitly frees any var currently in reg. reg is now
    /// considedred to be the source of truth for var, so any others are invalidated
    ///
    /// does not generate assembly
    pub fn force_reg(&mut self, var: &Loc, reg: u8) {
        let var = var.id;
        self.touch_reg(reg);
        if self.regs[reg as usize] != Some(var) {
            self.remove_reg_entry(reg);
            self.regs[reg as usize] = Some(var);
        }
        let v = self.vars.entry(var).or_default();
        for loc in &v.locs {
            match loc {
                VarLoc::Stack { slot } => self.free_stack.push(*slot),
                VarLoc::Reg { reg } => self.regs[*reg as usize] = None,
                VarLoc::Uninit => todo!(),
            }
        }
        v.locs = vec![VarLoc::Reg { reg }];
    }

    pub fn print_regs(&self) {
        eprintln!("registers:");
        for r in 0..self.regs.len() {
            eprint!("{reg:>5} = ", reg = Reg::from_idx(r as u8).name());
            if let Some(var) = self.regs[r] {
                eprintln!("{var}");
            } else {
                eprintln!("[[EMPTY]]");
            }
        }
        eprintln!()
    }

    pub fn print_vars(&self) {
        eprintln!("variables:");
        for (var, locs) in self.vars.iter() {
            eprint!("{var:>15} in");
            for loc in &locs.locs {
                match loc {
                    VarLoc::Stack { slot } => eprint!(" [rsp + {slot}]"),
                    VarLoc::Reg { reg } => eprint!(" {reg}", reg = Reg::from_idx(*reg).name()),
                    VarLoc::Uninit => todo!(),
                }
            }
            eprintln!();
        }
        eprintln!()
    }

    pub(crate) fn setup_call(&mut self, vars: &[Loc], call_type: CallType) -> Vec<AsmOp> {
        // eprintln!("Setting up {call_type:?} with arguments {vars:#?}");
        if let Some(args_regs) = call_type.arg_regs_idx() {
            // eprintln!("\thas a specific calling convention");
            let arg_regs = &args_regs[..vars.len()];
            for (var, &target) in vars.iter().zip(args_regs) {
                // eprintln!(
                //     "\tloading {reg:?} <- {var}",
                //     reg = Reg::from_idx(target),
                // );
                self.move_to_specific_reg(var.id, target);
            }
            for (&clobber) in call_type.clobers_idx() {
                if !self.regs[clobber as usize]
                    .and_then(|v| self.vars.get(&v))
                    .is_some_and(|e| e.allow_clobber)
                {
                    self.backup_reg(clobber);
                }
            }
        } else {
            eprintln!("\tHas no particular calling convention - moving everything to regs");
            let mut occupied = Vec::with_capacity(vars.len());
            for var in vars {
                let reg = self.move_to_reg_protected(&var, &occupied);
                occupied.push(reg);
                eprintln!("\tloading {reg:?} <- {var:?}");
            }
        }
        self.take_queue()
    }

    /// allow this var to be clobbered (only happens via specific clobber calls)
    pub(crate) fn allow_clobber(&mut self, var: &Loc) {
        if let Some(entry) = self.vars.get_mut(&var.id) {
            entry.allow_clobber = true;
        }
    }

    /// hints that vars should move to the registers for the call if given the opportunity
    pub(crate) fn hint_call(&mut self, vars: &[Loc], call_type: CallType) {
        if let Some(args_regs) = call_type.arg_regs_idx() {
            let arg_regs = &args_regs[..vars.len()];
            for (var, &target) in vars.iter().zip(args_regs) {
                let pref = &mut self.vars.entry(var.id).or_default().preferred_location;
                if let Some(old_pref) = pref {
                    self.soft_reserved[*old_pref as usize] -= 1;
                }
                self.soft_reserved[target as usize] += 1;
                *pref = Some(target);
            }
        } else {
            todo!()
        }
    }

    pub(crate) fn take_queue(&mut self) -> Vec<AsmOp> {
        // eprintln!("exporting assembly: {:?}", self.opqueue);
        std::mem::take(&mut self.opqueue)
    }

    /// get the var held in reg
    pub fn reg_var(&self, reg: u8) -> Option<&Loc> {
        self.regs[reg as usize].map(|id| self.vars_full.get(&id).expect("register has valid LocId"))
    }

    /// get the first reg that holds var
    pub fn var_reg(&self, var: LocId) -> Option<u8> {
        self.regs
            .iter()
            .position(|&v| v == Some(var))
            .map(|i| i as u8)
    }

    pub fn free(&mut self, var: &Loc) {
        let Some(vars) = self.vars.get_mut(&var.id) else {
            return;
        };
        if let Some(prefer) = vars.preferred_location.take() {
            self.soft_reserved[prefer as usize] -= 1;
        }
        for o_var in std::mem::take(&mut vars.locs) {
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
            .push(AsmOp::Comment(format!("( free {var} )",)));
        self.vars.remove(&var.id);
    }

    pub fn stack_needed(&self) -> usize {
        self.max_stack
    }

    pub fn alloca(&mut self, var: &Loc, size: usize) -> usize {
        eprintln!("allocating {var}");
        let alloc_size = ((size + 7) / 8) * 8;
        assert_eq!(alloc_size % 8, 0);
        if size <= 8 {
            if let Some(slot) = self.free_stack.pop() {
                self.stack_allocs.insert(var.id, (slot, alloc_size));
                // dbg!(&self.stack_allocs);
                return slot;
            };
        }
        let ret = self.max_stack;
        self.stack_allocs.insert(var.id, (ret, 8));
        self.max_stack += alloc_size;
        // dbg!(&self.stack_allocs);
        ret
    }

    /// get the location of var
    pub fn get_alloc(&self, var: &Loc) -> Option<usize> {
        eprintln!("fetching {var}");
        // dbg!(&self.stack_allocs);
        self.stack_allocs.get(&var.id).map(|&(pos, _size)| pos)
    }

    pub fn freea(&mut self, var: &Loc) {
        let Some((pos, size)) = self.stack_allocs.remove(&var.id) else {
            panic!("attempt to free allocated var: {var}",)
        };
        if size == 0 {
            return;
        }
        let mut size = (((size + 7) / 8) * 8 - 8) as isize;
        while size >= 0 {
            assert!(!self.free_stack.contains(&(pos + size as usize)));
            self.free_stack.push(pos + size as usize);
            size -= 8;
        }
    }

    pub(crate) fn apply_op(&mut self, op: &RegAllocOp) {
        todo!()
    }

    pub(crate) fn organize_block(block: Vec<Op>, args: &[Loc], ctx: &Ctx) -> Vec<AsmOp> {
        let vars = &ctx.vars;
        let mut alloc = Self::new(Reg::count(), Rc::clone(vars));
        let mut last_occurrence = Vec::new();
        let mut upcoming_calls: Vec<(CallType, Vec<Loc>)> = Vec::new();
        // validate block and determine lifetimes
        for (i, op) in block.iter().enumerate() {
            for var in op.vars_referenced() {
                if var.id >= last_occurrence.len() {
                    last_occurrence.resize(var.id + 1, usize::MAX);
                }
                last_occurrence[var.id] = i;
            }
            if let Some(call) = op.call_type() {
                if i != 0 {
                    upcoming_calls.push((call, op.vars_referenced().into_iter().cloned().collect()))
                }
            }
            if let OpInner::Block { .. } = op.inner {
                if i != 0 {
                    panic!("Cannot have block in another block")
                }
            }
            if op.is_branch() {
                assert_eq!(i + 1, block.len(), "can only branch at end of block")
            }
        }
        let mut ret = VecDeque::with_capacity(block.len() * 2);

        for (arg, &reg) in args.iter().zip(
            CallType::Block
                .arg_regs_idx()
                .expect("block has calling conv"),
        ) {
            alloc.force_reg(&arg, reg)
        }

        for hint in &upcoming_calls {
            // do it once before to try and saturate the registers before doing a bunch of
            // shuffling movs
            alloc.hint_call(&hint.1, hint.0)
        }

        let mut hint_it = upcoming_calls.into_iter();

        if let Some(hint) = hint_it.next() {
            alloc.hint_call(&hint.1, hint.0);
        }

        for (i, op) in block.into_iter().enumerate() {
            for &clobbered in op.regs_clobbered() {
                if let Some(var) = alloc.reg_var(clobbered).cloned() {
                    if last_occurrence[var.id] > i {
                        alloc.backup_reg(clobbered);
                    } else {
                        alloc.allow_clobber(&var);
                    }
                }
            }
            for var in op.vars_clobbered() {
                if last_occurrence[var.id] > i {
                    alloc.backup_var(var.id);
                } else {
                    alloc.allow_clobber(&var);
                }
            }
            ret.extend(op.to_asm(&mut alloc, ctx));
            for &clobbered in op.regs_clobbered() {
                alloc.clobber_reg(clobbered);
            }
            for vref in op.vars_referenced() {
                if last_occurrence[vref.id] <= i {
                    alloc.free(vref);
                }
            }
            // we just made a call, start setting up next call
            if op.call_type().is_some() {
                if let Some(hint) = hint_it.next() {
                    alloc.hint_call(&hint.1, hint.0);
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
        let block_id = new_block_id();
        ret.push_front(AsmOp::BlockBegin(block_id));
        ret.push_back(AsmOp::BlockEnd(block_id));
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
