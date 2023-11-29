#![allow(unused)]

// MVP FIRST!!! 

use std::collections::BTreeMap;
use std::fmt::Write as FmtWrite;
use std::io::Write;

/// symbol id currently global scope and unique
type Id = usize;

/// Id of a local
type Loc = usize;

#[derive(Debug)]
enum Val {
    GlobalLabel(Id),
    LocalLabel(Loc),
    Literal(u64),
}


struct GlobalData {
    name: Box<str>,
    data: Box<[u8]>
}

impl GlobalData {
    fn to_asm(&self) -> String {
        let mut out = String::new();
        writeln!(out, "{name}:", name = self.name).unwrap();
        for byte in self.data.iter() {
            writeln!(out, ".byte {byte}").unwrap();
        }
        writeln!(out).unwrap();
        out
    }
}

struct Routine {
    name: Box<str>,
    ops: Vec<Op>
}

impl Routine {
    fn to_asm(&self, globals: &[GlobalData]) -> String {
        let mut out = String::new();
        let mut alloc = RegAlloc::new();
        let mut last_occurrence = Vec::new();
        for (i, op) in self.ops.iter().enumerate() {
            for &var in op.vars_referenced() {
                if var >= last_occurrence.len() {
                    last_occurrence.resize(var + 1, usize::MAX);
                }
                last_occurrence[var] = i;
            }
        }
        writeln!(out, "{name}:", name = self.name).unwrap();
        for (i, op) in self.ops.iter().enumerate() {
            let asm = op.to_asm(&mut alloc);
            for instr in asm {
                instr.write_op(&mut out, globals);
            }
            for &var in op.vars_referenced() {
                if last_occurrence[var] <= i {
                    alloc.free(var);
                }
            }
        }
        writeln!(out).unwrap();
        out
    }
}

#[derive(Debug)]
enum OpTarget {
    Literal(u64),
    Reg(Reg),
    Label(usize),
    Stack(usize),
}

impl OpTarget {
    fn to_display(&self, w: &mut impl std::fmt::Write, globals: &[GlobalData], ptr_load: bool) -> std::fmt::Result {
        match self {
            OpTarget::Literal(x) => write!(w, "{x}"),
            OpTarget::Reg(r) => write!(w, "{}", r.name()),
            OpTarget::Label(i) if ptr_load => write!(w, "qword ptr [rip + {}]", globals[*i].name),
            OpTarget::Label(i) => write!(w, "[rip + {}]", globals[*i].name),
            OpTarget::Stack(s) if ptr_load => write!(w, "qword ptr [rsp + {s}]"),
            OpTarget::Stack(s) => write!(w, "[rsp + {s}]"),
        }
    }
}

#[derive(Debug)]
enum AsmOp {
    Mov(OpTarget, OpTarget),
    Lea(Reg, OpTarget),
    Syscall
}

impl AsmOp {
    fn write_op(&self, w: &mut impl std::fmt::Write, globals: &[GlobalData]) -> std::fmt::Result {
        match self {
            AsmOp::Mov(dst, src) => {
                write!(w, "    mov ")?;
                dst.to_display(w, globals, true)?;
                write!(w, ", ")?;
                src.to_display(w, globals, true)?;
                writeln!(w)
            },
            AsmOp::Lea(dst, addr) => {
                write!(w, "    lea {}", dst.name())?;
                write!(w, ", ")?;
                addr.to_display(w, globals, false)?;
                writeln!(w)
            },
            AsmOp::Syscall => writeln!(w, "    syscall"),
        }
    }
}

enum Op {
    Call{id: Id, args: Vec<Loc>},
    Load{loc: Loc, val: Val},
}

impl Op {
    fn to_asm(&self, alloc: &mut RegAlloc) -> Vec<AsmOp> {
        match self {
            Op::Call { id, args } => {
                if *id == 0 {
                    let mut ret = alloc.setup_call(&args, CallType::Syscall);
                    ret.push(AsmOp::Syscall);
                    // eprintln!("syscall complete");
                    ret
                } else {
                    todo!()
                }
            },
            Op::Load { loc, val } => {
                let reg = Reg::from_loc(*loc);
                let ret = match val {
                    Val::GlobalLabel(g) => {
                        let reg = alloc.move_to_reg(*loc);
                        let mut ret = alloc.take_queue();
                        ret.push(AsmOp::Lea(reg, OpTarget::Label(*g)));
                        // eprintln!("load of {val:?} to {loc} ({reg:?}) complete");
                        ret
                    },
                    Val::LocalLabel(_) => todo!(),
                    Val::Literal(v) => {
                        let reg = alloc.move_to_reg(*loc);
                        let mut ret = alloc.take_queue();
                        ret.push(AsmOp::Mov(OpTarget::Reg(reg), OpTarget::Literal(*v)));
                        // eprintln!("load of {val:?} to {loc} ({reg:?}) complete");
                        ret
                    }
                };
                ret
            },
        }
    }

    fn is_var_referenced(&self, var: Loc) -> bool {
        match self {
            Op::Call { id, args } => args.contains(&var),
            Op::Load { loc, val } => *loc == var,
        }
    }

    fn vars_referenced(&self) -> &[Loc] {
        match self {
            Op::Call { id, args } => &args,
            Op::Load { loc, val } => std::slice::from_ref(loc),
        }
    }
}

enum VarLoc {
    Stack(usize),
    Reg { reg: Reg, stack_slot: Option<usize> },
    Uninit,
}

struct RegAlloc{
    vars: BTreeMap<Loc, VarLoc>,
    regs: [Option<Loc>; Reg::count()],
    times: [usize; Reg::count()],
    age: usize,
    max_stack: usize,
    free_stack: Vec<usize>,
    opqueue: Vec<AsmOp>,
}

impl RegAlloc {
    fn new() -> Self {
        Self {
            vars: BTreeMap::new(),
            regs: [None; Reg::count()],
            times: [0; Reg::count()],
            age: 0,
            max_stack: 0,
            free_stack: Vec::new(),
            opqueue: Vec::new(),
        }
    }

    fn free_reg(&self) -> Option<usize> {
        self.regs.iter().position(|x| x == &None)
    }

    fn touch_reg(&mut self, reg: Reg) {
        self.age += 1;
        self.times[reg.idx()] = self.age
    }

    fn reserve_stack(&mut self) -> usize {
        if let Some(free) = self.free_stack.pop() {
            return free;
        }
        let ret = self.max_stack;
        self.max_stack += 8;
        ret
    }

    /// allocates space for var on the stack and evicts it if need be
    fn evict_var(&mut self, var: Loc) {
        if let Some(entry) = self.vars.get_mut(&var) {
            let VarLoc::Reg { reg: r, stack_slot} = entry else {
                // already on the stack
                return;
            };
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
            // eprintln!("evicting {var:?} from {r:?} to stack offset {slot:?}");
            self.opqueue.push(AsmOp::Mov(OpTarget::Stack(slot), OpTarget::Reg(*r)));
            *entry = VarLoc::Stack(slot);
        }
    }

    /// frees register
    fn evict_reg(&mut self, reg: Reg) {
        let idx = reg.idx();
        if let Some(var) = self.regs[idx] {
            self.evict_var(var);
        }
    }

    fn evict_oldest(&mut self) -> usize {
        let oldest = self.times.iter().enumerate().min_by_key(|&(_i, &a)| a).expect("nonempty array").0;
        self.evict_reg(Reg::from_loc(oldest));
        oldest
    }

    fn evict_oldest_protected(&mut self, protected: &[Reg]) -> usize {
        let oldest = self.times.iter()
            .enumerate()
            .filter(|&(i, _)| !protected.contains(&Reg::from_loc(i)))
            .min_by_key(|&(_i, &a)| a)
            .expect("nonempty array and protected leaves room").0;
        self.evict_reg(Reg::from_loc(oldest));
        oldest
    }

    fn move_to_specific_reg(&mut self, var: Loc, target: Reg) {
        self.touch_reg(target);
        if let Some(curr) = self.vars.get_mut(&var) {
            match curr {
                VarLoc::Stack(slot) => {
                    let slot = *slot;
                    let reg = self.evict_reg(target);
                    self.regs[target.idx()] = Some(var);
                    let curr = self.vars.get_mut(&var).expect("var declared");
                    *curr = VarLoc::Reg { reg: target, stack_slot: Some(slot) };
                    self.opqueue.push(AsmOp::Mov(OpTarget::Reg(target), OpTarget::Stack(slot)));
                },
                VarLoc::Reg { reg, .. } => {
                    let reg = *reg;
                    if reg == target {
                        // already in the right spot, don't need to do anything
                        return;
                    }
                    self.evict_reg(target);
                    self.opqueue.push(AsmOp::Mov(OpTarget::Reg(target), OpTarget::Reg(reg)));
                },
                VarLoc::Uninit => todo!(),
            };
        } else {
            panic!("cannot move uninitialized var {var} to register {target:?}");
        }
    }

    fn move_to_reg_protected(&mut self, var: Loc, protected: &[Reg]) -> Reg {
        if let Some(curr) = self.vars.get_mut(&var) {
            match curr {
                VarLoc::Stack(slot) => {
                    let slot = *slot;
                    let reg = self.evict_oldest_protected(protected);
                    self.regs[reg] = Some(var);
                    let reg = Reg::from_loc(reg);
                    self.touch_reg(reg);
                    let curr = self.vars.get_mut(&var).expect("var declared");
                    *curr = VarLoc::Reg { reg, stack_slot: Some(slot) };
                    self.opqueue.push(AsmOp::Mov(OpTarget::Reg(reg), OpTarget::Stack(slot)));
                    reg
                },
                VarLoc::Reg { reg, .. } => {
                    let reg = *reg;
                    assert!(!protected.contains(&reg));
                    self.touch_reg(reg);
                    reg
                },
                VarLoc::Uninit => todo!(),
            }
        } else {
            let reg = self.evict_oldest_protected(protected);
            self.regs[reg] = Some(var);
            let reg = Reg::from_loc(reg);
            self.touch_reg(reg);
            self.vars.insert(var, VarLoc::Reg { reg, stack_slot: None });
            reg
        }
    }

    fn move_to_reg(&mut self, var: Loc) -> Reg {
        self.move_to_reg_protected(var, &[])
    }

    fn setup_call(&mut self, vars: &[Loc], call_type: CallType) -> Vec::<AsmOp> {
        eprintln!("Setting up {call_type:?} with arguments {vars:?}");
        if let Some(args_regs) = call_type.arg_regs() {
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

    fn take_queue(&mut self) -> Vec<AsmOp> {
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

    fn var_reg(&self, var: Loc) -> Reg {
        let reg = self.regs.iter().position(|&v| v == Some(var)).expect("var_reg should only be called on vars in registers");
        Reg::from_loc(reg)
    }

    fn free(&mut self, var: Loc) {
        match self.vars[&var] {
            VarLoc::Stack(s) => {
                self.free_stack.push(s);
            },
            VarLoc::Reg { reg, stack_slot } => {
                if let Some(slot) = stack_slot {
                    self.free_stack.push(slot);
                }
                self.regs[reg.idx()] = None;
                self.times[reg.idx()] = 0;
            },
            VarLoc::Uninit => todo!(),
        };
        self.vars.remove(&var);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CallType {
    Syscall,
    Op2,
    Op3,
}

impl CallType {
    fn arg_regs(&self) -> Option<&[Reg]> {
        match self {
            CallType::Syscall => Some(&[Reg::Rax, Reg::Rdi, Reg::Rsi, Reg::Rdx]),
            CallType::Op2 => None,
            CallType::Op3 => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Reg {
    Rax,
    Rcx,
    Rdx,
    Rsi,
    Rdi,
    Rsp,
    Rbp,
}

impl Reg {
    const fn count() -> usize {
        5
    }

    fn from_loc(loc: Loc) -> Self {
        match loc {
            0 => Self::Rax,
            1 => Self::Rcx,
            2 => Self::Rdx,
            3 => Self::Rsi,
            4 => Self::Rdi,
            5 => Self::Rsp,
            6 => Self::Rbp,
            _ => unimplemented!("no register allocator")
        }
    }

    fn idx(&self) -> usize {
        match self {
            Self::Rax => 0,
            Self::Rcx => 1,
            Self::Rdx => 2,
            Self::Rsi => 3,
            Self::Rdi => 4,
            Self::Rsp => 5,
            Self::Rbp => 6,
        }
    }

    fn name(&self) -> &'static str {
        match self {
            Reg::Rax => "rax",
            Reg::Rcx => "rcx",
            Reg::Rdx => "rdx",
            Reg::Rsi => "rsi",
            Reg::Rdi => "rdi",
            Reg::Rsp => "rsp",
            Reg::Rbp => "rbp",
        }
    }
}

fn make_asm(globals: &[GlobalData], functions: &[Routine]) -> String {
    let mut s = String::new();
    writeln!(s, ".intel_syntax noprefix");
    writeln!(s, ".intel_mnemonic");
    writeln!(s);

    writeln!(s, ".section .text");
    writeln!(s);
    for routine in functions {
        writeln!(s, "    .globl {}", routine.name).unwrap();
        write!(s, "{}", routine.to_asm(&globals)).unwrap();
    }

    writeln!(s);
    writeln!(s, ".section .rodata");
    for global in globals {
        write!(s, "{}", global.to_asm()).unwrap();
    }
    writeln!(s, ".section \".note.GNU-stack\"");
    s
}

fn main() -> Result<(), Box<dyn std::error::Error>>{
    let globals = [
        GlobalData {
            name: ".hello".into(),
            data: b"Hello, World!\n".as_slice().into(),
        }
    ];
    // syscall uses rdi rsi rdx r10 r8 r9
    let functions = [
        Routine {
            name: "_start".into(),
            ops: vec![
                Op::Load { loc: 0, val: Val::Literal(1) },
                Op::Load { loc: 1, val: Val::Literal(1) },
                Op::Load { loc: 2, val: Val::GlobalLabel(0) },
                Op::Load { loc: 3, val: Val::Literal(14) },
                Op::Call { id: 0, args: vec![0, 1, 2, 3] },
                Op::Load { loc: 4, val: Val::Literal(60) },
                Op::Load { loc: 5, val: Val::Literal(0) },
                Op::Call { id: 0, args: vec![4, 5] }
            ],
        }
    ];


    let mut f = std::fs::File::create("out.s")?;
    f.set_len(0);
    let asm = make_asm(&globals, &functions);
    write!(f, "{asm}");

    let success = std::process::Command::new("as")
        .args(["--fatal-warnings", "-o", "out.o", "out.s"]).spawn()?.wait()?.success();

    if !success {
        eprintln!("assembling failed");
        return Ok(());
    }

    let success = std::process::Command::new("gcc")
        .args(["-nostdlib", "-o", "out", "out.o", "-lgcc"]).spawn()?.wait()?.success();
        // .args(["-nostdlib", "-ffreestanding", "-o", "out", "out.o", "-lgcc"]).spawn()?.wait()?.success();

    if !success {
        eprintln!("linking failed");
        return Ok(());
    }

    Ok(())
}
