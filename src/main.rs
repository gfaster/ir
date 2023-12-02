#![allow(unused)]

// MVP FIRST!!!

use std::collections::{BTreeMap, VecDeque};
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
    data: Box<[u8]>,
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

fn unique_label() -> String {
    static LABEL_COUNTER: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
    let label = LABEL_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    format!(".LU_{label}")
}

fn block_label(id: usize) -> String {
    format!(".B_{id}")
}

struct Routine {
    name: Box<str>,
    ops: Vec<Op>,
}

impl Routine {
    fn to_asm(&self, globals: &[GlobalData], config: &Config) -> String {
        let mut out = String::new();
        let mut alloc = RegAlloc::new(Reg::count());
        let mut last_occurrence = Vec::new();
        let mut other_labels = BTreeMap::new();
        for (i, op) in self.ops.iter().enumerate() {
            for var in op.vars_referenced() {
                if var >= last_occurrence.len() {
                    last_occurrence.resize(var + 1, usize::MAX);
                }
                last_occurrence[var] = i;
            }
            if let Op::Block { id, .. } = op {
                if globals.len() > *id {
                    panic!("block label id conflicts with global");
                }
                other_labels
                    .entry(*id).and_modify(|_| panic!("block label redeclared"))
                    .or_insert_with(|| block_label(*id));
            }
        }
        writeln!(out, "{name}:", name = self.name).unwrap();
        let mut vasm = VecDeque::new();
        for (i, op) in self.ops.iter().enumerate() {
            let asm = op.to_asm(&mut alloc);
            vasm.extend(asm);
            for var in op.vars_referenced() {
                if last_occurrence[var] <= i {
                    alloc.free(var);
                }
            }
        }
        if alloc.stack_needed() > 0 {
            vasm.push_front(AsmOp::Mov(
                OpTarget::Reg(Reg::Rbp.idx()),
                OpTarget::Reg(Reg::Rsp.idx()),
            ));
            vasm.push_front(AsmOp::Push(Reg::Rbp));
            vasm.push_back(AsmOp::Pop(Reg::Rbp));
        }
        let get_label = |i: usize| -> Option<&str> {
            if i < globals.len() {
                Some(&globals[i].name)
            } else {
                other_labels.get(&i).map(|x: &String| x.as_str())
            }
        };
        for asm in vasm {
            asm.write_op(&mut out, &get_label, config);
        }

        writeln!(out).unwrap();
        out
    }
}

#[derive(Debug, Clone)]
enum OpTarget {
    Literal(u64),
    Reg(u8),
    Label(usize),
    LitLabel(String),
    Stack(usize),
}

impl OpTarget {
    fn from_reg(reg: u8) -> Self {
        Self::Reg(reg)
    }
}

impl From<VarLoc> for OpTarget {
    fn from(value: VarLoc) -> Self {
        match value {
            VarLoc::Stack(off) => OpTarget::Stack(off),
            VarLoc::Reg { reg, .. } => OpTarget::Reg(reg),
            VarLoc::Uninit => unimplemented!(),
        }
    }
}

#[derive(Clone, Copy)]
enum PtrMode {
    Address,
    JmpTarget,
    MemTarget,
    None,
}

struct LabelDisplay<'a>(&'a str, PtrMode);

impl std::fmt::Display for LabelDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let l = self.0;
        match self.1 {
            PtrMode::Address => write!(f, "qword [rip + {l}]"),
            PtrMode::MemTarget => write!(f, "qword ptr [rip + {l}]"),
            PtrMode::JmpTarget => write!(f, "{l}"),
            PtrMode::None => panic!("cannot use a label in this position"),
        }
    }
}

struct OpTargetDisplay<'a, 'b, T>(&'a OpTarget, &'b T , PtrMode)
    where 
        T: Fn(usize) -> Option<&'b str> ;

impl<'a, 'b, T> std::fmt::Display for OpTargetDisplay<'a, 'b, T>
    where 
    T: Fn(usize) -> Option<&'b str>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let lab = &self.1;
        match self.0 {
            OpTarget::Literal(x) => write!(f, "{x}"),
            OpTarget::Reg(r) => write!(f, "{}", Reg::from_idx(*r).name()),
            OpTarget::Label(i) => write!(f, "{}", LabelDisplay(lab(*i).expect("has label declared"), self.2)),
            OpTarget::LitLabel(i) => write!(f, "{}", LabelDisplay(&i, self.2)),
            OpTarget::Stack(s) => write!(f, "qword ptr [rsp + {s}]"),
            OpTarget::Stack(s) => write!(f, "[rsp + {s}]"),
        }
    }
}

#[derive(Debug, Clone)]
enum AsmOp {
    Push(Reg),
    Pop(Reg),
    Add(OpTarget, OpTarget),
    Comment(String),
    Label(String),
    Jne(OpTarget),
    Jeq(OpTarget),
    Jmp(OpTarget),
    Test(OpTarget, OpTarget),
    Mov(OpTarget, OpTarget),
    Lea(Reg, OpTarget),
    Syscall,
}

impl AsmOp {
    fn write_op<'a, 'b: 'a>(
        &'a self,
        w: &'_ mut impl std::fmt::Write,
        label: &'b impl Fn(usize) -> Option<&'b str>,
        config: &Config,
    ) -> std::fmt::Result {
        match self {
            AsmOp::Mov(dst, src) => {
                writeln!(w, "    mov {dst}, {src}", 
                    dst = OpTargetDisplay(dst, label, PtrMode::MemTarget), 
                    src = OpTargetDisplay(src, label, PtrMode::MemTarget))
            }
            AsmOp::Lea(dst, addr) => {
                writeln!(w, "    mov {dst}, {addr}", 
                    dst = dst.name(), 
                    addr = OpTargetDisplay(addr, label, PtrMode::Address))
            }
            AsmOp::Syscall => writeln!(w, "    syscall"),
            AsmOp::Comment(s) if config.emit_comments => writeln!(w, "    /* {s} */"),
            AsmOp::Comment(s) => Ok(()),
            AsmOp::Push(r) => writeln!(w, "    pushq {reg}", reg = r.name()),
            AsmOp::Pop(r) => writeln!(w, "    popq {reg}", reg = r.name()),
            AsmOp::Add(dst, op1) => {
                writeln!(w, "    add {dst}, {op1}", 
                    dst = OpTargetDisplay(dst, label, PtrMode::MemTarget), 
                    op1 = OpTargetDisplay(op1, label, PtrMode::MemTarget),
                )
            }
            AsmOp::Label(l) => {
                writeln!(w, "{l}:")
            },
            AsmOp::Jne(l) => {
                writeln!(w, "    jne {dst}",
                    dst = OpTargetDisplay(l, label, PtrMode::JmpTarget), 
                )
            },
            AsmOp::Jeq(l) => {
                writeln!(w, "    je {dst}",
                    dst = OpTargetDisplay(l, label, PtrMode::JmpTarget), 
                )
            },
            AsmOp::Jmp(l) => {
                writeln!(w, "    jmp {dst}",
                    dst = OpTargetDisplay(l, label, PtrMode::JmpTarget), 
                )
            },
            AsmOp::Test(l, r) => {
                writeln!(w, "    test {l}, {r}", 
                    l = OpTargetDisplay(l, label, PtrMode::Address), 
                    r = OpTargetDisplay(r, label, PtrMode::Address))
            },
        }
    }
}

enum Op {
    Call { id: Id, args: Vec<Loc> },
    Load { loc: Loc, val: Val },
    Block { id: Id, args: Vec<Loc> },
    Bne { target: Id, check: (Loc, Loc), args: Vec<Loc> },
    Add { dst: Loc, op1: Loc, op2: Loc },
}

impl Op {
    fn to_asm(&self, alloc: &mut RegAlloc) -> Vec<AsmOp> {
        match self {
            Op::Call { id, args } => {
                if *id == 0 {
                    let mut ret = alloc.setup_call(&args, CallType::Syscall);
                    ret.push(AsmOp::Syscall);
                    ret.push(AsmOp::Comment(format!("^^^ has arguments {args:?}")));
                    // eprintln!("syscall complete");
                    ret
                } else {
                    todo!()
                }
            }
            Op::Load { loc, val } => {
                let ret = match val {
                    Val::GlobalLabel(g) => {
                        let reg = alloc.move_to_reg(*loc);
                        let mut ret = alloc.take_queue();
                        ret.push(AsmOp::Lea(Reg::from_idx(reg), OpTarget::Label(*g)));
                        ret.push(AsmOp::Comment(format!(" ^^^ loading to L_{loc}")));
                        // eprintln!("load of {val:?} to {loc} ({reg:?}) complete");
                        ret
                    }
                    Val::LocalLabel(_) => todo!(),
                    Val::Literal(v) => {
                        let reg = alloc.move_to_reg(*loc);
                        let mut ret = alloc.take_queue();
                        ret.push(AsmOp::Mov(OpTarget::Reg(reg), OpTarget::Literal(*v)));
                        ret.push(AsmOp::Comment(format!(" ^^^ loading literal to L_{loc}")));
                        // eprintln!("load of {val:?} to {loc} ({reg:?}) complete");
                        ret
                    }
                };
                ret
            }
            Op::Block { id, args } => {
                let mut ret = alloc.setup_call(&args, CallType::Block);
                ret.push(AsmOp::Label(block_label(*id)));
                ret
            }
            Op::Bne { target, check, args } => {
                let regl = alloc.move_to_reg(check.0);
                let regr = alloc.move_to_reg(check.1);
                let mut ret = alloc.take_queue();
                let post_label = unique_label();
                ret.push(AsmOp::Test(OpTarget::Reg(regl), OpTarget::Reg(regr)));
                let backup = alloc.clone();
                ret.push(AsmOp::Jeq(OpTarget::LitLabel(post_label.clone())));
                ret.extend(alloc.setup_call(&args, CallType::Block));
                ret.push(AsmOp::Jmp(OpTarget::Label(*target)));
                ret.push(AsmOp::Label(post_label));
                std::mem::replace(alloc, backup);
                ret
            },
            Op::Add { dst, op1, op2 } => {
                let regl = alloc.move_to_reg(*op1);
                let regr = alloc.move_to_reg(*op2);
                let mut ret = alloc.take_queue();
                ret.push(AsmOp::Add(OpTarget::Reg(regl), OpTarget::Reg(regl)));
                alloc.force_reg(*dst, regl);
                ret
            },
        }
    }

    fn vars_referenced(&self) -> Vec<Loc> {
        match self {
            Op::Call { id, args } => args.clone(),
            Op::Load { loc, val } => std::slice::from_ref(loc).into(),
            Op::Block { id, args } => args.clone(),
            Op::Bne { target, check, args } => Vec::from_iter([check.0, check.1].into_iter()
                .chain(args.iter().copied())),
            Op::Add { dst, op1, op2 } => vec![*dst, *op1, *op2],
        }
    }
}

mod reg_alloc;
use reg_alloc::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CallType {
    Syscall,
    Block,
    Op2,
    Op3,
}

impl CallType {
    fn arg_regs(&self) -> Option<&[Reg]> {
        match self {
            CallType::Syscall => Some(&[
                Reg::Rax,
                Reg::Rdi,
                Reg::Rsi,
                Reg::Rdx,
                Reg::R10,
                Reg::R8,
                Reg::R9,
            ]),
            CallType::Op2 => None,
            CallType::Op3 => None,
            CallType::Block => Some(&[
                Reg::Rax,
                Reg::Rcx,
                Reg::Rdx,
                Reg::Rsi,
                Reg::Rdi,
                Reg::Rsp,
                Reg::Rbp,
                Reg::R8,
                Reg::R9,
                Reg::R10,
                Reg::R11,
                Reg::R12,
                Reg::R13,
                Reg::R14,
                Reg::R15,
            ]),
        }
    }
    fn arg_regs_idx(&self) -> Option<&[u8]> {
        const SYSCALL_REGS: [u8; 7] = [
            Reg::Rax.idx(),
            Reg::Rdi.idx(),
            Reg::Rsi.idx(),
            Reg::Rdx.idx(),
            Reg::R10.idx(),
            Reg::R8.idx(),
            Reg::R9.idx(),
        ];
        const BLOCK_REGS: [u8; 15] = [
                Reg::Rax.idx(),
                Reg::Rcx.idx(),
                Reg::Rdx.idx(),
                Reg::Rsi.idx(),
                Reg::Rdi.idx(),
                Reg::Rsp.idx(),
                Reg::Rbp.idx(),
                Reg::R8.idx(),
                Reg::R9.idx(),
                Reg::R10.idx(),
                Reg::R11.idx(),
                Reg::R12.idx(),
                Reg::R13.idx(),
                Reg::R14.idx(),
                Reg::R15.idx(),
        ];
        match self {
            CallType::Syscall => Some(&SYSCALL_REGS),
            CallType::Op2 => None,
            CallType::Op3 => None,
            CallType::Block => Some(&BLOCK_REGS),
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
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

impl Reg {
    const fn count() -> usize {
        13
    }

    const fn from_idx(loc: u8) -> Self {
        match loc {
            0 => Self::Rax,
            1 => Self::Rcx,
            2 => Self::Rdx,
            3 => Self::Rsi,
            4 => Self::Rdi,
            5 => Self::R8,
            6 => Self::R9,
            7 => Self::R10,
            8 => Self::R11,
            9 => Self::R12,
            10 => Self::R13,
            11 => Self::R14,
            12 => Self::R15,
            13 => Self::Rsp,
            14 => Self::Rbp,
            _ => panic!("no register allocator"),
        }
    }

    const fn idx(self) -> u8 {
        match self {
            Self::Rax => 0,
            Self::Rcx => 1,
            Self::Rdx => 2,
            Self::Rsi => 3,
            Self::Rdi => 4,
            Self::R8 => 5,
            Self::R9 => 6,
            Self::R10 => 7,
            Self::R11 => 8,
            Self::R12 => 9,
            Self::R13 => 10,
            Self::R14 => 11,
            Self::R15 => 12,
            Self::Rsp => 13,
            Self::Rbp => 14,
        }
    }

    fn name(&self) -> &'static str {
        match self {
            Self::Rax => "rax",
            Self::Rcx => "rcx",
            Self::Rdx => "rdx",
            Self::Rsi => "rsi",
            Self::Rdi => "rdi",
            Self::Rsp => "rsp",
            Self::Rbp => "rbp",
            Self::R8 => "r8",
            Self::R9 => "r9",
            Self::R10 => "r10",
            Self::R11 => "r11",
            Self::R12 => "r12",
            Self::R13 => "r13",
            Self::R14 => "r14",
            Self::R15 => "r15",
        }
    }
}

fn make_asm(globals: &[GlobalData], functions: &[Routine], config: &Config) -> String {
    let mut s = String::new();
    writeln!(s, ".intel_syntax noprefix");
    writeln!(s, ".intel_mnemonic");
    writeln!(s);

    writeln!(s, ".section .text");
    writeln!(s);
    for routine in functions {
        writeln!(s, "    .globl {}", routine.name).unwrap();
        write!(s, "{}", routine.to_asm(&globals, config)).unwrap();
    }

    writeln!(s);
    writeln!(s, ".section .rodata");
    for global in globals {
        write!(s, "{}", global.to_asm()).unwrap();
    }
    writeln!(s, ".section \".note.GNU-stack\"");
    s
}

macro_rules! lang {
    () => {};
}

struct Config {
    emit_comments: bool,
}

impl Config {
    fn new() -> Self {
        Self {
            emit_comments: false,
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let globals = [GlobalData {
        name: ".hello".into(),
        data: b"Hello, World!\n".as_slice().into(),
    },
    ];
    let config = Config::new();
    // syscall uses rdi rsi rdx r10 r8 r9
    let functions = [Routine {
        name: "_start".into(),
        ops: vec![
            Op::Load { loc: 4, val: Val::Literal(0), },
            Op::Block { id: 1, args: vec![4] },
            Op::Load { loc: 6, val: Val::Literal(5), },
            Op::Load { loc: 0, val: Val::Literal(1), },
            Op::Load { loc: 1, val: Val::Literal(1), },
            Op::Load { loc: 2, val: Val::GlobalLabel(0), },
            Op::Load { loc: 3, val: Val::Literal(14), },
            Op::Call { id: 0, args: vec![0, 1, 2, 3], },
            Op::Add { dst: 7, op1: 4, op2: 1 },
            Op::Bne { target: 1, check: (7, 6), args: vec![7] },
            Op::Load { loc: 4, val: Val::Literal(60), },
            Op::Load { loc: 5, val: Val::Literal(0), },
            Op::Call { id: 0, args: vec![4, 5], },
        ],
    }];

    let asm = make_asm(&globals, &functions, &config);

    let mut f = std::fs::File::create("out.s")?;
    f.set_len(0);
    write!(f, "{asm}");

    let success = std::process::Command::new("as")
        .args(["--fatal-warnings", "-o", "out.o", "out.s"])
        .spawn()?
        .wait()?
        .success();

    if !success {
        eprintln!("assembling failed");
        return Ok(());
    }

    let success = std::process::Command::new("gcc")
        .args(["-nostdlib", "-o", "out", "out.o", "-lgcc"])
        .spawn()?
        .wait()?
        .success();
    // .args(["-nostdlib", "-ffreestanding", "-o", "out", "out.o", "-lgcc"]).spawn()?.wait()?.success();

    if !success {
        eprintln!("linking failed");
        return Ok(());
    }

    Ok(())
}
