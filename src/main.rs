#![allow(unused)]

// MVP FIRST!!!

use std::collections::{BTreeMap, VecDeque};
use std::fmt::Write as FmtWrite;
use std::io::Write;

// mod reorder;
// use reorder::*;

mod parse;
use parse::*;

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
    fn to_asm(&mut self, globals: &BTreeMap<usize, GlobalData>, config: &Config) -> String {
        let mut out = String::new();
        let mut alloc = RegAlloc::new(Reg::count());
        let mut other_labels = BTreeMap::new();
        let mut block_start = 0;
        let mut blocks = Vec::new();

        for (i, op) in self.ops.iter().enumerate() {
            if let Op::Block { id, .. } = op {
                if globals.len() > *id {
                    panic!("block label id conflicts with global");
                }
                other_labels
                    .entry(*id).and_modify(|_| panic!("block label redeclared"))
                    .or_insert_with(|| block_label(*id));
                blocks.push(block_start..i);
                block_start = i;
            }
        }
        blocks.push(block_start..self.ops.len());
        writeln!(out, "{name}:", name = self.name).unwrap();

        let get_label = |i: usize| -> Option<&str> {
            globals.get(&i).map_or_else(|| other_labels.get(&i).map(
                |x: &String| x.as_str()),
                |g| Some(&globals[&i].name))
            // if i < globals.len() {
            //     Some(&globals[&i].name)
            // } else {
            //     other_labels.get(&i).map(|x: &String| x.as_str())
            // }
        };

        for (i, block) in blocks.into_iter().enumerate() {
            let args = if i == 0 {
                vec![]
            } else {
                let Op::Block { args, .. } = &self.ops[0] else {
                    panic!("all blocks except the first must start with block op, \
                        but this starts with {:?}", &self.ops[0]);
                };
                args.clone()
            };
            let rem = self.ops.split_off(block.len());
            let block = std::mem::replace(&mut self.ops, rem);
            // eprintln!("{:?}\n", &block);
            let vasm = RegAlloc::organize_block(block, &args);
            for asm in vasm {
                asm.write_op(&mut out, &get_label, config);
            }
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
            VarLoc::Stack { slot } => OpTarget::Stack(slot),
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
            PtrMode::Address => write!(f, "[rip + {l}]"),
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
    Cmp(OpTarget, OpTarget),
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
                writeln!(w, "    lea {dst}, {addr}", 
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
            AsmOp::Cmp(l, r) => {
                writeln!(w, "    cmp {l}, {r}", 
                    l = OpTargetDisplay(l, label, PtrMode::Address), 
                    r = OpTargetDisplay(r, label, PtrMode::Address))
            },
        }
    }
}

#[derive(Debug)]
struct Target {
    id: Id,
    args: Vec<Loc>
}

#[derive(Debug)]
enum Op {
    Call { id: Id, args: Vec<Loc> },
    RegAllocOp(RegAllocOp),
    Load { loc: Loc, val: Val },
    Block { id: Id, args: Vec<Loc> },
    Bne { check: (Loc, Loc), success: Target, fail: Target },
    Jmp { target: Target },
    Add { dst: Loc, op1: Loc, op2: Loc },
}

impl From<RegAllocOp> for Op {
    fn from(v: RegAllocOp) -> Self {
        Self::RegAllocOp(v)
    }
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
            Op::Bne { check, success, fail } => {
                let regl = alloc.move_to_reg(check.0);
                let regr = alloc.move_to_reg(check.1);
                let mut ret = alloc.take_queue();
                let post_label = unique_label();
                ret.push(AsmOp::Cmp(OpTarget::Reg(regl), OpTarget::Reg(regr)));
                let backup = alloc.clone();
                ret.push(AsmOp::Jeq(OpTarget::LitLabel(post_label.clone())));
                ret.extend(alloc.setup_call(&success.args, CallType::Block));
                ret.push(AsmOp::Jmp(OpTarget::Label(success.id)));
                ret.push(AsmOp::Label(post_label));
                ret.extend(alloc.setup_call(&fail.args, CallType::Block));
                ret.push(AsmOp::Jmp(OpTarget::Label(fail.id)));

                std::mem::replace(alloc, backup);
                ret
            },
            Op::Jmp { target } => {
                let mut ret = alloc.setup_call(&target.args, CallType::Block);
                ret.push(AsmOp::Jmp(OpTarget::Label(target.id)));
                ret
            },
            Op::Add { dst, op1, op2 } => {
                let regl = alloc.move_to_reg(*op1);
                let regr = alloc.move_to_reg(*op2);
                let mut ret = alloc.take_queue();
                ret.push(AsmOp::Add(OpTarget::Reg(regl), OpTarget::Reg(regr)));
                alloc.force_reg(*dst, regl);
                ret
            },
            Op::RegAllocOp(op) => {
                alloc.apply_op(op);
                vec![]
            },
        }
    }

    fn vars_referenced(&self) -> Vec<Loc> {
        match self {
            Op::Call { id, args } => args.clone(),
            Op::Load { loc, val } => std::slice::from_ref(loc).into(),
            Op::Block { id, args } => args.clone(),
            Op::Bne { check, success, fail } => Vec::from_iter([check.0, check.1].into_iter()
                .chain(success.args.iter().copied().chain(fail.args.iter().copied()))),
            Op::Add { dst, op1, op2 } => vec![*dst, *op1, *op2],
            Op::Jmp { target } => Vec::from(&target.args[..]),
            Op::RegAllocOp(_) => panic!("vars_referenced should not be called after reg ops added"),
        }
    }

    fn regs_clobbered(&self) -> &[u8] {
        if let Op::Call { id, args } = self {
            if *id == 0 {
                return CallType::Syscall.clobers_idx();
            }
        }
        &[]
    }

    /// Returns `true` if the op is a branch instruction.
    #[must_use]
    fn is_branch(&self) -> bool {
        matches!(self, Self::Bne { .. })
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

    fn clobers_idx(&self) -> &[u8] {
        const SYSCALL_REGS: [u8; 3] = [
            Reg::Rax.idx(),
            Reg::Rcx.idx(),
            Reg::R11.idx(),
        ];
        match self {
            CallType::Syscall => &SYSCALL_REGS,
            CallType::Op2 => &[],
            CallType::Op3 => &[],
            CallType::Block => &[],
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

fn make_asm(globals: &BTreeMap<usize, GlobalData>, functions: &mut [Routine], config: &Config) -> String {
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
    for (_id, global) in globals {
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
            emit_comments: true,
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // let globals = [GlobalData {
    //     name: ".hello".into(),
    //     data: b"Hello, World!\n".as_slice().into(),
    // },
    // ];
    // syscall uses rdi rsi rdx r10 r8 r9
    // let mut functions = [Routine {
    //     name: "_start".into(),
    //     ops: vec![
    //         Op::Load { loc: 4, val: Val::Literal(0), },
    //         Op::Block { id: 1, args: vec![4] },
    //         Op::Load { loc: 6, val: Val::Literal(5), },
    //         Op::Load { loc: 0, val: Val::Literal(1), },
    //         Op::Load { loc: 1, val: Val::Literal(1), },
    //         Op::Load { loc: 2, val: Val::GlobalLabel(0), },
    //         Op::Load { loc: 3, val: Val::Literal(14), },
    //         Op::Call { id: 0, args: vec![0, 1, 2, 3], },
    //         Op::Add { dst: 7, op1: 4, op2: 1 },
    //         Op::Bne { check:(7,6), 
    //             success: Target { id: 1, args: vec![7] },
    //             fail: Target { id: 2, args: vec![] } },
    //         Op::Block { id: 2, args: vec![] },
    //         Op::Load { loc: 4, val: Val::Literal(60), },
    //         Op::Load { loc: 5, val: Val::Literal(0), },
    //         Op::Call { id: 0, args: vec![4, 5], },
    //     ],
    // }];

    let config = Config::new();

    let mut input = parse::Parser::new_file("./input.ll")?;
    let (functions, mut globals) = input.parse();
    let mut functions = [functions];

    let asm = make_asm(&globals, &mut functions, &config);

    let mut f = std::fs::File::create("out.s")?;
    f.set_len(0);
    write!(f, "{asm}");

    let success = std::process::Command::new("as")
        .args(["--fatal-warnings", "-g", "-o", "out.o", "out.s"])
        .spawn()?
        .wait()?
        .success();

    if !success {
        eprintln!("assembling failed");
        return Ok(());
    }

    let success = std::process::Command::new("gcc")
        .args(["-nostdlib", "-g", "-o", "out", "out.o", "-lgcc"])
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
