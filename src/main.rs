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
    fn to_asm(&self, globals: &[GlobalData], config: &Config) -> String {
        let mut out = String::new();
        let mut alloc = RegAlloc::new(Reg::count());
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
        let mut vasm = VecDeque::new();
        for (i, op) in self.ops.iter().enumerate() {
            let asm = op.to_asm(&mut alloc);
            vasm.extend(asm);
            for &var in op.vars_referenced() {
                if last_occurrence[var] <= i {
                    alloc.free(var);
                }
            }
        }
        if alloc.stack_needed() > 0 {
            vasm.push_front(AsmOp::Mov(OpTarget::Reg(Reg::Rbp.idx()), OpTarget::Reg(Reg::Rsp.idx())));
            vasm.push_front(AsmOp::Push(Reg::Rbp));
            vasm.push_back(AsmOp::Pop(Reg::Rbp));
        }
        for asm in vasm {
            asm.write_op(&mut out, globals, config);
        }
        
        writeln!(out).unwrap();
        out
    }
}

#[derive(Debug)]
enum OpTarget {
    Literal(u64),
    Reg(u8),
    Label(usize),
    Stack(usize),
}

impl OpTarget {
    fn to_display(&self, w: &mut impl std::fmt::Write, globals: &[GlobalData], ptr_load: bool) -> std::fmt::Result {
        match self {
            OpTarget::Literal(x) => write!(w, "{x}"),
            OpTarget::Reg(r) => write!(w, "{}", Reg::from_idx(*r).name()),
            OpTarget::Label(i) if ptr_load => write!(w, "qword ptr [rip + {}]", globals[*i].name),
            OpTarget::Label(i) => write!(w, "[rip + {}]", globals[*i].name),
            OpTarget::Stack(s) if ptr_load => write!(w, "qword ptr [rsp + {s}]"),
            OpTarget::Stack(s) => write!(w, "[rsp + {s}]"),
        }
    }
}

#[derive(Debug)]
enum AsmOp {
    Push(Reg),
    Pop(Reg),
    Add(OpTarget, OpTarget, OpTarget),
    Comment(String),
    Mov(OpTarget, OpTarget),
    Lea(Reg, OpTarget),
    Syscall
}

impl AsmOp {
    fn write_op(&self, w: &mut impl std::fmt::Write, globals: &[GlobalData], config: &Config) -> std::fmt::Result {
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
            AsmOp::Comment(s) if config.emit_comments => writeln!(w, "    /* {s} */"),
            AsmOp::Comment(s) => Ok(()),
            AsmOp::Push(r) => writeln!(w, "    pushq {reg}", reg = r.name()),
            AsmOp::Pop(r) => writeln!(w, "    popq {reg}", reg = r.name()),
            AsmOp::Add(dst, op1, op2) => {
                write!(w, "    add ")?;
                dst.to_display(w, globals, false)?;
                write!(w, " , ")?;
                op1.to_display(w, globals, false)?;
                write!(w, " , ")?;
                op2.to_display(w, globals, false)?;
                writeln!(w)
            },
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
                    ret.push(AsmOp::Comment(format!("^^^ has arguments {args:?}")));
                    // eprintln!("syscall complete");
                    ret
                } else {
                    todo!()
                }
            },
            Op::Load { loc, val } => {
                let ret = match val {
                    Val::GlobalLabel(g) => {
                        let reg = alloc.move_to_reg(*loc);
                        let mut ret = alloc.take_queue();
                        ret.push(AsmOp::Lea(Reg::from_idx(reg), OpTarget::Label(*g)));
                        ret.push(AsmOp::Comment(format!(" ^^^ loading to L_{loc}")));
                        // eprintln!("load of {val:?} to {loc} ({reg:?}) complete");
                        ret
                    },
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


mod reg_alloc;
use reg_alloc::*;

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
    fn arg_regs_idx(&self) -> Option<&[u8]> {
        const SYSCALL_REGS: [u8; 4] = [Reg::Rax.idx(), Reg::Rdi.idx(), Reg::Rsi.idx(), Reg::Rdx.idx()];
        match self {
            CallType::Syscall => Some(&SYSCALL_REGS),
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
            _ => panic!("no register allocator")
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
    () => {
        
    };
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

fn main() -> Result<(), Box<dyn std::error::Error>>{
    let globals = [
        GlobalData {
            name: ".hello".into(),
            data: b"Hello, World!\n".as_slice().into(),
        }
    ];
    let config = Config {
        emit_comments: true,
    };
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


    let asm = make_asm(&globals, &functions, &config);

    let mut f = std::fs::File::create("out.s")?;
    f.set_len(0);
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
