use std::fmt::Write;
use std::{collections::BTreeMap, rc::Rc};

use crate::{reg_alloc::VarLoc, Config, GlobalData, Reg};
use crate::{BlockId, Ctx, InstrArg};

#[derive(Debug, Clone)]
pub enum OpTarget {
    Literal(u64),
    Reg(u8),
    Ptr(u8),
    Label(InstrArg),
    LitLabel(String),
    Stack(usize),
}

impl OpTarget {
    pub fn from_reg(reg: u8) -> Self {
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

#[derive(Debug, Clone, Copy)]
enum PtrStride {
    X1,
    X2,
    X4,
    X8,
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

struct OpTargetDisplay<'a, 'b, T>(&'a OpTarget, &'b T, PtrMode)
where
    T: Fn(InstrArg) -> Option<&'b str>;

impl<'a, 'b, T> std::fmt::Display for OpTargetDisplay<'a, 'b, T>
where
    T: Fn(InstrArg) -> Option<&'b str>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let lab = &self.1;
        let get_label = |i: InstrArg| {
            let Some(label) = lab(i) else {
                panic!("label {i} is not declared")
            };
            label
        };
        match self.0 {
            OpTarget::Literal(x) => write!(f, "{x}"),
            OpTarget::Reg(r) => write!(f, "{}", Reg::from_idx(*r).name()),
            OpTarget::Label(i) => write!(f, "{}", LabelDisplay(get_label(*i), self.2)),
            OpTarget::LitLabel(i) => write!(f, "{}", LabelDisplay(&i, self.2)),
            OpTarget::Stack(s) => write!(f, "qword ptr [rsp + {s}]"),
            OpTarget::Stack(s) => write!(f, "[rsp + {s}]"),
            OpTarget::Ptr(p) => write!(f, "qword ptr [{}]", Reg::from_idx(*p).name()),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum AsmOp {
    File(String, u32),
    Loc(u32),
    FuncBegin(u32),
    FuncEnd(u32),
    BlockBegin(BlockId),
    BlockEnd(BlockId),
    Push(Reg),
    Pop(Reg),
    Add(OpTarget, OpTarget),
    Comment(String),
    // VarComment(Box<dyn FnOnce(&VarMap) -> String + Clone>),
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
    pub(crate) fn write_op<'a, 'b: 'a>(
        &'a self,
        w: &'_ mut impl std::fmt::Write,
        label: &'b impl Fn(InstrArg) -> Option<&'b str>,
        ctx: &Ctx,
    ) -> std::fmt::Result {
        let config = &ctx.config;
        match self {
            AsmOp::Mov(dst, src) => {
                writeln!(
                    w,
                    "    mov {dst}, {src}",
                    dst = OpTargetDisplay(dst, label, PtrMode::MemTarget),
                    src = OpTargetDisplay(src, label, PtrMode::MemTarget)
                )
            }
            AsmOp::Lea(dst, addr) => {
                writeln!(
                    w,
                    "    lea {dst}, {addr}",
                    addr = OpTargetDisplay(addr, label, PtrMode::Address)
                )
            }
            AsmOp::Syscall => writeln!(w, "    syscall"),
            AsmOp::Comment(s) if config.emit_comments => writeln!(w, "    /* {s} */"),
            AsmOp::Comment(s) => Ok(()),
            AsmOp::Push(reg) => writeln!(w, "    pushq {reg}"),
            AsmOp::Pop(reg) => writeln!(w, "    popq {reg}"),
            AsmOp::Add(dst, op1) => {
                writeln!(
                    w,
                    "    add {dst}, {op1}",
                    dst = OpTargetDisplay(dst, label, PtrMode::MemTarget),
                    op1 = OpTargetDisplay(op1, label, PtrMode::MemTarget),
                )
            }
            AsmOp::Label(l) => {
                writeln!(w, "{l}:")
            }
            AsmOp::Jne(l) => {
                writeln!(
                    w,
                    "    jne {dst}",
                    dst = OpTargetDisplay(l, label, PtrMode::JmpTarget),
                )
            }
            AsmOp::Jeq(l) => {
                writeln!(
                    w,
                    "    je {dst}",
                    dst = OpTargetDisplay(l, label, PtrMode::JmpTarget),
                )
            }
            AsmOp::Jmp(l) => {
                writeln!(
                    w,
                    "    jmp {dst}",
                    dst = OpTargetDisplay(l, label, PtrMode::JmpTarget),
                )
            }
            AsmOp::Test(l, r) => {
                writeln!(
                    w,
                    "    test {l}, {r}",
                    l = OpTargetDisplay(l, label, PtrMode::Address),
                    r = OpTargetDisplay(r, label, PtrMode::Address)
                )
            }
            AsmOp::Cmp(l, r) => {
                writeln!(
                    w,
                    "    cmp {l}, {r}",
                    l = OpTargetDisplay(l, label, PtrMode::Address),
                    r = OpTargetDisplay(r, label, PtrMode::Address)
                )
            }
            AsmOp::File(name, id) => {
                writeln!(w, ".file {id} {name:?}")
            }
            AsmOp::Loc(line) => {
                if config.emit_debug_syms {
                    writeln!(w, ".loc 0 {line}")?
                }
                Ok(())
            }
            AsmOp::FuncBegin(id) => writeln!(w, ".LFB{id}:"),
            AsmOp::FuncEnd(id) => writeln!(w, ".LFE{id}:"),
            AsmOp::BlockBegin(id) => {
                if config.emit_debug_syms {
                    writeln!(w, ".LBB{id}:")?;
                }
                Ok(())
            }
            AsmOp::BlockEnd(id) => {
                if config.emit_debug_syms {
                    writeln!(w, ".LBE{id}:")?;
                }
                Ok(())
            }
        }
    }
}

// pub(crate) fn make_asm(ctx: &Ctx, functions: &mut [Routine]) -> String {
//     let Ctx {
//         config,
//         vars,
//         globals,
//     } = ctx;
//     let mut s = String::new();
//     writeln!(s, ".intel_syntax noprefix");
//     writeln!(s, ".intel_mnemonic");
//     writeln!(s);
//
//     writeln!(s, ".section .text");
//     writeln!(
//         s,
//         ".file 0 \"{}\"",
//         config.input_files.first().expect("has input file")
//     );
//     writeln!(s);
//     for routine in functions {
//         writeln!(s, "    .globl {}", routine.name).unwrap();
//         write!(s, "{}", routine.to_asm(ctx)).unwrap();
//     }
//
//     writeln!(s);
//     writeln!(s, ".section .rodata");
//     for (_id, global) in globals {
//         write!(s, "{}", global.to_asm()).unwrap();
//     }
//     writeln!(s, ".section \".note.GNU-stack\"");
//     s
// }
