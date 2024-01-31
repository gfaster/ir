#![allow(unused)]

// MVP FIRST!!!

use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::fmt::Write as FmtWrite;
use std::io::Write;
use std::ops::Deref;
use std::rc::{Rc, Weak};
use std::sync::Arc;

// mod reorder;
// use reorder::*;

// mod assign_lit;

mod cli;
mod arch;
mod instr;
mod reg;
use instr::{MachineInstruction, BasicInstrProp, Instruction};
use reg::{Binding, BlockId, InstrArg};
mod ir;
mod vec_map;
mod stack;
// mod dag;
mod sig;
mod ty;
use ty::Type;
mod attr;
mod edag;
mod fold;
mod appendvec;
mod list;
mod regstate;
mod value;
mod tagged_ptr;

mod parse;
use asm::{AsmOp, OpTarget};
use parse::*;
mod asm;
mod reg_alloc;
use reg_alloc::*;

use crate::cli::Input;

/// symbol id currently global scope and unique
type Id = usize;

type VarSet = Rc<BTreeMap<Binding, Arc<str>>>;

type IdTy = usize;


#[macro_export]
macro_rules! warn_once {
    ($($tt:tt)*) => {
        {
            static WARN_CELL: ::std::sync::OnceLock<()> = ::std::sync::OnceLock::new();
            WARN_CELL.get_or_init(|| eprintln!($($tt)*));
        }
    };
}

#[derive(Debug, Clone, Copy)]
enum OperationalValidity {
    Valid,
    UndefinedBehavior,
}

impl OperationalValidity {
    #[must_use]
    fn is_valid(&self) -> bool {
        matches!(self, Self::Valid)
    }

    #[must_use]
    fn is_undefined_behavior(&self) -> bool {
        matches!(self, Self::UndefinedBehavior)
    }
}

struct Ctx {
    pub config: Config,
    pub vars: VarSet,
    pub globals: BTreeMap<Binding, GlobalData>,
}

#[derive(Debug, Clone, Copy)]
enum Val {
    GlobalBinding(Binding),
    Binding(InstrArg),
}

impl Val {
    pub fn as_binding(&self) -> Option<Binding> {
        match self {
            Val::GlobalBinding(b) => Some(*b),
            Val::Binding(b) => b.as_binding(),
        }
    }

    pub fn as_arg(&self) -> InstrArg {
        match self {
            Val::GlobalBinding(b) => b.into(),
            Val::Binding(b) => *b,
        }
    }
}

impl std::fmt::Display for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Val::GlobalBinding(g) => write!(f, "global {g}"),
            Val::Binding(b) => write!(f, "{b}"),
        }
    }
}

struct GlobalData {
    name: Box<str>,
    data: Box<[u8]>,
}

impl GlobalData {
    fn to_asm(&self) -> String {
        let mut out = String::new();
        writeln!(out, "{name}:", name = self.name).unwrap();
        if self.data.is_ascii() {
            writeln!(out, ".ascii \"{}\"", self.data.escape_ascii());
        } else {
            for byte in self.data.iter() {
                writeln!(out, ".byte {byte}");
            }
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

fn block_label(id: BlockId) -> String {
    format!(".LB_{id}")
}

fn global_label(id: Binding) -> String {
    format!(".L_DATA_{id}")
}

fn new_function_id() -> u32 {
    static LABEL_COUNTER: std::sync::atomic::AtomicU32 = std::sync::atomic::AtomicU32::new(0);
    LABEL_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
}

// impl Routine {
//     fn to_asm(&mut self, ctx: &Ctx) -> String {
//         let Ctx {
//             config,
//             vars,
//             globals,
//         } = ctx;
//         let mut out = String::new();
//         let mut other_labels = BTreeMap::new();
//         let mut block_start = 0;
//         let mut blocks = Vec::new();
//
//         for (i, op) in self.ops.iter().enumerate() {
//             if let OpInner::Block { id, .. } = &op.inner {
//                 // if globals.len() > id.0 {
//                 //     panic!("block label id conflicts with global");
//                 // }
//                 other_labels
//                     .entry(*id)
//                     .and_modify(|_| panic!("block label redeclared"))
//                     .or_insert_with(|| block_label(*id));
//                 blocks.push(block_start..i);
//                 block_start = i;
//             }
//         }
//         blocks.push(block_start..self.ops.len());
//         writeln!(out, "{name}:", name = self.name).unwrap();
//
//         let get_label = |i: Binding| -> Option<&str> {
//             None
//             // globals.get(&i).map_or_else(
//             //     || other_labels.get(&i).map(|x: &String| x.as_str()),
//             //     |g| Some(&globals[&i].name),
//             // )
//             // if i < globals.len() {
//             //     Some(&globals[&i].name)
//             // } else {
//             //     other_labels.get(&i).map(|x: &String| x.as_str())
//             // }
//         };
//
//         let func_id = new_function_id();
//         AsmOp::FuncBegin(func_id).write_op(&mut out, &get_label, ctx);
//         for (i, block) in blocks.into_iter().enumerate() {
//             let args = if i == 0 {
//                 vec![]
//             } else {
//                 let OpInner::Block { args, .. } = &self.ops[0].inner else {
//                     panic!(
//                         "all blocks except the first must start with block op, \
//                         but this starts with {:?}",
//                         &self.ops[0]
//                     );
//                 };
//                 args.clone()
//             };
//             let rem = self.ops.split_off(block.len());
//             let block = std::mem::replace(&mut self.ops, rem);
//             // eprintln!("{:?}\n", &block);
//             let vasm = RegAlloc::organize_block(block, &args, ctx);
//             for asm in vasm {
//                 asm.write_op(&mut out, &get_label, ctx);
//             }
//         }
//         AsmOp::FuncEnd(func_id).write_op(&mut out, &get_label, ctx);
//
//         writeln!(out).unwrap();
//         out
//     }
// }



/*
impl Op {
    fn to_asm(&self, alloc: &mut RegAlloc, ctx: &Ctx) -> Vec<AsmOp> {
        let vars = &ctx.vars;
        let mut ret = Vec::with_capacity(10);
        ret.push(AsmOp::Loc(self.line));
        match &self.inner {
            OpInner::Call { id, args } => {
                if *id == 0 {
                    ret.extend(alloc.setup_call(&args, CallType::Syscall));
                    ret.push(AsmOp::Syscall);
                    ret.push(AsmOp::Comment(format!(
                        "^^^ has arguments {:?}",
                        &args
                    )));
                } else {
                    todo!()
                }
            }
            OpInner::Assign { loc, val } => {
                match val {
                    Val::GlobalLabel(g) => {
                        let reg = alloc.move_to_reg(*loc);
                        ret.extend_from_slice(&alloc.take_queue());
                        ret.push(AsmOp::Lea(Reg::from_idx(reg), OpTarget::Label(*g)));
                        ret.push(AsmOp::Comment(format!(" ^^^ loading to {loc}")));
                        // eprintln!("load of {val:?} to {loc} ({reg:?}) complete");
                    }
                    Val::Binding(_) => todo!(),
                    Val::Literal(v) => {
                        let reg = alloc.move_to_reg(*loc);
                        ret.extend_from_slice(&alloc.take_queue());
                        ret.push(AsmOp::Mov(OpTarget::Reg(reg), OpTarget::Literal(*v)));
                        ret.push(AsmOp::Comment(format!(" ^^^ loading literal to {loc}",)));
                    }
                    Val::Alloca(ty) => {
                        alloc.alloca(*loc, ty.size());
                        ret.push(AsmOp::Comment(
                            format!(" allocating stack space for {loc}",),
                        ));
                    }
                };
            }
            OpInner::Block { id, args } => {
                ret.extend_from_slice(&alloc.setup_call(&args, CallType::Block));
                ret.push(AsmOp::Label(block_label(*id)));
            }
            OpInner::Br {
                check,
                success,
                fail,
            } => {
                let regl = alloc.move_to_reg(check.0);
                let regr = alloc.move_to_reg(check.1);
                ret.extend_from_slice(&alloc.take_queue());
                let post_label = unique_label();
                ret.push(AsmOp::Cmp(OpTarget::Reg(regl), OpTarget::Reg(regr)));
                ret.push(AsmOp::Comment(format!(
                    " ^^^ cmp {} with {}",
                    &check.0, &check.1
                )));
                let backup = alloc.clone();
                ret.push(AsmOp::Jeq(OpTarget::LitLabel(post_label.clone())));
                ret.extend(alloc.setup_call(&success.args, CallType::Block));
                ret.push(AsmOp::Jmp(OpTarget::Label(success.id)));
                ret.push(AsmOp::Label(post_label));
                ret.extend(alloc.setup_call(&fail.args, CallType::Block));
                ret.push(AsmOp::Jmp(OpTarget::Label(fail.id)));

                std::mem::replace(alloc, backup);
            }
            OpInner::Jmp { target } => {
                ret.extend_from_slice(&alloc.setup_call(&target.args, CallType::Block));
                ret.push(AsmOp::Jmp(OpTarget::Label(target.id)));
            }
            OpInner::RegAllocOp(op) => {
                alloc.apply_op(op);
                ret.extend_from_slice(&alloc.take_queue());
            }
            OpInner::Load { loc, ptr } => {
                let dst = alloc.move_to_reg(*loc);
                let src = alloc
                    .get_alloc(*ptr)
                    .expect("this fails often since allocators keep track of alloca, see comments");
                // I don't necessarily want to have to keep the stack-only variables in a register,
                // which means I need to either share the stack variables, or include in the
                // calling convention for blocks a way to pass variables that are just static
                // stack addresses. I suspect this will entail a much richer calling convention
                // (that may, for example let me pass registers out of order)
                ret.extend_from_slice(&alloc.take_queue());
                ret.push(AsmOp::Mov(OpTarget::Reg(dst), OpTarget::Stack(src)));
                ret.push(AsmOp::Comment(format!(" ^^^ {loc} := *{ptr}",)));
            }
            OpInner::Store { dst, src } => {
                let rsrc = alloc.move_to_reg(*src);
                let Some(odst) = alloc.get_alloc(*dst) else {
                    panic!("variable {dst} was never put on the stack")
                };
                ret.push(AsmOp::Mov(OpTarget::Stack(odst), OpTarget::Reg(rsrc)))
            }
            OpInner::Op { ins } => {

            },
        }
        ret
    }

    fn vars_referenced(&self) -> Vec<Binding> {
        match &self.inner {
            OpInner::Call { id, args } => args.iter().copied().collect(),
            OpInner::Assign { loc, val } => vec![*loc],
            OpInner::Block { id, args } => args.iter().copied().collect(),
            OpInner::Br {
                check,
                success,
                fail,
            } => Vec::from_iter(
                [*check]
                    .into_iter()
                    .chain(success.args.iter().copied().chain(fail.args.iter().copied())),
            ),
            OpInner::Jmp { target } => target.args.iter().copied().collect(),
            OpInner::Load { loc, ptr } => {
                vec![*loc, *ptr]
            }
            OpInner::Store { dst, src } => {
                vec![*dst, *src]
            }
            OpInner::Op { ins } => {
                ins.set_registers().chain(ins.read_registers()).collect()
            },
        }
    }

    fn vars_clobbered(&self) -> Vec<Binding> {
        if let OpInner::Op { ins } = self.inner {
            return ins.set_registers().collect()
        }
        Vec::new()
    }

    fn regs_clobbered(&self) -> &[u8] {
        if let OpInner::Call { id, .. } = &self.inner {
            if *id == 0 {
                return CallType::Syscall.clobers_idx();
            }
        }
        &[]
    }

    fn call_type(&self) -> Option<CallType> {
        if let OpInner::Call { id, args } = &self.inner {
            if *id == 0 {
                return Some(CallType::Syscall);
            }
        }
        if self.is_branch() {
            return Some(CallType::Block);
        }
        None
    }

    /// Returns `true` if the op is a branch instruction.
    #[must_use]
    fn is_branch(&self) -> bool {
        match self.inner {
            OpInner::Br { .. } => true,
            OpInner::Jmp { .. } => true,
            OpInner::Op { ins } => ins.is_branch,
            _ => false
        }
    }

    #[must_use]
    fn is_term(&self) -> bool {
        match self.inner {
            OpInner::Br { .. } => true,
            OpInner::Jmp { .. } => true,
            OpInner::Op { ins } => ins.is_branch,
            OpInner::Call { .. } => true,
            _ => false
        }
    }
}
*/

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
        const BLOCK_REGS: [u8; 13] = [
            Reg::Rax.idx(),
            Reg::Rcx.idx(),
            Reg::Rdx.idx(),
            Reg::Rsi.idx(),
            Reg::Rdi.idx(),
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
        const SYSCALL_REGS: [u8; 3] = [Reg::Rax.idx(), Reg::Rcx.idx(), Reg::R11.idx()];
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

impl std::fmt::Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.name().fmt(f)
    }
}

struct Config {
    emit_debug_syms: bool,
    emit_comments: bool,
    input_files: Vec<Box<str>>,
    output_file: Option<Box<str>>,
}

impl Config {
    fn new() -> Self {
        Self {
            emit_comments: true,
            emit_debug_syms: false,
            output_file: None,
            input_files: Vec::new(),
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

    let Input { mut config } = cli::read_args()?;
    if config.input_files.is_empty() {
        config.input_files.push("input.ll".into());
    }
    let input_file = config.input_files.first().map(|s| &**s).unwrap();

    let mut input = parse::Parser::new_file(input_file)?;
    let ParsedFile {
        routine,
        vars,
        globals,
    } = input.parse();
    reg::bind_names::register_many((*vars).clone());
    // let mut functions = [routine];
    let ctx = Ctx {
        config,
        vars,
        globals,
    };
    // for instr in routine.into_instr_vec() {
    //     println!("{instr}");
    // }

    // let asm = asm::make_asm(&ctx, &mut functions);

    let mut f = std::fs::File::create("out.s")?;
    f.set_len(0);
    // write!(f, "{asm}");

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
