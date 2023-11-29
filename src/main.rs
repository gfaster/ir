#![allow(unused)]

// MVP FIRST!!! 

use std::fmt::Write as FmtWrite;
use std::io::Write;

/// symbol id currently global scope and unique
type Id = usize;

/// Id of a local
type Loc = usize;

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
        writeln!(out, "{name}:", name = self.name).unwrap();
        for op in &self.ops {
            writeln!(out, "    {op}", op = op.to_asm(globals)).unwrap();
        }
        writeln!(out).unwrap();
        out
    }
}

enum Op {
    Call{id: Id, args: Vec<Loc>},
    Load{loc: Loc, val: Val},
}

impl Op {
    fn to_asm(&self, globals: &[GlobalData]) -> String {
        match self {
            Op::Call { id, args } => {
                match *id {
                    0 => "syscall",
                    _ => todo!()
                }.to_owned()
            },
            Op::Load { loc, val } => {
                let reg = Reg::from_loc(*loc).name();
                match val {
                    Val::GlobalLabel(g) => format!("lea {reg}, [rip + {}]" , &globals[*g].name),
                    Val::LocalLabel(_) => todo!(),
                    Val::Literal(v) => format!("mov {reg}, {v}"),
                }
            },
        }
    }
}

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
                Op::Load { loc: 4, val: Val::Literal(1) },
                Op::Load { loc: 3, val: Val::GlobalLabel(0) },
                Op::Load { loc: 2, val: Val::Literal(14) },
                Op::Call { id: 0, args: vec![0, 4, 3, 2] },
                Op::Load { loc: 0, val: Val::Literal(60) },
                Op::Load { loc: 4, val: Val::Literal(0) },
                Op::Call { id: 0, args: vec![0, 4] }
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
