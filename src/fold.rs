//! I need to think about how this is supposed to work. I need some way to transform all of the
//! following situations:
//!
//! ```txt
//! %1 = 5
//! %2 = add i64 %1 %other
//! to
//! %2 = add i64 5 %other
//! ```
//! Q: can a binding be an immediate?
//!
//! A: it's clear that an immediate can't be an lvalue, but otherwise it should be ok. It will make
//! folding checks much easier, but can potentially complicate legalization
//!
//! ```txt
//! %1 = 5
//! %2 = add i64 %1 %1
//! to
//! %2 = 10
//! ```

use crate::{attr::BindAttributes, instr::Instruction, reg::Immediate, warn_once};

// pub type FoldFn = fn(&mut SSAState, &Instruction, &[bool]) -> bool;
// ex:
// pub fn fold_sim(state: &mut SSAState, instr: &Instruction, uses: &[bool]) -> bool {}

/// remove non-assignment immediates
///
/// e.g:
/// ```llvm
/// %a = add i64 %b 1
/// ```
/// to
/// ```llvm
/// %1 = i64 1
/// %a = add i64 %b %1
/// ```
///
/// This allows us to guarantee there is no immediates in instructions, which will allow for an
/// easier time with selection.
pub fn unimmediate(instr: &Instruction, _uses: &[bool]) -> bool {
    todo!()
}

/// fold simulation - automatically applied to any instruction with a simulation
pub fn fold_sim(instr: &Instruction, _uses: &[bool]) -> bool {
    // let Some(sim) = instr.sim() else { return false };
    todo!()
}

/// assignment propagation
pub fn prop_assignment(instr: &Instruction, _uses: &[bool]) -> bool {
    todo!()
}
