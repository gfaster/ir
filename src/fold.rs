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

use crate::{instr::Instruction, regstate::SSAState, attr::BindAttributes, warn_once, reg::Immediate};

pub type FoldFn = fn(&mut SSAState, &Instruction, &[bool]) -> bool;
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
pub fn unimmediate(state: &mut SSAState, instr: &Instruction, _uses: &[bool]) -> bool {
    todo!()
}

/// fold simulation - automatically applied to any instruction with a simulation
pub fn fold_sim(state: &mut SSAState, instr: &Instruction, _uses: &[bool]) -> bool {
    let Some(sim) = instr.sim() else { return false };
    let Some((def, arg1, arg2)) = instr.as_binary() else { return false };

    let attr1_bind;
    let attr1 = if let Some(imm) = arg1.as_imm() {
        attr1_bind = BindAttributes::from_imm(crate::ty::Type::i64(), imm);
        warn_once!("FIXME: use real types");
        &attr1_bind
    } else {
        let bind = arg1.as_binding().expect("is binding if not imm");
        state.bind_attrs(bind)
    };

    let attr2_bind;
    let attr2 = if let Some(imm) = arg2.as_imm() {
        attr2_bind = BindAttributes::from_imm(crate::ty::Type::i64(), imm);
        warn_once!("FIXME: use real types");
        &attr2_bind
    } else {
        let bind = arg2.as_binding().expect("is binding if not imm");
        state.bind_attrs(bind)
    };

    let res = state.bind_attrs(def).constrain_val_pure(attr1, attr2, sim);
    let ret;
    if let Some(val) = res.as_known_val() {
        // let new = Instruction::make_assignment(def, Immediate(val as usize).into());
        // state.replace_instr(instr, new);
        ret = true;
        todo!();
    } else {
        ret = false;
    }
    state.update_bind_attrs(def, res);

    ret
}

/// assignment propagation
pub fn prop_assignment(state: &mut SSAState, instr: &Instruction, _uses: &[bool]) -> bool {
    todo!()
}
