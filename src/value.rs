//! I quite like the idea of LLVM's Value class, but it doesn't play nicely with instructions that
//! produce multiple values, so I need to figure out how to make it play nice. Also, I want to make
//! it not OOP.

use std::{cell::{RefCell, Ref, RefMut}, ops::{Deref, DerefMut}, ptr::NonNull};

use crate::{ty::Type, attr::BindAttributes, reg::Immediate, instr::Instruction};

type OptPtr<T> = Option<NonNull<T>>;

pub struct Value {
    inner: RefCell<ValueInner>
}

pub struct ValueInner {
    attr: BindAttributes,
    inner: ValueType,
}

enum ValueType {
    Imm(Immediate),
    Instr { 
        instr: Instruction,
        add_res: OptPtr<Value>
    },
    AdditionalResult { 
        origin: *const Value,
        next: OptPtr<Value>
    }
}

impl ValueInner {
    fn next_res(&self) -> Option<&ValueInner> {
        todo!()
    }
}

/* 
 *  ==============================
 *  boring stuffs
 *  ============================== 
*/

pub struct VRef<'a>(Ref<'a, ValueInner>);
pub struct VMut<'a>(RefMut<'a, ValueInner>);

impl Deref for VMut<'_> {
    type Target = ValueInner;

    fn deref(&self) -> &ValueInner {
        &self.0
    }
}

impl DerefMut for VMut<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Deref for VRef<'_> {
    type Target = ValueInner;

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
