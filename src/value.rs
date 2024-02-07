//! I quite like the idea of LLVM's Value class, but it doesn't play nicely with instructions that
//! produce multiple values, so I need to figure out how to make it play nice. Also, I want to make
//! it not OOP.

use std::{
    cell::{self, RefCell, RefMut},
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

use crate::{
    attr::BindAttributes,
    instr::{DebugInfo, Instruction},
    list::{self, List, ThinRef},
    reg::Immediate,
    ty::Type,
    warn_once, GlobalData,
};

type OptPtr<T> = Option<NonNull<T>>;
type InternalHandle = ThinRef<Value>;

/// A function.
pub struct Function {
    text: List<Value>,
}

impl Function {
    pub fn new() -> Self {
        Self { text: List::new() }
    }

    pub fn insert_value_after(&self, val: impl Into<Value>, loc: ValueHandle) -> ValueHandle {
        let wref = self.internal_deref(loc.0).insert_after(val.into());
        ValueHandle(wref.into())
    }

    pub fn push_value(&self, val: impl Into<Value>) -> ValueHandle {
        let wref = self.text.push_back(val.into());
        ValueHandle(wref.into())
    }

    pub fn insert_imm_after(&self, ty: Type, imm: Immediate, loc: ValueHandle) -> ValueHandle {
        let attr = BindAttributes::from_imm(ty, imm);
        let inner = ValueInner {
            attr,
            dbg_info: None,
            users: None,
            inner: ValueType::Imm(imm),
        };
        let loc = self.internal_deref(loc.0);
        let new = loc.insert_after(inner.into());
        ValueHandle(new.into())
    }

    pub fn push_imm(&self, ty: Type, imm: Immediate) -> ValueHandle {
        let attr = BindAttributes::from_imm(ty, imm);
        self.push_value(ValueInner {
            attr,
            dbg_info: None,
            users: None,
            inner: ValueType::Imm(imm),
        })
    }

    /// pointer only
    pub fn push_global(&self, global: GlobalData) -> ValueHandle {
        let attr = BindAttributes::new(Type::Ptr);
        self.push_value(ValueInner {
            attr,
            dbg_info: None,
            users: None,
            inner: ValueType::Global(global),
        })
    }

    /// returns an iterator with length one longer than the length of `args`, with the first is the
    /// label itself
    pub fn push_block(
        &self,
        args: impl IntoIterator<Item = BindAttributes>,
    ) -> impl Iterator<Item = ValueHandle> {
        let first = ValueInner {
            attr: BindAttributes::new(Type::Label),
            dbg_info: None,
            users: None,
            inner: ValueType::Block { args: None },
        };
        let origin = self.text.push_back(first.into());
        let mut prev = origin;

        let mut ret: Vec<ValueHandle> = vec![ValueHandle(origin.into())];

        for attr in args {
            let next = ValueInner {
                attr,
                dbg_info: None,
                users: None,
                inner: ValueType::BlockArg {
                    origin: origin.into(),
                    next: None,
                },
            };
            let next = prev.insert_after(next.into());
            ret.push(ValueHandle(next.into()));

            prev.inner.borrow_mut().set_next_ptr(next.into());
            prev = next;
        }

        ret.into_iter()
    }

    pub fn insert_instruction_after(
        &self,
        instr: Instruction,
        res_attr: impl IntoIterator<Item = BindAttributes>,
        loc: ValueHandle,
    ) -> impl Iterator<Item = ValueHandle> {
        self.insert_instruction_after_generic(instr, res_attr, |val| {
            self.internal_deref(loc.0).insert_after(val)
        })
        .into_iter()
    }

    pub fn push_instruction(
        &self,
        instr: Instruction,
        res_attr: impl IntoIterator<Item = BindAttributes>,
    ) -> impl Iterator<Item = ValueHandle> {
        self.insert_instruction_after_generic(instr, res_attr, |val| self.text.push_back(val))
            .into_iter()
    }

    fn insert_instruction_after_generic<'a>(
        &'a self,
        instr: Instruction,
        res_attr: impl IntoIterator<Item = BindAttributes>,
        insert_first: impl FnOnce(Value) -> list::Ref<'a, Value>,
    ) -> Vec<ValueHandle> {
        let mut attr_cnt = 1;
        let mut res_attr = res_attr.into_iter();
        let expected_res_num = instr.res_cnt();
        let attr = res_attr
            .next()
            .expect("require at least one result attribute");
        let first = ValueInner {
            attr,
            dbg_info: None,
            users: None,
            inner: ValueType::Instr {
                instr,
                add_res: None,
            },
        };
        let origin = insert_first(first.into());
        let mut prev = origin;

        let mut ret: Vec<ValueHandle> = vec![ValueHandle(origin.into())];

        for attr in res_attr {
            attr_cnt += 1;
            let next = ValueInner {
                attr,
                dbg_info: None,
                users: None,
                inner: ValueType::AdditionalResult {
                    origin: origin.into(),
                    next: None,
                },
            };
            let next = prev.insert_after(next.into());
            ret.push(ValueHandle(next.into()));

            // Safety: this is in the same function
            prev.inner.borrow_mut().set_next_ptr(next.into());
            prev = next;
        }

        if expected_res_num == 0 {
            assert_eq!(attr_cnt, 1);
            assert_eq!(origin.inner.borrow().attr.ty(), Type::void());
        } else {
            assert_eq!(attr_cnt, expected_res_num as u32);
        }
        ret
    }

    pub fn get_value<'a>(&'a self, vref: ValueHandle) -> VRef<'a> {
        let lref = vref.0.promote(&self.text);
        VRef(lref.into_inner().inner.borrow())
    }

    pub fn get_value_mut<'a>(&'a self, vref: ValueHandle) -> VMut<'a> {
        let lref = vref.0.promote(&self.text);
        VMut(lref.into_inner().inner.borrow_mut())
    }

    pub fn block_iter(&self) -> impl Iterator<Item = Block> {
        self.text.iter().filter(|v| {
            v.inner.borrow().inner.is_block()
        }).map(|b| {
                Block { func: self, head: b.into() }
            })
    }

    fn end_of_res_list<'a>(&'a self, ptr: list::Ref<'a, Value>) -> list::Ref<'a, Value> {
        match ptr.inner.borrow().inner {
            ValueType::Block { args: Some(next) }
            | ValueType::BlockArg {
                next: Some(next), ..
            }
            | ValueType::Instr {
                add_res: Some(next),
                ..
            }
            | ValueType::AdditionalResult {
                next: Some(next), ..
            } => self.end_of_res_list(next.promote(&self.text)),
            _ => ptr,
        }
    }

    fn internal_deref(&self, iref: InternalHandle) -> list::Ref<Value> {
        self.end_of_res_list(iref.promote(&self.text))
    }
}

impl std::fmt::Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for val in &self.text {
            let val = val.inner.borrow();
            let name = val.attr.name();
            let ty = val.attr.ty();
            if matches!(val.inner, ValueType::Imm(_)) {
                warn_once!("[WARNING] We lie about what immediates are");
                continue;
            }
            if !ty.is_void() {
                write!(f, "{ty:?} {name}")?;
            }
            match &val.inner {
                ValueType::Global(_) => writeln!(f, "[[global value]]"),
                ValueType::Imm(i) => writeln!(f, " = {i}"),
                ValueType::Block { args } => {
                    write!(f, "(")?;
                    if args.is_none() {
                        writeln!(f, ")")?;
                    }
                    Ok(())
                }
                ValueType::BlockArg { origin, next } => {
                    if next.is_none() {
                        writeln!(f, "):")?;
                    }
                    Ok(())
                }
                ValueType::Instr { instr, add_res } => {
                    let mnemonic = instr.mnemonic();
                    if !ty.is_void() {
                        write!(f, " = ")?;
                    }
                    write!(f, "{mnemonic} ")?;

                    if mnemonic == "call" {
                        write!(f, "(")?;
                    }

                    for op in instr.args_iter() {
                        let rcell = self.get_value(op);
                        let ty = rcell.attr.ty();
                        if let ValueType::Imm(i) = rcell.inner {
                            write!(f, "{ty:?} {i}, ")?;
                        } else {
                            let name = rcell.attr.name();
                            write!(f, "{ty:?} {name}, ")?;
                        }
                    }

                    if mnemonic != "call" {
                        writeln!(f, "")?;
                    } else {
                        writeln!(f, ")")?;
                    }
                    Ok(())
                }
                ValueType::AdditionalResult { origin, next } => Ok(()),
            }?
        }
        Ok(())
    }
}

pub struct Value {
    inner: RefCell<ValueInner>,
}

pub struct ValueInner {
    pub attr: BindAttributes,
    pub dbg_info: Option<DebugInfo>,
    users: Option<InternalHandle>,
    inner: ValueType,
}

enum ValueType {
    Global(GlobalData),
    Imm(Immediate),
    Block {
        args: Option<InternalHandle>,
    },
    BlockArg {
        origin: InternalHandle,
        next: Option<InternalHandle>,
    },
    Instr {
        instr: Instruction,
        add_res: Option<InternalHandle>,
    },
    AdditionalResult {
        origin: InternalHandle,
        next: Option<InternalHandle>,
    },
}

impl Value {
    pub fn borrow(&self) -> cell::Ref<'_, ValueInner> {
        self.inner.borrow()
    }

    pub fn borrow_mut(&self) -> RefMut<'_, ValueInner> {
        self.inner.borrow_mut()
    }
}

impl ValueType {
    /// Returns `true` if the value type is [`Block`].
    ///
    /// [`Block`]: ValueType::Block
    #[must_use]
    fn is_block(&self) -> bool {
        matches!(self, Self::Block { .. })
    }
}

impl ValueInner {
    /// set the next ptr for multiple result instructions
    fn set_next_ptr(&mut self, ptr: InternalHandle) {
        match &mut self.inner {
            ValueType::Instr { add_res, .. } => *add_res = Some(ptr),
            ValueType::Block { args } => *args = Some(ptr),
            ValueType::BlockArg { next, .. } => *next = Some(ptr),
            ValueType::AdditionalResult { next, .. } => *next = Some(ptr),
            _ => panic!("can only call set_next_ptr on an instruction result"),
        }
    }

    pub fn as_instr(&self) -> Option<&Instruction> {
        let ValueType::Instr { instr, .. } = &self.inner else { return None };
        Some(instr)
    }

    fn next_res(&self) -> Option<&ValueInner> {
        todo!()
    }
}

pub struct Block<'a> {
    func: &'a Function,
    head: InternalHandle,
}

impl Block<'_> {
    pub fn val_iter(&self) -> impl Iterator<Item = &Value> {
        let head = self.func.internal_deref(self.head);
        let mut past_first = false;
        head.make_iter().take_while(move |v| {
            let pass = !past_first || !v.inner.borrow().inner.is_block();
            past_first = true;
            pass
        }).map(|x| x.into_inner())
    }
}

/*
 *  ==============================
 *  boring stuffs
 *  ==============================
*/

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ValueHandle(ThinRef<Value>);

pub struct VRef<'a>(cell::Ref<'a, ValueInner>);

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

impl From<ValueInner> for Value {
    fn from(value: ValueInner) -> Self {
        Value {
            inner: value.into(),
        }
    }
}

impl ValueHandle {
    fn from_listref(lref: list::Ref<Value>) -> Self {
        Self(lref.into())
    }
}
