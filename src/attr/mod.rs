use std::{ops::{RangeBounds, Bound}, sync::{Arc, Mutex, MutexGuard}, collections::{BTreeSet, BTreeMap}};

use crate::{ty::Type, reg::{Binding, Immediate}};

mod valid_range;
use valid_range::KnownVal;

#[derive(Debug, Clone)]
pub struct BindAttributes {
    ty: Type,
    name: Option<Arc<str>>,
    val: KnownVal
}

impl BindAttributes {
    pub fn ty(&self) -> Type {
        self.ty
    }

    pub const fn from_imm(ty: Type, imm: Immediate) -> Self {
        Self {
            ty,
            name: None,
            val: KnownVal::from_imm(imm),
        }
    }

    pub const fn new(ty: Type) -> Self {
        Self {
            ty,
            name: None,
            val: KnownVal::new(),
        }
    }

    pub fn with_name(self, name: Arc<str>) -> Self {
        Self {
            name: Some(name),
            ..self
        }
    }

    pub fn set_name(&mut self, name: Arc<str>) -> &mut Self {
        self.name = Some(name);
        self
    }

    /// constrain the known value to the possible results of `f`
    ///
    /// `f` must satisfy the following properties:
    /// - returns `None` if the operation causes poison
    /// - *more to be added*
    pub fn constrain_val(&mut self, lhs: &Self, rhs: &Self, f: impl Fn(u64, u64) -> Option<u64>) {
        self.val = KnownVal::apply_op_conservative(self.ty, f, &lhs.val, &rhs.val);
    }

    /// constrain the known value to the possible results of `f`, returning a new attrs
    ///
    /// `f` must satisfy the following properties:
    /// - returns `None` if the operation causes poison
    /// - *more to be added*
    pub fn constrain_val_pure(&self, lhs: &Self, rhs: &Self, f: impl Fn(u64, u64) -> Option<u64>) -> Self {
        let mut new = self.clone();
        new.val = KnownVal::apply_op_conservative(self.ty, f, &lhs.val, &rhs.val);
        new
    }

    pub fn as_known_val(&self) -> Option<u64> {
        self.val.as_known_val()
    }

    /// returns true if range is known to be `val` or if it's undefined
    pub fn is_val(&self, val: u64) -> bool {
        self.val.is_val(val)
    }

    /// returns true if it's possible for this to be `val` 
    pub fn is_val_possible(&self, val: u64) -> bool {
        self.val.is_val_possible(val)
    }

    /// make the known value of this bounded by `bound`. Can make the value undefined if bounded to
    /// have no possible value.
    pub fn restrict_bound(&mut self, bound: impl RangeBounds<u64>) {
        self.val.restrict_bound(self.ty, bound)
    }

    pub fn max_val(&self) -> u64 {
        self.val.max_val(self.ty)
    }

    pub fn min_val(&self) -> u64 {
        self.val.min_val(self.ty)
    }

    pub fn is_not_val(&self, val: u64) -> bool {
        self.val.is_not_val(val)
    }
}
