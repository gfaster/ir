use std::ops::{Bound, RangeBounds};

use crate::{ty::Type, reg::Immediate};

#[derive(Debug, Clone)]
pub(super) enum KnownVal {
    /// an unknown value, but still well defined
    Unknown,

    /// a known, exact value
    Known(u64),

    /// the set of all possible values
    Undefined,

    /// can be treated as any value, using it causes UB
    ///
    /// see: [https://llvm.org/devmtg/2020-09/slides/Lee-UndefPoison.pdf]
    Poison,

    // /// not zero
    // NonZero,

    /// inclusive lower bound
    Lower(u64),

    /// inclusive upper bound
    Upper(u64),

    /// inclusive range like [`std::ops::RangeInclusive`]
    Range(u64, u64),
}

impl KnownVal {
    pub const fn from_imm(imm: Immediate) -> Self {
        Self::Known(imm.0 as u64)
    }

    pub const fn new() -> Self {
        Self::Unknown
    }

    /// returns true if range is known to be `val` or if it's undefined/poison
    ///
    /// Uses should use [`Self::is_not_val`] if it will allow better optimizations
    pub fn is_val(&self, val: u64) -> bool {
        (if let KnownVal::Known(k) = self {
            *k == val
        } else {
            false
        }) || matches!(self, KnownVal::Poison | KnownVal::Undefined)
    }

    /// returns true if range is known to be not `val` or if it's undefined/poison
    ///
    /// Uses should use [`Self::is_val`] if it will allow better optimizations
    pub fn is_not_val(&self, val: u64) -> bool {
        // don't include undef since undef is the set of all possible values
        !self.is_val_possible(val) || matches!(self, KnownVal::Poison)
    }

    pub fn min_val(&self, ty: Type) -> u64 {
        match self {
            KnownVal::Known(k) => *k,
            KnownVal::Lower(l) | KnownVal::Range(l, _) => *l,
            _ => 0
        }
    }

    pub fn max_val(&self, ty: Type) -> u64 {
        match self {
            KnownVal::Known(k) => *k,
            KnownVal::Upper(u) | KnownVal::Range(_, u) => *u,
            _ => ty.max()
        }
    }

    pub fn is_val_possible(&self, val: u64) -> bool {
        match self {
            KnownVal::Unknown => true,
            KnownVal::Known(v) => *v == val,
            KnownVal::Poison => true,
            // KnownVal::NonZero => val != 0,
            KnownVal::Lower(l) => (l..).contains(&&val),
            KnownVal::Upper(h) => (..=h).contains(&&val),
            KnownVal::Range(l, h) => (l..=h).contains(&&val),
            KnownVal::Undefined => true,
        }
    }

    pub fn as_known_val(&self) -> Option<u64> {
        if let KnownVal::Known(v) = self {
            Some(*v)
        } else {
            None
        }
    }

    /// restricts the lower bound, need to normalize after
    fn restrict_lower_bound(&mut self, ty: Type, bound: Bound<&u64>) {
        let lower_inc = match bound {
            Bound::Included(&b) => Bound::Included(b),
            Bound::Excluded(&b) => match b.checked_add(1) {
                Some(b) => Bound::Included(b),
                None => {
                    *self = KnownVal::Poison;
                    return
                },
            }
            Bound::Unbounded => Bound::Unbounded,
        };
        let Bound::Included(b) = lower_inc else {return};
        *self = match self {
            KnownVal::Unknown => KnownVal::Lower(b),
            KnownVal::Known(v) if *v < b => KnownVal::Poison,
            KnownVal::Known(k) => KnownVal::Known(*k),
            KnownVal::Poison => KnownVal::Poison,
            // KnownVal::NonZero if b > 0 => self.val = KnownVal::Lower(b),
            // KnownVal::NonZero => self.val = KnownVal::NonZero,
            KnownVal::Lower(l) => KnownVal::Lower((*l).max(b)),
            KnownVal::Upper(u) => KnownVal::Range(b, *u),
            KnownVal::Range(l, u) => KnownVal::Range((*l).max(b), *u),
            KnownVal::Undefined => KnownVal::Undefined,
        };
    }

    /// restricts the upper bound, need to normalize after
    fn restrict_upper_bound(&mut self, ty: Type, bound: Bound<&u64>) {
        let upper_inc = match bound {
            Bound::Excluded(b) => b.checked_sub(1).map_or(Bound::Unbounded, |b| Bound::Included(b)),
            Bound::Included(b) => Bound::Included(*b),
            Bound::Unbounded => Bound::Unbounded,
        };
        let Bound::Included(b) = upper_inc else {return};
        *self = match self {
            KnownVal::Unknown => KnownVal::Upper(b),
            KnownVal::Known(v) if *v > b => KnownVal::Poison,
            KnownVal::Known(k) => KnownVal::Known(*k),
            KnownVal::Poison => KnownVal::Poison,
            KnownVal::Upper(u) => KnownVal::Upper((*u).min(b)),
            KnownVal::Lower(l) => KnownVal::Range(*l, b),
            KnownVal::Range(l, u) => KnownVal::Range(*l, (*u).min(b)),
            KnownVal::Undefined => KnownVal::Undefined,
        };
    }

    fn normalize(&mut self, ty: Type) {
        if let KnownVal::Range(l, u) = self {
            if *l > *u {
                *self = KnownVal::Poison;
                return;
            };
            *u = ty.max().min(*u);
            if *l == *u {
                *self = KnownVal::Known(*l)
            } else if *l == u64::MIN {
                *self = KnownVal::Upper(*u)
            }
        }
        if let KnownVal::Lower(l) = self {
            if *l > ty.max() {
                *self = KnownVal::Poison
            } else if *l == u64::MIN {
                *self = KnownVal::Unknown
            }
        } else if let KnownVal::Upper(u) = self {
            *u = ty.max().min(*u);
            if *u == ty.max() {
                *self = KnownVal::Unknown
            }
        }
    }

    /// make the known value of this bounded by `bound`. Can make the value undefined if bounded to
    /// have no possible value.
    pub fn restrict_bound(&mut self, ty: Type, bound: impl RangeBounds<u64>) {
        self.restrict_lower_bound(ty, bound.start_bound());
        self.restrict_upper_bound(ty, bound.end_bound());
        self.normalize(ty);
    }

    /// Apply an operation and get a new constraint. I eventually want to expand this to handle
    /// triggering UB, but for now, it can just propagate poison and undef (i.e. extremely
    /// conservative).
    ///
    /// `f` must satisfy the following properties:
    /// - `{f(a, b) | a in {la, ha}, b in {lb, hb}}` contains the minimum and maximum values that `f`
    /// may return over `a in [la, ha], b in [lb,hb]` (is conservative about poison)
    /// - returns `None` if the operation causes poison
    /// - *more to be added*
    ///
    /// if this is too stringent, use [`apply_op_conservative`]
    pub fn apply_op(ty: Type, f: impl Fn(u64, u64) -> Option<u64>, lhs: &Self, rhs: &Self) -> Self {
        // see: https://llvm.org/devmtg/2020-09/slides/Lee-UndefPoison.pdf
        let ret = match (lhs, rhs) {
            (KnownVal::Undefined, KnownVal::Undefined) => KnownVal::Undefined,
            (KnownVal::Undefined, _) | (_, KnownVal::Undefined) => KnownVal::Unknown,
            (KnownVal::Poison, _) | (_, KnownVal::Poison) => KnownVal::Poison,
            (KnownVal::Known(l), KnownVal::Known(r)) => {
                match f(*l, *r) {
                    Some(k) => KnownVal::Known(k),
                    None => KnownVal::Poison,
                }
            },
            (_, _) => {
                let la = lhs.min_val(ty);
                let lb = rhs.min_val(ty);
                let ha = lhs.max_val(ty);
                let hb = rhs.max_val(ty);
                let vals = [
                    f(la, lb),
                    f(la, hb),
                    f(ha, lb),
                    f(ha, hb),
                ];
                let min = vals.iter().flatten().copied().min().unwrap_or(0);
                let max = vals.iter().flatten().copied().max().unwrap_or(u64::MAX);
                let mut ret = Self::new();
                ret.restrict_bound(ty, min..=max);
                ret
            },
        };
        ret
    }

    /// Apply an operation and get a new constraint. I eventually want to expand this to handle
    /// triggering UB, but for now, it can just propagate poison and undef (i.e. extremely
    /// conservative).
    ///
    /// `f` must satisfy the following properties:
    /// - returns `None` if the operation causes poison
    /// - *more to be added*
    pub fn apply_op_conservative(ty: Type, f: impl Fn(u64, u64) -> Option<u64>, lhs: &Self, rhs: &Self) -> Self {
        // see: https://llvm.org/devmtg/2020-09/slides/Lee-UndefPoison.pdf
        let ret = match (lhs, rhs) {
            (KnownVal::Undefined, KnownVal::Undefined) => KnownVal::Undefined,
            (KnownVal::Undefined, _) | (_, KnownVal::Undefined) => KnownVal::Unknown,
            (KnownVal::Poison, _) | (_, KnownVal::Poison) => KnownVal::Poison,
            (KnownVal::Known(l), KnownVal::Known(r)) => {
                match f(*l, *r) {
                    Some(k) => KnownVal::Known(k),
                    None => KnownVal::Poison,
                }
            },
            (_, _) => KnownVal::Unknown,
        };
        ret
    }
}
