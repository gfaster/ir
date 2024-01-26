use std::mem::MaybeUninit;
use std::ptr::NonNull;
use std::{num::NonZeroUsize, cell::{UnsafeCell, Cell}};



#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Idx(NonZeroUsize);

impl Idx {
    pub fn new(val: usize) -> Self {
        Self(NonZeroUsize::new(val + 1).expect("val nonzero"))
    }

    pub fn idx(&self) -> usize {
        self.0.get()
    }
}
impl From<Idx> for usize {
    fn from(value: Idx) -> Self {
        value.idx()
    }
}
impl From<&Idx> for usize {
    fn from(&value: &Idx) -> Self {
        value.into()
    }
}
impl<T> std::ops::Index<Idx> for Vec<T> {
    type Output = T;

    fn index(&self, index: Idx) -> &Self::Output {
        &self[index.idx()]
    }
}
impl<T> std::ops::Index<&Idx> for Vec<T> {
    type Output = T;

    fn index(&self, &index: &Idx) -> &Self::Output {
        &self[index.idx()]
    }
}

struct AppendVecInner<T> {
    // prev: *const AppendVecInner<T>,
    next: UnsafeCell<Option<NonNull<AppendVecInner<T>>>>,
    start: usize,
    cnt: Cell<usize>,
    vals: [UnsafeCell<MaybeUninit<T>>; 50]
}

impl<T> AppendVecInner<T> {
    // fn new(start: usize, prev: Option<&Self>) -> Box<Self> {
    fn new(start: usize) -> NonNull<Self> {
        unsafe {
            NonNull::new_unchecked(Box::into_raw(Box::new( Self {
                // prev: prev.map_or(ptr::null(), |p| p as *const Self),
                next: UnsafeCell::new(None),
                start,
                cnt: 0.into(),
                vals: std::array::from_fn(|_| UnsafeCell::new(MaybeUninit::uninit())),
            })))
        }
    }
}

impl<T> Drop for AppendVecInner<T> {
    fn drop(&mut self) {
        unsafe {
            for i in 0..self.cnt.get() {
                self.vals[i].get_mut().assume_init_drop()
            }
            if let Some(next) = self.next.get_mut() {
                let _ = Box::from_raw(next.as_ptr());
            }
        }
    }
}

/// append-only Vec. Indexing may be slow-ish, but elements have stable pointers for the lifetime
/// of the `AppendVec`
pub struct AppendVec<T> {
    inner: NonNull<AppendVecInner<T>>,
    len: Cell<usize>
}

impl<T> AppendVec<T> {
    pub fn new() -> Self {
        AppendVec { inner: AppendVecInner::new(0), len: 0.into() }
    }

    pub fn len(&self) -> usize {
        self.len.get()
    }

    unsafe fn get_idx(&self, mut idx: usize) -> (&AppendVecInner<T>, usize) {
        let mut seg = self.inner.as_ref();
        while idx >= seg.vals.len() {
            idx -= seg.vals.len();
            unsafe {
                if let Some(next) = &*seg.next.get() {
                    seg = next.as_ref()
                } else {
                    let start = seg.start + seg.vals.len();
                    seg.next.get().write(Some(NonNull::from(AppendVecInner::new(start))));
                    seg = (*seg.next.get()).as_ref().unwrap().as_ref()
                }
            }
        }
        (seg, idx)
    }

    pub fn append(&self, val: T) -> usize {
        let ret = self.len.take();
        self.len.set(ret + 1);
        unsafe {
            let (inner, idx) = self.get_idx(ret);
            assert_eq!(idx, inner.cnt.get());
            inner.cnt.set(idx + 1);
            (*inner.vals[idx].get()).write(val);
        }
        ret
    }

    /// append `val` and get a reference to it
    pub fn append_ref(&self, val: T) -> &T {
        let len = self.len.take();
        self.len.set(len + 1);
        unsafe {
            let (inner, idx) = self.get_idx(len);
            assert_eq!(idx, inner.cnt.get());
            inner.cnt.set(idx + 1);
            (*inner.vals[idx].get()).write(val)
        }
    }

    pub fn get(&self, idx: usize) -> Option<&T> {
        if idx >= self.len.get() {
            return None;
        }
        unsafe {
            let (inner, idx) = self.get_idx(idx);
            Some((*inner.vals[idx].get()).assume_init_ref())
        }
    }

    pub fn iter(&self) -> AppendVecIter<T> {
        unsafe {
        AppendVecIter {
            ptr: self.inner.as_ref(),
            idx: 0,
        }
        }
    }

    pub fn iter_mut(&mut self) -> AppendVecIterMut<T> {
        unsafe {
            AppendVecIterMut {
                ptr: self.inner.as_mut(),
                idx: 0,
            }
        }
    }
}

impl<T> Drop for AppendVec<T> {
    fn drop(&mut self) {
        unsafe {
            let mut ptr = self.inner.as_ptr();
            while !ptr.is_null() {
                let mut bx = Box::from_raw(ptr);
                ptr = bx.next.get_mut().map_or(std::ptr::null_mut(), |p| p.as_ptr());
                drop(bx)
            }
        }
    }
}

impl<T> std::ops::Index<usize> for AppendVec<T>
{
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        self.get(index).expect("valid index")
    }
}

impl<T> std::ops::Index<Idx> for AppendVec<T>
{
    type Output = T;

    fn index(&self, index: Idx) -> &Self::Output {
        self.get(index.into()).expect("valid index")
    }
}

pub struct AppendVecIter<'a, T> {
    ptr: &'a AppendVecInner<T>,
    idx: usize,
}

impl<'a, T> Iterator for AppendVecIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        unsafe {
            if self.idx >= self.ptr.cnt.get() {
                let next_inner = (&*self.ptr.next.get()).as_ref()?;
                self.ptr = next_inner.as_ref();
                self.idx = 0;
            }
            self.idx += 1;
            Some((*self.ptr.vals[self.idx - 1].get()).assume_init_ref())
        }
    }
}

pub struct AppendVecIterMut<'a, T> {
    ptr: &'a mut AppendVecInner<T>,
    idx: usize,
}

impl<'a, T> Iterator for AppendVecIterMut<'a, T> {
    type Item = &'a mut T;

    fn next(&mut self) -> Option<Self::Item> {
        unsafe {
            if self.idx >= self.ptr.cnt.get() {
                let next_inner = (&mut *self.ptr.next.get()).as_mut()?;
                self.ptr = next_inner.as_mut();
                self.idx = 0;
            }
            self.idx += 1;
            Some((*self.ptr.vals[self.idx - 1].get()).assume_init_mut())
        }
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for AppendVec<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.iter()).finish()
    }
}
