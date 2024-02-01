use std::mem::{align_of, size_of};

pub struct TPtr<T>(*const T);

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct PtrTag(usize);

const PTR_BITS: usize = {
    if cfg!(target_arch = "aarch64") {
        // https://www.kernel.org/doc/html/v5.8/arm64/memory.html
        52
    } else if cfg!(target_arch = "x86_64") {
        // Some very high end processors use 57 bits, need to call assert_valid_cpu
        48
    } else {
        usize::BITS as usize
    }
};

pub fn assert_valid_cpu() {
    use std::arch::asm;

    if PTR_BITS == usize::BITS as usize {
        panic!("I don't know how big the virtual address space is!")
    }
    if cfg!(target_arch = "x86_64") {
        // https://en.wikipedia.org/wiki/CPUID#EAX=80000008h:_Virtual_and_Physical_address_Sizes
        let info: u32;
        unsafe {
            // cpuid clobbers rbx, which is used internally by llvm, so we have to save it
            // specifically
            asm! {
                "mov eax, 0x80000008",
                "mov {tmp}, rbx",
                "cpuid",
                "mov rbx, {tmp}",
                out("eax") info,
                out("ecx") _,
                out("edx") _,
                tmp = out(reg) _,
            }
        }
        let len = (info >> 8) & ((1 << 8) - 1);
        assert_eq!(len as usize, PTR_BITS);
    }
}

impl<T> TPtr<T> {
    const LOW_BITS: usize = align_of::<T>().trailing_zeros() as usize;
    const HIGH_BITS: usize = usize::BITS as usize - PTR_BITS;
    const BITS: usize = Self::LOW_BITS + Self::HIGH_BITS;
    const MASK_LOW: usize = align_of::<T>() - 1;
    const MASK_HIGH: usize = usize::MAX << PTR_BITS;
    const MASK: usize = Self::MASK_LOW | Self::MASK_HIGH;

    /// create a new tagged ptr. Note that ptr should be valid and aligned, or it may be clobbered
    ///
    /// Safety: Since the purpose of a tagged pointer is to allow safe dereferencing of raw
    /// pointers, creating one should uphold that it points to a valid allocation.
    pub unsafe fn new(tag: PtrTag, ptr: *const T) -> Self {
        debug_assert_eq!(ptr.align_offset(align_of::<T>()), 0);
        debug_assert!(tag.0.leading_zeros() as usize >= usize::BITS as usize - Self::BITS);
        debug_assert!(tag.0.leading_zeros() >= Self::MASK.count_ones());

        if cfg!(target_feature = "bmi2") && cfg!(target_arch = "x86_64") && !cfg!(miri) {
            unsafe { Self::new_fast(tag, ptr) }
        } else {
            Self::new_fallback(tag, ptr)
        }
    }

    #[cfg(target_arch = "x86_64")]
    #[target_feature(enable = "bmi2")]
    unsafe fn new_fast(tag: PtrTag, ptr: *const T) -> Self {
        use core::arch::x86_64::_pdep_u64;

        let PtrTag(tag) = tag;
        let addr = ptr as usize;
        let masked = !Self::MASK & addr;
        let tag = _pdep_u64(tag as u64, Self::MASK as u64) as usize;
        let res = masked | tag;
        Self(res as *const T)
    }

    unsafe fn new_fallback(tag: PtrTag, ptr: *const T) -> Self {
        let PtrTag(tag) = tag;
        let masked = !Self::MASK & ptr as usize;
        let low = tag & ((1 << Self::LOW_BITS) - 1);
        let high = tag & (((1 << Self::HIGH_BITS) - 1) << Self::LOW_BITS);
        let res = masked | low | (high << (PTR_BITS - Self::LOW_BITS));
        Self(res as *const T)
    }

    pub fn tag(self) -> PtrTag {
        if cfg!(target_feature = "bmi2") && cfg!(target_arch = "x86_64") && !cfg!(miri) {
            unsafe { self.tag_fast() }
        } else {
            self.tag_fallback()
        }
    }

    /// This should compile to a single `pext` instruction
    #[cfg(target_arch = "x86_64")]
    #[target_feature(enable = "bmi2")]
    unsafe fn tag_fast(self) -> PtrTag {
        use core::arch::x86_64::_pext_u64;

        let addr = self.0 as usize;
        let tag = _pext_u64(addr as u64, Self::MASK as u64);
        PtrTag(tag as usize)
    }

    fn tag_fallback(self) -> PtrTag {
        let addr = self.0 as usize;
        let high = addr >> PTR_BITS;
        let low = addr & ((1 << Self::LOW_BITS) - 1);
        let tag = (high << Self::LOW_BITS) | low;
        PtrTag(tag)
    }

    pub fn ptr(self) -> *const T {
        let Self(ptr) = self;
        let addr = ptr as usize;
        let addr = addr & !Self::MASK;
        let sign = addr & (1 << (PTR_BITS - 1)) != 0;
        let addr = addr | Self::MASK_HIGH * sign as usize;
        let ptr = addr as *const T;
        debug_assert_eq!(ptr.align_offset(align_of::<T>()), 0);
        ptr
    }

    pub fn destructure(self) -> (PtrTag, *const T) {
        (self.tag(), self.ptr())
    }
}

impl<T> std::hash::Hash for TPtr<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::hash(self.0, state)
    }
}

impl<T> Clone for TPtr<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for TPtr<T> {}

impl<T> std::fmt::Debug for TPtr<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Tptr")
            .field("tag", &self.tag().0)
            .field("ptr", &self.ptr())
            .finish()
    }
}

impl<T> PartialEq for TPtr<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> Eq for TPtr<T> {}

static TAG_SEED: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(123946789);

impl PtrTag {
    pub fn new_any() -> Self {
        PtrTag::new::<()>()
    }

    pub fn new<T>() -> Self {
        use std::sync::atomic::*;
        let update = || {
            TAG_SEED
                .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |seed| {
                    // https://stackoverflow.com/a/3062783/7487237
                    let a: usize = 1103515245;
                    let c = 12345;
                    let m = 1 << 31;
                    Some(a.wrapping_mul(seed).wrapping_add(c) % m)
                })
                .expect("always valid")
        };
        let num = update().reverse_bits();
        let num = num % (1 << TPtr::<T>::BITS);
        PtrTag(num)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::ptr;

    fn make_tag<T>(base: usize) -> PtrTag {
        let tag = base % (1 << TPtr::<T>::BITS);
        PtrTag(tag)
    }

    macro_rules! tag_case {
        ($ty:ty, $val:expr, $ptr:expr) => {
            let ptr: *const _ = $ptr;
            let tag = make_tag::<$ty>($val);
            let tptr: TPtr<$ty> = unsafe { TPtr::new(tag, ptr) };
            // eprintln!("----------");
            // dbg!(tptr);
            // dbg!(tag);
            // dbg!(ptr);
            assert_eq!(tptr.tag(), tag);
            assert_eq!(tptr.ptr(), ptr);
        };
    }

    #[repr(align(1024))]
    struct BigAlign;

    #[test]
    fn assert_ptr_size() {
        assert_valid_cpu()
    }

    #[test]
    fn tag_basic() {
        tag_case!((), usize::MAX, ptr::null());
        tag_case!(usize, usize::MAX, ptr::null());
        tag_case!(usize, 1234194128, ptr::null());
        tag_case!(usize, 1234194128, std::ptr::NonNull::dangling().as_ptr());
        tag_case!(BigAlign, 1234194128, std::ptr::NonNull::dangling().as_ptr());
    }

    #[test]
    fn tag_fast_vs_fallback() {
        let tag = make_tag::<usize>(usize::MAX);
        let ptr: TPtr<usize> = unsafe { TPtr::new(tag, std::ptr::null()) };
        let fast = unsafe { ptr.tag_fast() };
        let fallback = ptr.tag_fallback();
        assert_eq!(fast, tag);
        assert_eq!(fallback, tag);
    }

    #[test]
    fn new_fast_vs_fallback() {
        let tag = make_tag::<usize>(usize::MAX);
        let fallback: TPtr<usize> = unsafe { TPtr::new_fallback(tag, std::ptr::null()) };
        let fast: TPtr<usize> = unsafe { TPtr::new_fast(tag, ptr::null()) };
        assert_eq!(fast, fallback);
        assert_eq!(fast.tag(), tag);
    }

    #[test]
    fn count_high() {
        let count = 1 << (usize::BITS as usize - PTR_BITS);
        for _ in 0..=(count * 2) {
            let tag = PtrTag::new_any();
            let ptr: *const _ = &5;
            let tptr = unsafe { TPtr::new(tag, ptr) };
            assert_eq!(tptr.ptr(), ptr);
            assert_eq!(tptr.tag(), tag);
        }
    }
}
