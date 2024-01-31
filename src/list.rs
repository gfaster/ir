use std::{cell::Cell, ptr::{NonNull, addr_of}, mem::MaybeUninit, iter::FusedIterator, fmt::Debug, default};

use crate::{appendvec::AppendVec, tagged_ptr::TPtr};


type ListPtr<T> = Option<NonNull<Node<T>>>;

pub struct List<T> {
    data: AppendVec<Node<T>>,
}

// https://internals.rust-lang.org/t/get-the-offset-of-a-field-from-the-base-of-a-struct/14163/6
fn item_offset<T>() -> usize {
    let dummy = MaybeUninit::<Node<T>>::uninit();
    let base_ptr = dummy.as_ptr();
    let member_ptr = unsafe{ core::ptr::addr_of!((*base_ptr).item) };
    member_ptr as usize - base_ptr as usize
}

/// Linked List Node
///
/// head and tail sentinels have their next or prev ptr respectively pointing to the start of the
/// struct. This lets iterators spin on the tail node. Detached nodes have their prev ptr set to
/// None and the next ptr set to a forward node. 
struct Node<T> {
    item: MaybeUninit<T>,
    next: Cell<ListPtr<T>>,
    prev: Cell<ListPtr<T>>,
}

impl<T> Node<T> {
    fn is_sentinel(&self) -> bool {
        // eprintln!("Checking if sentinel:\n\t\
        //     next: {:?}\n\tprev: {:?}", self.next.get(), self.prev.get());
        // self.next.get().is_none() || self.prev.get().is_none()
        self.next.get() == Some(NonNull::from(self)) || 
        self.prev.get() == Some(NonNull::from(self))
    }

    fn is_detached(&self) -> bool {
        debug_assert!(self.next.get().is_some());
        self.prev.get().is_none()
    }

    /// get the next forwarded node if `self` is detached
    fn get_forwarding(&self) -> &Self {
        let mut node = self;
        while node.is_detached() {
            unsafe {
                node = node.next.get().expect("next ptr should never be None").as_ref();
            }
        }
        node
    }

    /// get the next value, even if it's a sentinel.
    fn next_any(&self) -> &Self {
        let node = self.get_forwarding();
        unsafe {
            &*node.next.get().expect("next ptr should never be None").as_ptr()
        }
    }

    fn next(&self) -> Option<&Self> {
        let node = self.next_any();
        if node.is_sentinel() {
            None
        } else {
            Some(node)
        }
    }

    /// get the next value, even if it's a sentinel
    fn prev_any(&self) -> &Self {
        let node = self.get_forwarding();
        unsafe {
            node.prev.get().expect("non-detached node should have prev ptr").as_ref()
        }
    }

    fn prev(&self) -> Option<&Self> {
        let node = self.prev_any();
        if node.is_sentinel() {
            None
        } else {
            Some(node)
        }
    }

    /// detaches a node from the list, sets the `next` ptr to point to the predecessor
    fn detach(&self) {
        assert!(!self.is_sentinel(), "sentinel nodes cannot be detached from the list");
        unsafe {
            let Some(prev) = self.prev.take() else {
                // if prev is None, then this node has been detached
                return
            };
            let next = self.next.get().expect("next should never be None");
            (*prev.as_ptr()).next.set(Some(next));
            (*next.as_ptr()).prev.set(Some(prev));
            self.next.set(Some(prev));
        }
    }
}

pub struct Ref<'a, T> {
    list: &'a List<T>,
    node: &'a Node<T>
}

impl<T> Clone for Ref<'_, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Ref<'_, T> {}

impl<T> List<T> {
    pub fn new() -> Self {
        let data = AppendVec::new();
        let head = data.append_ref(Node {
            item: MaybeUninit::uninit(),
            next: None.into(),
            prev: None.into(),
        });
        let tail = data.append_ref(Node {
            item: MaybeUninit::uninit(),
            next: None.into(),
            prev: None.into(),
        });
        head.prev.set(Some(NonNull::from(head)));
        head.next.set(Some(NonNull::from(tail)));
        tail.prev.set(Some(NonNull::from(head)));
        tail.next.set(Some(NonNull::from(tail)));
        Self { data }
    }

    fn head_sentinel(&self) -> &Node<T> {
        &self.data[0]
    }

    fn tail_sentinel(&self) -> &Node<T> {
        &self.data[1]
    }

    /// insert an item after the given node
    ///
    /// Safety: node must be in the same list
    unsafe fn insert_after_node(&self, node: &Node<T>, item: T) -> &Node<T> {
        let node = node.get_forwarding();
        let next = node.next_any();
        let new = self.data.append_ref(
            Node { 
                item: MaybeUninit::new(item), 
                prev: Some(node.into()).into(),
                next: Some(next.into()).into(),
            }
        );
        node.next.set(Some(new.into()));
        next.prev.set(Some(new.into()));
        new
    }

    pub fn push_back(&self, item: T) -> Ref<T> {
        unsafe {
            let back = self.tail_sentinel().prev_any();
            let node = self.insert_after_node(back, item);
            Ref { list: self, node }
        }
    }

    pub fn push_front(&self, item: T) -> Ref<T> {
        unsafe {
            let front = self.head_sentinel();
            let node = self.insert_after_node(front, item);
            Ref { list: self, node }
        }
    }

    pub fn head(&self) -> Option<Ref<T>> {
        let node = self.head_sentinel().next()?;
        Some(Ref { list: self, node })
    }

    pub fn iter(&self) -> NodeIter<T> {
        NodeIter {
            list: self,
            node: self.head_sentinel(),
        }
    }

    pub fn tail(&self) -> Option<Ref<T>> {
        let node = self.tail_sentinel().prev()?;
        Some(Ref { list: self, node })
    }
}

impl<T> Default for List<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a, T> Ref<'a, T> {
    /// detaches the node from the list. It cannot be added back later. The item will only be
    /// dropped when the whole list is dropped. After detaching, inserting before or after will
    /// cause a panic.
    pub fn detach(&self) {
        self.node.detach();
    }

    pub fn insert_after(&self, item: T) -> Self {
        unsafe {
            let node = self.list.insert_after_node(self.node, item);
            Self { list: self.list, node }
        }
    }

    /// detaches self and adds a new node, returning a reference to it
    pub fn replace(&self, item: T) -> Self {
        let ret = self.insert_before(item);
        self.detach();
        ret
    }

    pub fn insert_before(&self, item: T) -> Self {
        unsafe {
            let prev = self.node.prev_any();
            let node = self.list.insert_after_node(prev, item);
            Self { list: self.list, node }
        }
    }

    pub fn make_iter(self) -> NodeIter<'a, T> {
        NodeIter { list: self.list, node: self.node }
    }
}

impl<T: Debug> Debug for Ref<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (**self).fmt(f)
    }
}

impl<T> std::ops::Deref for Ref<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        if self.node.is_sentinel() {
            panic!("attempted to deref a sentinel node - this is a bug in list.rs")
        };
        unsafe {
            self.node.get_forwarding().item.assume_init_ref()
        }
    }
}

pub struct NodeIter<'a, T> {
    list: &'a List<T>,
    node: &'a Node<T>,
}

impl<T> NodeIter<'_, T> {
    // note we can't (shouldn't) convert NodeIter to a ListRef, because it may do deref coersion
    // and panic. I want to always be able to create a NodeIter, even on an empty list
}

impl<'a, T> Iterator for NodeIter<'a, T> {
    type Item = Ref<'a, T>;

    fn next(&mut self) -> Option<Self::Item> {
        self.node = self.node.get_forwarding();
        let node = self.node.next()?;
        self.node = node;
        Some(Ref { list: self.list, node })
    }
}

impl<'a, T> IntoIterator for &'a List<T> {
    type Item = Ref<'a, T>;

    type IntoIter = NodeIter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<A> FromIterator<A> for List<A> {
    fn from_iter<T: IntoIterator<Item = A>>(iter: T) -> Self {
        let list = List::new();
        for item in iter {
            list.push_back(item);
        }
        list
    }
}

impl<T, I> From<I> for List<T> 
    where I: IntoIterator<Item = T> 
{
    fn from(value: I) -> Self {
        value.into_iter().collect()
    }
}

impl<T: Debug> Debug for List<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.iter()).finish()
    }
}

impl<T> Drop for List<T> {
    fn drop(&mut self) {
        unsafe {
            // skip 2 because that's the head and tail nodes
            for item in self.data.iter_mut().skip(2) {
                item.item.assume_init_drop()
            }
        }
    }
}

// Not necessarily fused
// impl<T> FusedIterator for NodeIter<'_, T> {}

/// pointer to a list node. Unsafe to promote.
pub struct ThinRef<T>(TPtr<Node<T>>);

impl<T> Clone for ThinRef<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for ThinRef<T> {}

impl<T> Debug for ThinRef<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl<T> From<Ref<'_, T>> for ThinRef<T> {
    fn from(value: Ref<'_, T>) -> Self {
        unsafe { ThinRef(value.list.data.ref_to_ptr(value.node)) }
    }
}

impl<T> PartialEq for ThinRef<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> Eq for ThinRef<T> {}

impl<T> PartialOrd for ThinRef<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        (self.0.ptr() as usize).partial_cmp(&(other.0.ptr() as usize))
    }
}

impl<T> Ord for ThinRef<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (self.0.ptr() as usize).cmp(&(other.0.ptr() as usize))
    }
}

impl<T> ThinRef<T> {
    /// Promote to a ListRef. 
    ///
    /// Safety:
    /// - `self` must have been created from a list ref
    /// - `list` must be the same list this was created from
    pub unsafe fn promote_unchecked(self, list: &List<T>) -> Ref<T> {
        Ref {
            list,
            node: &*self.0.ptr(),
        }
    }

    pub fn try_promote(self, list: &List<T>) -> Option<Ref<T>> {
        let node = list.data.try_ptr_deref(self.0)?;
        Some(Ref { list, node })
    }

    pub fn promote(self, list: &List<T>) -> Ref<T> {
        let Some(lref) = self.try_promote(list) else {
            panic!("ThinRef does not belong to list");
        };
        lref
    }

    /// Create a `ThinRef` from a reference to a item in a list.
    /// 
    /// ### Safety:
    /// - `item` is a reference to an item added to a [`List`]
    /// - violating this constraint may cause instant UB
    #[deprecated]
    pub unsafe fn from_element_ref(item: &T) -> Self {
        // assert!(std::mem::size_of_val(item) < isize::MAX as usize);
        // let ptr = item as *const T;
        // let node = ptr.byte_sub(item_offset::<T>());
        unimplemented!()
    }

    #[deprecated]
    pub fn addr(self) -> usize {
        self.0.ptr().wrapping_byte_add(item_offset::<T>()) as usize
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn smoke() {
        let l = List::from([1, 2, 3]);
        let mut it = l.iter();
        assert_eq!(it.next().as_deref(), Some(&1));
        assert_eq!(it.next().as_deref(), Some(&2));
        assert_eq!(it.next().as_deref(), Some(&3));
        assert_eq!(it.next().as_deref(), None);
    }

    #[test]
    fn insert_in_iter() {
        let l = List::from([1, 2, 3]);
        let mut it = l.iter();
        assert_eq!(it.next().as_deref(), Some(&1));

        let next = it.next().unwrap();
        assert_eq!(*next, 2);
        next.insert_after(4);

        assert_eq!(it.next().as_deref(), Some(&4));
        assert_eq!(it.next().as_deref(), Some(&3));
        assert_eq!(it.next().as_deref(), None);
    }

    #[test]
    fn push_front() {
        let l = List::from([1, 2, 3]);
        let mut it = l.iter();
        l.push_front(0);
        assert_eq!(it.next().as_deref(), Some(&0));
        l.push_front(-1);
        assert_eq!(it.next().as_deref(), Some(&1));
        assert_eq!(it.next().as_deref(), Some(&2));
        assert_eq!(it.next().as_deref(), Some(&3));
        assert_eq!(it.next().as_deref(), None);
    }

    #[test]
    fn boxes() {
        let _ = List::from([Box::new(1), Box::new(2), Box::new(3)]);
    }

    #[test]
    fn head_tail() {
        let l = List::new();
        assert_eq!(l.head().as_deref(), None);
        assert_eq!(l.tail().as_deref(), None);
        l.push_back(1);
        assert_eq!(l.head().as_deref(), Some(&1));
        assert_eq!(l.tail().as_deref(), Some(&1));
        l.push_back(2);
        assert_eq!(l.head().as_deref(), Some(&1));
        assert_eq!(l.tail().as_deref(), Some(&2));
        l.push_front(3);
        assert_eq!(l.head().as_deref(), Some(&3));
        assert_eq!(l.tail().as_deref(), Some(&2));
    }

    #[test]
    fn detach() {
        let l = List::from([1, 2, 3]);
        assert_eq!(l.iter().map(|i| *i).collect::<Vec<_>>(), vec![1, 2, 3]);
        let middle = l.iter().skip(1).next().unwrap();
        middle.detach();
        assert_eq!(l.iter().map(|i| *i).collect::<Vec<_>>(), vec![1, 3]);
        l.head().unwrap().detach();
        assert_eq!(l.iter().map(|i| *i).collect::<Vec<_>>(), vec![3]);
        l.head().unwrap().detach();
        assert_eq!(l.iter().map(|i| *i).collect::<Vec<_>>(), vec![]);
    }

    #[test]
    fn detach_iter() {
        let l = List::from([1, 2, 3, 4]);
        let mut it = l.iter();

        let next = it.next().unwrap();
        assert_eq!(*next, 1);
        next.detach();

        let next = it.next().unwrap();
        assert_eq!(*next, 2);
        next.detach();

        let next = it.next().unwrap();
        assert_eq!(*next, 3);
        next.detach();

        let next = it.next().unwrap();
        assert_eq!(*next, 4);
        next.detach();

        assert_eq!(l.iter().map(|i| *i).collect::<Vec<_>>(), vec![]);
    }

    #[test]
    fn detach_and_insert() {
        let l = List::from([1, 2, 3, 4]);
        let mut it = l.iter();

        assert_eq!(it.next().as_deref(), Some(&1));

        let next = it.next().unwrap();
        assert_eq!(*next, 2);
        next.insert_before(-2);
        next.insert_before(-1);
        next.insert_after(5);
        next.insert_after(6);
        next.detach();
        // dbg!(unsafe { it.node.item.assume_init_ref() });
        // dbg!(unsafe { it.node.next_any().item.assume_init_ref() });
        // dbg!(&l);

        assert_eq!(it.next().as_deref(), Some(&6));
        assert_eq!(it.next().as_deref(), Some(&5));
        assert_eq!(it.next().as_deref(), Some(&3));
        assert_eq!(it.next().as_deref(), Some(&4));
        assert_eq!(it.next().as_deref(), None);
    }

    #[test]
    fn replacing() {
        let l = List::from([1, 2, 3]);
        let mut it = l.iter();
        assert_eq!(it.next().as_deref(), Some(&1));

        let next = it.next().unwrap();
        assert_eq!(*next, 2);
        next.replace(4);
        assert_eq!(*next, 4);

        assert_eq!(it.next().as_deref(), Some(&3));
    }

    #[test]
    fn replace_and_insert() {
        let l = List::from([1, 2, 3]);
        let mut it = l.iter();
        assert_eq!(it.next().as_deref(), Some(&1));

        let next = it.next().unwrap();
        assert_eq!(*next, 2);
        next.replace(4);
        assert_eq!(*next, 4);
        next.insert_before(5);
        next.insert_after(6);

        assert_eq!(it.next().as_deref(), Some(&6));
        assert_eq!(it.next().as_deref(), Some(&3));
    }
}
