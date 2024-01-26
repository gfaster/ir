use std::{cell::Cell, ptr::{NonNull, addr_of}, mem::MaybeUninit, iter::FusedIterator, fmt::Debug};

use crate::appendvec::AppendVec;


type ListPtr<T> = Option<NonNull<Node<T>>>;

pub struct List<T> {
    data: AppendVec<Node<T>>,
}

pub struct Node<T> {
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

    fn next(&self) -> Option<&Self> {
        unsafe {
            let node = &*self.next.get()?.as_ptr();
            if node.is_sentinel() {
                None
            } else {
                Some(node)
            }
        }
    }

    fn prev(&self) -> Option<&Self> {
        unsafe {
            let node = &*self.prev.get()?.as_ptr();
            if node.is_sentinel() {
                None
            } else {
                Some(node)
            }
        }
    }

    /// detaches a node from the list, returns the prev and next nodes
    fn detach(&self) -> (*const Node<T>, *const Node<T>) {
        assert!(!self.is_sentinel(), "sentinel nodes cannot be detached from the list");
        unsafe {
            let prev = self.prev.take().expect("node should not be a sentinel");
            let next = self.next.take().expect("node should not be a sentinel");
            (*prev.as_ptr()).next.set(Some(next));
            (*next.as_ptr()).prev.set(Some(prev));
            (prev.as_ptr(), next.as_ptr())
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

    unsafe fn insert_after_node(&self, node: &Node<T>, item: T) -> &Node<T> {
        unsafe {
            let next = node.next.get().expect("has next node").as_ref();
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
    }

    pub fn push_back(&self, item: T) -> Ref<T> {
        unsafe {
            let back = &*self.tail_sentinel().prev.get().expect("tail sentinel's prev ptr should be Some").as_ptr();
            let node = self.insert_after_node(back, item);
            Ref { list: self, node }
        }
    }

    pub fn push_front(&self, item: T) -> Ref<T> {
        unsafe {
            let front = &*self.head_sentinel();
            let node = self.insert_after_node(front, item);
            Ref { list: self, node }
        }
    }

    pub fn head(&self) -> Option<Ref<T>> {
        unsafe {
            let node = &*self.head_sentinel().next.get().expect("head sentinel's head ptr should be Some").as_ptr();
            if node.is_sentinel() {
                None
            } else {
                Some(Ref { node, list: self })
            }
        }
    }

    pub fn iter(&self) -> NodeIter<T> {
        NodeIter {
            list: self,
            node: self.head_sentinel(),
        }
    }

    pub fn tail(&self) -> Option<Ref<T>> {
        unsafe {
            let node = &*self.tail_sentinel().prev.get().expect("tail sentinel's tail ptr should be Some").as_ptr();
            if node.is_sentinel() {
                None
            } else {
                Some(Ref { node , list: self})
            }
        }
    }
}

impl<T> Ref<'_, T> {
    /// detaches the node from the list. It can be added back later still.
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
        let ret = self.insert_after(item);
        self.detach();
        ret
    }

    pub fn insert_before(&self, item: T) -> Self {
        unsafe {
            let prev = self.node.prev.get().expect("has prev node").as_ref();
            let node = self.list.insert_after_node(prev, item);
            Self { list: self.list, node }
        }
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
            self.node.item.assume_init_ref()
        }
    }
}

pub struct NodeIter<'a, T> {
    list: &'a List<T>,
    node: &'a Node<T>
}

impl<T> NodeIter<'_, T> {
    // note we can't (shouldn't) convert NodeIter to a ListRef, because it may do deref coersion
    // and panic. I want to always be able to create a NodeIter, even on an empty list
}

impl<'a, T> Iterator for NodeIter<'a, T> {
    type Item = Ref<'a, T>;

    fn next(&mut self) -> Option<Self::Item> {
        let node = self.node.next()?;
        self.node = node;
        Some(Ref { node, list: self.list })
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
        let Some(next) = it.next() else { panic!() };
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

}
