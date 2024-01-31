//! DAG made for holding instructions: can be safely modified with a shared reference

use std::{
    borrow::BorrowMut,
    cell::{Cell, RefCell, UnsafeCell},
    collections::VecDeque,
    mem::{ManuallyDrop, MaybeUninit},
    num::NonZeroUsize,
    ptr,
    sync::atomic::AtomicPtr,
};

use crate::{
    appendvec::{AppendVec, Idx},
    vec_map::VecSet,
};

/// append only connected DAG that is modifiable through shared references.
///
/// There is a lot of room for algorithmic optimization: I expect this to get somewhat slow on
/// larger graphs due to traversal time of [`AppendVec`]. That will be fixed by either twiddling
/// with virtual memory or by replacing the index in [`DagRef`] with a pointer, or both.
pub struct Dag<T> {
    // nodes: AppendVec<Option<T>>,
    nodes: AppendVec<DagNode<T>>,
    root: Cell<Idx>,
}

struct DagNode<T> {
    item: UnsafeCell<ManuallyDrop<T>>,
    parents: [Cell<Option<Idx>>; 3],
    children: RefCell<VecSet<Idx>>,
}

pub struct DagRef<'a, T> {
    dag: &'a Dag<T>,
    idx: Idx,
}
impl<T> Clone for DagRef<'_, T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T> Copy for DagRef<'_, T> {}

impl<T> std::ops::Deref for DagRef<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        // Safety:
        // will only ever have exclusive reference from `IntoIter`, which cannot lend out
        // references
        unsafe { &*self.dag.nodes[self.idx.idx()].item.get() }
    }
}

impl<T> DagNode<T> {
    fn just_add_parent(&self, parent: Idx) {
        self.parents
            .iter()
            .find(|i| i.get().is_none())
            .expect("only two parents are allowed")
            .set(Some(parent));
    }
    fn just_add_child(&self, child: Idx) {
        self.children.borrow_mut().insert(child);
    }
}

impl<'a, T> DagRef<'a, T> {
    fn node(&self) -> &DagNode<T> {
        &self.dag.nodes[self.idx.idx()]
    }

    fn mk_idx(self, idx: Idx) -> Self {
        Self { dag: self.dag, idx }
    }

    fn to_idx(self) -> DagIdx {
        DagIdx(self.idx)
    }

    pub fn children(self) -> impl Iterator<Item = Self> {
        let ret = self.node().children.borrow().clone();
        ret.into_iter().map(move |idx| self.mk_idx(idx))
    }

    pub fn parents(self) -> impl Iterator<Item = Self> {
        let ret = self.node().parents.clone();
        ret.into_iter()
            .filter_map(|o| o.take())
            .map((move |idx| self.mk_idx(idx)))
    }

    pub fn add_child(self, child: Self) -> Self {
        self.node().just_add_child(child.idx);
        child.node().just_add_parent(self.idx);
        self
    }

    pub fn add_parent(self, parent: Self) -> Self {
        self.node().just_add_parent(parent.idx);
        parent.node().just_add_child(self.idx);
        self
    }

    pub fn set_parent_idx(self, idx: usize, parent: Self) -> Option<Self> {
        let node = self.node();
        let ret = node.parents[idx].get();
        if let Some(prev) = ret {
            self.mk_idx(prev).node().children.borrow_mut().remove(&prev);
        }
        let parent_node = parent.node();
        parent_node.children.borrow_mut().insert(self.idx);
        node.parents[idx].set(Some(parent.idx));

        ret.map(|i| self.mk_idx(i))
    }

    pub fn detach_parents(self) {
        let node = self.node();
        for parent in &node.parents {
            if let Some(parent_child) = parent.take() {
                self.dag.nodes[parent_child]
                    .children
                    .borrow_mut()
                    .remove(&self.idx);
            }
        }
    }

    pub fn get_parent_idx(self, idx: usize) -> Option<Self> {
        let idx = self.node().parents.get(idx)?.get()?;
        Some(self.mk_idx(idx))
    }

    pub fn detach_children(self) {
        let node = self.node();
        for child in std::mem::take(&mut *node.children.borrow_mut()) {
            self.dag.nodes[child]
                .parents
                .iter()
                .find(|o| o.get() == Some(self.idx))
                .map(|p| p.take());
        }
    }

    pub fn detach(self) {
        let node = self.node();
        for parent in &node.parents {
            if let Some(parent_child) = parent.take() {
                self.dag.nodes[parent_child]
                    .children
                    .borrow_mut()
                    .remove(&self.idx);
            }
        }
        for child in std::mem::take(&mut *node.children.borrow_mut()) {
            self.dag.nodes[child]
                .parents
                .iter()
                .find(|o| o.get() == Some(self.idx))
                .map(|p| p.take());
        }
    }

    pub fn replace(self, item: Self) {
        let node = self.node();
        let item_node = item.node();
        for parent in &node.parents {
            if let Some(parent_child) = parent.take() {
                let mut v = self.dag.nodes[parent_child].children.borrow_mut();
                v.remove(&self.idx);
                v.insert(item.idx);
                item_node.just_add_parent(parent_child)
            }
        }
        for child in std::mem::take(&mut *node.children.borrow_mut()) {
            self.dag.nodes[child]
                .parents
                .iter()
                .find(|o| o.get() == Some(self.idx))
                .map(|p| p.set(Some(item.idx)));
            item_node.just_add_child(child)
        }
    }
}

impl<T> Dag<T> {
    pub fn new(root: T) -> Self {
        let nodes = AppendVec::new();
        let item = ManuallyDrop::new(root).into();
        let idx = nodes.append(DagNode {
            item,
            parents: std::array::from_fn(|_| None.into()),
            children: RefCell::new(VecSet::new()),
        });
        let root = Idx::new(idx).into();
        Self { nodes, root }
    }

    pub fn get_root(&self) -> DagRef<T> {
        DagRef {
            dag: self,
            idx: self.root.get(),
        }
    }

    /// push an item onto the DAG. Note that if the node is not connected to the rest of the DAG,
    /// then it will not be emitted when made into a topological sort
    pub fn push(&self, item: T) -> DagRef<T> {
        let item = ManuallyDrop::new(item).into();
        let idx = self.nodes.append(DagNode {
            item,
            parents: std::array::from_fn(|_| None.into()),
            children: RefCell::new(VecSet::new()),
        });
        let idx: Idx = Idx::new(idx);
        DagRef { dag: self, idx }
    }

    pub fn into_topological_iter(self) -> DagIntoIter<T> {
        let sent = vec![false; self.nodes.len()];
        let queue = VecDeque::from([self.root.get()]);
        DagIntoIter {
            origin: self,
            sent,
            queue,
        }
    }

    /// returns an iterator over the elements in a topological order
    pub fn iter(&self) -> DagIter<T> {
        let sent = vec![false; self.nodes.len()];
        let queue = VecDeque::from([self.root.get()]);
        DagIter {
            origin: self,
            sent,
            queue,
        }
    }

    fn get_ref(&self, idx: Idx) -> DagRef<T> {
        DagRef { dag: self, idx }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct DagIdx(Idx);
impl DagIdx {
    fn to_ref<T>(self, dag: &Dag<T>) -> DagRef<T> {
        DagRef { dag, idx: self.0 }
    }
}

impl<T> IntoIterator for Dag<T> {
    type Item = T;

    type IntoIter = DagIntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.into_topological_iter()
    }
}

impl<T> Drop for Dag<T> {
    fn drop(&mut self) {
        // Safety:
        // we only mess with this in IntoIter
        unsafe {
            for node in self.nodes.iter_mut() {
                ManuallyDrop::drop(&mut node.item.get_mut())
            }
        }
    }
}

pub struct DagIntoIter<T> {
    origin: Dag<T>,
    sent: Vec<bool>,
    queue: VecDeque<Idx>,
}

impl<T> Iterator for DagIntoIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        unsafe {
            let idx = loop {
                let first = self.queue.pop_front()?;
                if self.sent[first] {
                    continue;
                };
                let mut next = Some(first);
                for parent in self
                    .origin
                    .get_ref(first)
                    .parents()
                    .filter(|p| !self.sent[p.idx])
                {
                    if let Some(next) = next.take() {
                        self.queue.push_front(next);
                    }
                    self.queue.push_front(parent.idx);
                }
                if let Some(next) = next {
                    break next;
                }
            };
            for child in self.origin.get_ref(idx).children() {
                if !self.sent[child.idx] {
                    self.queue.push_back(child.idx)
                }
            }
            self.sent[idx.idx()] = true;
            Some(ManuallyDrop::take(
                &mut *self.origin.nodes[idx.idx()].item.get(),
            ))
        }
    }
}

impl<T> Drop for DagIntoIter<T> {
    fn drop(&mut self) {
        // Safety:
        // we only take the items we sent
        unsafe {
            for (idx, item) in self
                .sent
                .iter()
                .zip(self.origin.nodes.iter())
                .filter(|(sent, _)| !*sent)
            {
                ManuallyDrop::drop(&mut *item.item.get())
            }
        }
    }
}

pub struct DagIter<'a, T> {
    origin: &'a Dag<T>,
    sent: Vec<bool>,
    queue: VecDeque<Idx>,
}

impl<'a, T> Iterator for DagIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        let idx = loop {
            let first = self.queue.pop_front()?;
            if self.sent[first] {
                continue;
            };
            let mut next = Some(first);
            for parent in self
                .origin
                .get_ref(first)
                .parents()
                .filter(|p| !self.sent[p.idx])
            {
                if let Some(next) = next.take() {
                    self.queue.push_front(next);
                }
                self.queue.push_front(parent.idx);
            }
            if let Some(next) = next {
                break next;
            }
        };
        for child in self.origin.get_ref(idx).children() {
            if !self.sent[child.idx] {
                self.queue.push_back(child.idx)
            }
        }
        self.sent[idx.idx()] = true;
        // Safety:
        // item is never mutably accessed until origin is dropped
        unsafe { Some(&*self.origin.nodes[idx.idx()].item.get()) }
    }
}
