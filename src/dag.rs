use std::{rc::Rc, collections::{BTreeMap, BTreeSet, VecDeque}, cell::RefCell};

use crate::{instr::{DebugInfo, OpInner, Instruction}, vec_map::{VecSet, VecMap}, reg::{Binding, BlockId}};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct NodeId (usize);
impl NodeId {
    fn new() -> Self {
        use std::sync::atomic::{AtomicUsize, Ordering};
        static CNT: AtomicUsize = AtomicUsize::new(0);
        Self(CNT.fetch_add(1, Ordering::Relaxed))
    }
}

struct NodeSet (RefCell<VecSet<Rc<DagNode>>>);
impl NodeSet {
    fn new() -> Self {
        NodeSet(RefCell::new(VecSet::new()))
    }

    fn insert(&self, node: Rc<DagNode>) {
        self.0.borrow_mut().insert(node);
    }

    fn contains(&self, node: &DagNode) -> bool {
        self.0.borrow().contains(node)
    }

    /// returns true if every element in `self` is in `other`
    fn contained_by(&self, other: &Self) -> bool {
        let this = self.0.borrow();
        let other = other.0.borrow();
        if this.len() > other.len() {
            return false;
        }
        for node in this.iter() {
            if !other.contains(node) {
                return false;
            }
        }
        true
    }

    /// TODO: this will probably be horribly slow
    fn contains_in_lineage<F>(&self, node: &DagNode, f: &F) -> bool 
    where for<'a> F: Fn(&'a DagNode) -> &'a Self
    {
        if self.contains(node) {
            return true
        };
        self.0.borrow().iter().any(|n| f(n).contains_in_lineage(node, &f))
    }

    fn iter(&self) -> impl Iterator<Item = Rc<DagNode>> {
        let v: Vec<_> = self.0.borrow().iter().map(|n| Rc::clone(n)).collect();
        v.into_iter()
    }
}

pub struct DagNode {
    id: NodeId,
    instr: Instruction,

    /// instructions that cause side effects that make it so this node can't be ordered before.
    ///
    /// Note that this currently is only non-empty if this instruction has side-effects or is a
    /// block header/terminator
    pred: NodeSet,

    /// instructions that define bindings this node depends on
    parents: NodeSet,

    /// instructions that depend on bindings this node defines
    children: NodeSet,

    /// instructions that cause side effects that make it so this node can't be ordered after.
    ///
    /// Note that this currently is only non-empty if this instruction has side-effects or is a
    /// block header/terminator
    succ: NodeSet,
}

impl DagNode {
    fn from_instr(instr: Instruction) -> Rc<Self> {
        Rc::new(DagNode {
            pred: NodeSet::new(),
            parents: NodeSet::new(),
            id: NodeId::new(),
            instr,
            children: NodeSet::new(),
            succ: NodeSet::new(),
        })
    }

    fn immediate_predecessors(&self) -> impl Iterator<Item = Rc<DagNode>> {
        self.pred.iter().chain(self.parents.iter())
    }
}

impl PartialOrd for DagNode {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.id.partial_cmp(&other.id)
    }
}

impl Ord for DagNode {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

impl PartialEq for DagNode {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
impl Eq for DagNode {}

/// there's 2 types of dependencies: side-effect and def-use.
pub struct InstructionDag {
    block_id: BlockId,
    definitions: BTreeMap<Binding, Rc<DagNode>>,
    undefined: BTreeSet<Binding>,
    root: Rc<DagNode>,
}

impl InstructionDag {
    fn single_from_iter(instrs: &mut impl Iterator<Item = Instruction>) -> Result<Self, ()> {
        let root = instrs.next().ok_or(())?;
        if !root.is_block_header() {
            eprintln!("block didn't start with header");
            return Err(())
        }
        let block_id = root.as_block_header_id().expect("is a header");
        let root = DagNode::from_instr(root);
        let mut definitions = BTreeMap::new();
        for bind in root.instr.defined_bindings() {
            let contained = definitions.insert(bind, Rc::clone(&root)).is_some();
            if contained {
                eprintln!("duplicate def of {bind}");
                return Err(())
            }
        }
        let mut undefined = BTreeSet::new();
        let mut last_pred = Rc::clone(&root);
        let mut last_was_term = false;
        for next in instrs {
            let next = DagNode::from_instr(next);
            for def in next.instr.defined_bindings() {
                let contained = definitions.insert(def, Rc::clone(&next)).is_some();
                if contained {
                    eprintln!("duplicate def of {def}");
                    return Err(())
                }
                if undefined.contains(&def) {
                    eprintln!("use before def of {def}");
                    return Err(())
                }
            }

            for used in next.instr.read_bindings() {
                if let Some(definer) = definitions.get(&used) {
                    definer.children.insert(Rc::clone(&next));
                    next.parents.insert(Rc::clone(&next));
                } else {

                }
            }

            if next.instr.is_term() || next.instr.has_side_effects() {
                last_pred.succ.insert(Rc::clone(&next));
                next.pred.insert(last_pred);
                last_pred = Rc::clone(&next);
            }

            if next.instr.is_term() {
                last_was_term = true;
                break
            }
        }
        assert!(last_was_term);

        Ok(InstructionDag { definitions, undefined, root, block_id })
    }

    fn from_iter(instrs: impl IntoIterator<Item = Instruction>) -> Result<Vec<Self>, ()> {
        let mut it = instrs.into_iter().peekable();
        let mut blocks: VecMap<BlockId, InstructionDag> = VecMap::new();
        while it.peek().is_some() {
            let next = Self::single_from_iter(&mut it)?;
            let repl = blocks.insert(next.block_id, next);
            if repl.is_some() {
                eprintln!("error: duplicate block id");
                return Err(())
            }
        }
        // let ids: Vec<_> = blocks.keys().copied().collect();
        // for id in ids {
        //     let seq_ids: Vec<_> = blocks[&id].tail.jump_dsts().map(|x| x.as_label()
        //         .expect("Non-label jump targets are not supported")).collect();
        //     let mut seq_idxs = Vec::new();
        //     let idx = blocks.get_index(&id).unwrap();
        //     for seq in &seq_ids {
        //         blocks[seq].pred.insert(idx);
        //         let Some(seq) = blocks.get_index(seq) else {
        //             panic!("block {seq} was defined in a different function");
        //         };
        //         seq_idxs.push(seq);
        //     }
        //     blocks[&id].seq.extend(seq_idxs.iter().copied());
        // }

        Ok(blocks.into_value_vec())
    }

    fn into_instructions(self) -> impl Iterator<Item = Instruction> {
        let mut ret = Vec::new();
        let mut emitted: BTreeSet<Rc<DagNode>> = BTreeSet::new();
        let mut queue = VecDeque::new();
        queue.push_back(Rc::clone(&self.root));
        while let Some(node) = queue.pop_front() {
            if emitted.contains(&node) {
                continue
            }
            let mut rem_node = Some(Rc::clone(&node));
            for pred in node.immediate_predecessors() {
                if !emitted.contains(&pred) {
                    if let Some(rem) = rem_node.take() {
                        queue.push_front(rem)
                    }
                    queue.push_front(pred);
                }
            }
            let Some(node) = rem_node else {continue};
            ret.push(node.instr.clone());
            emitted.insert(node);
        }

        ret.into_iter()
    }
}

pub struct FunctionDag {
    name: Box<str>,
    blocks: Vec<InstructionDag>,
}

impl FunctionDag {
    pub fn from_iter(name: impl Into<Box<str>> + ?Sized, instrs: impl IntoIterator<Item = Instruction>) -> Result<Self, ()> {
        Ok(Self {
            name: name.into(),
            blocks: InstructionDag::from_iter(instrs)?
        })
    }

    pub fn into_instr_vec(self) -> Vec<Instruction> {
        let mut ret = Vec::new();
        for block in self.blocks {
            ret.extend(block.into_instructions())
        }
        ret
    }
}
