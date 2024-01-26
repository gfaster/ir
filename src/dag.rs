use std::{rc::Rc, collections::{BTreeMap, BTreeSet, VecDeque}, cell::RefCell};

use crate::{instr::{DebugInfo, OpInner, Instruction}, vec_map::{VecSet, VecMap}, reg::{Binding, BlockId}, edag::{Dag, DagIdx}};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct NodeId (usize);
impl NodeId {
    fn new() -> Self {
        use std::sync::atomic::{AtomicUsize, Ordering};
        static CNT: AtomicUsize = AtomicUsize::new(0);
        Self(CNT.fetch_add(1, Ordering::Relaxed))
    }
}


/// there's 2 types of dependencies: side-effect and def-use.
pub struct InstructionDag {
    block_id: BlockId,
    undefined: BTreeSet<Binding>,
    dag: Dag<Instruction>
}

impl InstructionDag {
    fn single_from_iter(instrs: &mut impl Iterator<Item = Instruction>) -> Result<Self, ()> {
        let root = instrs.next().ok_or(())?;
        if !root.is_block_header() {
            eprintln!("block didn't start with header, is: {root:#?}");
            return Err(())
        }
        let dag = Dag::new(root);
        let root = dag.get_root();
        let block_id = root.as_block_header_id().expect("is a header");
        let mut definitions = BTreeMap::new();
        for bind in root.defined_bindings() {
            let contained = definitions.insert(bind, root).is_some();
            if contained {
                eprintln!("duplicate def of {bind}");
                return Err(())
            }
        }
        let mut undefined = BTreeSet::new();
        let mut last_pred = root;
        let mut last_was_term = false;
        for next in instrs {
            let dref = dag.push(next);
            for def in next.defined_bindings() {
                let contained = definitions.insert(def, dref).is_some();
                if contained {
                    eprintln!("duplicate def of {def}");
                    return Err(())
                }
                if undefined.contains(&def) {
                    eprintln!("use before def of {def}");
                    return Err(())
                }
            }

            for (i, used) in next.read_bindings().enumerate() {
                if let Some(&definer) = definitions.get(&used) {
                    dref.set_parent_idx(i + 1, definer);
                } else {
                    undefined.insert(used);
                }
            }

            if next.is_term() || next.has_side_effects() {
                
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

    // fn into_instructions(self) -> impl Iterator<Item = Instruction> {
    //     let mut ret = Vec::new();
    //     let mut emitted: BTreeSet<Rc<DagNode>> = BTreeSet::new();
    //     let mut queue = VecDeque::new();
    //     let mut queue_contents: BTreeSet<Rc<DagNode>> = BTreeSet::new();
    //     queue.push_back(Rc::clone(&self.root));
    //     while let Some(node) = queue.pop_front() {
    //         // eprintln!("Looking at node: {:?}", node.id);
    //         if emitted.contains(&node) {
    //             continue
    //         }
    //         let mut rem_node = Some(Rc::clone(&node));
    //         for pred in node.immediate_predecessors() {
    //             // eprintln!("looking at pred {:?} for node {:?}", pred.id,  node.id);
    //             if !emitted.contains(&pred) {
    //                 if let Some(rem) = rem_node.take() {
    //                     // eprintln!("putting node {:?} back on the queue", node.id);
    //                     queue.push_front(rem)
    //                 }
    //                 if queue_contents.insert(Rc::clone(&pred)) {
    //                     // eprintln!("putting predecessor {:?} for node {:?} on the queue", pred.id, node.id);
    //                     queue.push_front(pred);
    //                 }
    //             }
    //         }
    //         let Some(node) = rem_node else {continue};
    //         queue.extend(node.immediate_successors());
    //         ret.push(node.instr.clone());
    //         // eprintln!("emitting node {:?}", node.id);
    //         emitted.insert(node);
    //     }
    //
    //     ret.into_iter()
    // }
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
