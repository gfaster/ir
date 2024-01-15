use std::collections::BTreeMap;

use crate::{reg::Virtual, OperationalValidity};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StackSlotState {
    Allocated,
    Freed,
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SlotOccupancy {
    Allocated,
    NeverUsed,
    Free,
}

impl SlotOccupancy {
    fn is_free(&self) -> bool {
        matches!(self, SlotOccupancy::Free | SlotOccupancy::NeverUsed)
    }
}

impl StackSlotState {
    fn merge(&lhs: &Self, &rhs: &Self) -> Self {
        match (lhs, rhs) {
            (StackSlotState::Allocated, StackSlotState::Allocated) => StackSlotState::Allocated,
            (StackSlotState::Freed, StackSlotState::Freed) => StackSlotState::Freed,
            _ => StackSlotState::Unknown
        }
    }

    #[must_use]
    fn is_freed(&self) -> bool {
        matches!(self, Self::Freed)
    }
}

pub struct StackSlot {
    state: StackSlotState,
    idx: u32,
}

impl StackSlot {
    fn size(&self) -> usize {
        8
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct StackSlotRef(usize);

impl StackSlotRef {
    fn new() -> Self {
        use std::sync::atomic::{AtomicUsize, Ordering};
        static CNT: AtomicUsize = AtomicUsize::new(0);
        Self(CNT.fetch_add(1, Ordering::Relaxed))
    }
}

/// stack state at a certain point, mutates during codegen. This should be created during register
/// allocation for a function but the exact slot positions should only be extracted after register
/// allocation is complete for that function.
pub struct StackState {
    frame_size: usize,

    /// slot indices indexed by [`StackSlotRef`]
    slot_map: BTreeMap<StackSlotRef, StackSlot>,

    slots: Vec<SlotOccupancy>,
}

impl StackState {
    pub fn new() -> Self {
        StackState { frame_size: 0, slots: Vec::new(), slot_map: BTreeMap::new() }
    }

    pub fn frame_size(&self) -> usize {
        self.frame_size * 8
    }

    /// marks a slot as allocated, expanding the stack frame if needed 
    fn mark_pos(&mut self, slot: usize) {
        if self.slots.len() <= slot {
            self.frame_size = slot + 1;
            self.slots.resize_with(slot + 1, || SlotOccupancy::NeverUsed);
        }
        self.slots[slot] = SlotOccupancy::Allocated;
    }

    /// finds a free slot and marks it as allocated, expanding the stack frame if needed
    fn find_free_idx(&mut self) -> usize {
        let pos = self.slots.iter().position(|s| s.is_free()).unwrap_or(self.slots.len());
        self.mark_pos(pos);
        pos
    }

    /// finds a free slot that has never been used and marks it as allocated, expanding the stack
    /// frame if needed
    fn find_never_used_idx(&mut self) -> usize {
        let pos = self.slots.iter().position(|&s| s == SlotOccupancy::NeverUsed).unwrap_or(self.slots.len());
        self.mark_pos(pos);
        pos
    }

    /// return true if slot has never been used
    fn is_never_used(&self, slot: usize) -> bool {
        self.slots.get(slot).map_or(true, |&occ| occ == SlotOccupancy::NeverUsed)
    }

    fn alloc(&mut self, size: usize) -> StackSlotRef {
        assert_eq!(size, 8, "single sized slots for now");

        let pos = self.find_free_idx();

        let new = StackSlot {
            state: StackSlotState::Allocated,
            idx: pos as u32,
        };
        let sref = StackSlotRef::new();
        self.slot_map.insert(sref, new);
        sref
    }

    fn free(&mut self, slot: StackSlotRef) -> OperationalValidity {
        let Some(data) = self.slot_map.get_mut(&slot) else {
            eprintln!("Warining: tried to free an unknown slot");
            return OperationalValidity::UndefinedBehavior;
        };
        if data.state.is_freed() {
            return OperationalValidity::UndefinedBehavior;
        }
        data.state = StackSlotState::Freed;
        self.slots[data.idx as usize] = SlotOccupancy::Free;
        OperationalValidity::Valid
    }

    /// merge the state of 2 [`StackState`], we assume that any behavior divergence is defined for
    /// now. We'll also shift the location of any conflicts retroactively
    fn merge(mut lhs: Self, rhs: Self) -> Self {
        for (key, rslot) in rhs.slot_map {
            if let Some(lslot) = lhs.slot_map.get_mut(&key) {
                assert_eq!(lslot.idx, rslot.idx);
                lslot.state = StackSlotState::merge(&lslot.state, &rslot.state);
            } else {
                if !lhs.is_never_used(rslot.idx as usize) && !rslot.state.is_freed() {
                    let idx = lhs.find_never_used_idx() as u32;
                    lhs.slot_map.insert(key, 
                        StackSlot { idx, ..rslot });
                }
            }
        }
        lhs
    }
}
