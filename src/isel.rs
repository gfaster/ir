use crate::value::{Function, Block};

pub fn select(func: &Function) {
    for block in func.block_iter() {

    }
}

fn select_block(block: Block) {
    for val in block.val_iter() {
        let val_bind = val.borrow();
        let Some(instr) = val_bind.as_instr()  else { continue; };

    }
}
