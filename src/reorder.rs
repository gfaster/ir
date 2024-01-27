use std::ops::Range;

use crate::Op;


fn find_last_occurrences(block: &[Op]) -> Vec<usize> {
    let mut last_occurrence = Vec::new();
    for (i, op) in block.iter().enumerate() {
        for var in op.vars_referenced() {
            if var >= last_occurrence.len() {
                last_occurrence.resize(var + 1, usize::MAX);
            }
            last_occurrence[var] = i;
        }
    }
    last_occurrence
}

fn find_first_occurrences(block: &[Op]) -> Vec<usize> {
    let mut first_occurrence = Vec::new();
    for (i, op) in block.iter().enumerate() {
        for var in op.vars_referenced() {
            if var >= first_occurrence.len() {
                first_occurrence.resize(var + 1, usize::MAX);
            }
            if first_occurrence[var] == usize::MAX {
                first_occurrence[var] = i;
            }
        }
    }
    first_occurrence
}

fn find_lifetimes(block: &[Op]) -> Vec<Range<usize>> {
    let vfirst = find_first_occurrences(block);
    let vlast = find_last_occurrences(block);
    let mut ret = Vec::with_capacity(vfirst.len());
    for (first, last) in vfirst.into_iter().zip(vlast) {
        ret.push(first..last)
    }
    ret
}

/// to be called in a single block
pub(crate) fn minimize_lifetimes(ops: &mut [Op]) {
}
