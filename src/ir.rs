use std::{collections::HashMap, sync::OnceLock};

use crate::instr::*;


const IR_INSTRUCTIONS: [&'static BasicInstrProp; 5] = [
    &ADD,
    &MUL,
    &LT,
    &GT,
    &EQ,
];

pub fn instruction_map() -> &'static HashMap<&'static str, &'static BasicInstrProp> {
    static MAP: OnceLock<HashMap<&'static str, &'static BasicInstrProp>> = OnceLock::new();
    MAP.get_or_init(|| {
        let mut map = HashMap::with_capacity(IR_INSTRUCTIONS.len());
        for i in IR_INSTRUCTIONS {
            let res = map.insert(i.mnemonic, i);
            assert!(res.is_none(), "mnemonic {} was defined twice", i.mnemonic);
        }
        map
    })
}

macro_rules! new_op {
    ($vis:vis $name:ident < $template:path = {$($tt:tt)*}) => {
        $vis const $name: BasicInstrProp = BasicInstrProp {
            $($tt)*,
            ..$template
        };
        const _: () = $name.assert_makes_sense();
    };
}

macro_rules! new_template {
    ($name:ident < $template:path = {$($tt:tt)*}) => {
        const $name: BasicInstrProp = BasicInstrProp {
            $($tt)*,
            ..$template
        };
    };
}

const BASE_OP: BasicInstrProp = BasicInstrProp {
    op_cnt: 2,
    res_cnt: 1,
    mnemonic: "[TEMPLATE]",
    is_branch: false,
    is_commutative: false,
    is_terminator: false,
    is_block_header: false,
    has_side_effects: true,
    may_read_memory: true,
    may_write_memory: true,
    is_barrier: false,
    operand_relative_type_constraints: &[0, 0, 0],
    simulation: None
};

new_template!{SIMPLE_OP < BASE_OP = {
    has_side_effects: false,
    may_read_memory: false,
    may_write_memory: false
}}

new_op!{pub ADD < SIMPLE_OP = {mnemonic: "add", is_commutative: true, simulation: Some(|l, r| Some(l+r))}}
new_op!{pub MUL < SIMPLE_OP = {mnemonic: "mul", is_commutative: true, simulation: Some(|l, r| Some(l*r))}}
new_op!{pub SHR < SIMPLE_OP = {mnemonic: "shr", is_commutative: false, simulation: Some(|l, r| Some(l>>r))}}
new_op!{pub SAR < SIMPLE_OP = {mnemonic: "sar", is_commutative: false}}
new_op!{pub SHL < SIMPLE_OP = {mnemonic: "shl", is_commutative: false, simulation: Some(|l, r| Some(l<<r))}}
new_op!{pub AND < SIMPLE_OP = {mnemonic: "and", is_commutative: true, simulation: Some(|l, r| Some(l&r))}}
new_op!{pub OR  < SIMPLE_OP = {mnemonic: "or",  is_commutative: true, simulation: Some(|l, r| Some(l|r))}}
new_op!{pub XOR < SIMPLE_OP = {mnemonic: "xor", is_commutative: true, simulation: Some(|l, r| Some(l^r))}}

new_template!{CMP_OP < SIMPLE_OP = {
    operand_relative_type_constraints: &[0, 1, 1]
}}

new_op!{pub LT < CMP_OP = {mnemonic: "lt"}}
new_op!{pub GT < CMP_OP = {mnemonic: "gt"}}
new_op!{pub EQ < CMP_OP = {mnemonic: "eq", is_commutative: true}}
