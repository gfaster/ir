use std::collections::HashMap;

use crate::instr::*;


const IR_INSTRUCTIONS: [&'static InstrProp; 7] = [
    &ADD,
    &MUL,
    &LT,
    &GT,
    &EQ,
    &JMP,
    &BR,
];

pub fn instruction_map() -> &'static HashMap<&'static str, &'static InstrProp> {
    static mut MAP: Option<HashMap<&'static str, &'static InstrProp>> = None;
    // SAFETY: only one thread for now
    unsafe {
        if let Some(m) = &MAP {
            return m;
        }
        let mut map = HashMap::new();
        map.extend(IR_INSTRUCTIONS.into_iter().map(|i| (i.mnemonic, i)));
        assert_eq!(map.len(), IR_INSTRUCTIONS.len(), "there are duplicate mnemonics in basic IR");
        MAP = Some(map);
        MAP.as_ref().unwrap()
    }
}

macro_rules! new_op {
    ($vis:vis $name:ident < $template:path = {$($tt:tt)*}) => {
        $vis const $name: InstrProp = InstrProp {
            $($tt)*,
            ..$template
        };
        const _: () = $name.assert_makes_sense();
    };
}

macro_rules! new_template {
    ($name:ident < $template:path = {$($tt:tt)*}) => {
        const $name: InstrProp = InstrProp {
            $($tt)*,
            ..$template
        };
    };
}

const BASE_OP: InstrProp = InstrProp {
    op_cnt: 0,
    mnemonic: "[TEMPLATE]",
    is_branch: false,
    is_commutative: false,
    is_terminator: false,
    set_regs: &[],
    read_regs: &[],
    set_operands: &[],
    read_operands: &[],
    operand_relative_type_constraints: &[],
};

new_template!{BIN_OP < BASE_OP = {
    op_cnt: 3,
    set_operands: &[0],
    read_operands: &[1, 2],
    operand_relative_type_constraints: &[0, 0, 0]
}}

new_template!{ARITH_OP < BIN_OP = {
    is_commutative: true
}}

new_op!{pub ADD < ARITH_OP = {mnemonic: "add"}}
new_op!{pub MUL < ARITH_OP = {mnemonic: "mul"}}

new_template!{CMP_OP < BIN_OP = {
    operand_relative_type_constraints: &[0, 1, 1]
}}

new_op!{pub LT < CMP_OP = {mnemonic: "lt"}}
new_op!{pub GT < CMP_OP = {mnemonic: "gt"}}
new_op!{pub EQ < CMP_OP = {mnemonic: "gt", is_commutative: true}}

new_template!{BR_OP < BASE_OP = {
    is_branch: true,
    is_terminator: true,
    op_cnt: 3,
    read_operands: &[0, 1, 2],
    operand_relative_type_constraints: &[0, 1, 1]
}}
new_op!{pub BR < BR_OP = {mnemonic: "br"}}

new_template!{JMP_OP < BR_OP = {
    op_cnt: 1,
    read_operands: &[0],
    operand_relative_type_constraints: &[0]
}}
new_op!{pub JMP < JMP_OP = {mnemonic: "br"}}
