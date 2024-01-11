macro_rules! decl_instr {
    ($inst:ident = $expr:expr) => {
        impl AsmProperties for $inst {
            fn props() -> &'static InstrProp {
                const PROPS: InstrProp = $expr;
                &PROPS
            }
        }
    };
}

struct InstrProp {
    is_barrier: bool,
    is_commutable: bool,
    mnemonic: &'static str,
    op_cnt: u8,
    writes_flags: bool,
    reads_flags: bool,
    has_side_effects: bool,
    may_load: bool,
    overwrites_src1: bool,
}

trait AsmProperties {
    fn props() -> &'static InstrProp;
}

trait Asm: AsmProperties {

}
