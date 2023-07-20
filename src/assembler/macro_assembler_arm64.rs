use std::ops::{Deref, DerefMut};

use crate::assembler::abstract_macro_assembler::AbsoluteAddress;

use super::abstract_macro_assembler::{
    AbstractMacroAssembler, Address, BaseIndex, Call, DataLabelCompact, Extend, Jump, JumpList,
    Operand, Scale,
};
use super::arm64assembler::*;
use super::buffer::AssemblerLabel;

pub struct MacroAssemblerARM64(AbstractMacroAssembler);

impl Deref for MacroAssemblerARM64 {
    type Target = AbstractMacroAssembler;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for MacroAssemblerARM64 {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl MacroAssemblerARM64 {
    pub const MASK_HALF_WORD_0: i64 =  0xffff;
    pub const MASK_HALF_WORD_1: i64 =  0xffff0000;
    pub const MASK_HALF_WORD_2: i64 =  0xffffffff00000000u64 as i64;

    pub const REPATCH_OFFSET_CALL_TO_POINTER: isize = -((NUMBER_OF_ADDRESS_ENCODING_INSTRUCTIONS as isize + 1) * 4);

    pub const NUM_GPRS: usize = 32;
    pub const NUM_FPRS: usize = 32;

    pub const NEAR_JUMP_RANGE: usize = 128 * 1024 * 1024;
    pub const DATA_TEMP_REGISTER: u8 = ip0;
    pub const MEMORY_TEMP_REGISTER: u8 = ip1;

    pub const FP_TEMP_REGISTER: u8 = q31;

    pub fn breakpoint(&mut self) {
        todo!()
    }

    pub unsafe fn link_call(code: *mut u8, call: Call, function: *const u8) {
        todo!()
    }

    pub fn link_jump_cond(
        &mut self,
        from: AssemblerLabel,
        to: AssemblerLabel,
        typ: JumpType,
        cond: Condition,
    ) {
        self.assembler.link_jump_cond(from, to, typ, cond)
    }

    pub fn link_jump_cmp(
        &mut self,
        from: AssemblerLabel,
        to: AssemblerLabel,
        typ: JumpType,
        cond: Condition,
        is_64bit: bool,
        compare_register: u8 
    ) {
        self.assembler.link_jump_cmp(from, to, typ, cond, is_64bit, compare_register)
    }

    pub fn link_jump_test_bit(
        &mut self,
        from: AssemblerLabel,
        to: AssemblerLabel,
        typ: JumpType,
        cond: Condition,
        bit_number: u8,
        compare_register: u8,
    ) {
        self.assembler.link_jump_test_bit(from, to, typ, cond, bit_number, compare_register)
    }


    pub unsafe fn link_pointer(code: *mut u8, label: AssemblerLabel, target: *const u8) {
        ARM64Assembler::link_pointer(code, label, target as _)
    }
}
