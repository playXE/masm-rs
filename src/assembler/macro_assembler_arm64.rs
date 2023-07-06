use std::ops::{Deref, DerefMut};

use crate::assembler::abstract_macro_assembler::AbsoluteAddress;

use super::abstract_macro_assembler::{
    AbstractMacroAssembler, Address, BaseIndex, Call, DataLabelCompact, Extend, Jump, JumpList,
    Operand, Scale,
};
use super::arm64assembler;
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
    pub fn breakpoint(&mut self) {
        todo!()
    }

    pub unsafe fn link_call(code: *mut u8, call: Call, function: *const u8) {
        todo!()
    }

    pub unsafe fn link_jump(code: *mut u8, jump: Jump, target: *const u8) {
        todo!()
    }

    pub unsafe fn link_pointer(code: *mut u8, label: AssemblerLabel, target: *const u8) {
        todo!()
    }
}
