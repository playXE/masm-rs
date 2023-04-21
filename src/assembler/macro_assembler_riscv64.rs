use std::ops::{Deref, DerefMut};

use super::{abstract_macro_assembler::{AbstractMacroAssembler, Call, Jump}, buffer::AssemblerLabel};

pub struct MacroAssemblerRISCV64 {
    pub base: AbstractMacroAssembler
}

impl Deref for MacroAssemblerRISCV64 {
    type Target = AbstractMacroAssembler;

    fn deref(&self) -> &Self::Target {
        &self.base
    }
}

impl DerefMut for MacroAssemblerRISCV64 {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.base
    }
}

impl MacroAssemblerRISCV64 {
    pub fn breakpoint(&mut self) {

    }

    pub unsafe fn link_call(_: *mut u8, _: Call, _: *const u8) {}
    pub unsafe fn link_jump(_: *mut u8, _: Jump, _: *const u8) {}
    pub unsafe fn link_pointer(_: *mut u8, _: AssemblerLabel, _: *const u8) {}
}