pub mod buffer;
pub mod code_location;
pub mod common;
pub mod abstract_macro_assembler;
pub mod link_buffer;
pub mod assembly_comments;
pub mod disassembler;

use cfg_if::*;

use self::abstract_macro_assembler::Address;


cfg_if! {
    if #[cfg(target_arch = "x86_64")] {
        #[macro_use]
        pub mod x86_64_registers;
        pub mod x86assembler;
        pub mod macro_assembler_x86_common;
        pub mod macro_assembler_x86_64;

        pub type TargetAssembler = x86assembler::X86Assembler;
        pub type TargetMacroAssembler = macro_assembler_x86_common::MacroAssemblerX86Common;
    } else {
        compile_error!("Unsupported architecture");
    }
}


impl TargetMacroAssembler {
    pub fn ret32(&mut self, _: u8) {
        self.ret();
    }

    pub fn ret64(&mut self, _: u8) {
        self.ret();
    }

    pub fn ret_float(&mut self, _: u8) {
        self.ret();
    }

    pub fn ret_double(&mut self, _: u8) {
        self.ret();
    }

    pub fn ret_void(&mut self) {
        self.ret();
    }

    pub fn oops(&mut self) {
        self.assembler.int3();
    }

    pub fn move_double_rrr(&mut self, src: Address, dest: Address, scratch: u8) {
        self.load_double(src, scratch);
        self.store_double(scratch, dest);
    }

    pub fn move_float_rrr(&mut self, src: Address, dest: Address, scratch: u8) {
        self.load_float(src, scratch);
        self.store_float(scratch, dest);
    }

    pub fn move32_rrr(&mut self, src: Address, dest: Address, scratch: u8) {
        self.load32(src, scratch);
        self.store32(scratch, dest);
    }

    pub fn move_rrr(&mut self, src: Address, dest: Address, scratch: u8) {
        self.load64(src, scratch);
        self.store64(scratch, dest);
    }

    pub fn lea32(&mut self, address: Address, dest: u8) {
        self.add32_rrr(address.offset, address.base, dest);
    }

    pub fn lea64(&mut self, address: Address, dest: u8) {
        self.add64_rrr(address.offset, address.base, dest);
    }
}
