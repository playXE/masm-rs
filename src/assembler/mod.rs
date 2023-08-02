pub mod abstract_macro_assembler;
pub mod assembler_common;
pub mod assembly_comments;
pub mod buffer;
pub mod code_location;
pub mod common;
pub mod disassembler;
pub mod link_buffer;

use std::mem::size_of;

use cfg_if::*;

use self::abstract_macro_assembler::Address;

cfg_if! {
    if #[cfg(target_arch = "x86_64")] {
        #[macro_use]
        pub mod x86_64_registers;
        pub mod x86assembler;
        pub mod macro_assembler_x86_common;
        pub mod macro_assembler_x86_64;
        pub use macro_assembler_x86_common::{RelationalCondition, DoubleCondition, ResultCondition};
        pub type TargetAssembler = x86assembler::X86Assembler;
        pub type TargetMacroAssembler = macro_assembler_x86_common::MacroAssemblerX86Common;
    } else if #[cfg(target_arch="riscv64")] {
        #[macro_use]
        pub mod riscv64_registers;
        pub mod riscv64assembler;
        pub mod macro_assembler_riscv64;
        pub mod riscv64disassembler;
        pub use macro_assembler_riscv64::{RelationalCondition, ZeroCondition, DoubleCondition, ResultCondition};
        pub type TargetAssembler = riscv64assembler::RISCV64Assembler;
        pub type TargetMacroAssembler = macro_assembler_riscv64::MacroAssemblerRISCV64;
    } else if #[cfg(target_arch="aarch64")] {
        #[macro_use]
        pub mod arm64registers;
        pub mod arm64assembler;
        pub mod macro_assembler_arm64;
        pub use macro_assembler_arm64::{RelationalCondition, ZeroCondition, DoubleCondition, ResultCondition};
        pub type TargetAssembler = arm64assembler::ARM64Assembler;
        pub type TargetMacroAssembler = macro_assembler_arm64::MacroAssemblerARM64;
    } else {
        compile_error!("Unsupported architecture");
    }
}

#[cfg(any(target_arch = "x86_64", target_arch = "riscv64"))]
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
        self.breakpoint();
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

    pub fn push_to_save_gpr(&mut self, src: u8) {
        self.push(src);
    }

    pub fn push_to_save_fpr(&mut self, src: u8) {
        self.sub64(size_of::<f64>() as i32, Self::STACK_POINTER_REGISTER);
        self.store_double(src, Address::new(Self::STACK_POINTER_REGISTER, 0));
    }

    pub fn pop_to_restore_gpr(&mut self, dest: u8) {
        self.pop(dest);
    }

    pub fn pop_to_restore_fpr(&mut self, dest: u8) {
        self.load_double(Address::new(Self::STACK_POINTER_REGISTER, 0), dest);
        self.add64(size_of::<f64>() as i32, Self::STACK_POINTER_REGISTER);
    }
}
