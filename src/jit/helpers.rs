#![allow(unused_imports)]
use crate::assembler::{abstract_macro_assembler::Operand, TargetMacroAssembler};

pub trait AssemblyHelpers {
    fn emit_function_prologue(&mut self);
    fn emit_function_epilogue_with_empty_frame(&mut self);
    fn emit_function_epilogue(&mut self);

    fn preserve_return_address_after_call(&mut self, reg: u8);
    fn restore_return_address_before_return(&mut self, op: impl Into<Operand>);
}

#[cfg(target_arch = "riscv64")]
use crate::assembler::riscv64assembler::*;
#[cfg(target_arch = "x86_64")]
use crate::assembler::x86assembler::*;

#[cfg(target_arch = "riscv64")]
impl AssemblyHelpers for TargetMacroAssembler {
    fn emit_function_prologue(&mut self) {
        self.push_pair(Self::FRAME_POINTER_REGISTER, Self::LINK_REGISTER);
        self.mov(Self::STACK_POINTER_REGISTER, Self::FRAME_POINTER_REGISTER);
    }

    fn emit_function_epilogue(&mut self) {
        self.mov(Self::FRAME_POINTER_REGISTER, Self::STACK_POINTER_REGISTER);
        self.emit_function_epilogue_with_empty_frame();
    }

    fn emit_function_epilogue_with_empty_frame(&mut self) {
        self.pop_pair(Self::FRAME_POINTER_REGISTER, Self::LINK_REGISTER);
    }

    fn restore_return_address_before_return(&mut self, op: impl Into<Operand>) {
        match op.into() {
            Operand::Register(reg) => {
                self.mov(reg, Self::LINK_REGISTER);
            }

            Operand::Address(addr) => {
                self.load64(addr, Self::LINK_REGISTER);
            }

            _ => unreachable!(),
        }
    }

    fn preserve_return_address_after_call(&mut self, reg: u8) {
        self.mov(Self::LINK_REGISTER, reg);
    }
}

#[cfg(target_arch = "x86_64")]
impl AssemblyHelpers for TargetMacroAssembler {
    fn emit_function_prologue(&mut self) {
        self.push(Self::FRAME_POINTER_REGISTER);
        self.mov(Self::STACK_POINTER_REGISTER, Self::FRAME_POINTER_REGISTER);
    }

    fn emit_function_epilogue_with_empty_frame(&mut self) {
        self.pop(Self::FRAME_POINTER_REGISTER);
    }

    fn emit_function_epilogue(&mut self) {
        self.mov(Self::FRAME_POINTER_REGISTER, Self::STACK_POINTER_REGISTER);
        self.pop(Self::FRAME_POINTER_REGISTER);
    }

    fn restore_return_address_before_return(&mut self, op: impl Into<Operand>) {
        self.push(op);
    }

    fn preserve_return_address_after_call(&mut self, reg: u8) {
        self.pop(reg);
    }
}
