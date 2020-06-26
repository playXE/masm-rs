#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]
#![allow(non_camel_case_types)]
pub mod asm_constant_pool;
pub mod assembler_buffer;
pub mod linkbuffer;
pub mod mips_assembler;
pub mod utils;
pub mod x86_assembler;
pub mod x86masm;
use assembler_buffer::AsmLabel;

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
#[repr(u8)]
pub enum CallFlags {
    None = 0x0,
    Linkable = 0x1,
    Near = 0x2,
    Tail = 0x4,
    LinkableNear = 0x3,
    LinkableNearTail = 0x7,
}

pub trait MacroAssemblerBase {
    fn link_call(code: *mut u8, call: Call, func: *const u8, flags: u8);
    fn link_pointer(code: *mut u8, label: assembler_buffer::AsmLabel, value: *mut u8);
    fn finalize(self) -> Vec<u8>;
}

pub struct Call {
    pub label: AsmLabel,
    pub flag: u8,
}

impl Call {
    pub fn new(jmp: AsmLabel, flags: u8) -> Self {
        Self {
            label: jmp,
            flag: flags,
        }
    }
    pub const INIT: Self = Self {
        flag: CallFlags::None as u8,
        label: AsmLabel::INIT,
    };

    pub fn is_flag_set(&self, f: CallFlags) -> bool {
        (self.flag & f as u8) != 0
    }

    pub fn from_tail_jump(jmp: AsmLabel) -> Self {
        Self::new(jmp, CallFlags::Linkable as _)
    }
}
