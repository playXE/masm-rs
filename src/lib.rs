#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]
#![allow(non_camel_case_types)]
pub mod air;
pub mod arvm7_assembler;
pub mod asm_constant_pool;
pub mod assembler_buffer;
pub mod linkbuffer;
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
    fn link_jump(
        code: *mut u8,
        jump: assembler_buffer::AsmLabel,
        label: assembler_buffer::AsmLabel,
    );
    fn link_jump_ptr(code: *mut u8, jump: AsmLabel, to: *const u8);
    fn get_linker_addr(code: *mut u8, label: AsmLabel) -> *mut u8;
    fn finalize(self) -> Vec<u8>;
}

#[derive(Default)]
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

pub mod machine_masm {
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    pub use super::x86_assembler::*;
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    pub use super::x86masm::*;
}
use std::sync::Mutex;
lazy_static::lazy_static! {
    pub(crate) static ref MEM_ALLOC: Mutex<linkbuffer::Memory> =
        Mutex::new(linkbuffer::Memory::new());
}

pub fn set_memory_rwx() {
    MEM_ALLOC.lock().unwrap().set_rwx();
}
