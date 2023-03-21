pub mod buffer;
pub mod code_location;
pub mod common;
pub mod abstract_macro_assembler;
pub mod link_buffer;
pub mod assembly_comments;
pub mod disassembler;
use cfg_if::*;


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


