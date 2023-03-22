use std::{collections::hash_map::Entry, ptr::null_mut};

use crate::{
    assembler::abstract_macro_assembler::Call,
    wtf::{
        executable_memory_handle::{ExecutableMemoryHandle, CodeRef},
        virtual_memory::{PlatformVirtualMemory, VirtualMemory},
    },
};

use super::{
    abstract_macro_assembler::{AbstractMacroAssembler, Label, Location, PatchableJump, Jump, JumpList, DataLabelPtr},
    assembly_comments::{AssemblyCommentsRegistry, CommentMap},
    buffer::AssemblerLabel,
    TargetMacroAssembler, disassembler::try_to_disassemble,
};
use std::sync::Arc;

/// LinkBuffer:
///
/// This class assists in linking code generated by the macro assembler, once code generation
/// has been completed, and the code has been copied to is final location in memory.  At this
/// time pointers to labels within the code may be resolved, and relative offsets to external
/// addresses may be fixed.
///
/// Specifically:
///   * Jump objects may be linked to external targets,
///   * The address of Jump objects may taken, such that it can later be relinked.
///   * The return address of a Call may be acquired.
///   * The address of a Label pointing into the code may be resolved.
///   * The value referenced by a DataLabel may be set.
///
pub struct LinkBuffer {
    executable_memory: Option<Arc<ExecutableMemoryHandle>>,
    size: usize,
    is_already_disassembled: bool,
    is_thunk: bool,
    code: *mut u8,
    link_tasks: Vec<Box<dyn FnOnce(&mut Self)>>,
    late_link_tasks: Vec<Box<dyn FnOnce(&mut Self)>>,
    did_allocate: bool,
}

impl LinkBuffer {

    pub fn from_macro_assembler(macro_assembler: &mut TargetMacroAssembler) -> Self {
        let mut this = Self {
            executable_memory: None,
            size: 0,
            is_already_disassembled: false,
            is_thunk: false,
            code: null_mut(),
            link_tasks: vec![],
            late_link_tasks: vec![],
            did_allocate: false 
        };

        this.link_code(macro_assembler);

        this 
    }

    pub fn from_code(macro_assembler: &mut TargetMacroAssembler, code: *mut u8, size: usize) -> Self {
        let mut this = Self {
            executable_memory: None,
            size,
            is_already_disassembled: false,
            is_thunk: false,
            code,
            link_tasks: vec![],
            late_link_tasks: vec![],
            did_allocate: false 
        };

        this.link_code(macro_assembler);

        this 
    }

    pub fn location_of_near_call(&self, call: Call) -> (*mut u8, bool) {
        assert!(call.is_flag_set(Call::LINKABLE));
        assert!(call.is_flag_set(Call::NEAR));

        (
            self.get_linker_address(call.label),
            call.is_flag_set(Call::TAIL),
        )
    }

    pub fn location_of(&self, location: impl Into<Location>) -> *mut u8 {
        match location.into() {
            Location::Call(call) => {
                assert!(call.is_flag_set(Call::LINKABLE));
                assert!(!call.is_flag_set(Call::NEAR));

                self.get_linker_address(call.label)
            }
            Location::Label(label) => self.get_linker_address(label.label),
            Location::ConvertibleLoadLabel(label) => self.get_linker_address(label.label),
            Location::DataLabel32(label) => self.get_linker_address(label.label),
            Location::DataLabelPtr(label) => self.get_linker_address(label.label),
            Location::DataLabelCompact(label) => self.get_linker_address(label.label),
            Location::PatchableJump(label) => self.get_linker_address(label.label),
            _ => unreachable!("Use location_of_near_call for NearCall"),
        }
    }

    /// This method obtains the return address of the call, given as an offset from
    /// the start of the code.
    pub fn return_address_offset(&self, call: Call) -> usize {
        AbstractMacroAssembler::get_linker_call_return_offset(call)
    }

    pub fn offset_of(&mut self, label: Label) -> u32 {
        label.label.offset()
    }

    pub fn offset_of_patachable_jump(&mut self, jump: PatchableJump) -> u32 {
        jump.0.label.offset()
    }

    pub fn was_already_disassembled(&self) -> bool {
        self.is_already_disassembled
    }

    pub fn did_already_disassemble(&mut self) {
        self.is_already_disassembled = true;
    }

    pub fn set_is_thunk(&mut self) {
        self.is_thunk = true;
    }

    pub fn did_fail_to_allocate(&self) -> bool {
        !self.did_allocate
    }

    pub fn is_valid(&self) -> bool {
        !self.did_fail_to_allocate()
    }

    pub fn link_call(&mut self, call: Call, function: *const u8) {
        unsafe {
            TargetMacroAssembler::link_call(self.code(), call, function);
        }
    }

    pub fn link_jump(&mut self, jump: Jump, target: *const u8) {
        unsafe {
            TargetMacroAssembler::link_jump(self.code(), jump, target);
        }
    }

    pub fn link_jumps(&mut self, jump: &JumpList, target: *const u8) {
        unsafe {
            for jump in jump.jumps() {
                TargetMacroAssembler::link_jump(self.code(), *jump, target);
            }
        }
    }

    pub fn patch(&mut self, label: DataLabelPtr, value: *const u8) {
        unsafe {
            TargetMacroAssembler::link_pointer(self.code(), label.label, value);
        }
    }

    pub fn entrypoint(&self) -> *const u8 {
        self.code()
    }

    pub fn link_code(&mut self, macro_assembler: &mut TargetMacroAssembler) {
        macro_assembler.pad_before_patch();

        self.allocate(macro_assembler);
        if !self.did_allocate {
            return;
        }

        let buffer = macro_assembler.assembler.buffer().data();

        unsafe {
            std::ptr::copy_nonoverlapping(buffer.as_ptr(), self.code, buffer.len());
        }

        self.link_tasks = std::mem::take(&mut macro_assembler.link_tasks);
        self.late_link_tasks = std::mem::take(&mut macro_assembler.late_link_tasks);

        self.link_comments(macro_assembler);
    }

    pub fn finalize_without_disassembly(&mut self) -> CodeRef {
        self.finalize_code_without_disassembly_impl()
    }

    pub fn finalize_with_disassembly(&mut self, dump_disassembly: bool, format: &str, out: &mut impl std::fmt::Write) -> Result<CodeRef, std::fmt::Error> {
        self.finalize_code_with_disassembly_impl(dump_disassembly, format, out)
    }

    fn finalize_code_without_disassembly_impl(&mut self) -> CodeRef {
        self.perform_finalization();

        if let Some(executable_memory) = self.executable_memory.take() {
            CodeRef::External(executable_memory)
        } else {
            CodeRef::SelfManaged((self.code, self.size))
        }
    } 
    fn finalize_code_with_disassembly_impl<W: std::fmt::Write>(&mut self, dump_disassembly: bool, format: &str, out: &mut W) -> Result<CodeRef, std::fmt::Error> {
        let result = self.finalize_code_without_disassembly_impl();
        let just_dumping_header = !dump_disassembly || self.is_already_disassembled;

        write!(out, "Generated JIT code for {}:\n", format)?;

        let executable_address = result.start();
        let executable_end = result.end();

        write!(out, "    Code at [{:p}, {:p}){}\n", executable_address, executable_end, if just_dumping_header {
            "."
        } else {
            ":"
        })?;

        if just_dumping_header {
            return Ok(result);
        }

        try_to_disassemble(result.start(), result.end() as usize - result.start() as usize, "    ", out)?;

        Ok(result)
    }


    fn allocate(&mut self, macro_assembler: &mut TargetMacroAssembler) {
        let mut initial_size = macro_assembler.code_size();

        if !self.code.is_null() {
            if initial_size > self.size {
                return;
            }

            let nops_to_fill_in_bytes = self.size - initial_size;
            macro_assembler.emit_nops(nops_to_fill_in_bytes);
            self.did_allocate = true;
            return;
        }

        while initial_size % 32 != 0 {
            macro_assembler.breakpoint();
            initial_size = macro_assembler.code_size();
        }

        let memory = VirtualMemory::<PlatformVirtualMemory>::allocate_aligned(
            crate::wtf::round_up_usize(
                initial_size,
                VirtualMemory::<PlatformVirtualMemory>::page_size(),
                0,
            ),
            VirtualMemory::<PlatformVirtualMemory>::page_size(),
            true,
            "code",
        )
        .unwrap();

        self.size = memory.size();
        self.did_allocate = true;
        self.code = memory.start() as _;
        self.executable_memory = Some(ExecutableMemoryHandle::new(memory, initial_size));
      
    }

    fn perform_finalization(&mut self) {
        for task in std::mem::take(&mut self.link_tasks) {
            task(self);
        }

        for task in std::mem::take(&mut self.late_link_tasks) {
            task(self);
        }
    }

    fn link_comments(&self, assembler: &AbstractMacroAssembler) {
        if let Some(executable_memory) = self.executable_memory.as_ref() {
            let mut map = CommentMap::new();

            for comment in assembler.comments.iter() {
                let comment_location = self.location_of(comment.0);

                match map.entry(comment_location as usize) {
                    Entry::Occupied(mut entry) => {
                        entry.get_mut().push_str(&format!("\n; {}", comment.1));
                    }

                    Entry::Vacant(entry) => {
                        entry.insert(format!("{}", comment.1));
                    }
                }
            }

            AssemblyCommentsRegistry::singleton().register_code_range(
                executable_memory.start(),
                executable_memory.end(),
                map,
            );
        }
    }

    fn code(&self) -> *mut u8 {
        self.code
    }

    fn get_linker_address(&self, src: AssemblerLabel) -> *mut u8 {
        let code = unsafe { AbstractMacroAssembler::get_linker_address(self.code(), src) };

        code
    }
}


