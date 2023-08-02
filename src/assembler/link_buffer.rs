use std::{collections::hash_map::Entry, ptr::null_mut};

use jit_allocator::{flush_instruction_cache, protect_jit_memory, ProtectJitAccess};

use crate::{
    assembler::abstract_macro_assembler::Call,
    jit::allocate_executable_memory,
    wtf::executable_memory_handle::{CodeRef, ExecutableMemoryHandle},
};

use super::{
    abstract_macro_assembler::{
        AbstractMacroAssembler, DataLabelPtr, Jump, JumpList, Label, Location, PatchableJump,
    },
    assembly_comments::{AssemblyCommentsRegistry, CommentMap},
    buffer::AssemblerLabel,
    disassembler::try_to_disassemble,
    TargetAssembler, TargetMacroAssembler,
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
    code_rx: *const u8,
    link_tasks: Vec<Box<dyn FnOnce(&mut Self)>>,
    late_link_tasks: Vec<Box<dyn FnOnce(&mut Self)>>,
    did_allocate: bool,
}

impl LinkBuffer {
    pub fn from_macro_assembler(
        macro_assembler: &mut TargetMacroAssembler,
    ) -> Result<Self, jit_allocator::Error> {
        let mut this = Self {
            executable_memory: None,
            size: 0,
            is_already_disassembled: false,
            is_thunk: false,
            code: null_mut(),
            link_tasks: vec![],
            late_link_tasks: vec![],
            did_allocate: false,
            code_rx: null_mut(),
        };

        this.link_code(macro_assembler)?;

        Ok(this)
    }

    pub fn from_code(
        macro_assembler: &mut TargetMacroAssembler,
        code_rx: *const u8,
        code_rw: *mut u8,
        size: usize,
    ) -> Result<Self, jit_allocator::Error> {
        let mut this = Self {
            executable_memory: None,
            size,
            is_already_disassembled: false,
            is_thunk: false,
            code: code_rw,
            code_rx,
            link_tasks: vec![],
            late_link_tasks: vec![],
            did_allocate: false,
        };

        this.link_code(macro_assembler)?;

        Ok(this)
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

    pub fn rx_location_of(&self, location: impl Into<Location>) -> *const u8 {
        match location.into() {
            Location::Call(call) => {
                assert!(call.is_flag_set(Call::LINKABLE));
                assert!(!call.is_flag_set(Call::NEAR));

                self.get_rx_linker_address(call.label)
            }
            Location::Label(label) => self.get_rx_linker_address(label.label),
            Location::ConvertibleLoadLabel(label) => self.get_rx_linker_address(label.label),
            Location::DataLabel32(label) => self.get_rx_linker_address(label.label),
            Location::DataLabelPtr(label) => self.get_rx_linker_address(label.label),
            Location::DataLabelCompact(label) => self.get_rx_linker_address(label.label),
            Location::PatchableJump(label) => self.get_rx_linker_address(label.label),
            _ => unreachable!("Use location_of_near_call for NearCall"),
        }
    }

    pub fn rx_location_of_near_call(&self, call: Call) -> (*const u8, bool) {
        assert!(call.is_flag_set(Call::LINKABLE));
        assert!(call.is_flag_set(Call::NEAR));

        (
            self.get_rx_linker_address(call.label),
            call.is_flag_set(Call::TAIL),
        )
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
    #[allow(clippy::not_unsafe_ptr_arg_deref)]
    pub fn link_call(&mut self, call: Call, function: *const u8) {
        // # Safety
        // `self.code()` is initialized by `allocate()`
        unsafe {
            protect_jit_memory(ProtectJitAccess::ReadWrite);
            TargetMacroAssembler::link_call(self.code(), call, function);
            protect_jit_memory(ProtectJitAccess::ReadExecute);
            flush_instruction_cache(self.code_rx, self.size);
        }
    }

    pub fn link_jump(&mut self, jump: Jump, target: *const u8) {
        // # Safety
        // `self.code()` is initialized by `allocate()`
        unsafe {
            protect_jit_memory(ProtectJitAccess::ReadWrite);
            TargetAssembler::link_jump_(self.code(), jump.label, target as *mut u8);
            protect_jit_memory(ProtectJitAccess::ReadExecute);
            flush_instruction_cache(self.code_rx, self.size);
        }
    }

    pub fn link_jumps(&mut self, jump: &JumpList, target: *const u8) {
        unsafe {
            protect_jit_memory(ProtectJitAccess::ReadWrite);
            for jump in jump.jumps() {
                TargetAssembler::link_jump_(self.code(), jump.label, target as *mut u8);
            }
            protect_jit_memory(ProtectJitAccess::ReadExecute);
            flush_instruction_cache(self.code_rx, self.size);
        }
    }

    pub fn patch(&mut self, label: DataLabelPtr, value: *const u8) {
        unsafe {
            protect_jit_memory(ProtectJitAccess::ReadWrite);
            TargetAssembler::link_pointer(self.code().cast(), label.label, value as *mut u8);
            protect_jit_memory(ProtectJitAccess::ReadExecute);
            flush_instruction_cache(self.code_rx, self.size);
        }
    }

    pub fn entrypoint(&self) -> *const u8 {
        self.code()
    }

    #[cfg(target_arch = "aarch64")]
    fn copy_and_compact_link_code(&mut self, macroassembler: &mut TargetMacroAssembler) -> Result<(), jit_allocator::Error> {
        use std::mem::size_of;
        self.allocate(macroassembler)?;
        if self.did_fail_to_allocate() {
            return Ok(());
        }
        protect_jit_memory(ProtectJitAccess::ReadWrite);
        let initial_size = macroassembler.code_size();
        let mut jumps_to_link = macroassembler.jumps_to_link().to_vec();
        let mut assembler_storage = macroassembler.assembler.buffer_mut().release_storage();
        let in_data = assembler_storage.buffer();
        let code_out_data = self.code;
        let jump_count = jumps_to_link.len();
        let mut read_ptr = 0;
        let mut write_ptr = 0;
        let storage = assembler_storage.buffer().cast::<i32>();
  
        unsafe {
            let executable_offset_for = |loc: i32| -> i32 {
                if loc < size_of::<i32>() as i32 {
                    return 0;
                }
    
                storage.add(loc as usize / size_of::<i32>() - 1).read()
            };
            for i in 0..jump_count {
                let offset = read_ptr - write_ptr;
                
                let region_size = jumps_to_link[i].from() as isize - read_ptr as isize;
                let mut copy_source = in_data.offset(read_ptr as _).cast::<i32>();
                let copy_end = in_data.offset(read_ptr as isize + region_size).cast::<i32>();
                let mut copy_dst = code_out_data.offset(write_ptr as _).cast::<i32>();

                while copy_source != copy_end {
                    copy_dst.write(copy_source.read());
                 
                    copy_dst = copy_dst.add(1);
                    copy_source = copy_source.add(1);
                }

                Self::record_link_offsets(storage, read_ptr, jumps_to_link[i].from() as _, offset);

                read_ptr += region_size as i32;
                write_ptr += region_size as i32;

                let to = jumps_to_link[i].to();

                let target = if to >= jumps_to_link[i].from() as isize {
                    code_out_data.offset(to - offset as isize)
                } else {
                    let _off = executable_offset_for(to as _);
                  
                    code_out_data.offset(to - executable_offset_for(to as _) as isize)
                };
                TargetAssembler::compute_jump_type(&mut jumps_to_link[i], code_out_data.offset(write_ptr as _), target);
                jumps_to_link[i].set_from(write_ptr as _);  
            }


            let read = |ptr: *const i32| -> i32 { ptr.read() };

            let mut dst = code_out_data.add(write_ptr as _).cast::<i32>();
            let mut src = in_data.add(read_ptr as _).cast::<i32>();
            let bytes = (initial_size as isize - read_ptr as isize) as usize;
            let mut i = 0;
            while i < bytes {
                let insn = read(src);
                src = src.add(1);
                dst.write(insn);
             
                dst = dst.add(1);
                i += size_of::<i32>();
            }

            
            Self::record_link_offsets(storage, read_ptr, initial_size as _, read_ptr - write_ptr);
            
            for i in 0..jump_count {
                let location = code_out_data.offset(jumps_to_link[i].from() as isize);
                let to = jumps_to_link[i].to();

                let target = code_out_data
                    .offset(to as isize)
                    .offset(-(executable_offset_for(to as _) as isize));

                TargetAssembler::link(
                    &jumps_to_link[i],
                    code_out_data.offset(jumps_to_link[i].from() as isize),
                    location.cast(),
                    target.cast(),
                );
            }

            let compact_size = write_ptr + initial_size as i32 - read_ptr;
            let nop_size_in_bytes = initial_size - compact_size as usize;
            
            TargetAssembler::fill_nops(code_out_data.add(compact_size as _), nop_size_in_bytes);

            flush_instruction_cache(self.code_rx, self.size);
            protect_jit_memory(ProtectJitAccess::ReadExecute);

            Ok(())
        }
    }

    #[cfg(target_arch = "aarch64")]
    unsafe fn record_link_offsets(
        assembler_data: *mut i32,
        region_start: i32,
        region_end: i32,
        offset: i32,
    ) {
        let mut ptr = region_start / 4;
        let end = region_end / 4;

        let offsets = assembler_data;

        while ptr < end {
            offsets.offset(ptr as isize).write(offset);
            ptr += 1;
        }
    }

    pub fn link_code(
        &mut self,
        macro_assembler: &mut TargetMacroAssembler,
    ) -> Result<(), jit_allocator::Error> {
        macro_assembler.pad_before_patch();
        #[cfg(not(target_arch="aarch64"))]
        {
            self.allocate(macro_assembler)?;
            if !self.did_allocate {
                return Ok(());
            }

            let buffer = macro_assembler.assembler.buffer().data();

            unsafe {
                protect_jit_memory(ProtectJitAccess::ReadWrite);
                std::ptr::copy_nonoverlapping(buffer.as_ptr(), self.code, buffer.len());
                protect_jit_memory(ProtectJitAccess::ReadExecute);
                flush_instruction_cache(self.code_rx, self.size);
            }
        }
        #[cfg(target_arch = "aarch64")]
        {
            self.copy_and_compact_link_code(macro_assembler)?;
        }
        self.link_tasks = std::mem::take(&mut macro_assembler.link_tasks);
        self.late_link_tasks = std::mem::take(&mut macro_assembler.late_link_tasks);

        self.link_comments(macro_assembler);

        Ok(())
    }

    pub fn finalize_without_disassembly(&mut self) -> CodeRef {
        self.finalize_code_without_disassembly_impl()
    }

    pub fn finalize_with_disassembly(
        &mut self,
        dump_disassembly: bool,
        format: &str,
        out: &mut impl std::fmt::Write,
    ) -> Result<CodeRef, std::fmt::Error> {
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
    fn finalize_code_with_disassembly_impl<W: std::fmt::Write>(
        &mut self,
        dump_disassembly: bool,
        format: &str,
        out: &mut W,
    ) -> Result<CodeRef, std::fmt::Error> {
        let result = self.finalize_code_without_disassembly_impl();
        let just_dumping_header = !dump_disassembly || self.is_already_disassembled;

        write!(out, "Generated JIT code for {}:\n", format)?;

        let executable_address = result.start();
        let executable_end = result.end();

        write!(
            out,
            "    Code at [{:p}, {:p}){}\n",
            executable_address,
            executable_end,
            if just_dumping_header { "." } else { ":" }
        )?;

        if just_dumping_header {
            return Ok(result);
        }

        // # Safety
        //  we just allocated the code and it is for sure a valid pointer
        unsafe {
            try_to_disassemble(
                result.start(),
                result.end() as usize - result.start() as usize,
                "    ",
                out,
            )?;
        }

        Ok(result)
    }

    fn allocate(
        &mut self,
        macro_assembler: &mut TargetMacroAssembler,
    ) -> Result<(), jit_allocator::Error> {
        let mut initial_size = macro_assembler.code_size();

        if !self.code.is_null() {
            if initial_size > self.size {
                return Ok(());
            }

            let nops_to_fill_in_bytes = self.size - initial_size;
            macro_assembler.emit_nops(nops_to_fill_in_bytes);
            self.did_allocate = true;
            return Ok(());
        }

        while initial_size % 32 != 0 {
            macro_assembler.breakpoint();
            initial_size = macro_assembler.code_size();
        }

        let (rx, rw) = allocate_executable_memory(initial_size)?;

        self.size = initial_size as usize;
        self.did_allocate = true;
        self.code = rw;
        self.code_rx = rx;
        self.executable_memory = Some(ExecutableMemoryHandle::new(rx, rw, initial_size));

        Ok(())
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

    pub fn get_linker_address(&self, src: AssemblerLabel) -> *mut u8 {
        let code = unsafe { AbstractMacroAssembler::get_linker_address(self.code(), src) };

        code
    }

    pub fn get_rx_linker_address(&self, src: AssemblerLabel) -> *const u8 {
        let code = unsafe { AbstractMacroAssembler::get_linker_address(self.code_rx as _, src) };

        code
    }
}
