//! Direct port of `JavaScriptCore/jit` module.

use jit_allocator::{JitAllocator, JitAllocatorOptions};
use std::sync::{Mutex, OnceLock, PoisonError};

pub mod fpr_info;
pub mod gpr_info;
pub mod helpers;

pub static EXECUTABLE_ALLOCATOR: OnceLock<Mutex<Box<JitAllocator>>> = OnceLock::new();
pub fn init_executable_allocator_with(opts: JitAllocatorOptions) {
    EXECUTABLE_ALLOCATOR.get_or_init(|| Mutex::new(JitAllocator::new(opts)));
}

pub fn init_executable_allocator() {
    init_executable_allocator_with(JitAllocatorOptions {
        use_dual_mapping: true,
        use_multiple_pools: true,
        fill_unused_memory: true,
        immediate_release: false,
        ..Default::default()
    });
}

pub fn allocate_executable_memory(
    size: usize,
) -> Result<(*const u8, *mut u8), jit_allocator::Error> {
    EXECUTABLE_ALLOCATOR
        .get_or_init(|| Mutex::new(JitAllocator::new(JitAllocatorOptions::default())))
        .lock()
        .unwrap_or_else(PoisonError::into_inner)
        .alloc(size)
}

/// Free executable memory allocated by `allocate_executable_memory`.
///
/// # Safety
///
/// - `rx` must be pointer to read-execute memory allocated by `allocate_executable_memory`.
/// - `rx` must not be null.
/// - `rx` must not be double freed.
pub unsafe fn free_executable_memory(rx: *const u8) -> Result<(), jit_allocator::Error> {
    {
        EXECUTABLE_ALLOCATOR
            .get()
            .expect("Executable Allocator must be initialized before freeing")
            .lock()
            .unwrap_or_else(PoisonError::into_inner)
            .release(rx)
    }
}
