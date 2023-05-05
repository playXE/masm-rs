//! Direct port of `JavaScriptCore/jit` module.

use parking_lot::Mutex;
use vm_allocator::RangeInclusive;

use crate::wtf::virtual_memory::{Protection, VirtualMemory};

pub mod fpr_info;
pub mod helpers;
pub mod gpr_info;

/// Simple executable memory allocator. It uses `vm-allocator` crate to allocate memory. 
/// 
/// The memory for JIT code is allocated in a single chunk. The allocator is not thread-safe.
pub struct ExecutableAllocator {
    vmem: VirtualMemory,
    vm_allocator: vm_allocator::AddressAllocator,
}

impl ExecutableAllocator {
    pub fn new(size: usize) -> Self {
        let vmem = VirtualMemory::allocate(size, true, "JIT code").unwrap();
        let vm_allocator = vm_allocator::AddressAllocator::new(vmem.start() as _, vmem.size() as _)
            .expect("Failed to create address allocator");
        Self { vmem, vm_allocator }
    }

    pub fn allocate(&mut self, size: usize, alignment: usize) -> RangeInclusive {
        self.vm_allocator
            .allocate(
                size as _,
                alignment as _,
                vm_allocator::AllocPolicy::FirstMatch,
            )
            .unwrap()
    }

    pub fn free(&mut self, range: RangeInclusive) {
        self.vm_allocator.free(&range).unwrap()
    }

    pub fn protect(&self, prot: Protection) {
        self.vmem.protect(prot);
    }
}

pub static EXECUTABLE_ALLOCATOR: once_cell::sync::OnceCell<Mutex<ExecutableAllocator>> =
    once_cell::sync::OnceCell::new();

pub fn init_executable_allocator_with(size: usize) {
    EXECUTABLE_ALLOCATOR.get_or_init(|| Mutex::new(ExecutableAllocator::new(size)));
}

pub fn init_executable_allocator() {
    init_executable_allocator_with(256 * 1024 * 1024);
}

pub const EXEC_MEM_SIZE: usize = 256 * 1024 * 1024;

pub fn allocate_executable_memory(size: usize, alignment: usize) -> RangeInclusive {
    EXECUTABLE_ALLOCATOR
        .get_or_init(|| Mutex::new(ExecutableAllocator::new(EXEC_MEM_SIZE)))
        .lock()
        .allocate(size, alignment)
}

pub fn free_executable_memory(range: RangeInclusive) {
    EXECUTABLE_ALLOCATOR.get().expect("Executable Allocator must be initialized before freeing").lock().free(range)
}

pub fn protect_executable_memory(prot: Protection) {
    EXECUTABLE_ALLOCATOR.get().unwrap().lock().protect(prot)
}
