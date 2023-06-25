use std::sync::Arc;

use crate::jit::free_executable_memory;

pub struct ExecutableMemoryHandle {
    rx: *const u8,
    rw: *mut u8,
    size: usize,
}

impl ExecutableMemoryHandle {
    pub(crate) fn new(rx: *const u8, rw: *mut u8, size: usize) -> Arc<Self> {
        Arc::new(ExecutableMemoryHandle { rx, rw, size })
    }

    pub fn contains(&self, address: *const u8) -> bool {
        let start = self.start() as u64;
        let end = self.end() as u64;
        let address = address as u64;
        address >= start && address < end
    }

    pub fn contains_rw(&self, address: *const u8) -> bool {
        let start = self.start_rw() as u64;
        let end = self.end_rw() as u64;
        let address = address as u64;
        address >= start && address < end
    }

    pub fn start(&self) -> *mut u8 {
        self.rx as _
    }

    pub fn size_in_bytes(&self) -> usize {
        self.size
    }

    pub fn end(&self) -> *mut u8 {
        unsafe { self.start().add(self.size) }
    }

    pub fn start_rw(&self) -> *mut u8 {
        self.rw
    }

    pub fn end_rw(&self) -> *mut u8 {
        unsafe { self.rw.add(self.size) }
    }
}

impl Drop for ExecutableMemoryHandle {
    fn drop(&mut self) {
        free_executable_memory(self.rx);
    }
}

#[derive(Clone)]
pub enum CodeRef {
    SelfManaged((*mut u8, usize)),
    External(Arc<ExecutableMemoryHandle>),
}

impl CodeRef {
    pub fn start(&self) -> *mut u8 {
        match self {
            CodeRef::SelfManaged((start, _)) => *start,
            CodeRef::External(handle) => handle.start(),
        }
    }

    pub fn size_in_bytes(&self) -> usize {
        match self {
            CodeRef::SelfManaged((_, size)) => *size,
            CodeRef::External(handle) => handle.size_in_bytes(),
        }
    }

    pub fn end(&self) -> *mut u8 {
        unsafe { self.start().add(self.size_in_bytes()) }
    }

    pub fn contains(&self, address: *const u8) -> bool {
        let start = self.start() as usize;
        let end = self.end() as usize;
        let address = address as usize;
        address >= start && address < end
    }

    pub fn is_self_managed(&self) -> bool {
        match self {
            CodeRef::SelfManaged(_) => true,
            CodeRef::External(_) => false,
        }
    }

    pub fn is_external(&self) -> bool {
        match self {
            CodeRef::SelfManaged(_) => false,
            CodeRef::External(_) => true,
        }
    }
}

pub trait Function<R, A> {
    fn call(&self, args: A) -> R;
}
