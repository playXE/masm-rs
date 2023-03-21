use std::sync::Arc;

use super::virtual_memory::VirtualMemory;


pub struct ExecutableMemoryHandle {
    vmem: VirtualMemory,
    size: usize,
}

impl ExecutableMemoryHandle {
    pub(crate) fn new(vmem: VirtualMemory, size: usize) -> Arc<Self> {
        Arc::new(ExecutableMemoryHandle { vmem, size })
    }

    pub fn contains(&self, address: *const u8) -> bool {
        let start = self.vmem.start();
        let end = self.vmem.end();
        let address = address as usize;
        address >= start && address < end
    }

    pub fn start(&self) -> *mut u8 {
        self.vmem.start() as _
    }

    pub fn size_in_bytes(&self) -> usize {
        self.size
    }

    pub fn end(&self) -> *mut u8 {
        self.vmem.end() as _
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


macro_rules! as_func {
    ($this: expr, $typ: ty) => {
        unsafe {
            std::mem::transmute::<*mut u8, $typ>($this.start())
        }
    };
}

