use crate::*;
use assembler_buffer::*;

use std::marker::PhantomData;
pub struct LinkBuffer<T: MacroAssemblerBase> {
    pub code: *mut u8,
    pub size: usize,
    pub did_allocate: bool,
    pub exec_mem: *mut u8,
    pub link_tasks: Vec<Box<dyn FnOnce(&mut LinkBuffer<T>)>>,
    _m: PhantomData<T>,
}

impl<T: MacroAssemblerBase> LinkBuffer<T> {
    pub fn new(code: *mut u8) -> Self {
        Self {
            code,
            size: 0,
            did_allocate: false,
            exec_mem: std::ptr::null_mut(),
            _m: PhantomData::default(),
            link_tasks: vec![],
        }
    }

    pub fn perform_finalization(&mut self) {
        while let Some(task) = self.link_tasks.pop() {
            task(self);
        }
    }

    pub fn link_data(&self, at: AsmLabel, value: *mut u8) {
        T::link_pointer(self.code, at, value);
    }
    pub fn link_call(&self, at: Call, with: *const u8) {
        T::link_call(self.code, at, with as *mut _, 0);
    }

    pub fn link_jump(&self, jump: AsmLabel, to: AsmLabel) {
        T::link_jump(self.code, jump, to);
    }
    pub fn link_jump_ptr(&self, jump: AsmLabel, to: *const u8) {
        T::link_jump_ptr(self.code, jump, to);
    }
    pub fn location_of_label(&self, label: AsmLabel) -> *mut u8 {
        T::get_linker_addr(self.code, label)
    }

    pub fn did_fail_to_allocate(&self) -> bool {
        !self.did_allocate
    }
}

#[cfg(not(feature = "selinux-fix"))]
use errno;

#[cfg(not(any(feature = "selinux-fix", windows)))]
use libc;

#[cfg(feature = "selinux-fix")]
use memmap::MmapMut;

use region;
use std::mem;
use std::ptr;
/// Round `size` up to the nearest multiple of `page_size`.
fn round_up_to_page_size(size: usize, page_size: usize) -> usize {
    (size + (page_size - 1)) & !(page_size - 1)
}

/// A simple struct consisting of a pointer and length.
pub struct PtrLen {
    #[cfg(feature = "selinux-fix")]
    pub map: Option<MmapMut>,

    pub ptr: *mut u8,
    pub len: usize,
}

impl PtrLen {
    /// Create a new empty `PtrLen`.
    pub fn new() -> Self {
        Self {
            #[cfg(feature = "selinux-fix")]
            map: None,

            ptr: ptr::null_mut(),
            len: 0,
        }
    }

    /// Create a new `PtrLen` pointing to at least `size` bytes of memory,
    /// suitably sized and aligned for memory protection.
    #[cfg(all(not(target_os = "windows"), feature = "selinux-fix"))]
    pub fn with_size(size: usize) -> Result<Self, String> {
        let page_size = region::page::size();
        let alloc_size = round_up_to_page_size(size, page_size);
        let map = MmapMut::map_anon(alloc_size);

        match map {
            Ok(mut map) => {
                // The order here is important; we assign the pointer first to get
                // around compile time borrow errors.
                Ok(Self {
                    ptr: map.as_mut_ptr(),
                    map: Some(map),
                    len: alloc_size,
                })
            }
            Err(e) => Err(e.to_string()),
        }
    }

    #[cfg(all(not(target_os = "windows"), not(feature = "selinux-fix")))]
    pub fn with_size(size: usize) -> Result<Self, String> {
        let mut ptr = ptr::null_mut();
        let page_size = region::page::size();
        let alloc_size = round_up_to_page_size(size, page_size);
        unsafe {
            let err = libc::posix_memalign(&mut ptr, page_size, alloc_size);

            if err == 0 {
                Ok(Self {
                    ptr: ptr as *mut u8,
                    len: alloc_size,
                })
            } else {
                Err(errno::Errno(err).to_string())
            }
        }
    }

    #[cfg(target_os = "windows")]
    pub fn with_size(size: usize) -> Result<Self, String> {
        use winapi::um::memoryapi::VirtualAlloc;
        use winapi::um::winnt::{MEM_COMMIT, MEM_RESERVE, PAGE_READWRITE};

        let page_size = region::page::size();

        // VirtualAlloc always rounds up to the next multiple of the page size
        let ptr = unsafe {
            VirtualAlloc(
                ptr::null_mut(),
                size,
                MEM_COMMIT | MEM_RESERVE,
                PAGE_READWRITE,
            )
        };
        if !ptr.is_null() {
            Ok(Self {
                ptr: ptr as *mut u8,
                len: round_up_to_page_size(size, page_size),
            })
        } else {
            Err(errno::errno().to_string())
        }
    }
}

// `MMapMut` from `cfg(feature = "selinux-fix")` already deallocates properly.
#[cfg(all(not(target_os = "windows"), not(feature = "selinux-fix")))]
impl Drop for PtrLen {
    fn drop(&mut self) {
        if !self.ptr.is_null() {
            unsafe {
                region::protect(self.ptr, self.len, region::Protection::READ_WRITE)
                    .expect("unable to unprotect memory");
                libc::free(self.ptr as _);
            }
        }
    }
}

// TODO: add a `Drop` impl for `cfg(target_os = "windows")`

/// JIT memory manager. This manages pages of suitably aligned and
/// accessible memory. Memory will be leaked by default to have
/// function pointers remain valid for the remainder of the
/// program's life.
pub struct Memory {
    allocations: Vec<PtrLen>,
    executable: usize,
    current: PtrLen,
    position: usize,
}

impl Memory {
    pub fn new() -> Self {
        Self {
            allocations: Vec::new(),
            executable: 0,
            current: PtrLen::new(),
            position: 0,
        }
    }

    fn finish_current(&mut self) {
        self.allocations
            .push(mem::replace(&mut self.current, PtrLen::new()));
        self.position = 0;
    }

    /// TODO: Use a proper error type.
    pub fn allocate(&mut self, size: usize, align: u8) -> Result<*mut u8, String> {
        if self.position % align as usize != 0 {
            self.position += align as usize - self.position % align as usize;
            debug_assert!(self.position % align as usize == 0);
        }

        if size <= self.current.len - self.position {
            // TODO: Ensure overflow is not possible.
            let ptr = unsafe { self.current.ptr.add(self.position) };
            self.position += size;
            return Ok(ptr);
        }

        self.finish_current();

        // TODO: Allocate more at a time.
        self.current = PtrLen::with_size(size)?;
        self.position = size;
        Ok(self.current.ptr)
    }

    pub fn set_readable_and_executable_ptr(&mut self, code: *mut u8, size: usize) {
        unsafe {
            region::protect(code, size, region::Protection::READ_EXECUTE).unwrap();
        }
    }

    fn set_protection(&mut self, protection: region::Protection) {
        self.finish_current();

        #[cfg(feature = "selinux-fix")]
        {
            for &PtrLen { ref map, ptr, len } in &self.allocations[self.executable..] {
                if len != 0 && map.is_some() {
                    unsafe {
                        region::protect(ptr, len, protection)
                            .expect("unable to set memory protection");
                    }
                }
            }
        }

        #[cfg(not(feature = "selinux-fix"))]
        {
            for &PtrLen { ptr, len } in &self.allocations[self.executable..] {
                if len != 0 {
                    unsafe {
                        region::protect(ptr, len, protection)
                            .expect("unable to set memory protection");
                    }
                }
            }
        }
    }

    /// Set all memory allocated in this `Memory` up to now as readable and executable.
    pub fn set_readable_and_executable(&mut self) {
        self.set_protection(region::Protection::READ_EXECUTE);
    }

    /// Set all memory allocated in this `Memory` up to now as readonly.
    pub fn set_readonly(&mut self) {
        self.set_protection(region::Protection::READ);
    }

    pub fn set_rwx(&mut self) {
        self.set_protection(region::Protection::READ_WRITE_EXECUTE);
    }

    pub fn set_rwx_mem(mem: *mut u8, len: usize) {
        if len != 0 {
            unsafe {
                region::protect(mem, len, region::Protection::READ_WRITE_EXECUTE).unwrap();
            }
        }
    }

    /// Frees all allocated memory regions that would be leaked otherwise.
    /// Likely to invalidate existing function pointers, causing unsafety.
    pub unsafe fn free_memory(&mut self) {
        self.allocations.clear();
    }
}

impl Drop for Memory {
    fn drop(&mut self) {
        // leak memory to guarantee validity of function pointers
        mem::replace(&mut self.allocations, Vec::new())
            .into_iter()
            .for_each(mem::forget);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_round_up_to_page_size() {
        assert_eq!(round_up_to_page_size(0, 4096), 0);
        assert_eq!(round_up_to_page_size(1, 4096), 4096);
        assert_eq!(round_up_to_page_size(4096, 4096), 4096);
        assert_eq!(round_up_to_page_size(4097, 4096), 8192);
    }
}
unsafe impl Send for Memory {}
unsafe impl Sync for Memory {}
