use tinyvec::TinyVec;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct AssemblerLabel {
    pub offset: u32,
}

impl Default for AssemblerLabel {
    fn default() -> Self {
        Self { offset: u32::MAX }
    }
}

impl AssemblerLabel {
    pub const fn offset(self) -> u32 {
        self.offset
    }

    pub const fn new(offset: u32) -> Self {
        Self { offset }
    }

    pub const fn is_set(self) -> bool {
        self.offset != u32::MAX
    }

    pub const fn label_at_offset(self, offset: i32) -> Self {
        Self {
            offset: (self.offset as i32 + offset) as u32,
        }
    }
}

pub struct AssemblerData {
    pub buffer: TinyVec<[u8; 128]>,
}

impl AssemblerData {
    pub fn new() -> Self {
        Self {
            buffer: TinyVec::new(),
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            buffer: TinyVec::with_capacity(capacity),
        }
    }

    pub fn take_buffer_if_larger(&mut self, other: &mut Self) {
        if self.buffer.len() > other.buffer.len() {
            other.buffer = std::mem::take(&mut self.buffer);
        }
    }

    pub fn clear(&mut self) {
        self.buffer.clear();
    }

    pub fn capacity(&self) -> usize {
        self.buffer.capacity()
    }

    pub fn grow(&mut self, size: usize) {
        self.buffer.resize(self.buffer.len() + size, 0);
    }

    pub fn buffer(&mut self) -> *mut u8 {
        self.buffer.as_mut_ptr()
    }
}

pub struct AssemblerBuffer {
    storage: AssemblerData,
    index: usize,
}

impl AssemblerBuffer {
    pub fn data(&self) -> &[u8] {
        &self.storage.buffer
    }

    pub fn data_mut(&mut self) -> &mut [u8] {
        &mut self.storage.buffer
    }

    pub fn set_code_size(&mut self, size: usize) {
        self.index = size;
    }

    pub fn grow(&mut self, size: usize) {
        self.storage.grow(size);
    }

    pub(crate) fn out_of_line_grow(&mut self, size: usize) {
        self.storage.grow(size);
    }

    pub fn put_integral_unchecked<T: Copy>(&mut self, value: T) {
        assert!(self.index + std::mem::size_of::<T>() <= self.storage.buffer.len());
        unsafe {
            self.storage
                .buffer()
                .add(self.index)
                .copy_from_nonoverlapping(
                    &value as *const T as *const u8,
                    std::mem::size_of::<T>(),
                );
        }
        self.index += std::mem::size_of::<T>();
    }

    pub fn put_integral<T: Copy>(&mut self, value: T) {
        if self.index + std::mem::size_of::<T>() > self.storage.buffer.len() {
            self.out_of_line_grow(std::mem::size_of::<T>());
        }
        self.put_integral_unchecked(value);
    }

    pub fn is_available(&self, size: usize) -> bool {
        self.index + size <= self.storage.buffer.len()
    }

    pub fn ensure_space(&mut self, size: usize) {
        if !self.is_available(size) {
            self.out_of_line_grow(size);
        }
    }

    pub fn put_byte(&mut self, val: i8) {
        self.put_integral(val);
    }

    pub fn put_u8(&mut self, val: u8) {
        self.put_integral(val);
    }

    pub fn put_u16(&mut self, val: u16) {
        self.put_integral(val);
    }

    pub fn put_u32(&mut self, val: u32) {
        self.put_integral(val);
    }

    pub fn put_u64(&mut self, val: u64) {
        self.put_integral(val);
    }

    pub fn put_i8(&mut self, val: i8) {
        self.put_integral(val);
    }

    pub fn put_i16(&mut self, val: i16) {
        self.put_integral(val);
    }

    pub fn put_i32(&mut self, val: i32) {
        self.put_integral(val);
    }

    pub fn put_i64(&mut self, val: i64) {
        self.put_integral(val);
    }

    pub fn put_short(&mut self, val: i16) {
        self.put_integral(val);
    }

    pub fn put_int(&mut self, val: i32) {
        self.put_integral(val);
    }

    pub fn put_long(&mut self, val: i64) {
        self.put_integral(val);
    }

    pub fn put_byte_unchecked(&mut self, val: i8) {
        {
            self.put_integral_unchecked(val);
        }
    }

    pub fn put_u8_unchecked(&mut self, val: u8) {
        {
            self.put_integral_unchecked(val);
        }
    }

    pub fn put_u16_unchecked(&mut self, val: u16) {
        {
            self.put_integral_unchecked(val);
        }
    }

    pub fn put_u32_unchecked(&mut self, val: u32) {
        {
            self.put_integral_unchecked(val);
        }
    }

    pub fn put_u64_unchecked(&mut self, val: u64) {
        {
            self.put_integral_unchecked(val);
        }
    }

    pub fn put_i8_unchecked(&mut self, val: i8) {
        {
            self.put_integral_unchecked(val);
        }
    }

    pub fn put_i16_unchecked(&mut self, val: i16) {
        {
            self.put_integral_unchecked(val);
        }
    }

    pub fn put_i32_unchecked(&mut self, val: i32) {
        {
            self.put_integral_unchecked(val);
        }
    }

    pub fn is_aligned(&self, alignment: usize) -> bool {
        (self.index & (alignment - 1)) == 0
    }

    pub fn new() -> Self {
        Self {
            storage: AssemblerData::new(),
            index: 0,
        }
    }

    pub fn label(&self) -> AssemblerLabel {
        AssemblerLabel::new(self.index as _)
    }

    pub fn debug_offset(&self) -> usize {
        self.index
    }

    pub fn take_assembler_data(&mut self) -> AssemblerData {
        std::mem::replace(&mut self.storage, AssemblerData::new())
    }

    pub fn code_size(&self) -> usize {
        self.index
    }
}

/// LocalWriter is a trick to keep the storage buffer and the index
/// in memory while issuing multiple Stores.
/// It is created in a block scope and its attribute can stay live
/// between writes.
pub struct LocalWriter<'a> {
    pub buffer: &'a mut AssemblerBuffer,
    index: usize,
}

impl<'a> LocalWriter<'a> {
    pub fn new(buffer: &'a mut AssemblerBuffer, required_space: usize) -> Self {
        buffer.ensure_space(required_space);
        Self {
            index: buffer.index,
            buffer,
        }
    }

    pub fn put_byte_unchecked(&mut self, byte: i8) {
        self.put_integral_unchecked(byte);
    }

    pub fn put_short_unchecked(&mut self, short: i16) {
        self.put_integral_unchecked(short);
    }

    pub fn put_int_unchecked(&mut self, int: i32) {
        self.put_integral_unchecked(int);
    }

    pub fn put_int64_unchecked(&mut self, int64: i64) {
        self.put_integral_unchecked(int64);
    }

    fn put_integral_unchecked<T: Copy>(&mut self, value: T) {
        assert!(self.index + std::mem::size_of::<T>() <= self.buffer.storage.buffer.len());
        unsafe {
            self.buffer
                .storage
                .buffer()
                .add(self.index)
                .copy_from_nonoverlapping(
                    &value as *const T as *const u8,
                    std::mem::size_of::<T>(),
                );
        }
        self.index += std::mem::size_of::<T>();
    }
}

impl<'a> Drop for LocalWriter<'a> {
    fn drop(&mut self) {
        self.buffer.index = self.index;
    }
}
