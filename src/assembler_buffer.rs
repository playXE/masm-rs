#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Debug, Hash)]
pub struct AsmLabel(u32);

impl AsmLabel {
    pub const INIT: Self = Self(u32::MAX);
    pub const fn new(offset: u32) -> Self {
        Self(offset)
    }

    pub const fn is_set(self) -> bool {
        self.0 != Self::INIT.0
    }

    pub const fn label_at_offset(self, off: u32) -> Self {
        Self(self.0 + off)
    }
}

pub struct AsmBuffer {
    storage: Vec<u8>,
}

impl AsmBuffer {
    pub fn new() -> Self {
        Self {
            storage: Vec::with_capacity(128),
        }
    }
    pub fn index(&self) -> usize {
        self.storage.len()
    }
    pub fn append(&mut self, data: &[u8]) {
        self.storage.extend(data);
    }

    pub fn label(&self) -> AsmLabel {
        AsmLabel(self.index() as _)
    }

    pub fn put_byte(&mut self, byte: i8) {
        self.storage.push(byte as _);
    }

    pub fn put_short(&mut self, short: i16) {
        let bytes: [i8; 2] = unsafe { std::mem::transmute(short) };
        self.put_byte(bytes[0]);
        self.put_byte(bytes[1]);
    }

    pub fn put_int(&mut self, int: i32) {
        let shorts: [i16; 2] = unsafe { std::mem::transmute(int) };
        self.put_short(shorts[0]);
        self.put_short(shorts[1]);
    }

    pub fn put_long(&mut self, long: i64) {
        let ints: [i32; 2] = unsafe { std::mem::transmute(long) };
        self.put_int(ints[0]);
        self.put_int(ints[1]);
    }

    pub fn is_aligned(&self, align: usize) -> bool {
        return (self.index() & (align - 1)) == 0;
    }
}
