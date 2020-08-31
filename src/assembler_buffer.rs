#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Debug, Hash, Default)]
pub struct AsmLabel(pub u32);

impl AsmLabel {
    pub const INIT: Self = Self(u32::MAX);
    pub const fn new(offset: u32) -> Self {
        Self(offset)
    }
    pub fn offset(self) -> u32 {
        self.0
    }

    pub const fn is_set(self) -> bool {
        self.0 != Self::INIT.0
    }

    pub const fn label_at_offset(self, off: i32) -> Self {
        Self((self.0 as i32 + off) as u32)
    }
}

pub struct AsmBuffer {
    pub storage: Vec<u8>,
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
        let bytes = short.to_le_bytes();
        self.append(&bytes[..]);
    }

    pub fn put_int(&mut self, int: i32) {
        let bytes = int.to_le_bytes();
        self.append(&bytes[..]);
    }

    pub fn put_long(&mut self, long: i64) {
        let bytes = long.to_le_bytes();
        self.append(&bytes[..]);
    }

    pub fn is_aligned(&self, align: usize) -> bool {
        return (self.index() & (align - 1)) == 0;
    }
}
