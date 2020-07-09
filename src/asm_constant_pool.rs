use crate::assembler_buffer::*;
use crate::utils::SegmentedVec;

pub struct AsmBufferWithConstPool {
    pub pool: *mut u32,
    pub mask: *mut u8,
    pub load_offsets: SegmentedVec<u32>,
    pub num_consts: usize,
    pub max_dist: usize,
    pub last_const_delta: usize,
    buf: AsmBuffer,
}

impl AsmBufferWithConstPool {
    pub fn put_byte(&mut self, b: i8) {
        self.buf.put_byte(b);
    }

    pub fn put_short(&mut self, s: i16) {
        self.buf.put_short(s);
    }

    pub fn put_int(&mut self, i: i32) {
        self.buf.put_int(i);
    }
    pub fn put_long(&mut self, l: i64) {
        self.buf.put_long(l);
    }
}
