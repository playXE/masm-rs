use std::ptr::null;
use std::ops::*;
use std::mem::size_of;

const CONDITION_NAMES: [&str; 16] = [
    "eq", "ne", "hs", "lo", "mi", "pl", "vs", "vc",
    "hi", "ls", "ge", "lt", "gt", "le", "al", "ne"
];

const OPTION_NAME: [&str; 8] = [
    "uxtb", "uxth", "uxtw", "uxtx", "sxtb", "sxth", "sxtw", "sxtx"
];

const SHIFT_NAME: [&str; 4] = [
    "lsl", "lsr", "asr", "ror"
];

const FP_REGISTER_PREFIX: [char; 5] = [
    'b', 'h', 's', 'd', 'q'
];


#[repr(C)]
pub struct A64DOpcode {
    pub format_buffer: String,
    pub start_pc: *const u32,
    pub end_pc: *const u32,
    pub current_pc: *const u32,
    pub opcode: u32,
    pub buffer_offset: usize,
    pub built_constant: usize, 
}

impl A64DOpcode {
    pub fn new(start_pc: *const u32, end_pc: *const u32) -> Self {
        Self {
            start_pc,
            end_pc,
            format_buffer: String::new(),
            opcode: 0,
            buffer_offset: 0,
            current_pc: null(),
            built_constant: 0,
        }

    }

    fn set_pc_and_opcode(&mut self, new_pc: *const u32, opcode: u32) {
        self.current_pc = new_pc;
        self.opcode = opcode;
        self.buffer_offset = 0;
        self.format_buffer.clear();
    }


    pub const fn opcode_group_number(opcode: u32) -> usize {
        (opcode >> 24) as usize & 0x1f
    }

    pub const fn is_64bit(&self) -> bool {
        (self.opcode & 0x80000000) != 0 
    }

    pub const fn size(&self) -> usize {
        (self.opcode >> 30) as usize
    }

    pub const fn option(&self) -> usize {
        (self.opcode >> 13) as usize & 0x7
    }

    pub const fn rd(&self) -> usize {
        self.opcode as usize & 0x1f
    }

    pub const fn rt(&self) -> usize {
        self.opcode as usize & 0x1f
    }

    pub const fn rn(&self) -> usize {
        (self.opcode >> 5) as usize & 0x1f
    }

    pub const fn rm(&self) -> usize {
        (self.opcode >> 16) as usize & 0x1f
    }

    pub fn option_name(&self) -> &str {
        OPTION_NAME[self.option()]
    }

    pub const fn shift_name(shift_value: usize) -> &'static str {
        SHIFT_NAME[shift_value & 0x3]
    }

    pub const fn condition_name(condition: usize) -> &'static str {
        CONDITION_NAMES[condition & 0xf]
    }

    pub const fn fp_register_prefix(size: usize) -> char {
        FP_REGISTER_PREFIX[size & 0x4]
    }

    pub fn append_instruction_name(&mut self, name: &str) {
        self.format_buffer.push_str(&format!("   {}", name));
    }

    pub fn append_str(&mut self, s: &str) {
        self.format_buffer.push_str(s);
    }

    pub fn append_register_name(&mut self, register_index: usize, is_64bit: bool) {
        if register_index == 29 {
            if is_64bit {
                self.append_str("fp");
            } else {
                self.append_str("wfp");
            }
            return;
        }

        if register_index == 30 {
            if is_64bit {
                self.append_str("lr");
            } else {
                self.append_str("wlr");
            }
            return;
        }

        self.append_str(&format!("{}{}", if is_64bit { 'x' } else { 'w' }, register_index));
    }

    pub fn append_sp_or_register_name(&mut self, register_index: usize, is_64bit: bool) {
        if register_index == 31 {
            if is_64bit {
                self.append_str("sp");
            } else {
                self.append_str("wsp");
            }

            return;
        }

        self.append_register_name(register_index, is_64bit);
    }

    pub fn append_zr_or_register_name(&mut self, register_index: usize, is_64bit: bool) {
        if register_index == 31 {
            if is_64bit {
                self.append_str("zr");
            } else {
                self.append_str("wzr");
            }

            return;
        }

        self.append_register_name(register_index, is_64bit);
    }

    pub fn append_fp_register_name(&mut self, register_index: usize, register_size: usize) {
        self.append_str(&format!("{}{}", Self::fp_register_prefix(register_size), register_index));
    }

    pub fn append_vector_register_name(&mut self, register_index: usize, register_size: usize) {
        self.append_str(&format!("Q{}", register_index));
    }

    pub fn append_separator(&mut self) {
        self.append_str(", ");
    }

    pub fn append_char(&mut self, c: char) {
        self.format_buffer.push(c);
    }


    pub fn append_shift_type(&mut self, shift_type: usize) {
        self.append_char('#');
        self.append_str(Self::shift_name(shift_type));
    }

    pub fn append_signed_immediate(&mut self, immediate: i32) {
        self.append_char('#');
        self.append_str(&immediate.to_string());
    }

    pub fn append_signed_immediate64(&mut self, immediate: i64) {
        self.append_char('#');
        self.append_str(&immediate.to_string());
    }

    pub fn append_unsigned_immediate(&mut self, immediate: u32) {
        self.append_char('#');
        self.append_str(&immediate.to_string());
    }

    pub fn append_unsigned_immediate64(&mut self, immediate: u64) {
        self.append_char('#');
        self.append_str(&immediate.to_string());
    }

    pub fn append_shift_amount(&mut self, amount: usize) {
        self.append_str(&format!("lsl #{}", 16 * amount));
    }

    pub fn append_simd_lane_index_and_type(&mut self, imm6: usize) {
        let mut lane = 0;

        if (imm6 & 0b100001) == 0b000001 {
            self.append_str(".8B");
            lane = (imm6 & 0b011110) >> 1;
        } else if (imm6 & 0b100001) == 0b000001 {
            self.append_str(".16B");
            lane = (imm6 & 0b011110) >> 1;
        } else if (imm6 & 0b100011) == 0b000010 {
            self.append_str(".H");
            lane = (imm6 & 0b011100) >> 2;
        } else if (imm6 & 0b100111) == 0b000100 {
            self.append_str(".S");
            lane = (imm6 & 0b011000) >> 3;
        } else if (imm6 & 0b001111) == 0b001000 {
            self.append_str(".D");
            lane = (imm6 & 0b010000) >> 4;
        } else {
            self.append_str(".INVALID_LANE_TYPE")
        }

        self.append_str(&format!("[{}]", lane));
    }

    pub fn append_simd_lane_type(&mut self, q : usize) {
        if q != 0 {
            self.append_str(".16B");
        } else {
            self.append_str(".8B");
        }
    }

    pub fn append_pc_relative_offset(&mut self, pc: *const u32, immediate: i32) {
        unsafe {
            let target_pc = pc.wrapping_offset(immediate as isize);
            let target_info = if self.start_pc.is_null() {
                "".to_owned()
            } else if target_pc >= self.start_pc && target_pc < self.end_pc {
                format!(" -> <{:x}>", target_pc.offset_from(self.start_pc) * size_of::<u32>() as isize)
            } else {
                format!(" -> <unknown>")
            };

            self.append_str(&format!("0x{:x}{}", target_pc as usize, target_info))
        }
    }

}



struct OpcodeGroup {
    opcode_mask: u32,
    opcode_pattern: u32,
    format: for <'a> fn(&'a mut A64DOpcode) -> &'a [u8],
    next: Option<&'static OpcodeGroup>,
}

impl OpcodeGroup {
    fn new(opcode_mask: u32, opcode_pattern: u32, fmt: fn(&mut A64DOpcode) -> &[u8], next: Option<&'static OpcodeGroup>) -> OpcodeGroup {
        OpcodeGroup {
            opcode_mask,
            opcode_pattern,
            format: fmt,
            next,
        }
    }

    fn set_next(&mut self, next: &'static OpcodeGroup) {
        self.next = Some(next);
    }

    const fn next(&self) -> Option<&'static OpcodeGroup> {
        self.next
    }

    const fn matches(&self, opcode: u32) -> bool {
        (opcode & self.opcode_mask) == self.opcode_pattern
    }
    
    fn format<'a>(&'a self, opc: &'a mut A64DOpcode) -> &[u8] {
        (self.format)(opc)
    }

}


pub struct A64DOpcodeAddSubtract(pub A64DOpcode);

impl A64DOpcodeAddSubtract {
    pub const MASK: u32 = 0x1f200000;
    pub const PATTERN: u32 = 0x0b000000;

}

impl Deref for A64DOpcodeAddSubtract {
    type Target = A64DOpcode;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for A64DOpcodeAddSubtract {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

