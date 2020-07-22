use super::assembler_buffer::*;
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Debug, Hash)]
#[repr(i8)]
pub enum RegisterID {
    X0,
    X1,
    X2,
    X3,
    X4,
    X5,
    X6,
    X7,
    X8,
    X9,
    X10,
    X11,
    X12,
    X13,
    X14,
    X15,
    X16,
    X17,
    X18,
    X19,
    X20,
    X21,
    X22,
    X23,
    X24,
    X25,
    X26,
    X27,
    X28,
    FP,
    LR,
    SP,
    Invalid = -1,
}

pub const IP0: RegisterID = RegisterID::X16;
pub const IP1: RegisterID = RegisterID::X17;
pub const X29: RegisterID = RegisterID::FP;
pub const X30: RegisterID = RegisterID::LR;
pub const ZR: u8 = 0x3f;
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Debug, Hash)]
#[repr(u8)]
pub enum FPRegisterID {
    Q0,
    Q1,
    Q2,
    Q3,
    Q4,
    Q5,
    Q6,
    Q7,
    Q8,
    Q9,
    Q10,
    Q11,
    Q12,
    Q13,
    Q14,
    Q15,
    Q16,
    Q17,
    Q18,
    Q19,
    Q20,
    Q21,
    Q22,
    Q23,
    Q24,
    Q25,
    Q26,
    Q27,
    Q28,
    Q29,
    Q30,
    Q31,
}

macro_rules! is_int {
    ($value: expr,$bits: expr) => {{
        let shift = std::mem::size_of_val(&$value) * libc::CHAR_BIT - $bits;
        (($value << shift) >> shift) == $value
    }};
}

macro_rules! is_4byte_aligned {
    ($val: expr) => {
        ($val & 0x3) == 0
    };
}

macro_rules! is_uint5 {
    ($x: expr) => {
        (value & !0x1f) == 0
    };
}

pub const fn get_halfword(value: u64, which: i32) -> u16 {
    return (value >> ((which as u64) << 4)) as u16;
}

const fn jump_enum_with_size(index: i32, val: i32) -> i32 {
    (val << 4) | index
}
use std::mem::size_of;
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Debug, Hash)]
#[repr(i32)]
pub enum JumpType {
    Fixed = jump_enum_with_size(0, 0),
    NoCondition = jump_enum_with_size(1, 1 * size_of::<u32>() as i32),
    Condition = jump_enum_with_size(2, 2 * size_of::<u32>() as i32),
    CompareAndBranch = jump_enum_with_size(3, 2 * size_of::<u32>() as i32),
    TestBit = jump_enum_with_size(4, 2 * size_of::<u32>() as i32),
    NoConditionFixedSize = jump_enum_with_size(5, 1 * size_of::<u32>() as i32),
    ConditionFixedSize = jump_enum_with_size(6, 2 * size_of::<u32>() as i32),
    CompareAndBranchFixedSize = jump_enum_with_size(7, 2 * size_of::<u32>() as i32),
    TestBitFixedSize = jump_enum_with_size(8, 2 * size_of::<u32>() as i32),
}
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Debug, Hash)]
#[repr(i32)]
pub enum JumpLinkType {
    Invalid = jump_enum_with_size(0, 0),
    NoCondition = jump_enum_with_size(1, 1 * size_of::<u32>() as i32),
    ConditionDirect = jump_enum_with_size(2, 1 * size_of::<u32>() as i32),
    Condition = jump_enum_with_size(3, 2 * size_of::<u32>() as i32),
    CompareAndBranch = jump_enum_with_size(4, 2 * size_of::<u32>() as i32),
    CompareAndBranchDirect = jump_enum_with_size(5, 1 * size_of::<u32>() as i32),
    TestBit = jump_enum_with_size(6, 2 * size_of::<u32>() as i32),
    TestBitDirect = jump_enum_with_size(7, 1 * size_of::<u32>() as i32),
}
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Debug, Hash)]
#[repr(u8)]
pub enum Condition {
    EQ,
    NE,
    HS,
    LO,
    MI,
    PL,
    VS,
    VC,
    HI,
    LS,
    GE,
    LT,
    GT,
    LE,
    AL,
}
pub const COND_CS: Condition = Condition::HS;
pub const COND_CC: Condition = Condition::LO;

#[derive(Copy, Clone)]
pub struct LinkRecord {
    from: i64,
    to: i64,
    cmp_reg: RegisterID,
    ty: JumpType,
    link_ty: JumpLinkType,
    condition: Condition,
    bit_number: u8,
    is_64bit: bool,
} // bits(N) VFPExpandImm(bits(8) imm8);
  //
  // Encoding of floating point immediates is a litte complicated. Here's a
  // high level description:
  //     +/-m*2-n where m and n are integers, 16 <= m <= 31, 0 <= n <= 7
  // and the algirithm for expanding to a single precision float:
  //     return imm8<7>:NOT(imm8<6>):Replicate(imm8<6>,5):imm8<5:0>:Zeros(19);
  //
  // The trickiest bit is how the exponent is handled. The following table
  // may help clarify things a little:
  //     654
  //     100 01111100 124 -3 1020 01111111100
  //     101 01111101 125 -2 1021 01111111101
  //     110 01111110 126 -1 1022 01111111110
  //     111 01111111 127  0 1023 01111111111
  //     000 10000000 128  1 1024 10000000000
  //     001 10000001 129  2 1025 10000000001
  //     010 10000010 130  3 1026 10000000010
  //     011 10000011 131  4 1027 10000000011
  // The first column shows the bit pattern stored in bits 6-4 of the arm
  // encoded immediate. The second column shows the 8-bit IEEE 754 single
  // -precision exponent in binary, the third column shows the raw decimal
  // value. IEEE 754 single-precision numbers are stored with a bias of 127
  // to the exponent, so the fourth column shows the resulting exponent.
  // From this was can see that the exponent can be in the range -3..4,
  // which agrees with the high level description given above. The fifth
  // and sixth columns shows the value stored in a IEEE 754 double-precision
  // number to represent these exponents in decimal and binary, given the
  // bias of 1023.
  //
  // Ultimately, detecting doubles that can be encoded as immediates on arm
  // and encoding doubles is actually not too bad. A floating point value can
  // be encoded by retaining the sign bit, the low three bits of the exponent
  // and the high 4 bits of the mantissa. To validly be able to encode an
  // immediate the remainder of the mantissa must be zero, and the high part
  // of the exponent must match the top bit retained, bar the highest bit
  // which must be its inverse
fn can_encode_fp_imm(d: f64) -> bool {
    // Discard the sign bit, the low two bits of the exponent & the highest
    // four bits of the mantissa.
    let masked = d.to_bits() & 0x7fc0ffffffffffffu64;
    return (masked == 0x3fc0000000000000u64) || (masked == 0x4000000000000000u64);
}
