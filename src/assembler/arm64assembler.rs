#![allow(non_upper_case_globals, unused_mut, unused_variables)]
use crate::assembler::assembler_common::{is_int, is_int9, is_uint12};
use std::{
    mem::size_of,
    ops::{Deref, DerefMut},
};

use super::{
    assembler_common::{
        is_valid_scaled_uimm12, is_valid_signed_imm9, ARM64LogicalImmediate, SIMDLane,
    },
    buffer::{AssemblerBuffer, AssemblerLabel},
};

const fn is_4byte_aligned(ptr: *const u8) -> bool {
    (unsafe { std::mem::transmute::<_, usize>(ptr) } & 0x3) == 0
}

#[inline(always)]
pub const fn is_int5(value: i32) -> bool {
    (value & !0x1f) == 0
}

pub struct UInt5(pub i32);

impl UInt5 {
    pub const fn new(value: i32) -> Self {
        assert!(is_int5(value));
        UInt5(value)
    }
}

impl Deref for UInt5 {
    type Target = i32;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for UInt5 {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Into<i32> for UInt5 {
    fn into(self) -> i32 {
        self.0
    }
}

impl Into<u32> for UInt5 {
    fn into(self) -> u32 {
        self.0 as u32
    }
}

pub struct UInt12(pub i32);

impl UInt12 {
    pub const fn new(value: i32) -> Self {
        assert!(is_uint12(value));
        UInt12(value)
    }
}

impl Deref for UInt12 {
    type Target = i32;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for UInt12 {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Into<i32> for UInt12 {
    fn into(self) -> i32 {
        self.0
    }
}

impl Into<u32> for UInt12 {
    fn into(self) -> u32 {
        self.0 as u32
    }
}

pub struct PostIndex(pub i32);

impl PostIndex {
    pub const fn new(value: i32) -> Self {
        assert!(is_int9(value));
        PostIndex(value)
    }
}

impl Deref for PostIndex {
    type Target = i32;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for PostIndex {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Into<i32> for PostIndex {
    fn into(self) -> i32 {
        self.0
    }
}

impl Into<u32> for PostIndex {
    fn into(self) -> u32 {
        self.0 as u32
    }
}

pub struct PreIndex(pub i32);

impl PreIndex {
    pub const fn new(value: i32) -> Self {
        assert!(is_int9(value));
        PreIndex(value)
    }
}

impl Deref for PreIndex {
    type Target = i32;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for PreIndex {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Into<i32> for PreIndex {
    fn into(self) -> i32 {
        self.0
    }
}

impl Into<u32> for PreIndex {
    fn into(self) -> u32 {
        self.0 as u32
    }
}

pub struct PairPostIndex(pub i32);

impl PairPostIndex {
    pub const fn new(value: i32) -> Self {
        assert!(is_int::<11>(value));
        PairPostIndex(value)
    }
}

impl Deref for PairPostIndex {
    type Target = i32;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for PairPostIndex {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Into<i32> for PairPostIndex {
    fn into(self) -> i32 {
        self.0
    }
}

impl Into<u32> for PairPostIndex {
    fn into(self) -> u32 {
        self.0 as u32
    }
}

pub type LogicalImmediate = ARM64LogicalImmediate;

pub fn get_half_word(value: u64, which: usize) -> u16 {
    debug_assert!(which < 4);
    (value >> (which << 4)) as u16
}

macro_rules! decl_gpr {
    (@internal $counter: expr, ($id: ident, $l: expr, $x: expr, $y: expr), $($rest: tt)*) => (
        pub const $id: u8 = $counter;
        decl_gpr!(@internal $counter + 1, $($rest)*);
    );
    (@internal $counter: expr,) => (
        pub const INVALID_GPR: u8 = 0xFF;
    );
    (($id: ident, $l: expr, $x: expr, $y: expr), $($rest: tt)*) => (
        decl_gpr!(@internal 0, ($id, $l, $x, $y), $($rest)*);
    );
}

for_each_gp_register!(decl_gpr);

macro_rules! decl_fp {
    (@internal $counter: expr, ($id: ident, $l: expr, $x: expr, $y: expr), $($rest: tt)*) => (
        pub const $id: u8 = $counter;
        decl_fp!(@internal $counter + 1, $($rest)*);
    );
    (@internal $counter: expr,) => (
        pub const INVALID_FPR: u8 = 0xFF;
    );
    (($id: ident, $l: expr, $x: expr, $y: expr), $($rest: tt)*) => (
        decl_fp!(@internal 0, ($id, $l, $x, $y), $($rest)*);
    );
}

for_each_fp_register!(decl_fp);

macro_rules! decl_alias {
    (@internal ($id: ident, $l: expr, $y: expr), $($rest: tt)*) => (
        pub const $id: u8 = $y;
        decl_alias!(@internal $($rest)*);
    );
    (@internal ) => (

    );
    (($id: ident, $l: expr, $y: expr), $($rest: tt)*) => (
        decl_alias!(@internal ($id, $l, $y), $($rest)*);
    );
}

for_each_register_alias!(decl_alias);

macro_rules! decl_sp {
    (@internal $counter: expr, ($id: ident, $l: expr), $($rest: tt)*) => (
        pub const $id: u8 = $counter;
        decl_sp!(@internal $counter + 1, $($rest)*);
    );
    (@internal $counter: expr,) => (
        pub const INVALID_SP: u8 = 0xFF;
    );
    (($id: ident, $l: expr), $($rest: tt)*) => (
        decl_sp!(@internal 0, ($id, $l), $($rest)*);
    );
}

for_each_sp_register!(decl_sp);

pub const fn is_sp(r: u8) -> bool {
    r == sp
}

pub const fn is_zr(r: u8) -> bool {
    r == zr
}

#[cfg(all(target_vendor = "apple", target_arch = "aarch64"))]
const MAX_POINTER_BITS: usize = 64;
#[cfg(not(all(target_vendor = "apple", target_arch = "aarch64")))]
const MAX_POINTER_BITS: usize = 48;

const NUMBER_OF_ADDRESS_ENCODING_INSTRUCTIONS: usize = MAX_POINTER_BITS / 16;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
#[repr(u8)]
pub enum Condition {
    EQ = 0,
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
    Invalid,
}

impl Condition {
    pub const CS: Self = Condition::HS;
    pub const CC: Self = Condition::LO;

    pub const fn invert(self) -> Self {
        unsafe { core::mem::transmute(self as u8 ^ 1) }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
#[repr(u8)]
pub enum ShiftType {
    LSL = 0,
    LSR,
    ASR,
    ROR,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
#[repr(u8)]
pub enum ExtendType {
    UXTB = 0,
    UXTH,
    UXTW,
    UXTX,
    SXTB,
    SXTH,
    SXTW,
    SXTX,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
#[repr(u8)]
pub enum SetFlags {
    DontSetFlags = 0,
    S,
}

const fn jump_with_size(index: u8, value: u8) -> u8 {
    (value << 4) | index
}

const fn jump_size(jump: u8) -> usize {
    jump as usize >> 4
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
#[repr(u8)]
pub enum JumpType {
    Fixed = jump_with_size(0, 0),
    NoCondition = jump_with_size(1, 1),
    Condition = jump_with_size(2, 2 * size_of::<u32>() as u8),
    CompareAndBranch = jump_with_size(3, 2 * size_of::<u32>() as u8),
    TestBit = jump_with_size(4, 2 * size_of::<u32>() as u8),
    NoConditionFixedSize = jump_with_size(5, 1 * size_of::<u32>() as u8),
    ConditionFixedSize = jump_with_size(6, 2 * size_of::<u32>() as u8),
    CompareAndBranchFixedSize = jump_with_size(7, 2 * size_of::<u32>() as u8),
    TestBitFixedSize = jump_with_size(8, 2 * size_of::<u32>() as u8),
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
#[repr(u8)]
pub enum JumpLinkType {
    Invalid = jump_with_size(0, 0),
    NoCondition = jump_with_size(1, 1 * size_of::<u32>() as u8),
    ConditionDirect = jump_with_size(2, 1 * size_of::<u32>() as u8),
    Condition = jump_with_size(3, 2 * size_of::<u32>() as u8),
    CompareAndBranch = jump_with_size(4, 2 * size_of::<u32>() as u8),
    CompareAndBranchDirect = jump_with_size(5, 1 * size_of::<u32>() as u8),
    TestBit = jump_with_size(6, 2 * size_of::<u32>() as u8),
    TestBitDirect = jump_with_size(7, 1 * size_of::<u32>() as u8),
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct LinkRecord {
    pub from: i64,
    pub to: i64,
    pub compare_register: u8,
    pub typ: JumpType,
    pub link_type: JumpLinkType,
    pub condition: Condition,
    pub bit_number: u8,
    pub is_64bit: bool,
}

impl LinkRecord {
    pub fn new_cond(from: isize, to: isize, typ: JumpType, cond: Condition) -> Self {
        Self {
            from: from as i64,
            to: to as i64,
            compare_register: INVALID_GPR,
            typ,
            link_type: JumpLinkType::Invalid,
            condition: cond,
            bit_number: 0,
            is_64bit: false,
        }
    }

    pub fn new_cmp(
        from: isize,
        to: isize,
        typ: JumpType,
        cond: Condition,
        is_64bit: bool,
        compare_register: u8,
    ) -> Self {
        Self {
            from: from as i64,
            to: to as i64,
            compare_register,
            typ,
            link_type: JumpLinkType::Invalid,
            condition: cond,
            bit_number: 0,
            is_64bit,
        }
    }

    pub fn new_test_bit(
        from: isize,
        to: isize,
        typ: JumpType,
        cond: Condition,
        bit_number: u8,
        compare_register: u8,
    ) -> Self {
        Self {
            from: from as i64,
            to: to as i64,
            compare_register,
            typ,
            link_type: JumpLinkType::Invalid,
            condition: cond,
            bit_number,
            is_64bit: false,
        }
    }

    pub fn from(&self) -> isize {
        self.from as isize
    }

    pub fn set_from(&mut self, from: isize) {
        self.from = from as i64;
    }

    pub fn to(&self) -> isize {
        self.to as isize
    }

    pub fn set_to(&mut self, to: isize) {
        self.to = to as i64;
    }
}

/// bits(N) VFPExpandImm(bits(8) imm8);
///
/// Encoding of floating point immediates is a litte complicated. Here's a
/// high level description:
///     +/-m*2-n where m and n are integers, 16 <= m <= 31, 0 <= n <= 7
/// and the algirithm for expanding to a single precision float:
///     return imm8<7>:NOT(imm8<6>):Replicate(imm8<6>,5):imm8<5:0>:Zeros(19);
///
/// The trickiest bit is how the exponent is handled. The following table
/// may help clarify things a little:
///     654
///     100 01111100 124 -3 1020 01111111100
///     101 01111101 125 -2 1021 01111111101
///     110 01111110 126 -1 1022 01111111110
///     111 01111111 127  0 1023 01111111111
///     000 10000000 128  1 1024 10000000000
///     001 10000001 129  2 1025 10000000001
///     010 10000010 130  3 1026 10000000010
///     011 10000011 131  4 1027 10000000011
/// The first column shows the bit pattern stored in bits 6-4 of the arm
/// encoded immediate. The second column shows the 8-bit IEEE 754 single
/// -precision exponent in binary, the third column shows the raw decimal
/// value. IEEE 754 single-precision numbers are stored with a bias of 127
/// to the exponent, so the fourth column shows the resulting exponent.
/// From this was can see that the exponent can be in the range -3..4,
/// which agrees with the high level description given above. The fifth
/// and sixth columns shows the value stored in a IEEE 754 double-precision
/// number to represent these exponents in decimal and binary, given the
/// bias of 1023.
///
/// Ultimately, detecting doubles that can be encoded as immediates on arm
/// and encoding doubles is actually not too bad. A floating point value can
/// be encoded by retaining the sign bit, the low three bits of the exponent
/// and the high 4 bits of the mantissa. To validly be able to encode an
/// immediate the remainder of the mantissa must be zero, and the high part
/// of the exponent must match the top bit retained, bar the highest bit
/// which must be its inverse.
pub fn can_encode_fp_imm(d: f64) -> bool {
    // Discard the sign bit, the low two bits of the exponent & the highest
    // four bits of the mantissa.
    let masked = d.to_bits() & 0x7fc0ffffffffffff;
    masked == 0x3fc0000000000000 || masked == 0x4000000000000000
}

const fn can_encode_pimm_offset<const DATASIZE: i32>(offset: i32) -> bool {
    is_valid_scaled_uimm12::<DATASIZE>(offset)
}

const fn can_encode_simm_offset(offset: i32) -> bool {
    is_valid_signed_imm9(offset)
}

pub fn encode_fp_imm(d: f64) -> i32 {
    let bits = d.to_bits();

    ((bits >> 56) as i32 & 0x80) | ((bits >> 48) as i32 & 0x7f)
}

pub fn encode_shift_amount<const DATASIZE: i32>(shift_amount: i32) -> i32 {
    assert!(shift_amount == 0 || DATASIZE == (8 << shift_amount));

    shift_amount
}

pub fn encode_positive_immediate<const DATASIZE: i32>(imm: usize) -> i32 {
    (imm / (DATASIZE as usize / 8)) as i32
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum Datasize {
    D32 = 0,
    D64 = 1,
    D128 = 2,
    D16 = 3,
}

const fn datasize(x: i32) -> Datasize {
    if x == 64 {
        Datasize::D64
    } else if x == 128 {
        Datasize::D128
    } else {
        Datasize::D32
    }
}

const fn set_flags(x: bool) -> SetFlags {
    if !x {
        SetFlags::DontSetFlags
    } else {
        SetFlags::S
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum MemOpSize {
    M8Or128 = 0,
    M16 = 1,
    M32 = 2,
    M64 = 3,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum BranchType {
    JMP,
    CALL,
    RET,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum AddOp {
    ADD,
    SUB,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum BitfieldOp {
    SBFM,
    BFM,
    UBFM,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum DataOp1Source {
    RBIT,
    REV16,
    REV32,
    REV64,
    CLZ,
    CLS,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum DataOp2Source {
    UDIV = 2,
    SDIV = 3,
    LSLV = 8,
    LSRV = 9,
    ASRV = 10,
    RORV = 11,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum DataOp3Source {
    MADD = 0,
    MSBU = 1,
    SMADDL = 2,
    SMSUBL = 3,
    SMULH = 4,
    UMADDL = 10,
    UMSUBL = 11,
    UMULH = 12,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum ExcepnOp {
    EXCEPTION = 0,
    BREAKPOINT = 1,
    HALT = 2,
    DCPS = 5,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum FPCmpOp {
    FCMP = 0x00,
    FCMP0 = 0x08,
    FCMPE = 0x10,
    FCMPE0 = 0x18,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum FPCondCmpOp {
    FCMP,
    FCMPE,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum FPDataOp1Source {
    FMOV = 0,
    FABS = 1,
    FNEG = 2,
    FSQRT = 3,
    FCVTToSingle = 4,
    FCVTToDouble = 5,
    FCVTToHalf = 7,
    FRINTN = 8,
    FRINTP = 9,
    FRINTM = 10,
    FRINTZ = 11,
    FRINTA = 12,
    FRINTX = 14,
    FRINTI = 15,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum FPDataOp2Source {
    FMUL,
    FDIV,
    FADD,
    FSUB,
    FMAX,
    FMIN,
    FMAXNM,
    FMINNM,
    FNMUL,
}
#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum SIMD3Same {
    LogicalOp = 0x03,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum SIMD3SameLogical {
    AND = 0x00,
    BIC = 0x01,
    ORR = 0x02,
    ORN = 0x03,
    EOR = 0x80,
    BSL = 0x81,
    BIT = 0x82,
    BIF = 0x83,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum FPIntConvOp {
    FCVTNS = 0x00,
    FCVTNU = 0x01,
    SCVTF = 0x02,
    UCVTF = 0x03,
    FCVTAS = 0x04,
    FCVTAU = 0x05,
    FMOVQ2X = 0x06,
    FMOVX2Q = 0x07,
    FCVTPS = 0x08,
    FCVTPU = 0x09,
    FMOVQ2XTOP = 0x0e,
    FMOVX2QTOP = 0x0f,
    FCVTMS = 0x10,
    FCVTMU = 0x11,
    FCVTZS = 0x18,
    FCVTZU = 0x19,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum LogicalOp {
    AND,
    ORR,
    EOR,
    ANDS,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum MemOp {
    STORE = 4,
    LOAD,
    STOREV128,
    LAODV128,
    PREFETCH = 2,
    LOADS32 = 3,
}

impl MemOp {
    pub const LOADS64: Self = Self::PREFETCH;
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum MemPairOpSize {
    S32 = 0,
    LOADS32 = 1,
    S64 = 2,
}

impl MemPairOpSize {
    pub const V32: Self = Self::S32;
    pub const V64: Self = Self::LOADS32;
    pub const V128: Self = Self::S64;
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum MoveWideOp {
    N = 0,
    Z = 2,
    K = 3,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum LdrLiteralOp {
    S32 = 0,
    S64 = 1,
    LDRSW = 2,
}

impl LdrLiteralOp {
    pub const S128: Self = Self::LDRSW;
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum ExoticLoadFence {
    None,
    Acquire,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum ExoticLoadAtomic {
    Link,
    None,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum ExoticStoreFence {
    None,
    Release,
}

const fn mem_pair_offset_shift(v: bool, size: MemPairOpSize) -> usize {
    if v {
        size as usize + 2
    } else {
        (size as usize >> 2) + 2
    }
}

pub struct ARM64Assembler {
    buffer: AssemblerBuffer,
    index_of_last_watchpoint: usize,
    index_of_tail_of_last_watchpoint: usize,
    jumps_to_link: Vec<LinkRecord>,
}

impl ARM64Assembler {
    pub fn new() -> Self {
        ARM64Assembler {
            buffer: AssemblerBuffer::new(),
            index_of_last_watchpoint: 0,
            index_of_tail_of_last_watchpoint: 0,
            jumps_to_link: Vec::new(),
        }
    }

    pub const fn first_register() -> u8 {
        x0
    }

    pub const fn last_register() -> u8 {
        sp
    }

    pub const fn number_of_registers() -> u8 {
        Self::last_register() - Self::first_register() + 1
    }

    pub const fn first_sp_register() -> u8 {
        pc
    }

    pub const fn last_sp_register() -> u8 {
        fpsr
    }

    pub const fn number_of_sp_registers() -> u8 {
        Self::last_sp_register() - Self::first_sp_register() + 1
    }

    pub const fn first_fp_register() -> u8 {
        q0
    }

    pub const fn last_fp_register() -> u8 {
        q31
    }

    pub const fn number_of_fp_registers() -> u8 {
        Self::last_fp_register() - Self::first_fp_register() + 1
    }

    pub const fn gpr_name(r: u8) -> &'static str {
        match r {
            x0 => "x0",
            x1 => "x1",
            x2 => "x2",
            x3 => "x3",
            x4 => "x4",
            x5 => "x5",
            x6 => "x6",
            x7 => "x7",
            x8 => "x8",
            x9 => "x9",
            x10 => "x10",
            x11 => "x11",
            x12 => "x12",
            x13 => "x13",
            x14 => "x14",
            x15 => "x15",
            x16 => "x16",
            x17 => "x17",
            x18 => "x18",
            x19 => "x19",
            x20 => "x20",
            x21 => "x21",
            x22 => "x22",
            x23 => "x23",
            x24 => "x24",
            x25 => "x25",
            x26 => "x26",
            x27 => "x27",
            x28 => "x28",
            fp => "fp",
            lr => "lr",
            sp => "sp",
            _ => unreachable!(),
        }
    }

    pub const fn spr_name(r: u8) -> &'static str {
        match r {
            pc => "pc",
            nzcv => "nzcv",
            fpsr => "fpsr",
            _ => unreachable!(),
        }
    }

    pub const fn frp_name(r: u8) -> &'static str {
        match r {
            q0 => "q0",
            q1 => "q1",
            q2 => "q2",
            q3 => "q3",
            q4 => "q4",
            q5 => "q5",
            q6 => "q6",
            q7 => "q7",
            q8 => "q8",
            q9 => "q9",
            q10 => "q10",
            q11 => "q11",
            q12 => "q12",
            q13 => "q13",
            q14 => "q14",
            q15 => "q15",
            q16 => "q16",
            q17 => "q17",
            q18 => "q18",
            q19 => "q19",
            q20 => "q20",
            q21 => "q21",
            q22 => "q22",
            q23 => "q23",
            q24 => "q24",
            q25 => "q25",
            q26 => "q26",
            q27 => "q27",
            q28 => "q28",
            q29 => "q29",
            q30 => "q30",
            q31 => "q31",
            _ => unreachable!(),
        }
    }

    pub fn buffer(&self) -> &AssemblerBuffer {
        &self.buffer
    }

    pub fn buffer_mut(&mut self) -> &mut AssemblerBuffer {
        &mut self.buffer
    }

    pub unsafe fn link_pointer(code: *mut u8, where_: AssemblerLabel, to: *mut u8) {
        todo!()
    }

    pub unsafe fn link_call(code: *mut u8, where_: AssemblerLabel, to: *mut u8) {
        todo!()
    }

    pub unsafe fn link_tail_call(code: *mut u8, where_: AssemblerLabel, to: *mut u8) {
        todo!()
    }

    pub unsafe fn link_jump_(code: *mut u8, where_: AssemblerLabel, to: *mut u8) {
        todo!()
    }

    pub fn label_for_watchpoint(&mut self) -> AssemblerLabel {
        let mut result = self.buffer.label();

        if result.offset() != self.index_of_tail_of_last_watchpoint as u32 {
            result = self.buffer.label();
        }

        self.index_of_last_watchpoint = result.offset() as _;
        self.index_of_tail_of_last_watchpoint = result.offset() as usize;

        result
    }

    pub fn label_ignoring_watchpoints(&mut self) -> AssemblerLabel {
        self.buffer.label()
    }

    pub fn label(&mut self) -> AssemblerLabel {
        self.buffer.label()
    }

    pub fn align(&mut self, alignment: usize) -> AssemblerLabel {
        self.buffer.label()
    }

    pub fn breakpoint(&mut self) {
        todo!()
    }

    #[allow(unused_mut)]
    pub unsafe fn fill_nops(base: *mut u8, mut size: usize) {
        todo!()
    }

    pub unsafe fn replace_with_address_computation(mut instruction_start: *mut u8) {
        todo!()
    }

    pub unsafe fn replace_with_load(mut instruction_start: *mut u8) {
        todo!()
    }

    pub unsafe fn read_pointer(ip: *mut u8) -> *mut u8 {
        let address = ip.cast::<i32>();

        todo!()
    }

    pub unsafe fn repatch_pointer(where_: *mut u8, to: *mut u8) {
        todo!()
    }

    pub unsafe fn repatch_int32(where_: *mut u8, value: i32) {
        todo!()
    }

    pub unsafe fn relink_jump(from: *mut u8, to: *mut u8) {
        todo!()
    }

    pub unsafe fn relink_tail_call(from: *mut u8, to: *mut u8) {
        todo!()
    }

    pub unsafe fn relink_call(from: *mut u8, to: *mut u8) {
        todo!()
    }

    pub fn get_call_return_offset(call: AssemblerLabel) -> usize {
        call.offset() as _
    }

    pub unsafe fn get_relocate_address(code: *mut u8, label: AssemblerLabel) -> *mut u8 {
        code.add(label.offset() as _)
    }

    pub fn link_jump(&mut self, from: AssemblerLabel, to: AssemblerLabel) {
        todo!()
    }

    pub fn debug_offset(&self) -> usize {
        self.buffer.debug_offset()
    }

    pub fn code_size(&self) -> usize {
        self.buffer.code_size()
    }

    pub fn insn(&mut self, instruction: i32) {
        self.buffer.put_i32(instruction);
    }

    pub fn adc<const DATASIZE: i32, const SET_FLAGS: bool>(&mut self, rd: u8, rn: u8, rm: u8) {
        self.insn(Self::add_subtract_with_carry(
            datasize(DATASIZE),
            AddOp::ADD,
            set_flags(SET_FLAGS),
            rm,
            rn,
            rd,
        ))
    }

    pub fn add_imm<const DATASIZE: i32, const SET_FLAGS: bool>(
        &mut self,
        rd: u8,
        rn: u8,
        imm12: UInt12,
        shift: i32,
    ) {
        self.insn(Self::add_subtract_immediate(
            datasize(DATASIZE),
            AddOp::ADD,
            set_flags(SET_FLAGS),
            shift,
            imm12.0,
            rn,
            rd,
        ))
    }

    pub fn add_extend<const DATASIZE: i32, const SET_FLAGS: bool>(
        &mut self,
        rd: u8,
        rn: u8,
        rm: u8,
        extend: ExtendType,
        amount: i32,
    ) {
        self.insn(Self::add_subtract_extended_register(
            datasize(DATASIZE),
            AddOp::ADD,
            set_flags(SET_FLAGS),
            rm,
            extend,
            amount,
            rn,
            rd,
        ))
    }

    pub fn add_shifted<const DATASIZE: i32, const SET_FLAGS: bool>(
        &mut self,
        rd: u8,
        rn: u8,
        rm: u8,
        shift: ShiftType,
        amount: i32,
    ) {
        if is_sp(rd) || is_sp(rn) {
            assert!(shift == ShiftType::LSL);
            assert!(!is_sp(rm));

            return self.add_extend::<DATASIZE, SET_FLAGS>(rd, rn, rm, ExtendType::UXTX, amount);
        }
        self.insn(Self::add_subtract_shifted_registers(
            datasize(DATASIZE),
            AddOp::ADD,
            set_flags(SET_FLAGS),
            shift,
            rm,
            amount,
            rn,
            rd,
        ))
    }

    pub fn add<const DATASIZE: i32, const SET_FLAGS: bool>(
        &mut self,
        rd: u8,
        rn: u8,
        rm: u8,
        
    ) {
        self.add_shifted::<DATASIZE, SET_FLAGS>(rd, rn, rm, ShiftType::LSL, 0)
    }

    pub fn adr(&mut self, rd: u8, offset: i32) {
        self.insn(Self::pc_relative(false, offset, rd))
    }

    pub fn adrp(&mut self, rd: u8, offset: i32) {
        self.insn(Self::pc_relative(true, offset >> 12, rd))
    }

    pub fn and_shifted<const DATASIZE: i32, const SET_FLAGS: bool>(
        &mut self,
        rd: u8,
        rn: u8,
        rm: u8,
        shift: ShiftType,
        amount: i32,
    ) {
        self.insn(Self::logical_shifted_register(
            datasize(DATASIZE),
            if SET_FLAGS {
                LogicalOp::ANDS
            } else {
                LogicalOp::AND
            },
            shift,
            false,
            rm,
            amount,
            rn,
            rd,
        ))
    }

    pub fn and<const DATASIZE: i32, const SET_FLAGS: bool>(&mut self, rd: u8, rn: u8, rm: u8) {
        self.and_shifted::<DATASIZE, SET_FLAGS>(rd, rn, rm, ShiftType::LSL, 0)
    }

    pub fn and_logical_immediate<const DATASIZE: i32, const SET_FLAGS: bool>(
        &mut self,
        rd: u8,
        rn: u8,
        imm12: LogicalImmediate,
    ) {
        self.insn(Self::logical_immediate(
            datasize(DATASIZE),
            if SET_FLAGS {
                LogicalOp::ANDS
            } else {
                LogicalOp::AND
            },
            imm12.value(),
            rn,
            rd,
        ))
    }

    pub fn asr_shifted<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, shift: i32) {
        self.sbfm::<DATASIZE>(rd, rn, shift, DATASIZE - 1)
    }

    pub fn asrv<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, rm: u8) {
        self.insn(Self::data_processing_2_source(
            datasize(DATASIZE),
            rm,
            DataOp2Source::ASRV,
            rn,
            rd,
        ))
    }

    pub fn asr<const DATASIZE: i32, const SET_FLAGS: bool>(&mut self, rd: u8, rn: u8, rm: u8) {
        self.asrv::<DATASIZE>(rd, rn, rm);
    }

    pub fn b(&mut self) {
        self.insn(Self::unconditional_branch_immediate(false, 0))
    }

    pub fn b_cond(&mut self, cond: Condition, mut offset: i32) {
        offset >>= 2;

        self.insn(Self::conditional_branch_immediate(offset, cond))
    }

    pub fn bfm<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, immr: i32, imms: i32) {
        self.insn(Self::bitfield(
            datasize(DATASIZE),
            BitfieldOp::BFM,
            immr,
            imms,
            rn,
            rd,
        ))
    }

    pub fn bfi<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, lsb: i32, width: i32) {
        self.bfm::<DATASIZE>(
            rd,
            rn,
            DATASIZE.wrapping_sub(lsb) & (DATASIZE - 1),
            width - 1,
        )
    }

    pub fn bfc<const DATASIZE: i32>(&mut self, rd: u8, lsb: i32, width: i32) {
        self.bfm::<DATASIZE>(rd, zr, lsb, width - 1)
    }

    pub fn bfxil<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, lsb: i32, width: i32) {
        self.bfm::<DATASIZE>(rd, rn, lsb, lsb + width - 1)
    }

    pub fn bic_shifted<const DATASIZE: i32, const SET_FLAGS: bool>(
        &mut self,
        rd: u8,
        rn: u8,
        rm: u8,
        shift: ShiftType,
        amount: i32,
    ) {
        self.insn(Self::logical_shifted_register(
            datasize(DATASIZE),
            if SET_FLAGS {
                LogicalOp::ANDS
            } else {
                LogicalOp::AND
            },
            shift,
            true,
            rm,
            amount,
            rn,
            rd,
        ))
    }

    pub fn bic<const DATASIZE: i32, const SET_FLAGS: bool>(&mut self, rd: u8, rn: u8, rm: u8) {
        self.bic_shifted::<DATASIZE, SET_FLAGS>(rd, rn, rm, ShiftType::LSL, 0)
    }

    pub fn bl(&mut self) {
        self.insn(Self::unconditional_branch_immediate(true, 0))
    }

    pub fn blr(&mut self, rn: u8) {
        self.insn(Self::unconditional_branch_register(BranchType::CALL, rn));
    }

    pub fn br(&mut self, rn: u8) {
        self.insn(Self::unconditional_branch_register(BranchType::JMP, rn));
    }

    pub fn brk(&mut self, imm: u16) {
        self.insn(Self::excepn_generation(ExcepnOp::BREAKPOINT, imm, 0))
    }

    pub unsafe fn is_brk(addr: *const u8) -> bool {
        let expected = Self::excepn_generation(ExcepnOp::BREAKPOINT, 0, 0);
        let immediate_mask = Self::excepn_generation_imm_mask();
        let candidate = addr.cast::<i32>().read();

        (candidate & !immediate_mask) == expected
    }

    pub fn cbnz<const DATASIZE: i32>(&mut self, rt: u8, mut offset: i32) {
        offset >>= 2;

        self.insn(Self::compare_and_branch_immediate(
            datasize(DATASIZE),
            true,
            offset,
            rt,
        ))
    }

    pub fn cbz<const DATASIZE: i32>(&mut self, rt: u8, mut offset: i32) {
        offset >>= 2;

        self.insn(Self::compare_and_branch_immediate(
            datasize(DATASIZE),
            false,
            offset,
            rt,
        ))
    }

    pub fn ccmn_imm<const DATASIZE: i32>(
        &mut self,
        rn: u8,
        imm: UInt5,
        _nzcv: i32,
        cond: Condition,
    ) {
        self.insn(Self::conditional_compare_immediate(
            datasize(DATASIZE),
            AddOp::ADD,
            imm.0,
            cond,
            rn,
            _nzcv
        ))
    }

    pub fn ccmn<const DATASIZE: i32>(&mut self, rn: u8, rm: u8, _nzcv: i32, cond: Condition) {
        self.insn(Self::conditional_compare_register(
            datasize(DATASIZE),
            AddOp::ADD,
            rm,
            cond,
            rn,
            _nzcv
        ));
    }

    pub fn ccmp_imm<const DATASIZE: i32>(
        &mut self,
        rn: u8,
        imm: UInt5,
        _nzcv: i32,
        cond: Condition,
    ) {
        self.insn(Self::conditional_compare_immediate(
            datasize(DATASIZE),
            AddOp::SUB,
            imm.0,
            cond,
            rn,
            _nzcv
        ))
    }

    pub fn ccmp<const DATASIZE: i32>(&mut self, rn: u8, rm: u8, _nzcv: i32, cond: Condition) {
        self.insn(Self::conditional_compare_register(
            datasize(DATASIZE),
            AddOp::SUB,
            rm,
            cond,
            rn,
            _nzcv
        ));
    }



    pub fn sbfm<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, immr: i32, imms: i32) {
        self.insn(Self::bitfield(
            datasize(DATASIZE),
            BitfieldOp::SBFM,
            immr,
            imms,
            rn,
            rd,
        ))
    }

    pub fn ret(&mut self, rn: u8) {
        self.insn(Self::unconditional_branch_register(BranchType::RET, rn))
    }

    const fn x_or_sp(r: u8) -> i32 {
        r as i32
    }

    const fn x_or_zr(r: u8) -> i32 {
        r as i32 & 31
    }

    const fn x_or_zr_or_sp(r: u8, use_zr: bool) -> i32 {
        if use_zr {
            Self::x_or_zr(r)
        } else {
            Self::x_or_sp(r)
        }
    }

    const fn add_subtract_extended_register(
        sf: Datasize,
        op: AddOp,
        s: SetFlags,
        rm: u8,
        option: ExtendType,
        imm3: i32,
        rn: u8,
        rd: u8,
    ) -> i32 {
        let opt = 0;
        0x0b200000
            | (sf as i32) << 31
            | (op as i32) << 30
            | (s as i32) << 22
            | Self::x_or_zr(rm) << 16
            | (option as i32) << 13
            | (imm3 & 0x7) << 10
            | Self::x_or_sp(rn) << 5
            | Self::x_or_zr_or_sp(rd, s as u8 == SetFlags::S as u8)
    }

    const fn add_subtract_immediate(
        sf: Datasize,
        op: AddOp,
        s: SetFlags,
        shift: i32,
        imm12: i32,
        rn: u8,
        rd: u8,
    ) -> i32 {
        0x11000000
            | (sf as i32) << 31
            | (op as i32) << 30
            | (s as i32) << 29
            | shift << 22
            | (imm12 & 0xfff) << 10
            | Self::x_or_sp(rn) << 5
            | Self::x_or_zr_or_sp(rd, s as u8 == SetFlags::S as u8)
    }

    const fn add_subtract_shifted_registers(
        sf: Datasize,
        op: AddOp,
        s: SetFlags,
        shift: ShiftType,
        rm: u8,
        imm6: i32,
        rn: u8,
        rd: u8,
    ) -> i32 {
        0x0b000000
            | (sf as i32) << 31
            | (op as i32) << 30
            | (s as i32) << 29
            | (shift as i32) << 22
            | Self::x_or_zr(rm) << 16
            | (imm6 & 0x3f) << 10
            | Self::x_or_zr(rn) << 5
            | Self::x_or_zr(rd)
    }

    const fn add_subtract_with_carry(
        sf: Datasize,
        op: AddOp,
        s: SetFlags,
        rm: u8,
        rn: u8,
        rd: u8,
    ) -> i32 {
        0x1a000000
            | (sf as i32) << 31
            | (op as i32) << 30
            | (s as i32) << 29
            | Self::x_or_zr(rm) << 16
            | 0 << 10
            | Self::x_or_zr(rn) << 5
            | Self::x_or_zr(rd)
    }

    const fn bitfield(sf: Datasize, opc: BitfieldOp, immr: i32, imms: i32, rn: u8, rd: u8) -> i32 {
        let n = sf as i32;

        0x13000000
            | (sf as i32) << 31
            | (opc as i32) << 29
            | n << 22
            | immr << 16
            | imms << 10
            | Self::x_or_zr(rn) << 5
            | Self::x_or_zr(rd)
    }

    const fn compare_and_branch_immediate(sf: Datasize, op: bool, imm19: i32, rt: u8) -> i32 {
        0x34000000
            | (sf as i32) << 31
            | (op as i32) << 24
            | (imm19 & 0x7ffff) << 5
            | Self::x_or_zr(rt)
    }

    const fn conditional_branch_immediate(imm19: i32, cond: Condition) -> i32 {
        0x54000000 | 0 << 24 | (imm19 & 0x7ffff) << 5 | 0 << 4 | (cond as i32)
    }

    const fn conditional_compare_immediate(
        sf: Datasize,
        op: AddOp,
        imm5: i32,
        cond: Condition,
        rn: u8,
        _nzcv: i32,
    ) -> i32 {
        let s = 1;
        let o2 = 0;
        let o3 = 0;
        0x1a400800
            | (sf as i32) << 31
            | (op as i32) << 30
            | s << 29
            | (imm5 & 0x1f) << 16
            | (cond as i32) << 12
            | o2 << 10
            | Self::x_or_zr(rn) << 5
            | o3 << 4
            | _nzcv
    }

    const fn conditional_compare_register(
        sf: Datasize,
        op: AddOp,
        rm: u8,
        cond: Condition,
        rn: u8,
        _nzcv: i32,
    ) -> i32 {
        let s = 1;
        let o2 = 0;
        let o3 = 0;
        0x1a400000
            | (sf as i32) << 31
            | (op as i32) << 30
            | s << 29
            | Self::x_or_zr(rm) << 16
            | (cond as i32) << 12
            | o2 << 10
            | Self::x_or_zr(rn) << 5
            | o3 << 4
            | _nzcv
    }

    const fn conditional_select(
        sf: Datasize,
        op: bool,
        rm: u8,
        cond: Condition,
        op2: bool,
        rn: u8,
        rd: u8,
    ) -> i32 {
        let s = 0;
        0x1a800000
            | (sf as i32) << 31
            | (op as i32) << 30
            | s << 29
            | Self::x_or_zr(rm) << 16
            | (cond as i32) << 12
            | (op2 as i32) << 10
            | Self::x_or_zr(rn) << 5
            | Self::x_or_zr(rd)
    }

    const fn data_processing_1_source(sf: Datasize, opcode: DataOp1Source, rn: u8, rd: u8) -> i32 {
        let s = 0;
        let opcode2 = 0;
        0x5ac00000
            | (sf as i32) << 31
            | s << 29
            | opcode2 << 16
            | (opcode as i32) << 10
            | Self::x_or_zr(rn) << 5
            | Self::x_or_zr(rd)
    }

    const fn data_processing_2_source(
        sf: Datasize,
        rm: u8,
        opcode: DataOp2Source,
        rn: u8,
        rd: u8,
    ) -> i32 {
        let s = 0;
        0x1ac00000
            | (sf as i32) << 31
            | s << 29
            | Self::x_or_zr(rm) << 16
            | (opcode as i32) << 10
            | Self::x_or_zr(rn) << 5
            | Self::x_or_zr(rd)
    }

    const fn data_processing_3_source(
        sf: Datasize,
        opcode: DataOp3Source,
        rm: u8,
        ra: u8,
        rn: u8,
        rd: u8,
    ) -> i32 {
        let op54 = opcode as i32 >> 4;
        let op31 = (opcode as i32 >> 1) & 7;
        let op0 = opcode as i32 & 1;

        0x1b000000
            | (sf as i32) << 31
            | op54 << 29
            | op31 << 21
            | Self::x_or_zr(rm) << 16
            | op0 << 15
            | Self::x_or_zr(ra) << 10
            | Self::x_or_zr(rn) << 5
            | Self::x_or_zr(rd)
    }

    const fn excepn_generation(opc: ExcepnOp, imm16: u16, ll: i32) -> i32 {
        let op2 = 0;

        0xd4000000u32 as i32 | (opc as i32) << 21 | (imm16 as i32) << 5 | op2 << 2 | ll
    }

    const fn excepn_generation_imm_mask() -> i32 {
        (u16::MAX as i32) << 5
    }

    const fn extract(sf: Datasize, rm: u8, imms: i32, rn: u8, rd: u8) -> i32 {
        let op21 = 0;
        let n = sf as i32;
        let o0 = 0;

        0x13800000
            | (sf as i32) << 31
            | op21 << 29
            | n << 22
            | o0 << 21
            | Self::x_or_zr(rm) << 16
            | imms << 10
            | Self::x_or_zr(rn) << 5
            | Self::x_or_zr(rd)
    }

    const fn floating_point_compare(typ: Datasize, rm: u8, rn: u8, opcode2: FPCmpOp) -> i32 {
        let m = 0;
        let s = 0;
        let op = 0;

        0x1e202000
            | m << 31
            | s << 29
            | (typ as i32) << 22
            | (rm as i32) << 16
            | op << 14
            | (rn as i32) << 5
            | opcode2 as i32
    }

    const fn floating_point_conditional_compare(
        typ: Datasize,
        rm: u8,
        cond: Condition,
        rn: u8,
        op: FPCondCmpOp,
        _nzcv: i32,
    ) -> i32 {
        let m = 0;
        let s = 0;

        0x1e200400
            | m << 31
            | s << 29
            | (typ as i32) << 22
            | (rm as i32) << 16
            | (cond as i32) << 12
            | (rn as i32) << 5
            | (op as i32) << 4
            | _nzcv
    }

    const fn floating_point_conditional_select(
        typ: Datasize,
        rm: u8,
        cond: Condition,
        rn: u8,
        rd: u8,
    ) -> i32 {
        let m = 0;
        let s = 0;

        0x1e200c00
            | m << 31
            | s << 29
            | (typ as i32) << 22
            | (rm as i32) << 16
            | (cond as i32) << 12
            | (rn as i32) << 5
            | (rd as i32)
    }

    const fn floating_point_immediate(typ: Datasize, imm8: i32, rd: u8) -> i32 {
        let m = 0;
        let s = 0;
        let imm5 = 0;

        0x1e201000
            | m << 31
            | s << 29
            | (typ as i32) << 22
            | (imm8 & 0xff) << 13
            | imm5 << 10
            | (rd as i32)
    }

    const fn floating_point_integer_conversion_f2f(
        sf: Datasize,
        typ: Datasize,
        rmode_opcode: FPIntConvOp,
        rn: u8,
        rd: u8,
    ) -> i32 {
        let s = 0;

        0x1e200000
            | (sf as i32) << 31
            | s << 29
            | (typ as i32) << 22
            | (rmode_opcode as i32) << 16
            | (rn as i32) << 5
            | (rd as i32)
    }

    const fn floating_point_integer_conversion_f2i(
        sf: Datasize,
        typ: Datasize,
        rmode_opcode: FPIntConvOp,
        rn: u8,
        rd: u8,
    ) -> i32 {
        Self::floating_point_integer_conversion_f2f(
            sf,
            typ,
            rmode_opcode,
            rn,
            Self::x_or_zr(rd) as _,
        )
    }

    const fn floating_point_integer_conversion_i2f(
        sf: Datasize,
        typ: Datasize,
        rmode_opcode: FPIntConvOp,
        rn: u8,
        rd: u8,
    ) -> i32 {
        Self::floating_point_integer_conversion_f2f(
            sf,
            typ,
            rmode_opcode,
            Self::x_or_zr(rn) as _,
            rd,
        )
    }

    const fn floating_point_data_processing_1_source(
        typ: Datasize,
        opcode: FPDataOp1Source,
        rn: u8,
        rd: u8,
    ) -> i32 {
        let m = 0;
        let s = 0;
        0x1e204000
            | m << 31
            | s << 29
            | (typ as i32) << 22
            | (opcode as i32) << 15
            | (rn as i32) << 5
            | (rd as i32)
    }

    const fn floating_point_data_processing_2_source(
        typ: Datasize,
        opcode: FPDataOp2Source,
        rm: u8,
        rn: u8,
        rd: u8,
    ) -> i32 {
        let m = 0;
        let s = 0;
        0x1e200800
            | m << 31
            | s << 29
            | (typ as i32) << 22
            | (rm as i32) << 26
            | (opcode as i32) << 12
            | (rn as i32) << 5
            | (rd as i32)
    }

    const fn vector_data_processing_logical(
        datasize: i32,
        u_and_size: SIMD3SameLogical,
        vm: u8,
        vn: u8,
        vd: u8,
    ) -> i32 {
        let q = datasize == 128;

        0xe200400
            | (q as i32) << 30
            | (u_and_size as i32) << 29
            | (vm as i32) << 16
            | (SIMD3Same::LogicalOp as i32) << 11
            | (vn as i32) << 5
            | (vd as i32)
    }

    const fn floating_point_data_processing_3_source(
        typ: Datasize,
        o1: bool,
        rm: u8,
        o2: AddOp,
        ra: u8,
        rn: u8,
        rd: u8,
    ) -> i32 {
        let m = 0;
        let s = 0;

        0x1f000000
            | m << 31
            | s << 29
            | (typ as i32) << 22
            | (o1 as i32) << 21
            | (rm as i32) << 16
            | (o2 as i32) << 15
            | (ra as i32) << 10
            | (rn as i32) << 5
            | (rd as i32)
    }

    const fn load_register_literal(opc: LdrLiteralOp, v: bool, imm19: i32, rt: u8) -> i32 {
        0x18000000 | (opc as i32) << 30 | (v as i32) << 26 | (imm19 & 0x7ffff) << 5 | (rt as i32)
    }

    const fn load_register_literal_gp(opc: LdrLiteralOp, v: bool, imm19: i32, rt: u8) -> i32 {
        Self::load_register_literal(opc, v, imm19, Self::x_or_zr(rt) as _)
    }

    const fn load_store_register_post_index(
        size: MemOpSize,
        v: bool,
        opc: MemOp,
        imm9: i32,
        rn: u8,
        rt: u8,
    ) -> i32 {
        0x38000400
            | (size as i32) << 30
            | (v as i32) << 26
            | (opc as i32) << 22
            | (imm9 & 0x1ff) << 12
            | Self::x_or_sp(rn) << 5
            | (rt as i32)
    }

    const fn load_store_register_post_index_gp(
        size: MemOpSize,
        v: bool,
        opc: MemOp,
        imm9: i32,
        rn: u8,
        rt: u8,
    ) -> i32 {
        Self::load_store_register_post_index(size, v, opc, imm9, rn, Self::x_or_zr(rt) as _)
    }

    const fn load_store_register_pair_post_index(
        size: MemPairOpSize,
        v: bool,
        opc: MemOp,
        immediate: i32,
        rn: u8,
        rt: u8,
        rt2: u8,
    ) -> i32 {
        let immed_shift_amount = mem_pair_offset_shift(v, size);
        let imm7 = immediate >> immed_shift_amount;

        0x28800000
            | (size as i32) << 30
            | (v as i32) << 26
            | (opc as i32) << 22
            | (imm7 & 0x7f) << 15
            | (rt2 as i32) << 10
            | Self::x_or_sp(rn) << 5
            | (rt as i32)
    }

    const fn load_store_register_pair_post_index_gp(
        size: MemPairOpSize,
        v: bool,
        opc: MemOp,
        immediate: i32,
        rn: u8,
        rt: u8,
        rt2: u8,
    ) -> i32 {
        Self::load_store_register_pair_post_index(
            size,
            v,
            opc,
            immediate,
            rn,
            Self::x_or_zr(rt) as _,
            Self::x_or_zr(rt2) as _,
        )
    }

    const fn load_store_register_pair_pre_index(
        size: MemOpSize,
        v: bool,
        opc: MemOp,
        imm9: i32,
        rn: u8,
        rt: u8,
        rt2: u8,
    ) -> i32 {
        0x38000c00
            | (size as i32) << 30
            | (v as i32) << 26
            | (opc as i32) << 22
            | (imm9 & 0x1ff) << 12
            | Self::x_or_sp(rn) << 5
            | (rt as i32)
    }

    const fn load_store_register_pair_pre_index_gp(
        size: MemOpSize,
        v: bool,
        opc: MemOp,
        imm9: i32,
        rn: u8,
        rt: u8,
        rt2: u8,
    ) -> i32 {
        Self::load_store_register_pair_pre_index(
            size,
            v,
            opc,
            imm9,
            rn,
            Self::x_or_zr(rt) as _,
            Self::x_or_zr(rt2) as _,
        )
    }

    const fn load_store_register_pair_offset(
        size: MemPairOpSize,
        v: bool,
        opc: MemOp,
        immediate: i32,
        rn: u8,
        rt: u8,
        rt2: u8,
    ) -> i32 {
        let imm_shift = mem_pair_offset_shift(v, size);
        let imm7 = immediate >> imm_shift;

        0x29000000
            | (size as i32) << 30
            | (v as i32) << 26
            | (opc as i32) << 22
            | (imm7 & 0x7f) << 15
            | (rt2 as i32) << 10
            | Self::x_or_sp(rn) << 5
            | (rt as i32)
    }

    const fn load_store_register_pair_offset_gp(
        size: MemPairOpSize,
        v: bool,
        opc: MemOp,
        immediate: i32,
        rn: u8,
        rt: u8,
        rt2: u8,
    ) -> i32 {
        Self::load_store_register_pair_offset(
            size,
            v,
            opc,
            immediate,
            rn,
            Self::x_or_zr(rt) as _,
            Self::x_or_zr(rt2) as _,
        )
    }

    const fn load_store_register_pair_non_temporal(
        size: MemPairOpSize,
        v: bool,
        opc: MemOp,
        immediate: i32,
        rn: u8,
        rt: u8,
        rt2: u8,
    ) -> i32 {
        let imm_shift = mem_pair_offset_shift(v, size);
        let imm7 = immediate >> imm_shift;

        0x28000000
            | (size as i32) << 30
            | (v as i32) << 26
            | (opc as i32) << 22
            | (imm7 & 0x7f) << 15
            | (rt2 as i32) << 10
            | Self::x_or_sp(rn) << 5
            | (rt as i32)
    }

    const fn load_store_register_pair_non_temporal_gp(
        size: MemPairOpSize,
        v: bool,
        opc: MemOp,
        immediate: i32,
        rn: u8,
        rt: u8,
        rt2: u8,
    ) -> i32 {
        Self::load_store_register_pair_non_temporal(
            size,
            v,
            opc,
            immediate,
            rn,
            Self::x_or_zr(rt) as _,
            Self::x_or_zr(rt2) as _,
        )
    }

    const fn load_store_register_offset(
        size: MemOpSize,
        v: bool,
        opc: MemOp,
        rm: u8,
        option: ExtendType,
        s: bool,
        rn: u8,
        rt: u8,
    ) -> i32 {
        0x38200800
            | (size as i32) << 30
            | (v as i32) << 26
            | (opc as i32) << 22
            | Self::x_or_zr(rm) << 16
            | (option as i32) << 13
            | (s as i32) << 12
            | Self::x_or_sp(rn) << 5
            | (rt as i32)
    }

    const fn load_store_register_offset_gp(
        size: MemOpSize,
        v: bool,
        opc: MemOp,
        rm: u8,
        option: ExtendType,
        s: bool,
        rn: u8,
        rt: u8,
    ) -> i32 {
        Self::load_store_register_offset(
            size,
            v,
            opc,
            Self::x_or_zr(rm) as _,
            option,
            s,
            rn,
            Self::x_or_zr(rt) as _,
        )
    }

    const fn load_store_register_unscaled_immediate(
        size: MemOpSize,
        v: bool,
        opc: MemOp,
        imm9: i32,
        rn: u8,
        rd: u8,
    ) -> i32 {
        0x38000000
            | (size as i32) << 30
            | (v as i32) << 26
            | (opc as i32) << 22
            | (imm9 & 0x1ff) << 12
            | Self::x_or_sp(rn) << 5
            | (rd as i32)
    }

    const fn load_store_register_unscaled_immediate_gp(
        size: MemOpSize,
        v: bool,
        opc: MemOp,
        imm9: i32,
        rn: u8,
        rd: u8,
    ) -> i32 {
        Self::load_store_register_unscaled_immediate(size, v, opc, imm9, rn, Self::x_or_zr(rd) as _)
    }

    const fn logical_immediate(
        sf: Datasize,
        opc: LogicalOp,
        n_immr_imms: i32,
        rn: u8,
        rd: u8,
    ) -> i32 {
        0x12000000
            | (sf as i32) << 31
            | (opc as i32) << 29
            | n_immr_imms << 10
            | Self::x_or_zr(rn) << 5
            | Self::x_or_zr_or_sp(rd, opc as u8 == LogicalOp::ANDS as u8)
    }

    const fn logical_shifted_register(
        sf: Datasize,
        opc: LogicalOp,
        shift: ShiftType,
        n: bool,
        rm: u8,
        imm6: i32,
        rn: u8,
        rd: u8,
    ) -> i32 {
        0x0a000000
            | (sf as i32) << 31
            | (opc as i32) << 29
            | (shift as i32) << 22
            | (n as i32) << 21
            | Self::x_or_zr(rm) << 16
            | (imm6 & 0x3f) << 10
            | Self::x_or_zr(rn) << 5
            | Self::x_or_zr(rd)
    }

    const fn move_wide_immediate(
        sf: Datasize,
        opc: MoveWideOp,
        hw: i32,
        imm16: u16,
        rd: u8,
    ) -> i32 {
        0x12800000
            | (sf as i32) << 31
            | (opc as i32) << 29
            | (hw as i32) << 21
            | (imm16 as i32) << 5
            | Self::x_or_zr(rd)
    }

    const fn unconditional_branch_immediate(op: bool, imm26: i32) -> i32 {
        0x14000000 | (op as i32) << 31 | (imm26 & 0x3ffffff)
    }

    const fn pc_relative(op: bool, imm21: i32, rd: u8) -> i32 {
        let immlo = imm21 & 3;
        let immhi = (imm21 >> 2) & 0x7ffff;
        0x10000000 | (op as i32) << 31 | (immlo << 29) | (immhi << 5) | Self::x_or_zr(rd)
    }

    const fn system(l: bool, op0: i32, op1: i32, crn: i32, crm: i32, op2: i32, rt: u8) -> i32 {
        0xd5000000u32 as i32
            | (l as i32) << 21
            | op0 << 19
            | op1 << 16
            | crn << 12
            | crm << 8
            | op2 << 5
            | Self::x_or_zr(rt)
    }

    const fn hint_pseudo(imm: i32) -> i32 {
        Self::system(false, 0, 3, 2, (imm >> 3) & 0xf, imm & 0x7, zr as _)
    }

    const fn nop_pseudo() -> i32 {
        Self::hint_pseudo(0)
    }

    const fn data_cache_zero_virtual_address(rt: u8) -> i32 {
        Self::system(false, 1, 0x3, 0x7, 0x4, 0x1, rt)
    }

    const fn test_and_branch_immediate(op: bool, b50: i32, imm14: i32, rt: u8) -> i32 {
        let b5 = b50 >> 5;
        let b40 = b50 & 0x1f;

        0x36000000
            | b5 << 31
            | (op as i32) << 24
            | b40 << 19
            | (imm14 & 0x3fff) << 5
            | Self::x_or_zr(rt)
    }

    const fn unconditional_branch_register(opc: BranchType, rn: u8) -> i32 {
        let op2 = 0x1f;
        let op3 = 0;
        let op4 = 0;

        0xd6000000u32 as i32
            | (opc as i32) << 21
            | op2 << 16
            | op3 << 10
            | Self::x_or_zr(rn) << 5
            | op4
    }

    const fn exotic_load(
        size: MemOpSize,
        fence: ExoticLoadFence,
        atomic: ExoticLoadAtomic,
        dst: u8,
        src: u8,
    ) -> i32 {
        0x085f7c00
            | (size as i32) << 30
            | (fence as i32) << 15
            | (atomic as i32) << 23
            | (src as i32) << 5
            | dst as i32
    }

    const fn store_release(size: MemOpSize, src: u8, dst: u8) -> i32 {
        0x089ffc00 | (size as i32) << 30 | (dst as i32) << 5 | src as i32
    }

    const fn exotic_store(
        size: MemOpSize,
        fence: ExoticStoreFence,
        result: u8,
        src: u8,
        dst: u8,
    ) -> i32 {
        0x08007c00
            | (size as i32) << 30
            | (result as i32) << 16
            | (fence as i32) << 15
            | (dst as i32) << 5
            | src as i32
    }

    const fn simd_general(scalar: bool, q: bool, imm5: i32, imm4: i32, rn: i32, rd: i32) -> i32 {
        0b00001110000000000000010000000000
            | (q as i32) << 30
            | (scalar as i32) << 28
            | imm5 << 16
            | imm4 << 11
            | rn << 5
            | rd
    }

    const fn simd_floating_point_vector_compare(
        u: bool,
        e: bool,
        ac: bool,
        lane: SIMDLane,
        rd: u8,
        rn: u8,
        rm: u8,
    ) -> i32 {
        let sz = size_for_floating_point_simd(lane) as i32;

        0b01001110001000001110010000000000
            | (u as i32) << 29
            | (e as i32) << 23
            | sz << 22
            | (rm as i32) << 16
            | (ac as i32) << 11
            | (rn as i32) << 5
            | rd as i32
    }

    const fn simd_move_immediate(q: bool, op: bool, cmode: u8, imm: u8, rd: u8) -> i32 {
        0b00001111000000000000010000000000
            | (q as i32) << 30
            | (op as i32) << 29
            | (((imm as usize) >> 5) << 16) as i32
            | (cmode as i32) << 12
            | (((imm as usize) & 0b11111) << 5) as i32
            | rd as i32
    }
}

const fn size_for_floating_point_simd(lane: SIMDLane) -> bool {
    if lane.element_byte_size() == 4 {
        false
    } else {
        true
    }
}

const fn size_for_integral_simd_op(lane: SIMDLane) -> i32 {
    match lane.element_byte_size() {
        1 => 0,
        2 => 1,
        4 => 2,
        8 => 3,
        _ => unreachable!(),
    }
}
