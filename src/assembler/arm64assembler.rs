#![allow(non_upper_case_globals, unused_mut, unused_variables, dead_code)]
use crate::assembler::assembler_common::{is_int, is_int9, is_uint12};
use std::{
    mem::{size_of, transmute},
    ops::{Deref, DerefMut},
};

use super::{
    assembler_common::{
        is_valid_scaled_uimm12, is_valid_signed_imm7, is_valid_signed_imm9, ARM64LogicalImmediate,
        SIMDLane,
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

pub struct PairPreIndex(pub i32);

impl PairPreIndex {
    pub const fn new(value: i32) -> Self {
        assert!(is_int::<11>(value));
        PairPreIndex(value)
    }
}

impl Deref for PairPreIndex {
    type Target = i32;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for PairPreIndex {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Into<i32> for PairPreIndex {
    fn into(self) -> i32 {
        self.0
    }
}

impl Into<u32> for PairPreIndex {
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

pub const NUMBER_OF_ADDRESS_ENCODING_INSTRUCTIONS: usize = MAX_POINTER_BITS / 16;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Default)]
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
    #[default]
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
enum ExoticAtomicLoadStore {
    Add = 0b0_000_00,
    Clear = 0b0_001_00,
    Xor = 0b0_010_00,
    Set = 0b0_011_00,
    Swap = 0b1_000_00,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Default)]
#[repr(u8)]
pub enum JumpType {
    Fixed = jump_with_size(0, 0),
    #[default]
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

pub const fn can_encode_pimm_offset<const DATASIZE: i32>(offset: i32) -> bool {
    is_valid_scaled_uimm12::<DATASIZE>(offset)
}

pub const fn can_encode_simm_offset(offset: i32) -> bool {
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

const fn datasize_of(size: i32) -> Datasize {
    match size {
        64 => Datasize::D64,
        128 => Datasize::D128,
        _ => Datasize::D32,
    }
}

fn mem_pair_op_size(x: i32) -> MemPairOpSize {
    if x == 64 {
        MemPairOpSize::S64
    } else {
        MemPairOpSize::S32
    }
}

fn mem_pair_op_size_fp(x: i32) -> MemPairOpSize {
    if x == 64 {
        MemPairOpSize::V64
    } else if x == 128 {
        MemPairOpSize::V128
    } else {
        MemPairOpSize::V32
    }
}

fn memopsize(x: i32) -> MemOpSize {
    if x == 64 {
        MemOpSize::M64
    } else if x == 128 {
        MemOpSize::M8Or128
    } else if x == 16 {
        MemOpSize::M16
    } else {
        MemOpSize::M32
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
    MSUB = 1,
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
    STORE,
    LOAD,
    STOREV128,
    LAODV128,
    //PREFETCH = 2,
    //LOADS32 = 3,
}

impl MemOp {
    pub const LOADS64: Self = Self::PREFETCH;
    pub const PREFETCH: Self = Self::LOAD;
    pub const LOADS32: Self = Self::LAODV128;
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

    pub unsafe fn address_of(code: *mut u8, label: AssemblerLabel) -> *mut i32 {
        code.add(label.offset() as _).cast()
    }

    pub unsafe fn link_pointer(code: *mut u8, where_: AssemblerLabel, to: *mut u8) {
        Self::link_pointer_raw(Self::address_of(code, where_), to, false);
    }

    pub unsafe fn link_call(code: *mut u8, where_: AssemblerLabel, to: *mut u8) {
        Self::link_jump_or_call(
            BranchType::CALL,
            Self::address_of(code, where_).sub(1),
            Self::address_of(code, where_).sub(1),
            to,
        );
    }

    pub unsafe fn link_tail_call(code: *mut u8, where_: AssemblerLabel, to: *mut u8) {
        Self::link_jump_(code, where_, to);
    }

    pub unsafe fn link_jump_(code: *mut u8, where_: AssemblerLabel, to: *mut u8) {
        Self::link_jump_or_call(
            BranchType::JMP,
            Self::address_of(code, where_),
            Self::address_of(code, where_),
            to,
        );
    }

    pub fn label_for_watchpoint(&mut self) -> AssemblerLabel {
        let mut result = self.buffer.label();

        if result.offset() != self.index_of_tail_of_last_watchpoint as u32 {
            result = self.label();
        }

        self.index_of_last_watchpoint = result.offset() as _;
        self.index_of_tail_of_last_watchpoint = result.offset() as usize;

        result
    }

    pub fn label_ignoring_watchpoints(&mut self) -> AssemblerLabel {
        self.buffer.label()
    }

    pub fn label(&mut self) -> AssemblerLabel {
        let mut result = self.buffer.label();

        while result.offset < self.index_of_tail_of_last_watchpoint as u32 {
            self.nop();
            result = self.buffer.label();
        }

        result
    }

    pub fn align(&mut self, alignment: usize) -> AssemblerLabel {
        while !self.buffer.is_aligned(alignment) {
            self.brk(0);
        }

        self.label()
    }

    pub unsafe fn replace_with_address_computation(mut instruction_start: *mut u8) {
        todo!()
    }

    pub unsafe fn replace_with_load(mut instruction_start: *mut u8) {
        todo!()
    }

    pub unsafe fn repatch_pointer(where_: *mut u8, to: *mut u8) {
        todo!()
    }

    pub unsafe fn repatch_int32(where_: *mut u8, value: i32) {
        todo!()
    }

    pub unsafe fn replace_with_vm_halt(where_: *mut u8) {
        let insn = Self::data_cache_zero_virtual_address(zr);
        where_.cast::<i32>().write_unaligned(insn);
        jit_allocator::virtual_memory::flush_instruction_cache(where_, size_of::<i32>());
    }

    pub unsafe fn replace_with_jump(where_: *mut u8, to: *const u8) {
        let offset = to.offset_from(where_) as i32;

        let insn = Self::unconditional_branch_immediate(false, offset);
        where_.cast::<i32>().write_unaligned(insn);
        jit_allocator::virtual_memory::flush_instruction_cache(where_, size_of::<i32>());
    }

    pub unsafe fn relink_jump(from: *mut u8, to: *mut u8) {
        Self::relink_jump_or_call(BranchType::JMP, from.cast(), from.cast(), to);
        jit_allocator::virtual_memory::flush_instruction_cache(from, size_of::<i32>());
    }

    pub unsafe fn relink_call(from: *mut u8, to: *mut u8) {
        Self::relink_jump_or_call(
            BranchType::CALL,
            from.cast::<i32>().sub(1),
            from.cast::<i32>().sub(1),
            to,
        );
        jit_allocator::virtual_memory::flush_instruction_cache(
            from.cast::<i32>().sub(1).cast(),
            size_of::<i32>(),
        );
    }

    pub unsafe fn relink_tail_call(from: *mut u8, to: *mut u8) {
        Self::relink_jump(from, to);
    }

    pub fn get_call_return_offset(call: AssemblerLabel) -> usize {
        call.offset() as _
    }

    pub unsafe fn get_relocate_address(code: *mut u8, label: AssemblerLabel) -> *mut u8 {
        code.add(label.offset() as _)
    }

    pub fn link_jump_cond(
        &mut self,
        from: AssemblerLabel,
        to: AssemblerLabel,
        typ: JumpType,
        cond: Condition,
    ) {
        self.jumps_to_link.push(LinkRecord::new_cond(
            from.offset() as _,
            to.offset() as _,
            typ,
            cond,
        ))
    }

    pub fn link_jump_cmp(
        &mut self,
        from: AssemblerLabel,
        to: AssemblerLabel,
        typ: JumpType,
        cond: Condition,
        is_64bit: bool,
        compare_register: u8,
    ) {
        self.jumps_to_link.push(LinkRecord::new_cmp(
            from.offset() as _,
            to.offset() as _,
            typ,
            cond,
            is_64bit,
            compare_register,
        ))
    }

    pub fn link_jump_test_bit(
        &mut self,
        from: AssemblerLabel,
        to: AssemblerLabel,
        typ: JumpType,
        cond: Condition,
        bit_number: u8,
        compare_register: u8,
    ) {
        self.jumps_to_link.push(LinkRecord::new_test_bit(
            from.offset() as _,
            to.offset() as _,
            typ,
            cond,
            bit_number,
            compare_register,
        ))
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

    pub fn add<const DATASIZE: i32, const SET_FLAGS: bool>(&mut self, rd: u8, rn: u8, rm: u8) {
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

    pub fn and_imm<const DATASIZE: i32, const SET_FLAGS: bool>(
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

    pub fn asr_imm<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, shift: i32) {
        self.sbfm::<DATASIZE>(rd, rn, shift, DATASIZE - 1);
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
            _nzcv,
        ))
    }

    pub fn ccmn<const DATASIZE: i32>(&mut self, rn: u8, rm: u8, _nzcv: i32, cond: Condition) {
        self.insn(Self::conditional_compare_register(
            datasize(DATASIZE),
            AddOp::ADD,
            rm,
            cond,
            rn,
            _nzcv,
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
            _nzcv,
        ))
    }

    pub fn ccmp<const DATASIZE: i32>(&mut self, rn: u8, rm: u8, _nzcv: i32, cond: Condition) {
        self.insn(Self::conditional_compare_register(
            datasize(DATASIZE),
            AddOp::SUB,
            rm,
            cond,
            rn,
            _nzcv,
        ));
    }

    pub fn cinc<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, rm: u8, cond: Condition) {
        self.csinc::<DATASIZE>(rd, rn, rm, cond.invert());
    }

    pub fn cinv<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, rm: u8, cond: Condition) {
        self.csinv::<DATASIZE>(rd, rn, rm, cond.invert());
    }

    pub fn cls<const DATASIZE: i32>(&mut self, rd: u8, rn: u8) {
        self.insn(Self::data_processing_1_source(
            datasize(DATASIZE),
            DataOp1Source::CLS,
            rn,
            rd,
        ));
    }

    pub fn clz<const DATASIZE: i32>(&mut self, rd: u8, rn: u8) {
        self.insn(Self::data_processing_1_source(
            datasize(DATASIZE),
            DataOp1Source::CLZ,
            rn,
            rd,
        ));
    }

    pub fn cmn_imm<const DATASIZE: i32>(&mut self, rn: u8, imm12: UInt12, shift: i32) {
        self.add_imm::<DATASIZE, true>(zr, rn, imm12, shift)
    }

    pub fn cmn<const DATASIZE: i32>(&mut self, rn: u8, rm: u8) {
        self.add::<DATASIZE, true>(zr, rn, rm)
    }

    pub fn cmn_extend<const DATASIZE: i32>(
        &mut self,
        rn: u8,
        rm: u8,
        extend: ExtendType,
        amount: i32,
    ) {
        self.add_extend::<DATASIZE, true>(zr, rn, rm, extend, amount)
    }

    pub fn cmn_shifted<const DATASIZE: i32>(
        &mut self,
        rn: u8,
        rm: u8,
        shift: ShiftType,
        amount: i32,
    ) {
        self.add_shifted::<DATASIZE, true>(zr, rn, rm, shift, amount)
    }

    pub fn cmp_imm<const DATASIZE: i32>(&mut self, rn: u8, imm12: UInt12, shift: i32) {
        self.sub_imm::<DATASIZE, true>(zr, rn, imm12, shift)
    }

    pub fn cmp<const DATASIZE: i32>(&mut self, rn: u8, rm: u8) {
        self.sub::<DATASIZE, true>(zr, rn, rm)
    }

    pub fn cmp_extend<const DATASIZE: i32>(
        &mut self,
        rn: u8,
        rm: u8,
        extend: ExtendType,
        amount: i32,
    ) {
        self.sub_extend::<DATASIZE, true>(zr, rn, rm, extend, amount)
    }

    pub fn cmp_shifted<const DATASIZE: i32>(
        &mut self,
        rn: u8,
        rm: u8,
        shift: ShiftType,
        amount: i32,
    ) {
        self.sub_shifted::<DATASIZE, true>(zr, rn, rm, shift, amount)
    }

    pub fn cneg<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, cond: Condition) {
        self.csneg::<DATASIZE>(rd, rn, zr, cond.invert())
    }

    pub fn csel<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, rm: u8, cond: Condition) {
        self.insn(Self::conditional_select(
            datasize(DATASIZE),
            false,
            rm,
            cond,
            false,
            rn,
            rd,
        ))
    }

    pub fn cset<const DATASIZE: i32>(&mut self, rd: u8, cond: Condition) {
        self.csinc::<DATASIZE>(rd, zr, zr, cond.invert())
    }

    pub fn csetm<const DATASIZE: i32>(&mut self, rd: u8, cond: Condition) {
        self.csinv::<DATASIZE>(rd, zr, zr, cond.invert())
    }

    pub fn csneg<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, rm: u8, cond: Condition) {
        self.insn(Self::conditional_select(
            datasize(DATASIZE),
            true,
            rm,
            cond,
            true,
            rn,
            rd,
        ))
    }

    pub fn csinc<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, rm: u8, cond: Condition) {
        self.insn(Self::conditional_select(
            datasize(DATASIZE),
            false,
            rm,
            cond,
            true,
            rn,
            rd,
        ))
    }

    pub fn csinv<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, rm: u8, cond: Condition) {
        self.insn(Self::conditional_select(
            datasize(DATASIZE),
            true,
            rm,
            cond,
            false,
            rn,
            rd,
        ))
    }

    pub fn eon_shifted<const DATASIZE: i32>(
        &mut self,
        rd: u8,
        rn: u8,
        rm: u8,
        shift: ShiftType,
        amount: i32,
    ) {
        self.insn(Self::logical_shifted_register(
            datasize(DATASIZE),
            LogicalOp::EOR,
            shift,
            true,
            rm,
            amount,
            rn,
            rd,
        ))
    }

    pub fn eon<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, rm: u8) {
        self.eon_shifted::<DATASIZE>(rd, rn, rm, ShiftType::LSL, 0)
    }

    pub fn eor_shifted<const DATASIZE: i32>(
        &mut self,
        rd: u8,
        rn: u8,
        rm: u8,
        shift: ShiftType,
        amount: i32,
    ) {
        self.insn(Self::logical_shifted_register(
            datasize(DATASIZE),
            LogicalOp::EOR,
            shift,
            false,
            rm,
            amount,
            rn,
            rd,
        ))
    }

    pub fn eor_imm<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, imm: LogicalImmediate) {
        self.insn(Self::logical_immediate(
            datasize(DATASIZE),
            LogicalOp::EOR,
            imm.value(),
            rn,
            rd,
        ))
    }

    pub fn eor<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, rm: u8) {
        self.eor_shifted::<DATASIZE>(rd, rn, rm, ShiftType::LSL, 0)
    }

    pub fn extr<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, rm: u8, lsb: i32) {
        self.insn(Self::extract(datasize(DATASIZE), rm, lsb, rn, rd))
    }

    pub fn hlt(&mut self, imm16: u16) {
        self.insn(Self::excepn_generation(ExcepnOp::HALT, imm16, 0))
    }

    pub fn illegal_instruction(&mut self) {
        self.insn(0x0);
    }

    pub fn is_valid_ldp_imm<const DATASIZE: i32>(imm: i32) -> bool {
        let immed_shift_amount = mem_pair_offset_shift(false, mem_pair_op_size(DATASIZE));
        is_valid_signed_imm7(imm, immed_shift_amount as _)
    }

    pub fn is_valid_ldp_fp_imm<const DATASIZE: i32>(imm: i32) -> bool {
        let immed_shift_amount = mem_pair_offset_shift(true, mem_pair_op_size_fp(DATASIZE));
        is_valid_signed_imm7(imm, immed_shift_amount as _)
    }

    pub fn ldp_post<const DATASIZE: i32>(&mut self, rt: u8, rt2: u8, rn: u8, simm: PairPostIndex) {
        self.insn(Self::load_store_register_pair_post_index_gp(
            mem_pair_op_size(DATASIZE),
            false,
            MemOp::LOAD,
            simm.0,
            rn,
            rt,
            rt2,
        ))
    }

    pub fn ldp_pre<const DATASIZE: i32>(&mut self, rt: u8, rt2: u8, rn: u8, simm: PairPreIndex) {
        self.insn(Self::load_store_register_pair_pre_index_gp(
            mem_pair_op_size(DATASIZE),
            MemOp::LOAD,
            simm.0,
            rn,
            rt,
            rt2,
        ))
    }

    pub fn ldp_imm<const DATASIZE: i32>(&mut self, rt: u8, rt2: u8, rn: u8, imm: i32) {
        self.insn(Self::load_store_register_pair_offset_gp(
            mem_pair_op_size(DATASIZE),
            false,
            MemOp::LOAD,
            imm,
            rn,
            rt,
            rt2,
        ))
    }

    pub fn ldnp<const DATASIZE: i32>(&mut self, rt: u8, rt2: u8, rn: u8, imm: i32) {
        self.insn(Self::load_store_register_pair_non_temporal_gp(
            mem_pair_op_size(DATASIZE),
            true,
            MemOp::LOAD,
            imm,
            rn,
            rt,
            rt2,
        ))
    }

    pub fn ldnp_fp<const DATASIZE: i32>(&mut self, rt: u8, rt2: u8, rn: u8, imm: i32) {
        self.insn(Self::load_store_register_pair_non_temporal_gp(
            mem_pair_op_size_fp(DATASIZE),
            true,
            MemOp::LOAD,
            imm,
            rn,
            rt,
            rt2,
        ))
    }

    pub fn ldp_post_fp<const DATASIZE: i32>(
        &mut self,
        rt: u8,
        rt2: u8,
        rn: u8,
        simm: PairPostIndex,
    ) {
        self.insn(Self::load_store_register_pair_post_index_gp(
            mem_pair_op_size_fp(DATASIZE),
            true,
            MemOp::LOAD,
            simm.0,
            rn,
            rt,
            rt2,
        ))
    }

    pub fn ldp_pre_fp<const DATASIZE: i32>(&mut self, rt: u8, rt2: u8, rn: u8, simm: PairPreIndex) {
        self.insn(Self::load_store_register_pair_pre_index_gp(
            mem_pair_op_size_fp(DATASIZE),
            MemOp::LOAD,
            simm.0,
            rn,
            rt,
            rt2,
        ))
    }

    pub fn ldp_imm_fp<const DATASIZE: i32>(&mut self, rt: u8, rt2: u8, rn: u8, imm: i32) {
        self.insn(Self::load_store_register_pair_offset_gp(
            mem_pair_op_size_fp(DATASIZE),
            true,
            MemOp::LOAD,
            imm,
            rn,
            rt,
            rt2,
        ))
    }

    pub fn ldr_extend<const DATASIZE: i32>(
        &mut self,
        rt: u8,
        rn: u8,
        rm: u8,
        extendop: ExtendType,
        amount: i32,
    ) {
        self.insn(Self::load_store_register_offset_gp(
            memopsize(DATASIZE),
            false,
            MemOp::LOAD,
            rm,
            extendop,
            encode_shift_amount::<DATASIZE>(amount) != 0,
            rn,
            rt,
        ))
    }

    pub fn ldr<const DATASIZE: i32>(&mut self, rt: u8, rn: u8, rm: u8) {
        self.ldr_extend::<DATASIZE>(rt, rn, rm, ExtendType::UXTX, 0)
    }

    pub fn ldr_imm<const DATASIZE: i32>(&mut self, rt: u8, rn: u8, imm: i32) {
        self.insn(Self::load_store_register_unscaled_immediate_gp(
            memopsize(DATASIZE),
            false,
            MemOp::LOAD,
            imm,
            rn,
            rt,
        ))
    }

    pub fn ldr_post<const DATASIZE: i32>(&mut self, rt: u8, rn: u8, simm: PostIndex) {
        self.insn(Self::load_store_register_post_index_gp(
            memopsize(DATASIZE),
            false,
            MemOp::LOAD,
            simm.0,
            rn,
            rt,
        ))
    }

    pub fn ldr_pre<const DATASIZE: i32>(&mut self, rt: u8, rn: u8, simm: PreIndex) {
        self.insn(Self::load_store_register_pre_index_gp(
            memopsize(DATASIZE),
            false,
            MemOp::LOAD,
            simm.0,
            rn,
            rt,
        ))
    }

    pub fn ldr_literal<const DATASIZE: i32>(&mut self, rt: u8, imm: i32) {
        self.insn(Self::load_register_literal_gp(
            if DATASIZE == 64 {
                LdrLiteralOp::S64
            } else {
                LdrLiteralOp::S32
            },
            false,
            imm,
            rt,
        ))
    }

    pub fn ldrb(&mut self, rt: u8, rn: u8, rm: u8) {
        self.insn(Self::load_store_register_offset_gp(
            MemOpSize::M8Or128,
            false,
            MemOp::LOAD,
            rm,
            ExtendType::UXTX,
            false,
            rn,
            rt,
        ));
    }

    pub fn ldrb_extend(&mut self, rt: u8, rn: u8, rm: u8, extendop: ExtendType, amount: i32) {
        assert!(amount == 0);
        self.insn(Self::load_store_register_offset_gp(
            MemOpSize::M8Or128,
            false,
            MemOp::LOAD,
            rm,
            extendop,
            true,
            rn,
            rt,
        ))
    }

    pub fn ldrb_imm(&mut self, rt: u8, rn: u8, imm: i32) {
        self.insn(Self::load_store_register_unsigned_immediate_gp(
            MemOpSize::M8Or128,
            false,
            MemOp::LOAD,
            imm,
            rn,
            rt,
        ))
    }

    pub fn ldrb_post(&mut self, rt: u8, rn: u8, simm: PostIndex) {
        self.insn(Self::load_store_register_post_index_gp(
            MemOpSize::M8Or128,
            false,
            MemOp::LOAD,
            simm.0,
            rn,
            rt,
        ))
    }

    pub fn ldrb_pre(&mut self, rt: u8, rn: u8, simm: PreIndex) {
        self.insn(Self::load_store_register_pre_index_gp(
            MemOpSize::M8Or128,
            false,
            MemOp::LOAD,
            simm.0,
            rn,
            rt,
        ))
    }

    pub fn ldrh_extend(&mut self, rt: u8, rn: u8, rm: u8, extendop: ExtendType, amount: i32) {
        self.insn(Self::load_store_register_offset_gp(
            MemOpSize::M16,
            false,
            MemOp::LOAD,
            rm,
            extendop,
            amount == 1,
            rn,
            rt,
        ))
    }

    pub fn ldrh(&mut self, rt: u8, rn: u8, rm: u8) {
        self.ldrh_extend(rt, rn, rm, ExtendType::UXTX, 0)
    }

    pub fn ldrh_imm(&mut self, rt: u8, rn: u8, imm: i32) {
        self.insn(Self::load_store_register_unsigned_immediate_gp(
            MemOpSize::M16,
            false,
            MemOp::LOAD,
            imm,
            rn,
            rt,
        ))
    }

    pub fn ldrh_post(&mut self, rt: u8, rn: u8, simm: PostIndex) {
        self.insn(Self::load_store_register_post_index_gp(
            MemOpSize::M16,
            false,
            MemOp::LOAD,
            simm.0,
            rn,
            rt,
        ))
    }

    pub fn ldrh_pre(&mut self, rt: u8, rn: u8, simm: PreIndex) {
        self.insn(Self::load_store_register_pre_index_gp(
            MemOpSize::M16,
            false,
            MemOp::LOAD,
            simm.0,
            rn,
            rt,
        ))
    }

    pub fn ldrsb<const DATASIZE: i32>(&mut self, rt: u8, rn: u8, rm: u8) {
        self.insn(Self::load_store_register_offset_gp(
            MemOpSize::M8Or128,
            false,
            if DATASIZE == 64 {
                MemOp::LOADS64
            } else {
                MemOp::LOADS32
            },
            rm,
            ExtendType::UXTX,
            false,
            rn,
            rt,
        ))
    }

    pub fn ldrsb_extend<const DATASIZE: i32>(
        &mut self,
        rt: u8,
        rn: u8,
        rm: u8,
        extendop: ExtendType,
        amount: i32,
    ) {
        assert!(amount == 0);
        self.insn(Self::load_store_register_offset_gp(
            MemOpSize::M8Or128,
            false,
            if DATASIZE == 64 {
                MemOp::LOADS64
            } else {
                MemOp::LOADS32
            },
            rm,
            extendop,
            true,
            rn,
            rt,
        ))
    }

    pub fn ldrsb_imm<const DATASIZE: i32>(&mut self, rt: u8, rn: u8, imm: i32) {
        self.insn(Self::load_store_register_unsigned_immediate_gp(
            MemOpSize::M8Or128,
            false,
            if DATASIZE == 64 {
                MemOp::LOADS64
            } else {
                MemOp::LOADS32
            },
            imm,
            rn,
            rt,
        ))
    }

    pub fn ldrsb_post<const DATASIZE: i32>(&mut self, rt: u8, rn: u8, simm: PostIndex) {
        self.insn(Self::load_store_register_post_index_gp(
            MemOpSize::M8Or128,
            false,
            if DATASIZE == 64 {
                MemOp::LOADS64
            } else {
                MemOp::LOADS32
            },
            simm.0,
            rn,
            rt,
        ))
    }

    pub fn ldrsb_pre<const DATASIZE: i32>(&mut self, rt: u8, rn: u8, simm: PreIndex) {
        self.insn(Self::load_store_register_pre_index_gp(
            MemOpSize::M8Or128,
            false,
            if DATASIZE == 64 {
                MemOp::LOADS64
            } else {
                MemOp::LOADS32
            },
            simm.0,
            rn,
            rt,
        ))
    }

    pub fn ldrsh<const DATASIZE: i32>(&mut self, rt: u8, rn: u8, rm: u8) {
        self.insn(Self::load_store_register_offset_gp(
            MemOpSize::M16,
            false,
            if DATASIZE == 64 {
                MemOp::LOADS64
            } else {
                MemOp::LOADS32
            },
            rm,
            ExtendType::UXTX,
            false,
            rn,
            rt,
        ))
    }

    pub fn ldrsh_extend<const DATASIZE: i32>(
        &mut self,
        rt: u8,
        rn: u8,
        rm: u8,
        extendop: ExtendType,
        amount: i32,
    ) {
        assert!(amount == 0 || amount == 1);
        self.insn(Self::load_store_register_offset_gp(
            MemOpSize::M16,
            false,
            if DATASIZE == 64 {
                MemOp::LOADS64
            } else {
                MemOp::LOADS32
            },
            rm,
            extendop,
            amount == 1,
            rn,
            rt,
        ))
    }

    pub fn ldrsh_imm<const DATASIZE: i32>(&mut self, rt: u8, rn: u8, imm: i32) {
        self.insn(Self::load_store_register_unsigned_immediate_gp(
            MemOpSize::M16,
            false,
            if DATASIZE == 64 {
                MemOp::LOADS64
            } else {
                MemOp::LOADS32
            },
            imm,
            rn,
            rt,
        ))
    }

    pub fn ldrsh_post<const DATASIZE: i32>(&mut self, rt: u8, rn: u8, simm: PostIndex) {
        self.insn(Self::load_store_register_post_index_gp(
            MemOpSize::M16,
            false,
            if DATASIZE == 64 {
                MemOp::LOADS64
            } else {
                MemOp::LOADS32
            },
            simm.0,
            rn,
            rt,
        ))
    }

    pub fn ldrsh_pre<const DATASIZE: i32>(&mut self, rt: u8, rn: u8, simm: PreIndex) {
        self.insn(Self::load_store_register_pre_index_gp(
            MemOpSize::M16,
            false,
            if DATASIZE == 64 {
                MemOp::LOADS64
            } else {
                MemOp::LOADS32
            },
            simm.0,
            rn,
            rt,
        ))
    }

    pub fn ldrsw_extend(&mut self, rt: u8, rn: u8, rm: u8, extendop: ExtendType, amount: i32) {
        assert!(amount == 0 || amount == 2);
        self.insn(Self::load_store_register_offset_gp(
            MemOpSize::M32,
            false,
            MemOp::LOADS64,
            rm,
            extendop,
            amount == 2,
            rn,
            rt,
        ))
    }

    pub fn ldrsw(&mut self, rt: u8, rn: u8, rm: u8) {
        self.ldrsw_extend(rt, rn, rm, ExtendType::UXTX, 0)
    }

    pub fn ldrsw_imm(&mut self, rt: u8, rn: u8, imm: i32) {
        self.insn(Self::load_store_register_unsigned_immediate_gp(
            MemOpSize::M32,
            false,
            MemOp::LOADS64,
            imm,
            rn,
            rt,
        ))
    }

    pub fn ldrsw_post(&mut self, rt: u8, rn: u8, simm: PostIndex) {
        self.insn(Self::load_store_register_post_index_gp(
            MemOpSize::M32,
            false,
            MemOp::LOADS64,
            simm.0,
            rn,
            rt,
        ))
    }

    pub fn ldrsw_pre(&mut self, rt: u8, rn: u8, simm: PreIndex) {
        self.insn(Self::load_store_register_pre_index_gp(
            MemOpSize::M32,
            false,
            MemOp::LOADS64,
            simm.0,
            rn,
            rt,
        ))
    }

    pub fn ldrsw_literal(&mut self, rt: u8, offset: i32) {
        self.insn(Self::load_register_literal_gp(
            LdrLiteralOp::LDRSW,
            false,
            offset >> 2,
            rt,
        ))
    }

    pub fn ldur<const DATASIZE: i32>(&mut self, rt: u8, rn: u8, simm: i32) {
        self.insn(Self::load_store_register_unscaled_immediate_gp(
            memopsize(DATASIZE),
            false,
            MemOp::LOAD,
            simm,
            rn,
            rt,
        ));
    }

    pub fn ldurb(&mut self, rt: u8, rn: u8, simm: i32) {
        self.insn(Self::load_store_register_unscaled_immediate_gp(
            MemOpSize::M8Or128,
            false,
            MemOp::LOAD,
            simm,
            rn,
            rt,
        ))
    }

    pub fn ldurh(&mut self, rt: u8, rn: u8, simm: i32) {
        self.insn(Self::load_store_register_unscaled_immediate_gp(
            MemOpSize::M16,
            false,
            MemOp::LOAD,
            simm,
            rn,
            rt,
        ))
    }

    pub fn ldursb<const DATASIZE: i32>(&mut self, rt: u8, rn: u8, simm: i32) {
        self.insn(Self::load_store_register_unscaled_immediate_gp(
            MemOpSize::M8Or128,
            false,
            if DATASIZE == 64 {
                MemOp::LOADS64
            } else {
                MemOp::LOADS32
            },
            simm,
            rn,
            rt,
        ))
    }

    pub fn ldursh<const DATASIZE: i32>(&mut self, rt: u8, rn: u8, simm: i32) {
        self.insn(Self::load_store_register_unscaled_immediate_gp(
            MemOpSize::M16,
            false,
            if DATASIZE == 64 {
                MemOp::LOADS64
            } else {
                MemOp::LOADS32
            },
            simm,
            rn,
            rt,
        ))
    }

    pub fn ldursw(&mut self, rt: u8, rn: u8, simm: i32) {
        self.insn(Self::load_store_register_unscaled_immediate_gp(
            MemOpSize::M32,
            false,
            MemOp::LOADS64,
            simm,
            rn,
            rt,
        ))
    }

    pub fn lsl_imm<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, shift: i32) {
        self.ubfm::<DATASIZE>(
            rd,
            rn,
            (DATASIZE - shift) & (DATASIZE - 1),
            DATASIZE - 1 - shift,
        );
    }

    pub fn lslv<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, rm: u8) {
        self.insn(Self::data_processing_2_source(
            datasize(DATASIZE),
            rm,
            DataOp2Source::LSLV,
            rn,
            rd,
        ))
    }

    pub fn lsl<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, rm: u8) {
        self.lslv::<DATASIZE>(rd, rn, rm)
    }

    pub fn lsr_imm<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, shift: i32) {
        self.ubfm::<DATASIZE>(rd, rn, shift, DATASIZE - 1);
    }

    pub fn lsrv<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, rm: u8) {
        self.insn(Self::data_processing_2_source(
            datasize(DATASIZE),
            rm,
            DataOp2Source::LSRV,
            rn,
            rd,
        ))
    }

    pub fn madd<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, rm: u8, ra: u8) {
        let insn =
            Self::data_processing_3_source(datasize(DATASIZE), DataOp3Source::MADD, rm, ra, rn, rd);

        self.insn(insn);
    }

    pub fn lsr<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, rm: u8) {
        self.lsrv::<DATASIZE>(rd, rn, rm)
    }

    pub fn mneg<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, rm: u8) {
        self.msub::<DATASIZE>(rd, rn, rm, zr);
    }

    pub const fn simd_qbit(lane: SIMDLane) -> bool {
        lane.element_byte_size() == 8
    }

    pub const fn encode_lane_and_index(lane: SIMDLane, lane_index: u32) -> i32 {
        match lane.element_byte_size() {
            1 => 0b00001 | (lane_index as i32) << 1,
            2 => 0b00010 | (lane_index as i32) << 2,
            4 => 0b00100 | (lane_index as i32) << 3,
            8 => 0b01000 | (lane_index as i32) << 4,
            _ => unreachable!(),
        }
    }

    pub fn ins_i2f(&mut self, vd: u8, rn: u8, lane: SIMDLane, lane_index: u32) {
        self.insn(Self::simd_general(
            false,
            true,
            Self::encode_lane_and_index(lane, lane_index),
            0b0011,
            rn as _,
            vd as _,
        ))
    }

    pub fn ins_ff(&mut self, vd: u8, vn: u8, lane: SIMDLane, lane_index: u32) {
        let mut inst = 0b01101110000000000000010000000000;
        inst |= Self::encode_lane_and_index(lane, lane_index);
        inst |= 0 << 11;
        inst |= (vn as i32) << 5;
        inst |= vd as i32;

        self.insn(inst);
    }

    pub fn umov(&mut self, rd: u8, vn: u8, lane: SIMDLane, lane_index: u32) {
        self.insn(Self::simd_general(
            false,
            Self::simd_qbit(lane),
            Self::encode_lane_and_index(lane, lane_index),
            0b0111,
            vn as _,
            rd as _,
        ))
    }

    pub fn smov(&mut self, rd: u8, vn: u8, lane: SIMDLane, lane_index: u32) {
        self.insn(Self::simd_general(
            false,
            Self::simd_qbit(lane),
            Self::encode_lane_and_index(lane, lane_index),
            0b0101,
            vn as _,
            rd as _,
        ))
    }

    pub fn dup_element(&mut self, vd: u8, vn: u8, lane: SIMDLane, lane_index: u32) {
        self.insn(Self::simd_general(
            false,
            true,
            Self::encode_lane_and_index(lane, lane_index),
            0b0000,
            vn as _,
            vd as _,
        ))
    }

    pub fn dup_general(&mut self, vd: u8, rn: u8, lane: SIMDLane) {
        self.insn(Self::simd_general(
            false,
            true,
            Self::encode_lane_and_index(lane, 0),
            0b0001,
            rn as _,
            vd as _,
        ))
    }

    pub fn fcmeq(&mut self, vd: u8, vn: u8, vm: u8, lane: SIMDLane) {
        self.insn(Self::simd_floating_point_vector_compare(
            false, false, false, lane, vd, vn, vm,
        ))
    }

    pub fn fcmgt(&mut self, vd: u8, vn: u8, vm: u8, lane: SIMDLane) {
        self.insn(Self::simd_floating_point_vector_compare(
            true, true, false, lane, vd, vn, vm,
        ))
    }

    pub fn fcmge(&mut self, vd: u8, vn: u8, vm: u8, lane: SIMDLane) {
        self.insn(Self::simd_floating_point_vector_compare(
            true, false, false, lane, vd, vn, vm,
        ))
    }

    pub fn vector_not(&mut self, vd: u8, vn: u8) {
        self.insn(0b01101110001000000101100000000000 | (vn as i32) << 5 | vd as i32);
    }

    pub fn cmeq(&mut self, vd: u8, vn: u8, vm: u8, lane: SIMDLane) {
        self.insn(
            0b01101110001000001000110000000000
                | (size_for_integral_simd_op(lane) << 22)
                | (vm as i32) << 16
                | (vn as i32) << 5
                | vd as i32,
        );
    }

    pub fn cmeqz(&mut self, vd: u8, vn: u8, lane: SIMDLane) {
        self.insn(
            0b01001110001000001001100000000000
                | (size_for_integral_simd_op(lane) << 22)
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn cmhi(&mut self, vd: u8, vn: u8, vm: u8, lane: SIMDLane) {
        self.insn(
            0b01101110001000000011010000000000
                | (size_for_integral_simd_op(lane) << 22)
                | (vm as i32) << 16
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn cmhs(&mut self, vd: u8, vn: u8, vm: u8, lane: SIMDLane) {
        self.insn(
            0b01101110001000000011110000000000
                | (size_for_integral_simd_op(lane) << 22)
                | (vm as i32) << 16
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn cmgt(&mut self, vd: u8, vn: u8, vm: u8, lane: SIMDLane) {
        self.insn(
            0b01001110001000000011010000000000
                | (size_for_integral_simd_op(lane) << 22)
                | (vm as i32) << 16
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn cmge(&mut self, vd: u8, vn: u8, vm: u8, lane: SIMDLane) {
        self.insn(
            0b01001110001000000011110000000000
                | (size_for_integral_simd_op(lane) << 22)
                | (vm as i32) << 16
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn vector_add(&mut self, vd: u8, vn: u8, vm: u8, lane: SIMDLane) {
        self.insn(
            0b01001110001000001000010000000000
                | (size_for_integral_simd_op(lane) << 22)
                | (vm as i32) << 16
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn urhadd(&mut self, vd: u8, vn: u8, vm: u8, lane: SIMDLane) {
        self.insn(
            0b01101110001000000001010000000000
                | (size_for_integral_simd_op(lane) << 22)
                | (vm as i32) << 16
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn addpv(&mut self, vd: u8, vn: u8, vm: u8, lane: SIMDLane) {
        self.insn(
            0b01001110001000001011110000000000
                | (size_for_integral_simd_op(lane) << 22)
                | (vm as i32) << 16
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn addv(&mut self, vd: u8, vn: u8, lane: SIMDLane) {
        self.insn(
            0b01001110001100011011100000000000
                | (size_for_integral_simd_op(lane) << 22)
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn zip1(&mut self, vd: u8, vn: u8, vm: u8, lane: SIMDLane) {
        self.insn(
            0b01001110000000000011100000000000
                | (size_for_integral_simd_op(lane) << 22)
                | (vm as i32) << 16
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn uzip1(&mut self, vd: u8, vn: u8, vm: u8, lane: SIMDLane) {
        self.insn(
            0b01001110000000000001100000000000
                | (size_for_integral_simd_op(lane) << 22)
                | (vm as i32) << 16
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn ext(&mut self, vd: u8, vn: u8, vm: u8, first_lane: u8, _lane: SIMDLane) {
        self.insn(
            0b01101110000000000000000000000000
                | (vm as i32) << 16
                | (first_lane as i32) << 11
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn vector_sub(&mut self, vd: u8, vn: u8, vm: u8, lane: SIMDLane) {
        self.insn(
            0b01101110001000001000010000000000
                | (size_for_integral_simd_op(lane) << 22)
                | (vm as i32) << 16
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn vector_mul(&mut self, vd: u8, vn: u8, vm: u8, lane: SIMDLane) {
        self.insn(
            0b01001110001000001001110000000000
                | (size_for_integral_simd_op(lane) << 22)
                | (vm as i32) << 16
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn smullv(&mut self, vd: u8, vn: u8, vm: u8, input_lane: SIMDLane, q: bool, u: bool) {
        self.insn(
            0b00001110001000001100000000000000
                | (q as i32) << 30
                | (u as i32) << 29
                | (size_for_integral_simd_op(input_lane) << 22)
                | (vm as i32) << 16
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn smull2v(&mut self, vd: u8, vn: u8, vm: u8, input_lane: SIMDLane) {
        self.smullv(vd, vn, vm, input_lane, true, false)
    }

    pub fn umullv(&mut self, vd: u8, vn: u8, vm: u8, input_lane: SIMDLane) {
        self.smullv(vd, vn, vm, input_lane, false, true)
    }

    pub fn umull2v(&mut self, vd: u8, vn: u8, vm: u8, input_lane: SIMDLane) {
        self.smullv(vd, vn, vm, input_lane, true, true)
    }

    pub fn sqrdmlahv(&mut self, vd: u8, vn: u8, vm: u8, lane: SIMDLane) {
        self.insn(
            0b01101110000000001000010000000000
                | (size_for_integral_simd_op(lane) << 22)
                | (vm as i32) << 16
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn sqrdmulhv(&mut self, vd: u8, vn: u8, vm: u8, lane: SIMDLane) {
        self.insn(
            0b01101110001000001011010000000000
                | (size_for_integral_simd_op(lane) << 22)
                | (vm as i32) << 16
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn vector_fadd(&mut self, vd: u8, vn: u8, vm: u8, lane: SIMDLane) {
        self.insn(
            0b01001110001000001101010000000000
                | (size_for_floating_point_simd(lane) as i32) << 22
                | (vm as i32) << 16
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn vector_fsub(&mut self, vd: u8, vn: u8, vm: u8, lane: SIMDLane) {
        self.insn(
            0b01001110101000001101010000000000
                | (size_for_floating_point_simd(lane) as i32) << 22
                | (vm as i32) << 16
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn vector_fmul(&mut self, vd: u8, vn: u8, vm: u8, lane: SIMDLane) {
        self.insn(
            0b01101110001000001101110000000000
                | (size_for_floating_point_simd(lane) as i32) << 22
                | (vm as i32) << 16
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn vector_fdiv(&mut self, vd: u8, vn: u8, vm: u8, lane: SIMDLane) {
        self.insn(
            0b01101110001000001111110000000000
                | (size_for_floating_point_simd(lane) as i32) << 22
                | (vm as i32) << 16
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn vector_fmla(&mut self, vd: u8, vn: u8, vm: u8, lane: SIMDLane) {
        self.insn(
            0b01001110001000001100110000000000
                | (size_for_floating_point_simd(lane) as i32) << 22
                | (vm as i32) << 16
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn vector_fmls(&mut self, vd: u8, vn: u8, vm: u8, lane: SIMDLane) {
        self.insn(
            0b01001110101000001100110000000000
                | (size_for_floating_point_simd(lane) as i32) << 22
                | (vm as i32) << 16
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub const fn encode_lane_and_index_to_hlm(lane: SIMDLane, lane_index: u32) -> u32 {
        match lane.element_byte_size() {
            4 => (((lane_index & 0b10) >> 1) << 11) | ((lane_index & 0b01) << 21),

            8 => lane_index << 11,

            _ => unreachable!(),
        }
    }

    pub fn vector_fmul_by_element(
        &mut self,
        vd: u8,
        vn: u8,
        vm: u8,
        lane: SIMDLane,
        lane_index: u32,
    ) {
        self.insn(
            0b01001111100000001001000000000000
                | (size_for_floating_point_simd(lane) as i32) << 22
                | Self::encode_lane_and_index_to_hlm(lane, lane_index) as i32
                | (vm as i32) << 16
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn umax(&mut self, vd: u8, vn: u8, vm: u8, lane: SIMDLane) {
        self.insn(
            0b01101110001000000110010000000000
                | (size_for_integral_simd_op(lane) << 22)
                | (vm as i32) << 16
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn umaxv<const DESTSIZE: i32>(&mut self, vd: u8, vn: u8) {
        let q = 1;
        let size = if DESTSIZE == 8 {
            0
        } else if DESTSIZE == 16 {
            1
        } else {
            2
        };

        self.insn(
            0b00101110001100001010100000000000
                | (q << 30)
                | (size << 22)
                | (vn as i32) << 5
                | vd as i32,
        );
    }

    pub fn smax(&mut self, vd: u8, vn: u8, vm: u8, lane: SIMDLane) {
        self.insn(
            0b01001110001000000110010000000000
                | (size_for_integral_simd_op(lane) << 22)
                | (vm as i32) << 16
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn umin(&mut self, vd: u8, vn: u8, vm: u8, lane: SIMDLane) {
        self.insn(
            0b01101110001000000110110000000000
                | (size_for_integral_simd_op(lane) << 22)
                | (vm as i32) << 16
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn smin(&mut self, vd: u8, vn: u8, vm: u8, lane: SIMDLane) {
        self.insn(
            0b01001110001000000110110000000000
                | (size_for_integral_simd_op(lane) << 22)
                | (vm as i32) << 16
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn vector_fmax(&mut self, vd: u8, vn: u8, vm: u8, lane: SIMDLane) {
        self.insn(
            0b01001110001000001111010000000000
                | (size_for_integral_simd_op(lane) as i32) << 22
                | (vm as i32) << 16
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn vector_fmin(&mut self, vd: u8, vn: u8, vm: u8, lane: SIMDLane) {
        self.insn(
            0b01001110101000001111010000000000
                | (size_for_floating_point_simd(lane) as i32) << 22
                | (vm as i32) << 16
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn bsl(&mut self, vd: u8, vn: u8, vm: u8) {
        self.insn(
            0b01101110001000000001110000000000 | (vm as i32) << 16 | (vn as i32) << 5 | vd as i32,
        )
    }

    pub fn vector_abs(&mut self, vd: u8, vn: u8, lane: SIMDLane) {
        self.insn(
            0b01001110001000001011100000000000
                | (size_for_integral_simd_op(lane) as i32) << 22
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn vector_fabs(&mut self, vd: u8, vn: u8, lane: SIMDLane) {
        self.insn(
            0b01001110101000001111100000000000
                | (size_for_floating_point_simd(lane) as i32) << 22
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn vector_neg(&mut self, vd: u8, vn: u8, lane: SIMDLane) {
        self.insn(
            0b01101110001000001011100000000000
                | (size_for_floating_point_simd(lane) as i32) << 22
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn vector_fneg(&mut self, vd: u8, vn: u8, lane: SIMDLane) {
        self.insn(
            0b01101110101000001111100000000000
                | (size_for_floating_point_simd(lane) as i32) << 22
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn vector_cnt(&mut self, vd: u8, vn: u8, lane: SIMDLane) {
        self.insn(
            0b01001110001000000101100000000000
                | (size_for_integral_simd_op(lane) as i32) << 22
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn vector_fcvtps(&mut self, vd: u8, vn: u8, lane: SIMDLane) {
        self.insn(
            0b01001110101000011010100000000000
                | (size_for_floating_point_simd(lane) as i32) << 22
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn vector_fcvtms(&mut self, vd: u8, vn: u8, lane: SIMDLane) {
        self.insn(
            0b01001110001000011011100000000000
                | (size_for_floating_point_simd(lane) as i32) << 22
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn vector_frintz(&mut self, vd: u8, vn: u8, lane: SIMDLane) {
        self.insn(
            0b01001110101000011001100000000000
                | (size_for_floating_point_simd(lane) as i32) << 22
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn vector_fcvtns(&mut self, vd: u8, vn: u8, lane: SIMDLane) {
        self.insn(
            0b01001110001000011010100000000000
                | (size_for_floating_point_simd(lane) as i32) << 22
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn vector_fsqrt(&mut self, vd: u8, vn: u8, lane: SIMDLane) {
        self.insn(
            0b01101110101000011111100000000000
                | (size_for_floating_point_simd(lane) as i32) << 22
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub const fn immh_for_extend(lane: SIMDLane) -> i32 {
        match lane.element_byte_size() {
            2 => 0b0001,
            4 => 0b0010,
            8 => 0b0100,
            _ => unreachable!(),
        }
    }

    pub fn uxtl(&mut self, vd: u8, vn: u8, lane: SIMDLane) {
        self.insn(
            0b00101111000000001010010000000000
                | (Self::immh_for_extend(lane) << 19)
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn uxtl2(&mut self, vd: u8, vn: u8, lane: SIMDLane) {
        self.insn(
            0b01101111000000001010010000000000
                | (Self::immh_for_extend(lane) << 19)
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn sxtl(&mut self, vd: u8, vn: u8, lane: SIMDLane) {
        self.insn(
            0b00001111000000001010010000000000
                | (Self::immh_for_extend(lane) << 19)
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn sxtl2(&mut self, vd: u8, vn: u8, lane: SIMDLane) {
        self.insn(
            0b01001111000000001010010000000000
                | (Self::immh_for_extend(lane) << 19)
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    /// Convert lower two f32s into two f64s, so sz bit == 1
    /// This represents the input lane, not the output.
    /// The instruction encodes input element size as 16 << sz_bit
    pub fn fcvtl(&mut self, vd: u8, vn: u8, lane: SIMDLane) {
        assert_eq!(lane, SIMDLane::F32X4);
        self.insn(0b00001110011000010111100000000000 | (vn as i32) << 5 | vd as i32)
    }

    pub fn fcvtn(&mut self, vd: u8, vn: u8, lane: SIMDLane) {
        assert_eq!(lane, SIMDLane::F64X2);
        self.insn(0b00001110011000010110100000000000 | (vn as i32) << 5 | vd as i32)
    }

    pub fn uqxtn(&mut self, vd: u8, vn: u8, input_lane: SIMDLane) {
        let lane = input_lane.narrowed();
        self.insn(
            0b00101110001000010100100000000000
                | size_for_integral_simd_op(lane) << 22
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn sqxtn(&mut self, vd: u8, vn: u8, input_lane: SIMDLane) {
        let lane = input_lane.narrowed();
        self.insn(
            0b00001110001000010100100000000000
                | size_for_integral_simd_op(lane) << 22
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn sqxtn2(&mut self, vd: u8, vn: u8, input_lane: SIMDLane) {
        let lane = input_lane.narrowed();
        self.insn(
            0b01001110001000010100100000000000
                | size_for_integral_simd_op(lane) << 22
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn sqxtun(&mut self, vd: u8, vn: u8, input_lane: SIMDLane) {
        let lane = input_lane.narrowed();
        self.insn(
            0b00101110001000010010100000000000
                | size_for_integral_simd_op(lane) << 22
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn sqxtun2(&mut self, vn: u8, vd: u8, input_lane: SIMDLane) {
        let lane = input_lane.narrowed();
        self.insn(
            0b01101110001000010010100000000000
                | size_for_integral_simd_op(lane) << 22
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn ushl(&mut self, vd: u8, vn: u8, vm: u8, lane: SIMDLane) {
        self.insn(
            0b01101110001000000100010000000000
                | size_for_integral_simd_op(lane) << 22
                | (vm as i32) << 16
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn sshl(&mut self, vd: u8, vn: u8, vm: u8, lane: SIMDLane) {
        self.insn(
            0b01001110001000000100010000000000
                | size_for_integral_simd_op(lane) << 22
                | (vm as i32) << 16
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn sshr_vi(&mut self, vd: u8, vn: u8, mut shift: u8, lane: SIMDLane) {
        let max_shift = lane.element_byte_size() * 8;
        assert!((shift as i32) < max_shift && shift < 64 && shift != 0);
        shift = (max_shift - shift as i32) as u8;

        let immh = lane.element_byte_size() as usize | ((shift & 0b0111000) >> 3) as usize;
        let immb = shift as usize & 0b0111;

        self.insn(
            0b01001111000000000000010000000000
                | (immh as i32) << 19
                | (immb as i32) << 16
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn sqadd(&mut self, vd: u8, vn: u8, vm: u8, lane: SIMDLane) {
        self.insn(
            0b01001110001000000000110000000000
                | size_for_integral_simd_op(lane) << 22
                | (vm as i32) << 16
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn sqsub(&mut self, vd: u8, vn: u8, vm: u8, lane: SIMDLane) {
        self.insn(
            0b01001110001000000010110000000000
                | size_for_integral_simd_op(lane) << 22
                | (vm as i32) << 16
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn uqadd(&mut self, vd: u8, vn: u8, vm: u8, lane: SIMDLane) {
        self.insn(
            0b01101110001000000000110000000000
                | size_for_integral_simd_op(lane) << 22
                | (vm as i32) << 16
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn uqsub(&mut self, vd: u8, vn: u8, vm: u8, lane: SIMDLane) {
        self.insn(
            0b01101110001000000010110000000000
                | size_for_integral_simd_op(lane) << 22
                | (vm as i32) << 16
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn vector_fcvtzs(&mut self, vd: u8, vn: u8, lane: SIMDLane) {
        self.insn(
            0b01001110101000011011100000000000
                | (size_for_floating_point_simd(lane) as i32) << 22
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn vector_fcvtzu(&mut self, vd: u8, vn: u8, lane: SIMDLane) {
        self.insn(
            0b01101110101000011011100000000000
                | (size_for_floating_point_simd(lane) as i32) << 22
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn vector_frintp(&mut self, vd: u8, vn: u8, lane: SIMDLane) {
        self.insn(
            0b01001110101000011000100000000000
                | (size_for_floating_point_simd(lane) as i32) << 22
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn vector_frintn(&mut self, vd: u8, vn: u8, lane: SIMDLane) {
        self.insn(
            0b01001110001000011000100000000000
                | (size_for_floating_point_simd(lane) as i32) << 22
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn vector_frintm(&mut self, vd: u8, vn: u8, lane: SIMDLane) {
        self.insn(
            0b01001110001000011001100000000000
                | (size_for_floating_point_simd(lane) as i32) << 22
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn vector_scvtf(&mut self, vd: u8, vn: u8, lane: SIMDLane) {
        self.insn(
            0b01001110001000011101100000000000
                | (size_for_floating_point_simd(lane) as i32) << 22
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn vector_ucvtf(&mut self, vd: u8, vn: u8, lane: SIMDLane) {
        self.insn(
            0b01101110001000011101100000000000
                | (size_for_floating_point_simd(lane) as i32) << 22
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn vector_saddlp(&mut self, vd: u8, vn: u8, lane: SIMDLane) {
        self.insn(
            0b01001110001000000010100000000000
                | (size_for_integral_simd_op(lane) as i32) << 22
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn vector_uaddlp(&mut self, vd: u8, vn: u8, lane: SIMDLane) {
        self.insn(
            0b01101110001000000010100000000000
                | (size_for_integral_simd_op(lane) as i32) << 22
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn tbl(&mut self, vd: u8, vn: u8, vm: u8) {
        let len = 0;
        let q = 1;

        self.insn(
            0b00001110000000000000000000000000
                | q << 30
                | (vm as i32) << 16
                | len << 13
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn tbl2(&mut self, vd: u8, vn: u8, vn2: u8, vm: u8) {
        assert_eq!(vn2, vn + 1);
        let len = 1;
        let q = 1;

        self.insn(
            0b00001110000000000000000000000000
                | q << 30
                | (vm as i32) << 16
                | len << 13
                | (vn as i32) << 5
                | vd as i32,
        )
    }

    pub fn ld1r<const DATASIZE: i32>(&mut self, vt: u8, rn: u8) {
        let size_encoding: i32 = match DATASIZE {
            8 => 0,
            16 => 1,
            32 => 2,
            64 => 3,
            _ => unreachable!(),
        };

        self.insn(
            0b01001101010000001100000000000000
                | (size_encoding << 10)
                | (rn as i32) << 5
                | (vt as i32),
        )
    }

    pub fn ld1<const DATASIZE: i32>(&mut self, vt: u8, rn: u8, lane_index: i32) {
        let opcode;
        let q;
        let s;
        let size;

        match DATASIZE {
            8 => {
                opcode = 0;
                q = ((lane_index & 0b1000) == 0) as i32;
                s = ((lane_index & 0b0100) == 0) as i32;
                size = lane_index & 0b0011;
            }

            16 => {
                opcode = 1;
                q = ((lane_index & 0b100) == 0) as i32;
                s = ((lane_index & 0b010) == 0) as i32;
                size = (lane_index & 0b001) << 1;
            }

            32 => {
                opcode = 2;
                size = 0;
                q = ((lane_index & 0b10) == 0) as i32;
                s = (lane_index & 0b1) as i32;
            }

            64 => {
                opcode = 2;
                size = 1;
                s = 0;
                q = lane_index & 0b1;
            }

            _ => unreachable!(),
        }

        self.insn(
            0b00001101010000000000000000000000
                | q << 30
                | opcode << 14
                | s << 12
                | size << 10
                | (rn as i32) << 5
                | (vt as i32),
        )
    }

    pub fn st1<const DATASIZE: i32>(&mut self, vt: u8, rn: u8, lane_index: i32) {
        let opcode;
        let q;
        let s;
        let size;

        match DATASIZE {
            8 => {
                opcode = 0;
                q = ((lane_index & 0b1000) == 0) as i32;
                s = ((lane_index & 0b0100) == 0) as i32;
                size = lane_index & 0b0011;
            }

            16 => {
                opcode = 1;
                q = ((lane_index & 0b100) == 0) as i32;
                s = ((lane_index & 0b010) == 0) as i32;
                size = (lane_index & 0b001) << 1;
            }

            32 => {
                opcode = 2;
                size = 0;
                q = ((lane_index & 0b10) == 0) as i32;
                s = (lane_index & 0b1) as i32;
            }

            64 => {
                opcode = 2;
                size = 1;
                s = 0;
                q = lane_index & 0b1;
            }

            _ => unreachable!(),
        }
        self.insn(
            0b00001101000000000000000000000000
                | q << 30
                | opcode << 14
                | s << 12
                | size << 10
                | (rn as i32) << 5
                | (vt as i32),
        )
    }

    pub fn mov<const DATASIZE: i32>(&mut self, rd: u8, rm: u8) {
        if is_sp(rd) || is_sp(rm) {
            self.add_imm::<DATASIZE, false>(rd, rm, UInt12(0), 0);
        } else {
            self.orr::<DATASIZE>(rd, zr, rm);
        }
    }

    pub fn movi<const DATASIZE: i32>(&mut self, rd: u8, imm: LogicalImmediate) {
        assert!(imm.is_valid());
        self.orr_imm::<DATASIZE>(rd, zr, imm);
    }

    pub fn movi_fp<const DATASIZE: i32>(&mut self, rd: u8, imm: u8) {
        self.insn(Self::simd_move_immediate(
            DATASIZE == 128,
            true,
            0b1110,
            imm as _,
            rd,
        ))
    }

    pub fn movi8<const DATASIZE: i32>(&mut self, rd: u8, imm: u8) {
        self.insn(Self::simd_move_immediate(
            DATASIZE == 128,
            true,
            0b1110,
            imm as _,
            rd,
        ))
    }

    pub fn movk<const DATASIZE: i32>(&mut self, rd: u8, value: u16, shift: i32) {
        self.insn(Self::move_wide_immediate(
            datasize(DATASIZE),
            MoveWideOp::K,
            shift >> 4,
            value,
            rd,
        ))
    }

    pub fn movn<const DATASIZE: i32>(&mut self, rd: u8, value: u16, shift: i32) {
        self.insn(Self::move_wide_immediate(
            datasize(DATASIZE),
            MoveWideOp::N,
            shift >> 4,
            value,
            rd,
        ))
    }

    pub fn movz<const DATASIZE: i32>(&mut self, rd: u8, value: u16, shift: i32) {
        self.insn(Self::move_wide_immediate(
            datasize(DATASIZE),
            MoveWideOp::Z,
            shift >> 4,
            value,
            rd,
        ))
    }

    pub fn msub<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, rm: u8, ra: u8) {
        self.insn(Self::data_processing_3_source(
            datasize(DATASIZE),
            DataOp3Source::MSUB,
            rm,
            ra,
            rn,
            rd,
        ))
    }

    pub fn mul<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, rm: u8) {
        self.madd::<DATASIZE>(rd, rn, rm, zr)
    }

    pub fn mvn<const DATASIZE: i32>(&mut self, rd: u8, rm: u8) {
        self.orn::<DATASIZE>(rd, zr, rm);
    }

    pub fn mvn_shifted<const DATASIZE: i32>(
        &mut self,
        rd: u8,
        rm: u8,
        shift: ShiftType,
        amount: i32,
    ) {
        self.orn_shifted::<DATASIZE>(rd, zr, rm, shift, amount);
    }

    pub fn neg<const DATASIZE: i32, const S: bool>(&mut self, rd: u8, rm: u8) {
        self.sub::<DATASIZE, S>(rd, zr, rm);
    }

    pub fn neg_shifted<const DATASIZE: i32, const S: bool>(
        &mut self,
        rd: u8,
        rm: u8,
        shift: ShiftType,
        amount: i32,
    ) {
        self.sub_shifted::<DATASIZE, S>(rd, zr, rm, shift, amount);
    }

    pub fn ngc<const DATASIZE: i32, const S: bool>(&mut self, rd: u8, rm: u8) {
        self.sbc::<DATASIZE, S>(rd, zr, rm);
    }
    /*
    pub fn ngc_shifted<const DATASIZE: i32, const S: bool>(
        &mut self,
        rd: u8,
        rm: u8,
        shift: ShiftType,
        amount: i32,
    ) {
        self.sbc_shifted::<DATASIZE, S>(rd, zr, rm, shift, amount);
    }*/

    pub fn nop(&mut self) {
        self.insn(Self::nop_pseudo())
    }

    #[allow(unused_mut)]
    pub unsafe fn fill_nops(base: *mut u8, mut size: usize) {
        let mut n = size / size_of::<u32>();

        let mut ptr = base.cast::<i32>();
        while n != 0 {
            n -= 1;

            let insn = Self::nop_pseudo();
            *ptr = insn;
            ptr = ptr.add(1);
        }
    }

    pub fn dmb_ish(&mut self) {
        self.insn(0xd5033bbfu32 as i32);
    }

    pub fn dmb_ishst(&mut self) {
        self.insn(0xd5033abfu32 as i32);
    }

    pub fn ldar<const DATASIZE: i32>(&mut self, dst: u8, src: u8) {
        self.insn(Self::exotic_load(
            memopsize(DATASIZE),
            ExoticLoadFence::Acquire,
            ExoticLoadAtomic::None,
            dst,
            src,
        ))
    }

    pub fn ldxr<const DATASIZE: i32>(&mut self, dst: u8, src: u8) {
        self.insn(Self::exotic_load(
            memopsize(DATASIZE),
            ExoticLoadFence::None,
            ExoticLoadAtomic::Link,
            dst,
            src,
        ))
    }

    pub fn ldaxr<const DATASIZE: i32>(&mut self, dst: u8, src: u8) {
        self.insn(Self::exotic_load(
            memopsize(DATASIZE),
            ExoticLoadFence::Acquire,
            ExoticLoadAtomic::Link,
            dst,
            src,
        ))
    }

    pub fn stxr<const DATASIZE: i32>(&mut self, result: u8, src: u8, dst: u8) {
        self.insn(Self::exotic_store(
            memopsize(DATASIZE),
            ExoticStoreFence::None,
            result,
            src,
            dst,
        ))
    }

    pub fn stlr<const DATASIZE: i32>(&mut self, src: u8, dst: u8) {
        self.insn(Self::store_release(memopsize(DATASIZE), src, dst))
    }

    pub fn stlxr<const DATASIZE: i32>(&mut self, result: u8, src: u8, dst: u8) {
        self.insn(Self::exotic_store(
            memopsize(DATASIZE),
            ExoticStoreFence::Release,
            result,
            src,
            dst,
        ))
    }

    pub fn mrs_tpidrro_el0(&mut self, dst: u8) {
        self.insn(0xd53bd060u32 as i32 | dst as i32);
    }

    pub fn orn<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, rm: u8) {
        self.orn_shifted::<DATASIZE>(rd, rn, rm, ShiftType::LSL, 0);
    }

    pub fn orn_shifted<const DATASIZE: i32>(
        &mut self,
        rd: u8,
        rn: u8,
        rm: u8,
        shift: ShiftType,
        amount: i32,
    ) {
        self.insn(Self::logical_shifted_register(
            datasize(DATASIZE),
            LogicalOp::ORR,
            shift,
            true,
            rm,
            amount,
            rn,
            rd,
        ))
    }

    pub fn orr<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, rm: u8) {
        self.orr_shifted::<DATASIZE>(rd, rn, rm, ShiftType::LSL, 0);
    }

    pub fn orr_shifted<const DATASIZE: i32>(
        &mut self,
        rd: u8,
        rn: u8,
        rm: u8,
        shift: ShiftType,
        amount: i32,
    ) {
        self.insn(Self::logical_shifted_register(
            datasize(DATASIZE),
            LogicalOp::ORR,
            shift,
            false,
            rm,
            amount,
            rn,
            rd,
        ))
    }

    pub fn orr_imm<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, imm: LogicalImmediate) {
        self.insn(Self::logical_immediate(
            datasize(DATASIZE),
            LogicalOp::ORR,
            imm.value(),
            rn,
            rd,
        ))
    }

    pub fn rbit<const DATASIZE: i32>(&mut self, rd: u8, rn: u8) {
        self.insn(Self::data_processing_1_source(
            datasize(DATASIZE),
            DataOp1Source::RBIT,
            rn,
            rd,
        ))
    }

    pub fn ret(&mut self, rn: u8) {
        self.insn(Self::unconditional_branch_register(BranchType::RET, rn))
    }

    pub fn rev<const DATASIZE: i32>(&mut self, rd: u8, rn: u8) {
        if DATASIZE == 32 {
            self.insn(Self::data_processing_1_source(
                Datasize::D32,
                DataOp1Source::REV32,
                rn,
                rd,
            ))
        } else {
            self.insn(Self::data_processing_1_source(
                Datasize::D64,
                DataOp1Source::REV64,
                rn,
                rd,
            ))
        }
    }

    pub fn rev16<const DATASIZE: i32>(&mut self, rd: u8, rn: u8) {
        self.insn(Self::data_processing_1_source(
            datasize(DATASIZE),
            DataOp1Source::REV16,
            rn,
            rd,
        ))
    }

    pub fn rev32<const DATASIZE: i32>(&mut self, rd: u8, rn: u8) {
        assert_eq!(DATASIZE, 64);
        self.insn(Self::data_processing_1_source(
            Datasize::D64,
            DataOp1Source::REV32,
            rn,
            rd,
        ))
    }

    pub fn ror<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, rm: u8) {
        self.rorv::<DATASIZE>(rd, rn, rm);
    }

    pub fn ror_imm<const DATASIZE: i32>(&mut self, rd: u8, rs: u8, shift: i32) {
        self.extr::<DATASIZE>(rd, rs, rs, shift);
    }

    pub fn rorv<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, rm: u8) {
        self.insn(Self::data_processing_2_source(
            datasize(DATASIZE),
            rm,
            DataOp2Source::RORV,
            rn,
            rd,
        ))
    }

    pub fn sbc<const DATASIZE: i32, const S: bool>(&mut self, rd: u8, rn: u8, rm: u8) {
        self.insn(Self::add_subtract_with_carry(
            datasize(DATASIZE),
            AddOp::SUB,
            set_flags(S),
            rm,
            rn,
            rd,
        ))
    }

    pub fn sbfiz<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, lsb: i32, width: i32) {
        self.sbfm::<DATASIZE>(rd, rn, (DATASIZE - lsb) & (DATASIZE - 1), width - 1);
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

    pub fn sbfx<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, lsb: i32, width: i32) {
        self.sbfm::<DATASIZE>(rd, rn, lsb, lsb + width - 1);
    }

    pub fn sdiv<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, rm: u8) {
        self.insn(Self::data_processing_2_source(
            datasize(DATASIZE),
            rm,
            DataOp2Source::SDIV,
            rn,
            rd,
        ))
    }

    pub fn smaddl(&mut self, rd: u8, rn: u8, rm: u8, ra: u8) {
        self.insn(Self::data_processing_3_source(
            Datasize::D64,
            DataOp3Source::SMADDL,
            rm,
            ra,
            rn,
            rd,
        ))
    }

    pub fn smnegl(&mut self, rd: u8, rn: u8, rm: u8) {
        self.smsubl(rd, rn, rm, zr);
    }

    pub fn smsubl(&mut self, rd: u8, rn: u8, rm: u8, ra: u8) {
        self.insn(Self::data_processing_3_source(
            Datasize::D64,
            DataOp3Source::SMSUBL,
            rm,
            ra,
            rn,
            rd,
        ))
    }

    pub fn smulh(&mut self, rd: u8, rn: u8, rm: u8) {
        self.insn(Self::data_processing_3_source(
            Datasize::D64,
            DataOp3Source::SMULH,
            rm,
            zr,
            rn,
            rd,
        ))
    }

    pub fn smull(&mut self, rd: u8, rn: u8, rm: u8) {
        self.smaddl(rd, rn, rm, zr);
    }

    pub fn umull(&mut self, rd: u8, rn: u8, rm: u8) {
        self.umaddl(rd, rn, rm, zr);
    }

    pub fn stp_pair_post<const DATASIZE: i32>(
        &mut self,
        rt: u8,
        rt2: u8,
        rn: u8,
        simm: PairPostIndex,
    ) {
        self.insn(Self::load_store_register_pair_post_index_gp(
            mem_pair_op_size(DATASIZE),
            false,
            MemOp::STORE,
            simm.0,
            rn,
            rt,
            rt2,
        ))
    }

    pub fn stp_pair_pre<const DATASIZE: i32>(
        &mut self,
        rt: u8,
        rt2: u8,
        rn: u8,
        simm: PairPreIndex,
    ) {
        self.insn(Self::load_store_register_pair_pre_index_gp(
            mem_pair_op_size(DATASIZE),
            MemOp::STORE,
            simm.0,
            rn,
            rt,
            rt2,
        ))
    }

    pub fn stp_pair_imm<const DATASIZE: i32>(&mut self, rt: u8, rt2: u8, rn: u8, simm: i32) {
        self.insn(Self::load_store_register_pair_offset_gp(
            mem_pair_op_size(DATASIZE),
            false,
            MemOp::STORE,
            simm,
            rn,
            rt,
            rt2,
        ))
    }

    pub fn stnp_pair<const DATASIZE: i32>(&mut self, rt: u8, rt2: u8, rn: u8, simm: i32) {
        self.insn(Self::load_store_register_pair_non_temporal_gp(
            mem_pair_op_size(DATASIZE),
            false,
            MemOp::STORE,
            simm,
            rn,
            rt,
            rt2,
        ))
    }

    pub fn stp_pair_post_fp<const DATASIZE: i32>(
        &mut self,
        rt: u8,
        rt2: u8,
        rn: u8,
        simm: PairPostIndex,
    ) {
        self.insn(Self::load_store_register_pair_post_index(
            mem_pair_op_size_fp(DATASIZE),
            false,
            MemOp::STORE,
            simm.0,
            rn,
            rt,
            rt2,
        ))
    }

    pub fn stp_pair_pre_fp<const DATASIZE: i32>(
        &mut self,
        rt: u8,
        rt2: u8,
        rn: u8,
        simm: PairPreIndex,
    ) {
        self.insn(Self::load_store_register_pair_pre_index(
            mem_pair_op_size_fp(DATASIZE),
            true,
            MemOp::STORE,
            simm.0,
            rn,
            rt,
            rt2,
        ))
    }

    pub fn stnp_pair_fp<const DATASIZE: i32>(&mut self, rt: u8, rt2: u8, rn: u8, simm: i32) {
        self.insn(Self::load_store_register_pair_non_temporal(
            mem_pair_op_size_fp(DATASIZE),
            false,
            MemOp::STORE,
            simm,
            rn,
            rt,
            rt2,
        ))
    }

    pub fn stp_pair_fp<const DATASIZE: i32>(&mut self, rt: u8, rt2: u8, rn: u8, simm: i32) {
        self.insn(Self::load_store_register_pair_offset(
            mem_pair_op_size_fp(DATASIZE),
            true,
            MemOp::STORE,
            simm,
            rn,
            rt,
            rt2,
        ))
    }

    pub fn str<const DATASIZE: i32>(&mut self, rt: u8, rn: u8, rm: u8) {
        self.str_extend::<DATASIZE>(rt, rn, rm, ExtendType::UXTX, 0);
    }

    pub fn str_extend<const DATASIZE: i32>(
        &mut self,
        rt: u8,
        rn: u8,
        rm: u8,
        extend: ExtendType,
        amount: i32,
    ) {
        self.insn(Self::load_store_register_offset_gp(
            memopsize(DATASIZE),
            false,
            MemOp::STORE,
            rm,
            extend,
            encode_shift_amount::<DATASIZE>(amount) != 0,
            rn,
            rt,
        ))
    }

    pub fn str_imm<const DATASIZE: i32>(&mut self, rt: u8, rn: u8, pimm: i32) {
        self.insn(Self::load_store_register_unsigned_immediate_gp(
            memopsize(DATASIZE),
            false,
            MemOp::LOAD,
            pimm,
            rn,
            rt,
        ))
    }

    pub fn str_post<const DATASIZE: i32>(&mut self, rt: u8, rn: u8, simm: PostIndex) {
        self.insn(Self::load_store_register_post_index_gp(
            memopsize(DATASIZE),
            false,
            MemOp::STORE,
            simm.0,
            rn,
            rt,
        ))
    }

    pub fn str_pre<const DATASIZE: i32>(&mut self, rt: u8, rn: u8, simm: PreIndex) {
        self.insn(Self::load_store_register_pre_index_gp(
            memopsize(DATASIZE),
            true,
            MemOp::STORE,
            simm.0,
            rn,
            rt,
        ))
    }

    pub fn strb(&mut self, rt: u8, rn: u8, rm: u8) {
        self.insn(Self::load_store_register_offset_gp(
            MemOpSize::M8Or128,
            false,
            MemOp::STORE,
            rm,
            ExtendType::UXTX,
            false,
            rn,
            rt,
        ))
    }

    pub fn strb_extend(&mut self, rt: u8, rn: u8, rm: u8, extend: ExtendType, _amount: i32) {
        self.insn(Self::load_store_register_offset_gp(
            MemOpSize::M8Or128,
            false,
            MemOp::STORE,
            rm,
            extend,
            true,
            rn,
            rt,
        ))
    }

    pub fn strb_imm(&mut self, rt: u8, rn: u8, pimm: i32) {
        println!("strb {} {}, {}", rt, rn, pimm);
        self.insn(Self::load_store_register_unsigned_immediate_gp(
            MemOpSize::M8Or128,
            false,
            MemOp::STORE,
            pimm,
            rn,
            rt,
        ))
    }

    pub fn strb_post(&mut self, rt: u8, rn: u8, simm: PostIndex) {
        self.insn(Self::load_store_register_post_index_gp(
            MemOpSize::M8Or128,
            false,
            MemOp::STORE,
            simm.0,
            rn,
            rt,
        ))
    }

    pub fn strb_pre(&mut self, rt: u8, rn: u8, simm: PreIndex) {
        self.insn(Self::load_store_register_pre_index_gp(
            MemOpSize::M8Or128,
            true,
            MemOp::STORE,
            simm.0,
            rn,
            rt,
        ))
    }

    pub fn strh(&mut self, rt: u8, rn: u8, rm: u8) {
        self.strh_extend(rt, rn, rm, ExtendType::UXTX, 0);
    }

    pub fn strh_extend(&mut self, rt: u8, rn: u8, rm: u8, extend: ExtendType, amount: i32) {
        self.insn(Self::load_store_register_offset_gp(
            MemOpSize::M16,
            false,
            MemOp::STORE,
            rm,
            extend,
            amount == 1,
            rn,
            rt,
        ))
    }

    pub fn strh_imm(&mut self, rt: u8, rn: u8, pimm: i32) {
        self.insn(Self::load_store_register_unsigned_immediate_gp(
            MemOpSize::M16,
            false,
            MemOp::STORE,
            pimm,
            rn,
            rt,
        ))
    }

    pub fn strh_post(&mut self, rt: u8, rn: u8, simm: PostIndex) {
        self.insn(Self::load_store_register_post_index_gp(
            MemOpSize::M16,
            false,
            MemOp::STORE,
            simm.0,
            rn,
            rt,
        ))
    }

    pub fn strh_pre(&mut self, rt: u8, rn: u8, simm: PreIndex) {
        self.insn(Self::load_store_register_pre_index_gp(
            MemOpSize::M16,
            true,
            MemOp::STORE,
            simm.0,
            rn,
            rt,
        ))
    }

    pub fn stur<const DATASIZE: i32>(&mut self, rt: u8, rn: u8, simm: i32) {
        self.insn(Self::load_store_register_unscaled_immediate_gp(
            memopsize(DATASIZE),
            false,
            MemOp::STORE,
            simm,
            rn,
            rt,
        ));
    }

    pub fn sturb(&mut self, rt: u8, rn: u8, simm: i32) {
        self.insn(Self::load_store_register_unscaled_immediate_gp(
            MemOpSize::M8Or128,
            false,
            MemOp::STORE,
            simm,
            rn,
            rt,
        ));
    }

    pub fn sturh(&mut self, rt: u8, rn: u8, simm: i32) {
        self.insn(Self::load_store_register_unscaled_immediate_gp(
            MemOpSize::M16,
            false,
            MemOp::STORE,
            simm,
            rn,
            rt,
        ));
    }

    pub fn sub_imm<const DATASIZE: i32, const S: bool>(
        &mut self,
        rd: u8,
        rn: u8,
        imm12: UInt12,
        shift: i32,
    ) {
        self.insn(Self::add_subtract_immediate(
            datasize(DATASIZE),
            AddOp::SUB,
            set_flags(S),
            (shift == 12) as i32,
            imm12.0,
            rn,
            rd,
        ))
    }

    pub fn sub_shifted<const DATASIZE: i32, const SET_FLAGS: bool>(
        &mut self,
        rd: u8,
        rn: u8,
        rm: u8,
        shift: ShiftType,
        amount: i32,
    ) {
        self.insn(Self::add_subtract_shifted_registers(
            datasize(DATASIZE),
            AddOp::SUB,
            set_flags(SET_FLAGS),
            shift,
            rm,
            amount,
            rn,
            rd,
        ))
    }

    pub fn sub<const DATASIZE: i32, const SET_FLAGS: bool>(&mut self, rd: u8, rn: u8, rm: u8) {
        if is_sp(rd) || is_sp(rn) {
            self.sub_shifted::<DATASIZE, SET_FLAGS>(rd, rn, rm, ShiftType::LSL, 0)
        } else {
            self.sub_shifted::<DATASIZE, SET_FLAGS>(rd, rn, rm, ShiftType::LSL, 0)
        }
    }

    pub fn sub_extend<const DATASIZE: i32, const SET_FLAGS: bool>(
        &mut self,
        rd: u8,
        rn: u8,
        rm: u8,
        extend: ExtendType,
        amount: i32,
    ) {
        self.insn(Self::add_subtract_extended_register(
            datasize(DATASIZE),
            AddOp::SUB,
            set_flags(SET_FLAGS),
            rm,
            extend,
            amount,
            rn,
            rd,
        ))
    }

    pub fn sxtb<const DATASIZE: i32>(&mut self, rd: u8, rn: u8) {
        self.sbfm::<DATASIZE>(rd, rn, 0, 7);
    }

    pub fn sxth<const DATASIZE: i32>(&mut self, rd: u8, rn: u8) {
        self.sbfm::<DATASIZE>(rd, rn, 0, 15);
    }

    pub fn sxtw(&mut self, rd: u8, rn: u8) {
        self.sbfm::<64>(rd, rn, 0, 31);
    }

    pub fn tbz(&mut self, rt: u8, imm: i32, mut offset: i32) {
        offset >>= 2;

        self.insn(Self::test_and_branch_immediate(false, imm, offset, rt))
    }

    pub fn tbnz(&mut self, rt: u8, imm: i32, mut offset: i32) {
        offset >>= 2;

        self.insn(Self::test_and_branch_immediate(true, imm, offset, rt))
    }

    pub fn tst<const DATASIZE: i32>(&mut self, rn: u8, rm: u8) {
        self.and::<DATASIZE, true>(zr, rn, rm);
    }

    pub fn tst_shifted<const DATASIZE: i32>(
        &mut self,
        rn: u8,
        rm: u8,
        shift: ShiftType,
        amount: i32,
    ) {
        self.and_shifted::<DATASIZE, true>(zr, rn, rm, shift, amount);
    }

    pub fn tst_imm<const DATASIZE: i32>(&mut self, rn: u8, imm: LogicalImmediate) {
        self.and_imm::<DATASIZE, true>(zr, rn, imm);
    }

    pub fn ubfiz<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, lsb: i32, width: i32) {
        self.ubfm::<DATASIZE>(rd, rn, (DATASIZE - lsb) & (DATASIZE - 1), width - 1);
    }

    pub fn ubfm<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, immr: i32, imms: i32) {
        self.insn(Self::bitfield(
            datasize(DATASIZE),
            BitfieldOp::UBFM,
            immr,
            imms,
            rn,
            rd,
        ))
    }

    pub fn ubfx<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, lsb: i32, width: i32) {
        self.ubfm::<DATASIZE>(rd, rn, lsb, lsb + width - 1);
    }

    pub fn udiv<const DATASIZE: i32>(&mut self, rd: u8, rn: u8, rm: u8) {
        self.insn(Self::data_processing_2_source(
            datasize(DATASIZE),
            rm,
            DataOp2Source::UDIV,
            rn,
            rd,
        ))
    }

    pub fn umnegl(&mut self, rd: u8, rn: u8, rm: u8) {
        self.umsubl(rd, rn, rm, zr);
    }

    pub fn umsubl(&mut self, rd: u8, rn: u8, rm: u8, ra: u8) {
        self.insn(Self::data_processing_3_source(
            datasize(64),
            DataOp3Source::UMSUBL,
            rm,
            ra,
            rn,
            rd,
        ))
    }

    pub fn umaddl(&mut self, rd: u8, rn: u8, rm: u8, ra: u8) {
        self.insn(Self::data_processing_3_source(
            datasize(64),
            DataOp3Source::UMADDL,
            rm,
            ra,
            rn,
            rd,
        ))
    }

    pub fn umulh(&mut self, rd: u8, rn: u8, rm: u8, ra: u8) {
        self.insn(Self::data_processing_3_source(
            datasize(64),
            DataOp3Source::UMULH,
            rm,
            ra,
            rn,
            rd,
        ))
    }

    pub fn umulh_rr(&mut self, rd: u8, rn: u8, rm: u8) {
        self.umulh(rd, rn, rm, zr);
    }

    pub fn uxtb<const DATASIZE: i32>(&mut self, rd: u8, rn: u8) {
        self.ubfm::<DATASIZE>(rd, rn, 0, 7);
    }

    pub fn uxth<const DATASIZE: i32>(&mut self, rd: u8, rn: u8) {
        self.ubfm::<DATASIZE>(rd, rn, 0, 15);
    }

    pub fn uxtw(&mut self, rd: u8, rn: u8) {
        self.ubfm::<64>(rd, rn, 0, 31);
    }

    pub fn fabs<const DATASIZE: i32>(&mut self, vd: u8, vn: u8) {
        self.insn(Self::floating_point_data_processing_1_source(
            datasize(DATASIZE),
            FPDataOp1Source::FABS,
            vn,
            vd,
        ));
    }

    pub fn fadd<const DATASIZE: i32>(&mut self, vd: u8, vn: u8, vm: u8) {
        self.insn(Self::floating_point_data_processing_2_source(
            datasize(DATASIZE),
            FPDataOp2Source::FADD,
            vm,
            vn,
            vd,
        ));
    }

    pub fn fccmp<const DATASIZE: i32>(&mut self, vn: u8, vm: u8, _nzcv: i32, cond: Condition) {
        self.insn(Self::floating_point_conditional_compare(
            datasize(DATASIZE),
            vm,
            cond,
            vn,
            FPCondCmpOp::FCMP,
            _nzcv,
        ));
    }

    pub fn fccmpe<const DATASIZE: i32>(&mut self, vn: u8, vm: u8, _nzcv: i32, cond: Condition) {
        self.insn(Self::floating_point_conditional_compare(
            datasize(DATASIZE),
            vm,
            cond,
            vn,
            FPCondCmpOp::FCMPE,
            _nzcv,
        ));
    }

    pub fn fcmp<const DATASIZE: i32>(&mut self, vn: u8, vm: u8) {
        self.insn(Self::floating_point_compare(
            datasize(DATASIZE),
            vm,
            vn,
            FPCmpOp::FCMP,
        ));
    }

    pub fn fcmp_0<const DATASIZE: i32>(&mut self, vn: u8) {
        self.insn(Self::floating_point_compare(
            datasize(DATASIZE),
            0,
            vn,
            FPCmpOp::FCMP,
        ));
    }

    pub fn fcmpe<const DATASIZE: i32>(&mut self, vn: u8, vm: u8) {
        self.insn(Self::floating_point_compare(
            datasize(DATASIZE),
            vm,
            vn,
            FPCmpOp::FCMPE,
        ));
    }

    pub fn fcmpe_0<const DATASIZE: i32>(&mut self, vn: u8) {
        self.insn(Self::floating_point_compare(
            datasize(DATASIZE),
            0,
            vn,
            FPCmpOp::FCMPE,
        ));
    }

    pub fn fcsel<const DATASIZE: i32>(&mut self, vd: u8, vn: u8, vm: u8, cond: Condition) {
        self.insn(Self::floating_point_conditional_select(
            datasize(DATASIZE),
            vm,
            cond,
            vn,
            vd,
        ));
    }

    pub fn fcvt<const DSTSIZE: i32, const SRCSIZE: i32>(&mut self, vd: u8, vn: u8) {
        let ty = if SRCSIZE == 64 {
            Datasize::D64
        } else if SRCSIZE == 32 {
            Datasize::D32
        } else {
            Datasize::D16
        };

        let opcode = if DSTSIZE == 64 {
            FPDataOp1Source::FCVTToDouble
        } else if DSTSIZE == 32 {
            FPDataOp1Source::FCVTToSingle
        } else {
            FPDataOp1Source::FCVTToHalf
        };

        self.insn(Self::floating_point_data_processing_1_source(
            ty, opcode, vn, vd,
        ));
    }

    pub fn fcvtas<const DSTSIZE: i32, const SRCSIZE: i32>(&mut self, rd: u8, vn: u8) {
        self.insn(Self::floating_point_integer_conversion_f2i(
            datasize(DSTSIZE),
            datasize(SRCSIZE),
            FPIntConvOp::FCVTAS,
            vn,
            rd,
        ));
    }

    pub fn fcvtau<const DSTSIZE: i32, const SRCSIZE: i32>(&mut self, rd: u8, vn: u8) {
        self.insn(Self::floating_point_integer_conversion_f2i(
            datasize(DSTSIZE),
            datasize(SRCSIZE),
            FPIntConvOp::FCVTAU,
            vn,
            rd,
        ));
    }

    pub fn fcvtms<const DSTSIZE: i32, const SRCSIZE: i32>(&mut self, rd: u8, vn: u8) {
        self.insn(Self::floating_point_integer_conversion_f2i(
            datasize(DSTSIZE),
            datasize(SRCSIZE),
            FPIntConvOp::FCVTMS,
            vn,
            rd,
        ));
    }

    pub fn fcvtmu<const DSTSIZE: i32, const SRCSIZE: i32>(&mut self, rd: u8, vn: u8) {
        self.insn(Self::floating_point_integer_conversion_f2i(
            datasize(DSTSIZE),
            datasize(SRCSIZE),
            FPIntConvOp::FCVTMU,
            vn,
            rd,
        ));
    }

    pub fn fcvtns<const DSTSIZE: i32, const SRCSIZE: i32>(&mut self, rd: u8, vn: u8) {
        self.insn(Self::floating_point_integer_conversion_f2i(
            datasize(DSTSIZE),
            datasize(SRCSIZE),
            FPIntConvOp::FCVTNS,
            vn,
            rd,
        ));
    }

    pub fn fcvtnu<const DSTSIZE: i32, const SRCSIZE: i32>(&mut self, rd: u8, vn: u8) {
        self.insn(Self::floating_point_integer_conversion_f2i(
            datasize(DSTSIZE),
            datasize(SRCSIZE),
            FPIntConvOp::FCVTNU,
            vn,
            rd,
        ));
    }

    pub fn fcvtps<const DSTSIZE: i32, const SRCSIZE: i32>(&mut self, rd: u8, vn: u8) {
        self.insn(Self::floating_point_integer_conversion_f2i(
            datasize(DSTSIZE),
            datasize(SRCSIZE),
            FPIntConvOp::FCVTPS,
            vn,
            rd,
        ));
    }

    pub fn fcvtpu<const DSTSIZE: i32, const SRCSIZE: i32>(&mut self, rd: u8, vn: u8) {
        self.insn(Self::floating_point_integer_conversion_f2i(
            datasize(DSTSIZE),
            datasize(SRCSIZE),
            FPIntConvOp::FCVTPU,
            vn,
            rd,
        ));
    }

    pub fn fcvtzs<const DSTSIZE: i32, const SRCSIZE: i32>(&mut self, rd: u8, vn: u8) {
        self.insn(Self::floating_point_integer_conversion_f2i(
            datasize(DSTSIZE),
            datasize(SRCSIZE),
            FPIntConvOp::FCVTZS,
            vn,
            rd,
        ));
    }

    pub fn fcvtzu<const DSTSIZE: i32, const SRCSIZE: i32>(&mut self, rd: u8, vn: u8) {
        self.insn(Self::floating_point_integer_conversion_f2i(
            datasize(DSTSIZE),
            datasize(SRCSIZE),
            FPIntConvOp::FCVTZU,
            vn,
            rd,
        ));
    }

    pub fn fdiv<const DATASIZE: i32>(&mut self, vd: u8, vn: u8, vm: u8) {
        self.insn(Self::floating_point_data_processing_2_source(
            datasize(DATASIZE),
            FPDataOp2Source::FDIV,
            vm,
            vn,
            vd,
        ));
    }

    pub fn fmadd<const DATASIZE: i32>(&mut self, vd: u8, vn: u8, vm: u8, va: u8) {
        self.insn(Self::floating_point_data_processing_3_source(
            datasize(DATASIZE),
            false,
            vm,
            AddOp::ADD,
            va,
            vn,
            vd,
        ));
    }

    pub fn fmax<const DATASIZE: i32>(&mut self, vd: u8, vn: u8, vm: u8) {
        self.insn(Self::floating_point_data_processing_2_source(
            datasize(DATASIZE),
            FPDataOp2Source::FMAX,
            vm,
            vn,
            vd,
        ));
    }

    pub fn fmaxnm<const DATASIZE: i32>(&mut self, vd: u8, vn: u8, vm: u8) {
        self.insn(Self::floating_point_data_processing_2_source(
            datasize(DATASIZE),
            FPDataOp2Source::FMAXNM,
            vm,
            vn,
            vd,
        ));
    }

    pub fn fmin<const DATASIZE: i32>(&mut self, vd: u8, vn: u8, vm: u8) {
        self.insn(Self::floating_point_data_processing_2_source(
            datasize(DATASIZE),
            FPDataOp2Source::FMIN,
            vm,
            vn,
            vd,
        ));
    }

    pub fn fminnm<const DATASIZE: i32>(&mut self, vd: u8, vn: u8, vm: u8) {
        self.insn(Self::floating_point_data_processing_2_source(
            datasize(DATASIZE),
            FPDataOp2Source::FMINNM,
            vm,
            vn,
            vd,
        ));
    }

    pub fn fmov<const DATASIZE: i32>(&mut self, vd: u8, vn: u8) {
        self.insn(Self::floating_point_data_processing_1_source(
            datasize(DATASIZE),
            FPDataOp1Source::FMOV,
            vn,
            vd,
        ));
    }

    pub fn fmov_i2f<const DATASIZE: i32>(&mut self, rd: u8, vn: u8) {
        self.insn(Self::floating_point_integer_conversion_i2f(
            datasize(DATASIZE),
            datasize(DATASIZE),
            FPIntConvOp::FMOVX2Q,
            vn,
            rd,
        ));
    }

    pub fn fmov_f2i<const DATASIZE: i32>(&mut self, rd: u8, vn: u8) {
        self.insn(Self::floating_point_integer_conversion_f2i(
            datasize(DATASIZE),
            datasize(DATASIZE),
            FPIntConvOp::FMOVQ2X,
            vn,
            rd,
        ));
    }

    pub fn fmov_top_i2f(&mut self, rd: u8, vn: u8) {
        self.insn(Self::floating_point_integer_conversion_i2f(
            Datasize::D64,
            Datasize::D64,
            FPIntConvOp::FMOVX2QTOP,
            vn,
            rd,
        ));
    }

    pub fn fmov_top_f2i(&mut self, rd: u8, vn: u8) {
        self.insn(Self::floating_point_integer_conversion_f2i(
            Datasize::D64,
            Datasize::D64,
            FPIntConvOp::FMOVQ2XTOP,
            vn,
            rd,
        ));
    }

    pub fn fmsub<const DATASIZE: i32>(&mut self, vd: u8, vn: u8, vm: u8, va: u8) {
        self.insn(Self::floating_point_data_processing_3_source(
            datasize(DATASIZE),
            false,
            vm,
            AddOp::ADD,
            va,
            vn,
            vd,
        ));
    }

    pub fn fmul<const DATASIZE: i32>(&mut self, vd: u8, vn: u8, vm: u8) {
        self.insn(Self::floating_point_data_processing_2_source(
            datasize(DATASIZE),
            FPDataOp2Source::FMUL,
            vm,
            vn,
            vd,
        ));
    }

    pub fn fneg<const DATASIZE: i32>(&mut self, vd: u8, vn: u8) {
        self.insn(Self::floating_point_data_processing_1_source(
            datasize(DATASIZE),
            FPDataOp1Source::FNEG,
            vn,
            vd,
        ));
    }

    pub fn fnmadd<const DATASIZE: i32>(&mut self, vd: u8, vn: u8, vm: u8, va: u8) {
        self.insn(Self::floating_point_data_processing_3_source(
            datasize(DATASIZE),
            true,
            vm,
            AddOp::ADD,
            va,
            vn,
            vd,
        ));
    }

    pub fn fnmsub<const DATASIZE: i32>(&mut self, vd: u8, vn: u8, vm: u8, va: u8) {
        self.insn(Self::floating_point_data_processing_3_source(
            datasize(DATASIZE),
            true,
            vm,
            AddOp::SUB,
            va,
            vn,
            vd,
        ));
    }

    pub fn fnmmul<const DATASIZE: i32>(&mut self, vd: u8, vn: u8, vm: u8) {
        self.insn(Self::floating_point_data_processing_2_source(
            datasize(DATASIZE),
            FPDataOp2Source::FNMUL,
            vm,
            vn,
            vd,
        ));
    }

    pub fn vand<const DATASIZE: i32>(&mut self, vd: u8, vn: u8, vm: u8) {
        self.insn(Self::vector_data_processing_logical(
            DATASIZE,
            SIMD3SameLogical::AND,
            vm,
            vn,
            vd,
        ));
    }

    pub fn vorr<const DATASIZE: i32>(&mut self, vd: u8, vn: u8, vm: u8) {
        self.insn(Self::vector_data_processing_logical(
            DATASIZE,
            SIMD3SameLogical::ORR,
            vm,
            vn,
            vd,
        ));
    }

    pub fn vbic<const DATASIZE: i32>(&mut self, vd: u8, vn: u8, vm: u8) {
        self.insn(Self::vector_data_processing_logical(
            DATASIZE,
            SIMD3SameLogical::BIC,
            vm,
            vn,
            vd,
        ));
    }

    pub fn frinta<const DATASIZE: i32>(&mut self, vd: u8, vn: u8) {
        self.insn(Self::floating_point_data_processing_1_source(
            datasize(DATASIZE),
            FPDataOp1Source::FRINTA,
            vn,
            vd,
        ));
    }

    pub fn frinti<const DATASIZE: i32>(&mut self, vd: u8, vn: u8) {
        self.insn(Self::floating_point_data_processing_1_source(
            datasize(DATASIZE),
            FPDataOp1Source::FRINTI,
            vn,
            vd,
        ));
    }

    pub fn frintm<const DATASIZE: i32>(&mut self, vd: u8, vn: u8) {
        self.insn(Self::floating_point_data_processing_1_source(
            datasize(DATASIZE),
            FPDataOp1Source::FRINTM,
            vn,
            vd,
        ));
    }

    pub fn frintn<const DATASIZE: i32>(&mut self, vd: u8, vn: u8) {
        self.insn(Self::floating_point_data_processing_1_source(
            datasize(DATASIZE),
            FPDataOp1Source::FRINTN,
            vn,
            vd,
        ));
    }

    pub fn frintp<const DATASIZE: i32>(&mut self, vd: u8, vn: u8) {
        self.insn(Self::floating_point_data_processing_1_source(
            datasize(DATASIZE),
            FPDataOp1Source::FRINTP,
            vn,
            vd,
        ));
    }

    pub fn frintx<const DATASIZE: i32>(&mut self, vd: u8, vn: u8) {
        self.insn(Self::floating_point_data_processing_1_source(
            datasize(DATASIZE),
            FPDataOp1Source::FRINTX,
            vn,
            vd,
        ));
    }

    pub fn frintz<const DATASIZE: i32>(&mut self, vd: u8, vn: u8) {
        self.insn(Self::floating_point_data_processing_1_source(
            datasize(DATASIZE),
            FPDataOp1Source::FRINTZ,
            vn,
            vd,
        ));
    }

    pub fn fsqrt<const DATASIZE: i32>(&mut self, vd: u8, vn: u8) {
        self.insn(Self::floating_point_data_processing_1_source(
            datasize(DATASIZE),
            FPDataOp1Source::FSQRT,
            vn,
            vd,
        ));
    }

    pub fn fsub<const DATASIZE: i32>(&mut self, vd: u8, vn: u8, vm: u8) {
        self.insn(Self::floating_point_data_processing_2_source(
            datasize(DATASIZE),
            FPDataOp2Source::FSUB,
            vm,
            vn,
            vd,
        ));
    }

    pub fn ldr_fp<const DATASIZE: i32>(&mut self, rt: u8, rn: u8, rm: u8) {
        self.ldr_extend_fp::<DATASIZE>(rt, rn, rm, ExtendType::UXTX, 0);
    }

    pub fn ldr_extend_fp<const DATASIZE: i32>(
        &mut self,
        rt: u8,
        rn: u8,
        rm: u8,
        extend: ExtendType,
        amount: i32,
    ) {
        self.insn(Self::load_store_register_offset(
            memopsize(DATASIZE),
            true,
            if DATASIZE == 128 {
                MemOp::LAODV128
            } else {
                MemOp::LOAD
            },
            rm,
            extend,
            encode_shift_amount::<DATASIZE>(amount) != 0,
            rn,
            rt,
        ))
    }

    pub fn ldr_imm_fp<const DATASIZE: i32>(&mut self, rt: u8, rn: u8, pimm: i32) {
        self.insn(Self::load_store_register_unsigned_immediate(
            memopsize(DATASIZE),
            true,
            if DATASIZE == 128 {
                MemOp::LAODV128
            } else {
                MemOp::LOAD
            },
            encode_positive_immediate::<DATASIZE>(pimm as _),
            rn,
            rt,
        ))
    }

    pub fn ldr_post_fp<const DATASIZE: i32>(&mut self, rt: u8, rn: u8, imm: PostIndex) {
        self.insn(Self::load_store_register_post_index(
            memopsize(DATASIZE),
            true,
            if DATASIZE == 128 {
                MemOp::LAODV128
            } else {
                MemOp::LOAD
            },
            imm.0,
            rn,
            rt,
        ))
    }

    pub fn ldr_pre_fp<const DATASIZE: i32>(&mut self, rt: u8, rn: u8, imm: PreIndex) {
        self.insn(Self::load_store_register_pre_index(
            memopsize(DATASIZE),
            true,
            if DATASIZE == 128 {
                MemOp::LAODV128
            } else {
                MemOp::LOAD
            },
            imm.0,
            rn,
            rt,
        ))
    }

    pub fn ldr_literal_fp<const DATASIZE: i32>(&mut self, rt: u8, offset: i32) {
        self.insn(Self::load_register_literal(
            if DATASIZE == 128 {
                LdrLiteralOp::S128
            } else if DATASIZE == 64 {
                LdrLiteralOp::S64
            } else {
                LdrLiteralOp::S32
            },
            true,
            offset >> 2,
            rt,
        ))
    }

    pub fn ldur_fp<const DATASIZE: i32>(&mut self, rt: u8, rn: u8, simm: i32) {
        self.insn(Self::load_store_register_unscaled_immediate(
            memopsize(DATASIZE),
            true,
            if DATASIZE == 128 {
                MemOp::LAODV128
            } else {
                MemOp::LOAD
            },
            simm,
            rn,
            rt,
        ))
    }

    pub fn scvtf<const DSTSIZE: i32, const SRCSIZE: i32>(&mut self, vd: u8, rn: u8) {
        self.insn(Self::floating_point_integer_conversion_i2f(
            datasize(SRCSIZE),
            datasize(DSTSIZE),
            FPIntConvOp::SCVTF,
            rn,
            vd,
        ))
    }

    pub fn str_fp<const DATASIZE: i32>(&mut self, rt: u8, rn: u8, rm: u8) {
        self.str_extend_fp::<DATASIZE>(rt, rn, rm, ExtendType::UXTX, 0);
    }

    pub fn str_extend_fp<const DATASIZE: i32>(
        &mut self,
        rt: u8,
        rn: u8,
        rm: u8,
        extend: ExtendType,
        amount: i32,
    ) {
        self.insn(Self::load_store_register_offset(
            memopsize(DATASIZE),
            false,
            if DATASIZE == 128 {
                MemOp::STOREV128
            } else {
                MemOp::STORE
            },
            rm,
            extend,
            encode_shift_amount::<DATASIZE>(amount) != 0,
            rn,
            rt,
        ))
    }

    pub fn str_imm_fp<const DATASIZE: i32>(&mut self, rt: u8, rn: u8, pimm: i32) {
        self.insn(Self::load_store_register_unsigned_immediate(
            memopsize(DATASIZE),
            false,
            if DATASIZE == 128 {
                MemOp::STOREV128
            } else {
                MemOp::STORE
            },
            encode_positive_immediate::<DATASIZE>(pimm as _),
            rn,
            rt,
        ))
    }

    pub fn str_post_fp<const DATASIZE: i32>(&mut self, rt: u8, rn: u8, imm: PostIndex) {
        self.insn(Self::load_store_register_post_index(
            memopsize(DATASIZE),
            false,
            if DATASIZE == 128 {
                MemOp::STOREV128
            } else {
                MemOp::STORE
            },
            imm.0,
            rn,
            rt,
        ))
    }

    pub fn str_pre_fp<const DATASIZE: i32>(&mut self, rt: u8, rn: u8, imm: PreIndex) {
        self.insn(Self::load_store_register_pre_index(
            memopsize(DATASIZE),
            false,
            if DATASIZE == 128 {
                MemOp::STOREV128
            } else {
                MemOp::STORE
            },
            imm.0,
            rn,
            rt,
        ))
    }

    pub fn stur_fp<const DATASIZE: i32>(&mut self, rt: u8, rn: u8, simm: i32) {
        self.insn(Self::load_store_register_unscaled_immediate(
            memopsize(DATASIZE),
            false,
            if DATASIZE == 128 {
                MemOp::STOREV128
            } else {
                MemOp::STORE
            },
            simm,
            rn,
            rt,
        ))
    }

    pub fn ucvtf<const DSTSIZE: i32, const SRCSIZE: i32>(&mut self, vd: u8, rn: u8) {
        self.insn(Self::floating_point_integer_conversion_i2f(
            datasize(SRCSIZE),
            datasize(DSTSIZE),
            FPIntConvOp::UCVTF,
            rn,
            vd,
        ))
    }

    fn fjcvtzs_insn(dn: u8, rd: u8) -> i32 {
        0x1e7e0000 | (dn as i32) << 5 | (rd as i32)
    }

    pub fn fjcvtzs(&mut self, rd: u8, dn: u8) {
        self.insn(Self::fjcvtzs_insn(dn, rd))
    }

    const fn exotic_atomic_load_store(
        size: MemOpSize,
        op: ExoticAtomicLoadStore,
        load_fence: ExoticLoadFence,
        store_fence: ExoticStoreFence,
        rs: u8,
        rt: u8,
        rn: u8,
    ) -> i32 {
        0b00111000_00100000_00000000_00000000
            | (size as i32) << 30
            | (load_fence as i32) << 23
            | (store_fence as i32) << 22
            | (rs as i32) << 16
            | (op as i32) << 10
            | (rn as i32) << 5
            | (rt as i32)
    }

    const fn exotic_atomic_cas(
        size: MemOpSize,
        load_fence: ExoticLoadFence,
        store_fence: ExoticStoreFence,
        rs: u8,
        rt: u8,
        rn: u8,
    ) -> i32 {
        0b00001000_10100000_01111100_00000000
            | (size as i32) << 30
            | (store_fence as i32) << 22
            | (rs as i32) << 16
            | (load_fence as i32) << 15
            | (rn as i32) << 5
            | (rt as i32)
    }

    pub fn ldaddal<const DATASIZE: i32>(&mut self, rs: u8, rt: u8, rn: u8) {
        self.insn(Self::exotic_atomic_load_store(
            memopsize(DATASIZE),
            ExoticAtomicLoadStore::Add,
            ExoticLoadFence::Acquire,
            ExoticStoreFence::Release,
            rs,
            rt,
            rn,
        ))
    }

    pub fn ldeoral<const DATASIZE: i32>(&mut self, rs: u8, rt: u8, rn: u8) {
        self.insn(Self::exotic_atomic_load_store(
            memopsize(DATASIZE),
            ExoticAtomicLoadStore::Xor,
            ExoticLoadFence::Acquire,
            ExoticStoreFence::Release,
            rs,
            rt,
            rn,
        ))
    }

    pub fn ldclral<const DATASIZE: i32>(&mut self, rs: u8, rt: u8, rn: u8) {
        self.insn(Self::exotic_atomic_load_store(
            memopsize(DATASIZE),
            ExoticAtomicLoadStore::Clear,
            ExoticLoadFence::Acquire,
            ExoticStoreFence::Release,
            rs,
            rt,
            rn,
        ))
    }

    pub fn ldsetal<const DATASIZE: i32>(&mut self, rs: u8, rt: u8, rn: u8) {
        self.insn(Self::exotic_atomic_load_store(
            memopsize(DATASIZE),
            ExoticAtomicLoadStore::Set,
            ExoticLoadFence::Acquire,
            ExoticStoreFence::Release,
            rs,
            rt,
            rn,
        ))
    }

    pub fn swpal<const DATASIZE: i32>(&mut self, rs: u8, rt: u8, rn: u8) {
        self.insn(Self::exotic_atomic_load_store(
            memopsize(DATASIZE),
            ExoticAtomicLoadStore::Swap,
            ExoticLoadFence::Acquire,
            ExoticStoreFence::Release,
            rs,
            rt,
            rn,
        ))
    }

    pub fn casal<const DATASIZE: i32>(&mut self, rs: u8, rt: u8, rn: u8) {
        self.insn(Self::exotic_atomic_cas(
            memopsize(DATASIZE),
            ExoticLoadFence::Acquire,
            ExoticStoreFence::Release,
            rs,
            rt,
            rn,
        ))
    }

    const fn x_or_sp(r: u8) -> i32 {
        r as i32
    }

    const fn x_or_zr(r: u8) -> i32 {
        (r & 31) as i32
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
        size: MemPairOpSize,
        v: bool,
        opc: MemOp,
        imm9: i32,
        rn: u8,
        rt: u8,
        rt2: u8,
    ) -> i32 {
        let immed_shift_amount = mem_pair_offset_shift(v, size);
        let imm7 = imm9 >> immed_shift_amount;
        0x29800000
            | (size as i32) << 30
            | (v as i32) << 26
            | (opc as i32) << 22
            | (imm7 & 0x7f) << 15
            | (rt2 as i32) << 10
            | Self::x_or_sp(rn) << 5
            | (rt as i32)
    }

    const fn load_store_register_pair_pre_index_gp(
        size: MemPairOpSize,
        opc: MemOp,
        imm9: i32,
        rn: u8,
        rt: u8,
        rt2: u8,
    ) -> i32 {
        Self::load_store_register_pair_pre_index(
            size,
            false,
            opc,
            imm9,
            rn,
            Self::x_or_zr(rt) as _,
            Self::x_or_zr(rt2) as _,
        )
    }

    const fn load_store_register_pre_index(
        size: MemOpSize,
        v: bool,
        opc: MemOp,
        imm9: i32,
        rn: u8,
        rt: u8,
    ) -> i32 {
        0x38000c00
            | (size as i32) << 30
            | (v as i32) << 26
            | (opc as i32) << 22
            | (imm9 & 0x1ff) << 12
            | Self::x_or_sp(rn) << 5
            | (rt as i32)
    }

    const fn load_store_register_pre_index_gp(
        size: MemOpSize,
        v: bool,
        opc: MemOp,
        imm9: i32,
        rn: u8,
        rt: u8,
    ) -> i32 {
        Self::load_store_register_pre_index(size, v, opc, imm9, rn, Self::x_or_zr(rt) as _)
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

    fn load_store_register_offset(
        size: MemOpSize,
        v: bool,
        opc: MemOp,
        rm: u8,
        option: ExtendType,
        s: bool,
        rn: u8,
        rt: u8,
    ) -> i32 {
        assert!(option as usize & 2 != 0);
        println!("ldr x{} [x{}, x{}]", rt, rn, rm);
        let x = 0x38200800  | (size as i32) << 30
        | ((v as i32) << 26);
        //| ((opc as i32) << 22);
        println!("x = {:x}", x);
        0x38200800
            | (size as i32) << 30
            | ((v as i32) << 26)
            | ((opc as i32) << 22)
            | (Self::x_or_zr(rm) << 16)
            | ((option as i32) << 13)
            | ((s as i32) << 12)
            | (Self::x_or_sp(rn) << 5)
            | (rt as i32)
    }

    fn load_store_register_offset_gp(
        size: MemOpSize,
        v: bool,
        opc: MemOp,
        rm: u8,
        option: ExtendType,
        s: bool,
        rn: u8,
        rt: u8,
    ) -> i32 {
        Self::load_store_register_offset(size, v, opc, rm, option, s, rn, Self::x_or_zr(rt) as _)
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

    const fn load_store_register_unsigned_immediate(
        size: MemOpSize,
        v: bool,
        opc: MemOp,
        imm12: i32,
        rn: u8,
        rd: u8,
    ) -> i32 {
        0x39000000
            | (size as i32) << 30
            | (v as i32) << 26
            | (opc as i32) << 22
            | (imm12 & 0xfff) << 10
            | Self::x_or_sp(rn) << 5
            | (rd as i32)
    }

    const fn load_store_register_unsigned_immediate_gp(
        size: MemOpSize,
        v: bool,
        opc: MemOp,
        imm9: i32,
        rn: u8,
        rd: u8,
    ) -> i32 {
        Self::load_store_register_unsigned_immediate(size, v, opc, imm9, rn, Self::x_or_zr(rd) as _)
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

    unsafe fn disassemble_unconditional_branch_immediate(addr: *const u8) -> Option<(bool, i32)> {
        let insn = addr.cast::<i32>().read();
        let op = (insn >> 31) & 1;
        let imm26 = (insn << 6) >> 6;

        ((insn & 0x7c000000) == 0x14000000).then(|| (op == 1, imm26))
    }

    unsafe fn disassemble_test_and_branch_immediate(
        addr: *const u8,
    ) -> Option<(bool, usize, i32, u8)> {
        let insn = addr.cast::<i32>().read();
        let op = (insn >> 24) & 0x1;
        let imm14 = (insn << 13) >> 18;
        let bit_number = (insn >> 26) & 0x20 | ((insn >> 19) & 0x1f);
        let rt = insn & 0x1f;

        ((insn & 0x7e000000) == 0x36000000).then(|| (op == 1, bit_number as usize, imm14, rt as u8))
    }

    unsafe fn disassemble_conditional_branch_immediate(
        addr: *const u8,
    ) -> Option<(usize, i32, Condition)> {
        let insn = addr.cast::<i32>().read();

        let op01 = ((insn >> 23) & 0x2) | ((insn >> 4) & 0x1);
        let imm19 = (insn << 8) >> 13;
        let condition: Condition = transmute((insn & 0xf) as u8);

        ((insn as u32 & 0xfe000000u32) == 0x54000000).then(|| (op01 as usize, imm19, condition))
    }

    unsafe fn disassemble_compare_and_branch_immediate(
        addr: *const u8,
    ) -> Option<(Datasize, bool, i32, u8)> {
        let insn = addr.cast::<i32>().read();
        let sf: Datasize = transmute(((insn >> 31) & 0x1) as u8);
        let op = (insn >> 24) & 0x1;
        let imm19 = (insn << 8) >> 13;
        let rt = insn & 0x1f;

        ((insn & 0x7e000000) == 0x34000000).then(|| (sf, op == 1, imm19, rt as u8))
    }

    unsafe fn disassemble_nop(addr: *const u8) -> bool {
        let insn = addr.cast::<u32>().read();
        insn == 0xd503201fu32
    }

    const fn disassemble_x_or_sp(reg: i32) -> u8 {
        if reg == 31 {
            sp
        } else {
            reg as u8
        }
    }

    const fn disassemble_x_or_zr(reg: i32) -> u8 {
        if reg == 31 {
            zr
        } else {
            reg as u8
        }
    }

    const fn disassemble_x_or_zr_or_sp(reg: i32, use_zr: bool) -> u8 {
        if reg == 31 {
            if !use_zr {
                sp
            } else {
                zr
            }
        } else {
            reg as u8
        }
    }

    unsafe fn disassemble_move_wide_immediate(
        address: *const u8,
    ) -> Option<(Datasize, MoveWideOp, i32, u16, u8)> {
        let insn = address.cast::<i32>().read();
        let sf = transmute(((insn >> 31) & 0x1) as u8);
        let opc = transmute(((insn >> 29) & 0x3) as u8);
        let hw = (insn >> 21) & 0x3;
        let imm16 = insn >> 5;
        let rd = Self::disassemble_x_or_zr(insn & 0x1f);

        ((insn & 0x1f800000) == 0x12800000).then(|| (sf, opc, hw, imm16 as u16, rd))
    }

    unsafe fn disassemble_load_store_register_insigned_immediate(
        address: *const u8,
    ) -> Option<(MemOpSize, bool, MemOp, i32, u8, u8)> {
        let insn = address.cast::<i32>().read();
        let size = transmute(((insn >> 30) & 0x3) as u8);
        let v = ((insn >> 26) & 0x1) == 1;
        let opc = transmute(((insn >> 22) & 0x3) as u8);
        let imm9 = (insn >> 10) & 0xfff;
        let rn = Self::disassemble_x_or_sp((insn >> 5) & 0x1f);
        let rt = Self::disassemble_x_or_zr(insn & 0x1f);

        ((insn & 0x3b000000) == 0x39000000).then(|| (size, v, opc, imm9, rn, rt))
    }

    unsafe fn disassemble_add_subtract_immediate(
        addr: *const u8,
    ) -> Option<(Datasize, AddOp, SetFlags, i32, i32, u8, u8)> {
        let insn = addr.cast::<i32>().read();
        let sf = transmute(((insn >> 31) & 0x1) as u8);
        let op = transmute(((insn >> 30) & 0x1) as u8);
        let s = transmute(((insn >> 29) & 0x1) as u8);
        let shift = (insn >> 22) & 3;
        let imm12 = (insn >> 10) & 0x3ff;

        let rn = Self::disassemble_x_or_sp((insn >> 5) & 0x1f);
        let rd = Self::disassemble_x_or_zr_or_sp(insn & 0x1f, s == SetFlags::S);

        ((insn & 0x1f000000) == 0x11000000).then(|| (sf, op, s, shift, imm12, rn, rd))
    }

    pub unsafe fn relink_jump_or_call(
        typ: BranchType,
        from: *mut i32,
        from_instruction: *const i32,
        to: *mut u8,
    ) {
        if typ == BranchType::JMP || typ == BranchType::CALL {
            if let Some((op01, imm19, mut cond)) =
                Self::disassemble_conditional_branch_immediate(from.sub(1).cast())
            {
                if imm19 == 8 {
                    cond = cond.invert();
                }

                Self::link_conditional_branch(
                    false,
                    cond,
                    from.sub(1).cast(),
                    from_instruction.sub(1),
                    to,
                );

                return;
            }

            if let Some((opsize, mut op, imm14, rt)) =
                Self::disassemble_compare_and_branch_immediate(from.sub(1).cast())
            {
                if imm14 == 8 {
                    op = !op;
                }

                Self::link_compare_and_branch(
                    false,
                    if op { Condition::NE } else { Condition::EQ },
                    opsize == Datasize::D64,
                    rt,
                    from.sub(1),
                    from_instruction.sub(1),
                    to,
                );

                return;
            }

            if let Some((mut op, bit_number, imm14, rt)) =
                Self::disassemble_test_and_branch_immediate(from.sub(1).cast())
            {
                if imm14 == 8 {
                    op = !op;
                }

                Self::link_test_and_branch(
                    false,
                    if op { Condition::NE } else { Condition::EQ },
                    bit_number,
                    rt,
                    from.sub(1),
                    from_instruction.sub(1),
                    to,
                );

                return;
            }
        }

        Self::link_jump_or_call(typ, from, from_instruction, to);
    }

    pub unsafe fn link_pointer_raw(address: *mut i32, value_ptr: *const u8, flush: bool) {
        let (_, _, _, imm16, rd) = Self::disassemble_move_wide_immediate(address.cast()).unwrap();

        Self::set_pointer(address, value_ptr, rd, flush);
    }
    #[allow(unused_assignments)]
    pub unsafe fn link_jump_or_call(
        typ: BranchType,
        from: *mut i32,
        from_instruction: *const i32,
        to: *const u8,
    ) {
        let unconditional_branch = Self::disassemble_unconditional_branch_immediate(from.cast());
        let mut link = false;
        let mut imm26 = 0;

        if let Some((l, i)) = unconditional_branch {
            link = l;
            imm26 = i;
        } else {
            assert!(
                Self::disassemble_nop(from.cast()),
                "nop expected at {:p}",
                from
            );
        }

        let is_call = typ == BranchType::CALL;

        let offset = (to as isize).wrapping_sub(from_instruction as isize) >> 2;

        let insn = Self::unconditional_branch_immediate(is_call, offset as _);

        from.write_volatile(insn);
    }

    pub unsafe fn link_compare_and_branch(
        direct: bool,
        cond: Condition,
        is64bit: bool,
        rt: u8,
        from: *mut i32,
        from_instruction: *const i32,
        to: *const u8,
    ) {
        let offset = (to as isize).wrapping_sub(from_instruction as isize) >> 2;
        let use_direct = is_int::<19>(offset as _);

        if use_direct || direct {
            let insn = Self::compare_and_branch_immediate(
                if is64bit {
                    Datasize::D64
                } else {
                    Datasize::D32
                },
                cond == Condition::NE,
                offset as _,
                rt,
            );

            from.write_volatile(insn);

            if !direct {
                let insn = Self::nop_pseudo();
                from.add(1).write_volatile(insn);
            }
        } else {
            let insn = Self::compare_and_branch_immediate(
                if is64bit {
                    Datasize::D64
                } else {
                    Datasize::D32
                },
                cond.invert() == Condition::NE,
                2,
                rt,
            );

            from.write_volatile(insn);

            Self::link_jump_or_call(BranchType::JMP, from.add(1), from_instruction.add(1), to);
        }
    }

    pub unsafe fn link_conditional_branch(
        direct: bool,
        cond: Condition,
        from: *mut i32,
        from_instruction: *const i32,
        to: *const u8,
    ) {
        let offset = (to as isize).wrapping_sub(from_instruction as isize) >> 2;

        let use_direct = is_int::<19>(offset as _);

        if use_direct || direct {
            let insn = Self::conditional_branch_immediate(offset as _, cond);
            from.write_volatile(insn);

            if !direct {
                let insn = Self::nop_pseudo();
                from.add(1).write_volatile(insn);
            }
        } else {
            let insn = Self::conditional_branch_immediate(2, cond.invert());
            from.write_volatile(insn);

            Self::link_jump_or_call(BranchType::JMP, from.add(1), from_instruction.add(1), to);
        }
    }

    pub unsafe fn link_test_and_branch(
        direct: bool,
        cond: Condition,
        bit_number: usize,
        rt: u8,
        from: *mut i32,
        from_instruction: *const i32,
        to: *const u8,
    ) {
        let offset = (to as isize).wrapping_sub(from_instruction as isize) >> 2;

        let use_direct = is_int::<14>(offset as _);

        if use_direct || direct {
            let insn = Self::test_and_branch_immediate(
                cond == Condition::NE,
                bit_number as _,
                offset as _,
                rt,
            );

            from.write_volatile(insn);

            if !direct {
                let insn = Self::nop_pseudo();
                from.add(1).write_volatile(insn);
            }
        } else {
            let insn = Self::test_and_branch_immediate(
                cond.invert() == Condition::NE,
                bit_number as _,
                2,
                rt,
            );

            from.write_volatile(insn);

            Self::link_jump_or_call(BranchType::JMP, from.add(1), from_instruction.add(1), to);
        }
    }

    pub const fn patchable_jump_size() -> usize {
        4
    }

    pub unsafe fn set_pointer(address: *mut i32, value_ptr: *const u8, rd: u8, flush: bool) {
        let value = value_ptr as usize as u64;
        let mut buffer = vec![0i32; NUMBER_OF_ADDRESS_ENCODING_INSTRUCTIONS];
        buffer[0] =
            Self::move_wide_immediate(Datasize::D64, MoveWideOp::Z, 0, get_half_word(value, 0), rd);
        buffer[1] =
            Self::move_wide_immediate(Datasize::D64, MoveWideOp::K, 1, get_half_word(value, 1), rd);

        if NUMBER_OF_ADDRESS_ENCODING_INSTRUCTIONS > 2 {
            buffer[2] = Self::move_wide_immediate(
                Datasize::D64,
                MoveWideOp::K,
                2,
                get_half_word(value, 2),
                rd,
            );
        }

        if NUMBER_OF_ADDRESS_ENCODING_INSTRUCTIONS > 3 {
            buffer[3] = Self::move_wide_immediate(
                Datasize::D64,
                MoveWideOp::K,
                3,
                get_half_word(value, 3),
                rd,
            );
        }

        core::ptr::copy_nonoverlapping(
            buffer.as_ptr(),
            address,
            NUMBER_OF_ADDRESS_ENCODING_INSTRUCTIONS,
        );
    }

    pub unsafe fn read_pointer(at: *mut u8) -> *const u8 {
        let address = at.cast::<i32>();

        let (_, _, _, imm16, rd_first) =
            Self::disassemble_move_wide_immediate(address.cast()).unwrap();

        let mut result = imm16 as usize;

        let (_, _, _, imm16, rd) =
            Self::disassemble_move_wide_immediate(address.add(1).cast()).unwrap();

        result |= (imm16 as usize) << 16;

        if NUMBER_OF_ADDRESS_ENCODING_INSTRUCTIONS > 2 {
            let (_, _, _, imm16, _) =
                Self::disassemble_move_wide_immediate(address.add(2).cast()).unwrap();

            result |= (imm16 as usize) << 32;
        }

        if NUMBER_OF_ADDRESS_ENCODING_INSTRUCTIONS > 3 {
            let (_, _, _, imm16, _) =
                Self::disassemble_move_wide_immediate(address.add(3).cast()).unwrap();

            result |= (imm16 as usize) << 48;
        }

        result as *const u8
    }

    pub unsafe fn read_call_target(from: *mut u8) -> *const u8 {
        Self::read_pointer(
            from.cast::<i32>()
                .sub(1)
                .sub(NUMBER_OF_ADDRESS_ENCODING_INSTRUCTIONS)
                .cast(),
        )
    }

    pub fn can_emit_jump(from: *const u8, to: *const u8) -> bool {
        let diff = (to as isize).wrapping_sub(from as isize);

        is_int::<26>(diff as _)
    }

    pub unsafe fn link(
        record: &LinkRecord,
        from: *mut u8,
        from_instruction: *const u8,
        to: *mut u8,
    ) {
        let from_instruction = from_instruction.cast::<i32>();

        match record.link_type {
            JumpLinkType::NoCondition => {
                Self::link_jump_or_call(BranchType::JMP, from.cast(), from_instruction, to);
            }

            JumpLinkType::ConditionDirect => {
                Self::link_conditional_branch(
                    true,
                    record.condition,
                    from.cast(),
                    from_instruction,
                    to,
                );
            }

            JumpLinkType::Condition => {
                Self::link_conditional_branch(
                    false,
                    record.condition,
                    from.cast::<i32>().sub(1),
                    from_instruction.sub(1),
                    to,
                );
            }

            JumpLinkType::CompareAndBranchDirect => {
                Self::link_compare_and_branch(
                    true,
                    record.condition,
                    record.is_64bit,
                    record.compare_register,
                    from.cast(),
                    from_instruction,
                    to,
                );
            }

            JumpLinkType::CompareAndBranch => {
                Self::link_compare_and_branch(
                    false,
                    record.condition,
                    record.is_64bit,
                    record.compare_register,
                    from.cast(),
                    from_instruction,
                    to,
                );
            }

            JumpLinkType::TestBitDirect => {
                Self::link_test_and_branch(
                    true,
                    record.condition,
                    record.bit_number as _,
                    record.compare_register,
                    from.cast(),
                    from_instruction,
                    to,
                );
            }

            JumpLinkType::TestBit => {
                Self::link_test_and_branch(
                    false,
                    record.condition,
                    record.bit_number as _,
                    record.compare_register,
                    from.cast(),
                    from_instruction,
                    to,
                );
            }

            JumpLinkType::Invalid => {
                unreachable!("Invalid jump link type");
            }
        }
    }

    pub fn jumps_to_link(&mut self) -> &[LinkRecord] {
        self.jumps_to_link.sort_by(|a, b| a.from.cmp(&b.from));

        &self.jumps_to_link
    }

    pub fn compute_jump_type_raw(jt: JumpType, from: *const u8, to: *const u8) -> JumpLinkType {
        match jt {
            JumpType::Fixed => JumpLinkType::Invalid,
            JumpType::NoConditionFixedSize => JumpLinkType::NoCondition,
            JumpType::ConditionFixedSize => JumpLinkType::Condition,
            JumpType::CompareAndBranchFixedSize => JumpLinkType::CompareAndBranch,
            JumpType::TestBitFixedSize => JumpLinkType::TestBit,
            JumpType::NoCondition => JumpLinkType::NoCondition,
            JumpType::Condition => {
                let relative = (to as isize).wrapping_sub(from as isize);

                if is_int::<21>(relative as _) {
                    JumpLinkType::ConditionDirect
                } else {
                    JumpLinkType::Condition
                }
            }

            JumpType::CompareAndBranch => {
                let relative = (to as isize).wrapping_sub(from as isize);

                if is_int::<19>(relative as _) {
                    JumpLinkType::CompareAndBranchDirect
                } else {
                    JumpLinkType::CompareAndBranch
                }
            }

            JumpType::TestBit => {
                let relative = (to as isize).wrapping_sub(from as isize);

                if is_int::<14>(relative as _) {
                    JumpLinkType::TestBitDirect
                } else {
                    JumpLinkType::TestBit
                }
            }
        }
    }

    pub fn compute_jump_type(
        record: &mut LinkRecord,
        from: *const u8,
        to: *const u8,
    ) -> JumpLinkType {
        let link_type = Self::compute_jump_type_raw(record.typ, from, to);
        record.link_type = link_type;
        link_type
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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(u32)]
pub enum Group1Op {
    PACIA1716 = 0b0001 << 8 | 0b000 << 5,
    PACIB1716 = 0b0001 << 8 | 0b010 << 5,
    AUTIA1716 = 0b0001 << 8 | 0b100 << 5,
    AUTIB1716 = 0b0001 << 8 | 0b110 << 5,
    PACIAZ = 0b0011 << 8 | 0b000 << 5,
    PACIASP = 0b0011 << 8 | 0b001 << 5,
    PACIBZ = 0b0011 << 8 | 0b010 << 5,
    PACIBSP = 0b0011 << 8 | 0b011 << 5,
    AUTIAZ = 0b0011 << 8 | 0b100 << 5,
    AUTIASP = 0b0011 << 8 | 0b101 << 5,
    AUTIBZ = 0b0011 << 8 | 0b110 << 5,
    AUTIBSP = 0b0011 << 8 | 0b111 << 5,
    XPACLRI = 0b0000 << 8 | 0b111 << 5,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(u32)]
pub enum Group2Op {
    PACIA = 1 << 30 | 0b00001 << 16 | 0b00000 << 10,
    PACIB = 1 << 30 | 0b00001 << 16 | 0b00001 << 10,
    PACDA = 1 << 30 | 0b00001 << 16 | 0b00010 << 10,
    PACDB = 1 << 30 | 0b00001 << 16 | 0b00011 << 10,
    AUTIA = 1 << 30 | 0b00001 << 16 | 0b00100 << 10,
    AUTIB = 1 << 30 | 0b00001 << 16 | 0b00101 << 10,
    AUTDA = 1 << 30 | 0b00001 << 16 | 0b00110 << 10,
    AUTDB = 1 << 30 | 0b00001 << 16 | 0b00111 << 10,
    PACIZA = 1 << 30 | 0b00001 << 16 | 0b01000 << 10,
    PACIZB = 1 << 30 | 0b00001 << 16 | 0b01001 << 10,
    PACDZA = 1 << 30 | 0b00001 << 16 | 0b01010 << 10,
    PACDZB = 1 << 30 | 0b00001 << 16 | 0b01011 << 10,
    AUTIZA = 1 << 30 | 0b00001 << 16 | 0b01100 << 10,
    AUTIZB = 1 << 30 | 0b00001 << 16 | 0b01101 << 10,
    AUTDZA = 1 << 30 | 0b00001 << 16 | 0b01110 << 10,
    AUTDZB = 1 << 30 | 0b00001 << 16 | 0b01111 << 10,
    XPACI = 1 << 30 | 0b00001 << 16 | 0b10000 << 10,
    XPACD = 1 << 30 | 0b00001 << 16 | 0b10001 << 10,

    PACGA = 0 << 30 | 0b01100 << 10,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(u32)]
pub enum Group4Op {
    BRAA = 0b1000 << 21 | 0 << 10,
    BRAB = 0b1000 << 21 | 1 << 10,
    BLRAA = 0b1001 << 21 | 0 << 10,
    BLRAB = 0b1001 << 21 | 1 << 10,

    BRAAZ = 0b0000 << 21 | 0 << 10,
    BRABZ = 0b0000 << 21 | 1 << 10,
    BLRAAZ = 0b0001 << 21 | 0 << 10,
    BLRABZ = 0b0001 << 21 | 1 << 10,
    RETAA = 0b0010 << 21 | 0 << 10 | 0b11111 << 5,
    RETAB = 0b0010 << 21 | 1 << 10 | 0b11111 << 5,
    ERETAA = 0b0100 << 21 | 0 << 10 | 0b11111 << 5,
    ERETAB = 0b0100 << 21 | 1 << 10 | 0b11111 << 5,
}

pub const fn encode_group1(op: Group1Op) -> i32 {
    (op as i32) | 0b1101 << 28 | 0b0101 << 24 | 0b011 << 16 | 0b0010 << 12 | 0b11111
}

pub const fn encode_group2(op: Group2Op, rn: u8, rd: u8, rm: u8) -> i32 {
    op as i32 | 1 << 31 | 0b11010110 << 21 | (rm as i32) << 16 | (rn as i32) << 5 | rd as i32
}

pub const fn encode_group4(op: Group4Op, rn: u8, rm: u8) -> i32 {
    0b1101011 << 25 | op as i32 | 0b11111 << 16 | 0b00001 << 11 | (rn as i32) << 5 | rm as i32
}

const UNUSED_ID: u8 = 0b11111;

impl ARM64Assembler {
    pub fn pacia1716(&mut self) {
        self.insn(encode_group1(Group1Op::PACIA1716))
    }

    pub fn pacib1716(&mut self) {
        self.insn(encode_group1(Group1Op::PACIB1716))
    }

    pub fn autia1716(&mut self) {
        self.insn(encode_group1(Group1Op::AUTIA1716))
    }

    pub fn autib1716(&mut self) {
        self.insn(encode_group1(Group1Op::AUTIB1716))
    }

    pub fn paciaz(&mut self) {
        self.insn(encode_group1(Group1Op::PACIAZ));
    }

    pub fn paciasp(&mut self) {
        self.insn(encode_group1(Group1Op::PACIASP));
    }

    pub fn pacibz(&mut self) {
        self.insn(encode_group1(Group1Op::PACIBZ));
    }

    pub fn pacibsp(&mut self) {
        self.insn(encode_group1(Group1Op::PACIBSP));
    }

    pub fn autiaz(&mut self) {
        self.insn(encode_group1(Group1Op::AUTIAZ));
    }

    pub fn autiasp(&mut self) {
        self.insn(encode_group1(Group1Op::AUTIASP));
    }

    pub fn autibz(&mut self) {
        self.insn(encode_group1(Group1Op::AUTIBZ));
    }

    pub fn autibsp(&mut self) {
        self.insn(encode_group1(Group1Op::AUTIBSP));
    }

    pub fn xpaclri(&mut self) {
        self.insn(encode_group1(Group1Op::XPACLRI));
    }

    pub fn pacia(&mut self, rd: u8, rn: u8) {
        self.insn(encode_group2(Group2Op::PACIA, rn, rd, UNUSED_ID));
    }

    pub fn pacib(&mut self, rd: u8, rn: u8) {
        self.insn(encode_group2(Group2Op::PACIB, rn, rd, UNUSED_ID));
    }

    pub fn pacda(&mut self, rd: u8, rn: u8) {
        self.insn(encode_group2(Group2Op::PACDA, rn, rd, UNUSED_ID));
    }

    pub fn pacdb(&mut self, rd: u8, rn: u8) {
        self.insn(encode_group2(Group2Op::PACDB, rn, rd, UNUSED_ID));
    }

    pub fn autia(&mut self, rd: u8, rn: u8) {
        self.insn(encode_group2(Group2Op::AUTIA, rn, rd, UNUSED_ID));
    }

    pub fn autib(&mut self, rd: u8, rn: u8) {
        self.insn(encode_group2(Group2Op::AUTIB, rn, rd, UNUSED_ID));
    }

    pub fn autda(&mut self, rd: u8, rn: u8) {
        self.insn(encode_group2(Group2Op::AUTDA, rn, rd, UNUSED_ID));
    }

    pub fn autdb(&mut self, rd: u8, rn: u8) {
        self.insn(encode_group2(Group2Op::AUTDB, rn, rd, UNUSED_ID));
    }

    pub fn paciza(&mut self, rd: u8) {
        self.insn(encode_group2(Group2Op::PACIZA, UNUSED_ID, rd, UNUSED_ID));
    }

    pub fn pacizb(&mut self, rd: u8) {
        self.insn(encode_group2(Group2Op::PACIZB, UNUSED_ID, rd, UNUSED_ID));
    }

    pub fn pacdza(&mut self, rd: u8) {
        self.insn(encode_group2(Group2Op::PACDZA, UNUSED_ID, rd, UNUSED_ID));
    }

    pub fn pacdzb(&mut self, rd: u8) {
        self.insn(encode_group2(Group2Op::PACDZB, UNUSED_ID, rd, UNUSED_ID));
    }

    pub fn autiza(&mut self, rd: u8) {
        self.insn(encode_group2(Group2Op::AUTIZA, UNUSED_ID, rd, UNUSED_ID));
    }

    pub fn autizb(&mut self, rd: u8) {
        self.insn(encode_group2(Group2Op::AUTIZB, UNUSED_ID, rd, UNUSED_ID));
    }

    pub fn autdza(&mut self, rd: u8) {
        self.insn(encode_group2(Group2Op::AUTDZA, UNUSED_ID, rd, UNUSED_ID));
    }

    pub fn autdzb(&mut self, rd: u8) {
        self.insn(encode_group2(Group2Op::AUTDZB, UNUSED_ID, rd, UNUSED_ID));
    }

    pub fn xpaci(&mut self, rd: u8) {
        self.insn(encode_group2(Group2Op::XPACI, UNUSED_ID, rd, UNUSED_ID));
    }

    pub fn xpacd(&mut self, rd: u8) {
        self.insn(encode_group2(Group2Op::XPACD, UNUSED_ID, rd, UNUSED_ID));
    }

    pub fn pacga(&mut self, rd: u8, rn: u8, rm: u8) {
        self.insn(encode_group2(Group2Op::PACGA, rn, rd, rm));
    }
}
