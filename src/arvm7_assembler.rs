#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
#[repr(u8)]
pub enum RegisterID {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    IP,
    SP,
    LR,
    PC,
}

pub const FP_ALIAS: RegisterID = RegisterID::R7;
pub const SB_ALIAS: RegisterID = RegisterID::R9;
pub const SL_ALIAS: RegisterID = RegisterID::R10;
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
#[repr(u8)]
pub enum SPRegisterID {
    APSR,
    FPSCR,
}
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
#[repr(u8)]
pub enum FPSingleRegID {
    S0,
    S1,
    S2,
    S3,
    S4,
    S5,
    S6,
    S7,
    S8,
    S9,
    S10,
    S11,
    S12,
    S13,
    S14,
    S15,
    S16,
    S17,
    S18,
    S19,
    S20,
    S21,
    S22,
    S23,
    S24,
    S25,
    S26,
    S27,
    S28,
    S29,
    S30,
    S31,
}
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
#[repr(u8)]
pub enum FPDoubleRegID {
    D0,
    D1,
    D2,
    D3,
    D4,
    D5,
    D6,
    D7,
    D8,
    D9,
    D10,
    D11,
    D12,
    D13,
    D14,
    D15,
    D16,
    D17,
    D18,
    D19,
    D20,
    D21,
    D22,
    D23,
    D24,
    D25,
    D26,
    D27,
    D28,
    D29,
    D30,
    D31,
}
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
#[repr(u8)]
pub enum FPQuadReg {
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
}

impl FPDoubleRegID {
    pub fn as_single(self) -> u8 {
        (self as u8) << 1
    }

    pub fn as_single_upper(self) -> u8 {
        ((self as u8) << 1) + 1
    }
}

impl FPSingleRegID {
    pub fn as_double(self) -> u8 {
        self as u8 >> 1
    }
}
pub struct ARMThumbImmedate {}

pub union ThumbImmediateValue {
    pub as_int: i16,
    pub a: A,
    pub b: B,
    pub c: C,
}

bitfield::bitfield! {
    pub struct A(u16);
    imm8,set_imm8: 7,0;
    imm3,set_imm3: 10,7;
    i,set_i: 11,10;
    imm4,set_i4: 15,11;
}

bitfield::bitfield! {
    pub struct B(u16);
    shift_value7,set_shift_value7: 7,0;
    shift_amount,set_shift_amount: 12,7;
}

bitfield::bitfield! {
    pub struct C(u16);
    immediate,set_imm: 8,0;
    pattern,set_pattern: 12,8;
}

impl Clone for A {
    fn clone(&self) -> Self {
        Self(self.0)
    }
}
impl Clone for B {
    fn clone(&self) -> Self {
        Self(self.0)
    }
}

impl Clone for C {
    fn clone(&self) -> Self {
        Self(self.0)
    }
}

impl Copy for A {}
impl Copy for C {}
impl Copy for B {}
