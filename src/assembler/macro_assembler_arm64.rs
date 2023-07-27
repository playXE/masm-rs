use std::mem::size_of;
use std::ops::{Deref, DerefMut, Not};

use crate::assembler::abstract_macro_assembler::AbsoluteAddress;
use crate::assembler::assembler_common::ARM64LogicalImmediate;

use super::abstract_macro_assembler::{
    AbstractMacroAssembler, Address, BaseIndex, CachedTempRegister, Call, DataLabelCompact, Extend,
    Jump, JumpList, Operand, Scale,
};
use super::arm64assembler::*;
use super::assembler_common::{is_int, is_uint12, SIMDLane};
use super::buffer::AssemblerLabel;

pub struct MacroAssemblerARM64 {
    pub masm: AbstractMacroAssembler,
    pub data_temp_register: CachedTempRegister,
    pub memory_temp_register: CachedTempRegister,
}

impl Deref for MacroAssemblerARM64 {
    type Target = AbstractMacroAssembler;

    fn deref(&self) -> &Self::Target {
        &self.masm
    }
}

impl DerefMut for MacroAssemblerARM64 {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.masm
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum RelationalCondition {
    Equal = Condition::EQ as u8,
    NotEqual = Condition::NE as u8,
    Above = Condition::HI as u8,
    AboveOrEqual = Condition::HS as u8,
    Below = Condition::LO as u8,
    BelowOrEqual = Condition::LS as u8,
    GreaterThan = Condition::GT as u8,
    GreaterThanOrEqual = Condition::GE as u8,
    LessThan = Condition::LT as u8,
    LessThanOrEqual = Condition::LE as u8,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum ResultCondition {
    Overflow = Condition::VS as u8,
    Signed = Condition::MI as u8,
    PositiveOrZero = Condition::PL as u8,
    Zero = Condition::EQ as u8,
    NonZero = Condition::NE as u8,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum ZeroCondition {
    IsZero = Condition::EQ as u8,
    IsNotZero = Condition::NE as u8,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum DoubleCondition {
    EqualAndOrdered = Condition::EQ as u8,
    NotEqualAndOrdered = Condition::VC as u8,
    GreaterThanAndOrdered = Condition::GT as u8,
    GreaterThanOrEqualAndOrdered = Condition::GE as u8,
    LessThanAndOrdered = Condition::LO as u8,
    LessThanOrEqualAndOrdered = Condition::LS as u8,

    EqualOrUnordered = Condition::VS as u8,
    NotEqualOrUnordered = Condition::NE as u8,
    GreaterThanOrUnordered = Condition::HI as u8,
    GreaterThanOrEqualOrUnordered = Condition::HS as u8,
    LessThanOrUnordered = Condition::LT as u8,
    LessThanOrEqualOrUnordered = Condition::LE as u8,
}

pub enum ClearAttributes {
    OKToClobberMask,
    MustPreserveMask,
}

impl MacroAssemblerARM64 {
    pub const MASK_HALF_WORD_0: i64 = 0xffff;
    pub const MASK_HALF_WORD_1: i64 = 0xffff0000;
    pub const MASK_UPPER_WORD: i64 = 0xffffffff00000000u64 as i64;

    pub const REPATCH_OFFSET_CALL_TO_POINTER: isize =
        -((NUMBER_OF_ADDRESS_ENCODING_INSTRUCTIONS as isize + 1) * 4);

    pub const NUM_GPRS: usize = 32;
    pub const NUM_FPRS: usize = 32;

    pub const NEAR_JUMP_RANGE: usize = 128 * 1024 * 1024;
    pub const DATA_TEMP_REGISTER: u8 = ip0;
    pub const MEMORY_TEMP_REGISTER: u8 = ip1;

    pub const FP_TEMP_REGISTER: u8 = q31;

    pub const STACK_POINTER_REGISTER: u8 = sp;
    pub const FRAME_POINTER_REGISTER: u8 = fp;
    pub const LINK_REGISTER: u8 = lr;

    pub const fn should_blind_for_specific_arch(x: u32) -> bool {
        x >= 0x00ffffff
    }

    pub const fn should_blind_for_specific_arch64(x: u64) -> bool {
        x >= 0x00ffffff
    }

    pub fn add32_rrr(&mut self, a: impl Into<Operand>, mut b: u8, dst: u8) {
        match a.into() {
            Operand::Register(mut a) => {
                if b == sp {
                    std::mem::swap(&mut a, &mut b);
                }

                self.assembler.add::<32, false>(dst, a, b);
            }

            Operand::Imm32(imm) => {
                let src = b;
                let dest = dst;

                if let Some((u12, shift, inverted)) = Self::try_extract_shifted_imm(imm as _) {
                    if !inverted {
                        self.assembler.add_imm::<32, false>(dest, src, u12, shift)
                    } else {
                        self.assembler.sub_imm::<32, false>(dest, src, u12, shift)
                    }
                    return;
                }

                if src != dest {
                    self.mov(imm, dest);
                    self.add32(src, dest);
                } else {
                    let r = self.get_cached_data_temp_register_id_and_invalidate();
                    self.mov(imm, r);
                    self.assembler.add::<32, false>(dest, src, r);
                }
            }

            _ => todo!(),
        }
    }

    pub fn add32(&mut self, src: impl Into<Operand>, dest: impl Into<Operand>) {
        match (src.into(), dest.into()) {
            (Operand::Imm32(imm), Operand::Address(address)) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load32(address, r);

                if let Some((u12, shift, inverted)) = Self::try_extract_shifted_imm(imm as _) {
                    if !inverted {
                        self.assembler.add_imm::<32, false>(r, r, u12, shift)
                    } else {
                        self.assembler.sub_imm::<32, false>(r, r, u12, shift)
                    }
                } else {
                    let rm = self.get_cached_memory_temp_register_id_and_invalidate();
                    self.mov(imm, r);
                    self.assembler.add::<32, false>(r, r, rm);
                }

                self.store32(r, address);
            }

            (Operand::Imm32(imm), Operand::AbsoluteAddress(address)) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load32(address, r);

                if let Some((u12, shift, inverted)) = Self::try_extract_shifted_imm(imm as _) {
                    if !inverted {
                        self.assembler.add_imm::<32, false>(r, r, u12, shift)
                    } else {
                        self.assembler.sub_imm::<32, false>(r, r, u12, shift)
                    }
                } else {
                    let rm = self.get_cached_memory_temp_register_id_and_invalidate();
                    self.mov(imm, r);
                    self.assembler.add::<32, false>(r, r, rm);
                }

                self.store32(r, address);
            }

            (Operand::Address(src), Operand::Register(dest)) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load32(src, r);
                self.add32(r, dest);
            }

            (Operand::AbsoluteAddress(src), Operand::Register(dest)) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load32(src, r);
                self.add32(r, dest);
            }
            (Operand::Register(src), Operand::Register(dest)) => {
                if src == sp {
                    self.assembler.add::<32, false>(dest, src, dest);
                } else {
                    self.assembler.add::<32, false>(dest, dest, src);
                }
            }
            (Operand::Imm32(imm), Operand::Register(dest)) => {
                self.add32_rrr(imm, dest, dest);
            }

            _ => unreachable!(),
        }
    }

    pub fn add64_rrr(&mut self, a: impl Into<Operand>, mut b: u8, dest: u8) {
        match a.into() {
            Operand::Register(mut a) => {
                if b == sp {
                    std::mem::swap(&mut a, &mut b);
                }

                self.assembler.add::<64, false>(dest, a, b);
            }
            Operand::Imm32(imm) => {
                let src = b;
                if let Some((u12, shift, inverted)) = Self::try_extract_shifted_imm(imm as _) {
                    if !inverted {
                        self.assembler.add_imm::<64, false>(dest, src, u12, shift)
                    } else {
                        self.assembler.sub_imm::<64, false>(dest, src, u12, shift)
                    }
                    return;
                } else {
                    let r = self.get_cached_data_temp_register_id_and_invalidate();
                    self.sign_extend32_to_64(imm, r);
                    self.assembler.add::<64, false>(dest, src, r);
                }
            }

            Operand::Imm64(imm) => {
                if let Some((u12, shift, inverted)) = Self::try_extract_shifted_imm(imm as _) {
                    if !inverted {
                        self.assembler.add_imm::<64, false>(dest, b, u12, shift)
                    } else {
                        self.assembler.sub_imm::<64, false>(dest, b, u12, shift)
                    }
                    return;
                } else {
                    let r = self.get_cached_data_temp_register_id_and_invalidate();
                    self.mov(imm, r);
                    self.assembler.add::<64, false>(dest, b, r);
                }
            }

            _ => unreachable!(),
        }
    }

    pub fn add64(&mut self, src: impl Into<Operand>, dest: impl Into<Operand>) {
        match (src.into(), dest.into()) {
            (Operand::Imm32(imm), Operand::Address(address)) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();

                self.load64(address, r);

                if let Some((u12, shift, inverted)) = Self::try_extract_shifted_imm(imm as _) {
                    if !inverted {
                        self.assembler.add_imm::<64, false>(r, r, u12, shift)
                    } else {
                        self.assembler.sub_imm::<64, false>(r, r, u12, shift)
                    }
                } else {
                    let rm = self.get_cached_memory_temp_register_id_and_invalidate();
                    self.sign_extend32_to_64(imm, rm);
                    self.assembler.add::<64, false>(r, r, rm);
                }

                self.store64(r, address);
            }

            (Operand::Imm32(imm), Operand::Register(dest)) => {
                self.add64_rrr(imm, dest, dest);
            }

            (Operand::Imm64(imm), Operand::Register(dest)) => {
                self.add64_rrr(imm, dest, dest);
            }

            (Operand::Register(src), Operand::Register(dest)) => {
                if src == sp {
                    self.assembler.add::<64, false>(dest, src, dest);
                } else {
                    self.assembler.add::<64, false>(dest, dest, src);
                }
            }

            (Operand::Imm32(imm), Operand::AbsoluteAddress(address)) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();

                self.load64(address, r);

                if let Some((u12, shift, inverted)) = Self::try_extract_shifted_imm(imm as _) {
                    if !inverted {
                        self.assembler.add_imm::<64, false>(r, r, u12, shift)
                    } else {
                        self.assembler.sub_imm::<64, false>(r, r, u12, shift)
                    }
                } else {
                    let rm = self.get_cached_memory_temp_register_id_and_invalidate();
                    self.sign_extend32_to_64(imm, rm);
                    self.assembler.add::<64, false>(r, r, rm);
                }

                self.store64(r, address);
            }

            (Operand::Address(src), Operand::Register(dest)) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load64(src, r);
                self.assembler.add::<64, false>(dest, dest, r);
            }

            (Operand::Register(src), Operand::Address(dest)) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load64(dest, r);
                self.assembler.add::<64, false>(r, r, src);
                self.store64(r, dest);
            }

            (Operand::AbsoluteAddress(src), Operand::Register(dest)) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load64(src, r);
                self.assembler.add::<64, false>(dest, dest, r);
            }
            _ => todo!(),
        }
    }

    pub fn add_zero_extend64(&mut self, src: u8, src_extend: u8, dest: u8) {
        self.assembler
            .add_extend::<64, false>(dest, src, src_extend, ExtendType::UXTW, 0);
    }

    pub fn add_sign_extend64(&mut self, src: u8, src_extend: u8, dest: u8) {
        self.assembler
            .add_extend::<64, false>(dest, src, src_extend, ExtendType::SXTW, 0);
    }

    pub fn and32_rrr(&mut self, op1: impl Into<Operand>, op2: u8, dest: u8) {
        match op1.into() {
            Operand::Imm32(imm) => {
                let logical_imm = LogicalImmediate::create32(imm as _);
                if logical_imm.is_valid() {
                    self.assembler.and_imm::<32, false>(dest, op2, logical_imm);
                    return;
                }

                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.mov(imm, r);
                self.assembler.and::<32, false>(dest, op2, r);
            }

            Operand::Register(op1) => {
                self.assembler.and::<32, false>(dest, op2, op1);
            }

            _ => unreachable!(),
        }
    }

    pub fn and32(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Address(address) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load32(address, r);
                self.assembler.and::<32, false>(dest, dest, r);
            }

            Operand::Imm32(imm) => {
                let logical_imm = LogicalImmediate::create32(imm as _);
                if logical_imm.is_valid() {
                    self.assembler.and_imm::<32, false>(dest, dest, logical_imm);
                    return;
                }

                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.mov(imm, r);
                self.assembler.and::<32, false>(dest, dest, r);
            }

            Operand::Register(src) => {
                self.assembler.and::<32, false>(dest, dest, src);
            }

            _ => unreachable!(),
        }
    }

    pub fn and64_rrr(&mut self, op1: impl Into<Operand>, op2: u8, dest: u8) {
        match op1.into() {
            Operand::Imm64(imm) => {
                let logical_imm = LogicalImmediate::create64(imm as _);
                if logical_imm.is_valid() {
                    self.assembler.and_imm::<64, false>(dest, op2, logical_imm);
                    return;
                }

                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.mov(imm, r);
                self.assembler.and::<64, false>(dest, op2, r);
            }

            Operand::Imm32(imm) => {
                let logical_imm = LogicalImmediate::create64(imm as _);
                if logical_imm.is_valid() {
                    self.assembler.and_imm::<64, false>(dest, op2, logical_imm);
                    return;
                }

                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.sign_extend32_to_64(imm, r);
                self.assembler.and::<64, false>(dest, op2, r);
            }

            Operand::Register(op1) => {
                self.assembler.and::<64, false>(dest, op2, op1);
            }

            _ => unreachable!(),
        }
    }

    pub fn and64(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Address(address) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load64(address, r);
                self.assembler.and::<64, false>(dest, dest, r);
            }

            Operand::Imm64(imm) => {
                let logical_imm = LogicalImmediate::create64(imm as _);
                if logical_imm.is_valid() {
                    self.assembler.and_imm::<64, false>(dest, dest, logical_imm);
                    return;
                }

                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.mov(imm, r);
                self.assembler.and::<64, false>(dest, dest, r);
            }

            Operand::Imm32(imm) => {
                let logical_imm = LogicalImmediate::create64(imm as _);
                if logical_imm.is_valid() {
                    self.assembler.and_imm::<64, false>(dest, dest, logical_imm);
                    return;
                }

                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.sign_extend32_to_64(imm, r);
                self.assembler.and::<64, false>(dest, dest, r);
            }

            Operand::Register(src) => {
                self.assembler.and::<64, false>(dest, dest, src);
            }

            _ => unreachable!(),
        }
    }

    pub fn and16(&mut self, src: Address, dest: u8) {
        let r = self.get_cached_data_temp_register_id_and_invalidate();
        self.load16(src, r);
        self.assembler.and::<16, false>(dest, dest, r);
    }

    pub fn extract_unsigned_bitfield32(&mut self, src: u8, lsb: i32, width: i32, dest: u8) {
        self.assembler.ubfx::<32>(dest, src, lsb, width);
    }

    pub fn extract_unsigned_bitfield64(&mut self, src: u8, lsb: i32, width: i32, dest: u8) {
        self.assembler.ubfx::<64>(dest, src, lsb, width);
    }

    pub fn extract_unsigned_bitfield_in_zero32(&mut self, src: u8, lsb: i32, width: i32, dest: u8) {
        self.assembler.ubfiz::<32>(dest, src, lsb, width);
    }

    pub fn extract_unsigned_bitfield_in_zero64(&mut self, src: u8, lsb: i32, width: i32, dest: u8) {
        self.assembler.ubfiz::<64>(dest, src, lsb, width);
    }

    pub fn insert_bitfield32(&mut self, src: u8, lsb: i32, width: i32, dest: u8) {
        self.assembler.bfi::<32>(dest, src, lsb, width);
    }

    pub fn insert_bitfield64(&mut self, src: u8, lsb: i32, width: i32, dest: u8) {
        self.assembler.bfi::<64>(dest, src, lsb, width);
    }

    pub fn clear_bitfield32(&mut self, lsb: i32, width: i32, dest: u8) {
        self.assembler.bfc::<32>(dest, lsb, width);
    }

    pub fn clear_bitfield64(&mut self, lsb: i32, width: i32, dest: u8) {
        self.assembler.bfc::<64>(dest, lsb, width);
    }

    pub fn clear_bits_with_mask32(&mut self, src: u8, mask: u8, dest: u8) {
        self.assembler.bic::<32, false>(dest, src, mask);
    }

    pub fn clear_bits_with_mask64(&mut self, src: u8, mask: u8, dest: u8) {
        self.assembler.bic::<64, false>(dest, src, mask);
    }

    pub fn or_not32(&mut self, src: u8, mask: u8, dest: u8) {
        self.assembler.orn::<32>(dest, src, mask);
    }

    pub fn or_not64(&mut self, src: u8, mask: u8, dest: u8) {
        self.assembler.orn::<64>(dest, src, mask);
    }

    pub fn xor_not32(&mut self, src: u8, mask: u8, dest: u8) {
        self.assembler.eon::<32>(dest, src, mask);
    }

    pub fn xor_not64(&mut self, src: u8, mask: u8, dest: u8) {
        self.assembler.eon::<64>(dest, src, mask);
    }

    pub fn xor_not_left_shift32(&mut self, src: u8, mask: u8, shift: i32, dest: u8) {
        self.assembler
            .eon_shifted::<32>(dest, src, mask, ShiftType::LSL, shift);
    }

    pub fn xor_not_right_shift32(&mut self, src: u8, mask: u8, shift: i32, dest: u8) {
        self.assembler
            .eon_shifted::<32>(dest, src, mask, ShiftType::ASR, shift);
    }

    pub fn xor_not_unsigned_right_shift32(&mut self, src: u8, mask: u8, shift: i32, dest: u8) {
        self.assembler
            .eon_shifted::<32>(dest, src, mask, ShiftType::LSR, shift);
    }

    pub fn xor_not_left_shift64(&mut self, src: u8, mask: u8, shift: i32, dest: u8) {
        self.assembler
            .eon_shifted::<64>(dest, src, mask, ShiftType::LSL, shift);
    }

    pub fn xor_not_right_shift64(&mut self, src: u8, mask: u8, shift: i32, dest: u8) {
        self.assembler
            .eon_shifted::<64>(dest, src, mask, ShiftType::ASR, shift);
    }

    pub fn xor_not_unsigned_right_shift64(&mut self, src: u8, mask: u8, shift: i32, dest: u8) {
        self.assembler
            .eon_shifted::<64>(dest, src, mask, ShiftType::LSR, shift);
    }

    pub fn extract_insert_bitfield_at_lowend32(&mut self, src: u8, lsb: i32, width: i32, dest: u8) {
        self.assembler.bfxil::<32>(dest, src, lsb, width);
    }

    pub fn extract_insert_bitfield_at_lowend64(&mut self, src: u8, lsb: i32, width: i32, dest: u8) {
        self.assembler.bfxil::<64>(dest, src, lsb, width);
    }

    pub fn insert_signed_bitfield_in_zero32(&mut self, src: u8, lsb: i32, width: i32, dest: u8) {
        self.assembler.sbfiz::<32>(dest, src, lsb, width);
    }

    pub fn insert_signed_bitfield_in_zero64(&mut self, src: u8, lsb: i32, width: i32, dest: u8) {
        self.assembler.sbfiz::<64>(dest, src, lsb, width);
    }

    pub fn extract_signed_bitfield32(&mut self, src: u8, lsb: i32, width: i32, dest: u8) {
        self.assembler.sbfx::<32>(dest, src, lsb, width);
    }

    pub fn extract_signed_bitfield64(&mut self, src: u8, lsb: i32, width: i32, dest: u8) {
        self.assembler.sbfx::<64>(dest, src, lsb, width);
    }

    pub fn extract_register32(&mut self, n: u8, m: u8, lsb: i32, d: u8) {
        self.assembler.extr::<32>(d, n, m, lsb);
    }

    pub fn extract_register64(&mut self, n: u8, m: u8, lsb: i32, d: u8) {
        self.assembler.extr::<64>(d, n, m, lsb);
    }

    pub fn add_left_shift32(&mut self, n: u8, m: u8, amount: i32, d: u8) {
        self.assembler
            .add_shifted::<32, false>(d, n, m, ShiftType::LSL, amount);
    }

    pub fn add_left_shift64(&mut self, n: u8, m: u8, amount: i32, d: u8) {
        self.assembler
            .add_shifted::<64, false>(d, n, m, ShiftType::LSL, amount);
    }

    pub fn add_right_shift32(&mut self, n: u8, m: u8, amount: i32, d: u8) {
        self.assembler
            .add_shifted::<32, false>(d, n, m, ShiftType::ASR, amount);
    }

    pub fn add_right_shift64(&mut self, n: u8, m: u8, amount: i32, d: u8) {
        self.assembler
            .add_shifted::<64, false>(d, n, m, ShiftType::ASR, amount);
    }

    pub fn add_unsigned_right_shift32(&mut self, n: u8, m: u8, amount: i32, d: u8) {
        self.assembler
            .add_shifted::<32, false>(d, n, m, ShiftType::LSR, amount);
    }

    pub fn add_unsigned_right_shift64(&mut self, n: u8, m: u8, amount: i32, d: u8) {
        self.assembler
            .add_shifted::<64, false>(d, n, m, ShiftType::LSR, amount);
    }

    pub fn sub_left_shift32(&mut self, n: u8, m: u8, amount: i32, d: u8) {
        self.assembler
            .sub_shifted::<32, false>(d, n, m, ShiftType::LSL, amount);
    }

    pub fn sub_left_shift64(&mut self, n: u8, m: u8, amount: i32, d: u8) {
        self.assembler
            .sub_shifted::<64, false>(d, n, m, ShiftType::LSL, amount);
    }

    pub fn sub_right_shift32(&mut self, n: u8, m: u8, amount: i32, d: u8) {
        self.assembler
            .sub_shifted::<32, false>(d, n, m, ShiftType::ASR, amount);
    }

    pub fn sub_right_shift64(&mut self, n: u8, m: u8, amount: i32, d: u8) {
        self.assembler
            .sub_shifted::<64, false>(d, n, m, ShiftType::ASR, amount);
    }

    pub fn sub_unsigned_right_shift32(&mut self, n: u8, m: u8, amount: i32, d: u8) {
        self.assembler
            .sub_shifted::<32, false>(d, n, m, ShiftType::LSR, amount);
    }

    pub fn sub_unsigned_right_shift64(&mut self, n: u8, m: u8, amount: i32, d: u8) {
        self.assembler
            .sub_shifted::<64, false>(d, n, m, ShiftType::LSR, amount);
    }

    pub fn and_left_shift32(&mut self, n: u8, m: u8, amount: i32, d: u8) {
        self.assembler
            .and_shifted::<32, false>(d, n, m, ShiftType::LSL, amount);
    }

    pub fn and_left_shift64(&mut self, n: u8, m: u8, amount: i32, d: u8) {
        self.assembler
            .and_shifted::<64, false>(d, n, m, ShiftType::LSL, amount);
    }

    pub fn and_right_shift32(&mut self, n: u8, m: u8, amount: i32, d: u8) {
        self.assembler
            .and_shifted::<32, false>(d, n, m, ShiftType::ASR, amount);
    }

    pub fn and_right_shift64(&mut self, n: u8, m: u8, amount: i32, d: u8) {
        self.assembler
            .and_shifted::<64, false>(d, n, m, ShiftType::ASR, amount);
    }

    pub fn and_unsigned_right_shift32(&mut self, n: u8, m: u8, amount: i32, d: u8) {
        self.assembler
            .and_shifted::<32, false>(d, n, m, ShiftType::LSR, amount);
    }

    pub fn and_unsigned_right_shift64(&mut self, n: u8, m: u8, amount: i32, d: u8) {
        self.assembler
            .and_shifted::<64, false>(d, n, m, ShiftType::LSR, amount);
    }

    pub fn xor_left_shift32(&mut self, n: u8, m: u8, amount: i32, d: u8) {
        self.assembler
            .eor_shifted::<32>(d, n, m, ShiftType::LSL, amount);
    }

    pub fn xor_left_shift64(&mut self, n: u8, m: u8, amount: i32, d: u8) {
        self.assembler
            .eor_shifted::<64>(d, n, m, ShiftType::LSL, amount);
    }

    pub fn xor_right_shift32(&mut self, n: u8, m: u8, amount: i32, d: u8) {
        self.assembler
            .eor_shifted::<32>(d, n, m, ShiftType::ASR, amount);
    }

    pub fn xor_right_shift64(&mut self, n: u8, m: u8, amount: i32, d: u8) {
        self.assembler
            .eor_shifted::<64>(d, n, m, ShiftType::ASR, amount);
    }

    pub fn xor_unsigned_right_shift32(&mut self, n: u8, m: u8, amount: i32, d: u8) {
        self.assembler
            .eor_shifted::<32>(d, n, m, ShiftType::LSR, amount);
    }

    pub fn xor_unsigned_right_shift64(&mut self, n: u8, m: u8, amount: i32, d: u8) {
        self.assembler
            .eor_shifted::<64>(d, n, m, ShiftType::LSR, amount);
    }

    pub fn or_left_shift32(&mut self, n: u8, m: u8, amount: i32, d: u8) {
        self.assembler
            .orr_shifted::<32>(d, n, m, ShiftType::LSL, amount);
    }

    pub fn or_left_shift64(&mut self, n: u8, m: u8, amount: i32, d: u8) {
        self.assembler
            .orr_shifted::<64>(d, n, m, ShiftType::LSL, amount);
    }

    pub fn or_right_shift32(&mut self, n: u8, m: u8, amount: i32, d: u8) {
        self.assembler
            .orr_shifted::<32>(d, n, m, ShiftType::ASR, amount);
    }

    pub fn or_right_shift64(&mut self, n: u8, m: u8, amount: i32, d: u8) {
        self.assembler
            .orr_shifted::<64>(d, n, m, ShiftType::ASR, amount);
    }

    pub fn or_unsigned_right_shift32(&mut self, n: u8, m: u8, amount: i32, d: u8) {
        self.assembler
            .orr_shifted::<32>(d, n, m, ShiftType::LSR, amount);
    }

    pub fn or_unsigned_right_shift64(&mut self, n: u8, m: u8, amount: i32, d: u8) {
        self.assembler
            .orr_shifted::<64>(d, n, m, ShiftType::LSR, amount);
    }

    pub fn clear_bit64(&mut self, bit_to_clear: u8, dest: u8, mut scratch_for_mask: u8) {
        if scratch_for_mask == INVALID_GPR {
            scratch_for_mask = self.get_cached_data_temp_register_id_and_invalidate();
        }

        self.mov(1i32, scratch_for_mask);
        self.lshift64(bit_to_clear, scratch_for_mask);
        self.clear_bits64_with_mask(scratch_for_mask, dest);
    }

    pub fn clear_bits64_with_mask(&mut self, mask: u8, dest: u8) {
        self.clear_bits64_with_mask_rrr(dest, mask, dest);
    }

    pub fn clear_bits64_with_mask_rrr(&mut self, src: u8, mask: u8, dest: u8) {
        self.assembler.bic::<64, false>(dest, src, mask);
    }

    pub fn count_leading_zeros32(&mut self, src: u8, dest: u8) {
        self.assembler.clz::<32>(dest, src);
    }

    pub fn count_leading_zeros64(&mut self, src: u8, dest: u8) {
        self.assembler.clz::<64>(dest, src);
    }

    pub fn count_trailing_zeros32(&mut self, src: u8, dest: u8) {
        self.assembler.rbit::<32>(dest, src);
        self.assembler.clz::<32>(dest, dest);
    }

    pub fn count_trailing_zeros64(&mut self, src: u8, dest: u8) {
        self.assembler.rbit::<64>(dest, src);
        self.assembler.clz::<64>(dest, dest);
    }

    pub fn byte_swap16(&mut self, src: u8, dest: u8) {
        self.assembler.rev16::<32>(dest, src);
        self.zero_extend16_to_32(dest, dest);
    }

    pub fn byte_swap32(&mut self, src: u8, dest: u8) {
        self.assembler.rev::<32>(dest, src);
    }

    pub fn byte_swap64(&mut self, src: u8, dest: u8) {
        self.assembler.rev::<64>(dest, src);
    }

    pub fn illegal_instruction(&mut self) {
        self.assembler.illegal_instruction();
    }

    pub fn count_population32(&mut self, src: u8, dst: u8, temp: u8) {
        self.move32_to_float(src, temp);
        self.assembler.vector_cnt(temp, temp, SIMDLane::I8X16);
        self.assembler.addv(temp, temp, SIMDLane::I8X16);
        self.move_float_to32(temp, dst);
    }

    pub fn count_population64(&mut self, src: u8, dst: u8, temp: u8) {
        self.move64_to_float(src, temp);
        self.assembler.vector_cnt(temp, temp, SIMDLane::I8X16);
        self.assembler.addv(temp, temp, SIMDLane::I8X16);
        self.move_float_to64(temp, dst);
    }

    pub fn lshift32_rrr(&mut self, src: impl Into<Operand>, shift: impl Into<Operand>, dest: u8) {
        let src = src.into();
        match src.into() {
            Operand::Register(src) => match shift.into() {
                Operand::Register(shift) => {
                    self.assembler.lsl::<32>(dest, src, shift);
                }

                Operand::Imm32(imm) => {
                    self.assembler.lsl_imm::<32>(dest, src, imm & 0x1f);
                }

                _ => unreachable!(),
            },

            Operand::Address(address) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load32(address, r);
                self.lshift32_rrr(r, shift, dest);
            }

            _ => unreachable!(),
        }
    }

    pub fn lshift32(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(src) => {
                self.lshift32_rrr(dest, src, dest);
            }

            Operand::Imm32(imm) => {
                self.lshift32_rrr(dest, imm, dest);
            }

            _ => unreachable!(),
        }
    }

    pub fn lshift64_rrr(&mut self, src: impl Into<Operand>, shift: impl Into<Operand>, dest: u8) {
        let src = src.into();
        match src.into() {
            Operand::Register(src) => match shift.into() {
                Operand::Register(shift) => {
                    self.assembler.lsl::<64>(dest, src, shift);
                }

                Operand::Imm32(imm) => {
                    self.assembler.lsl_imm::<64>(dest, src, imm & 0x3f);
                }

                _ => unreachable!(),
            },

            Operand::Address(address) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load64(address, r);
                self.lshift64_rrr(r, shift, dest);
            }

            _ => unreachable!(),
        }
    }

    pub fn lshift64(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(src) => {
                self.lshift64_rrr(dest, src, dest);
            }

            Operand::Imm32(imm) => {
                self.lshift64_rrr(dest, imm, dest);
            }

            _ => unreachable!(),
        }
    }

    pub fn mul32_rrr(&mut self, left: impl Into<Operand>, right: u8, dest: u8) {
        match left.into() {
            Operand::Imm32(imm) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.mov(imm, r);
                self.mul32_rrr(r, right, dest);
            }

            Operand::Register(left) => {
                self.assembler.mul::<32>(dest, left, right);
            }

            _ => unreachable!(),
        }
    }

    pub fn mul32(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(src) => {
                self.mul32_rrr(src, dest, dest);
            }

            Operand::Imm32(imm) => {
                self.mul32_rrr(imm, dest, dest);
            }

            _ => unreachable!(),
        }
    }

    pub fn mul64_rrr(&mut self, left: impl Into<Operand>, right: u8, dest: u8) {
        match left.into() {
            Operand::Imm32(imm) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.mov(imm, r);
                self.mul64_rrr(r, right, dest);
            }

            Operand::Register(left) => {
                self.assembler.mul::<64>(dest, left, right);
            }

            _ => unreachable!(),
        }
    }

    pub fn multiply_add32(&mut self, mul_left: u8, mul_right: u8, summand: u8, dest: u8) {
        self.assembler
            .madd::<32>(dest, mul_left, mul_right, summand);
    }

    pub fn multiply_sub32(&mut self, mul_left: u8, mul_right: u8, subtrahend: u8, dest: u8) {
        self.assembler
            .msub::<32>(dest, mul_left, mul_right, subtrahend);
    }

    pub fn multiply_neg32(&mut self, mul_left: u8, mul_right: u8, dest: u8) {
        self.assembler.mneg::<32>(dest, mul_left, mul_right);
    }

    pub fn multiply_add64(&mut self, mul_left: u8, mul_right: u8, summand: u8, dest: u8) {
        self.assembler
            .madd::<64>(dest, mul_left, mul_right, summand);
    }

    pub fn multiply_sub64(&mut self, mul_left: u8, mul_right: u8, subtrahend: u8, dest: u8) {
        self.assembler
            .msub::<64>(dest, mul_left, mul_right, subtrahend);
    }

    pub fn multiply_neg64(&mut self, mul_left: u8, mul_right: u8, dest: u8) {
        self.assembler.mneg::<64>(dest, mul_left, mul_right);
    }

    pub fn multiply_add_signed_extend32(
        &mut self,
        mul_left: u8,
        mul_right: u8,
        summand: u8,
        dest: u8,
    ) {
        self.assembler.smaddl(dest, mul_left, mul_right, summand);
    }

    pub fn multiply_add_zero_extend32(
        &mut self,
        mul_left: u8,
        mul_right: u8,
        summand: u8,
        dest: u8,
    ) {
        self.assembler.umaddl(dest, mul_left, mul_right, summand);
    }

    pub fn multiply_sub_signed_extend32(
        &mut self,
        mul_left: u8,
        mul_right: u8,
        subtrahend: u8,
        dest: u8,
    ) {
        self.assembler.smsubl(dest, mul_left, mul_right, subtrahend);
    }

    pub fn multiply_sub_zero_extend32(
        &mut self,
        mul_left: u8,
        mul_right: u8,
        subtrahend: u8,
        dest: u8,
    ) {
        self.assembler.umsubl(dest, mul_left, mul_right, subtrahend);
    }

    pub fn multiply_neg_sign_extend32(&mut self, mul_left: u8, mul_right: u8, dest: u8) {
        self.assembler.smnegl(dest, mul_left, mul_right);
    }

    pub fn multiply_neg_zero_extend32(&mut self, mul_left: u8, mul_right: u8, dest: u8) {
        self.assembler.umnegl(dest, mul_left, mul_right);
    }

    pub fn multiply_neg_sign_extend64(&mut self, mul_left: u8, mul_right: u8, dest: u8) {
        self.assembler.smnegl(dest, mul_left, mul_right);
    }

    pub fn multiply_neg_zero_extend64(&mut self, mul_left: u8, mul_right: u8, dest: u8) {
        self.assembler.umnegl(dest, mul_left, mul_right);
    }

    pub fn multiply_sign_extend32(&mut self, mul_left: u8, mul_right: u8, dest: u8) {
        self.assembler.smull(dest, mul_left, mul_right);
    }

    pub fn multiply_zero_extend32(&mut self, mul_left: u8, mul_right: u8, dest: u8) {
        self.assembler.umull(dest, mul_left, mul_right);
    }

    pub fn div32(&mut self, dividend: u8, divisor: u8, dest: u8) {
        self.assembler.sdiv::<32>(dest, dividend, divisor);
    }

    pub fn div64(&mut self, dividend: u8, divisor: u8, dest: u8) {
        self.assembler.sdiv::<64>(dest, dividend, divisor);
    }

    pub fn udiv32(&mut self, dividend: u8, divisor: u8, dest: u8) {
        self.assembler.udiv::<32>(dest, dividend, divisor);
    }

    pub fn udiv64(&mut self, dividend: u8, divisor: u8, dest: u8) {
        self.assembler.udiv::<64>(dest, dividend, divisor);
    }

    pub fn neg32(&mut self, op: u8) {
        self.assembler.neg::<32, false>(op, op);
    }

    pub fn neg32_rr(&mut self, src: u8, dest: u8) {
        self.assembler.neg::<32, false>(dest, src);
    }

    pub fn neg64(&mut self, op: u8) {
        self.assembler.neg::<64, false>(op, op);
    }

    pub fn neg64_rr(&mut self, src: u8, dest: u8) {
        self.assembler.neg::<64, false>(dest, src);
    }

    pub fn or16(&mut self, imm: i32, address: AbsoluteAddress) {
        let logical_imm = LogicalImmediate::create32(imm as _);

        if logical_imm.is_valid() {
            let r = self.get_cached_data_temp_register_id_and_invalidate();
            self.load16(address, r);
            self.assembler.orr_imm::<32>(r, r, logical_imm);
            self.store16(r, address);
        } else {
            let r = self.get_cached_data_temp_register_id_and_invalidate();
            let rm = self.get_cached_memory_temp_register_id_and_invalidate();
            self.load16(address, rm);

            self.or32_rrr(imm, Self::MEMORY_TEMP_REGISTER, r);
            self.store16(r, address);
        }
    }

    pub fn or32_rrr(&mut self, a: impl Into<Operand>, b: impl Into<Operand>, dest: u8) {
        match (a.into(), b.into()) {
            (Operand::Imm32(imm), Operand::Register(reg)) => {
                let logical_imm = LogicalImmediate::create32(imm as _);

                if logical_imm.is_valid() {
                    self.assembler.orr_imm(dest, reg, logical_imm)
                } else {
                    let r = self.get_cached_data_temp_register_id_and_invalidate();
                    self.mov(imm, r);
                    self.assembler.orr::<32>(dest, reg, r);
                }
            }

            (Operand::Register(a), Operand::Register(b)) => {
                self.assembler.orr::<32>(dest, a, b);
            }
            _ => unreachable!("Invalid operands"),
        }
    }

    pub fn or32(&mut self, src: impl Into<Operand>, dest: impl Into<Operand>) {
        match (src.into(), dest.into()) {
            (Operand::Register(src), Operand::Register(dest)) => {
                self.or32_rrr(dest, src, dest);
            }

            (Operand::Imm32(imm), Operand::Register(dest)) => {
                self.or32_rrr(imm, dest, dest);
            }

            (Operand::Register(src), Operand::AbsoluteAddress(dest)) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load32(dest, r);
                self.or32_rrr(src, r, r);
                self.store32(r, dest);
            }

            (Operand::Imm32(imm), Operand::AbsoluteAddress(dest)) => {
                let logical_imm = LogicalImmediate::create32(imm as _);
                if logical_imm.is_valid() {
                    let r = self.get_cached_data_temp_register_id_and_invalidate();
                    self.load32(dest, r);
                    self.assembler.orr_imm::<32>(r, r, logical_imm);
                    self.store32(r, dest);
                } else {
                    let r = self.get_cached_data_temp_register_id_and_invalidate();
                    let rm = self.get_cached_memory_temp_register_id_and_invalidate();
                    self.load32(dest, rm);
                    self.or32_rrr(imm, Self::MEMORY_TEMP_REGISTER, r);
                    self.store32(r, dest);
                }
            }

            (Operand::Imm32(imm), Operand::Address(address)) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.or32_rrr(imm, r, r);
                self.store32(r, address);
            }

            _ => unreachable!("Invalid operands"),
        }
    }

    pub fn mov(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Imm32(imm) => {
                self.move_internal32(imm, dest);
            }

            Operand::AbsoluteAddress(addr) => self.move_internal64(addr.ptr as _, dest),

            Operand::Imm64(imm) => {
                self.move_internal64(imm, dest);
            }

            Operand::ImmPtr(imm) => {
                self.move_internal64(imm as _, dest);
            }

            Operand::Register(reg) => {
                if reg != dest {
                    if reg == zr && dest == sp {
                        self.assembler.movz::<54>(dest, 0, 0);
                    } else {
                        self.assembler.mov::<64>(dest, reg);
                    }
                }
            }

            _ => unreachable!("Invalid operand"),
        }
    }

    pub fn load64(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Address(address) => {
                if self.try_load_with_offset::<64>(dest, address.base, address.offset) {
                    return;
                }
                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                self.sign_extend32_to_64(address.offset, r);
                self.assembler.ldr::<64>(dest, address.base, r);
            }

            Operand::BaseIndex(address) => {
                if address.scale == Scale::TimesOne || address.scale == Scale::TimesFour {
                    if let Some(base) = self.try_fold_base_and_offset_part(address) {
                        self.assembler.ldr_extend::<64>(
                            dest,
                            base,
                            address.index,
                            Self::index_extend_type(address),
                            address.scale as _,
                        );
                        return;
                    }
                }

                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                self.sign_extend32_to_64(address.offset, r);
                self.assembler.ldr::<64>(dest, address.base, r);
                self.assembler.ldr_extend::<64>(
                    dest,
                    dest,
                    address.index,
                    Self::index_extend_type(address),
                    address.scale as _,
                );
            }

            Operand::AbsoluteAddress(address) => {
                self.load_internal::<64>(address.ptr as _, dest);
            }
            _ => unreachable!("Invalid operand"),
        }
    }

    pub fn load32(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Address(address) => {
                if self.try_load_with_offset::<32>(dest, address.base, address.offset) {
                    return;
                }
                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                self.sign_extend32_to_64(address.offset, r);
                self.assembler.ldr::<32>(dest, address.base, r);
            }

            Operand::BaseIndex(address) => {
                if address.scale == Scale::TimesOne || address.scale == Scale::TimesFour {
                    if let Some(base) = self.try_fold_base_and_offset_part(address) {
                        self.assembler.ldr_extend::<32>(
                            dest,
                            base,
                            address.index,
                            Self::index_extend_type(address),
                            address.scale as _,
                        );
                        return;
                    }
                }
                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                self.sign_extend32_to_64(address.offset, r);
                self.assembler.add_extend::<64, false>(
                    r,
                    r,
                    address.index,
                    Self::index_extend_type(address),
                    address.scale as _,
                );
                self.assembler.ldr::<32>(dest, address.base, r);
            }

            Operand::AbsoluteAddress(address) => {
                self.load_internal::<32>(address.ptr as _, dest);
            }

            Operand::PreIndexAddress(src) => {
                self.assembler
                    .ldr_pre::<32>(dest, src.base, PreIndex(src.index));
            }

            Operand::PostIndexAddress(src) => {
                self.assembler
                    .ldr_post::<32>(dest, src.base, PostIndex(src.index));
            }

            _ => unreachable!(),
        }
    }

    pub fn load16(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Address(address) => {
                if self.try_load_with_offset::<16>(dest, address.base, address.offset) {
                    return;
                }
                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                self.sign_extend32_to_64(address.offset, r);
                self.assembler.ldr::<16>(dest, address.base, r);
            }

            Operand::BaseIndex(address) => {
                if address.scale == Scale::TimesOne || address.scale == Scale::TimesFour {
                    if let Some(base) = self.try_fold_base_and_offset_part(address) {
                        self.assembler.ldr_extend::<16>(
                            dest,
                            base,
                            address.index,
                            Self::index_extend_type(address),
                            address.scale as _,
                        );
                        return;
                    }
                }
                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                self.sign_extend32_to_64(address.offset, r);
                self.assembler.add_extend::<64, false>(
                    r,
                    r,
                    address.index,
                    Self::index_extend_type(address),
                    address.scale as _,
                );
                self.assembler.ldr::<16>(dest, address.base, r);
            }

            Operand::ExtendedAddress(address) => {
                let mut r = self.memory_temp_register;
                self.move_to_cached_reg64(address.offset as _, &mut r);
                self.memory_temp_register = r;
                self.assembler.ldrh_extend(
                    dest,
                    Self::MEMORY_TEMP_REGISTER,
                    address.base,
                    ExtendType::UXTX,
                    1,
                );
                if dest == Self::MEMORY_TEMP_REGISTER {
                    let mut r = self.memory_temp_register;
                    r.invalidate(self);
                    self.memory_temp_register = r;
                }
            }

            Operand::AbsoluteAddress(address) => {
                self.load_internal::<16>(address.ptr as _, dest);
            }

            _ => unreachable!(),
        }
    }

    pub fn store32(&mut self, src: impl Into<Operand>, dest: impl Into<Operand>) {
        match (src.into(), dest.into()) {
            (Operand::Register(src), Operand::Address(address)) => {
                if self.try_store_with_offset::<32>(src, address.base, address.offset) {
                    return;
                }

                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                self.sign_extend32_to_64(address.offset, r);
                self.assembler.str::<32>(src, address.base, r);
            }

            (Operand::Register(src), Operand::BaseIndex(address)) => {
                if matches!(address.scale, Scale::TimesOne | Scale::TimesFour) {
                    if let Some(base) = self.try_fold_base_and_offset_part(address) {
                        self.assembler.str_extend::<32>(
                            src,
                            base,
                            address.index,
                            Self::index_extend_type(address),
                            address.scale as _,
                        );
                        return;
                    }
                }

                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                self.sign_extend32_to_64(address.offset, r);
                self.assembler.add_extend::<64, false>(
                    r,
                    r,
                    address.index,
                    Self::index_extend_type(address),
                    address.scale as _,
                );
                self.assembler.str::<32>(src, address.base, r);
            }

            (Operand::Register(src), Operand::AbsoluteAddress(address)) => {
                self.store_internal::<32>(src, address.ptr as _);
            }

            (Operand::Imm32(imm), Operand::Address(address)) => {
                if imm == 0 {
                    self.store32(zr, address);
                    return;
                }

                let mut r = self.data_temp_register;
                self.move_to_cached_reg32(imm, &mut r);
                self.data_temp_register = r;
                self.store32(Self::DATA_TEMP_REGISTER, address);
            }

            (Operand::Imm32(imm), Operand::BaseIndex(address)) => {
                if imm == 0 {
                    self.store32(zr, address);
                    return;
                }

                let mut r = self.data_temp_register;
                self.move_to_cached_reg32(imm, &mut r);
                self.data_temp_register = r;
                self.store32(Self::DATA_TEMP_REGISTER, address);
            }

            (Operand::Imm32(imm), Operand::AbsoluteAddress(address)) => {
                if imm == 0 {
                    self.store32(zr, address);
                    return;
                }

                let mut r = self.data_temp_register;
                self.move_to_cached_reg32(imm, &mut r);
                self.data_temp_register = r;
                self.store32(Self::DATA_TEMP_REGISTER, address);
            }

            (Operand::Register(src), Operand::PreIndexAddress(dest)) => {
                self.assembler
                    .str_pre::<32>(src, dest.base, PreIndex(dest.index));
            }

            (Operand::Register(src), Operand::PostIndexAddress(dest)) => {
                self.assembler
                    .str_post::<32>(src, dest.base, PostIndex(dest.index));
            }

            _ => unreachable!(),
        }
    }

    pub fn store16(&mut self, src: impl Into<Operand>, dest: impl Into<Operand>) {
        let src = src.into();
        match src {
            Operand::Register(src) => match dest.into() {
                Operand::Address(address) => {
                    if self.try_store_with_offset::<16>(src, address.base, address.offset) {
                        return;
                    }
                    self.get_cached_memory_temp_register_id_and_invalidate();
                    self.sign_extend32_to_64(address.offset, Self::MEMORY_TEMP_REGISTER);
                    self.assembler
                        .strh(src, address.base, Self::MEMORY_TEMP_REGISTER);
                }

                Operand::BaseIndex(address) => {
                    if matches!(address.scale, Scale::TimesOne | Scale::TimesFour) {
                        if let Some(base) = self.try_fold_base_and_offset_part(address) {
                            self.assembler.strh_extend(
                                src,
                                base,
                                address.index,
                                Self::index_extend_type(address),
                                address.scale as _,
                            );
                            return;
                        }
                    }

                    let r = self.get_cached_memory_temp_register_id_and_invalidate();
                    self.sign_extend32_to_64(address.offset, r);
                    self.assembler.add_extend::<64, false>(
                        r,
                        r,
                        address.index,
                        Self::index_extend_type(address),
                        address.scale as _,
                    );

                    self.assembler.strh(src, address.base, r);
                }

                Operand::AbsoluteAddress(address) => {
                    self.store_internal::<16>(src, address.ptr as _);
                }

                _ => unreachable!(),
            },

            Operand::Imm32(imm) => match dest.into() {
                Operand::AbsoluteAddress(address) => {
                    if imm == 0 {
                        self.store16(zr, address);
                        return;
                    }

                    let mut r = self.data_temp_register;
                    self.move_to_cached_reg32(imm, &mut r);
                    self.data_temp_register = r;
                    self.store16(Self::DATA_TEMP_REGISTER, address);
                }

                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    pub fn store64(&mut self, src: impl Into<Operand>, dest: impl Into<Operand>) {
        match (src.into(), dest.into()) {
            (Operand::Register(src), Operand::Address(address)) => {
                if self.try_store_with_offset::<64>(src, address.base, address.offset) {
                    return;
                }

                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                self.assembler.str::<64>(src, address.base, r);
                self.sign_extend32_to_64(address.offset, r);
            }

            (Operand::Register(src), Operand::BaseIndex(address)) => {
                if matches!(address.scale, Scale::TimesOne | Scale::TimesFour) {
                    if let Some(base) = self.try_fold_base_and_offset_part(address) {
                        self.assembler.str_extend::<64>(
                            src,
                            base,
                            address.index,
                            Self::index_extend_type(address),
                            address.scale as _,
                        );
                        return;
                    }
                }

                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                self.assembler.str::<64>(src, address.base, r);
                self.sign_extend32_to_64(address.offset, r);
                self.assembler.add_extend::<64, false>(
                    r,
                    r,
                    address.index,
                    Self::index_extend_type(address),
                    address.scale as _,
                );
            }

            (Operand::Register(src), Operand::AbsoluteAddress(address)) => {
                self.store_internal::<64>(src, address.ptr as _);
            }

            (Operand::Imm64(imm), Operand::Address(address)) => {
                if imm == 0 {
                    self.store64(zr, address);
                    return;
                }

                let mut r = self.data_temp_register;
                self.move_to_cached_reg64(imm, &mut r);
                self.data_temp_register = r;
                self.store64(Self::DATA_TEMP_REGISTER, address);
            }

            (Operand::Imm64(imm), Operand::BaseIndex(address)) => {
                if imm == 0 {
                    self.store64(zr, address);
                    return;
                }

                let mut r = self.data_temp_register;
                self.move_to_cached_reg64(imm, &mut r);
                self.data_temp_register = r;
                self.store64(Self::DATA_TEMP_REGISTER, address);
            }

            (Operand::Imm64(imm), Operand::AbsoluteAddress(address)) => {
                if imm == 0 {
                    self.store64(zr, address);
                    return;
                }

                let mut r = self.data_temp_register;
                self.move_to_cached_reg64(imm, &mut r);
                self.data_temp_register = r;
                self.store64(Self::DATA_TEMP_REGISTER, address);
            }

            (Operand::Imm32(imm), Operand::Address(address)) => {
                if imm == 0 {
                    self.store64(zr, address);
                    return;
                }

                let mut r = self.data_temp_register;
                self.move_to_cached_reg32(imm, &mut r);
                self.data_temp_register = r;
                self.store64(Self::DATA_TEMP_REGISTER, address);
            }

            (Operand::Imm32(imm), Operand::BaseIndex(address)) => {
                if imm == 0 {
                    self.store64(zr, address);
                    return;
                }

                let mut r = self.data_temp_register;
                self.move_to_cached_reg32(imm, &mut r);
                self.data_temp_register = r;
                self.store64(Self::DATA_TEMP_REGISTER, address);
            }

            (Operand::Imm32(imm), Operand::AbsoluteAddress(address)) => {
                if imm == 0 {
                    self.store64(zr, address);
                    return;
                }

                let mut r = self.data_temp_register;
                self.move_to_cached_reg32(imm, &mut r);
                self.data_temp_register = r;
                self.store64(Self::DATA_TEMP_REGISTER, address);
            }

            _ => unreachable!(),
        }
    }

    pub fn index_extend_type(address: BaseIndex) -> ExtendType {
        match address.extend {
            Extend::None => ExtendType::UXTX,
            Extend::ZExt32 => ExtendType::UXTW,
            Extend::SExt32 => ExtendType::SXTW,
        }
    }

    pub fn sign_extend16_to_32(&mut self, src: u8, dest: u8) {
        self.assembler.sxth::<32>(dest, src);
    }

    pub fn sign_extend16_to_64(&mut self, src: u8, dest: u8) {
        self.assembler.sxth::<64>(dest, src);
    }

    pub fn sign_extend8_to_32(&mut self, src: u8, dest: u8) {
        self.assembler.sxtb::<32>(dest, src);
    }

    pub fn sign_extend8_to_64(&mut self, src: u8, dest: u8) {
        self.assembler.sxtb::<64>(dest, src);
    }

    pub fn sign_extend32_to_64(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Imm32(imm) => {
                self.mov(imm as i64, dest);
            }

            Operand::Register(src) => {
                self.assembler.sxtw(dest, src);
            }

            _ => unreachable!(),
        }
    }

    pub fn breakpoint(&mut self) {
        todo!()
    }

    pub unsafe fn link_call(code: *mut u8, call: Call, function: *const u8) {
        if !call.is_flag_set(Call::NEAR) {
            ARM64Assembler::link_pointer(
                code,
                call.label
                    .label_at_offset(Self::REPATCH_OFFSET_CALL_TO_POINTER as i32),
                function as _,
            );
        } else if call.is_flag_set(Call::TAIL) {
            ARM64Assembler::link_jump_(code, call.label, function as _);
        } else {
            ARM64Assembler::link_call(code, call.label, function as _);
        }
    }

    pub fn link_jump_cond(
        &mut self,
        from: AssemblerLabel,
        to: AssemblerLabel,
        typ: JumpType,
        cond: Condition,
    ) {
        self.assembler.link_jump_cond(from, to, typ, cond)
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
        self.assembler
            .link_jump_cmp(from, to, typ, cond, is_64bit, compare_register)
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
        self.assembler
            .link_jump_test_bit(from, to, typ, cond, bit_number, compare_register)
    }

    pub unsafe fn link_pointer(code: *mut u8, label: AssemblerLabel, target: *const u8) {
        ARM64Assembler::link_pointer(code, label, target as _)
    }

    pub fn load_unsigned_immediate<const N: i32>(&mut self, rt: u8, rn: u8, pimm: usize) {
        match N {
            8 => self.assembler.ldrb_imm(rt, rn, pimm as _),
            16 => self.assembler.ldrh_imm(rt, rn, pimm as _),
            _ => self.assembler.ldr_imm::<N>(rt, rn, pimm as _),
        }
    }

    pub fn load_signed_address_by_unsigned_immediate<const N: i32>(
        &mut self,
        rt: u8,
        rn: u8,
        pimm: usize,
    ) {
        match N {
            8 => self.assembler.ldrsb_imm::<64>(rt, rn, pimm as _),
            16 => self.assembler.ldrsh_imm::<64>(rt, rn, pimm as _),
            _ => unreachable!(),
        }
    }

    pub fn load_unscaled_immediate<const N: i32>(&mut self, rt: u8, rn: u8, simm: i32) {
        match N {
            8 => self.assembler.ldurb(rt, rn, simm),
            16 => self.assembler.ldurh(rt, rn, simm),

            _ => self.assembler.ldur::<N>(rt, rn, simm),
        }
    }

    pub fn load_signed_address_by_unscaled_immediate<const N: i32>(
        &mut self,
        rt: u8,
        rn: u8,
        simm: i32,
    ) {
        match N {
            8 => self.assembler.ldursb::<64>(rt, rn, simm),
            16 => self.assembler.ldursh::<64>(rt, rn, simm),
            _ => unreachable!(),
        }
    }

    pub fn store_unsigned_immediate<const N: i32>(&mut self, rt: u8, rn: u8, pimm: usize) {
        match N {
            8 => self.assembler.strb_imm(rt, rn, pimm as _),
            16 => self.assembler.strh_imm(rt, rn, pimm as _),
            _ => self.assembler.str_imm::<N>(rt, rn, pimm as _),
        }
    }

    pub fn store_unscaled_immediate<const N: i32>(&mut self, rt: u8, rn: u8, simm: i32) {
        match N {
            8 => self.assembler.sturb(rt, rn, simm),
            16 => self.assembler.sturh(rt, rn, simm),
            _ => self.assembler.stur::<N>(rt, rn, simm),
        }
    }

    fn move_internal32(&mut self, value: i32, dest: u8) {
        const DATASIZE: i32 = 32;
        const NUMBER_HALF_WORDS: usize = DATASIZE as usize / 16;
        let mut halfword = [0u16; NUMBER_HALF_WORDS];

        if value == 0 {
            self.assembler.movz::<DATASIZE>(dest, 0, 0);
            return;
        }

        if !value == 0 {
            self.assembler.movn::<DATASIZE>(dest, 0, 0);
            return;
        }

        let logical_imm = ARM64LogicalImmediate::create32(value as _);

        if logical_imm.is_valid() {
            self.assembler.movi::<DATASIZE>(dest, logical_imm);
            return;
        }
        // Figure out how many halfwords are 0 or FFFF, then choose movz or movn accordingly.
        let mut zero_or_negate_vote = 0;
        for i in 0..NUMBER_HALF_WORDS {
            halfword[i] = get_half_word(value as u32 as u64, i);
            if halfword[i] == 0 {
                zero_or_negate_vote += 1;
            } else if halfword[i] == 0xffff {
                zero_or_negate_vote -= 1;
            }
        }

        let mut need_to_clear_register = false;

        if zero_or_negate_vote >= 0 {
            for i in 0..NUMBER_HALF_WORDS {
                if halfword[i] != 0 {
                    if need_to_clear_register {
                        self.assembler
                            .movz::<DATASIZE>(dest, halfword[i], 16 * i as i32);
                        need_to_clear_register = false;
                    } else {
                        self.assembler
                            .movk::<DATASIZE>(dest, halfword[i], 16 * i as i32);
                    }
                }
            }
        } else {
            for i in 0..NUMBER_HALF_WORDS {
                if halfword[i] != 0xffff {
                    if need_to_clear_register {
                        self.assembler
                            .movn::<DATASIZE>(dest, halfword[i], 16 * i as i32);
                        need_to_clear_register = false;
                    } else {
                        self.assembler
                            .movk::<DATASIZE>(dest, halfword[i], 16 * i as i32);
                    }
                }
            }
        }
    }

    fn move_internal64(&mut self, value: i64, dest: u8) {
        const DATASIZE: i32 = 64;
        const NUMBER_HALF_WORDS: usize = DATASIZE as usize / 16;
        let mut halfword = [0u16; NUMBER_HALF_WORDS];

        if value == 0 {
            self.assembler.movz::<DATASIZE>(dest, 0, 0);
            return;
        }

        if !value == 0 {
            self.assembler.movn::<DATASIZE>(dest, 0, 0);
            return;
        }

        let logical_imm = ARM64LogicalImmediate::create64(value as _);

        if logical_imm.is_valid() {
            self.assembler.movi::<DATASIZE>(dest, logical_imm);
            return;
        }
        // Figure out how many halfwords are 0 or FFFF, then choose movz or movn accordingly.
        let mut zero_or_negate_vote = 0;
        for i in 0..NUMBER_HALF_WORDS {
            halfword[i] = get_half_word(value as u64, i);
            if halfword[i] == 0 {
                zero_or_negate_vote += 1;
            } else if halfword[i] == 0xffff {
                zero_or_negate_vote -= 1;
            }
        }

        let mut need_to_clear_register = false;

        if zero_or_negate_vote >= 0 {
            for i in 0..NUMBER_HALF_WORDS {
                if halfword[i] != 0 {
                    if need_to_clear_register {
                        self.assembler
                            .movz::<DATASIZE>(dest, halfword[i], 16 * i as i32);
                        need_to_clear_register = false;
                    } else {
                        self.assembler
                            .movk::<DATASIZE>(dest, halfword[i], 16 * i as i32);
                    }
                }
            }
        } else {
            for i in 0..NUMBER_HALF_WORDS {
                if halfword[i] != 0xffff {
                    if need_to_clear_register {
                        self.assembler
                            .movn::<DATASIZE>(dest, halfword[i], 16 * i as i32);
                        need_to_clear_register = false;
                    } else {
                        self.assembler
                            .movk::<DATASIZE>(dest, halfword[i], 16 * i as i32);
                    }
                }
            }
        }
    }

    pub fn move_with_fixed_width32(&mut self, imm: i32, dest: u8) {
        self.assembler
            .movz::<32>(dest, get_half_word(imm as u32 as u64, 0), 0);
        self.assembler
            .movk::<32>(dest, get_half_word(imm as u32 as u64, 1), 16);
    }

    pub fn move_with_fixed_width64(&mut self, imm: i64, dest: u8) {
        self.assembler
            .movz::<64>(dest, get_half_word(imm as u64, 0), 0);
        self.assembler
            .movk::<64>(dest, get_half_word(imm as u64, 1), 16);

        if NUMBER_OF_ADDRESS_ENCODING_INSTRUCTIONS > 2 {
            self.assembler
                .movk::<64>(dest, get_half_word(imm as u64, 2), 32);
        }

        if NUMBER_OF_ADDRESS_ENCODING_INSTRUCTIONS > 3 {
            self.assembler
                .movk::<64>(dest, get_half_word(imm as u64, 3), 48);
        }
    }

    pub fn sign_extend_32_to_ptr_with_fixed_width(&mut self, value: i32, dst: u8) {
        if value >= 0 {
            self.assembler
                .movz::<32>(dst, get_half_word(value as u32 as u64, 0), 0);
            self.assembler
                .movk::<32>(dst, get_half_word(value as u32 as u64, 1), 16);
        } else {
            self.assembler
                .movn::<32>(dst, !get_half_word(value as u32 as u64, 0), 0);
            self.assembler
                .movk::<32>(dst, get_half_word(value as u32 as u64, 1), 16);
        }
    }

    fn load_internal<const DATASIZE: i32>(&mut self, address: *const u8, dst: u8) {
        if let Some(current_register_contents) = self.memory_temp_register.value(self) {
            let address_as_int = address as isize;
            let address_delta = address_as_int.wrapping_sub(current_register_contents as isize);

            if dst == Self::MEMORY_TEMP_REGISTER {
                let mut reg = self.memory_temp_register;
                reg.invalidate(self);
                self.memory_temp_register = reg;
            }

            if address_delta as i32 as isize == address_delta {
                if can_encode_simm_offset(address_delta as _) {
                    self.load_unscaled_immediate::<DATASIZE>(
                        dst,
                        Self::MEMORY_TEMP_REGISTER,
                        address_delta as _,
                    );
                    return;
                }

                if can_encode_pimm_offset::<DATASIZE>(address_delta as _) {
                    return self.load_unsigned_immediate::<DATASIZE>(
                        dst,
                        Self::MEMORY_TEMP_REGISTER,
                        address_delta as _,
                    );
                }
            }

            if (address_as_int & !(Self::MASK_HALF_WORD_0 as isize))
                == (current_register_contents & !(Self::MASK_HALF_WORD_0 as isize))
            {
                self.assembler.movk::<64>(
                    Self::MEMORY_TEMP_REGISTER,
                    (address_as_int & Self::MASK_HALF_WORD_0 as isize) as _,
                    0,
                );

                if DATASIZE == 16 {
                    self.assembler.ldrh(dst, Self::MEMORY_TEMP_REGISTER, zr);
                } else {
                    self.assembler
                        .ldr::<DATASIZE>(dst, Self::MEMORY_TEMP_REGISTER, zr);
                }

                return;
            }
        }

        self.mov(address as i64, Self::MEMORY_TEMP_REGISTER);
        let mut reg = self.memory_temp_register;
        if dst == Self::MEMORY_TEMP_REGISTER {
            reg.invalidate(self);
        } else {
            reg.set_value(self, address as _);
        }

        self.memory_temp_register = reg;

        if DATASIZE == 16 {
            self.assembler.ldrh(dst, Self::MEMORY_TEMP_REGISTER, zr);
        } else {
            self.assembler
                .ldr::<DATASIZE>(dst, Self::MEMORY_TEMP_REGISTER, zr);
        }
    }

    fn store_internal<const DATASIZE: i32>(&mut self, src: u8, address: *const u8) {
        if let Some(current_register_contents) = self.memory_temp_register.value(self) {
            let address_as_int = address as isize;
            let address_delta = address_as_int.wrapping_sub(current_register_contents as isize);

            if address_delta as i32 as isize == address_delta {
                if can_encode_simm_offset(address_delta as _) {
                    self.store_unscaled_immediate::<DATASIZE>(
                        src,
                        Self::MEMORY_TEMP_REGISTER,
                        address_delta as _,
                    );
                    return;
                }

                if can_encode_pimm_offset::<DATASIZE>(address_delta as _) {
                    return self.store_unsigned_immediate::<DATASIZE>(
                        src,
                        Self::MEMORY_TEMP_REGISTER,
                        address_delta as _,
                    );
                }
            }

            if (address_as_int & !(Self::MASK_HALF_WORD_0 as isize))
                == (current_register_contents & !(Self::MASK_HALF_WORD_0 as isize))
            {
                self.assembler.movk::<64>(
                    Self::MEMORY_TEMP_REGISTER,
                    (address_as_int & Self::MASK_HALF_WORD_0 as isize) as _,
                    0,
                );

                if DATASIZE == 16 {
                    self.assembler.strh(src, Self::MEMORY_TEMP_REGISTER, zr);
                } else {
                    self.assembler
                        .str::<DATASIZE>(src, Self::MEMORY_TEMP_REGISTER, zr);
                }

                return;
            }
        }

        self.mov(address as i64, Self::MEMORY_TEMP_REGISTER);
        let mut reg = self.memory_temp_register;
        reg.set_value(self, address as _);
        self.memory_temp_register = reg;

        if DATASIZE == 16 {
            self.assembler.strh(src, Self::MEMORY_TEMP_REGISTER, zr);
        } else {
            self.assembler
                .str::<DATASIZE>(src, Self::MEMORY_TEMP_REGISTER, zr);
        }
    }

    fn try_move_using_cache_register_contents<const DATASIZE: i32>(
        &mut self,
        immediate: isize,
        dest: &mut CachedTempRegister,
    ) -> bool {
        if let Some(current_register_contents) = dest.value(self) {
            if current_register_contents == immediate {
                return true;
            }

            let logical_imm = if DATASIZE == 64 {
                LogicalImmediate::create64(immediate as u64)
            } else {
                LogicalImmediate::create32(immediate as u32)
            };

            if logical_imm.is_valid() {
                self.assembler
                    .movi::<DATASIZE>(dest.register_id_no_invalidate(), logical_imm);
                dest.set_value(self, immediate);
                return true;
            }

            if (immediate & Self::MASK_UPPER_WORD as isize)
                == (current_register_contents & Self::MASK_UPPER_WORD as isize)
            {
                if (immediate & Self::MASK_HALF_WORD_1 as isize)
                    != (current_register_contents & Self::MASK_HALF_WORD_1 as isize)
                {
                    self.assembler.movk::<DATASIZE>(
                        dest.register_id_no_invalidate(),
                        ((immediate & Self::MASK_HALF_WORD_1 as isize) >> 16) as _,
                        16,
                    );
                }

                if (immediate & Self::MASK_HALF_WORD_0 as isize)
                    != (current_register_contents & Self::MASK_HALF_WORD_0 as isize)
                {
                    self.assembler.movk::<DATASIZE>(
                        dest.register_id_no_invalidate(),
                        (immediate & Self::MASK_HALF_WORD_0 as isize) as _,
                        0,
                    );
                }

                dest.set_value(self, immediate);
                return true;
            }
        }

        false
    }

    fn move_to_cached_reg32(&mut self, imm: i32, dest: &mut CachedTempRegister) {
        if self.try_move_using_cache_register_contents::<32>(imm as _, dest) {
            return;
        }

        self.move_internal32(imm, dest.register_id_no_invalidate());
        dest.set_value(self, imm as _);
    }

    fn move_to_cached_reg64(&mut self, imm: i64, dest: &mut CachedTempRegister) {
        if self.try_move_using_cache_register_contents::<64>(imm as _, dest) {
            return;
        }

        self.move_internal64(imm, dest.register_id_no_invalidate());
        dest.set_value(self, imm as _);
    }

    fn try_load_with_offset<const DATASIZE: i32>(&mut self, rt: u8, rn: u8, offset: i32) -> bool {
        if can_encode_simm_offset(offset) {
            self.load_unscaled_immediate::<DATASIZE>(rt, rn, offset);
            return true;
        }

        if can_encode_pimm_offset::<DATASIZE>(offset) {
            self.load_unsigned_immediate::<DATASIZE>(rt, rn, offset as _);
            return true;
        }

        false
    }

    fn try_load_signed_with_offset<const DATASIZE: i32>(
        &mut self,
        rt: u8,
        rn: u8,
        offset: i32,
    ) -> bool {
        if can_encode_simm_offset(offset) {
            self.load_signed_address_by_unscaled_immediate::<DATASIZE>(rt, rn, offset);
            return true;
        }

        if can_encode_pimm_offset::<DATASIZE>(offset) {
            self.load_signed_address_by_unsigned_immediate::<DATASIZE>(rt, rn, offset as _);
            return true;
        }

        false
    }

    fn fp_try_load_with_offset<const DATASIZE: i32>(
        &mut self,
        rt: u8,
        rn: u8,
        offset: i32,
    ) -> bool {
        if can_encode_simm_offset(offset) {
            self.assembler.ldur_fp::<DATASIZE>(rt, rn, offset);
            return true;
        }

        if can_encode_pimm_offset::<DATASIZE>(offset) {
            self.assembler.ldr_imm_fp::<DATASIZE>(rt, rn, offset as _);
            return true;
        }

        false
    }

    fn try_store_with_offset<const DATASIZE: i32>(&mut self, rt: u8, rn: u8, offset: i32) -> bool {
        if can_encode_simm_offset(offset) {
            self.store_unscaled_immediate::<DATASIZE>(rt, rn, offset);
            return true;
        }

        if can_encode_pimm_offset::<DATASIZE>(offset) {
            self.store_unsigned_immediate::<DATASIZE>(rt, rn, offset as _);
            return true;
        }

        false
    }

    fn fp_try_store_with_offset<const DATASIZE: i32>(
        &mut self,
        rt: u8,
        rn: u8,
        offset: i32,
    ) -> bool {
        if can_encode_simm_offset(offset) {
            self.assembler.stur_fp::<DATASIZE>(rt, rn, offset);
            return true;
        }

        if can_encode_pimm_offset::<DATASIZE>(offset) {
            self.assembler.str_imm_fp::<DATASIZE>(rt, rn, offset as _);
            return true;
        }

        false
    }

    fn try_fold_base_and_offset_part(&mut self, address: BaseIndex) -> Option<u8> {
        let immediate = address.offset;
        if immediate == 0 {
            return Some(address.base);
        }

        if let Some((uint12, shift, negated)) = Self::try_extract_shifted_imm(immediate as _) {
            let r = self.get_cached_memory_temp_register_id_and_invalidate();
            if !negated {
                self.assembler
                    .add_imm::<64, false>(r, address.base, uint12, shift as _);
            } else {
                self.assembler
                    .sub_imm::<64, false>(r, address.base, uint12, shift as _);
            }

            return Some(Self::MEMORY_TEMP_REGISTER);
        }

        None
    }

    fn get_cached_memory_temp_register_id_and_invalidate(&mut self) -> u8 {
        let mut reg = self.memory_temp_register;
        let r = reg.register_id_invalidate(self);
        self.memory_temp_register = reg;
        r
    }

    fn get_cached_data_temp_register_id_and_invalidate(&mut self) -> u8 {
        let mut reg = self.data_temp_register;
        let r = reg.register_id_invalidate(self);
        self.data_temp_register = reg;
        r
    }

    fn try_extract_shifted_imm(imm: isize) -> Option<(UInt12, i32, bool)> {
        if is_uint12(imm as _) {
            return Some((UInt12(imm as _), 0, false));
        }

        let negated_imm = {
            let imm = imm as usize;
            imm.not().wrapping_add(1) as isize
        };

        if is_uint12(negated_imm as _) {
            return Some((UInt12(negated_imm as _), 0, true));
        }

        let shifted = imm.wrapping_shr(12);

        if shifted.wrapping_shl(12) == imm {
            if is_uint12(shifted as _) {
                return Some((UInt12(shifted as _), 12, false));
            }

            let negated_shifted = {
                let shifted = shifted as usize;
                shifted.not().wrapping_add(1) as isize
            };

            if is_uint12(negated_shifted as _) {
                return Some((UInt12(negated_imm as _), 12, true));
            }
        }

        None
    }

    fn extract_simple_address(&mut self, address: impl Into<Operand>) -> u8 {
        match address.into() {
            Operand::Address(addr) => {
                if addr.offset == 0 {
                    return addr.base;
                }
                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                self.sign_extend32_to_64(addr.offset, r);
                return Self::MEMORY_TEMP_REGISTER;
            }

            Operand::BaseIndex(addr) => {
                let result = self.get_cached_memory_temp_register_id_and_invalidate();
                self.lshift64_rrr(addr.index as i32, addr.scale as i32, result);

                self.add64(addr.base, result);
                self.add64(addr.offset, result);
                result
            }

            _ => todo!(),
        }
    }
}
