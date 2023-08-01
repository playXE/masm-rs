use std::mem::transmute;
use std::ops::{Deref, DerefMut, Not};

use crate::assembler::abstract_macro_assembler::AbsoluteAddress;
use crate::assembler::assembler_common::ARM64LogicalImmediate;
use crate::wtf::has_one_bit_set;

use super::abstract_macro_assembler::{
    AbstractMacroAssembler, Address, BaseIndex, CachedTempRegister, Call, ConvertibleLoadLabel,
    DataLabel32, DataLabelCompact, Extend, Jump, JumpList, Operand, PostIndexAddress,
    PreIndexAddress, Scale,
};
use super::arm64assembler::*;
use super::assembler_common::{
    is_int, is_uint12, is_unsigned_res, mask16_on_condition_res, mask8_on_condition_res, SIMDLane,
};
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum BranchTruncateType {
    BranchIfTruncateFailed,
    BranchIfTruncateSucceeded,
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

    pub fn new() -> Self {
        Self {
            masm: AbstractMacroAssembler::new(),
            data_temp_register: CachedTempRegister::new(Self::DATA_TEMP_REGISTER),
            memory_temp_register: CachedTempRegister::new(Self::MEMORY_TEMP_REGISTER),
        }
    }

    pub fn jumps_to_link(&mut self) -> &[LinkRecord] {
        self.assembler.jumps_to_link()
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
        self.move64_to_double(src, temp);
        self.assembler.vector_cnt(temp, temp, SIMDLane::I8X16);
        self.assembler.addv(temp, temp, SIMDLane::I8X16);
        self.move_double_to64(temp, dst);
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
                    self.assembler.orr_imm::<32>(dest, reg, logical_imm)
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

    pub fn or8(&mut self, src: impl Into<Operand>, address: AbsoluteAddress) {
        match src.into() {
            Operand::Imm32(imm) => {
                let logical_imm = LogicalImmediate::create32(imm as _);

                if logical_imm.is_valid() {
                    let r = self.get_cached_data_temp_register_id_and_invalidate();
                    self.load8(address, r);
                    self.assembler.orr_imm::<32>(r, r, logical_imm);
                    self.store8(r, address);
                } else {
                    let r = self.get_cached_data_temp_register_id_and_invalidate();
                    let rm = self.get_cached_memory_temp_register_id_and_invalidate();
                    self.load8(address, rm);

                    self.or32_rrr(imm, Self::MEMORY_TEMP_REGISTER, r);
                    self.store8(r, address);
                }
            }

            Operand::Register(src) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load8(address, r);
                self.or32_rrr(src, r, r);
                self.store8(r, address);
            }

            _ => todo!(),
        }
    }

    pub fn or64_rrr(&mut self, a: impl Into<Operand>, src: u8, dest: u8) {
        match a.into() {
            Operand::Register(a) => {
                self.assembler.orr::<64>(dest, a, src);
            }

            Operand::Imm32(imm) => {
                let logical_imm = LogicalImmediate::create64(imm as i64 as _);

                if logical_imm.is_valid() {
                    self.assembler.orr_imm::<64>(dest, src, logical_imm);
                } else {
                    let r = self.get_cached_data_temp_register_id_and_invalidate();
                    self.sign_extend32_to_64(imm, r);
                    self.assembler.orr::<64>(dest, src, r);
                }
            }

            Operand::Imm64(imm) => {
                let logical_imm = LogicalImmediate::create64(imm as _);

                if logical_imm.is_valid() {
                    self.assembler.orr_imm::<64>(dest, src, logical_imm);
                } else {
                    let r = self.get_cached_data_temp_register_id_and_invalidate();
                    self.mov(imm, r);
                    self.assembler.orr::<64>(dest, src, r);
                }
            }

            _ => unreachable!(),
        }
    }

    pub fn or64(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(src) => {
                self.or64_rrr(dest, src, dest);
            }

            Operand::Imm32(imm) => {
                self.or64_rrr(imm, dest, dest);
            }

            Operand::Imm64(imm) => {
                self.or64_rrr(imm, dest, dest);
            }

            _ => unreachable!(),
        }
    }

    pub fn rotate_right32_rrr(&mut self, src: u8, shift: impl Into<Operand>, dest: u8) {
        match shift.into() {
            Operand::Imm32(imm) => {
                self.assembler.ror_imm::<32>(dest, src, imm & 31);
            }

            Operand::Register(shift) => {
                self.assembler.ror::<32>(dest, src, shift);
            }

            _ => unreachable!(),
        }
    }

    pub fn rotate_right32(&mut self, shift: impl Into<Operand>, dest: u8) {
        match shift.into() {
            Operand::Imm32(imm) => {
                self.assembler.ror_imm::<32>(dest, dest, imm & 31);
            }

            Operand::Register(shift) => {
                self.assembler.ror::<32>(dest, dest, shift);
            }

            _ => unreachable!(),
        }
    }

    pub fn rotate_right64_rrr(&mut self, src: u8, shift: impl Into<Operand>, dest: u8) {
        match shift.into() {
            Operand::Imm32(imm) => {
                self.assembler.ror_imm::<64>(dest, src, imm & 63);
            }

            Operand::Register(shift) => {
                self.assembler.ror::<64>(dest, src, shift);
            }

            _ => unreachable!(),
        }
    }

    pub fn rotate_right64(&mut self, shift: impl Into<Operand>, dest: u8) {
        match shift.into() {
            Operand::Imm32(imm) => {
                self.assembler.ror_imm::<64>(dest, dest, imm & 63);
            }

            Operand::Register(shift) => {
                self.assembler.ror::<64>(dest, dest, shift);
            }

            _ => unreachable!(),
        }
    }

    pub fn rshift32(&mut self, shift: impl Into<Operand>, dest: u8) {
        match shift.into() {
            Operand::Imm32(imm) => {
                self.assembler.asr_imm::<32>(dest, dest, imm & 0x1f);
            }

            Operand::Register(shift) => {
                self.assembler.asr::<32, false>(dest, dest, shift);
            }

            _ => unreachable!(),
        }
    }

    pub fn rshift32_rrr(&mut self, src: u8, shift: impl Into<Operand>, dest: u8) {
        match shift.into() {
            Operand::Imm32(imm) => {
                self.assembler.asr_imm::<32>(dest, src, imm & 0x1f);
            }

            Operand::Register(shift) => {
                self.assembler.asr::<32, false>(dest, src, shift);
            }

            _ => unreachable!(),
        }
    }

    pub fn rshift64(&mut self, shift: impl Into<Operand>, dest: u8) {
        match shift.into() {
            Operand::Imm32(imm) => {
                self.assembler.asr_imm::<64>(dest, dest, imm & 0x3f);
            }

            Operand::Register(shift) => {
                self.assembler.asr::<64, false>(dest, dest, shift);
            }

            _ => unreachable!(),
        }
    }

    pub fn rshift64_rrr(&mut self, src: u8, shift: impl Into<Operand>, dest: u8) {
        match shift.into() {
            Operand::Imm32(imm) => {
                self.assembler.asr_imm::<64>(dest, src, imm & 0x3f);
            }

            Operand::Register(shift) => {
                self.assembler.asr::<64, false>(dest, src, shift);
            }

            _ => unreachable!(),
        }
    }

    pub fn sub32_rrr(&mut self, left: impl Into<Operand>, right: impl Into<Operand>, dest: u8) {
        match (left.into(), right.into()) {
            (Operand::Register(left), Operand::Register(right)) => {
                self.assembler.sub::<32, false>(dest, left, right);
            }

            (Operand::Register(left), Operand::Imm32(imm)) => {
                if let Some((u12, shift, inverted)) = Self::try_extract_shifted_imm(imm as _) {
                    if !inverted {
                        self.assembler.sub_imm::<32, false>(dest, left, u12, shift);
                    } else {
                        self.assembler.add_imm::<32, false>(dest, left, u12, shift);
                    }
                } else {
                    let r = self.get_cached_data_temp_register_id_and_invalidate();
                    self.mov(imm, r);
                    self.assembler.sub::<32, false>(dest, left, r);
                }
            }

            _ => unreachable!(),
        }
    }

    pub fn sub32(&mut self, src: impl Into<Operand>, dest: impl Into<Operand>) {
        match (src.into(), dest.into()) {
            (Operand::Register(src), Operand::Register(dest)) => {
                self.assembler.sub::<32, false>(dest, dest, src);
            }

            (Operand::Imm32(imm), Operand::Register(dest)) => {
                self.sub32_rrr(dest, imm, dest);
            }

            (Operand::Imm32(imm), Operand::Address(address)) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load32(address, r);

                if let Some((u12, shift, inverted)) = Self::try_extract_shifted_imm(imm as _) {
                    if !inverted {
                        self.assembler.sub_imm::<32, false>(r, r, u12, shift);
                    } else {
                        self.assembler.add_imm::<32, false>(r, r, u12, shift);
                    }
                } else {
                    let r2 = self.get_cached_memory_temp_register_id_and_invalidate();
                    self.mov(imm, r2);
                    self.assembler.sub::<32, false>(r, r, r2);
                }

                self.store32(r, address);
            }

            (Operand::Imm32(imm), Operand::AbsoluteAddress(address)) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load32(address, r);

                if let Some((u12, shift, inverted)) = Self::try_extract_shifted_imm(imm as _) {
                    if !inverted {
                        self.assembler.sub_imm::<32, false>(r, r, u12, shift);
                    } else {
                        self.assembler.add_imm::<32, false>(r, r, u12, shift);
                    }
                } else {
                    let r2 = self.get_cached_memory_temp_register_id_and_invalidate();
                    self.mov(imm, r2);
                    self.assembler.sub::<32, false>(r, r, r2);
                }

                self.store32(r, address);
            }

            (Operand::Address(src), Operand::Register(dest)) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load32(src, r);
                self.sub32(dest, r);
            }

            _ => unreachable!(),
        }
    }

    pub fn sub64_rrr(&mut self, left: impl Into<Operand>, right: impl Into<Operand>, dest: u8) {
        match (left.into(), right.into()) {
            (Operand::Register(left), Operand::Register(right)) => {
                self.assembler.sub::<64, false>(dest, left, right);
            }

            (Operand::Register(left), Operand::Imm32(imm)) => {
                if let Some((u12, shift, inverted)) = Self::try_extract_shifted_imm(imm as _) {
                    if !inverted {
                        self.assembler.sub_imm::<64, false>(dest, left, u12, shift);
                    } else {
                        self.assembler.add_imm::<64, false>(dest, left, u12, shift);
                    }
                } else {
                    let r = self.get_cached_data_temp_register_id_and_invalidate();
                    self.mov(imm, r);
                    self.assembler.sub::<64, false>(dest, left, r);
                }
            }

            (Operand::Register(left), Operand::Imm64(imm)) => {
                if let Some((u12, shift, inverted)) = Self::try_extract_shifted_imm(imm as _) {
                    if !inverted {
                        self.assembler.sub_imm::<64, false>(dest, left, u12, shift);
                    } else {
                        self.assembler.add_imm::<64, false>(dest, left, u12, shift);
                    }
                } else {
                    let r = self.get_cached_data_temp_register_id_and_invalidate();
                    self.mov(imm, r);
                    self.assembler.sub::<64, false>(dest, left, r);
                }
            }

            _ => unreachable!(),
        }
    }

    pub fn sub64(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Imm32(imm) => {
                self.sub64_rrr(dest, imm, dest);
            }

            Operand::Imm64(imm) => {
                self.sub64_rrr(dest, imm, dest);
            }

            Operand::Register(src) => {
                self.assembler.sub::<64, false>(dest, dest, src);
            }

            _ => unreachable!("Invalid operand"),
        }
    }

    pub fn urshift32_rrr(&mut self, src: u8, shift_amount: impl Into<Operand>, dest: u8) {
        match shift_amount.into() {
            Operand::Imm32(imm) => {
                self.assembler.lsr_imm::<32>(dest, src, imm & 0x1f);
            }

            Operand::Register(shift_amount) => {
                self.assembler.lsr::<32>(dest, src, shift_amount);
            }

            _ => unreachable!("Invalid operand"),
        }
    }

    pub fn urshift32(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Imm32(imm) => {
                self.urshift32_rrr(dest, imm, dest);
            }

            Operand::Register(src) => {
                self.assembler.lsr::<32>(dest, dest, src);
            }

            _ => unreachable!("Invalid operand"),
        }
    }

    pub fn urshift64_rrr(&mut self, src: u8, shift_amount: impl Into<Operand>, dest: u8) {
        match shift_amount.into() {
            Operand::Imm32(imm) => {
                self.assembler.lsr_imm::<64>(dest, src, imm & 0x3f);
            }

            Operand::Register(shift_amount) => {
                self.assembler.lsr::<64>(dest, src, shift_amount);
            }

            _ => unreachable!("Invalid operand"),
        }
    }

    pub fn urshift64(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Imm32(imm) => {
                self.urshift64_rrr(dest, imm, dest);
            }

            Operand::Register(src) => {
                self.assembler.lsr::<64>(dest, dest, src);
            }

            _ => unreachable!("Invalid operand"),
        }
    }

    pub fn xor32_rrr(&mut self, op1: impl Into<Operand>, src: u8, dest: u8) {
        match op1.into() {
            Operand::Register(op1) => {
                self.assembler.eor::<32>(dest, op1, src);
            }

            Operand::Imm32(imm) => {
                if imm == -1 {
                    self.assembler.mvn::<32>(dest, src);
                } else {
                    let logical_imm = LogicalImmediate::create32(imm as _);
                    if logical_imm.is_valid() {
                        self.assembler.eor_imm::<32>(dest, src, logical_imm);
                    } else {
                        let r = self.get_cached_data_temp_register_id_and_invalidate();
                        self.mov(imm, r);
                        self.assembler.eor::<32>(dest, r, src);
                    }
                }
            }

            _ => unreachable!(),
        }
    }

    pub fn xor32(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(src) => {
                self.assembler.eor::<32>(dest, dest, src);
            }

            Operand::Imm32(imm) => {
                self.xor32_rrr(imm, dest, dest);
            }

            _ => unreachable!("Invalid operand"),
        }
    }

    pub fn xor64_rrr(&mut self, op1: impl Into<Operand>, src: u8, dest: u8) {
        match op1.into() {
            Operand::Register(op1) => {
                self.assembler.eor::<64>(dest, op1, src);
            }

            Operand::Imm64(imm) => {
                if imm == -1 {
                    self.assembler.mvn::<64>(dest, src);
                } else {
                    let logical_imm = LogicalImmediate::create64(imm as _);
                    if logical_imm.is_valid() {
                        self.assembler.eor_imm::<64>(dest, src, logical_imm);
                    } else {
                        let r = self.get_cached_data_temp_register_id_and_invalidate();
                        self.mov(imm, r);
                        self.assembler.eor::<64>(dest, r, src);
                    }
                }
            }

            Operand::Imm32(imm) => {
                if imm == -1 {
                    self.assembler.mvn::<64>(dest, src);
                } else {
                    let logical_imm = LogicalImmediate::create32(imm as i64 as _);
                    if logical_imm.is_valid() {
                        self.assembler.eor_imm::<64>(dest, src, logical_imm);
                    } else {
                        let r = self.get_cached_data_temp_register_id_and_invalidate();
                        self.mov(imm, r);
                        self.assembler.eor::<64>(dest, r, src);
                    }
                }
            }

            _ => unreachable!(),
        }
    }
    pub fn xor64(&mut self, src: impl Into<Operand>, dest: impl Into<Operand>) {
        match (src.into(), dest.into()) {
            (Operand::Register(src), Operand::Register(dest)) => {
                self.assembler.eor::<64>(dest, dest, src);
            }

            (Operand::Register(src), Operand::Address(address)) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load64(address, r);
                self.assembler.eor::<64>(r, r, src);
                self.store64(r, address);
            }

            (Operand::Imm32(imm), Operand::Register(dest)) => {
                self.xor64_rrr(imm, dest, dest);
            }

            (Operand::Imm64(imm), Operand::Register(dest)) => {
                self.xor64_rrr(imm, dest, dest);
            }

            (Operand::Address(address), Operand::Register(dest)) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load64(address, r);
                self.xor64(r, dest);
            }

            _ => unreachable!("Invalid operand"),
        }
    }

    pub fn not32(&mut self, src_dest: u8) {
        self.assembler.mvn::<32>(src_dest, src_dest);
    }

    pub fn not64(&mut self, src_dest: u8) {
        self.assembler.mvn::<64>(src_dest, src_dest);
    }

    pub fn not32_rr(&mut self, src: u8, dest: u8) {
        self.assembler.mvn::<32>(dest, src);
    }

    pub fn not64_rr(&mut self, src: u8, dest: u8) {
        self.assembler.mvn::<64>(dest, src);
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
                if address.scale == Scale::TimesOne || address.scale == Scale::TimesTwo {
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

    pub fn load16_unaligned(&mut self, address: impl Into<Operand>, dest: u8) {
        self.load16(address, dest);
    }

    pub fn load16_signed_extend_to_32(&mut self, address: impl Into<Operand>, dest: u8) {
        match address.into() {
            Operand::Address(address) => {
                if self.try_load_signed_with_offset::<16>(dest, address.base, address.offset) {
                    return;
                }

                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                self.sign_extend32_to_64(address.offset, r);
                self.assembler.ldrsh::<32>(dest, address.base, r);
            }

            Operand::BaseIndex(address) => {
                if address.scale == Scale::TimesOne || address.scale == Scale::TimesTwo {
                    if let Some(base) = self.try_fold_base_and_offset_part(address) {
                        self.assembler.ldrsh_extend::<32>(
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
                self.assembler.ldrsh::<32>(dest, address.base, r);
            }

            Operand::AbsoluteAddress(address) => {
                let mut r = self.memory_temp_register;
                self.move_to_cached_reg64(address.ptr as _, &mut r);

                self.assembler
                    .ldrsh::<32>(dest, Self::MEMORY_TEMP_REGISTER, zr);

                if dest == Self::MEMORY_TEMP_REGISTER {
                    r.invalidate(self);
                }

                self.memory_temp_register = r;
            }
            _ => unreachable!(),
        }
    }

    pub fn zero_extend16_to_32(&mut self, src: u8, dest: u8) {
        self.and32_rrr(0xffffi32, src, dest);
    }

    pub fn signed_extend16_to_32(&mut self, src: u8, dest: u8) {
        self.assembler.sxth::<32>(dest, src);
    }

    pub fn zero_extend16_to_64(&mut self, src: u8, dest: u8) {
        self.and64_rrr(0xffffi32, src, dest);
    }

    pub fn load8(&mut self, address: impl Into<Operand>, dest: u8) {
        match address.into() {
            Operand::Address(address) => {
                if self.try_load_with_offset::<8>(dest, address.base, address.offset) {
                    return;
                }
                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                self.sign_extend32_to_64(address.offset, r);
                self.assembler.ldrb(dest, address.base, r);
            }

            Operand::BaseIndex(address) => {
                if address.scale == Scale::TimesOne {
                    if let Some(base) = self.try_fold_base_and_offset_part(address) {
                        self.assembler.ldrb_extend(
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
                self.assembler.ldrb(dest, address.base, r);
            }
            Operand::AbsoluteAddress(address) => {
                let mut r = self.memory_temp_register;
                self.move_to_cached_reg64(address.ptr as i64, &mut r);

                self.assembler.ldrb(dest, Self::MEMORY_TEMP_REGISTER, zr);
                if dest == Self::MEMORY_TEMP_REGISTER {
                    r.invalidate(self);
                }
                self.memory_temp_register = r;
            }
            _ => unreachable!(),
        }
    }

    pub fn load8_simm(&mut self, src: u8, simm: PostIndex, dest: u8) {
        self.assembler.ldrb_post(dest, src, simm);
    }

    pub fn load8_signed_extend_to_32(&mut self, address: impl Into<Operand>, dest: u8) {
        match address.into() {
            Operand::Address(address) => {
                if self.try_load_signed_with_offset::<8>(dest, address.base, address.offset) {
                    return;
                }

                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                self.sign_extend32_to_64(address.offset, r);
                self.assembler.ldrsb::<32>(dest, address.base, r);
            }

            Operand::BaseIndex(address) => {
                if address.scale == Scale::TimesOne {
                    if let Some(base) = self.try_fold_base_and_offset_part(address) {
                        self.assembler.ldrsb_extend::<32>(
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
                self.assembler.ldrsb::<32>(dest, address.base, r);
            }

            Operand::AbsoluteAddress(address) => {
                let mut r = self.memory_temp_register;
                self.move_to_cached_reg64(address.ptr as i64, &mut r);
                self.assembler
                    .ldrsb::<32>(dest, Self::MEMORY_TEMP_REGISTER, zr);
                if dest == Self::MEMORY_TEMP_REGISTER {
                    r.invalidate(self);
                }
                self.memory_temp_register = r;
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

    pub fn store8(&mut self, src: impl Into<Operand>, dest: impl Into<Operand>) {
        match src.into() {
            Operand::Register(src) => match dest.into() {
                Operand::BaseIndex(address) => {
                    if address.scale == Scale::TimesOne {
                        if let Some(base) = self.try_fold_base_and_offset_part(address) {
                            self.assembler.strb_extend(
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
                    self.assembler.strb(src, address.base, r);
                }

                Operand::AbsoluteAddress(address) => {
                    let r = self.get_cached_memory_temp_register_id_and_invalidate();
                    self.mov(address.ptr as i64, r);
                    self.assembler.strb(src, Self::MEMORY_TEMP_REGISTER, 0);
                }

                Operand::Address(address) => {
                    if self.try_store_with_offset::<8>(src, address.base, address.offset) {
                        return;
                    }

                    let r = self.get_cached_memory_temp_register_id_and_invalidate();
                    self.sign_extend32_to_64(address.offset, r);
                    self.assembler.strb(src, address.base, r);
                }

                _ => unreachable!(),
            },

            Operand::Imm32(imm) => match dest.into() {
                Operand::AbsoluteAddress(address) => {
                    if imm == 0 {
                        self.store8(zr, address);
                        return;
                    }

                    let r = self.get_cached_data_temp_register_id_and_invalidate();
                    self.mov(imm as i64, r);
                    self.store8(r, address);
                }
                Operand::Address(address) => {
                    if imm == 0 {
                        self.store8(zr, address);
                        return;
                    }

                    let r = self.get_cached_data_temp_register_id_and_invalidate();
                    self.mov(imm as i64, r);
                    self.store8(r, address);
                }

                Operand::BaseIndex(address) => {
                    if imm == 0 {
                        self.store8(zr, address);
                        return;
                    }

                    let r = self.get_cached_data_temp_register_id_and_invalidate();
                    self.mov(imm as i64, r);
                    self.store8(r, address);
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    pub fn store8_simm(&mut self, src: u8, dest: u8, simm: PostIndex) {
        self.assembler.strb_post(src, dest, simm);
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
                    if matches!(address.scale, Scale::TimesOne | Scale::TimesTwo) {
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

    pub fn load64_with_address_offset_patch(&mut self, address: Address, dest: u8) -> DataLabel32 {
        let label = DataLabel32::new(self);
        let r = self.get_cached_memory_temp_register_id_and_invalidate();
        self.sign_extend_32_to_ptr_with_fixed_width(address.offset, r);
        self.assembler
            .ldr_extend::<64>(dest, address.base, r, ExtendType::SXTW, 0);
        label
    }

    pub fn load64_with_compact_address_offset_patch(
        &mut self,
        address: Address,
        dest: u8,
    ) -> DataLabelCompact {
        let label = DataLabelCompact::new(self);

        self.assembler
            .ldr::<64>(dest, address.base, address.offset as _);
        label
    }

    pub fn load_pair32(&mut self, src: u8, offset: i32, dest1: u8, dest2: u8) {
        if ARM64Assembler::is_valid_ldp_imm::<32>(offset) {
            self.assembler.ldp_imm::<32>(dest1, dest2, src, offset as _);
        } else {
            if src == dest1 {
                self.load32(Address::new(src, offset + 4), dest2);
                self.load32(Address::new(src, offset), dest1);
            } else {
                self.load32(Address::new(src, offset), dest1);
                self.load32(Address::new(src, offset + 4), dest2);
            }
        }
    }

    pub fn load_pair32_pre(&mut self, src: PreIndexAddress, dest1: u8, dest2: u8) {
        self.assembler
            .ldp_pre::<32>(dest1, dest2, src.base, PairPreIndex(src.index));
    }

    pub fn load_pair32_post(&mut self, src: PostIndexAddress, dest1: u8, dest2: u8) {
        self.assembler
            .ldp_post::<32>(dest1, dest2, src.base, PairPostIndex(src.index));
    }

    pub fn load_pair_float(&mut self, src: u8, offset: i32, dest1: u8, dest2: u8) {
        if ARM64Assembler::is_valid_ldp_fp_imm::<32>(offset) {
            self.assembler
                .ldp_imm_fp::<32>(dest1, dest2, src, offset as _);
        } else {
            if src == dest1 {
                self.load_float(Address::new(src, offset + 4), dest2);
                self.load_float(Address::new(src, offset), dest1);
            } else {
                self.load_float(Address::new(src, offset), dest1);
                self.load_float(Address::new(src, offset + 4), dest2);
            }
        }
    }

    pub fn load_pair64(&mut self, src: u8, offset: i32, dest1: u8, dest2: u8) {
        if ARM64Assembler::is_valid_ldp_imm::<64>(offset) {
            self.assembler.ldp_imm::<64>(dest1, dest2, src, offset as _);
        } else {
            if src == dest1 {
                self.load64(Address::new(src, offset + 8), dest2);
                self.load64(Address::new(src, offset), dest1);
            } else {
                self.load64(Address::new(src, offset), dest1);
                self.load64(Address::new(src, offset + 8), dest2);
            }
        }
    }

    pub fn load_pair64_with_non_temporal_access(
        &mut self,
        src: u8,
        offset: i32,
        dest1: u8,
        dest2: u8,
    ) {
        if ARM64Assembler::is_valid_ldp_imm::<64>(offset) {
            self.assembler.ldnp::<64>(dest1, dest2, src, offset as _);
        } else {
            if src == dest1 {
                self.load64(Address::new(src, offset + 8), dest2);
                self.load64(Address::new(src, offset), dest1);
            } else {
                self.load64(Address::new(src, offset), dest1);
                self.load64(Address::new(src, offset + 8), dest2);
            }
        }
    }

    pub fn load_pair_double(&mut self, src: u8, offset: i32, dest1: u8, dest2: u8) {
        if ARM64Assembler::is_valid_ldp_fp_imm::<64>(offset) {
            self.assembler
                .ldp_imm_fp::<64>(dest1, dest2, src, offset as _);
        } else {
            if src == dest1 {
                self.load_double(Address::new(src, offset + 8), dest2);
                self.load_double(Address::new(src, offset), dest1);
            } else {
                self.load_double(Address::new(src, offset), dest1);
                self.load_double(Address::new(src, offset + 8), dest2);
            }
        }
    }

    pub fn transfer32(&mut self, src: Address, dest: Address) {
        let r = self.get_cached_data_temp_register_id_and_invalidate();
        self.load32(src, r);
        let r = self.get_cached_data_temp_register_id_and_invalidate();
        self.store32(r, dest);
    }

    pub fn transfer64(&mut self, src: Address, dest: Address) {
        let r = self.get_cached_data_temp_register_id_and_invalidate();
        self.load64(src, r);
        let r = self.get_cached_data_temp_register_id_and_invalidate();
        self.store64(r, dest);
    }

    pub fn store64_with_address_offset_patch(&mut self, src: u8, address: Address) -> DataLabel32 {
        let result = DataLabel32::new(self);
        let r = self.get_cached_memory_temp_register_id_and_invalidate();
        self.sign_extend_32_to_ptr_with_fixed_width(address.offset, r);
        self.assembler
            .str_extend::<64>(src, address.base, r, ExtendType::SXTW, 0);
        result
    }

    pub fn store_pair32(&mut self, src1: u8, src2: u8, dest: u8, offset: i32) {
        if ARM64Assembler::is_valid_ldp_imm::<32>(offset) {
            self.assembler
                .stp_pair_imm::<32>(src1, src2, dest, offset as _);
        } else {
            self.store32(src1, Address::new(dest, offset));
            self.store32(src2, Address::new(dest, offset + 4));
        }
    }

    pub fn store_pair64(&mut self, src1: u8, src2: u8, dest: u8, offset: i32) {
        if ARM64Assembler::is_valid_ldp_imm::<64>(offset) {
            self.assembler
                .stp_pair_imm::<64>(src1, src2, dest, offset as _);
        } else {
            self.store64(src1, Address::new(dest, offset));
            self.store64(src2, Address::new(dest, offset + 8));
        }
    }

    pub fn store_pair32_pre(&mut self, src1: u8, src2: u8, dest: PreIndexAddress) {
        self.assembler
            .stp_pair_pre::<32>(src1, src2, dest.base, PairPreIndex(dest.index));
    }

    pub fn store_pair64_pre(&mut self, src1: u8, src2: u8, dest: PreIndexAddress) {
        self.assembler
            .stp_pair_pre::<64>(src1, src2, dest.base, PairPreIndex(dest.index));
    }

    pub fn store_pair32_post(&mut self, src1: u8, src2: u8, dest: PostIndexAddress) {
        self.assembler
            .stp_pair_post::<32>(src1, src2, dest.base, PairPostIndex(dest.index));
    }

    pub fn store_pair64_post(&mut self, src1: u8, src2: u8, dest: PostIndexAddress) {
        self.assembler
            .stp_pair_post::<64>(src1, src2, dest.base, PairPostIndex(dest.index));
    }

    pub fn store_pair_float(&mut self, src1: u8, src2: u8, dest: u8, offset: i32) {
        if ARM64Assembler::is_valid_ldp_fp_imm::<32>(offset) {
            self.assembler
                .stp_pair_fp::<32>(src1, src2, dest, offset as _);
        } else {
            self.store_float(src1, Address::new(dest, offset));
            self.store_float(src2, Address::new(dest, offset + 4));
        }
    }

    pub fn store_pair_double(&mut self, src1: u8, src2: u8, dest: u8, offset: i32) {
        if ARM64Assembler::is_valid_ldp_fp_imm::<64>(offset) {
            self.assembler
                .stp_pair_fp::<64>(src1, src2, dest, offset as _);
        } else {
            self.store_double(src1, Address::new(dest, offset));
            self.store_double(src2, Address::new(dest, offset + 8));
        }
    }

    pub fn store_pair64_with_non_temporal_access(
        &mut self,
        src1: u8,
        src2: u8,
        dest: u8,
        offset: i32,
    ) {
        if ARM64Assembler::is_valid_ldp_imm::<64>(offset) {
            self.assembler
                .stnp_pair::<64>(src1, src2, dest, offset as _);
        } else {
            self.store64(src1, Address::new(dest, offset));
            self.store64(src2, Address::new(dest, offset + 8));
        }
    }

    pub fn convertible_load_ptr(&mut self, address: Address, dest: u8) -> ConvertibleLoadLabel {
        let result = ConvertibleLoadLabel::new(self);
        self.assembler
            .ldr_imm::<64>(dest, address.base, address.offset as _);
        result
    }

    pub fn index_extend_type(address: BaseIndex) -> ExtendType {
        match address.extend {
            Extend::None => ExtendType::UXTX,
            Extend::ZExt32 => ExtendType::UXTW,
            Extend::SExt32 => ExtendType::SXTW,
        }
    }

    pub fn get_effective_address(&mut self, address: BaseIndex, dest: u8) {
        self.assembler.add_shifted::<64, false>(
            dest,
            address.base,
            address.index,
            ShiftType::LSL,
            address.scale as _,
        );
        if address.offset != 0 {
            self.add64(address.offset, dest);
        }
    }

    pub fn zero_extend8_to_64(&mut self, src: u8, dest: u8) {
        self.and64_rrr(0xffi64, src, dest);
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
        self.assembler.brk(0xc471)
    }

    pub fn abs_double(&mut self, src: u8, dest: u8) {
        self.assembler.fabs::<64>(dest, src);
    }

    pub fn abs_float(&mut self, src: u8, dest: u8) {
        self.assembler.fabs::<32>(dest, src);
    }

    pub fn add_double(&mut self, op1: u8, op2: u8, dest: u8) {
        self.assembler.fadd::<64>(dest, op1, op2);
    }

    pub fn add_double_rr(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(src) => {
                self.add_double(dest, src, dest);
            }

            Operand::Address(src) => {
                self.load_double(src, Self::FP_TEMP_REGISTER);
                self.add_double_rr(Self::FP_TEMP_REGISTER, dest);
            }

            Operand::AbsoluteAddress(address) => {
                self.load_double(address, Self::FP_TEMP_REGISTER);
                self.add_double_rr(Self::FP_TEMP_REGISTER, dest);
            }

            _ => unreachable!(),
        }
    }

    pub fn add_float(&mut self, op1: u8, op2: u8, dest: u8) {
        self.assembler.fadd::<32>(dest, op1, op2);
    }

    pub fn add_float_rr(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(src) => {
                self.add_float(dest, src, dest);
            }

            Operand::Address(src) => {
                self.load_float(src, Self::FP_TEMP_REGISTER);
                self.add_float_rr(Self::FP_TEMP_REGISTER, dest);
            }

            Operand::AbsoluteAddress(address) => {
                self.load_float(address, Self::FP_TEMP_REGISTER);
                self.add_float_rr(Self::FP_TEMP_REGISTER, dest);
            }

            _ => unreachable!(),
        }
    }

    pub fn ceil_double(&mut self, src: u8, dest: u8) {
        self.assembler.frintp::<64>(dest, src);
    }

    pub fn ceil_float(&mut self, src: u8, dest: u8) {
        self.assembler.frintp::<32>(dest, src);
    }

    pub fn floor_double(&mut self, src: u8, dest: u8) {
        self.assembler.frintm::<64>(dest, src);
    }

    pub fn floor_float(&mut self, src: u8, dest: u8) {
        self.assembler.frintm::<32>(dest, src);
    }

    pub fn round_to_nearest_int_double(&mut self, src: u8, dest: u8) {
        self.assembler.frintn::<64>(dest, src);
    }

    pub fn round_to_nearest_int_float(&mut self, src: u8, dest: u8) {
        self.assembler.frintn::<32>(dest, src);
    }

    pub fn round_toward_zero_double(&mut self, src: u8, dest: u8) {
        self.assembler.frintz::<64>(dest, src);
    }

    pub fn round_toward_zero_float(&mut self, src: u8, dest: u8) {
        self.assembler.frintz::<32>(dest, src);
    }
    /// Convert 'src' to an integer, and places the resulting 'dest'.
    /// If the result is not representable as a 32 bit value, branch.
    /// May also branch for some values that are representable in 32 bits
    /// (specifically, in this case, 0).
    pub fn branch_convert_double_to_int32(
        &mut self,
        src: u8,
        dest: u8,
        jumps: &mut JumpList,
        _: u8,
        neg_zero_check: bool,
    ) {
        self.assembler.fcvtns::<32, 64>(dest, src);
        self.assembler.scvtf::<64, 32>(Self::FP_TEMP_REGISTER, dest);

        jumps.push(self.branch_double(
            DoubleCondition::NotEqualOrUnordered,
            src,
            Self::FP_TEMP_REGISTER,
        ));

        if neg_zero_check {
            let value_is_non_zero = self.branch_test32(ResultCondition::NonZero, dest, -1);
            let scratch = self.get_cached_memory_temp_register_id_and_invalidate();
            self.assembler.fmov::<64>(scratch, dest);
            jumps.push(self.make_test_bit_and_branch(scratch, 63, ZeroCondition::IsNotZero));
            value_is_non_zero.link(self);
        }
    }

    pub fn branch_double(&mut self, cond: DoubleCondition, left: u8, right: u8) -> Jump {
        self.assembler.fcmp::<64>(left, right);
        self.jump_after_floating_point_compare(cond)
    }

    pub fn branch_float(&mut self, cond: DoubleCondition, left: u8, right: u8) -> Jump {
        self.assembler.fcmp::<32>(left, right);
        self.jump_after_floating_point_compare(cond)
    }

    pub fn branch_double_with_zero(&mut self, cond: DoubleCondition, left: u8) -> Jump {
        self.assembler.fcmp_0::<64>(left);
        self.jump_after_floating_point_compare(cond)
    }

    pub fn branch_float_with_zero(&mut self, cond: DoubleCondition, left: u8) -> Jump {
        self.assembler.fcmp_0::<32>(left);
        self.jump_after_floating_point_compare(cond)
    }

    pub fn compare_double(&mut self, cond: DoubleCondition, left: u8, right: u8, dest: u8) {
        self.floating_point_compare(cond, dest, |this| {
            this.assembler.fcmp::<64>(left, right);
        })
    }

    pub fn compare_float(&mut self, cond: DoubleCondition, left: u8, right: u8, dest: u8) {
        self.floating_point_compare(cond, dest, |this| {
            this.assembler.fcmp::<32>(left, right);
        })
    }

    pub fn compare_double_with_zero(&mut self, cond: DoubleCondition, left: u8, dest: u8) {
        self.floating_point_compare(cond, dest, |this| {
            this.assembler.fcmp_0::<64>(left);
        })
    }

    pub fn compare_float_with_zero(&mut self, cond: DoubleCondition, left: u8, dest: u8) {
        self.floating_point_compare(cond, dest, |this| {
            this.assembler.fcmp_0::<32>(left);
        })
    }

    pub fn branch_double_non_zero(&mut self, reg: u8, _: u8) -> Jump {
        self.assembler.fcmp_0::<64>(reg);
        let unordered = self.make_branch(Condition::VS);
        let result = self.make_branch(Condition::NE);
        unordered.link(self);
        result
    }

    pub fn branch_double_zero_or_nan(&mut self, reg: u8, _: u8) -> Jump {
        self.assembler.fcmp_0::<64>(reg);
        let unordered = self.make_branch(Condition::VS);
        let not_equal = self.make_branch(Condition::NE);
        unordered.link(self);
        let result = self.jump();
        not_equal.link(self);
        result
    }

    pub fn branch_truncate_double_to_int32(
        &mut self,
        src: u8,
        dest: u8,
        branch_type: BranchTruncateType,
    ) -> Jump {
        let r = self.get_cached_data_temp_register_id_and_invalidate();
        self.assembler.fcvtzs::<64, 64>(r, src);
        self.zero_extend32_to_word(r, dest);
        self.assembler.cmp_extend::<64>(r, r, ExtendType::SXTW, 0);
        self.make_branch_rel(
            if branch_type == BranchTruncateType::BranchIfTruncateSucceeded {
                RelationalCondition::Equal
            } else {
                RelationalCondition::NotEqual
            },
        )
    }

    pub fn convert_double_to_float(&mut self, src: u8, dest: u8) {
        self.assembler.fcvt::<32, 64>(dest, src);
    }

    pub fn convert_float_to_double(&mut self, src: u8, dest: u8) {
        self.assembler.fcvt::<64, 32>(dest, src);
    }

    pub fn convert_int32_to_double(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(r) => {
                self.assembler.scvtf::<64, 32>(dest, r);
            }

            Operand::Imm32(imm) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.mov(imm, r);
                self.assembler.scvtf::<64, 32>(dest, r);
            }

            Operand::Address(address) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load32(address, r);
                self.assembler.scvtf::<64, 32>(dest, r);
            }

            Operand::AbsoluteAddress(address) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load32(address, r);
                self.assembler.scvtf::<64, 32>(dest, r);
            }

            _ => unreachable!(),
        }
    }

    pub fn convert_int32_to_float(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(r) => {
                self.assembler.scvtf::<32, 32>(dest, r);
            }

            Operand::Imm32(imm) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.mov(imm, r);
                self.assembler.scvtf::<32, 32>(dest, r);
            }

            Operand::Address(address) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load32(address, r);
                self.assembler.scvtf::<32, 32>(dest, r);
            }

            Operand::AbsoluteAddress(address) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load32(address, r);
                self.assembler.scvtf::<32, 32>(dest, r);
            }

            _ => unreachable!(),
        }
    }

    pub fn convert_int64_to_double(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(r) => {
                self.assembler.scvtf::<64, 64>(dest, r);
            }

            Operand::Imm64(imm) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.mov(imm, r);
                self.assembler.scvtf::<64, 64>(dest, r);
            }

            Operand::Address(address) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load64(address, r);
                self.assembler.scvtf::<64, 64>(dest, r);
            }

            Operand::AbsoluteAddress(address) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load64(address, r);
                self.assembler.scvtf::<64, 64>(dest, r);
            }

            _ => unreachable!(),
        }
    }

    pub fn convert_int64_to_float(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(r) => {
                self.assembler.scvtf::<32, 64>(dest, r);
            }

            Operand::Imm64(imm) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.mov(imm, r);
                self.assembler.scvtf::<32, 64>(dest, r);
            }

            Operand::Address(address) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load64(address, r);
                self.assembler.scvtf::<32, 64>(dest, r);
            }

            Operand::AbsoluteAddress(address) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load64(address, r);
                self.assembler.scvtf::<32, 64>(dest, r);
            }

            _ => unreachable!(),
        }
    }

    pub fn convert_uint64_to_double(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(r) => {
                self.assembler.ucvtf::<64, 64>(dest, r);
            }

            Operand::Imm64(imm) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.mov(imm, r);
                self.assembler.ucvtf::<64, 64>(dest, r);
            }

            Operand::Address(address) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load64(address, r);
                self.assembler.ucvtf::<64, 64>(dest, r);
            }

            Operand::AbsoluteAddress(address) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load64(address, r);
                self.assembler.ucvtf::<64, 64>(dest, r);
            }

            _ => unreachable!(),
        }
    }

    pub fn convert_uint64_to_float(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(r) => {
                self.assembler.ucvtf::<32, 64>(dest, r);
            }

            Operand::Imm64(imm) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.mov(imm, r);
                self.assembler.ucvtf::<32, 64>(dest, r);
            }

            Operand::Address(address) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load64(address, r);
                self.assembler.ucvtf::<32, 64>(dest, r);
            }

            Operand::AbsoluteAddress(address) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load64(address, r);
                self.assembler.ucvtf::<32, 64>(dest, r);
            }

            _ => unreachable!(),
        }
    }

    pub fn div_double(&mut self, op1: u8, op2: u8, dest: u8) {
        self.assembler.fdiv::<64>(dest, op1, op2);
    }

    pub fn div_float(&mut self, op1: u8, op2: u8, dest: u8) {
        self.assembler.fdiv::<32>(dest, op1, op2);
    }

    pub fn div_double_rr(&mut self, src: u8, dest: u8) {
        self.div_double(dest, src, dest)
    }

    pub fn div_float_rr(&mut self, src: u8, dest: u8) {
        self.div_float(dest, src, dest)
    }

    pub fn load_vector(&mut self, address: impl Into<Operand>, dest: u8) {
        match address.into() {
            Operand::Address(address) => {
                if self.fp_try_load_with_offset::<128>(dest, address.base, address.offset) {
                    return;
                }
                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                self.sign_extend32_to_64(address.offset, r);
                self.assembler.ldr_fp::<128>(dest, address.base, r);
            }

            Operand::BaseIndex(address) => {
                if matches!(address.scale, Scale::TimesOne | Scale::TimesFour) {
                    if let Some(base) = self.try_fold_base_and_offset_part(address) {
                        self.assembler.ldr_extend_fp::<128>(
                            dest,
                            address.base,
                            address.index,
                            Self::index_extend_type(address),
                            address.scale as _,
                        );
                        return;
                    }
                }

                let r = self.get_cached_memory_temp_register_id_and_invalidate();

                self.sign_extend32_to_64(address.offset, r);
                self.assembler.add_extend::<128, false>(
                    Self::MEMORY_TEMP_REGISTER,
                    Self::MEMORY_TEMP_REGISTER,
                    address.index,
                    Self::index_extend_type(address),
                    address.scale as _,
                );
                self.assembler.ldr_fp::<128>(dest, address.base, r);
            }

            Operand::AbsoluteAddress(address) => {
                let mut r = self.memory_temp_register;
                self.move_to_cached_reg64(address.ptr as _, &mut r);
                self.assembler
                    .ldr_fp::<128>(dest, Self::MEMORY_TEMP_REGISTER, zr);
                self.memory_temp_register = r;
            }

            _ => unreachable!(),
        }
    }

    pub fn load_double(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Address(address) => {
                if self.fp_try_load_with_offset::<64>(dest, address.base, address.offset) {
                    return;
                }
                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                self.sign_extend32_to_64(address.offset, r);
                self.assembler.ldr_fp::<64>(dest, address.base, r);
            }

            Operand::BaseIndex(address) => {
                if matches!(address.scale, Scale::TimesOne | Scale::TimesEight) {
                    if let Some(base) = self.try_fold_base_and_offset_part(address) {
                        self.assembler.ldr_extend_fp::<64>(
                            dest,
                            address.base,
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
                    Self::MEMORY_TEMP_REGISTER,
                    Self::MEMORY_TEMP_REGISTER,
                    address.index,
                    Self::index_extend_type(address),
                    address.scale as _,
                );
                self.assembler.ldr_fp::<64>(dest, address.base, r);
            }

            Operand::AbsoluteAddress(address) => {
                let mut r = self.memory_temp_register;
                self.move_to_cached_reg64(address.ptr as _, &mut r);
                self.assembler
                    .ldr_fp::<64>(dest, Self::MEMORY_TEMP_REGISTER, zr);
                self.memory_temp_register = r;
            }

            _ => unreachable!(),
        }
    }

    pub fn load_float(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Address(address) => {
                if self.fp_try_load_with_offset::<32>(dest, address.base, address.offset) {
                    return;
                }
                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                self.sign_extend32_to_64(address.offset, r);
                self.assembler.ldr_fp::<32>(dest, address.base, r);
            }

            Operand::BaseIndex(address) => {
                if matches!(address.scale, Scale::TimesOne | Scale::TimesFour) {
                    if let Some(base) = self.try_fold_base_and_offset_part(address) {
                        self.assembler.ldr_extend_fp::<32>(
                            dest,
                            address.base,
                            address.index,
                            Self::index_extend_type(address),
                            address.scale as _,
                        );
                        return;
                    }
                }

                let r = self.get_cached_memory_temp_register_id_and_invalidate();

                self.sign_extend32_to_64(address.offset, r);
                self.assembler.add_extend::<32, false>(
                    Self::MEMORY_TEMP_REGISTER,
                    Self::MEMORY_TEMP_REGISTER,
                    address.index,
                    Self::index_extend_type(address),
                    address.scale as _,
                );
                self.assembler.ldr_fp::<32>(dest, address.base, r);
            }

            Operand::AbsoluteAddress(address) => {
                let mut r = self.memory_temp_register;
                self.move_to_cached_reg64(address.ptr as _, &mut r);
                self.assembler
                    .ldr_fp::<32>(dest, Self::MEMORY_TEMP_REGISTER, zr);
                self.memory_temp_register = r;
            }

            _ => unreachable!(),
        }
    }

    pub fn move_double(&mut self, src: u8, dest: u8) {
        self.assembler.fmov::<64>(dest, src);
    }

    pub fn move_vector(&mut self, src: u8, dest: u8) {
        self.assembler.fmov::<128>(dest, src);
    }

    pub fn materialize_vector(&mut self, src: u128, dest: u8) {
        if src == 0 {
            //self.move_zero_to_vector(dest);
            return;
        }

        let (lo, hi): (u64, u64) = unsafe { std::mem::transmute(src) };
        self.mov(lo as i64, dest);
        //self.vector_splat_int64(Self::DATA_TEMP_REGISTER, dest);
        self.mov(hi as i64, Self::DATA_TEMP_REGISTER);
        //self.vector_replace_lane_int64(1i32, Self::DATA_TEMP_REGISTER, dest);
    }

    pub fn move_zero_to_double(&mut self, dest: u8) {
        // Intentionally use 128bit width here to clear all part of this register with zero.
        self.assembler.movi_fp::<128>(dest, 0);
    }

    pub fn move_zero_to_float(&mut self, dest: u8) {
        self.assembler.movi_fp::<128>(dest, 0);
    }

    pub fn move_double_to64(&mut self, src: u8, dest: u8) {
        self.assembler.fmov_f2i::<64>(dest, src);
    }

    pub fn move_float_to32(&mut self, src: u8, dest: u8) {
        self.assembler.fmov_f2i::<32>(dest, src);
    }

    pub fn move_conditionally_double(
        &mut self,
        cond: DoubleCondition,
        left: u8,
        right: u8,
        src: u8,
        dest: u8,
    ) {
        self.assembler.fcmp::<64>(left, right);
        self.move_conditionally_after_floating_point_compare::<64>(cond, src, dest);
    }

    pub fn move_conditionally_double_with_zero(
        &mut self,
        cond: DoubleCondition,
        left: u8,
        src: u8,
        dest: u8,
    ) {
        self.assembler.fcmp_0::<64>(left);
        self.move_conditionally_after_floating_point_compare::<64>(cond, src, dest);
    }

    pub fn move_conditionally_double_then_else(
        &mut self,
        cond: DoubleCondition,
        left: u8,
        right: u8,
        then_case: u8,
        else_case: u8,
        dest: u8,
    ) {
        self.assembler.fcmp::<64>(left, right);
        self.move_conditionally_after_floating_point_compare_then_else::<64>(
            cond, then_case, else_case, dest,
        );
    }

    pub fn move_conditionally_double_with_zero_then_else(
        &mut self,
        cond: DoubleCondition,
        left: u8,
        then_case: u8,
        else_case: u8,
        dest: u8,
    ) {
        self.assembler.fcmp_0::<64>(left);
        self.move_conditionally_after_floating_point_compare_then_else::<64>(
            cond, then_case, else_case, dest,
        );
    }

    pub fn move_conditionally_float(
        &mut self,
        cond: DoubleCondition,
        left: u8,
        right: u8,
        src: u8,
        dest: u8,
    ) {
        self.assembler.fcmp::<32>(left, right);
        self.move_conditionally_after_floating_point_compare::<32>(cond, src, dest);
    }

    pub fn move_conditionally_float_with_zero(
        &mut self,
        cond: DoubleCondition,
        left: u8,
        src: u8,
        dest: u8,
    ) {
        self.assembler.fcmp_0::<32>(left);
        self.move_conditionally_after_floating_point_compare::<32>(cond, src, dest);
    }

    pub fn move_conditionally_float_then_else(
        &mut self,
        cond: DoubleCondition,
        left: u8,
        right: u8,
        then_case: u8,
        else_case: u8,
        dest: u8,
    ) {
        self.assembler.fcmp::<32>(left, right);
        self.move_conditionally_after_floating_point_compare_then_else::<32>(
            cond, then_case, else_case, dest,
        );
    }

    pub fn move_conditionally_float_with_zero_then_else(
        &mut self,
        cond: DoubleCondition,
        left: u8,
        then_case: u8,
        else_case: u8,
        dest: u8,
    ) {
        self.assembler.fcmp_0::<32>(left);
        self.move_conditionally_after_floating_point_compare_then_else::<32>(
            cond, then_case, else_case, dest,
        );
    }

    pub fn move_double_conditionally_double(
        &mut self,
        cond: DoubleCondition,
        left: u8,
        right: u8,
        then_case: u8,
        else_case: u8,
        dest: u8,
    ) {
        self.assembler.fcmp::<64>(left, right);
        self.move_double_conditionally_after_floating_point_compare_then_else::<64>(
            cond, then_case, else_case, dest,
        )
    }

    pub fn move_double_conditionally_double_with_zero(
        &mut self,
        cond: DoubleCondition,
        left: u8,
        then_case: u8,
        else_case: u8,
        dest: u8,
    ) {
        self.assembler.fcmp_0::<64>(left);
        self.move_double_conditionally_after_floating_point_compare_then_else::<64>(
            cond, then_case, else_case, dest,
        )
    }

    pub fn move_double_conditionally_float(
        &mut self,
        cond: DoubleCondition,
        left: u8,
        right: u8,
        then_case: u8,
        else_case: u8,
        dest: u8,
    ) {
        self.assembler.fcmp::<64>(left, right);
        self.move_double_conditionally_after_floating_point_compare_then_else::<32>(
            cond, then_case, else_case, dest,
        )
    }

    pub fn move_double_conditionally_float_with_zero(
        &mut self,
        cond: DoubleCondition,
        left: u8,
        then_case: u8,
        else_case: u8,
        dest: u8,
    ) {
        self.assembler.fcmp_0::<64>(left);
        self.move_double_conditionally_after_floating_point_compare_then_else::<32>(
            cond, then_case, else_case, dest,
        )
    }

    pub fn move_conditionally_after_floating_point_compare<const DATASIZE: i32>(
        &mut self,
        cond: DoubleCondition,
        src: u8,
        dest: u8,
    ) {
        if cond == DoubleCondition::NotEqualAndOrdered {
            let unordered = self.make_branch(Condition::VS);
            self.assembler
                .csel::<DATASIZE>(dest, src, dest, Condition::NE);
            unordered.link(self);
            return;
        }

        if cond == DoubleCondition::EqualOrUnordered {
            // If the compare is unordered, src is copied to dest and the
            // next csel has all arguments equal to src.
            // If the compare is ordered, dest is unchanged and EQ decides
            // what value to set.
            self.assembler
                .csel::<DATASIZE>(dest, src, dest, Condition::VS);
            self.assembler
                .csel::<DATASIZE>(dest, src, dest, Condition::EQ);
            return;
        }

        self.assembler
            .csel::<DATASIZE>(dest, src, dest, unsafe { transmute(cond) })
    }

    pub fn move_conditionally_after_floating_point_compare_then_else<const DATASIZE: i32>(
        &mut self,
        cond: DoubleCondition,
        then_case: u8,
        else_case: u8,
        dest: u8,
    ) {
        if cond == DoubleCondition::NotEqualAndOrdered {
            if dest == then_case {
                self.assembler
                    .csel::<DATASIZE>(then_case, else_case, then_case, Condition::VS);
                self.assembler
                    .csel::<DATASIZE>(dest, else_case, then_case, Condition::NE);
            } else {
                self.mov(else_case, dest);
                let unordered = self.make_branch(Condition::VS);
                self.assembler
                    .csel::<DATASIZE>(dest, then_case, else_case, Condition::NE);
                unordered.link(self);
            }
            return;
        }

        if cond == DoubleCondition::EqualOrUnordered {
            if dest == else_case {
                self.assembler
                    .csel::<DATASIZE>(else_case, then_case, else_case, Condition::VS);
                self.assembler
                    .csel::<DATASIZE>(dest, then_case, else_case, Condition::EQ);
            } else {
                self.mov(then_case, dest);
                let unordered = self.make_branch(Condition::VS);
                self.assembler
                    .csel::<DATASIZE>(dest, else_case, then_case, Condition::EQ);
                unordered.link(self);
            }

            return;
        }

        self.assembler
            .csel::<DATASIZE>(dest, then_case, else_case, unsafe { transmute(cond) })
    }

    pub fn move_double_conditionally_after_floating_point_compare_then_else<const DATASIZE: i32>(
        &mut self,
        cond: DoubleCondition,
        then_case: u8,
        else_case: u8,
        dest: u8,
    ) {
        if cond == DoubleCondition::NotEqualAndOrdered {
            if dest == then_case {
                // If the compare is unordered, else_case is copied to then_case and the
                // next fcsel has all arguments equal to else_case.
                // If the compare is ordered, dest is unchanged and NE decides
                // what value to set.
                self.assembler
                    .fcsel::<DATASIZE>(then_case, else_case, then_case, Condition::VS);
                self.assembler
                    .fcsel::<DATASIZE>(dest, else_case, then_case, Condition::NE);
            } else {
                self.assembler.fmov::<64>(dest, else_case);
                let unordered = self.make_branch(Condition::VS);
                self.assembler
                    .fcsel::<DATASIZE>(dest, then_case, else_case, Condition::NE);
                unordered.link(self);
            }

            return;
        }

        if cond == DoubleCondition::EqualOrUnordered {
            if dest == else_case {
                // If the compare is unordered, then_case is copied to else_case and the
                // next csel has all arguments equal to then_case.
                // If the compare is ordered, dest is unchanged and EQ decides
                // what value to set.
                self.assembler
                    .fcsel::<DATASIZE>(else_case, then_case, else_case, Condition::VS);
                self.assembler
                    .fcsel::<DATASIZE>(dest, then_case, else_case, Condition::EQ);
            } else {
                self.assembler.fmov::<64>(dest, then_case);
                let unordered = self.make_branch(Condition::VS);
                self.assembler
                    .fcsel::<DATASIZE>(dest, then_case, else_case, Condition::EQ);
                unordered.link(self);
            }

            return;
        }

        self.assembler
            .fcsel::<DATASIZE>(dest, then_case, else_case, unsafe { transmute(cond) })
    }

    pub fn mul_double(&mut self, op1: u8, op2: u8, dest: u8) {
        self.assembler.fmul::<64>(dest, op1, op2);
    }

    pub fn mul_double_rr(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(src) => {
                self.mul_double(dest, src, dest);
            }

            Operand::Address(address) => {
                self.load_double(address, Self::FP_TEMP_REGISTER);
                self.mul_double(dest, Self::FP_TEMP_REGISTER, dest);
            }

            _ => unreachable!(),
        }
    }

    pub fn mul_float(&mut self, op1: u8, op2: u8, dest: u8) {
        self.assembler.fmul::<32>(dest, op1, op2);
    }

    pub fn mul_float_rr(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(src) => {
                self.mul_float(dest, src, dest);
            }

            Operand::Address(address) => {
                self.load_float(address, Self::FP_TEMP_REGISTER);
                self.mul_float(dest, Self::FP_TEMP_REGISTER, dest);
            }

            _ => unreachable!(),
        }
    }

    pub fn and_double(&mut self, op1: u8, op2: u8, dest: u8) {
        self.assembler.vand::<64>(dest, op1, op2);
    }

    pub fn and_float(&mut self, op1: u8, op2: u8, dest: u8) {
        self.and_double(op1, op2, dest);
    }

    pub fn or_double(&mut self, op1: u8, op2: u8, dest: u8) {
        self.assembler.vorr::<64>(dest, op1, op2);
    }

    pub fn or_float(&mut self, op1: u8, op2: u8, dest: u8) {
        self.or_double(op1, op2, dest);
    }

    pub fn float_max(&mut self, op1: u8, op2: u8, dest: u8) {
        self.assembler.fmax::<64>(dest, op1, op2);
    }

    pub fn float_min(&mut self, op1: u8, op2: u8, dest: u8) {
        self.assembler.fmin::<64>(dest, op1, op2);
    }

    pub fn double_max(&mut self, op1: u8, op2: u8, dest: u8) {
        self.assembler.fmax::<64>(dest, op1, op2);
    }

    pub fn double_min(&mut self, op1: u8, op2: u8, dest: u8) {
        self.assembler.fmin::<64>(dest, op1, op2);
    }

    pub fn negate_double(&mut self, src: u8, dest: u8) {
        self.assembler.fneg::<64>(dest, src);
    }

    pub fn negate_float(&mut self, src: u8, dest: u8) {
        self.assembler.fneg::<32>(dest, src);
    }

    pub fn sqrt_double(&mut self, src: u8, dest: u8) {
        self.assembler.fsqrt::<64>(dest, src);
    }

    pub fn sqrt_float(&mut self, src: u8, dest: u8) {
        self.assembler.fsqrt::<32>(dest, src);
    }

    pub fn store_double(&mut self, src: u8, dest: impl Into<Operand>) {
        match dest.into() {
            Operand::Address(address) => {
                if self.fp_try_store_with_offset::<64>(src, address.base, address.offset) {
                    return;
                }
                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                self.sign_extend32_to_64(address.offset, r);
                self.assembler.str_fp::<64>(src, address.base, r);
            }

            Operand::BaseIndex(address) => {
                if matches!(address.scale, Scale::TimesOne | Scale::TimesEight) {
                    if let Some(base) = self.try_fold_base_and_offset_part(address) {
                        self.assembler.str_extend_fp::<64>(
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
                    Self::MEMORY_TEMP_REGISTER,
                    Self::MEMORY_TEMP_REGISTER,
                    address.index,
                    Self::index_extend_type(address),
                    address.scale as _,
                );

                self.assembler
                    .str_fp::<64>(src, address.base, Self::MEMORY_TEMP_REGISTER);
            }

            Operand::AbsoluteAddress(address) => {
                let mut r = self.memory_temp_register;
                self.move_to_cached_reg64(address.ptr as _, &mut r);
                self.memory_temp_register = r;
                self.assembler
                    .str_fp::<64>(src, Self::MEMORY_TEMP_REGISTER, zr);
            }

            _ => unreachable!(),
        }
    }

    pub fn store_float(&mut self, src: u8, dest: impl Into<Operand>) {
        match dest.into() {
            Operand::Address(address) => {
                if self.fp_try_store_with_offset::<32>(src, address.base, address.offset) {
                    return;
                }
                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                self.sign_extend32_to_64(address.offset, r);
                self.assembler.str_fp::<32>(src, address.base, r);
            }

            Operand::BaseIndex(address) => {
                if matches!(address.scale, Scale::TimesOne | Scale::TimesEight) {
                    if let Some(base) = self.try_fold_base_and_offset_part(address) {
                        self.assembler.str_extend_fp::<32>(
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
                    Self::MEMORY_TEMP_REGISTER,
                    Self::MEMORY_TEMP_REGISTER,
                    address.index,
                    Self::index_extend_type(address),
                    address.scale as _,
                );

                self.assembler
                    .str_fp::<32>(src, address.base, Self::MEMORY_TEMP_REGISTER);
            }

            Operand::AbsoluteAddress(address) => {
                let mut r = self.memory_temp_register;
                self.move_to_cached_reg64(address.ptr as _, &mut r);
                self.memory_temp_register = r;
                self.assembler
                    .str_fp::<32>(src, Self::MEMORY_TEMP_REGISTER, zr);
            }

            _ => unreachable!(),
        }
    }

    pub fn store_vector(&mut self, src: u8, dest: impl Into<Operand>) {
        match dest.into() {
            Operand::Address(address) => {
                if self.fp_try_store_with_offset::<128>(src, address.base, address.offset) {
                    return;
                }
                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                self.sign_extend32_to_64(address.offset, r);
                self.assembler.str_fp::<128>(src, address.base, r);
            }

            Operand::BaseIndex(address) => {
                if matches!(address.scale, Scale::TimesOne | Scale::TimesEight) {
                    if let Some(base) = self.try_fold_base_and_offset_part(address) {
                        self.assembler.str_extend_fp::<128>(
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
                    Self::MEMORY_TEMP_REGISTER,
                    Self::MEMORY_TEMP_REGISTER,
                    address.index,
                    Self::index_extend_type(address),
                    address.scale as _,
                );

                self.assembler
                    .str_fp::<128>(src, address.base, Self::MEMORY_TEMP_REGISTER);
            }

            Operand::AbsoluteAddress(address) => {
                let mut r = self.memory_temp_register;
                self.move_to_cached_reg64(address.ptr as _, &mut r);
                self.memory_temp_register = r;
                self.assembler
                    .str_fp::<128>(src, Self::MEMORY_TEMP_REGISTER, zr);
            }

            _ => unreachable!(),
        }
    }

    pub fn sub_double(&mut self, lhs: u8, rhs: u8, dest: u8) {
        self.assembler.fsub::<64>(dest, lhs, rhs);
    }

    pub fn sub_double_rr(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(reg) => {
                self.assembler.fsub::<64>(dest, dest, reg);
            }

            Operand::Address(address) => {
                self.load_double(address, Self::FP_TEMP_REGISTER);
                self.sub_double_rr(Self::FP_TEMP_REGISTER, dest);
            }

            _ => unreachable!(),
        }
    }

    pub fn sub_float(&mut self, lhs: u8, rhs: u8, dest: u8) {
        self.assembler.fsub::<32>(dest, lhs, rhs);
    }

    pub fn sub_float_rr(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(reg) => {
                self.assembler.fsub::<32>(dest, dest, reg);
            }

            Operand::Address(address) => {
                self.load_float(address, Self::FP_TEMP_REGISTER);
                self.sub_float_rr(Self::FP_TEMP_REGISTER, dest);
            }

            _ => unreachable!(),
        }
    }

    pub fn truncate_double_to_int32(&mut self, src: u8, dest: u8) {
        self.assembler.fcvtzs::<32, 64>(dest, src);
    }

    pub fn truncate_double_to_uint32(&mut self, src: u8, dest: u8) {
        self.assembler.fcvtzu::<32, 64>(dest, src);
    }

    pub fn truncate_double_to_int64(&mut self, src: u8, dest: u8) {
        self.assembler.fcvtzs::<64, 64>(dest, src);
    }

    pub fn truncate_double_to_uint64(&mut self, src: u8, dest: u8) {
        self.assembler.fcvtzu::<64, 64>(dest, src);
    }

    pub fn truncate_float_to_int32(&mut self, src: u8, dest: u8) {
        self.assembler.fcvtzs::<32, 32>(dest, src);
    }

    pub fn truncate_float_to_uint32(&mut self, src: u8, dest: u8) {
        self.assembler.fcvtzu::<32, 32>(dest, src);
    }

    pub fn truncate_float_to_int64(&mut self, src: u8, dest: u8) {
        self.assembler.fcvtzs::<64, 32>(dest, src);
    }

    pub fn truncate_float_to_uint64(&mut self, src: u8, dest: u8) {
        self.assembler.fcvtzu::<64, 32>(dest, src);
    }

    // Stack manipulation operations:
    //
    // The ABI is assumed to provide a stack abstraction to memory,
    // containing machine word sized units of data. Push and pop
    // operations add and remove a single register sized unit of data
    // to or from the stack. These operations are not supported on
    // ARM64. Peek and poke operations read or write values on the
    // stack, without moving the current stack position. Additionally,
    // there are popToRestore and pushToSave operations, which are
    // designed just for quick-and-dirty saving and restoring of
    // temporary values. These operations don't claim to have any
    // ABI compatibility.
    pub fn pop(_: impl Into<Operand>) {
        panic!("pop is not supported on ARM64")
    }

    pub fn push(_: impl Into<Operand>) {
        panic!("push is not supported on ARM64")
    }

    pub fn pop_pair(&mut self, dest1: u8, dest2: u8) {
        self.assembler
            .ldp_post::<64>(dest1, dest2, sp, PairPostIndex(16));
    }

    pub fn push_pair(&mut self, src1: u8, src2: u8) {
        self.assembler
            .stp_pair_pre::<64>(src1, src2, sp, PairPreIndex(-16));
    }

    pub fn pop_to_restore(&mut self, dest: u8) {
        self.assembler.ldr_post::<64>(dest, sp, PostIndex(16));
    }

    pub fn push_to_save(&mut self, src: impl Into<Operand>) {
        match src.into() {
            Operand::Address(address) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load32(address, r);
                self.push_to_save(r);
            }

            Operand::Register(src) => {
                self.assembler.str_pre::<64>(src, sp, PreIndex(-16));
            }

            Operand::Imm32(imm) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.mov(imm, r);
                self.push_to_save(r);
            }

            _ => unreachable!(),
        }
    }

    pub fn push_to_save_fp(&mut self, src: u8) {
        self.sub64(16i32, Self::STACK_POINTER_REGISTER);
        self.store_double(src, Address::new(Self::STACK_POINTER_REGISTER, 0));
    }

    pub fn pop_to_restore_fp(&mut self, dest: u8) {
        self.load_double(Address::new(Self::STACK_POINTER_REGISTER, 0), dest);
        self.add64(16i32, Self::STACK_POINTER_REGISTER);
    }

    pub fn push_to_save_immediate_without_touching_registers(&mut self, imm: i32) {
        // We can use any non-hardware reserved register here since we restore its value.
        // We pick dataTempRegister arbitrarily. We don't need to invalidate it here since
        // we restore its original value.
        let reg = Self::DATA_TEMP_REGISTER;
        self.push_pair(reg, reg);
        self.mov(imm, reg);
        self.store64(reg, Address::new(Self::STACK_POINTER_REGISTER, 0));
        self.load64(Address::new(Self::STACK_POINTER_REGISTER, 8), reg);
    }

    pub fn move64_to_double(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(reg) => {
                self.assembler.fmov_i2f::<64>(dest, reg);
            }

            Operand::Imm64(imm) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.mov(imm, r);
                self.assembler.fmov_i2f::<64>(dest, r);
            }

            _ => unreachable!(),
        }
    }

    pub fn move32_to_float(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(reg) => {
                self.assembler.fmov_i2f::<32>(dest, reg);
            }

            Operand::Imm32(imm) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.mov(imm, r);
                self.assembler.fmov_i2f::<32>(dest, r);
            }

            _ => unreachable!(),
        }
    }

    pub fn swap(&mut self, reg1: u8, reg2: u8) {
        if reg1 == reg2 {
            return;
        }
        let r = self.get_cached_data_temp_register_id_and_invalidate();
        self.mov(reg1, r);
        self.mov(reg2, reg1);
        self.mov(r, reg2);
    }

    pub fn swap_double(&mut self, reg1: u8, reg2: u8) {
        if reg1 == reg2 {
            return;
        }

        self.move_double(reg1, Self::FP_TEMP_REGISTER);
        self.move_double(reg2, reg1);
        self.move_double(Self::FP_TEMP_REGISTER, reg2);
    }

    pub fn zero_extend32_to_word(&mut self, src: u8, dest: u8) {
        self.and64_rrr(0xffffffffi64, src, dest);
    }

    pub fn zero_extend48_to_word(&mut self, src: u8, dest: u8) {
        self.assembler.ubfx::<64>(dest, src, 0, 48);
    }

    pub fn move_conditionally32(
        &mut self,
        cond: RelationalCondition,
        left: u8,
        right: impl Into<Operand>,
        src: u8,
        dest: u8,
    ) {
        match right.into() {
            Operand::Register(right) => {
                self.assembler.cmp::<32>(left, right);
                self.assembler
                    .csel::<64>(dest, src, dest, unsafe { transmute(cond) });
            }

            Operand::Imm32(imm) => {
                if imm == 0 {
                    if let Some(res) = Self::compute_compare_to_zero_test(cond) {
                        self.move_conditionally_test32(res, left, left, src, dest);
                    }
                }

                if let Some((u12, shift, inverted)) = Self::try_extract_shifted_imm(imm as _) {
                    if !inverted {
                        self.assembler.cmp_imm::<32>(left, u12, shift);
                    } else {
                        self.assembler.cmn_imm::<32>(left, u12, shift);
                    }
                } else {
                    let mut r = self.data_temp_register;
                    self.move_to_cached_reg32(imm, &mut r);
                    self.assembler.cmp::<32>(left, Self::DATA_TEMP_REGISTER);
                    self.data_temp_register = r;
                }

                self.assembler
                    .csel::<64>(dest, src, dest, unsafe { transmute(cond) });
            }

            _ => todo!(),
        }
    }

    pub fn move_conditionally32_then_else(
        &mut self,
        cond: RelationalCondition,
        left: u8,
        right: impl Into<Operand>,
        then_case: u8,
        else_case: u8,
        dest: u8,
    ) {
        match right.into() {
            Operand::Register(right) => {
                self.assembler.cmp::<32>(left, right);
                self.assembler
                    .csel::<64>(dest, then_case, else_case, unsafe { transmute(cond) });
            }

            Operand::Imm32(imm) => {
                if imm == 0 {
                    if let Some(res) = Self::compute_compare_to_zero_test(cond) {
                        self.move_conditionally_test32_then_else(
                            res, left, left, then_case, else_case, dest,
                        );
                    }
                }

                if let Some((u12, shift, inverted)) = Self::try_extract_shifted_imm(imm as _) {
                    if !inverted {
                        self.assembler.cmp_imm::<32>(left, u12, shift);
                    } else {
                        self.assembler.cmn_imm::<32>(left, u12, shift);
                    }
                } else {
                    let mut r = self.data_temp_register;
                    self.move_to_cached_reg32(imm, &mut r);
                    self.assembler.cmp::<32>(left, Self::DATA_TEMP_REGISTER);
                    self.data_temp_register = r;
                }

                self.assembler
                    .csel::<64>(dest, then_case, else_case, unsafe { transmute(cond) });
            }

            _ => todo!(),
        }
    }

    pub fn move_conditionally64(
        &mut self,
        cond: RelationalCondition,
        left: u8,
        right: u8,
        src: u8,
        dest: u8,
    ) {
        self.assembler.cmp::<64>(left, right);
        self.assembler
            .csel::<64>(dest, src, dest, unsafe { transmute(cond) });
    }

    pub fn move_conditionally64_then_else(
        &mut self,
        cond: RelationalCondition,
        left: u8,
        right: impl Into<Operand>,
        then_case: u8,
        else_case: u8,
        dest: u8,
    ) {
        match right.into() {
            Operand::Register(right) => {
                self.assembler.cmp::<64>(left, right);
                self.assembler
                    .csel::<64>(dest, then_case, else_case, unsafe { transmute(cond) });
            }

            Operand::Imm32(imm) => {
                if imm == 0 {
                    if let Some(res) = Self::compute_compare_to_zero_test(cond) {
                        self.move_conditionally_test64_then_else(
                            res, left, left, then_case, else_case, dest,
                        );
                        return;
                    }
                }

                if let Some((u12, shift, inverted)) = Self::try_extract_shifted_imm(imm as _) {
                    if !inverted {
                        self.assembler.cmp_imm::<64>(left, u12, shift);
                    } else {
                        self.assembler.cmn_imm::<64>(left, u12, shift);
                    }
                } else {
                    let mut r = self.data_temp_register;
                    self.move_to_cached_reg32(imm, &mut r);
                    self.assembler.cmp::<64>(left, Self::DATA_TEMP_REGISTER);
                    self.data_temp_register = r;
                }

                self.assembler
                    .csel::<64>(dest, then_case, else_case, unsafe { transmute(cond) });
            }

            _ => unreachable!(),
        }
    }

    pub fn move_conditionally_test32(
        &mut self,
        cond: ResultCondition,
        test_reg: u8,
        mask: impl Into<Operand>,
        src: u8,
        dest: u8,
    ) {
        match mask.into() {
            Operand::Register(reg) => {
                self.assembler.tst::<32>(test_reg, reg);
                self.assembler
                    .csel::<64>(dest, src, dest, unsafe { transmute(cond) });
            }

            Operand::Imm32(imm) => {
                self.test32(test_reg, imm);
                self.assembler
                    .csel::<64>(dest, src, dest, unsafe { transmute(cond) });
            }

            _ => unreachable!(),
        }
    }

    pub fn move_conditionally_test32_then_else(
        &mut self,
        cond: ResultCondition,
        test_reg: u8,
        mask: impl Into<Operand>,
        then_case: u8,
        else_case: u8,
        dest: u8,
    ) {
        match mask.into() {
            Operand::Register(reg) => {
                self.assembler.tst::<32>(test_reg, reg);
                self.assembler
                    .csel::<64>(dest, then_case, else_case, unsafe { transmute(cond) });
            }

            Operand::Imm32(imm) => {
                self.test32(test_reg, imm);
                self.assembler
                    .csel::<64>(dest, then_case, else_case, unsafe { transmute(cond) });
            }

            _ => unreachable!(),
        }
    }

    pub fn move_conditionally_test64(
        &mut self,
        cond: ResultCondition,
        test_reg: u8,
        mask: u8,
        src: u8,
        dest: u8,
    ) {
        self.assembler.tst::<64>(test_reg, mask);
        self.assembler
            .csel::<64>(dest, src, dest, unsafe { transmute(cond) });
    }

    pub fn move_conditionally_test64_then_else(
        &mut self,
        cond: ResultCondition,
        test_reg: u8,
        mask: u8,
        then_case: u8,
        else_case: u8,
        dest: u8,
    ) {
        self.assembler.tst::<64>(test_reg, mask);
        self.assembler
            .csel::<64>(dest, then_case, else_case, unsafe { transmute(cond) });
    }

    pub fn move_double_conditionally32(
        &mut self,
        cond: RelationalCondition,
        left: u8,
        right: impl Into<Operand>,
        then_case: u8,
        else_case: u8,
        dest: u8,
    ) {
        match right.into() {
            Operand::Register(right) => {
                self.assembler.cmp::<32>(left, right);
                self.assembler
                    .fcsel::<64>(dest, then_case, else_case, unsafe { transmute(cond) });
            }

            Operand::Imm32(imm) => {
                if imm == 0 {
                    if let Some(res) = Self::compute_compare_to_zero_test(cond) {
                        self.move_double_conditionally_test32(
                            res, left, left, then_case, else_case, dest,
                        );
                        return;
                    }
                }

                if let Some((u12, shift, inverted)) = Self::try_extract_shifted_imm(imm as _) {
                    if !inverted {
                        self.assembler.cmp_imm::<32>(left, u12, shift);
                    } else {
                        self.assembler.cmn_imm::<32>(left, u12, shift);
                    }
                } else {
                    let mut r = self.data_temp_register;
                    self.move_to_cached_reg32(imm, &mut r);
                    self.assembler.cmp::<32>(left, Self::DATA_TEMP_REGISTER);
                    self.data_temp_register = r;
                }

                self.assembler
                    .fcsel::<64>(dest, then_case, else_case, unsafe { transmute(cond) });
            }
            _ => unreachable!(),
        }
    }

    pub fn move_double_conditionally64(
        &mut self,
        cond: RelationalCondition,
        left: u8,
        right: impl Into<Operand>,
        then_case: u8,
        else_case: u8,
        dest: u8,
    ) {
        match right.into() {
            Operand::Register(right) => {
                self.assembler.cmp::<64>(left, right);
                self.assembler
                    .fcsel::<64>(dest, then_case, else_case, unsafe { transmute(cond) });
            }

            Operand::Imm32(imm) => {
                if imm == 0 {
                    if let Some(res) = Self::compute_compare_to_zero_test(cond) {
                        self.move_double_conditionally_test64(
                            res, left, left, then_case, else_case, dest,
                        );
                        return;
                    }
                }

                if let Some((u12, shift, inverted)) = Self::try_extract_shifted_imm(imm as _) {
                    if !inverted {
                        self.assembler.cmp_imm::<64>(left, u12, shift);
                    } else {
                        self.assembler.cmn_imm::<64>(left, u12, shift);
                    }
                } else {
                    let mut r = self.data_temp_register;
                    self.move_to_cached_reg32(imm, &mut r);
                    self.assembler.cmp::<64>(left, Self::DATA_TEMP_REGISTER);
                    self.data_temp_register = r;
                }

                self.assembler
                    .fcsel::<64>(dest, then_case, else_case, unsafe { transmute(cond) });
            }
            _ => unreachable!(),
        }
    }

    pub fn move_double_conditionally_test32(
        &mut self,
        cond: ResultCondition,
        left: u8,
        right: impl Into<Operand>,
        then_case: u8,
        else_case: u8,
        dest: u8,
    ) {
        match right.into() {
            Operand::Register(right) => {
                self.assembler.tst::<32>(left, right);
                self.assembler
                    .fcsel::<64>(dest, then_case, else_case, unsafe { transmute(cond) });
            }

            Operand::Imm32(imm) => {
                self.test32(left, imm);
                self.assembler
                    .fcsel::<64>(dest, then_case, else_case, unsafe { transmute(cond) });
            }
            _ => unreachable!(),
        }
    }

    pub fn move_double_conditionally_test64(
        &mut self,
        cond: ResultCondition,
        left: u8,
        right: u8,
        then_case: u8,
        else_case: u8,
        dest: u8,
    ) {
        self.assembler.tst::<64>(left, right);
        self.assembler
            .fcsel::<64>(dest, then_case, else_case, unsafe { transmute(cond) });
    }

    pub fn test32(&mut self, reg: u8, mask: i32) {
        if mask == -1 {
            self.assembler.tst::<32>(reg, reg);
        } else {
            let logical_imm = LogicalImmediate::create32(mask as _);
            if logical_imm.is_valid() {
                self.assembler.tst_imm::<32>(reg, logical_imm);
            } else {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.mov(mask, r);
                self.assembler.tst::<32>(reg, r);
            }
        }
    }

    pub fn test64_rrr(
        &mut self,
        cond: ResultCondition,
        reg: u8,
        mask: impl Into<Operand>,
        dest: u8,
    ) {
        match mask.into() {
            Operand::Register(mask) => {
                self.assembler.tst::<64>(reg, mask);
                self.assembler.cset::<32>(dest, unsafe { transmute(cond) });
            }

            Operand::Imm32(imm) => {
                if imm == -1 {
                    self.assembler.tst::<64>(reg, reg);
                } else {
                    let r = self.get_cached_data_temp_register_id_and_invalidate();
                    self.sign_extend32_to_64(imm, r);
                    self.assembler.tst::<64>(reg, r);
                }

                self.assembler.cset::<32>(dest, unsafe { transmute(cond) });
            }

            _ => unreachable!(),
        }
    }

    pub fn test32_rrr(
        &mut self,
        cond: ResultCondition,
        reg: u8,
        mask: impl Into<Operand>,
        dest: u8,
    ) {
        match mask.into() {
            Operand::Register(mask) => {
                self.assembler.tst::<32>(reg, mask);
                self.assembler.cset::<32>(dest, unsafe { transmute(cond) });
            }

            Operand::Imm32(imm) => {
                if imm == -1 {
                    self.assembler.tst::<32>(reg, reg);
                } else {
                    let r = self.get_cached_data_temp_register_id_and_invalidate();
                    self.mov(imm, r);
                    self.assembler.tst::<32>(reg, r);
                }

                self.assembler.cset::<32>(dest, unsafe { transmute(cond) });
            }

            _ => unreachable!(),
        }
    }

    // Forwards / external control flow operations:
    //
    // This set of jump and conditional branch operations return a Jump
    // object which may linked at a later point, allow forwards jump,
    // or jumps that will require external linkage (after the code has been
    // relocated).
    //
    // For branches, signed <, >, <= and >= are denoted as l, g, le, and ge
    // respecitvely, for unsigned comparisons the names b, a, be, and ae are
    // used (representing the names 'below' and 'above').
    //
    // Operands to the comparision are provided in the expected order, e.g.
    // jle32(reg1, TrustedImm32(5)) will branch if the value held in reg1, when
    // treated as a signed 32bit value, is less than or equal to 5.
    //
    // jz and jnz test whether the first operand is equal to zero, and take
    // an optional second operand of a mask under which to perform the test.

    pub fn branch(&mut self, res: ResultCondition) -> Jump {
        self.make_branch_res(res)
    }

    pub fn branch_test32(
        &mut self,
        cond: ResultCondition,
        src: impl Into<Operand>,
        mask: impl Into<Operand>,
    ) -> Jump {
        match (src.into(), mask.into()) {
            (Operand::Register(src), Operand::Imm32(mask)) => {
                if mask == -1 {
                    if matches!(cond, ResultCondition::Zero | ResultCondition::NonZero) {
                        return self.make_compare_and_branch::<32>(unsafe { transmute(cond) }, src);
                    }

                    self.assembler.tst::<32>(src, src);
                } else if has_one_bit_set(mask)
                    && matches!(cond, ResultCondition::Zero | ResultCondition::NonZero)
                {
                    return self.make_test_bit_and_branch(
                        src,
                        mask.trailing_zeros() as _,
                        unsafe { transmute(cond) },
                    );
                } else {
                    let logical_imm = LogicalImmediate::create32(mask as _);

                    if logical_imm.is_valid() {
                        self.assembler.tst_imm::<32>(src, logical_imm);
                        return self.make_branch_res(cond);
                    }

                    let r = self.get_cached_data_temp_register_id_and_invalidate();
                    self.mov(mask, r);
                    self.assembler.tst::<32>(src, r);
                }
                return self.make_branch_res(cond);
            }
            (Operand::Address(address), Operand::Imm32(mask)) => {
                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                self.load32(address, r);
                self.branch_test32(cond, r, mask)
            }

            (Operand::BaseIndex(address), Operand::Imm32(mask)) => {
                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                self.load32(address, r);
                self.branch_test32(cond, r, mask)
            }
            (Operand::AbsoluteAddress(address), Operand::Imm32(mask)) => {
                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                self.load32(address, r);
                self.branch_test32(cond, r, mask)
            }

            (Operand::Register(src), Operand::Register(mask)) => {
                if src == mask && matches!(cond, ResultCondition::Zero | ResultCondition::NonZero) {
                    return self.make_compare_and_branch::<32>(unsafe { transmute(cond) }, src);
                }
                self.assembler.tst::<32>(src, mask);
                return self.make_branch_res(cond);
            }
            _ => unreachable!(),
        }
    }

    pub fn branch_test64(
        &mut self,
        cond: ResultCondition,
        src: impl Into<Operand>,
        mask: impl Into<Operand>,
    ) -> Jump {
        match (src.into(), mask.into()) {
            (Operand::Register(src), Operand::Imm64(mask)) => {
                if mask == -1 {
                    if matches!(cond, ResultCondition::Zero | ResultCondition::NonZero) {
                        return self.make_compare_and_branch::<64>(unsafe { transmute(cond) }, src);
                    }

                    self.assembler.tst::<64>(src, src);
                } else if has_one_bit_set(mask)
                    && matches!(cond, ResultCondition::Zero | ResultCondition::NonZero)
                {
                    return self.make_test_bit_and_branch(
                        src,
                        mask.trailing_zeros() as _,
                        unsafe { transmute(cond) },
                    );
                } else {
                    let logical_imm = LogicalImmediate::create64(mask as _);

                    if logical_imm.is_valid() {
                        self.assembler.tst_imm::<64>(src, logical_imm);
                        return self.make_branch_res(cond);
                    }

                    let r = self.get_cached_data_temp_register_id_and_invalidate();
                    self.mov(mask, r);
                    self.assembler.tst::<64>(src, r);
                }
                return self.make_branch_res(cond);
            }
            (Operand::Register(src), Operand::Imm32(mask)) => {
                if mask == -1 {
                    if matches!(cond, ResultCondition::Zero | ResultCondition::NonZero) {
                        return self.make_compare_and_branch::<64>(unsafe { transmute(cond) }, src);
                    }

                    self.assembler.tst::<64>(src, src);
                } else if has_one_bit_set(mask)
                    && matches!(cond, ResultCondition::Zero | ResultCondition::NonZero)
                {
                    return self.make_test_bit_and_branch(
                        src,
                        mask.trailing_zeros() as _,
                        unsafe { transmute(cond) },
                    );
                } else {
                    let logical_imm = LogicalImmediate::create64(mask as _);

                    if logical_imm.is_valid() {
                        self.assembler.tst_imm::<64>(src, logical_imm);
                        return self.make_branch_res(cond);
                    }

                    let r = self.get_cached_data_temp_register_id_and_invalidate();
                    self.sign_extend32_to_64(mask, r);
                    self.assembler.tst::<64>(src, r);
                }
                return self.make_branch_res(cond);
            }
            (Operand::Address(address), Operand::Imm64(mask)) => {
                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                self.load64(address, r);
                self.branch_test64(cond, r, mask)
            }

            (Operand::BaseIndex(address), Operand::Imm64(mask)) => {
                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                self.load64(address, r);
                self.branch_test64(cond, r, mask)
            }
            (Operand::AbsoluteAddress(address), Operand::Imm64(mask)) => {
                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                self.load64(address, r);
                self.branch_test64(cond, r, mask)
            }

            (Operand::Register(src), Operand::Register(mask)) => {
                if src == mask && matches!(cond, ResultCondition::Zero | ResultCondition::NonZero) {
                    return self.make_compare_and_branch::<64>(unsafe { transmute(cond) }, src);
                }
                self.assembler.tst::<64>(src, mask);
                return self.make_branch_res(cond);
            }

            _ => unreachable!(),
        }
    }

    pub fn branch32(
        &mut self,
        cond: RelationalCondition,
        left: impl Into<Operand>,
        right: impl Into<Operand>,
    ) -> Jump {
        match (left.into(), right.into()) {
            (Operand::Register(left), Operand::Register(right)) => {
                self.assembler.cmp::<32>(left, right);
                self.make_branch_rel(cond)
            }
            (Operand::Register(left), Operand::Imm32(imm)) => {
                if imm == 0 {
                    if let Some(res) = Self::compute_compare_to_zero_test(cond) {
                        return self.branch_test32(res, left, left);
                    }
                }

                if let Some((u12, shift, inverted)) = Self::try_extract_shifted_imm(imm as _) {
                    if inverted {
                        self.assembler.cmn_imm::<32>(left, u12, shift);
                    } else {
                        self.assembler.cmp_imm::<32>(left, u12, shift);
                    }
                } else {
                    let mut r = self.data_temp_register;
                    self.move_to_cached_reg32(imm, &mut r);
                    self.assembler.cmp::<32>(left, Self::DATA_TEMP_REGISTER);
                    self.data_temp_register = r;
                }

                self.make_branch_rel(cond)
            }

            (Operand::Address(left), Operand::Register(right)) => {
                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                self.load32(left, r);
                self.branch32(cond, r, right)
            }

            (Operand::BaseIndex(left), Operand::Register(right)) => {
                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                self.load32(left, r);
                self.branch32(cond, r, right)
            }
            (Operand::AbsoluteAddress(left), Operand::Register(right)) => {
                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                self.load32(left, r);
                self.branch32(cond, r, right)
            }
            (Operand::AbsoluteAddress(left), Operand::Imm32(imm)) => {
                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                self.load32(left, r);
                self.branch32(cond, r, imm)
            }
            _ => unreachable!(),
        }
    }

    pub fn branch64(
        &mut self,
        cond: RelationalCondition,
        left: impl Into<Operand>,
        right: impl Into<Operand>,
    ) -> Jump {
        match (left.into(), right.into()) {
            (Operand::Register(left), Operand::Register(right)) => {
                self.assembler.cmp::<64>(left, right);
                self.make_branch_rel(cond)
            }
            (Operand::Register(left), Operand::Imm32(imm)) => {
                if imm == 0 {
                    if let Some(res) = Self::compute_compare_to_zero_test(cond) {
                        return self.branch_test64(res, left, left);
                    }
                }

                if let Some((u12, shift, inverted)) = Self::try_extract_shifted_imm(imm as _) {
                    if inverted {
                        self.assembler.cmn_imm::<64>(left, u12, shift);
                    } else {
                        self.assembler.cmp_imm::<64>(left, u12, shift);
                    }
                } else {
                    let mut r = self.data_temp_register;
                    self.move_to_cached_reg32(imm as _, &mut r);
                    self.assembler.cmp::<64>(left, Self::DATA_TEMP_REGISTER);
                    self.data_temp_register = r;
                }

                self.make_branch_rel(cond)
            }

            (Operand::Register(left), Operand::Imm64(imm)) => {
                if imm == 0 {
                    if let Some(res) = Self::compute_compare_to_zero_test(cond) {
                        return self.branch_test64(res, left, left);
                    }
                }

                if let Some((u12, shift, inverted)) = Self::try_extract_shifted_imm(imm as _) {
                    if inverted {
                        self.assembler.cmn_imm::<64>(left, u12, shift);
                    } else {
                        self.assembler.cmp_imm::<64>(left, u12, shift);
                    }
                } else {
                    let mut r = self.data_temp_register;
                    self.move_to_cached_reg64(imm, &mut r);
                    self.assembler.cmp::<64>(left, Self::DATA_TEMP_REGISTER);
                    self.data_temp_register = r;
                }

                self.make_branch_rel(cond)
            }

            (Operand::Register(left), Operand::Address(right)) => {
                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                self.load64(right, r);
                self.branch64(cond, left, r)
            }

            (Operand::AbsoluteAddress(left), Operand::Register(right)) => {
                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                self.load64(left, r);
                self.branch64(cond, r, right)
            }

            (Operand::Address(left), Operand::Register(right)) => {
                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                self.load64(left, r);
                self.branch64(cond, r, right)
            }

            (Operand::Address(left), Operand::Imm64(imm)) => {
                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                self.load64(left, r);
                self.branch64(cond, r, imm)
            }
            (Operand::BaseIndex(left), Operand::Register(right)) => {
                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                self.load64(left, r);
                self.branch64(cond, r, right)
            }
            (Operand::Address(left), Operand::Address(right)) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load64(left, r);
                self.branch64(cond, r, right)
            }
            _ => unreachable!(),
        }
    }

    pub fn branch_test8(
        &mut self,
        cond: ResultCondition,
        address: impl Into<Operand>,
        mask: i32,
    ) -> Jump {
        match address.into() {
            Operand::Address(address) => {
                let mask8 = mask8_on_condition_res(cond, mask);
                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                if is_unsigned_res(cond) {
                    self.load8(address, r);
                } else {
                    self.load8_signed_extend_to_32(address, r);
                }

                self.branch_test32(cond, Self::MEMORY_TEMP_REGISTER, mask8)
            }

            Operand::BaseIndex(address) => {
                let mask8 = mask8_on_condition_res(cond, mask);
                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                if is_unsigned_res(cond) {
                    self.load8(address, r);
                } else {
                    self.load8_signed_extend_to_32(address, r);
                }

                self.branch_test32(cond, Self::MEMORY_TEMP_REGISTER, mask8)
            }

            Operand::AbsoluteAddress(address) => {
                let mask8 = mask8_on_condition_res(cond, mask);
                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                if is_unsigned_res(cond) {
                    self.load8(address, r);
                } else {
                    self.load8_signed_extend_to_32(address, r);
                }

                self.branch_test32(cond, Self::MEMORY_TEMP_REGISTER, mask8)
            }

            _ => unreachable!(),
        }
    }

    pub fn branch_test16(
        &mut self,
        cond: ResultCondition,
        address: impl Into<Operand>,
        mask: i32,
    ) -> Jump {
        match address.into() {
            Operand::Address(address) => {
                let mask8 = mask16_on_condition_res(cond, mask);
                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                if is_unsigned_res(cond) {
                    self.load16(address, r);
                } else {
                    self.load16_signed_extend_to_32(address, r);
                }

                self.branch_test32(cond, Self::MEMORY_TEMP_REGISTER, mask8)
            }

            Operand::BaseIndex(address) => {
                let mask8 = mask16_on_condition_res(cond, mask);
                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                if is_unsigned_res(cond) {
                    self.load16(address, r);
                } else {
                    self.load16_signed_extend_to_32(address, r);
                }

                self.branch_test32(cond, Self::MEMORY_TEMP_REGISTER, mask8)
            }

            Operand::AbsoluteAddress(address) => {
                let mask8 = mask16_on_condition_res(cond, mask);
                let r = self.get_cached_memory_temp_register_id_and_invalidate();
                if is_unsigned_res(cond) {
                    self.load16(address, r);
                } else {
                    self.load16_signed_extend_to_32(address, r);
                }

                self.branch_test32(cond, Self::MEMORY_TEMP_REGISTER, mask8)
            }

            _ => unreachable!(),
        }
    }

    pub fn branch_add32_rrr(
        &mut self,
        cond: ResultCondition,
        op1: u8,
        op2: impl Into<Operand>,
        dest: u8,
    ) -> Jump {
        match op2.into() {
            Operand::Register(op2) => {
                self.assembler.add::<32, true>(dest, op1, op2);
                self.make_branch_res(cond)
            }

            Operand::Imm32(imm) => {
                if let Some((u12, shift, inverted)) = Self::try_extract_shifted_imm(imm as _) {
                    if !inverted {
                        self.assembler.add_imm::<32, true>(dest, op1, u12, shift);
                    } else {
                        self.assembler.sub_imm::<32, true>(dest, op1, u12, shift);
                    }

                    self.make_branch_res(cond)
                } else {
                    let r = self.get_cached_data_temp_register_id_and_invalidate();
                    self.sign_extend32_to_64(imm, r);
                    self.branch_add32_rrr(cond, op1, r, dest)
                }
            }

            _ => unreachable!(),
        }
    }

    pub fn branch_add32(
        &mut self,
        cond: ResultCondition,
        src: impl Into<Operand>,
        dest: impl Into<Operand>,
    ) -> Jump {
        match (src.into(), dest.into()) {
            (Operand::Register(src), Operand::Register(dest)) => {
                self.branch_add32_rrr(cond, dest, src, dest)
            }

            (Operand::Address(src), Operand::Register(dest)) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load32(src, r);
                self.branch_add32_rrr(cond, dest, r, dest)
            }

            (Operand::Imm32(imm), Operand::Register(dest)) => {
                self.branch_add32_rrr(cond, dest, imm, dest)
            }

            (Operand::Imm32(imm), Operand::AbsoluteAddress(dest)) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load32(dest, r);

                if let Some((u12, shift, inverted)) = Self::try_extract_shifted_imm(imm as _) {
                    if !inverted {
                        self.assembler.add_imm::<32, true>(r, r, u12, shift);
                    } else {
                        self.assembler.sub_imm::<32, true>(r, r, u12, shift);
                    }
                } else {
                    let r = self.get_cached_memory_temp_register_id_and_invalidate();
                    self.mov(imm, r);
                    self.assembler
                        .add::<32, true>(r, r, Self::MEMORY_TEMP_REGISTER);
                }

                self.store32(r, dest);
                self.make_branch_res(cond)
            }

            (Operand::Imm32(imm), Operand::Address(dest)) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load32(dest, r);

                if let Some((u12, shift, inverted)) = Self::try_extract_shifted_imm(imm as _) {
                    if !inverted {
                        self.assembler.add_imm::<32, true>(r, r, u12, shift);
                    } else {
                        self.assembler.sub_imm::<32, true>(r, r, u12, shift);
                    }
                } else {
                    let r = self.get_cached_memory_temp_register_id_and_invalidate();
                    self.mov(imm, r);
                    self.assembler
                        .add::<32, true>(r, r, Self::MEMORY_TEMP_REGISTER);
                }

                self.store32(r, dest);
                self.make_branch_res(cond)
            }

            _ => unreachable!(),
        }
    }

    pub fn branch_add64_rrr(
        &mut self,
        cond: ResultCondition,
        op1: u8,
        op2: impl Into<Operand>,
        dest: u8,
    ) -> Jump {
        match op2.into() {
            Operand::Register(op2) => {
                self.assembler.add::<64, true>(dest, op1, op2);
                self.make_branch_res(cond)
            }

            Operand::Imm32(imm) => {
                if let Some((u12, shift, inverted)) = Self::try_extract_shifted_imm(imm as _) {
                    if !inverted {
                        self.assembler.add_imm::<64, true>(dest, op1, u12, shift);
                    } else {
                        self.assembler.sub_imm::<64, true>(dest, op1, u12, shift);
                    }

                    self.make_branch_res(cond)
                } else {
                    let r = self.get_cached_data_temp_register_id_and_invalidate();
                    self.sign_extend32_to_64(imm, r);
                    self.branch_add64_rrr(cond, op1, r, dest)
                }
            }

            _ => unreachable!(),
        }
    }

    pub fn branch_add64(
        &mut self,
        cond: ResultCondition,
        src: impl Into<Operand>,
        dest: impl Into<Operand>,
    ) -> Jump {
        match (src.into(), dest.into()) {
            (Operand::Register(src), Operand::Register(dest)) => {
                self.branch_add64_rrr(cond, dest, src, dest)
            }

            (Operand::Address(src), Operand::Register(dest)) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load64(src, r);
                self.branch_add64_rrr(cond, dest, r, dest)
            }

            (Operand::Imm32(imm), Operand::Register(dest)) => {
                self.branch_add64_rrr(cond, dest, imm, dest)
            }

            (Operand::Imm32(imm), Operand::AbsoluteAddress(dest)) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load64(dest, r);

                if let Some((u12, shift, inverted)) = Self::try_extract_shifted_imm(imm as _) {
                    if !inverted {
                        self.assembler.add_imm::<64, true>(r, r, u12, shift);
                    } else {
                        self.assembler.sub_imm::<64, true>(r, r, u12, shift);
                    }
                } else {
                    let r = self.get_cached_memory_temp_register_id_and_invalidate();
                    self.mov(imm, r);
                    self.assembler
                        .add::<64, true>(r, r, Self::MEMORY_TEMP_REGISTER);
                }

                self.store64(r, dest);
                self.make_branch_res(cond)
            }

            (Operand::Imm32(imm), Operand::Address(dest)) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load64(dest, r);

                if let Some((u12, shift, inverted)) = Self::try_extract_shifted_imm(imm as _) {
                    if !inverted {
                        self.assembler.add_imm::<64, true>(r, r, u12, shift);
                    } else {
                        self.assembler.sub_imm::<64, true>(r, r, u12, shift);
                    }
                } else {
                    let r = self.get_cached_memory_temp_register_id_and_invalidate();
                    self.mov(imm, r);
                    self.assembler
                        .add::<64, true>(r, r, Self::MEMORY_TEMP_REGISTER);
                }

                self.store64(r, dest);
                self.make_branch_res(cond)
            }

            _ => unreachable!(),
        }
    }

    pub fn branch_sub32_rrr(
        &mut self,
        cond: ResultCondition,
        op1: u8,
        op2: impl Into<Operand>,
        dest: u8,
    ) -> Jump {
        match op2.into() {
            Operand::Register(op2) => {
                self.assembler.sub::<32, true>(dest, op1, op2);
                self.make_branch_res(cond)
            }

            Operand::Imm32(imm) => {
                if let Some((u12, shift, inverted)) = Self::try_extract_shifted_imm(imm as _) {
                    if !inverted {
                        self.assembler.sub_imm::<32, true>(dest, op1, u12, shift);
                    } else {
                        self.assembler.add_imm::<32, true>(dest, op1, u12, shift);
                    }

                    self.make_branch_res(cond)
                } else {
                    let r = self.get_cached_data_temp_register_id_and_invalidate();
                    self.sign_extend32_to_64(imm, r);
                    self.branch_sub32_rrr(cond, op1, r, dest)
                }
            }

            _ => unreachable!(),
        }
    }

    pub fn branch_sub32(
        &mut self,
        cond: ResultCondition,
        src: impl Into<Operand>,
        dest: impl Into<Operand>,
    ) -> Jump {
        match (src.into(), dest.into()) {
            (Operand::Register(src), Operand::Register(dest)) => {
                self.branch_sub32_rrr(cond, dest, src, dest)
            }

            (Operand::Address(src), Operand::Register(dest)) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load64(src, r);
                self.branch_sub32_rrr(cond, dest, r, dest)
            }

            (Operand::Imm32(imm), Operand::Register(dest)) => {
                self.branch_sub32_rrr(cond, dest, imm, dest)
            }

            (Operand::Imm32(imm), Operand::AbsoluteAddress(dest)) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load32(dest, r);

                if let Some((u12, shift, inverted)) = Self::try_extract_shifted_imm(imm as _) {
                    if !inverted {
                        self.assembler.sub_imm::<32, true>(r, r, u12, shift);
                    } else {
                        self.assembler.add_imm::<32, true>(r, r, u12, shift);
                    }
                } else {
                    let r = self.get_cached_memory_temp_register_id_and_invalidate();
                    self.mov(imm, r);
                    self.assembler
                        .add::<32, true>(r, r, Self::MEMORY_TEMP_REGISTER);
                }

                self.store32(r, dest);
                self.make_branch_res(cond)
            }

            (Operand::Imm32(imm), Operand::Address(dest)) => {
                let r = self.get_cached_data_temp_register_id_and_invalidate();
                self.load32(dest, r);

                if let Some((u12, shift, inverted)) = Self::try_extract_shifted_imm(imm as _) {
                    if !inverted {
                        self.assembler.sub_imm::<32, true>(r, r, u12, shift);
                    } else {
                        self.assembler.add_imm::<32, true>(r, r, u12, shift);
                    }
                } else {
                    let r = self.get_cached_memory_temp_register_id_and_invalidate();
                    self.mov(imm, r);
                    self.assembler
                        .add::<32, true>(r, r, Self::MEMORY_TEMP_REGISTER);
                }

                self.store32(r, dest);
                self.make_branch_res(cond)
            }

            _ => unreachable!(),
        }
    }

    pub fn jump(&mut self) -> Jump {
        let label = self.assembler.label();
        self.assembler.b();
        let mut j = Jump::new(label);
        j.typ = JumpType::NoConditionFixedSize;
        j
    }

    pub fn ret(&mut self) {
        self.assembler.ret(lr);
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

    fn jump_after_floating_point_compare(&mut self, cond: DoubleCondition) -> Jump {
        if cond == DoubleCondition::NotEqualAndOrdered {
            let unordered = self.make_branch(Condition::VS);
            let result = self.make_branch(Condition::NE);
            unordered.link(self);
            result
        } else if cond == DoubleCondition::EqualOrUnordered {
            let unordered = self.make_branch(Condition::VS);
            let not_equal = self.make_branch(Condition::NE);
            unordered.link(self);
            let result = self.jump();
            not_equal.link(self);
            result
        } else {
            self.make_branch(unsafe { transmute(cond) })
        }
    }

    fn make_branch_rel(&mut self, rel: RelationalCondition) -> Jump {
        self.make_branch(unsafe { transmute(rel) })
    }

    fn make_branch_res(&mut self, cond: ResultCondition) -> Jump {
        self.make_branch(unsafe { transmute(cond) })
    }

    pub fn make_branch_double(&mut self, cond: DoubleCondition) -> Jump {
        self.make_branch(unsafe { transmute(cond) })
    }

    fn make_compare_and_branch<const DATASIZE: i32>(
        &mut self,
        cond: ZeroCondition,
        reg: u8,
    ) -> Jump {
        self.pad_before_patch();
        if cond == ZeroCondition::IsZero {
            self.assembler.cbz::<DATASIZE>(reg, 0);
        } else {
            self.assembler.cbnz::<DATASIZE>(reg, 0);
        }

        let label = self.assembler.label_ignoring_watchpoints();
        self.assembler.nop();
        let mut j = Jump::new(label);
        j.typ = JumpType::CompareAndBranchFixedSize;
        j.condition = unsafe { transmute(cond) };
        j
    }

    fn make_test_bit_and_branch(&mut self, reg: u8, mut bit: usize, cond: ZeroCondition) -> Jump {
        self.pad_before_patch();
        bit &= 0x3F;
        if cond == ZeroCondition::IsZero {
            self.assembler.tbz(reg, bit as _, 0)
        } else {
            self.assembler.tbnz(reg, bit as _, 0);
        }

        let label = self.assembler.label_ignoring_watchpoints();
        self.assembler.nop();
        let mut j = Jump::new(label);
        j.typ = JumpType::TestBitFixedSize;
        j.condition = unsafe { transmute(cond) };
        j
    }

    fn make_branch(&mut self, cond: Condition) -> Jump {
        self.pad_before_patch();
        self.assembler.b_cond(cond, 0);
        let label = self.assembler.label_ignoring_watchpoints();
        self.assembler.nop();

        let mut j = Jump::new(label);
        j.typ = JumpType::ConditionFixedSize;
        j.condition = cond;
        j
    }

    fn floating_point_compare(
        &mut self,
        cond: DoubleCondition,
        dest: u8,
        compare: impl FnOnce(&mut Self),
    ) {
        if cond == DoubleCondition::NotEqualAndOrdered {
            self.mov(0i32, dest);
            compare(self);
            let unordered = self.make_branch(Condition::VS);
            self.assembler.cset::<32>(dest, Condition::EQ);
            unordered.link(self);
            return;
        }
        if cond == DoubleCondition::EqualOrUnordered {
            self.mov(1i32, dest);
            compare(self);
            let unordered = self.make_branch(Condition::VS);
            self.assembler.cset::<32>(dest, Condition::EQ);
            unordered.link(self);
            return;
        }

        compare(self);
        self.assembler.cset::<32>(dest, unsafe { transmute(cond) });
    }

    pub fn compute_compare_to_zero_test(cond: RelationalCondition) -> Option<ResultCondition> {
        match cond {
            RelationalCondition::Equal => Some(ResultCondition::Zero),
            RelationalCondition::NotEqual => Some(ResultCondition::NonZero),
            RelationalCondition::LessThan => Some(ResultCondition::Signed),
            RelationalCondition::GreaterThanOrEqual => Some(ResultCondition::PositiveOrZero),
            _ => None,
        }
    }
}
