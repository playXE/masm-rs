use std::mem::size_of;
use std::ops::{Deref, DerefMut, Not};

use crate::assembler::abstract_macro_assembler::AbsoluteAddress;
use crate::assembler::assembler_common::ARM64LogicalImmediate;

use super::abstract_macro_assembler::{
    AbstractMacroAssembler, Address, BaseIndex, CachedTempRegister, Call, DataLabelCompact, Extend,
    Jump, JumpList, Operand, Scale,
};
use super::arm64assembler::*;
use super::assembler_common::{is_int, is_uint12};
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

            _ => unreachable!()
        }
    }

    pub fn add64(&mut self, src: impl Into<Operand>, dest: impl Into<Operand>) {
        match (src.into(), dest.into()) {
            _ => todo!()
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
