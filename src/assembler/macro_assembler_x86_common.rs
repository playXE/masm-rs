use std::ops::{Deref, DerefMut};

use crate::assembler::abstract_macro_assembler::AbsoluteAddress;

use super::abstract_macro_assembler::{
    AbstractMacroAssembler, Address, BaseIndex, Call, DataLabelCompact, Extend, Jump, Operand,
    Scale,
};
use super::buffer::AssemblerLabel;
use super::x86assembler::*;

pub struct MacroAssemblerX86Common(AbstractMacroAssembler);

impl Deref for MacroAssemblerX86Common {
    type Target = AbstractMacroAssembler;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for MacroAssemblerX86Common {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum RelationalCondition {
    Equal = Condition::E as u8,
    NotEqual = Condition::NE as u8,
    Above = Condition::A as u8,
    AboveOrEqual = Condition::AE as u8,
    Below = Condition::B as u8,
    BelowOrEqual = Condition::BE as u8,
    GreaterThan = Condition::G as u8,
    GreaterThanOrEqual = Condition::GE as u8,
    LessThan = Condition::L as u8,
    LessThanOrEqual = Condition::LE as u8,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum ResultCondition {
    Overflow = Condition::O as u8,
    Signed = Condition::S as u8,
    PositiveOrZero = Condition::NS as u8,
    Zero = Condition::E as u8,
    NotZero = Condition::NE as u8,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum StatusCondition {
    Success,
    Failure,
}

pub const DOUBLE_CONDITION_BIT_INVERT: u8 = 0x10;
pub const DOUBLE_CONDITION_BIT_SPECIAL: u8 = 0x20;
pub const DOUBLE_CONDITION_BITS: u8 = DOUBLE_CONDITION_BIT_INVERT | DOUBLE_CONDITION_BIT_SPECIAL;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum DoubleCondition {
    EqualAndOrdered = Condition::E as u8 | DOUBLE_CONDITION_BIT_SPECIAL,
    NotEqualAndOrdered = Condition::NE as u8,
    GreaterThanAndOrdered = Condition::A as u8,
    GreaterThanOrEqualAndOrdered = Condition::AE as u8,
    LessThanAndOrdered = Condition::A as u8 | DOUBLE_CONDITION_BIT_INVERT,
    LessThanOrEqualAndOrdered = Condition::AE as u8 | DOUBLE_CONDITION_BIT_INVERT,

    EqualOrUnordered = Condition::E as u8,
    NotEqualOrUnordered = Condition::NE as u8 | DOUBLE_CONDITION_BIT_SPECIAL,
    GreaterThanOrUnordered = Condition::B as u8 | DOUBLE_CONDITION_BIT_INVERT,
    GreaterThanOrEqualOrUnordered = Condition::BE as u8 | DOUBLE_CONDITION_BIT_INVERT,
    LessThanOrUnordered = Condition::B as u8,
    LessThanOrEqualOrUnordered = Condition::BE as u8,
}

impl MacroAssemblerX86Common {
    pub const STACK_POINTER_REGISTER: u8 = esp;
    pub const FRAME_POINTER_REGISTER: u8 = ebp;

    pub unsafe fn link_call(code: *mut u8, call: Call, function: *const u8) {
        if !call.is_flag_set(Call::NEAR) {
            X86Assembler::link_pointer(code, call.label.label_at_offset(-3), function as _);
        } else if call.is_flag_set(Call::TAIL) {
            X86Assembler::link_jump_(code, call.label, function as _);
        } else {
            X86Assembler::link_call(code, call.label, function as _);
        }
    }

    pub unsafe fn link_jump(code: *mut u8, jump: Jump, target: *const u8) {
        X86Assembler::link_jump_(code, jump.label, target as _);
    }

    pub unsafe fn link_pointer(code: *mut u8, label: AssemblerLabel, target: *const u8) {
        X86Assembler::link_pointer(code, label, target as _);
    }

    pub const fn can_blind() -> bool {
        true
    }

    pub const fn should_blind_for_specific_arch(value: u32) -> bool {
        value >= 0x00ffffff
    }

    pub const fn should_blind_for_specific_arch64(value: u64) -> bool {
        value >= 0x00ffffff
    }

    pub fn new() -> Self {
        Self(AbstractMacroAssembler::new())
    }

    pub const SCRATCH_REGISTER: u8 = r11;

    pub fn scratch_register(&self) -> u8 {
        Self::SCRATCH_REGISTER
    }

    pub fn ret(&mut self) {
        self.assembler.ret();
    }

    // Integer arithmetic operations:
    //
    // Operations are typically two operand - operation(source, srcDst)
    // For many operations the source may be an i32, the srcDst operand
    // may often be a memory location (explictly described using an Address
    // object).

    pub fn add32(&mut self, src: impl Into<Operand>, dst: impl Into<Operand>) {
        let (src, dst) = (src.into(), dst.into());

        match (src, dst) {
            (Operand::Register(src), Operand::Register(dst)) => {
                self.assembler.addl_rr(src, dst);
            }

            (Operand::Imm32(imm), Operand::Register(dst)) => {
                if imm == 1 {
                    self.assembler.inc_r(dst);
                } else {
                    self.assembler.addl_ir(imm, dst);
                }
            }

            (Operand::Imm32(imm), Operand::Address(dst)) => {
                self.assembler.addl_im(imm, dst.offset, dst.base);
            }

            (Operand::Imm32(imm), Operand::BaseIndex(dst)) => {
                self.assembler
                    .addl_im_scaled(imm, dst.offset, dst.base, dst.index, dst.scale as _)
            }

            (Operand::Register(src), Operand::Address(dst)) => {
                self.assembler.addl_rm(src, dst.offset, dst.base);
            }

            (Operand::Address(src), Operand::Register(dst)) => {
                self.assembler.addl_mr(src.offset, src.base, dst);
            }

            (Operand::Imm32(imm), Operand::AbsoluteAddress(address)) => {
                self.mov(
                    Operand::Imm64(address.ptr as _),
                    Operand::Register(Self::SCRATCH_REGISTER),
                );
                self.add32(
                    Operand::Imm32(imm),
                    Operand::Address(Address::new(Self::SCRATCH_REGISTER, 0)),
                );
            }

            (Operand::AbsoluteAddress(address), Operand::Register(dst)) => {
                self.mov(
                    Operand::Imm64(address.ptr as _),
                    Operand::Register(Self::SCRATCH_REGISTER),
                );
                self.add32(
                    Operand::Address(Address::new(Self::SCRATCH_REGISTER, 0)),
                    Operand::Register(dst),
                );
            }

            _ => unreachable!("Invalid operands for add32: {:?}, {:?}", src, dst),
        }
    }

    pub fn add8(&mut self, src: impl Into<Operand>, dst: impl Into<Operand>) {
        let (src, dst) = (src.into(), dst.into());

        match (src, dst) {
            (Operand::Imm32(imm), Operand::Address(dst)) => {
                self.assembler.addb_im(imm as _, dst.offset, dst.base);
            }

            (Operand::Imm32(imm), Operand::BaseIndex(dst)) => self.assembler.addb_im_scaled(
                imm as _,
                dst.offset,
                dst.base,
                dst.index,
                dst.scale as _,
            ),

            (Operand::Register(src), Operand::Address(dst)) => {
                self.assembler.addb_rm(src, dst.offset, dst.base);
            }

            (Operand::Register(src), Operand::BaseIndex(dst)) => {
                self.assembler
                    .addb_rm_scaled(src, dst.offset, dst.base, dst.index, dst.scale as _)
            }

            _ => unreachable!("Invalid operands for add8: {:?}, {:?}", src, dst),
        }
    }

    pub fn add16(&mut self, src: impl Into<Operand>, dst: impl Into<Operand>) {
        let (src, dst) = (src.into(), dst.into());

        match (src, dst) {
            (Operand::Imm32(imm), Operand::Address(dst)) => {
                self.assembler.addw_im(imm as _, dst.offset, dst.base);
            }

            (Operand::Imm32(imm), Operand::BaseIndex(dst)) => self.assembler.addw_im_scaled(
                imm as _,
                dst.offset,
                dst.base,
                dst.index,
                dst.scale as _,
            ),

            (Operand::Register(src), Operand::Address(dst)) => {
                self.assembler.addw_rm(src, dst.offset, dst.base);
            }

            (Operand::Register(src), Operand::BaseIndex(dst)) => {
                self.assembler
                    .addw_rm_scaled(src, dst.offset, dst.base, dst.index, dst.scale as _)
            }

            _ => unreachable!("Invalid operands for add16: {:?}, {:?}", src, dst),
        }
    }

    pub fn add32_rrr(
        &mut self,
        a: impl Into<Operand>,
        b: impl Into<Operand>,
        dest: impl Into<Operand>,
    ) {
        let (a, b, dest) = (a.into(), b.into(), dest.into());

        match (a, b, dest) {
            (Operand::Register(a), Operand::Register(b), Operand::Register(dest)) => {
                self.x86_lea32(BaseIndex::new(a, b, Scale::TimesOne, 0, Extend::None), dest);
            }
            (Operand::Imm32(imm), Operand::Register(b), Operand::Register(dest)) => {
                if imm == 0 {
                    self.zero_extend32_to_word(b, dest);
                } else if b == dest {
                    self.add32(imm, dest);
                } else {
                    self.assembler.leal_mr(imm, b, dest);
                }
            }
            _ => unreachable!(
                "Invalid operands for add32_rrr: {:?}, {:?}, {:?}",
                a, b, dest
            ),
        }
    }

    pub fn x86_lea32(&mut self, index: BaseIndex, dest: u8) {
        if index.scale == Scale::TimesOne && index.offset == 0 {
            if index.base == dest {
                self.add32(index.index, dest);
                return;
            }

            if index.index == dest {
                self.add32(index.base, dest);
                return;
            }
        }

        self.assembler.leal_mr_scaled(
            index.offset,
            index.base,
            index.index,
            index.scale as _,
            dest,
        );
    }

    pub fn and32(&mut self, src: impl Into<Operand>, dest: impl Into<Operand>) {
        let src = src.into();
        let dest = dest.into();
        match (src, dest) {
            (Operand::Imm32(imm), Operand::Register(dst)) => {
                self.assembler.andl_ir(imm, dst);
            }

            (Operand::Imm32(imm), Operand::Address(dst)) => {
                self.assembler.andl_im(imm, dst.offset, dst.base);
            }

            (Operand::Imm32(imm), Operand::BaseIndex(dst)) => {
                self.assembler
                    .andl_im_scaled(imm, dst.offset, dst.base, dst.index, dst.scale as _);
            }

            (Operand::Register(src), Operand::Register(dst)) => {
                self.assembler.andl_rr(src, dst);
            }

            (Operand::Register(src), Operand::Address(dst)) => {
                self.assembler.andl_rm(src, dst.offset, dst.base);
            }

            (Operand::Register(src), Operand::BaseIndex(dst)) => {
                self.assembler
                    .andl_rm_scaled(src, dst.offset, dst.base, dst.index, dst.scale as _);
            }

            (Operand::Address(src), Operand::Register(dst)) => {
                self.assembler.andl_mr(src.offset, src.base, dst);
            }

            (Operand::BaseIndex(src), Operand::Register(dst)) => {
                self.assembler
                    .andl_mr_scaled(src.offset, src.base, src.index, src.scale as _, dst);
            }

            (Operand::Imm32(imm), Operand::AbsoluteAddress(addr)) => {
                self.mov(addr, Self::SCRATCH_REGISTER);
                self.and32(imm, Address::new(Self::SCRATCH_REGISTER, 0));
            }

            _ => unreachable!("Invalid operands for and32: {:?}, {:?}", src, dest),
        }
    }

    pub fn and16(&mut self, src: impl Into<Operand>, dest: impl Into<Operand>) {
        let src = src.into();
        let dest = dest.into();
        match (src, dest) {
            (Operand::Imm32(imm), Operand::Address(dst)) => {
                self.assembler.andw_im(imm as _, dst.offset, dst.base);
            }

            (Operand::Register(src), Operand::Address(dst)) => {
                self.assembler.andw_rm(src, dst.offset, dst.base);
            }

            (Operand::Register(src), Operand::BaseIndex(dst)) => {
                self.assembler
                    .andw_rm_scaled(src, dst.offset, dst.base, dst.index, dst.scale as _);
            }

            (Operand::Address(src), Operand::Register(dst)) => {
                self.assembler.andw_mr(src.offset, src.base, dst);
            }

            (Operand::BaseIndex(src), Operand::Register(dst)) => {
                self.assembler
                    .andw_mr_scaled(src.offset, src.base, src.index, src.scale as _, dst);
            }

            _ => unreachable!("Invalid operands for and16: {:?}, {:?}", src, dest),
        }
    }

    pub fn and8(&mut self, src: impl Into<Operand>, dest: impl Into<Operand>) {
        let src = src.into();
        let dest = dest.into();
        match (src, dest) {
            (Operand::Imm32(imm), Operand::Address(dst)) => {
                self.assembler.andb_im(imm as _, dst.offset, dst.base);
            }

            (Operand::Register(src), Operand::Address(dst)) => {
                self.assembler.andb_rm(src, dst.offset, dst.base);
            }

            (Operand::Register(src), Operand::BaseIndex(dst)) => {
                self.assembler
                    .andb_rm_scaled(src, dst.offset, dst.base, dst.index, dst.scale as _);
            }

            _ => unreachable!("Invalid operands for and8: {:?}, {:?}", src, dest),
        }
    }

    pub fn and32_rrr(
        &mut self,
        op1: impl Into<Operand>,
        op2: impl Into<Operand>,
        dest: impl Into<Operand>,
    ) {
        let (op1, op2, dest) = (op1.into(), op2.into(), dest.into());

        match (op1, op2, dest) {
            (Operand::Register(op1), Operand::Register(op2), Operand::Register(dest)) => {
                if op1 == op2 {
                    self.zero_extend32_to_word(op1, dest)
                } else if op1 == dest {
                    self.and32(op2, dest)
                } else {
                    self.move32_if_needed(op2, dest);
                    self.and32(op1, dest);
                }
            }

            (Operand::Address(op1), Operand::Register(op2), Operand::Register(dest)) => {
                if op2 == dest {
                    self.and32(op1, dest);
                } else if op1.base == dest {
                    self.load32(op1, dest);
                    self.and32(op2, dest);
                } else {
                    self.zero_extend32_to_word(op2, dest);
                    self.and32(op1, dest);
                }
            }

            (Operand::Register(op1), Operand::Address(op2), Operand::Register(dest)) => {
                self.and32_rrr(op2, op1, dest);
            }

            (Operand::Imm32(imm), Operand::Register(op2), Operand::Register(dest)) => {
                self.move32_if_needed(op2, dest);
                self.and32(imm, dest);
            }

            _ => unreachable!(
                "Invalid operands for and32_rrr: {:?}, {:?}, {:?}",
                op1, op2, dest
            ),
        }
    }

    pub fn count_leading_zeros32(&mut self, src: impl Into<Operand>, dst: impl Into<Operand>) {
        let src = src.into();
        let dst = dst.into();
        match (src, dst) {
            (Operand::Register(src), Operand::Register(dst)) => {
                self.assembler.lzcnt_rr(src, dst);
            }

            (Operand::Address(src), Operand::Register(dst)) => {
                self.assembler.lzcnt_mr(src.offset, src.base, dst);
            }

            _ => unreachable!(
                "Invalid operands for count_leading_zeros32: {:?}, {:?}",
                src, dst
            ),
        }
    }

    pub fn count_trailing_zeros32(&mut self, src: u8, dest: u8) {
        self.assembler.tzcnt_rr(src, dest);
    }

    pub fn count_population32(&mut self, src: impl Into<Operand>, dest: u8) {
        let src = src.into();
        match src {
            Operand::Register(src) => {
                self.assembler.popcnt_rr(src, dest);
            }

            Operand::Address(src) => {
                self.assembler.popcnt_mr(src.offset, src.base, dest);
            }

            _ => unreachable!(
                "Invalid operands for count_population32: {:?}, {:?}",
                src, dest
            ),
        }
    }

    pub fn byte_swap32(&mut self, dest: u8) {
        self.assembler.bswapl_r(dest);
    }

    pub fn byte_swap16(&mut self, dest: u8) {
        self.assembler.rolw_i8r(8, dest);
        self.zero_extend16_to_32(dest, dest);
    }

    pub fn byte_swap64(&mut self, dest: u8) {
        self.assembler.bswapq_r(dest);
    }

    pub fn illegal_instruction(&mut self) {
        self.assembler.illegal_instruction();
    }

    pub fn lshift32(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Imm32(imm) => {
                self.assembler.shll_i8r(imm as _, dest);
            }

            Operand::Register(shift_amount) => {
                if shift_amount == ecx {
                    self.assembler.shll_clr(dest);
                } else {
                    self.swap(shift_amount, ecx);
                    self.assembler
                        .shll_clr(if dest == ecx { shift_amount } else { dest });
                    self.swap(shift_amount, ecx);
                }
            }

            op => unreachable!("Invalid operand for lshift32: {:?}", op),
        }
    }

    pub fn lshift32_rrr(
        &mut self,
        src: impl Into<Operand>,
        shift_amount: impl Into<Operand>,
        dest: u8,
    ) {
        match (src.into(), shift_amount.into()) {
            (Operand::Register(src), Operand::Imm32(imm)) => {
                self.move32_if_needed(src, dest);
                self.lshift32(imm, dest);
            }

            (Operand::Register(src), Operand::Register(shift_amount)) => {
                self.move32_if_needed(src, dest);
                self.lshift32(shift_amount, dest);
            }

            (Operand::Address(src), Operand::Register(shift_amount)) => {
                if shift_amount == dest {
                    self.mov(shift_amount, Self::SCRATCH_REGISTER);
                    self.load32(src, dest);
                    self.lshift32(Self::SCRATCH_REGISTER, dest);
                } else {
                    self.load32(src, dest);
                    self.lshift32(shift_amount, dest);
                }
            }

            (src, shift_amount) => unreachable!(
                "Invalid operands for lshift32_rrr: {:?}, {:?}, {:?}",
                src, shift_amount, dest
            ),
        }
    }

    pub fn mul32(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(src) => {
                self.assembler.imull_rr(src, dest);
            }

            Operand::Address(src) => {
                self.assembler.imull_mr(src.offset, src.base, dest);
            }

            Operand::Imm32(imm) => {
                self.assembler.imull_i32r(dest, imm, dest);
            }

            src => unreachable!("Invalid operand for mul32: {:?}", src),
        }
    }

    pub fn mul32_rrr(&mut self, src: impl Into<Operand>, src2: impl Into<Operand>, dest: u8) {
        match (src.into(), src2.into()) {
            (Operand::Register(src1), Operand::Register(src2)) => {
                if src2 == dest {
                    self.assembler.imull_rr(src1, dest);
                    return;
                }

                self.move32_if_needed(src1, dest);
                self.assembler.imull_rr(src2, dest);
            }

            (Operand::Address(op1), Operand::Register(op2)) => {
                if op2 == dest {
                    self.mul32(op1, dest);
                } else if op1.base == dest {
                    self.load32(op1, dest);
                    self.mul32(op2, dest);
                } else {
                    self.zero_extend32_to_word(op2, dest);
                    self.mul32(op1, dest);
                }
            }

            (Operand::Imm32(imm), Operand::Register(op2)) => {
                return self.assembler.imull_i32r(op2, imm, dest);
            }

            (src, src2) => unreachable!(
                "Invalid operands for mul32_rrr: {:?}, {:?}, {:?}",
                src, src2, dest
            ),
        }
    }

    pub fn x86_convert_to_double_word32(&mut self) {
        self.assembler.cdq();
    }

    pub fn x86div32(&mut self, denominator: u8) {
        self.assembler.idivl_r(denominator);
    }

    pub fn x86udiv32(&mut self, denominator: u8) {
        self.assembler.divl_r(denominator);
    }

    pub fn neg32(&mut self, op: impl Into<Operand>) {
        match op.into() {
            Operand::Register(r) => {
                self.assembler.negl_r(r);
            }
            Operand::Address(a) => {
                self.assembler.negl_m(a.offset, a.base);
            }

            Operand::BaseIndex(a) => {
                self.assembler
                    .negl_m_scaled(a.offset, a.base, a.index, a.scale as _);
            }

            _ => unreachable!(),
        }
    }

    pub fn neg32_rr(&mut self, src: u8, dest: u8) {
        self.move32_if_needed(src, dest);
        self.neg32(dest);
    }

    pub fn neg16(&mut self, src: impl Into<Operand>) {
        match src.into() {
            Operand::Address(a) => {
                self.assembler.negw_m(a.offset, a.base);
            }

            Operand::BaseIndex(a) => {
                self.assembler
                    .negw_m_scaled(a.offset, a.base, a.index, a.scale as _);
            }

            _ => unreachable!(),
        }
    }

    pub fn neg8(&mut self, src: impl Into<Operand>) {
        match src.into() {
            Operand::Address(a) => {
                self.assembler.negb_m(a.offset, a.base);
            }

            Operand::BaseIndex(a) => {
                self.assembler
                    .negb_m_scaled(a.offset, a.base, a.index, a.scale as _);
            }

            _ => unreachable!(),
        }
    }

    pub fn or32(&mut self, src: impl Into<Operand>, dest: impl Into<Operand>) {
        match (src.into(), dest.into()) {
            (Operand::Register(src), Operand::Register(dest)) => {
                self.assembler.orl_rr(src, dest);
            }

            (Operand::Imm32(imm), Operand::Register(dest)) => {
                self.assembler.orl_ir(imm, dest);
            }

            (Operand::Register(src), Operand::Address(dest)) => {
                self.assembler.orl_rm(src, dest.offset, dest.base);
            }

            (Operand::Register(src), Operand::BaseIndex(dest)) => {
                self.assembler.orl_rm_scaled(
                    src,
                    dest.offset,
                    dest.base,
                    dest.index,
                    dest.scale as _,
                );
            }

            (Operand::Address(src), Operand::Register(dest)) => {
                self.assembler.orl_mr(src.offset, src.base, dest);
            }

            (Operand::Address(src), Operand::Address(dest)) => {
                self.load32(src, 0);
                self.assembler.orl_rm(0, dest.offset, dest.base);
            }

            (Operand::Imm32(imm), Operand::Address(dest)) => {
                self.assembler.orl_im(imm, dest.offset, dest.base);
            }

            (Operand::Imm32(imm), Operand::BaseIndex(dest)) => {
                self.assembler.orl_im_scaled(
                    imm,
                    dest.offset,
                    dest.base,
                    dest.index,
                    dest.scale as _,
                );
            }

            (Operand::Register(reg), Operand::AbsoluteAddress(addr)) => {
                self.mov(addr, Self::SCRATCH_REGISTER);
                self.or32(reg, Address::new(Self::SCRATCH_REGISTER, 0));
            }

            (src, dest) => unreachable!("Invalid operands for or32: {:?}, {:?}", src, dest),
        }
    }

    pub fn or32_rrr(&mut self, op1: impl Into<Operand>, op2: impl Into<Operand>, dest: u8) {
        match (op1.into(), op2.into()) {
            (Operand::Register(op1), Operand::Register(op2)) => {
                if op1 == op2 {
                    self.zero_extend32_to_word(op1, dest);
                } else if op1 == dest {
                    self.or32(op2, dest);
                } else {
                    self.move32_if_needed(op2, dest);
                    self.or32(op1, dest);
                }
            }

            (Operand::Address(op1), Operand::Register(op2))
            | (Operand::Register(op2), Operand::Address(op1)) => {
                if op2 == dest {
                    self.or32(op1, dest);
                } else if op1.base == dest {
                    self.load32(op1, dest);
                    self.or32(op2, dest);
                } else {
                    self.zero_extend32_to_word(op2, dest);
                    self.or32(op1, dest);
                }
            }
            (op1, op2) => unreachable!("Invalid operands for or32_rrr: {:?}, {:?}", op1, op2),
        }
    }

    pub fn or16(&mut self, src: impl Into<Operand>, dest: impl Into<Operand>) {
        match (src.into(), dest.into()) {
            (Operand::Register(src), Operand::Address(dest)) => {
                self.assembler.orw_rm(src, dest.offset, dest.base);
            }

            (Operand::Register(src), Operand::BaseIndex(dest)) => {
                self.assembler.orw_rm_scaled(
                    src,
                    dest.offset,
                    dest.base,
                    dest.index,
                    dest.scale as _,
                );
            }

            (Operand::Imm32(src), Operand::Address(dest)) => {
                self.assembler.orw_im(src, dest.offset, dest.base);
            }

            (Operand::Imm32(imm), Operand::BaseIndex(dest)) => {
                self.assembler.orw_im_scaled(
                    imm,
                    dest.offset,
                    dest.base,
                    dest.index,
                    dest.scale as _,
                );
            }
            (Operand::Imm32(imm), Operand::AbsoluteAddress(addr)) => {
                self.mov(addr, Self::SCRATCH_REGISTER);
                self.or16(imm, Address::new(Self::SCRATCH_REGISTER, 0));
            }
            _ => unreachable!(),
        }
    }

    pub fn or8(&mut self, src: impl Into<Operand>, dest: impl Into<Operand>) {
        match (src.into(), dest.into()) {
            (Operand::Register(src), Operand::Address(dest)) => {
                self.assembler.orb_rm(src, dest.offset, dest.base);
            }

            (Operand::Register(src), Operand::BaseIndex(dest)) => {
                self.assembler.orb_rm_scaled(
                    src,
                    dest.offset,
                    dest.base,
                    dest.index,
                    dest.scale as _,
                );
            }

            (Operand::Imm32(src), Operand::Address(dest)) => {
                self.assembler.orb_im(src, dest.offset, dest.base);
            }

            (Operand::Imm32(src), Operand::BaseIndex(dest)) => {
                self.assembler.orb_im_scaled(
                    src,
                    dest.offset,
                    dest.base,
                    dest.index,
                    dest.scale as _,
                );
            }

            (Operand::Register(src), Operand::AbsoluteAddress(addr)) => {
                self.mov(addr, Self::SCRATCH_REGISTER);
                self.or8(src, Address::new(Self::SCRATCH_REGISTER, 0));
            }

            (Operand::Imm32(src), Operand::AbsoluteAddress(addr)) => {
                self.mov(addr, Self::SCRATCH_REGISTER);
                self.or8(src, Address::new(Self::SCRATCH_REGISTER, 0));
            }

            _ => unreachable!(),
        }
    }

    pub fn rshift32(&mut self, shift_amount: impl Into<Operand>, dest: u8) {
        match shift_amount.into() {
            Operand::Register(shift_amount) => {
                if shift_amount == ecx {
                    self.assembler.sarl_clr(dest);
                } else {
                    self.swap(shift_amount, ecx);
                    self.assembler
                        .sarl_clr(if dest == ecx { shift_amount } else { dest });
                    self.swap(shift_amount, ecx);
                }
            }

            Operand::Imm32(imm) => {
                self.assembler.sarl_i8r(imm as _, dest);
            }

            _ => unreachable!(),
        }
    }

    pub fn rshift32_rrr(&mut self, src: u8, shift_amount: impl Into<Operand>, dest: u8) {
        match shift_amount.into() {
            Operand::Register(shift_amount) => {
                self.move32_if_needed(src, dest);
                self.rshift32(shift_amount, dest);
            }

            Operand::Imm32(imm) => {
                self.move32_if_needed(src, dest);
                self.rshift32(imm, dest);
            }

            _ => unreachable!(),
        }
    }

    pub fn urshift32(&mut self, shift_amount: impl Into<Operand>, dest: u8) {
        match shift_amount.into() {
            Operand::Register(shift_amount) => {
                if shift_amount == ecx {
                    self.assembler.shrl_clr(dest);
                } else {
                    self.swap(shift_amount, ecx);
                    self.assembler
                        .shrl_clr(if dest == ecx { shift_amount } else { dest });
                    self.swap(shift_amount, ecx);
                }
            }

            Operand::Imm32(imm) => {
                self.assembler.shrl_i8r(imm as _, dest);
            }

            _ => unreachable!(),
        }
    }

    pub fn urshift32_rrr(&mut self, src: u8, shift_amount: impl Into<Operand>, dest: u8) {
        match shift_amount.into() {
            Operand::Register(shift_amount) => {
                self.move32_if_needed(src, dest);
                self.urshift32(shift_amount, dest);
            }

            Operand::Imm32(imm) => {
                self.move32_if_needed(src, dest);
                self.urshift32(imm, dest);
            }

            _ => unreachable!(),
        }
    }

    pub fn rotate_right32(&mut self, shift_amount: impl Into<Operand>, dest: u8) {
        match shift_amount.into() {
            Operand::Register(shift_amount) => {
                if shift_amount == ecx {
                    self.assembler.rorl_clr(dest);
                } else {
                    self.swap(shift_amount, ecx);
                    self.assembler
                        .rorl_clr(if dest == ecx { shift_amount } else { dest });
                    self.swap(shift_amount, ecx);
                }
            }

            Operand::Imm32(imm) => {
                self.assembler.rorl_i8r(imm as _, dest);
            }

            _ => unreachable!(),
        }
    }

    pub fn rotate_right32_rrr(&mut self, src: u8, shift_amount: impl Into<Operand>, dest: u8) {
        match shift_amount.into() {
            Operand::Register(shift_amount) => {
                self.move32_if_needed(src, dest);
                self.rotate_right32(shift_amount, dest);
            }

            Operand::Imm32(imm) => {
                self.move32_if_needed(src, dest);
                self.rotate_right32(imm, dest);
            }

            _ => unreachable!(),
        }
    }

    pub fn rotate_left32(&mut self, shift_amount: impl Into<Operand>, dest: u8) {
        match shift_amount.into() {
            Operand::Register(shift_amount) => {
                if shift_amount == ecx {
                    self.assembler.roll_clr(dest);
                } else {
                    self.swap(shift_amount, ecx);
                    self.assembler
                        .roll_clr(if dest == ecx { shift_amount } else { dest });
                    self.swap(shift_amount, ecx);
                }
            }

            Operand::Imm32(imm) => {
                self.assembler.roll_i8r(imm as _, dest);
            }

            _ => unreachable!(),
        }
    }

    pub fn rotate_left32_rrr(&mut self, src: u8, shift_amount: impl Into<Operand>, dest: u8) {
        match shift_amount.into() {
            Operand::Register(shift_amount) => {
                self.move32_if_needed(src, dest);
                self.rotate_left32(shift_amount, dest);
            }

            Operand::Imm32(imm) => {
                self.move32_if_needed(src, dest);
                self.rotate_left32(imm, dest);
            }

            _ => unreachable!(),
        }
    }

    pub fn sub32(&mut self, src: impl Into<Operand>, dest: impl Into<Operand>) {
        let src = src.into();
        let dest = dest.into();
        match (src, dest) {
            (Operand::Register(src), Operand::Register(dest)) => {
                self.assembler.subl_rr(src, dest);
            }

            (Operand::Imm32(imm), Operand::Register(dest)) => {
                if imm == 1 {
                    self.assembler.dec_r(dest);
                } else {
                    self.assembler.subl_ir(imm as _, dest);
                }
            }

            (Operand::Register(src), Operand::Address(dest)) => {
                self.assembler.subl_rm(src, dest.offset, dest.base);
            }

            (Operand::Imm32(imm), Operand::Address(dest)) => {
                self.assembler.subl_im(imm as _, dest.offset, dest.base);
            }

            (Operand::Register(src), Operand::BaseIndex(dest)) => {
                self.assembler.subl_rm_scaled(
                    src,
                    dest.offset,
                    dest.base,
                    dest.index,
                    dest.scale as _,
                );
            }

            (Operand::Imm32(imm), Operand::BaseIndex(dest)) => {
                self.assembler.subl_im_scaled(
                    imm as _,
                    dest.offset,
                    dest.base,
                    dest.index,
                    dest.scale as _,
                );
            }

            (Operand::Address(src), Operand::Register(dest)) => {
                self.assembler.subl_mr(src.offset, src.base, dest);
            }

            (Operand::BaseIndex(src), Operand::Register(dest)) => {
                self.assembler.subl_mr_scaled(
                    src.offset,
                    src.base,
                    src.index,
                    src.scale as _,
                    dest,
                );
            }

            (Operand::Imm32(imm), Operand::AbsoluteAddress(addr)) => {
                self.mov(addr, Self::SCRATCH_REGISTER);
                self.sub32(imm, Address::new(Self::SCRATCH_REGISTER, 0));
            }
            _ => unreachable!(),
        }
    }

    pub fn sub32_rrr(&mut self, left: impl Into<Operand>, right: impl Into<Operand>, dest: u8) {
        let left = left.into();
        let right = right.into();
        match (left, right) {
            (Operand::Register(left), Operand::Register(right)) => {
                if dest == right {
                    self.neg32(dest);
                    self.sub32(left, dest);
                } else {
                    self.mov(left, dest);
                    self.sub32(right, dest);
                }
            }

            (Operand::Register(left), Operand::Imm32(right)) => {
                self.mov(left, dest);
                self.sub32(right, dest);
            }

            (Operand::Imm32(imm), Operand::BaseIndex(addr)) => {
                self.assembler.subl_im_scaled(
                    imm as _,
                    addr.offset,
                    addr.base,
                    addr.index,
                    addr.scale as _,
                );
            }

            (Operand::Imm32(imm), Operand::Address(addr)) => {
                self.assembler.subl_im(imm as _, addr.offset, addr.base);
            }

            _ => unreachable!(),
        }
    }

    pub fn sub16(&mut self, src: impl Into<Operand>, dest: impl Into<Operand>) {
        match (src.into(), dest.into()) {
            (Operand::Register(src), Operand::Address(addr)) => {
                self.assembler.subw_rm(src, addr.offset, addr.base);
            }

            (Operand::Register(src), Operand::BaseIndex(addr)) => {
                self.assembler.subw_rm_scaled(
                    src,
                    addr.offset,
                    addr.base,
                    addr.index,
                    addr.scale as _,
                );
            }

            (Operand::Imm32(imm), Operand::Address(addr)) => {
                self.assembler.subw_im(imm as _, addr.offset, addr.base);
            }

            (Operand::Imm32(imm), Operand::BaseIndex(addr)) => {
                self.assembler.subw_im_scaled(
                    imm as _,
                    addr.offset,
                    addr.base,
                    addr.index,
                    addr.scale as _,
                );
            }

            (src, dest) => unreachable!("{:?} {:?}", src, dest),
        }
    }

    pub fn sub8(&mut self, src: impl Into<Operand>, dest: impl Into<Operand>) {
        match (src.into(), dest.into()) {
            (Operand::Register(src), Operand::Address(addr)) => {
                self.assembler.subb_rm(src, addr.offset, addr.base);
            }

            (Operand::Register(src), Operand::BaseIndex(addr)) => {
                self.assembler.subb_rm_scaled(
                    src,
                    addr.offset,
                    addr.base,
                    addr.index,
                    addr.scale as _,
                );
            }

            (Operand::Imm32(imm), Operand::Address(addr)) => {
                self.assembler.subb_im(imm as _, addr.offset, addr.base);
            }

            (Operand::Imm32(imm), Operand::BaseIndex(addr)) => {
                self.assembler.subb_im_scaled(
                    imm as _,
                    addr.offset,
                    addr.base,
                    addr.index,
                    addr.scale as _,
                );
            }

            (src, dest) => unreachable!("{:?} {:?}", src, dest),
        }
    }

    pub fn xor32(&mut self, src: impl Into<Operand>, dest: impl Into<Operand>) {
        let src = src.into();
        let dest = dest.into();
        match (src, dest) {
            (Operand::Register(src), Operand::Register(dest)) => {
                self.assembler.xorl_rr(src, dest);
            }

            (Operand::Imm32(imm), Operand::Register(dest)) => {
                if imm == -1 {
                    self.assembler.notl_r(dest);
                    return;
                }

                self.assembler.xorl_ir(imm as _, dest);
            }

            (Operand::Register(src), Operand::Address(dest)) => {
                self.assembler.xorl_rm(src, dest.offset, dest.base);
            }

            (Operand::Imm32(imm), Operand::Address(dest)) => {
                if imm == -1 {
                    self.assembler.notl_m(dest.offset, dest.base);
                    return;
                }
                self.assembler.xorl_im(imm as _, dest.offset, dest.base);
            }

            (Operand::Register(src), Operand::BaseIndex(dest)) => {
                self.assembler.xorl_rm_scaled(
                    src,
                    dest.offset,
                    dest.base,
                    dest.index,
                    dest.scale as _,
                );
            }

            (Operand::Imm32(imm), Operand::BaseIndex(dest)) => {
                if imm == -1 {
                    self.assembler.notl_m_scaled(
                        dest.offset,
                        dest.base,
                        dest.index,
                        dest.scale as _,
                    );
                    return;
                }
                self.assembler.xorl_im_scaled(
                    imm as _,
                    dest.offset,
                    dest.base,
                    dest.index,
                    dest.scale as _,
                );
            }

            (Operand::Address(src), Operand::Register(dest)) => {
                self.assembler.xorl_mr(src.offset, src.base, dest);
            }

            (Operand::BaseIndex(src), Operand::Register(dest)) => {
                self.assembler.xorl_mr_scaled(
                    src.offset,
                    src.base,
                    src.index,
                    src.scale as _,
                    dest,
                );
            }

            _ => unreachable!(),
        }
    }

    pub fn xor32_rrr(&mut self, left: impl Into<Operand>, right: impl Into<Operand>, dest: u8) {
        let left = left.into();
        let right = right.into();
        match (left, right) {
            (Operand::Register(op1), Operand::Register(op2)) => {
                if op1 == op2 {
                    self.mov(0i32, dest);
                } else if op1 == dest {
                    self.xor32(op2, dest);
                } else {
                    self.move32_if_needed(op2, dest);
                    self.xor32(op1, dest);
                }
            }

            (Operand::Address(op1), Operand::Register(op2)) => {
                if op2 == dest {
                    self.xor32(op1, dest);
                } else if op1.base == dest {
                    self.load32(op1, dest);
                    self.xor32(op2, dest);
                } else {
                    self.zero_extend32_to_word(op2, dest);
                    self.xor32(op1, dest);
                }
            }

            (Operand::Imm32(imm), Operand::Register(src)) => {
                self.move32_if_needed(src, dest);
                self.xor32(imm, dest);
            }

            _ => unreachable!(),
        }
    }

    pub fn not32(&mut self, dest: impl Into<Operand>) {
        let dest = dest.into();
        match dest {
            Operand::Register(dest) => {
                self.assembler.notl_r(dest);
            }

            Operand::Address(dest) => {
                self.assembler.notl_m(dest.offset, dest.base);
            }

            Operand::BaseIndex(dest) => {
                self.assembler
                    .notl_m_scaled(dest.offset, dest.base, dest.index, dest.scale as _);
            }

            _ => unreachable!(),
        }
    }

    pub fn not16(&mut self, dest: impl Into<Operand>) {
        let dest = dest.into();
        match dest {
            Operand::Address(dest) => {
                self.assembler.notw_m(dest.offset, dest.base);
            }

            Operand::BaseIndex(dest) => {
                self.assembler
                    .notw_m_scaled(dest.offset, dest.base, dest.index, dest.scale as _);
            }

            _ => unreachable!(),
        }
    }

    pub fn not8(&mut self, dest: impl Into<Operand>) {
        let dest = dest.into();
        match dest {
            Operand::Address(dest) => {
                self.assembler.notb_m(dest.offset, dest.base);
            }

            Operand::BaseIndex(dest) => {
                self.assembler
                    .notb_m_scaled(dest.offset, dest.base, dest.index, dest.scale as _);
            }

            _ => unreachable!(),
        }
    }

    pub fn sqrt_double(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(src) => {
                self.assembler.sqrtsd_rr(src, dest);
            }

            Operand::Address(a) => {
                self.assembler.sqrtsd_mr(a.offset, a.base, dest);
            }

            op => unreachable!("{:?}", op),
        }
    }

    pub fn sqrt_float(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(src) => {
                self.assembler.sqrtss_rr(src, dest);
            }

            Operand::Address(a) => {
                self.assembler.sqrtss_mr(a.offset, a.base, dest);
            }

            op => unreachable!("{:?}", op),
        }
    }

    pub fn abs_double(&mut self, src: u8, dest: u8) {
        static NEGATIVE_ZERO_CONSTANT: f64 = -1.0;

        self.load_double(
            AbsoluteAddress::new(&NEGATIVE_ZERO_CONSTANT as *const _ as _),
            dest,
        );
        self.assembler.andnpd_rr(src, dest);
    }

    pub fn negate_double(&mut self, src: u8, dest: u8) {
        static NEGATIVE_ONE_CONSTANT: f64 = -1.0;

        self.load_double(
            AbsoluteAddress::new(&NEGATIVE_ONE_CONSTANT as *const _ as _),
            dest,
        );
        self.assembler.xorpd_rr(src, dest);
    }

    pub fn ceil_double(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(src) => {
                self.assembler
                    .roundsd_rr(src, dest, RoundingType::TowardInfinity);
            }

            Operand::Address(a) => {
                self.assembler
                    .roundsd_mr(a.offset, a.base, dest, RoundingType::TowardInfinity);
            }

            op => unreachable!("{:?}", op),
        }
    }

    pub fn ceil_float(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(src) => {
                self.assembler
                    .roundss_rr(src, dest, RoundingType::TowardInfinity);
            }

            Operand::Address(a) => {
                self.assembler
                    .roundss_mr(a.offset, a.base, dest, RoundingType::TowardInfinity);
            }

            op => unreachable!("{:?}", op),
        }
    }

    pub fn floor_double(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(src) => {
                self.assembler
                    .roundsd_rr(src, dest, RoundingType::TowardNegativeInfinity);
            }

            Operand::Address(a) => {
                self.assembler.roundsd_mr(
                    a.offset,
                    a.base,
                    dest,
                    RoundingType::TowardNegativeInfinity,
                );
            }

            op => unreachable!("{:?}", op),
        }
    }

    pub fn floor_float(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(src) => {
                self.assembler
                    .roundss_rr(src, dest, RoundingType::TowardNegativeInfinity);
            }

            Operand::Address(a) => {
                self.assembler.roundss_mr(
                    a.offset,
                    a.base,
                    dest,
                    RoundingType::TowardNegativeInfinity,
                );
            }

            op => unreachable!("{:?}", op),
        }
    }

    pub fn round_to_nearest_int_double(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(src) => {
                self.assembler
                    .roundsd_rr(src, dest, RoundingType::ToNearestWithTiesToEven);
            }

            Operand::Address(a) => {
                self.assembler.roundsd_mr(
                    a.offset,
                    a.base,
                    dest,
                    RoundingType::ToNearestWithTiesToEven,
                );
            }

            op => unreachable!("{:?}", op),
        }
    }

    pub fn round_to_nearest_int_float(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(src) => {
                self.assembler
                    .roundss_rr(src, dest, RoundingType::ToNearestWithTiesToEven);
            }

            Operand::Address(a) => {
                self.assembler.roundss_mr(
                    a.offset,
                    a.base,
                    dest,
                    RoundingType::ToNearestWithTiesToEven,
                );
            }

            op => unreachable!("{:?}", op),
        }
    }

    pub fn round_toward_zero_double(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(src) => {
                self.assembler
                    .roundsd_rr(src, dest, RoundingType::TowardZero);
            }

            Operand::Address(a) => {
                self.assembler
                    .roundsd_mr(a.offset, a.base, dest, RoundingType::TowardZero);
            }

            op => unreachable!("{:?}", op),
        }
    }

    pub fn round_toward_zero_float(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(src) => {
                self.assembler
                    .roundss_rr(src, dest, RoundingType::TowardZero);
            }

            Operand::Address(a) => {
                self.assembler
                    .roundss_mr(a.offset, a.base, dest, RoundingType::TowardZero);
            }

            op => unreachable!("{:?}", op),
        }
    }

    // Memory access operations:
    //
    // Loads are of the form load(address, destination) and stores of the form
    // store(source, address).  The source for a store may be an i32.  Address
    // operand objects to loads and store will be implicitly constructed if a
    // register is passed.

    pub fn load32(&mut self, address: impl Into<Operand>, dest: u8) {
        match address.into() {
            Operand::Address(addr) => {
                self.assembler.movl_mr(addr.offset, addr.base, dest);
            }

            Operand::BaseIndex(addr) => {
                self.assembler.movl_mr_scaled(
                    addr.offset,
                    addr.base,
                    addr.index,
                    addr.scale as _,
                    dest,
                );
            }

            Operand::AbsoluteAddress(addr) => {
                if dest == eax {
                    self.assembler.movl_meax(addr.ptr)
                } else {
                    self.mov(Operand::Imm64(addr.ptr as _), Operand::Register(dest));
                    self.load32(Address::new(dest, 0), dest);
                }
            }

            op => unreachable!("Invalid operand for load32: {:?}", op),
        }
    }

    pub fn load_compact_with_address_offset_patch(
        &mut self,
        address: Address,
        dest: u8,
    ) -> DataLabelCompact {
        self.pad_before_patch();
        self.assembler
            .movl_mr_disp8(address.offset, address.base, dest);
        DataLabelCompact::new(self)
    }

    pub fn load8(&mut self, src: impl Into<Operand>, dst: u8) {
        match src.into() {
            Operand::Address(address) => {
                self.assembler.movzbl_mr(address.offset, address.base, dst)
            }

            Operand::BaseIndex(address) => self.assembler.movzbl_mr_scaled(
                address.offset,
                address.base,
                address.index,
                address.scale as _,
                dst,
            ),

            Operand::AbsoluteAddress(addr) => {
                self.mov(addr, dst);
                self.load8(Address::new(dst, 0), dst);
            }

            _ => unreachable!(),
        }
    }

    pub fn load8_signed_extend_to_32(&mut self, src: impl Into<Operand>, dst: u8) {
        match src.into() {
            Operand::Address(address) => {
                self.assembler.movsbl_mr(address.offset, address.base, dst)
            }

            Operand::BaseIndex(address) => self.assembler.movsbl_mr_scaled(
                address.offset,
                address.base,
                address.index,
                address.scale as _,
                dst,
            ),

            _ => unreachable!(),
        }
    }

    pub fn zero_extend8_to_32(&mut self, src: u8, dst: u8) {
        self.assembler.movzbl_rr(src, dst)
    }

    pub fn sign_extend8_to_32(&mut self, src: u8, dst: u8) {
        self.assembler.movsbl_rr(src, dst)
    }

    pub fn load16(&mut self, src: impl Into<Operand>, dst: u8) {
        match src.into() {
            Operand::Address(address) => {
                self.assembler.movzwl_mr(address.offset, address.base, dst)
            }

            Operand::BaseIndex(address) => self.assembler.movzwl_mr_scaled(
                address.offset,
                address.base,
                address.index,
                address.scale as _,
                dst,
            ),

            Operand::ExtendedAddress(address) => {
                self.mov(address.offset as i64, Self::SCRATCH_REGISTER);
                self.load16(
                    BaseIndex::new(
                        Self::SCRATCH_REGISTER,
                        address.base,
                        Scale::TimesOne,
                        0,
                        Extend::None,
                    ),
                    dst,
                );
            }

            _ => unreachable!(),
        }
    }

    pub fn load16_signed_extend_to_32(&mut self, src: impl Into<Operand>, dst: u8) {
        match src.into() {
            Operand::Address(address) => {
                self.assembler.movswl_mr(address.offset, address.base, dst)
            }

            Operand::BaseIndex(address) => self.assembler.movswl_mr_scaled(
                address.offset,
                address.base,
                address.index,
                address.scale as _,
                dst,
            ),

            _ => unreachable!(),
        }
    }

    pub fn load_pair32(&mut self, src: u8, offset: i32, dest1: u8, dest2: u8) {
        if src == dest1 {
            self.load32(Address::new(src, offset + 4), dest2);
            self.load32(Address::new(src, offset), dest1);
        } else {
            self.load32(Address::new(src, offset), dest1);
            self.load32(Address::new(src, offset + 4), dest2);
        }
    }

    pub fn store32(&mut self, src: impl Into<Operand>, dst: impl Into<Operand>) {
        match (src.into(), dst.into()) {
            (Operand::Imm32(imm), Operand::Address(address)) => {
                self.assembler.movl_i32m(imm, address.offset, address.base)
            }

            (Operand::Imm32(imm), Operand::BaseIndex(address)) => self.assembler.movl_i32m_scaled(
                imm,
                address.offset,
                address.base,
                address.index,
                address.scale as _,
            ),

            (Operand::Register(src), Operand::Address(address)) => {
                self.assembler.movl_rm(src, address.offset, address.base)
            }

            (Operand::Register(src), Operand::BaseIndex(address)) => self.assembler.movl_rm_scaled(
                src,
                address.offset,
                address.base,
                address.index,
                address.scale as _,
            ),

            (Operand::Imm32(imm), Operand::AbsoluteAddress(addr)) => {
                self.mov(addr, Self::SCRATCH_REGISTER);
                self.store32(imm, Address::new(Self::SCRATCH_REGISTER, 0));
            }

            (Operand::Register(reg), Operand::AbsoluteAddress(addr)) => {
                if reg == eax {
                    self.assembler.movl_eaxm(addr.ptr);
                    return;
                }
                self.mov(addr, Self::SCRATCH_REGISTER);
                self.store32(reg, Address::new(Self::SCRATCH_REGISTER, 0));
            }

            (src, dst) => unreachable!("Invalid operands for store32: {:?} {:?}", src, dst),
        }
    }

    pub fn store8(&mut self, src: impl Into<Operand>, dst: impl Into<Operand>) {
        match (src.into(), dst.into()) {
            (Operand::Imm32(imm), Operand::Address(address)) => {
                self.assembler
                    .movb_i8m(imm as _, address.offset, address.base)
            }

            (Operand::Imm32(imm), Operand::BaseIndex(address)) => self.assembler.movb_i8m_scaled(
                imm as _,
                address.offset,
                address.base,
                address.index,
                address.scale as _,
            ),

            (Operand::Register(src), Operand::BaseIndex(address)) => self.assembler.movb_rm_scaled(
                src,
                address.offset,
                address.base,
                address.index,
                address.scale as _,
            ),

            (Operand::Register(src), Operand::Address(address)) => {
                self.assembler.movb_rm(src, address.offset, address.base)
            }

            (Operand::Imm32(imm), Operand::AbsoluteAddress(addr)) => {
                self.mov(addr, Self::SCRATCH_REGISTER);
                self.store8(imm, Address::new(Self::SCRATCH_REGISTER, 0));
            }

            (Operand::Register(reg), Operand::AbsoluteAddress(addr)) => {
                self.mov(addr, Self::SCRATCH_REGISTER);
                self.store8(reg, Address::new(Self::SCRATCH_REGISTER, 0));
            }

            (src, dst) => unreachable!("Invalid operands for store8: {:?} {:?}", src, dst),
        }
    }

    pub fn store_pair32(&mut self, src1: u8, src2: u8, dest: u8, offset: i32) {
        self.store32(src1, Address::new(dest, offset));
        self.store32(src2, Address::new(dest, offset + 4));
    }

    pub const fn get_unused_register(address: BaseIndex) -> u8 {
        if address.base != eax && address.index != eax {
            return eax;
        }

        if address.base != ebx && address.index != ebx {
            return ebx;
        }
        assert!(address.base != ecx && address.index != ecx);
        ecx
    }

    pub const fn get_unused_register_from_address(address: Address) -> u8 {
        if address.base != eax {
            return eax;
        }

        assert!(address.base != edx);
        edx
    }

    pub fn store16(&mut self, src: impl Into<Operand>, dst: impl Into<Operand>) {
        match (src.into(), dst.into()) {
            (Operand::Imm32(imm), Operand::Address(address)) => {
                self.assembler
                    .movw_im(imm as _, address.offset, address.base)
            }

            (Operand::Imm32(imm), Operand::BaseIndex(address)) => self.assembler.movw_im_scaled(
                imm as _,
                address.offset,
                address.base,
                address.index,
                address.scale as _,
            ),

            (Operand::Register(src), Operand::Address(address)) => {
                self.assembler.movw_rm(src, address.offset, address.base)
            }

            (Operand::Register(src), Operand::BaseIndex(address)) => self.assembler.movw_rm_scaled(
                src,
                address.offset,
                address.base,
                address.index,
                address.scale as _,
            ),

            (src, dst) => unreachable!("Invalid operands for store16: {:?} {:?}", src, dst),
        }
    }

    pub fn move_double(&mut self, src: u8, dst: u8) {
        if src == dst {
            return;
        }

        self.assembler.movaps_rr(src, dst);
    }

    pub fn store_double(&mut self, src: u8, dst: impl Into<Operand>) {
        match dst.into() {
            Operand::Address(addr) => {
                self.assembler.movsd_rm(src, addr.offset, addr.base);
            }

            Operand::BaseIndex(addr) => {
                self.assembler.movsd_rm_scaled(
                    src,
                    addr.offset,
                    addr.base,
                    addr.index,
                    addr.scale as _,
                );
            }

            op => unreachable!("{:?}", op),
        }
    }

    pub fn store_float(&mut self, src: u8, dst: impl Into<Operand>) {
        match dst.into() {
            Operand::Address(addr) => {
                self.assembler.movss_rm(src, addr.offset, addr.base);
            }

            Operand::BaseIndex(addr) => {
                self.assembler.movss_rm_scaled(
                    src,
                    addr.offset,
                    addr.base,
                    addr.index,
                    addr.scale as _,
                );
            }

            op => unreachable!("{:?}", op),
        }
    }

    pub fn convert_double_to_float(&mut self, src: impl Into<Operand>, dst: u8) {
        match src.into() {
            Operand::Address(addr) => {
                self.assembler.cvtsd2ss_mr(addr.offset, addr.base, dst);
            }

            Operand::Register(src) => {
                self.assembler.cvtsd2ss_rr(src, dst);
            }

            op => unreachable!("{:?}", op),
        }
    }

    pub fn convert_float_to_double(&mut self, src: impl Into<Operand>, dst: u8) {
        match src.into() {
            Operand::Address(addr) => {
                self.assembler.cvtss2sd_mr(addr.offset, addr.base, dst);
            }

            Operand::Register(src) => {
                self.assembler.cvtss2sd_rr(src, dst);
            }

            op => unreachable!("{:?}", op),
        }
    }

    pub fn add_double(&mut self, src1: impl Into<Operand>, src2: impl Into<Operand>, dest: u8) {
        match (src1.into(), src2.into()) {
            (Operand::Register(src1), Operand::Register(src2)) => {
                if src1 == dest {
                    self.assembler.addsd_rr(src2, dest);
                } else {
                    self.move_double(src2, dest);
                    self.assembler.addsd_rr(src1, dest);
                }
            }

            (Operand::Address(op1), Operand::Register(op2))
            | (Operand::Register(op2), Operand::Address(op1)) => {
                if op2 == dest {
                    self.assembler.addsd_mr(op1.offset, op1.base, dest);
                } else {
                    self.load_double(op1, dest);
                    self.assembler.addsd_rr(op2, dest);
                }
            }

            (Operand::BaseIndex(address), Operand::Register(op2)) => {
                if op2 == dest {
                    self.assembler.addsd_mr_scaled(
                        address.offset,
                        address.base,
                        address.index,
                        address.scale as _,
                        dest,
                    );
                } else {
                    self.load_double(address, dest);
                    self.assembler.addsd_rr(op2, dest);
                }
            }

            (Operand::AbsoluteAddress(address), Operand::Register(dest)) => {
                self.mov(address, Self::SCRATCH_REGISTER);
                self.assembler.addsd_mr(0, Self::SCRATCH_REGISTER, dest);
            }

            (src1, src2) => unreachable!("Invalid operands for add_double: {:?} {:?}", src1, src2),
        }
    }

    pub fn add_double_rr(&mut self, src1: impl Into<Operand>, dest: u8) {
        self.add_double(src1.into(), dest, dest);
    }

    pub fn add_float(&mut self, src1: impl Into<Operand>, src2: impl Into<Operand>, dest: u8) {
        match (src1.into(), src2.into()) {
            (Operand::Register(src1), Operand::Register(src2)) => {
                if src1 == dest {
                    self.assembler.addss_rr(src2, dest);
                } else {
                    self.move_double(src2, dest);
                    self.assembler.addss_rr(src1, dest);
                }
            }

            (Operand::Address(op1), Operand::Register(op2))
            | (Operand::Register(op2), Operand::Address(op1)) => {
                if op2 == dest {
                    self.assembler.addss_mr(op1.offset, op1.base, dest);
                } else {
                    self.load_float(op1, dest);
                    self.assembler.addss_rr(op2, dest);
                }
            }

            (Operand::BaseIndex(address), Operand::Register(op2)) => {
                if op2 == dest {
                    self.assembler.addss_mr_scaled(
                        address.offset,
                        address.base,
                        address.index,
                        address.scale as _,
                        dest,
                    );
                } else {
                    self.load_float(address, dest);
                    self.assembler.addss_rr(op2, dest);
                }
            }

            (src1, src2) => unreachable!("Invalid operands for add_float: {:?} {:?}", src1, src2),
        }
    }

    pub fn div_double(&mut self, src: impl Into<Operand>, dst: u8) {
        match src.into() {
            Operand::Register(src) => {
                self.assembler.divsd_rr(src, dst);
            }

            Operand::Address(op) => {
                self.assembler.divsd_mr(op.offset, op.base, dst);
            }

            op => unreachable!("{:?}", op),
        }
    }

    pub fn div_double_rrr(&mut self, op1: u8, op2: u8, dst: u8) {
        // B := A / B is invalid.
        assert!(op1 == dst || op2 != dst);
        self.move_double(op1, dst);
        self.div_double(op2, dst);
    }

    pub fn div_float(&mut self, src: impl Into<Operand>, dst: u8) {
        match src.into() {
            Operand::Register(src) => {
                self.assembler.divss_rr(src, dst);
            }

            Operand::Address(op) => {
                self.assembler.divss_mr(op.offset, op.base, dst);
            }

            op => unreachable!("{:?}", op),
        }
    }

    pub fn div_float_rrr(&mut self, op1: u8, op2: u8, dst: u8) {
        // B := A / B is invalid.
        assert!(op1 == dst || op2 != dst);
        self.move_double(op1, dst);
        self.div_float(op2, dst);
    }

    pub fn sub_double(&mut self, op1: u8, op2: impl Into<Operand>, dst: u8) {
        match op2.into() {
            Operand::Register(op2) => {
                self.move_double(op1, dst);
                self.assembler.subsd_rr(op2, dst);
            }

            Operand::Address(op2) => {
                self.move_double(op1, dst);
                self.assembler.subsd_mr(op2.offset, op2.base, dst);
            }

            Operand::BaseIndex(address) => {
                self.move_double(op1, dst);
                self.assembler.subsd_mr_scaled(
                    address.offset,
                    address.base,
                    address.index,
                    address.scale as _,
                    dst,
                );
            }

            op => unreachable!("{:?}", op),
        }
    }

    pub fn sub_double_rr(&mut self, src: impl Into<Operand>, dst: u8) {
        self.sub_double(dst, src, dst);
    }

    pub fn sub_float(&mut self, op1: u8, op2: impl Into<Operand>, dst: u8) {
        match op2.into() {
            Operand::Register(op2) => {
                self.move_double(op1, dst);
                self.assembler.subss_rr(op2, dst);
            }

            Operand::Address(op2) => {
                self.move_double(op1, dst);
                self.assembler.subss_mr(op2.offset, op2.base, dst);
            }

            Operand::BaseIndex(address) => {
                self.move_double(op1, dst);
                self.assembler.subss_mr_scaled(
                    address.offset,
                    address.base,
                    address.index,
                    address.scale as _,
                    dst,
                );
            }

            op => unreachable!("{:?}", op),
        }
    }

    pub fn sub_float_rr(&mut self, src: impl Into<Operand>, dst: u8) {
        self.sub_float(dst, src, dst);
    }

    pub fn mul_double(&mut self, op1: u8, op2: impl Into<Operand>, dst: u8) {
        match op2.into() {
            Operand::Register(op2) => {
                if op1 == dst {
                    self.assembler.mulsd_rr(op2, dst);
                    return;
                }

                self.move_double(op2, dst);
                self.assembler.mulsd_rr(op1, dst)
            }

            Operand::Address(op2) => {
                if op1 == dst {
                    self.assembler.mulsd_mr(op2.offset, op2.base, dst);
                    return;
                }

                self.load_double(op2, dst);
                self.assembler.mulsd_rr(op1, dst);
            }

            Operand::BaseIndex(address) => {
                if op1 == dst {
                    self.assembler.mulsd_mr_scaled(
                        address.offset,
                        address.base,
                        address.index,
                        address.scale as _,
                        dst,
                    );
                    return;
                }

                self.load_double(address, dst);
                self.assembler.mulsd_rr(op1, dst);
            }

            op => unreachable!("{:?}", op),
        }
    }

    pub fn mul_double_rr(&mut self, src: impl Into<Operand>, dst: u8) {
        self.mul_double(dst, src, dst);
    }

    pub fn mul_float(&mut self, op1: u8, op2: impl Into<Operand>, dst: u8) {
        match op2.into() {
            Operand::Register(op2) => {
                if op1 == dst {
                    self.assembler.mulss_rr(op2, dst);
                    return;
                }

                self.move_double(op2, dst);
                self.assembler.mulss_rr(op1, dst)
            }

            Operand::Address(op2) => {
                if op1 == dst {
                    self.assembler.mulss_mr(op2.offset, op2.base, dst);
                    return;
                }

                self.load_double(op2, dst);
                self.assembler.mulss_rr(op1, dst);
            }

            Operand::BaseIndex(address) => {
                if op1 == dst {
                    self.assembler.mulss_mr_scaled(
                        address.offset,
                        address.base,
                        address.index,
                        address.scale as _,
                        dst,
                    );
                    return;
                }
            }

            op => unreachable!("{:?}", op),
        }
    }

    pub fn mul_float_rr(&mut self, src: impl Into<Operand>, dst: u8) {
        self.mul_float(dst, src, dst);
    }

    pub fn and_double(&mut self, src1: u8, src2: u8, dst: u8) {
        if src1 == dst {
            self.assembler.andps_rr(src2, dst);
        } else {
            self.move_double(src2, dst);
            self.assembler.andps_rr(src1, dst);
        }
    }

    pub fn and_float(&mut self, src1: u8, src2: u8, dst: u8) {
        if src1 == dst {
            self.assembler.andps_rr(src2, dst);
        } else {
            self.move_double(src2, dst);
            self.assembler.andps_rr(src1, dst);
        }
    }

    pub fn and_double_rr(&mut self, src: u8, dst: u8) {
        self.and_double(dst, src, dst);
    }

    pub fn or_double(&mut self, src1: u8, src2: u8, dst: u8) {
        if src1 == dst {
            self.assembler.orps_rr(src2, dst);
        } else {
            self.move_double(src2, dst);
            self.assembler.orps_rr(src1, dst);
        }
    }

    pub fn or_float(&mut self, src1: u8, src2: u8, dst: u8) {
        if src1 == dst {
            self.assembler.orps_rr(src2, dst);
        } else {
            self.move_double(src2, dst);
            self.assembler.orps_rr(src1, dst);
        }
    }

    pub fn xor_double(&mut self, src1: u8, src2: u8, dst: u8) {
        if src1 == dst {
            self.assembler.xorps_rr(src2, dst);
        } else {
            self.move_double(src2, dst);
            self.assembler.xorps_rr(src1, dst);
        }
    }

    pub fn xor_float(&mut self, src1: u8, src2: u8, dst: u8) {
        if src1 == dst {
            self.assembler.xorps_rr(src2, dst);
        } else {
            self.move_double(src2, dst);
            self.assembler.xorps_rr(src1, dst);
        }
    }

    pub fn convert_int32_to_double(&mut self, src: impl Into<Operand>, dst: u8) {
        match src.into() {
            Operand::Register(src) => self.assembler.cvtsi2sd_rr(src, dst),
            Operand::Imm32(imm) => {
                self.mov(imm, Self::SCRATCH_REGISTER);
                self.assembler.cvtsi2sd_rr(Self::SCRATCH_REGISTER, dst);
            }

            _ => unreachable!(),
        }
    }

    pub fn convert_int32_to_float(&mut self, src: u8, dst: u8) {
        self.assembler.cvtsi2ss_rr(src, dst);
    }

    pub fn branch_double(&mut self, cond: DoubleCondition, left: u8, right: u8) -> Jump {
        if (cond as u8 & DOUBLE_CONDITION_BIT_INVERT) != 0 {
            self.assembler.ucomisd_rr(left, right);
        } else {
            self.assembler.ucomisd_rr(right, left);
        }

        self.jump_after_floating_point_compare(cond, left, right)
    }

    pub fn branch_float(&mut self, cond: DoubleCondition, left: u8, right: u8) -> Jump {
        if (cond as u8 & DOUBLE_CONDITION_BIT_INVERT) != 0 {
            self.assembler.ucomiss_rr(left, right);
        } else {
            self.assembler.ucomiss_rr(right, left);
        }

        self.jump_after_floating_point_compare(cond, left, right)
    }

    pub fn compare_double(&mut self, cond: DoubleCondition, left: u8, right: u8, dest: u8) {
        self.floating_point_compare(cond, left, right, dest, |this, arg1, arg2| {
            this.assembler.ucomisd_rr(arg1, arg2);
        })
    }

    pub fn compare_float(&mut self, cond: DoubleCondition, left: u8, right: u8, dest: u8) {
        self.floating_point_compare(cond, left, right, dest, |this, arg1, arg2| {
            this.assembler.ucomiss_rr(arg1, arg2);
        })
    }

    pub fn load_double(&mut self, address: impl Into<Operand>, dest: u8) {
        match address.into() {
            Operand::AbsoluteAddress(addr) => {
                self.mov(addr, Self::SCRATCH_REGISTER);
                self.load_double(Address::new(Self::SCRATCH_REGISTER, 0), dest);
            }

            Operand::Address(addr) => {
                self.assembler.movsd_mr(addr.offset, addr.base, dest);
            }

            op => unreachable!("{:?}", op),
        }
    }

    pub fn load_float(&mut self, address: impl Into<Operand>, dest: u8) {
        match address.into() {
            Operand::AbsoluteAddress(addr) => {
                self.mov(addr, Self::SCRATCH_REGISTER);
                self.load_float(Address::new(Self::SCRATCH_REGISTER, 0), dest);
            }

            Operand::Address(addr) => {
                self.assembler.movss_mr(addr.offset, addr.base, dest);
            }

            op => unreachable!("{:?}", op),
        }
    }

    pub fn swap(&mut self, r1: u8, r2: u8) {
        if r1 != r2 {
            self.assembler.xchgq_rr(r1, r2);
        }
    }

    pub fn move32_if_needed(&mut self, src: u8, dest: u8) {
        if src == dest {
            return;
        }

        self.assembler.movl_rr(src, dest);
    }

    pub fn pop(&mut self, dest: u8) {
        self.assembler.pop_r(dest);
    }

    pub fn push(&mut self, src: impl Into<Operand>) {
        match src.into() {
            Operand::Register(src) => self.assembler.push_r(src),
            Operand::Imm32(imm) => self.assembler.push_i32(imm),
            Operand::Address(addr) => self.assembler.push_m(addr.offset, addr.base),
            op => unreachable!("{:?}", op),
        }
    }

    pub fn mov(&mut self, src: impl Into<Operand>, dest: impl Into<Operand>) {
        let src = src.into();
        let dest = dest.into();
        match (src, dest) {
            (Operand::Imm32(imm), Operand::Register(dst)) => {
                if imm == 0 {
                    self.assembler.xorl_rr(dst, dst);
                } else {
                    self.assembler.movl_i32r(imm, dst);
                }
            }

            (Operand::Imm32(imm), Operand::Address(dst)) => {
                self.assembler.movl_i32m(imm, dst.offset, dst.base);
            }

            (Operand::Imm32(imm), Operand::BaseIndex(dst)) => {
                self.assembler.movl_i32m_scaled(
                    imm,
                    dst.offset,
                    dst.base,
                    dst.index,
                    dst.scale as _,
                );
            }

            (Operand::Imm64(imm), Operand::Register(dst)) => {
                if imm == 0 {
                    self.assembler.xorq_rr(dst, dst);
                    return;
                }
                self.assembler.movq_i64r(imm, dst);
            }

            (Operand::Register(src), Operand::Register(dst)) => {
                if src != dst {
                    if cfg!(target_pointer_width = "64") {
                        self.assembler.movq_rr(src, dst);
                    } else {
                        self.assembler.movl_rr(src, dst);
                    }
                }
            }

            (Operand::Address(src), Operand::Register(dst)) => {
                if cfg!(target_pointer_width = "64") {
                    self.assembler.movq_mr(src.offset, src.base, dst);
                } else {
                    self.assembler.movl_mr(src.offset, src.base, dst);
                }
            }

            (Operand::BaseIndex(src), Operand::Register(dst)) => {
                if cfg!(target_pointer_width = "64") {
                    self.assembler.movq_mr_scaled(
                        src.offset,
                        src.base,
                        src.index,
                        src.scale as _,
                        dst,
                    );
                } else {
                    self.assembler.movl_mr_scaled(
                        src.offset,
                        src.base,
                        src.index,
                        src.scale as _,
                        dst,
                    );
                }
            }

            (Operand::Register(src), Operand::Address(dst)) => {
                if cfg!(target_pointer_width = "64") {
                    self.assembler.movq_rm(src, dst.offset, dst.base);
                } else {
                    self.assembler.movl_rm(src, dst.offset, dst.base);
                }
            }

            (Operand::Register(src), Operand::BaseIndex(dst)) => {
                if cfg!(target_pointer_width = "64") {
                    self.assembler.movq_rm_scaled(
                        src,
                        dst.offset,
                        dst.base,
                        dst.index,
                        dst.scale as _,
                    );
                } else {
                    self.assembler.movl_rm_scaled(
                        src,
                        dst.offset,
                        dst.base,
                        dst.index,
                        dst.scale as _,
                    );
                }
            }

            (Operand::AbsoluteAddress(addr), Operand::Register(dst)) => {
                self.assembler.movq_i64r(addr.ptr as _, dst);
            }

            _ => unreachable!("Invalid operands for mov: {:?}, {:?}", src, dest),
        }
    }

    pub fn zero_extend32_to_word(&mut self, src: impl Into<Operand>, dest: impl Into<Operand>) {
        let src = src.into();
        let dest = dest.into();

        match (src, dest) {
            (Operand::Imm32(x), Operand::Register(dest)) => {
                self.assembler.movl_i32r(x, dest);
            }

            (Operand::Register(src), Operand::Register(dest)) => {
                self.assembler.movl_rr(src, dest);
            }

            _ => unreachable!(
                "Invalid operands for zero_extend32_to_word: {:?}, {:?}",
                src, dest
            ),
        }
    }

    pub fn sign_extend32_to_64(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Imm32(imm) => {
                if imm == 0 {
                    self.assembler.xorq_rr(dest, dest);
                } else {
                    self.assembler.mov_i32r(imm as _, dest);
                }
            }

            Operand::Register(src) => {
                self.assembler.movsxd_rr(src, dest);
            }

            op => unreachable!("{:?}", op),
        }
    }

    pub fn zero_extend16_to_32(&mut self, src: u8, dest: u8) {
        self.assembler.movzwl_rr(src, dest);
    }

    pub fn sign_extend16_to_32(&mut self, src: u8, dest: u8) {
        self.assembler.movswl_rr(src, dest);
    }

    pub fn sign_extend8_to_64(&mut self, src: u8, dest: u8) {
        self.assembler.movsbq_rr(src, dest);
    }

    pub fn zero_extend16_to_64(&mut self, src: u8, dest: u8) {
        self.zero_extend16_to_32(src, dest)
    }

    pub fn sign_extend16_to_64(&mut self, src: u8, dest: u8) {
        self.assembler.movswq_rr(src, dest);
    }

    pub fn branch8(
        &mut self,
        cond: RelationalCondition,
        left: impl Into<Operand>,
        right: i32,
    ) -> Jump {
        match left.into() {
            Operand::Address(address) => {
                self.assembler
                    .cmpb_im(right as _, address.offset, address.base);
            }

            Operand::BaseIndex(address) => {
                self.assembler.cmpb_im_scaled(
                    right as _,
                    address.offset,
                    address.base,
                    address.index,
                    address.scale as _,
                );
            }

            _ => unreachable!(),
        }

        Jump::new(self.assembler.jcc(cond.x86_condition()))
    }

    pub fn branch32(
        &mut self,
        cond: RelationalCondition,
        left: impl Into<Operand>,
        right: impl Into<Operand>,
    ) -> Jump {
        match (left.into(), right.into()) {
            (Operand::Register(left), Operand::Register(right)) => {
                self.assembler.cmpl_rr(right, left);

                Jump::new(self.assembler.jcc(cond.x86_condition()))
            }

            (Operand::Register(left), Operand::Imm32(right)) => {
                self.assembler.cmpl_ir(right, left);

                Jump::new(self.assembler.jcc(cond.x86_condition()))
            }

            (Operand::Address(address), Operand::Register(right)) => {
                self.assembler.cmpl_rm(right, address.offset, address.base);

                Jump::new(self.assembler.jcc(cond.x86_condition()))
            }

            (Operand::Address(address), Operand::Imm32(imm)) => {
                self.assembler.cmpl_im(imm, address.offset, address.base);

                Jump::new(self.assembler.jcc(cond.x86_condition()))
            }

            (Operand::BaseIndex(address), Operand::Imm32(imm)) => {
                self.assembler.cmpl_im_scaled(
                    imm,
                    address.offset,
                    address.base,
                    address.index,
                    address.scale as _,
                );

                Jump::new(self.assembler.jcc(cond.x86_condition()))
            }

            (Operand::AbsoluteAddress(left), Operand::Register(right)) => {
                self.load32(left, Self::SCRATCH_REGISTER);
                self.branch32(cond, Self::SCRATCH_REGISTER, right)
            }

            _ => unreachable!(),
        }
    }

    pub fn branch32_with_unaligned_half_words(
        &mut self,
        cond: RelationalCondition,
        address: BaseIndex,
        right: i32,
    ) -> Jump {
        self.branch32(cond, address, right)
    }

    pub fn branch_test32(
        &mut self,
        cond: ResultCondition,
        reg: impl Into<Operand>,
        mask: impl Into<Operand>,
    ) -> Jump {
        match (reg.into(), mask.into()) {
            (Operand::Register(reg), Operand::Register(mask)) => {
                self.assembler.testl_rr(reg, mask);

                return Jump::new(self.assembler.jcc(cond.x86_condition()));
            }

            (Operand::Register(reg), Operand::Imm32(imm)) => {
                self.test32(reg, imm);
                Jump::new(self.assembler.jcc(cond.x86_condition()))
            }

            (Operand::Address(address), Operand::Imm32(imm)) => {
                self.generate_test32(address, imm);
                Jump::new(self.assembler.jcc(cond.x86_condition()))
            }
            (Operand::BaseIndex(address), Operand::Imm32(imm)) => {
                if imm == -1 {
                    self.assembler.cmpl_im_scaled(
                        0,
                        address.offset,
                        address.base,
                        address.index,
                        address.scale as _,
                    );
                } else {
                    self.assembler.testl_i32m_scaled(
                        imm,
                        address.offset,
                        address.base,
                        address.index,
                        address.scale as _,
                    );
                }

                return Jump::new(self.assembler.jcc(cond.x86_condition()));
            }

            (Operand::AbsoluteAddress(address), Operand::Imm32(imm)) => {
                self.mov(address, Self::SCRATCH_REGISTER);
                return self.branch_test32(cond, Self::SCRATCH_REGISTER, imm);
            }

            (r, mask) => unreachable!("Invalid operands for branch_test32: {:?}, {:?}", r, mask),
        }
    }

    pub fn branch_test8(
        &mut self,
        cond: ResultCondition,
        op: impl Into<Operand>,
        mask: i32,
    ) -> Jump {
        match op.into() {
            Operand::Address(address) => {
                if mask == -1 {
                    self.assembler.cmpl_im(0, address.offset, address.base);
                } else {
                    self.assembler
                        .testb_im(mask as _, address.offset, address.base);
                }

                return Jump::new(self.assembler.jcc(cond.x86_condition()));
            }

            Operand::BaseIndex(address) => {
                if mask == -1 {
                    self.assembler.cmpl_im_scaled(
                        0,
                        address.offset,
                        address.base,
                        address.index,
                        address.scale as _,
                    );
                } else {
                    self.assembler.testb_im_scaled(
                        mask as _,
                        address.offset,
                        address.base,
                        address.index,
                        address.scale as _,
                    );
                }

                return Jump::new(self.assembler.jcc(cond.x86_condition()));
            }

            Operand::ExtendedAddress(address) => {
                self.mov(
                    AbsoluteAddress::new(address.offset as _),
                    Self::SCRATCH_REGISTER,
                );
                return self.branch_test8(
                    cond,
                    BaseIndex::new(
                        Self::SCRATCH_REGISTER,
                        address.base,
                        Scale::TimesOne,
                        0,
                        Extend::None,
                    ),
                    mask,
                );
            }

            Operand::AbsoluteAddress(address) => {
                self.mov(address, Self::SCRATCH_REGISTER);
                return self.branch_test8(cond, Address::new(Self::SCRATCH_REGISTER, 0), mask);
            }

            op => unreachable!("Invalid operand for branch_test8: {:?}", op),
        }
    }

    pub fn branch_test16(
        &mut self,
        cond: ResultCondition,
        op: impl Into<Operand>,
        mask: i32,
    ) -> Jump {
        match op.into() {
            Operand::Address(address) => {
                if mask == -1 {
                    self.assembler.cmpl_im(0, address.offset, address.base);
                } else {
                    self.assembler
                        .testw_im(mask as _, address.offset, address.base);
                }

                return Jump::new(self.assembler.jcc(cond.x86_condition()));
            }

            Operand::BaseIndex(address) => {
                if mask == -1 {
                    self.assembler.cmpl_im_scaled(
                        0,
                        address.offset,
                        address.base,
                        address.index,
                        address.scale as _,
                    );
                } else {
                    self.assembler.testw_im_scaled(
                        mask as _,
                        address.offset,
                        address.base,
                        address.index,
                        address.scale as _,
                    );
                }

                return Jump::new(self.assembler.jcc(cond.x86_condition()));
            }

            Operand::ExtendedAddress(address) => {
                self.mov(
                    AbsoluteAddress::new(address.offset as _),
                    Self::SCRATCH_REGISTER,
                );
                return self.branch_test16(
                    cond,
                    BaseIndex::new(
                        Self::SCRATCH_REGISTER,
                        address.base,
                        Scale::TimesOne,
                        0,
                        Extend::None,
                    ),
                    mask,
                );
            }

            Operand::AbsoluteAddress(address) => {
                self.mov(address, Self::SCRATCH_REGISTER);
                return self.branch_test16(cond, Address::new(Self::SCRATCH_REGISTER, 0), mask);
            }

            op => unreachable!("Invalid operand for branch_test16: {:?}", op),
        }
    }

    pub fn branch_test_bit32(
        &mut self,
        cond: ResultCondition,
        test_value: impl Into<Operand>,
        bit: impl Into<Operand>,
    ) -> Jump {
        match (test_value.into(), bit.into()) {
            (Operand::Register(reg), Operand::Imm32(bit)) => {
                self.assembler.bt_ir(bit % 32, reg);
                if cond == ResultCondition::NotZero {
                    return Jump::new(self.assembler.jb());
                } else if cond == ResultCondition::Zero {
                    return Jump::new(self.assembler.jae());
                } else {
                    unreachable!("Invalid condition for branch_test_bit32: {:?}", cond);
                }
            }

            (Operand::Address(test_value), Operand::Imm32(bit)) => {
                self.assembler
                    .bt_im(bit % 32, test_value.offset, test_value.base);
                if cond == ResultCondition::NotZero {
                    return Jump::new(self.assembler.jb());
                } else if cond == ResultCondition::Zero {
                    return Jump::new(self.assembler.jae());
                } else {
                    unreachable!("Invalid condition for branch_test_bit32: {:?}", cond);
                }
            }

            (Operand::Register(reg), Operand::Register(bit)) => {
                self.assembler.bt_rr(bit, reg);

                if cond == ResultCondition::NotZero {
                    return Jump::new(self.assembler.jb());
                } else if cond == ResultCondition::Zero {
                    return Jump::new(self.assembler.jae());
                } else {
                    unreachable!("Invalid condition for branch_test_bit32: {:?}", cond);
                }
            }

            (test_value, bit) => unreachable!(
                "Invalid operands for branch_test_bit32: {:?}, {:?}",
                test_value, bit
            ),
        }
    }

    pub fn branch(&mut self, cond: ResultCondition) -> Jump {
        Jump::new(self.assembler.jcc(cond.x86_condition()))
    }

    pub fn jump(&mut self) -> Jump {
        Jump::new(self.assembler.jmp())
    }

    pub fn far_jump(&mut self, target: impl Into<Operand>) {
        match target.into() {
            Operand::Register(r) => {
                self.assembler.jmp_r(r);
            }

            Operand::AbsoluteAddress(addr) => {
                self.mov(addr, Self::SCRATCH_REGISTER);
                self.assembler.jmp_r(Self::SCRATCH_REGISTER);
            }

            Operand::Address(addr) => {
                self.assembler.jmp_m(addr.offset, addr.base);
            }

            Operand::BaseIndex(addr) => {
                self.assembler
                    .jmp_m_scaled(addr.offset, addr.base, addr.index, addr.scale as _);
            }

            target => unreachable!("Invalid operand for far_jump: {:?}", target),
        }
    }

    pub fn branch_add32(
        &mut self,
        cond: ResultCondition,
        src: impl Into<Operand>,
        dst: impl Into<Operand>,
    ) -> Jump {
        self.add32(src, dst);
        Jump::new(self.assembler.jcc(cond.x86_condition()))
    }

    pub fn branch_add32_rrr(
        &mut self,
        cond: ResultCondition,
        a: impl Into<Operand>,
        b: impl Into<Operand>,
        dest: u8,
    ) -> Jump {
        match (a.into(), b.into()) {
            (Operand::Register(src1), Operand::Register(src2)) => {
                if src1 == dest {
                    return self.branch_add32(cond, src2, dest);
                }

                self.move32_if_needed(src2, dest);
                self.branch_add32(cond, src1, dest)
            }

            (Operand::Address(op1), Operand::Register(op2))
            | (Operand::Register(op2), Operand::Address(op1)) => {
                if op2 == dest {
                    return self.branch_add32(cond, op1, dest);
                }

                if op1.base == dest {
                    self.load32(op1, dest);
                    return self.branch_add32(cond, op2, dest);
                }

                self.zero_extend32_to_word(op2, dest);
                self.branch_add32(cond, op1, dest)
            }

            (Operand::Register(src), Operand::Imm32(imm)) => {
                self.move32_if_needed(src, dest);
                self.branch_add32(cond, imm, dest)
            }

            (a, b) => unreachable!("Invalid operands for branch_add32_rrr: {:?}, {:?}", a, b),
        }
    }

    pub fn branch_mul32(
        &mut self,
        cond: ResultCondition,
        src: impl Into<Operand>,
        dest: u8,
    ) -> Jump {
        match src.into() {
            Operand::Register(src) => {
                self.mul32(src, dest);
                if cond != ResultCondition::Overflow {
                    self.assembler.testl_rr(dest, dest);
                }

                Jump::new(self.assembler.jcc(cond.x86_condition()))
            }

            Operand::Address(src) => {
                self.mul32(src, dest);
                if cond != ResultCondition::Overflow {
                    self.assembler.testl_rr(dest, dest);
                }

                Jump::new(self.assembler.jcc(cond.x86_condition()))
            }

            src => unreachable!("Invalid operand for branch_mul32: {:?}", src),
        }
    }

    pub fn branch_mul32_rrr(
        &mut self,
        cond: ResultCondition,
        src1: impl Into<Operand>,
        src2: impl Into<Operand>,
        dest: u8,
    ) -> Jump {
        match (src1.into(), src2.into()) {
            (Operand::Register(src), Operand::Imm32(imm)) => {
                self.mul32_rrr(src, imm, dest);
                if cond != ResultCondition::Overflow {
                    self.assembler.testl_rr(dest, dest);
                }

                Jump::new(self.assembler.jcc(cond.x86_condition()))
            }

            (Operand::Register(src), Operand::Register(src2)) => {
                self.mul32_rrr(src, src2, dest);
                if cond != ResultCondition::Overflow {
                    self.assembler.testl_rr(dest, dest);
                }

                Jump::new(self.assembler.jcc(cond.x86_condition()))
            }

            (src1, src2) => unreachable!(
                "Invalid operands for branch_mul32_rrr: {:?}, {:?}",
                src1, src2
            ),
        }
    }

    pub fn branch_sub32(
        &mut self,
        cond: ResultCondition,
        src: impl Into<Operand>,
        dest: impl Into<Operand>,
    ) -> Jump {
        self.sub32(src, dest);
        Jump::new(self.assembler.jcc(cond.x86_condition()))
    }

    pub fn branch_sub32_rrr(
        &mut self,
        cond: ResultCondition,
        src1: impl Into<Operand>,
        src2: impl Into<Operand>,
        dest: u8,
    ) -> Jump {
        self.sub32_rrr(src1, src2, dest);
        Jump::new(self.assembler.jcc(cond.x86_condition()))
    }

    pub fn branch_neg32(&mut self, cond: ResultCondition, src_dest: u8) -> Jump {
        self.neg32(src_dest);
        Jump::new(self.assembler.jcc(cond.x86_condition()))
    }

    pub fn branch_or32(
        &mut self,
        cond: ResultCondition,
        src: impl Into<Operand>,
        dest: impl Into<Operand>,
    ) -> Jump {
        self.or32(src, dest);
        Jump::new(self.assembler.jcc(cond.x86_condition()))
    }

    pub fn breakpoint(&mut self) {
        self.assembler.int3();
    }

    pub fn is_breakpoint(&self, addr: &u8) -> bool {
        X86Assembler::is_int3(addr)
    }

    pub fn near_tail_call(&mut self) -> Call {
        Call::new(self.assembler.jmp(), Call::LINKABLE_NEAR_TAIL)
    }

    pub fn near_call(&mut self) -> Call {
        Call::new(self.assembler.call(), Call::LINKABLE_NEAR)
    }

    pub fn call_op(&mut self, op: Option<impl Into<Operand>>) -> Option<Call> {
        match op {
            Some(op) => match op.into() {
                Operand::Register(r) => Some(Call::new(self.assembler.call_r(r), Call::NONE)),

                Operand::Address(a) => {
                    self.assembler.call_m(a.offset, a.base);
                    None
                }

                Operand::AbsoluteAddress(a) => {
                    self.mov(a, Self::SCRATCH_REGISTER);
                    let label = self.assembler.call_r(Self::SCRATCH_REGISTER);
                    Some(Call::new(label, Call::NONE))
                }
                op => unreachable!("Invalid operand for call: {:?}", op),
            },
            None => todo!(),
        }
    }

    pub fn compare8(&mut self, cond: RelationalCondition, left: Address, right: i32, dest: u8) {
        self.assembler.cmpb_im(right, left.offset, left.base);
        self.set32(cond.x86_condition(), dest);
    }

    pub fn compare32(
        &mut self,
        cond: RelationalCondition,
        left: impl Into<Operand>,
        right: impl Into<Operand>,
        dest: u8,
    ) {
        match (left.into(), right.into()) {
            (Operand::Register(left), Operand::Register(right)) => {
                self.assembler.cmpl_rr(right, left);
                self.set32(cond.x86_condition(), dest);
            }

            (Operand::Register(left), Operand::Address(right)) => {
                self.assembler.cmpl_mr(right.offset, right.base, left);
                self.set32(cond.x86_condition(), dest);
            }

            (Operand::Register(left), Operand::Imm32(right)) => {
                if right == 0 {
                    if let Some(result_conditon) = self.compute_compare_to_zero_into_test(cond) {
                        self.test32_cond(result_conditon, left, left, dest);
                        return;
                    }
                }
                self.assembler.cmpl_ir(right, left);
                self.set32(cond.x86_condition(), dest);
            }

            (Operand::Address(left), Operand::Register(right)) => {
                self.assembler.cmpl_rm(right, left.offset, left.base);
                self.set32(cond.x86_condition(), dest);
            }

            (Operand::Address(left), Operand::Imm32(right)) => {
                self.assembler.cmpl_im(right, left.offset, left.base);
                self.set32(cond.x86_condition(), dest);
            }

            (left, right) => {
                unreachable!("Invalid operands for compare32: {:?}, {:?}", left, right)
            }
        }
    }

    pub fn xchg8(&mut self, reg: u8, address: impl Into<Operand>) {
        match address.into() {
            Operand::Address(a) => {
                self.assembler.xchgb_rm(reg, a.offset, a.base);
            }
            Operand::BaseIndex(bi) => {
                self.assembler
                    .xchgb_rm_scaled(reg, bi.offset, bi.base, bi.index, bi.scale as _);
            }
            op => unreachable!("Invalid operand for xchg8: {:?}", op),
        }
    }

    pub fn xchg16(&mut self, reg: u8, address: impl Into<Operand>) {
        match address.into() {
            Operand::Address(a) => {
                self.assembler.xchgw_rm(reg, a.offset, a.base);
            }
            Operand::BaseIndex(bi) => {
                self.assembler
                    .xchgw_rm_scaled(reg, bi.offset, bi.base, bi.index, bi.scale as _);
            }
            op => unreachable!("Invalid operand for xchg16: {:?}", op),
        }
    }

    pub fn xchg32(&mut self, reg: u8, address: impl Into<Operand>) {
        match address.into() {
            Operand::Address(a) => {
                self.assembler.xchgl_rm(reg, a.offset, a.base);
            }
            Operand::BaseIndex(bi) => {
                self.assembler
                    .xchgl_rm_scaled(reg, bi.offset, bi.base, bi.index, bi.scale as _);
            }
            op => unreachable!("Invalid operand for xchg32: {:?}", op),
        }
    }

    pub fn load_from_tls32(&mut self, offset: u32, dst: u8) {
        self.assembler.gs();
        self.assembler.movl_mr_addr(offset, dst);
    }

    pub fn store_to_tls32(&mut self, offset: u32, src: u8) {
        self.assembler.gs();
        self.assembler.movl_rm_addr(src, offset);
    }

    /// We take memoryFence to mean AcqRel. This has acqrel semantics on x86.
    pub fn memory_fence(&mut self) {
        // lock; orl $0, (%rsp)
        self.assembler.lock();
        self.assembler.orl_im(0, 0, esp);
    }

    pub fn jump_after_floating_point_compare(
        &mut self,
        cond: DoubleCondition,
        left: u8,
        right: u8,
    ) -> Jump {
        if cond == DoubleCondition::EqualAndOrdered {
            if left == right {
                return Jump::new(self.assembler.jnp());
            }

            let is_unordered = Jump::new(self.assembler.jp());
            let result = Jump::new(self.assembler.je());
            is_unordered.link(self);
            return result;
        }

        if cond == DoubleCondition::NotEqualOrUnordered {
            if left == right {
                return Jump::new(self.assembler.jp());
            }

            let is_unordered = Jump::new(self.assembler.jp());
            let is_equal = Jump::new(self.assembler.je());
            is_unordered.link(self);
            let result = self.jump();
            is_equal.link(self);
            return result;
        }

        return Jump::new(self.assembler.jcc(unsafe {
            std::mem::transmute::<u8, Condition>(cond as u8 & !DOUBLE_CONDITION_BITS)
        }));
    }

    pub fn set32(&mut self, cond: super::x86assembler::Condition, dest: u8) {
        self.assembler.setcc_r(cond, dest);
        self.assembler.movzbl_rr(dest, dest);
    }

    fn floating_point_compare(
        &mut self,
        cond: DoubleCondition,
        left: u8,
        right: u8,
        dest: u8,
        compare: impl FnOnce(&mut Self, u8, u8),
    ) {
        if (cond as u8 & DOUBLE_CONDITION_BIT_SPECIAL) != 0 {
            if cond == DoubleCondition::EqualAndOrdered {
                if left == right {
                    compare(self, right, left);
                    self.set32(super::x86assembler::Condition::NP, dest);
                    return;
                }

                self.mov(0i32, dest);
                compare(self, right, left);
                let is_unordered = Jump::new(self.assembler.jp());
                self.set32(super::x86assembler::Condition::E, dest);
                is_unordered.link(self);
                return;
            }

            if cond == DoubleCondition::NotEqualOrUnordered {
                if left == right {
                    compare(self, right, left);
                    self.set32(super::x86assembler::Condition::P, dest);
                    return;
                }

                self.mov(1i32, dest);
                compare(self, right, left);
                let is_unordered = Jump::new(self.assembler.jp());
                self.set32(super::x86assembler::Condition::E, dest);
                is_unordered.link(self);
                return;
            }

            unreachable!()
        }

        if (cond as u8 & DOUBLE_CONDITION_BIT_INVERT) != 0 {
            compare(self, left, right);
        } else {
            compare(self, right, left);
        }

        self.set32(
            unsafe { std::mem::transmute::<u8, Condition>(cond as u8 & !DOUBLE_CONDITION_BITS) },
            dest,
        );
    }

    pub fn compute_compare_to_zero_into_test(
        &mut self,
        cond: RelationalCondition,
    ) -> Option<ResultCondition> {
        match cond {
            RelationalCondition::Equal => Some(ResultCondition::Zero),
            RelationalCondition::NotEqual => Some(ResultCondition::NotZero),
            RelationalCondition::LessThan => Some(ResultCondition::Signed),
            RelationalCondition::GreaterThanOrEqual => Some(ResultCondition::PositiveOrZero),
            _ => None,
        }
    }

    pub fn test32(&mut self, reg: u8, mask: i32) {
        if mask == -1 {
            self.assembler.testl_rr(reg, reg);
        } else if (mask & !0xff) == 0 && reg < esp {
            if mask == 0xff {
                self.assembler.testb_rr(reg, reg);
            } else {
                self.assembler.testb_i8r(mask as _, reg);
            }
        } else {
            self.assembler.testl_i32r(mask, reg);
        }
    }

    pub fn set_carry(&mut self, dest: u8) {
        self.set32(super::x86assembler::Condition::C, dest);
    }

    pub fn invert(cond: RelationalCondition) -> RelationalCondition {
        unsafe { std::mem::transmute::<u8, RelationalCondition>(cond as u8 ^ 1) }
    }

    pub fn invert_fp(cond: DoubleCondition) -> DoubleCondition {
        match cond {
            DoubleCondition::EqualAndOrdered => DoubleCondition::NotEqualOrUnordered,
            DoubleCondition::NotEqualAndOrdered => DoubleCondition::EqualOrUnordered,
            DoubleCondition::LessThanAndOrdered => DoubleCondition::GreaterThanOrEqualOrUnordered,
            DoubleCondition::LessThanOrEqualAndOrdered => DoubleCondition::GreaterThanOrUnordered,
            DoubleCondition::GreaterThanAndOrdered => DoubleCondition::LessThanOrEqualOrUnordered,
            DoubleCondition::GreaterThanOrEqualAndOrdered => DoubleCondition::LessThanOrUnordered,
            DoubleCondition::EqualOrUnordered => DoubleCondition::NotEqualAndOrdered,
            DoubleCondition::NotEqualOrUnordered => DoubleCondition::EqualAndOrdered,
            DoubleCondition::LessThanOrUnordered => DoubleCondition::GreaterThanOrEqualAndOrdered,
            DoubleCondition::LessThanOrEqualOrUnordered => DoubleCondition::GreaterThanAndOrdered,
            DoubleCondition::GreaterThanOrUnordered => DoubleCondition::LessThanOrEqualAndOrdered,
            DoubleCondition::GreaterThanOrEqualOrUnordered => DoubleCondition::LessThanAndOrdered,
        }
    }

    pub fn is_invertible(cond: ResultCondition) -> bool {
        match cond {
            ResultCondition::Zero
            | ResultCondition::NotZero
            | ResultCondition::Signed
            | ResultCondition::PositiveOrZero => true,
            _ => false,
        }
    }

    pub fn invert_result(cond: ResultCondition) -> ResultCondition {
        match cond {
            ResultCondition::Zero => ResultCondition::NotZero,
            ResultCondition::NotZero => ResultCondition::Zero,
            ResultCondition::Signed => ResultCondition::PositiveOrZero,
            ResultCondition::PositiveOrZero => ResultCondition::Signed,
            _ => unreachable!(),
        }
    }

    pub fn nop(&mut self) {
        self.assembler.nop();
    }

    pub fn test32_cond(
        &mut self,
        cond: ResultCondition,
        src: impl Into<Operand>,
        mask: impl Into<Operand>,
        dest: u8,
    ) {
        match (src.into(), mask.into()) {
            (Operand::Address(address), Operand::Imm32(mask)) => {
                self.generate_test32(address, mask);
                self.set32(cond.x86_condition(), dest);
            }

            (Operand::Register(left), Operand::Register(right)) => {
                self.assembler.testl_rr(left, right);
                self.set32(cond.x86_condition(), dest);
            }

            (Operand::Register(left), Operand::Imm32(right)) => {
                self.test32(left, right);
                self.set32(cond.x86_condition(), dest);
            }
            _ => todo!(),
        }
    }

    pub fn move_conditionally_double(
        &mut self,
        cond: DoubleCondition,
        left: u8,
        right: u8,
        src: u8,
        dest: u8,
    ) {
        if (cond as u8 & DOUBLE_CONDITION_BIT_INVERT) != 0 {
            self.assembler.ucomisd_rr(left, right);
        } else {
            self.assembler.ucomisd_rr(right, left);
        }

        self.move_conditionally_after_floating_point_compare(cond, left, right, src, dest);
    }

    pub fn move_conditionally_float(
        &mut self,
        cond: DoubleCondition,
        left: u8,
        right: u8,
        src: u8,
        dest: u8,
    ) {
        if (cond as u8 & DOUBLE_CONDITION_BIT_INVERT) != 0 {
            self.assembler.ucomiss_rr(left, right);
        } else {
            self.assembler.ucomiss_rr(right, left);
        }

        self.move_conditionally_after_floating_point_compare(cond, left, right, src, dest);
    }

    pub fn move_conditionally_double_then_else(
        &mut self,
        cond: DoubleCondition,
        left: u8,
        right: u8,
        then_case: u8,
        mut else_case: u8,
        dest: u8,
    ) {
        if then_case != dest && else_case != dest {
            self.mov(else_case, dest);
            else_case = dest;
        }

        let src;
        let cond = if else_case == dest {
            src = then_case;
            cond
        } else {
            src = else_case;
            Self::invert_fp(cond)
        };

        if (cond as u8 & DOUBLE_CONDITION_BIT_INVERT) != 0 {
            self.assembler.ucomisd_rr(left, right);
        } else {
            self.assembler.ucomisd_rr(right, left);
        }

        self.move_conditionally_after_floating_point_compare(cond, left, right, src, dest)
    }

    pub fn move_conditionally_float_then_else(
        &mut self,
        cond: DoubleCondition,
        left: u8,
        right: u8,
        then_case: u8,
        mut else_case: u8,
        dest: u8,
    ) {
        if then_case != dest && else_case != dest {
            self.mov(else_case, dest);
            else_case = dest;
        }

        let src;
        let cond = if else_case == dest {
            src = then_case;
            cond
        } else {
            src = else_case;
            Self::invert_fp(cond)
        };

        if (cond as u8 & DOUBLE_CONDITION_BIT_INVERT) != 0 {
            self.assembler.ucomiss_rr(left, right);
        } else {
            self.assembler.ucomiss_rr(right, left);
        }

        self.move_conditionally_after_floating_point_compare(cond, left, right, src, dest)
    }

    pub fn swap32(&mut self, src: u8, dest: impl Into<Operand>) {
        match dest.into() {
            Operand::Address(address) => self.assembler.xchgl_rm(src, address.offset, address.base),
            Operand::Register(reg) => {
                self.assembler.xchgl_rr(src, reg);
            }
            _ => todo!(),
        }
    }

    pub fn move_conditionally32(
        &mut self,
        cond: RelationalCondition,
        left: u8,
        right: u8,
        src: u8,
        dest: u8,
    ) {
        self.assembler.cmpl_rr(right, left);
        self.cmov(cond.x86_condition(), src, dest);
    }

    pub fn move_conditionally32_then_else(
        &mut self,
        cond: RelationalCondition,
        left: u8,
        right: impl Into<Operand>,
        then_case: u8,
        mut else_case: u8,
        dest: u8,
    ) {
        match right.into() {
            Operand::Register(right) => self.assembler.cmpl_rr(right, left),
            Operand::Imm32(imm) => {
                if imm == 0 {
                    if let Some(cond) = self.compute_compare_to_zero_into_test(cond) {
                        self.move_conditionally_test32_then_else(
                            cond, left, left, then_case, else_case, dest,
                        );
                        return;
                    }
                }
            }

            right => todo!("{:?}", right),
        }

        if then_case != dest && else_case != dest {
            self.mov(else_case, dest);
            else_case = dest;
        }

        if else_case == dest {
            self.cmov(cond.x86_condition(), then_case, dest);
        } else {
            self.cmov(Self::invert(cond).x86_condition(), else_case, dest);
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
            Operand::Register(mask) => {
                self.assembler.testl_rr(test_reg, mask);
                self.cmov(cond.x86_condition(), src, dest);
            }

            Operand::Imm32(imm) => {
                self.test32(test_reg, imm);
                self.cmov(cond.x86_condition(), src, dest);
            }

            op => unreachable!("{:?}", op),
        }
    }

    pub fn move_conditionally_test32_then_else(
        &mut self,
        cond: ResultCondition,
        test_reg: u8,
        mask: impl Into<Operand>,
        then_case: u8,
        mut else_case: u8,
        dest: u8,
    ) {
        match mask.into() {
            Operand::Register(mask) => {
                self.assembler.testl_rr(test_reg, mask);
            }

            Operand::Imm32(imm) => {
                self.test32(test_reg, imm);
            }

            op => unreachable!("{:?}", op),
        }

        if then_case != dest && else_case != dest {
            self.mov(else_case, dest);
            else_case = dest;
        }

        if else_case == dest {
            self.cmov(cond.x86_condition(), then_case, dest);
        } else {
            self.cmov(Self::invert_result(cond).x86_condition(), else_case, dest);
        }
    }

    pub fn cmov(&mut self, cond: Condition, src: u8, dest: u8) {
        self.assembler.cmovq_rr(cond, src, dest)
    }

    #[inline(always)]
    fn generate_test32(&mut self, address: Address, mask: i32) {
        if mask == -1 {
            self.assembler.cmpl_im(0, address.offset, address.base);
        } else if (mask & !0xff) == 0 {
            self.assembler.testb_im(mask, address.offset, address.base)
        } else if (mask & !0xff00) == 0 {
            self.assembler
                .testb_im(mask >> 8, address.offset + 1, address.base);
        } else if (mask & !0xff0000) == 0 {
            self.assembler
                .testb_im(mask >> 16, address.offset + 2, address.base);
        } else if (mask as u32 & !0xff000000u32) == 0 {
            self.assembler
                .testb_im(mask >> 24, address.offset + 3, address.base);
        } else {
            self.assembler
                .testl_i32m(mask, address.offset, address.base);
        }
    }

    fn clz32_after_bsr(&mut self, dst: u8) {
        let src_is_non_zero =
            Jump::new(self.assembler.jcc(ResultCondition::NotZero.x86_condition()));
        self.mov(32i32, dst);
        let skip_non_zero_case = self.jump();
        src_is_non_zero.link(self);
        self.xor32(0x1fi32, dst);
        skip_non_zero_case.link(self);
    }

    fn move_conditionally_after_floating_point_compare(
        &mut self,
        cond: DoubleCondition,
        left: u8,
        right: u8,
        src: u8,
        dst: u8,
    ) {
        if cond == DoubleCondition::EqualAndOrdered {
            if left == right {
                self.assembler.cmovnpq_rr(src, dst);
                return;
            }

            let is_unordered = Jump::new(self.assembler.jp());
            self.assembler.cmoveq_rr(src, dst);
            is_unordered.link(self);
            return;
        }

        if cond == DoubleCondition::EqualOrUnordered {
            if left == right {
                self.assembler.cmovpq_rr(src, dst);
                return;
            }

            self.assembler.cmovpq_rr(src, dst);
            self.assembler.cmovneq_rr(src, dst);
            return;
        }

        self.cmov(
            unsafe { std::mem::transmute::<u8, Condition>(cond as u8 & !DOUBLE_CONDITION_BITS) },
            src,
            dst,
        );
    }
}

impl RelationalCondition {
    pub fn x86_condition(self) -> Condition {
        unsafe { std::mem::transmute::<u8, Condition>(self as u8) }
    }
}

impl ResultCondition {
    pub fn x86_condition(self) -> Condition {
        unsafe { std::mem::transmute::<u8, Condition>(self as u8) }
    }
}

impl StatusCondition {
    pub fn x86_condition(self) -> Condition {
        match self {
            Self::Success => Condition::E,
            Self::Failure => Condition::NE,
        }
    }
}
