use std::{
    f32::consts::E,
    ops::{Deref, DerefMut},
};

use num_traits::ops::inv;

use super::{abstract_macro_assembler::*, buffer::AssemblerLabel, riscv64assembler::*};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum RelationalCondition {
    Equal = Condition::EQ as u8,
    NotEqual = Condition::NE as u8,
    Above = Condition::GTU as u8,
    AboveOrEqual = Condition::GEU as u8,
    Below = Condition::LTU as u8,
    BelowOrEqual = Condition::LEU as u8,
    GreaterThan = Condition::GT as u8,
    GreaterThanOrEqual = Condition::GE as u8,
    LessThan = Condition::LT as u8,
    LessThanOrEqual = Condition::LE as u8,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum ResultCondition {
    Overflow,
    Signed,
    PositiveOrZero,
    Zero,
    NonZero,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum ZeroCondition {
    Zero,
    NonZero,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum DoubleCondition {
    EqualAndOrdered,
    NotEqualAndOrdered,
    GreaterThanAndOrdered,
    GreaterThanOrEqualAndOrdered,
    LessThanAndOrdered,
    LessThanOrEqualAndOrdered,
    EqualOrUnordered,
    NotEqualOrUnordered,
    GreaterThanOrUnordered,
    GreaterThanOrEqualOrUnordered,
    LessThanOrUnordered,
    LessThanOrEqualOrUnordered,
}

pub struct MacroAssemblerRISCV64 {
    pub base: AbstractMacroAssembler,
}

impl Deref for MacroAssemblerRISCV64 {
    type Target = AbstractMacroAssembler;

    fn deref(&self) -> &Self::Target {
        &self.base
    }
}

impl DerefMut for MacroAssemblerRISCV64 {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.base
    }
}

impl MacroAssemblerRISCV64 {
    pub fn new() -> Self {
        Self {
            base: AbstractMacroAssembler::new(),
        }
    }

    pub const NUM_GPRS: usize = 32;
    pub const NUM_FPRS: usize = 32;

    pub const NEAR_JUMP_RANGE: usize = 2 * 1024 * 1024 * 1024;

    pub const DATA_TEMP_REGISTER: u8 = x30;
    pub const MEMORY_TEMP_REGISTER: u8 = x31;

    pub const FP_TEMP_REGISTER: u8 = f30;
    pub const FP_TEMP_REGISTER2: u8 = f31;

    pub const STACK_POINTER_REGISTER: u8 = sp;
    pub const FRAME_POINTER_REGISTER: u8 = fp;
    pub const LINK_REGISTER: u8 = ra;

    pub const SCRATCH_REGISTER: u8 = Self::DATA_TEMP_REGISTER;

    pub fn nop(&mut self) {
        self.assembler.addi(zero, zero, 0);
    }

    pub fn invert(cond: RelationalCondition) -> RelationalCondition {
        let cond: Condition = unsafe { std::mem::transmute(cond as u8) };

        let cond = cond.invert();

        unsafe { std::mem::transmute(cond as u8) }
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

    pub fn invert_result(cond: ResultCondition) -> ResultCondition {
        match cond {
            ResultCondition::Zero => ResultCondition::NonZero,
            ResultCondition::NonZero => ResultCondition::Zero,
            ResultCondition::Signed => ResultCondition::PositiveOrZero,
            ResultCondition::PositiveOrZero => ResultCondition::Signed,
            _ => unreachable!(),
        }
    }

    pub fn is_invertible(cond: ResultCondition) -> bool {
        match cond {
            ResultCondition::Zero
            | ResultCondition::NonZero
            | ResultCondition::Signed
            | ResultCondition::PositiveOrZero => true,
            _ => false,
        }
    }

    pub fn add32(&mut self, src: impl Into<Operand>, dst: impl Into<Operand>) {
        match (src.into(), dst.into()) {
            (Operand::Register(src), Operand::Register(dst)) => {
                self.add32_rrr(src, dst, dst);
            }

            (Operand::Imm32(imm), Operand::Register(dst)) => {
                self.add32_rrr(imm, dst, dst);
            }

            (Operand::Imm32(imm), Operand::AbsoluteAddress(address)) => {
                self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);

                if IImmediate::is_valid(imm) {
                    self.assembler
                        .lw(Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, 0);
                    self.assembler
                        .addi(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, imm);
                    self.assembler
                        .sw(Self::MEMORY_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 0);
                    return;
                }

                self.assembler
                    .lw(Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, 0);
                self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                self.assembler.add(
                    Self::DATA_TEMP_REGISTER,
                    Self::MEMORY_TEMP_REGISTER,
                    Self::DATA_TEMP_REGISTER,
                );

                self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                self.assembler
                    .sw(Self::MEMORY_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 0);
            }

            (Operand::Imm32(imm), Operand::Address(address)) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);

                if IImmediate::is_valid(imm) {
                    self.assembler
                        .lw(Self::DATA_TEMP_REGISTER, resolution.base, resolution.offset);
                    self.assembler
                        .addi(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, imm);
                    self.assembler
                        .sw(resolution.base, Self::DATA_TEMP_REGISTER, resolution.offset);
                    return;
                }

                self.assembler.lw(
                    Self::MEMORY_TEMP_REGISTER,
                    resolution.base,
                    resolution.offset,
                );
                self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                self.assembler.add(
                    Self::DATA_TEMP_REGISTER,
                    Self::MEMORY_TEMP_REGISTER,
                    Self::DATA_TEMP_REGISTER,
                );

                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler
                    .sw(resolution.base, Self::DATA_TEMP_REGISTER, resolution.offset);
            }

            (Operand::Address(address), Operand::Register(dest)) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler
                    .lw(Self::DATA_TEMP_REGISTER, resolution.base, resolution.offset);
                self.assembler.addw(dest, Self::DATA_TEMP_REGISTER, zero);
                //self.assembler.mask_register(dest, dest, 32);
            }

            op => panic!("{:?}", op),
        }
    }

    pub fn add32_rrr(
        &mut self,
        lhs: impl Into<Operand>,
        rhs: impl Into<Operand>,
        dest: impl Into<Operand>,
    ) {
        let lhs = lhs.into();
        let rhs = rhs.into();
        let dest = dest.into();

        match (lhs, rhs, dest) {
            (Operand::Register(lhs), Operand::Register(rhs), Operand::Register(dst)) => {
                self.assembler.addw(dst, lhs, rhs);
                //self.assembler.mask_register(dst, dst, 32);
            }

            (Operand::Imm32(imm), Operand::Register(op2), Operand::Register(dst)) => {
                if IImmediate::is_valid(imm) {
                    self.assembler.addiw(dst, op2, imm as i32);
                    //self.assembler.mask_register(dst, dst, 32);
                    return;
                }

                self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                self.assembler.addw(dst, op2, Self::DATA_TEMP_REGISTER);
                //self.assembler.mask_register(dst, dst, 32);
            }

            _ => unreachable!(),
        }
    }

    pub fn add64(&mut self, src: impl Into<Operand>, dest: impl Into<Operand>) {
        match (src.into(), dest.into()) {
            (Operand::Register(src), Operand::Register(dst)) => {
                self.add64_rrr(src, dst, dst);
            }

            (Operand::Imm32(src), Operand::Register(dst)) => {
                self.add64_rrr(src, dst, dst);
            }

            (Operand::Imm64(src), Operand::Register(dst)) => {
                self.add64_rrr(src, dst, dst);
            }

            (Operand::Imm32(imm), Operand::AbsoluteAddress(address)) => {
                self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);

                if IImmediate::is_valid(imm) {
                    self.assembler
                        .ld(Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, 0);
                    self.assembler
                        .addi(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, imm);
                    self.assembler
                        .sd(Self::MEMORY_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 0);
                    return;
                }

                self.assembler
                    .ld(Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, 0);
                self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                self.assembler.add(
                    Self::DATA_TEMP_REGISTER,
                    Self::MEMORY_TEMP_REGISTER,
                    Self::DATA_TEMP_REGISTER,
                );

                self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                self.assembler
                    .sd(Self::MEMORY_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 0);
            }

            (Operand::Imm32(imm), Operand::Address(address)) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);

                self.assembler
                    .ld(Self::DATA_TEMP_REGISTER, resolution.base, resolution.offset);

                if IImmediate::is_valid(imm) {
                    self.assembler
                        .addi(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, imm);
                    self.assembler
                        .sd(resolution.base, Self::DATA_TEMP_REGISTER, resolution.offset);
                    return;
                }

                self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                self.assembler.add(
                    Self::DATA_TEMP_REGISTER,
                    Self::MEMORY_TEMP_REGISTER,
                    Self::DATA_TEMP_REGISTER,
                );

                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);

                self.assembler
                    .sd(resolution.base, Self::DATA_TEMP_REGISTER, resolution.offset);
            }

            (Operand::AbsoluteAddress(address), Operand::Register(dest)) => {
                self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                self.assembler
                    .ld(Self::MEMORY_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, 0);
                self.assembler.add(dest, Self::MEMORY_TEMP_REGISTER, dest);
            }

            (Operand::Address(address), Operand::Register(dest)) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler
                    .ld(Self::DATA_TEMP_REGISTER, resolution.base, resolution.offset);
                self.assembler.add(dest, Self::DATA_TEMP_REGISTER, dest);
            }

            _ => unreachable!(),
        }
    }

    pub fn add64_rrr(
        &mut self,
        lhs: impl Into<Operand>,
        rhs: impl Into<Operand>,
        dest: impl Into<Operand>,
    ) {
        let lhs = lhs.into();
        let rhs = rhs.into();
        let dest = dest.into();

        match (lhs, rhs, dest) {
            (Operand::Register(lhs), Operand::Register(rhs), Operand::Register(dst)) => {
                self.assembler.add(dst, lhs, rhs);
            }

            (Operand::Imm32(imm), Operand::Register(op2), Operand::Register(dst)) => {
                if IImmediate::is_valid(imm) {
                    self.assembler.addi(dst, op2, imm as i32);
                    return;
                }

                self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                self.assembler.add(dst, op2, Self::DATA_TEMP_REGISTER);
            }

            (Operand::Imm64(imm), Operand::Register(op2), Operand::Register(dst)) => {
                if IImmediate::is_valid(imm) {
                    self.assembler.addi(dst, op2, imm as i32);
                    return;
                }

                self.load_immediate64(imm, Self::DATA_TEMP_REGISTER);
                self.assembler.add(dst, op2, Self::DATA_TEMP_REGISTER);
            }

            _ => unreachable!(),
        }
    }

    pub fn sub32(&mut self, src: impl Into<Operand>, dst: impl Into<Operand>) {
        match (src.into(), dst.into()) {
            (Operand::Register(src), Operand::Register(dst)) => {
                self.sub32_rrr(src, dst, dst);
            }

            (Operand::Imm32(src), Operand::Register(dst)) => {
                self.sub32_rrr(src, dst, dst);
            }

            (Operand::Imm32(imm), Operand::Address(address)) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler
                    .lw(Self::DATA_TEMP_REGISTER, resolution.base, resolution.offset);

                if IImmediate::is_valid(-imm) {
                    self.assembler.addiw(
                        Self::DATA_TEMP_REGISTER,
                        Self::DATA_TEMP_REGISTER,
                        -imm as i32,
                    );
                    self.assembler
                        .sw(resolution.base, Self::DATA_TEMP_REGISTER, resolution.offset);
                    return;
                }

                self.load_immediate32(imm, Self::MEMORY_TEMP_REGISTER);
                self.assembler.subw(
                    Self::DATA_TEMP_REGISTER,
                    Self::DATA_TEMP_REGISTER,
                    Self::MEMORY_TEMP_REGISTER,
                );

                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler
                    .sw(resolution.base, Self::DATA_TEMP_REGISTER, resolution.offset);
            }

            (Operand::Imm32(imm), Operand::AbsoluteAddress(address)) => {
                self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);

                if IImmediate::is_valid(-imm) {
                    self.assembler
                        .lw(Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, 0);
                    self.assembler.addiw(
                        Self::DATA_TEMP_REGISTER,
                        Self::DATA_TEMP_REGISTER,
                        -imm as i32,
                    );
                    self.assembler
                        .sw(Self::MEMORY_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 0);
                } else {
                    self.assembler
                        .lw(Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, 0);
                    self.load_immediate32(imm, Self::MEMORY_TEMP_REGISTER);
                    self.assembler.subw(
                        Self::DATA_TEMP_REGISTER,
                        Self::DATA_TEMP_REGISTER,
                        Self::MEMORY_TEMP_REGISTER,
                    );

                    self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                    self.assembler
                        .sw(Self::MEMORY_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 0);
                }
            }

            (Operand::Address(address), Operand::Register(dest)) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);

                self.assembler
                    .lw(Self::DATA_TEMP_REGISTER, resolution.base, resolution.offset);

                self.assembler.subw(dest, Self::DATA_TEMP_REGISTER, dest);

                //self.assembler.mask_register(dest, dest, 32);
            }

            _ => todo!(),
        }
    }

    pub fn sub32_rrr(&mut self, op1: impl Into<Operand>, op2: impl Into<Operand>, dest: u8) {
        match (op1.into(), op2.into()) {
            (Operand::Register(op1), Operand::Register(op2)) => {
                self.assembler.subw(dest, op1, op2);
                //self.assembler.mask_register(dest, dest, 32);
            }

            (Operand::Register(op1), Operand::Imm32(op2)) => {
                self.add32_rrr(-op2, op1, dest);
            }

            _ => unreachable!(),
        }
    }

    pub fn sub64(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(src) => {
                self.sub64_rrr(dest, src, dest);
            }

            Operand::Imm32(imm) => {
                self.sub64_rrr(dest, imm, dest);
            }

            _ => unreachable!(),
        }
    }

    pub fn sub64_rrr(&mut self, op1: u8, op2: impl Into<Operand>, dest: u8) {
        match op2.into() {
            Operand::Register(op2) => {
                self.assembler.sub(dest, op1, op2);
            }

            Operand::Imm32(op2) => {
                self.add64_rrr(-op2 as i32, op1, dest);
            }

            Operand::Imm64(op2) => self.add64_rrr(-op2, op1, dest),

            _ => unreachable!(),
        }
    }

    pub fn mul32(&mut self, src: u8, dest: u8) {
        self.mul32_rrr(src, dest, dest);
    }

    pub fn mul32_rrr(&mut self, lhs: impl Into<Operand>, rhs: u8, dest: u8) {
        match lhs.into() {
            Operand::Register(lhs) => {
                self.assembler.mulw(dest, lhs, rhs);
                //self.assembler.mask_register(dest, dest, 32);
            }

            Operand::Imm32(imm) => {
                self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                self.assembler.mulw(dest, Self::DATA_TEMP_REGISTER, rhs);
                //self.assembler.mask_register(dest, dest, 32);
            }

            _ => unreachable!(),
        }
    }

    pub fn mul64(&mut self, src: u8, dest: u8) {
        self.mul64_rrr(src, dest, dest);
    }

    pub fn mul64_rrr(&mut self, lhs: u8, rhs: u8, dest: u8) {
        self.assembler.mul(dest, lhs, rhs);
    }

    pub fn count_leading_zeros32(&mut self, src: u8, dest: u8) {
        self.assembler
            .zero_extend(Self::DATA_TEMP_REGISTER, src, 32);
        self.assembler.addi(dest, zero, 32);

        let mut zero_ = JumpList::new();

        zero_.push(self.make_branch(RelationalCondition::Equal, Self::DATA_TEMP_REGISTER, zero));

        let lbl = self.label();
        self.assembler
            .srli(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 1);
        self.assembler.addi(dest, dest, -1);
        zero_.push(self.make_branch(RelationalCondition::Equal, Self::DATA_TEMP_REGISTER, zero));
        self.jump().link_to(self, lbl);

        zero_.link(self);
    }

    pub fn count_leading_zeros64(&mut self, src: u8, dest: u8) {
        self.assembler.addi(Self::DATA_TEMP_REGISTER, zero, 0);
        self.assembler.addi(dest, zero, 64);

        let mut zero_ = JumpList::new();

        zero_.push(self.make_branch(RelationalCondition::Equal, src, zero));

        let lbl = self.label();
        self.assembler.srli(src, src, 1);
        self.assembler.addi(dest, dest, -1);
        zero_.push(self.make_branch(RelationalCondition::Equal, src, zero));
        self.jump().link_to(self, lbl);

        zero_.link(self);
    }

    pub fn count_trailing_zeros32(&mut self, src: u8, dest: u8) {
        self.assembler.addi(dest, zero, 32);
        self.assembler
            .zero_extend(Self::DATA_TEMP_REGISTER, src, 32);

        let mut zero_ = JumpList::new();
        zero_.push(self.make_branch(RelationalCondition::Equal, Self::DATA_TEMP_REGISTER, zero));

        let lbl = self.label();
        self.assembler
            .slli(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 1);
        self.assembler.addi(dest, dest, -1);
        zero_.push(self.make_branch(RelationalCondition::Equal, Self::DATA_TEMP_REGISTER, zero));
        self.jump().link_to(self, lbl);

        zero_.link(self);
    }

    pub fn count_trailing_zeros64(&mut self, src: u8, dest: u8) {
        self.assembler.addi(dest, zero, 64);

        let mut zero_ = JumpList::new();
        zero_.push(self.make_branch(RelationalCondition::Equal, src, zero));

        let lbl = self.label();
        self.assembler.slli(src, src, 1);
        self.assembler.addi(dest, dest, -1);
        zero_.push(self.make_branch(RelationalCondition::Equal, src, zero));
        self.jump().link_to(self, lbl);

        zero_.link(self);
    }

    pub fn byte_swap16(&mut self, reg: u8) {
        self.assembler.andi(Self::DATA_TEMP_REGISTER, reg, 0xff);
        self.assembler
            .slli(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 8);
        self.assembler.andi(reg, reg, 48);
        self.assembler.andi(reg, reg, 56);
        self.assembler
            .or(reg, Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER);
    }

    pub fn byte_swap32(&mut self, reg: u8) {
        self.assembler.andi(Self::DATA_TEMP_REGISTER, reg, 0xff);
        self.assembler
            .slli(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 8);
        self.assembler.srli(reg, reg, 8);

        for i in 0..2 {
            self.assembler.andi(Self::MEMORY_TEMP_REGISTER, reg, 0xff);
            self.assembler.or(
                Self::DATA_TEMP_REGISTER,
                Self::DATA_TEMP_REGISTER,
                Self::MEMORY_TEMP_REGISTER,
            );
            self.assembler
                .slli(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 8);
            self.assembler.srli(reg, reg, 8);
        }

        self.assembler.andi(Self::MEMORY_TEMP_REGISTER, reg, 0xff);
        self.assembler
            .or(reg, Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER);
    }

    pub fn byte_swap64(&mut self, reg: u8) {
        self.assembler.andi(Self::DATA_TEMP_REGISTER, reg, 0xff);
        self.assembler
            .slli(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 8);
        self.assembler.srli(reg, reg, 8);

        for i in 0..6 {
            self.assembler.andi(Self::MEMORY_TEMP_REGISTER, reg, 0xff);
            self.assembler.or(
                Self::DATA_TEMP_REGISTER,
                Self::DATA_TEMP_REGISTER,
                Self::MEMORY_TEMP_REGISTER,
            );
            self.assembler
                .slli(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 8);
            self.assembler.srli(reg, reg, 8);
        }

        self.assembler.andi(Self::MEMORY_TEMP_REGISTER, reg, 0xff);
        self.assembler
            .or(reg, Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER);
    }

    pub fn lshift32(&mut self, shift_amount: impl Into<Operand>, dest: u8) {
        self.lshift32_rrr(dest, shift_amount, dest);
    }

    pub fn lshift32_rrr(
        &mut self,
        src: impl Into<Operand>,
        shift_amount: impl Into<Operand>,
        dest: u8,
    ) {
        match src.into() {
            Operand::Register(src) => match shift_amount.into() {
                Operand::Register(shift_amount) => {
                    self.assembler.sllw(dest, src, shift_amount);
                    //self.assembler.mask_register(dest, dest, 32);
                }

                Operand::Imm32(shift_amount) => {
                    self.assembler
                        .slliw(dest, src, (shift_amount & ((1 << 6) - 1)) as u32);
                    //self.assembler.mask_register(dest, dest, 32);
                }

                _ => unreachable!(),
            },
            Operand::Address(src) => {
                self.load32(src, Self::DATA_TEMP_REGISTER);
                self.lshift32_rrr(Self::DATA_TEMP_REGISTER, shift_amount, dest);
            }
            _ => unreachable!(),
        }
    }

    pub fn lshift64(&mut self, shift_amount: impl Into<Operand>, dest: u8) {
        self.lshift64_rrr(dest, shift_amount, dest);
    }

    pub fn lshift64_rrr(
        &mut self,
        src: impl Into<Operand>,
        shift_amount: impl Into<Operand>,
        dest: u8,
    ) {
        match src.into() {
            Operand::Register(src) => match shift_amount.into() {
                Operand::Register(shift_amount) => {
                    self.assembler.sll(dest, src, shift_amount);
                }

                Operand::Imm32(shift_amount) => {
                    self.assembler
                        .slli(dest, src, (shift_amount & ((1 << 6) - 1)) as u32);
                }

                _ => unreachable!(),
            },

            Operand::Address(src) => {
                self.load64(src, Self::DATA_TEMP_REGISTER);
                self.lshift64_rrr(Self::DATA_TEMP_REGISTER, shift_amount, dest);
            }

            _ => unreachable!(),
        }
    }

    pub fn rshift32(&mut self, shift_amount: impl Into<Operand>, dest: u8) {
        self.rshift32_rrr(dest, shift_amount, dest);
    }

    pub fn rshift32_rrr(&mut self, src: u8, shift_amount: impl Into<Operand>, dest: u8) {
        match shift_amount.into() {
            Operand::Register(shift_amount) => {
                self.assembler.sraw(dest, src, shift_amount);
                //self.assembler.mask_register(dest, dest, 32);
            }

            Operand::Imm32(shift_amount) => {
                self.assembler
                    .sraiw(dest, src, (shift_amount & ((1 << 6) - 1)) as u32);
                //self.assembler.mask_register(dest, dest, 32);
            }

            _ => unreachable!(),
        }
    }

    pub fn urshift32(&mut self, shift_amount: impl Into<Operand>, dest: u8) {
        self.urshift32_rrr(dest, shift_amount, dest);
    }

    pub fn urshift32_rrr(&mut self, src: u8, shift_amount: impl Into<Operand>, dest: u8) {
        match shift_amount.into() {
            Operand::Register(shift_amount) => {
                self.assembler.srlw(dest, src, shift_amount);
                //self.assembler.mask_register(dest, dest, 32);
            }

            Operand::Imm32(shift_amount) => {
                self.assembler
                    .srliw(dest, src, (shift_amount & ((1 << 6) - 1)) as u32);
                //self.assembler.mask_register(dest, dest, 32);
            }

            _ => unreachable!(),
        }
    }

    pub fn rshift64(&mut self, shift_amount: impl Into<Operand>, dest: u8) {
        self.rshift64_rrr(dest, shift_amount, dest);
    }

    pub fn rshift64_rrr(&mut self, src: u8, shift_amount: impl Into<Operand>, dest: u8) {
        match shift_amount.into() {
            Operand::Register(shift_amount) => {
                self.assembler.sra(dest, src, shift_amount);
            }

            Operand::Imm32(shift_amount) => {
                self.assembler
                    .srai(dest, src, (shift_amount & ((1 << 6) - 1)) as u32);
            }

            _ => unreachable!(),
        }
    }

    pub fn urshift64(&mut self, shift_amount: impl Into<Operand>, dest: u8) {
        self.urshift64_rrr(dest, shift_amount, dest);
    }

    pub fn urshift64_rrr(&mut self, src: u8, shift_amount: impl Into<Operand>, dest: u8) {
        match shift_amount.into() {
            Operand::Register(shift_amount) => {
                self.assembler.srl(dest, src, shift_amount);
            }

            Operand::Imm32(shift_amount) => {
                self.assembler
                    .srli(dest, src, (shift_amount & ((1 << 6) - 1)) as u32);
            }

            _ => unreachable!(),
        }
    }

    pub fn load8(&mut self, address: impl Into<Operand>, dest: u8) {
        match address.into() {
            Operand::Address(address) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.lbu(dest, resolution.base, resolution.offset);
            }

            Operand::BaseIndex(address) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.lbu(dest, resolution.base, resolution.offset);
            }

            Operand::AbsoluteAddress(address) => {
                self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                self.assembler.lbu(dest, Self::MEMORY_TEMP_REGISTER, 0);
            }

            Operand::Imm64(imm) => {
                self.load_immediate64(imm, Self::MEMORY_TEMP_REGISTER);
                self.assembler.lbu(dest, Self::MEMORY_TEMP_REGISTER, 0);
            }

            _ => unreachable!(),
        }
    }

    pub fn load8_signed_extend_to_32(&mut self, address: impl Into<Operand>, dest: u8) {
        match address.into() {
            Operand::Address(address) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.lb(dest, resolution.base, resolution.offset);
                //self.assembler.mask_register(dest, dest, 32)
            }

            Operand::BaseIndex(address) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.lb(dest, resolution.base, resolution.offset);
                //self.assembler.mask_register(dest, dest, 32)
            }

            Operand::AbsoluteAddress(address) => {
                self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                self.assembler.lb(dest, Self::MEMORY_TEMP_REGISTER, 0);
                //self.assembler.mask_register(dest, dest, 32)
            }

            Operand::Imm64(imm) => {
                self.load_immediate64(imm, Self::MEMORY_TEMP_REGISTER);
                self.assembler.lb(dest, Self::MEMORY_TEMP_REGISTER, 0);
                //self.assembler.mask_register(dest, dest, 32)
            }

            _ => unreachable!(),
        }
    }

    pub fn load16(&mut self, address: impl Into<Operand>, dest: u8) {
        match address.into() {
            Operand::Address(address) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.lhu(dest, resolution.base, resolution.offset);
            }

            Operand::BaseIndex(address) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.lhu(dest, resolution.base, resolution.offset);
            }

            Operand::AbsoluteAddress(address) => {
                self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                self.assembler.lhu(dest, Self::MEMORY_TEMP_REGISTER, 0);
            }

            Operand::Imm64(imm) => {
                self.load_immediate64(imm, Self::MEMORY_TEMP_REGISTER);
                self.assembler.lhu(dest, Self::MEMORY_TEMP_REGISTER, 0);
            }

            Operand::ExtendedAddress(address) => {
                self.load_immediate64(address.offset as _, Self::MEMORY_TEMP_REGISTER);
                self.assembler
                    .slli(Self::DATA_TEMP_REGISTER, address.base, -1i32 as u32);
                self.assembler.add(
                    Self::MEMORY_TEMP_REGISTER,
                    Self::MEMORY_TEMP_REGISTER,
                    Self::DATA_TEMP_REGISTER,
                );
                self.assembler.lhu(dest, Self::MEMORY_TEMP_REGISTER, 0);
            }

            _ => unreachable!(),
        }
    }

    pub fn load16_signed_extend_to_32(&mut self, address: impl Into<Operand>, dest: u8) {
        match address.into() {
            Operand::Address(address) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.lh(dest, resolution.base, resolution.offset);
                //self.assembler.mask_register(dest, dest, 32)
            }

            Operand::BaseIndex(address) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.lh(dest, resolution.base, resolution.offset);
                //self.assembler.mask_register(dest, dest, 32)
            }

            Operand::AbsoluteAddress(address) => {
                self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                self.assembler.lh(dest, Self::MEMORY_TEMP_REGISTER, 0);
                //self.assembler.mask_register(dest, dest, 32)
            }

            Operand::Imm64(imm) => {
                self.load_immediate64(imm, Self::MEMORY_TEMP_REGISTER);
                self.assembler.lh(dest, Self::MEMORY_TEMP_REGISTER, 0);
                //self.assembler.mask_register(dest, dest, 32)
            }

            Operand::ExtendedAddress(address) => {
                self.load_immediate64(address.offset as _, Self::MEMORY_TEMP_REGISTER);
                self.assembler
                    .slli(Self::DATA_TEMP_REGISTER, address.base, -1i32 as u32);
                self.assembler.add(
                    Self::MEMORY_TEMP_REGISTER,
                    Self::MEMORY_TEMP_REGISTER,
                    Self::DATA_TEMP_REGISTER,
                );
                self.assembler.lh(dest, Self::MEMORY_TEMP_REGISTER, 0);
                //self.assembler.mask_register(dest, dest, 32)
            }

            _ => unreachable!(),
        }
    }

    pub fn load32(&mut self, address: impl Into<Operand>, dest: u8) {
        match address.into() {
            Operand::Address(address) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.lwu(dest, resolution.base, resolution.offset);
            }

            Operand::BaseIndex(address) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.lwu(dest, resolution.base, resolution.offset);
            }

            Operand::AbsoluteAddress(address) => {
                self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                self.assembler.lwu(dest, Self::MEMORY_TEMP_REGISTER, 0);
            }

            Operand::Imm64(imm) => {
                self.load_immediate64(imm, Self::MEMORY_TEMP_REGISTER);
                self.assembler.lwu(dest, Self::MEMORY_TEMP_REGISTER, 0);
            }

            _ => unreachable!(),
        }
    }

    pub fn load16_unaligned(&mut self, address: impl Into<Operand>, dest: u8) {
        self.load16(address, dest);
    }

    pub fn load64(&mut self, address: impl Into<Operand>, dest: u8) {
        match address.into() {
            Operand::Address(address) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.ld(dest, resolution.base, resolution.offset);
            }

            Operand::BaseIndex(address) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.ld(dest, resolution.base, resolution.offset);
            }

            Operand::AbsoluteAddress(address) => {
                self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                self.assembler.ld(dest, Self::MEMORY_TEMP_REGISTER, 0);
            }

            Operand::Imm64(imm) => {
                self.load_immediate64(imm, Self::MEMORY_TEMP_REGISTER);
                self.assembler.ld(dest, Self::MEMORY_TEMP_REGISTER, 0);
            }

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

    pub fn load_pair64(&mut self, src: u8, offset: i32, dest1: u8, dest2: u8) {
        if src == dest1 {
            self.load64(Address::new(src, offset + 8), dest2);
            self.load64(Address::new(src, offset), dest1);
        } else {
            self.load64(Address::new(src, offset), dest1);
            self.load64(Address::new(src, offset + 8), dest2);
        }
    }

    pub fn store8(&mut self, src: impl Into<Operand>, address: impl Into<Operand>) {
        match src.into() {
            Operand::Register(src) => match address.into() {
                Operand::Address(address) => {
                    let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                    self.assembler.sb(resolution.base, src, resolution.offset);
                }

                Operand::BaseIndex(address) => {
                    let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                    self.assembler.sb(resolution.base, src, resolution.offset);
                }

                Operand::AbsoluteAddress(address) => {
                    self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                    self.assembler.sb(Self::MEMORY_TEMP_REGISTER, src, 0);
                }

                Operand::Imm64(imm) => {
                    self.load_immediate64(imm, Self::MEMORY_TEMP_REGISTER);
                    self.assembler.sb(Self::MEMORY_TEMP_REGISTER, src, 0);
                }

                _ => unreachable!(),
            },

            Operand::Imm32(imm) => match address.into() {
                Operand::Address(address) => {
                    let mut imm_register = zero;
                    let imm8 = imm as u8;

                    if imm8 != 0 {
                        self.load_immediate32(imm8 as i32, Self::DATA_TEMP_REGISTER);
                        imm_register = Self::DATA_TEMP_REGISTER;
                    }

                    let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                    self.assembler
                        .sb(resolution.base, imm_register, resolution.offset);
                }

                Operand::BaseIndex(address) => {
                    let mut imm_register = zero;
                    let imm8 = imm as u8;

                    if imm8 != 0 {
                        self.load_immediate32(imm8 as i32, Self::DATA_TEMP_REGISTER);
                        imm_register = Self::DATA_TEMP_REGISTER;
                    }

                    let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                    self.assembler
                        .sb(resolution.base, imm_register, resolution.offset);
                }

                Operand::AbsoluteAddress(address) => {
                    let mut imm_register = zero;
                    let imm8 = imm as u8;

                    if imm8 != 0 {
                        self.load_immediate32(imm8 as i32, Self::DATA_TEMP_REGISTER);
                        imm_register = Self::DATA_TEMP_REGISTER;
                    }

                    self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                    self.assembler
                        .sb(Self::MEMORY_TEMP_REGISTER, imm_register, 0);
                }

                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    pub fn store16(&mut self, src: impl Into<Operand>, address: impl Into<Operand>) {
        match src.into() {
            Operand::Register(src) => match address.into() {
                Operand::Address(address) => {
                    let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                    self.assembler.sh(resolution.base, src, resolution.offset);
                }

                Operand::BaseIndex(address) => {
                    let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                    self.assembler.sh(resolution.base, src, resolution.offset);
                }

                Operand::AbsoluteAddress(address) => {
                    self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                    self.assembler.sh(Self::MEMORY_TEMP_REGISTER, src, 0);
                }

                Operand::Imm64(imm) => {
                    self.load_immediate64(imm, Self::MEMORY_TEMP_REGISTER);
                    self.assembler.sh(Self::MEMORY_TEMP_REGISTER, src, 0);
                }

                _ => unreachable!(),
            },

            Operand::Imm32(imm) => match address.into() {
                Operand::Address(address) => {
                    let mut imm_register = zero;
                    let imm16 = imm as u16;

                    if imm16 != 0 {
                        self.load_immediate32(imm16 as i32, Self::DATA_TEMP_REGISTER);
                        imm_register = Self::DATA_TEMP_REGISTER;
                    }

                    let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                    self.assembler
                        .sh(resolution.base, imm_register, resolution.offset);
                }

                Operand::BaseIndex(address) => {
                    let mut imm_register = zero;
                    let imm16 = imm as u16;

                    if imm16 != 0 {
                        self.load_immediate32(imm16 as i32, Self::DATA_TEMP_REGISTER);
                        imm_register = Self::DATA_TEMP_REGISTER;
                    }

                    let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                    self.assembler
                        .sh(resolution.base, imm_register, resolution.offset);
                }

                Operand::AbsoluteAddress(address) => {
                    let mut imm_register = zero;
                    let imm16 = imm as u16;

                    if imm16 != 0 {
                        self.load_immediate32(imm16 as i32, Self::DATA_TEMP_REGISTER);
                        imm_register = Self::DATA_TEMP_REGISTER;
                    }

                    self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                    self.assembler
                        .sh(Self::MEMORY_TEMP_REGISTER, imm_register, 0);
                }

                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    pub fn store32(&mut self, src: impl Into<Operand>, address: impl Into<Operand>) {
        match src.into() {
            Operand::Register(src) => match address.into() {
                Operand::Address(address) => {
                    let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                    self.assembler.sw(resolution.base, src, resolution.offset);
                }

                Operand::BaseIndex(address) => {
                    let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                    self.assembler.sw(resolution.base, src, resolution.offset);
                }

                Operand::AbsoluteAddress(address) => {
                    self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                    self.assembler.sw(Self::MEMORY_TEMP_REGISTER, src, 0);
                }

                Operand::Imm64(imm) => {
                    self.load_immediate64(imm, Self::MEMORY_TEMP_REGISTER);
                    self.assembler.sw(Self::MEMORY_TEMP_REGISTER, src, 0);
                }

                _ => unreachable!(),
            },

            Operand::Imm32(imm) => match address.into() {
                Operand::Address(address) => {
                    let mut imm_register = zero;

                    if imm != 0 {
                        self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                        imm_register = Self::DATA_TEMP_REGISTER;
                    }

                    let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                    self.assembler
                        .sw(resolution.base, imm_register, resolution.offset);
                }

                Operand::BaseIndex(address) => {
                    let mut imm_register = zero;

                    if imm != 0 {
                        self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                        imm_register = Self::DATA_TEMP_REGISTER;
                    }

                    let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                    self.assembler
                        .sw(resolution.base, imm_register, resolution.offset);
                }

                Operand::AbsoluteAddress(address) => {
                    let mut imm_register = zero;

                    if imm != 0 {
                        self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                        imm_register = Self::DATA_TEMP_REGISTER;
                    }

                    self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                    self.assembler
                        .sw(Self::MEMORY_TEMP_REGISTER, imm_register, 0);
                }

                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    pub fn store64(&mut self, src: impl Into<Operand>, address: impl Into<Operand>) {
        match src.into() {
            Operand::Register(src) => match address.into() {
                Operand::Address(address) => {
                    let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                    self.assembler.sd(resolution.base, src, resolution.offset);
                }

                Operand::BaseIndex(address) => {
                    let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                    self.assembler.sd(resolution.base, src, resolution.offset);
                }

                Operand::AbsoluteAddress(address) => {
                    self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                    self.assembler.sd(Self::MEMORY_TEMP_REGISTER, src, 0);
                }

                Operand::Imm64(imm) => {
                    self.load_immediate64(imm, Self::MEMORY_TEMP_REGISTER);
                    self.assembler.sd(Self::MEMORY_TEMP_REGISTER, src, 0);
                }

                _ => unreachable!(),
            },

            Operand::Imm64(imm) => match address.into() {
                Operand::Address(address) => {
                    let mut imm_register = zero;

                    if imm != 0 {
                        self.load_immediate64(imm, Self::DATA_TEMP_REGISTER);
                        imm_register = Self::DATA_TEMP_REGISTER;
                    }

                    let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                    self.assembler
                        .sd(resolution.base, imm_register, resolution.offset);
                }

                Operand::BaseIndex(address) => {
                    let mut imm_register = zero;

                    if imm != 0 {
                        self.load_immediate64(imm, Self::DATA_TEMP_REGISTER);
                        imm_register = Self::DATA_TEMP_REGISTER;
                    }

                    let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                    self.assembler
                        .sd(resolution.base, imm_register, resolution.offset);
                }

                Operand::AbsoluteAddress(address) => {
                    let mut imm_register = zero;

                    if imm != 0 {
                        self.load_immediate64(imm, Self::DATA_TEMP_REGISTER);
                        imm_register = Self::DATA_TEMP_REGISTER;
                    }

                    self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                    self.assembler
                        .sd(Self::MEMORY_TEMP_REGISTER, imm_register, 0);
                }

                _ => unreachable!(),
            },

            Operand::Imm32(imm) => match address.into() {
                Operand::Address(address) => {
                    let mut imm_register = zero;

                    if imm != 0 {
                        self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                        imm_register = Self::DATA_TEMP_REGISTER;
                    }

                    let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                    self.assembler
                        .sd(resolution.base, imm_register, resolution.offset);
                }

                Operand::BaseIndex(address) => {
                    let mut imm_register = zero;

                    if imm != 0 {
                        self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                        imm_register = Self::DATA_TEMP_REGISTER;
                    }

                    let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                    self.assembler
                        .sd(resolution.base, imm_register, resolution.offset);
                }

                Operand::AbsoluteAddress(address) => {
                    let mut imm_register = zero;

                    if imm != 0 {
                        self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                        imm_register = Self::DATA_TEMP_REGISTER;
                    }

                    self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                    self.assembler
                        .sd(Self::MEMORY_TEMP_REGISTER, imm_register, 0);
                }

                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    pub fn transfer64(&mut self, src: Address, dest: Address) {
        self.load64(src, Self::DATA_TEMP_REGISTER);
        self.store64(Self::DATA_TEMP_REGISTER, dest);
    }

    pub fn transfer_ptr(&mut self, src: Address, dest: Address) {
        self.load64(src, Self::DATA_TEMP_REGISTER);
        self.store64(Self::DATA_TEMP_REGISTER, dest);
    }

    pub fn store_pair32(&mut self, src1: u8, src2: u8, dest: u8, offset: i32) {
        self.store32(src1, Address::new(dest, offset));
        self.store32(src2, Address::new(dest, offset + 4));
    }

    pub fn store_pair64(&mut self, src1: u8, src2: u8, dest: u8, offset: i32) {
        self.store64(src1, Address::new(dest, offset));
        self.store64(src2, Address::new(dest, offset + 8));
    }

    pub fn zero_extend8_to_32(&mut self, src: u8, dest: u8) {
        self.assembler.slli(dest, src, 56);
        self.assembler.srli(dest, dest, 56);
    }

    pub fn zero_extend16_to_32(&mut self, src: u8, dest: u8) {
        self.assembler.slli(dest, src, 48);
        self.assembler.srli(dest, dest, 48);
    }

    pub fn zero_extend32_to_word(&mut self, src: u8, dest: u8) {
        self.assembler.slli(dest, src, 32);
        self.assembler.srli(dest, dest, 32);
    }

    pub fn sign_extend8_to_32(&mut self, src: u8, dest: u8) {
        self.assembler.slli(dest, src, 56);
        self.assembler.srai(dest, dest, 24);
        self.assembler.srli(dest, dest, 32);
    }

    pub fn sign_extend16_to_32(&mut self, src: u8, dest: u8) {
        self.assembler.slli(dest, src, 48);
        self.assembler.srai(dest, dest, 16);
        self.assembler.srli(dest, dest, 32);
    }

    pub fn zero_extend8_to_64(&mut self, src: u8, dest: u8) {
        self.zero_extend8_to_32(src, dest);
    }

    pub fn zero_extend16_to_64(&mut self, src: u8, dest: u8) {
        self.zero_extend16_to_32(src, dest);
    }

    pub fn sign_extend8_to_64(&mut self, src: u8, dest: u8) {
        self.sign_extend8_to_32(src, dest);
        self.sign_extend32_to_64(src, dest);
    }

    pub fn sign_extend32_to_64(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Imm32(imm) => {
                self.load_immediate32(imm, dest);
            }

            Operand::Register(src) => {
                self.assembler.addiw(dest, src, 0);
            }

            _ => unreachable!(),
        }
    }

    pub fn and32(&mut self, src: impl Into<Operand>, dest: u8) {
        let src = src.into();
        if let Operand::Address(address) = src {
            let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
            self.assembler
                .lw(Self::DATA_TEMP_REGISTER, resolution.base, resolution.offset);
            self.assembler.and(dest, Self::DATA_TEMP_REGISTER, dest);
            //self.assembler.mask_register(dest, dest, 32);
            return;
        }
        self.and32_rrr(src, dest, dest);
    }

    pub fn and32_rrr(&mut self, op1: impl Into<Operand>, op2: u8, dest: u8) {
        match op1.into() {
            Operand::Imm32(imm) => {
                if !IImmediate::is_valid(imm) {
                    self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                    self.assembler.and(dest, Self::DATA_TEMP_REGISTER, op2);
                } else {
                    self.assembler.andi(dest, op2, imm);
                }

                //self.assembler.mask_register(dest, dest, 32);
            }

            Operand::Register(op1) => {
                self.assembler.and(dest, op1, op2);
                //self.assembler.mask_register(dest, dest, 32);
            }

            _ => unreachable!(),
        }
    }

    pub fn and64(&mut self, src: impl Into<Operand>, dest: u8) {
        self.and64_rrr(src, dest, dest);
    }

    pub fn and64_rrr(&mut self, op1: impl Into<Operand>, op2: u8, dest: u8) {
        match op1.into() {
            Operand::Imm32(imm) => {
                if !IImmediate::is_valid(imm) {
                    self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                    self.assembler.and(dest, Self::DATA_TEMP_REGISTER, op2);
                } else {
                    self.assembler.andi(dest, op2, imm);
                }
            }

            Operand::Register(op1) => {
                self.assembler.and(dest, op1, op2);
            }

            Operand::Imm64(imm) => {
                if IImmediate::is_valid(imm) {
                    self.assembler.andi(dest, op2, imm as i32);
                    return;
                }
                self.load_immediate64(imm, Self::DATA_TEMP_REGISTER);
                self.assembler.and(dest, Self::DATA_TEMP_REGISTER, op2);
            }

            _ => unreachable!(),
        }
    }

    pub fn or8(&mut self, src: impl Into<Operand>, address: AbsoluteAddress) {
        match src.into() {
            Operand::Register(src) => {
                self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                self.assembler
                    .lb(Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, 0);
                self.assembler
                    .or(Self::DATA_TEMP_REGISTER, src, Self::DATA_TEMP_REGISTER);
                self.assembler
                    .sb(Self::MEMORY_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 0);
            }

            Operand::Imm32(imm) => {
                self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                self.assembler
                    .lb(Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, 0);

                if IImmediate::is_valid(imm) {
                    self.assembler
                        .ori(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, imm);
                    self.assembler
                        .sb(Self::MEMORY_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 0);
                } else {
                    self.load_immediate32(imm, Self::MEMORY_TEMP_REGISTER);
                    self.assembler.or(
                        Self::DATA_TEMP_REGISTER,
                        Self::DATA_TEMP_REGISTER,
                        Self::MEMORY_TEMP_REGISTER,
                    );
                    self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                    self.assembler
                        .sb(Self::MEMORY_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 0);
                }
            }

            _ => unreachable!(),
        }
    }

    pub fn or16(&mut self, src: impl Into<Operand>, address: AbsoluteAddress) {
        match src.into() {
            Operand::Register(src) => {
                self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                self.assembler
                    .lh(Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, 0);
                self.assembler
                    .or(Self::DATA_TEMP_REGISTER, src, Self::DATA_TEMP_REGISTER);
                self.assembler
                    .sh(Self::MEMORY_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 0);
            }

            Operand::Imm32(imm) => {
                self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                self.assembler
                    .lh(Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, 0);

                if IImmediate::is_valid(imm) {
                    self.assembler
                        .ori(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, imm);
                    self.assembler
                        .sh(Self::MEMORY_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 0);
                } else {
                    self.load_immediate32(imm, Self::MEMORY_TEMP_REGISTER);
                    self.assembler.or(
                        Self::DATA_TEMP_REGISTER,
                        Self::DATA_TEMP_REGISTER,
                        Self::MEMORY_TEMP_REGISTER,
                    );
                    self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                    self.assembler
                        .sh(Self::MEMORY_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 0);
                }
            }

            _ => unreachable!(),
        }
    }

    pub fn or32(&mut self, src: impl Into<Operand>, dest: impl Into<Operand>) {
        match dest.into() {
            Operand::Register(dest) => {
                self.or32_rrr(src, dest, dest);
            }

            Operand::AbsoluteAddress(address) => match src.into() {
                Operand::Register(src) => {
                    self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                    self.assembler
                        .lw(Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, 0);
                    self.assembler
                        .or(Self::DATA_TEMP_REGISTER, src, Self::DATA_TEMP_REGISTER);
                    self.assembler
                        .sw(Self::MEMORY_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 0);
                }

                Operand::Imm32(imm) => {
                    self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                    self.assembler
                        .lw(Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, 0);

                    if IImmediate::is_valid(imm) {
                        self.assembler
                            .ori(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, imm);
                        self.assembler
                            .sw(Self::MEMORY_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 0);
                    } else {
                        self.load_immediate32(imm, Self::MEMORY_TEMP_REGISTER);
                        self.assembler.or(
                            Self::DATA_TEMP_REGISTER,
                            Self::DATA_TEMP_REGISTER,
                            Self::MEMORY_TEMP_REGISTER,
                        );
                        self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                        self.assembler
                            .sw(Self::MEMORY_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 0);
                    }
                }

                _ => unreachable!(),
            },

            Operand::Address(address) => {
                if let Operand::Imm32(imm) = src.into() {
                    let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);

                    self.assembler
                        .lw(Self::DATA_TEMP_REGISTER, resolution.base, resolution.offset);

                    if IImmediate::is_valid(imm) {
                        self.assembler.ori(
                            Self::DATA_TEMP_REGISTER,
                            Self::MEMORY_TEMP_REGISTER,
                            imm,
                        );
                        self.assembler
                            .sw(resolution.base, Self::DATA_TEMP_REGISTER, 0);
                    } else {
                        self.load_immediate32(imm, Self::MEMORY_TEMP_REGISTER);
                        self.assembler.or(
                            Self::DATA_TEMP_REGISTER,
                            Self::DATA_TEMP_REGISTER,
                            Self::MEMORY_TEMP_REGISTER,
                        );
                        let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                        self.assembler.sw(
                            resolution.base,
                            Self::DATA_TEMP_REGISTER,
                            resolution.offset,
                        );
                    }
                } else {
                    unreachable!()
                }
            }

            _ => unreachable!(),
        }
    }

    pub fn or32_rrr(&mut self, op1: impl Into<Operand>, op2: u8, dest: u8) {
        match op1.into() {
            Operand::Register(src) => {
                self.assembler.or(dest, src, op2);
                //self.assembler.mask_register(dest, dest, 32);
            }

            Operand::Imm32(imm) => {
                if IImmediate::is_valid(imm) {
                    self.assembler.ori(dest, op2, imm);
                    //self.assembler.mask_register(dest, dest, 32);
                } else {
                    self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                    self.assembler.or(dest, op2, Self::DATA_TEMP_REGISTER);
                    //self.assembler.mask_register(dest, dest, 32);
                }
            }

            _ => unreachable!(),
        }
    }

    pub fn or64_rrr(&mut self, op1: impl Into<Operand>, op2: u8, dest: u8) {
        match op1.into() {
            Operand::Imm32(imm) => {
                if IImmediate::is_valid(imm) {
                    self.assembler.ori(dest, op2, imm);
                } else {
                    self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                    self.assembler.or(dest, Self::DATA_TEMP_REGISTER, op2);
                }
            }

            Operand::Imm64(imm) => {
                if IImmediate::is_valid(imm) {
                    self.assembler.ori(dest, op2, imm as _);
                } else {
                    self.load_immediate64(imm, Self::DATA_TEMP_REGISTER);
                    self.assembler.or(dest, Self::DATA_TEMP_REGISTER, op2);
                }
            }

            Operand::Register(op1) => {
                self.assembler.or(dest, op1, op2);
            }

            _ => unreachable!(),
        }
    }

    pub fn or64(&mut self, src: impl Into<Operand>, dest: u8) {
        self.or64_rrr(src, dest, dest);
    }

    pub fn xor32(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Address(address) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler
                    .lw(Self::DATA_TEMP_REGISTER, resolution.base, resolution.offset);
                self.assembler.xor(dest, Self::DATA_TEMP_REGISTER, dest);
                //self.assembler.mask_register(dest, dest, 32);
            }

            op => self.xor32_rrr(op, dest, dest),
        }
    }

    pub fn xor32_rrr(&mut self, op1: impl Into<Operand>, op2: u8, dest: u8) {
        match op1.into() {
            Operand::Register(src) => {
                self.assembler.xor(dest, src, op2);
                //self.assembler.mask_register(dest, dest, 32);
            }

            Operand::Imm32(imm) => {
                if IImmediate::is_valid(imm) {
                    self.assembler.xori(dest, op2, imm);
                    //self.assembler.mask_register(dest, dest, 32);
                } else {
                    self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                    self.assembler.xor(dest, op2, Self::DATA_TEMP_REGISTER);
                    //self.assembler.mask_register(dest, dest, 32);
                }
            }

            _ => unreachable!(),
        }
    }

    pub fn xor64(&mut self, src: impl Into<Operand>, dest: impl Into<Operand>) {
        match (src.into(), dest.into()) {
            (Operand::Address(address), Operand::Register(dest)) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler
                    .ld(Self::DATA_TEMP_REGISTER, resolution.base, resolution.offset);
                self.assembler.xor(dest, Self::DATA_TEMP_REGISTER, dest);
            }

            (Operand::Register(src), Operand::Address(dest)) => {
                let resolution = self.resolve_address(dest, Self::MEMORY_TEMP_REGISTER);
                self.assembler
                    .ld(Self::DATA_TEMP_REGISTER, resolution.base, resolution.offset);
                self.assembler
                    .xor(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, src);
                self.assembler
                    .sd(resolution.base, Self::DATA_TEMP_REGISTER, resolution.offset);
            }

            (op, Operand::Register(dest)) => self.xor64_rrr(op, dest, dest),
            _ => unreachable!(),
        }
    }

    pub fn xor64_rrr(&mut self, op1: impl Into<Operand>, op2: u8, dest: u8) {
        match op1.into() {
            Operand::Imm32(imm) => {
                if IImmediate::is_valid(imm) {
                    self.assembler.xori(dest, op2, imm);
                } else {
                    self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                    self.assembler.xor(dest, Self::DATA_TEMP_REGISTER, op2);
                }
            }

            Operand::Imm64(imm) => {
                if IImmediate::is_valid(imm) {
                    self.assembler.xori(dest, op2, imm as _);
                } else {
                    self.load_immediate64(imm, Self::DATA_TEMP_REGISTER);
                    self.assembler.xor(dest, Self::DATA_TEMP_REGISTER, op2);
                }
            }

            Operand::Register(op1) => {
                self.assembler.xor(dest, op1, op2);
            }

            _ => unreachable!(),
        }
    }

    pub fn not32(&mut self, src_dest: u8) {
        self.assembler.xori(src_dest, src_dest, -1);
        //self.assembler.mask_register(src_dest, src_dest, 32);
    }

    pub fn not64(&mut self, src_dest: u8) {
        self.assembler.xori(src_dest, src_dest, -1);
    }

    pub fn neg32(&mut self, src_dest: u8) {
        self.assembler.subw(src_dest, zero, src_dest);
        self.assembler
            .mask_register(src_dest, Self::DATA_TEMP_REGISTER, 32);
    }

    pub fn neg64(&mut self, src_dest: u8) {
        self.assembler.sub(src_dest, zero, src_dest);
    }

    pub fn mov(&mut self, src: impl Into<Operand>, dst: impl Into<Operand>) {
        match dst.into() {
            Operand::Register(dest) => match src.into() {
                Operand::Register(src) => {
                    self.assembler.addi(dest, src, 0);
                }

                Operand::Imm32(imm) => {
                    self.load_immediate32(imm, dest);
                    //self.assembler.mask_register(dest, dest, 32);
                }

                Operand::Imm64(imm) => {
                    self.load_immediate64(imm, dest);
                }

                Operand::ImmPtr(imm) => {
                    self.load_immediate64(imm as i64, dest);
                }

                Operand::AbsoluteAddress(address) => {
                    self.load_immediate64(address.ptr as i64, dest);
                }

                _ => unreachable!(),
            },

            _ => unreachable!(),
        }
    }

    pub fn swap(&mut self, reg1: u8, reg2: u8) {
        self.mov(reg1, Self::DATA_TEMP_REGISTER);
        self.mov(reg2, reg1);
        self.mov(Self::DATA_TEMP_REGISTER, reg2);
    }

    pub fn swap_double(&mut self, reg1: u8, reg2: u8) {
        if reg1 == reg2 {
            return;
        }

        self.move_double(reg1, Self::FP_TEMP_REGISTER);
        self.move_double(reg2, reg1);
        self.move_double(Self::FP_TEMP_REGISTER, reg2);
    }

    pub fn move_zero_to_float(&mut self, dest: u8) {
        self.assembler
            .fcvt_si2fp::<32>(dest, zero, FPRoundingMode::DYN);
    }

    pub fn move_zero_to_double(&mut self, dest: u8) {
        self.assembler
            .fcvt_si2fp::<64>(dest, zero, FPRoundingMode::DYN);
    }

    pub fn move_double(&mut self, src: u8, dest: u8) {
        self.assembler.fsgnj::<64>(dest, src, src);
    }

    pub fn move_double_to64(&mut self, src: u8, dest: u8) {
        self.assembler.fmv_fp2i::<64>(dest, src);
    }

    pub fn move64_to_double(&mut self, src: u8, dest: u8) {
        self.assembler.fmv_i2fp::<64>(dest, src);
    }

    pub fn move_float(&mut self, src: u8, dest: u8) {
        self.assembler.fsgnj::<32>(dest, src, src);
    }

    pub fn move_float_to64(&mut self, src: u8, dest: u8) {
        self.assembler.fmv_fp2i::<32>(dest, src);
    }

    pub fn move_float_to32(&mut self, src: u8, dest: u8) {
        self.assembler.fmv_fp2i::<32>(dest, src);
        self.assembler.mask_register(dest, dest, 32);
    }

    pub fn move64_to_float(&mut self, src: u8, dest: u8) {
        self.assembler.fmv_i2fp::<32>(dest, src);
    }

    pub fn move32_to_float(&mut self, src: u8, dest: u8) {
        self.assembler.fmv_i2fp::<32>(dest, src);
    }

    pub fn jump(&mut self) -> Jump {
        let label = self.assembler.label();

        self.assembler.jump_placeholder(|asm| asm.jal(zero, 0));

        Jump::new(label)
    }

    pub unsafe fn read_call_target(call: *mut u8) -> *const u8 {
        RISCV64Assembler::read_call_target(call)
    }

    pub unsafe fn replace_with_vm_halt(instruction_start: *mut u8) {
        RISCV64Assembler::replace_with_vm_halt(instruction_start);
    }

    pub unsafe fn replace_with_jump(instruction_start: *mut u8, dest: *mut u8) {
        RISCV64Assembler::replace_with_jump(instruction_start, dest);
    }

    pub const fn max_jump_replacement_size() -> usize {
        RISCV64Assembler::max_jump_replacement_size()
    }

    pub const fn patchable_jump_size() -> usize {
        RISCV64Assembler::patchable_jump_size()
    }

    pub unsafe fn start_of_branch_ptr_with_patch_on_register(label: *mut u8) -> *mut u8 {
        label
    }

    pub unsafe fn revert_jump_replacement_to_branch_ptr_with_patch(
        jump: *mut u8,
        _: u8,
        initial_value: *mut u8,
    ) {
        RISCV64Assembler::revert_jump_replacement_to_patch(jump, initial_value);
    }

    pub unsafe fn link_call(code: *mut u8, call: Call, function: *const u8) {
        if !call.is_flag_set(Call::NEAR) {
            RISCV64Assembler::link_pointer(code, call.label, function as _)
        } else {
            RISCV64Assembler::link_call(code, call.label, function as _)
        }
    }

    pub unsafe fn repatch_call(call: *mut u8, destination: *mut u8) {
        RISCV64Assembler::repatch_pointer(call, destination);
    }

    pub fn far_jump(&mut self, target: impl Into<Operand>) {
        match target.into() {
            Operand::Register(target) => {
                self.assembler.jalr(zero, target, 0);
            }

            Operand::AbsoluteAddress(address) => {
                self.load_immediate64(address.ptr as _, Self::DATA_TEMP_REGISTER);
                self.assembler
                    .ld(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 0);
                self.assembler.jalr(zero, Self::DATA_TEMP_REGISTER, 0);
            }

            Operand::Address(address) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler
                    .ld(Self::DATA_TEMP_REGISTER, resolution.base, resolution.offset);
                self.assembler.jalr(zero, Self::DATA_TEMP_REGISTER, 0);
            }

            Operand::BaseIndex(address) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler
                    .ld(Self::DATA_TEMP_REGISTER, resolution.base, resolution.offset);
                self.assembler.jalr(zero, Self::DATA_TEMP_REGISTER, 0);
            }

            Operand::Imm64(imm) => {
                self.load_immediate64(imm, Self::DATA_TEMP_REGISTER);
                self.assembler.jalr(zero, Self::DATA_TEMP_REGISTER, 0);
            }

            Operand::ImmPtr(imm) => {
                self.load_immediate64(imm as _, Self::DATA_TEMP_REGISTER);
                self.assembler.jalr(zero, Self::DATA_TEMP_REGISTER, 0);
            }

            _ => unreachable!(),
        }
    }

    pub fn near_call(&mut self) -> Call {
        let label = self.assembler.label();

        self.assembler.near_call_placeholder(|asm| asm.jal(x1, 0));

        Call::new(label, Call::LINKABLE_NEAR)
    }

    pub fn near_tail_call(&mut self) -> Call {
        let label = self.assembler.label();

        self.assembler
            .near_call_placeholder(|asm| asm.jalr(zero, x1, 0));

        Call::new(label, Call::LINKABLE_NEAR_TAIL)
    }

    pub fn thread_safe_patchable_near_call(&mut self) -> Call {
        let label = self.assembler.label();

        self.assembler.near_call_placeholder(|asm| asm.jal(x1, 0));

        Call::new(label, Call::LINKABLE_NEAR)
    }

    pub fn thread_safe_patchable_near_tail_call(&mut self) -> Call {
        let label = self.assembler.label();

        self.assembler
            .near_call_placeholder(|asm| asm.jalr(zero, x1, 0));

        Call::new(label, Call::LINKABLE_NEAR_TAIL)
    }

    pub fn ret(&mut self) {
        self.assembler.jalr(zero, x1, 0);
    }

    pub fn compare8(&mut self, cond: RelationalCondition, address: Address, imm: i32, dest: u8) {
        let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
        self.assembler.lb(
            Self::MEMORY_TEMP_REGISTER,
            resolution.base,
            resolution.offset,
        );
        self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
        self.compare_finalize(
            cond,
            Self::MEMORY_TEMP_REGISTER,
            Self::DATA_TEMP_REGISTER,
            dest,
        )
    }

    pub fn compare32(
        &mut self,
        cond: RelationalCondition,
        lhs: impl Into<Operand>,
        rhs: impl Into<Operand>,
        dest: u8,
    ) {
        match lhs.into() {
            Operand::Register(lhs) => match rhs.into() {
                Operand::Register(rhs) => {
                    self.assembler
                        .sign_extend(Self::MEMORY_TEMP_REGISTER, lhs, 32);
                    self.assembler
                        .sign_extend(Self::DATA_TEMP_REGISTER, rhs, 32);
                    self.compare_finalize(
                        cond,
                        Self::MEMORY_TEMP_REGISTER,
                        Self::DATA_TEMP_REGISTER,
                        dest,
                    );
                }

                Operand::Imm32(rhs) => {
                    self.assembler
                        .sign_extend(Self::MEMORY_TEMP_REGISTER, lhs, 32);
                    self.load_immediate32(rhs, Self::DATA_TEMP_REGISTER);
                    self.compare_finalize(
                        cond,
                        Self::MEMORY_TEMP_REGISTER,
                        Self::DATA_TEMP_REGISTER,
                        dest,
                    );
                }

                _ => unreachable!(),
            },

            Operand::Address(lhs) => match rhs.into() {
                Operand::Register(rhs) => {
                    let resolution = self.resolve_address(lhs, Self::MEMORY_TEMP_REGISTER);
                    self.assembler.lw(
                        Self::MEMORY_TEMP_REGISTER,
                        resolution.base,
                        resolution.offset,
                    );
                    self.assembler
                        .sign_extend(Self::DATA_TEMP_REGISTER, rhs, 32);
                    self.compare_finalize(
                        cond,
                        Self::MEMORY_TEMP_REGISTER,
                        Self::DATA_TEMP_REGISTER,
                        dest,
                    );
                }
                _ => unreachable!(),
            },

            _ => unreachable!(),
        }
    }

    pub fn compare64(
        &mut self,
        cond: RelationalCondition,
        lhs: u8,
        rhs: impl Into<Operand>,
        dest: u8,
    ) {
        match rhs.into() {
            Operand::Register(rhs) => {
                self.assembler
                    .sign_extend(Self::MEMORY_TEMP_REGISTER, lhs, 64);
                self.assembler
                    .sign_extend(Self::DATA_TEMP_REGISTER, rhs, 64);
                self.compare_finalize(
                    cond,
                    Self::MEMORY_TEMP_REGISTER,
                    Self::DATA_TEMP_REGISTER,
                    dest,
                );
            }

            Operand::Imm32(rhs) => {
                self.assembler
                    .sign_extend(Self::MEMORY_TEMP_REGISTER, lhs, 64);
                self.load_immediate32(rhs, Self::DATA_TEMP_REGISTER);
                self.compare_finalize(
                    cond,
                    Self::MEMORY_TEMP_REGISTER,
                    Self::DATA_TEMP_REGISTER,
                    dest,
                );
            }

            _ => unreachable!(),
        }
    }

    pub fn test8(&mut self, cond: ResultCondition, address: Address, imm: i32, dest: u8) {
        let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
        self.assembler
            .lbu(Self::DATA_TEMP_REGISTER, resolution.base, resolution.offset);
        self.assembler
            .andi(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, imm as _);
        self.test_finalize(cond, Self::DATA_TEMP_REGISTER, dest);
    }

    pub fn test32_cond(
        &mut self,
        cond: ResultCondition,
        lhs: impl Into<Operand>,
        rhs: impl Into<Operand>,
        dest: u8,
    ) {
        match (lhs.into(), rhs.into()) {
            (Operand::Register(lhs), Operand::Register(rhs)) => {
                self.assembler.and(Self::DATA_TEMP_REGISTER, lhs, rhs);
                self.assembler.mask_register(
                    Self::DATA_TEMP_REGISTER,
                    Self::DATA_TEMP_REGISTER,
                    32,
                );
                self.test_finalize(cond, Self::DATA_TEMP_REGISTER, dest);
            }
            (Operand::Register(lhs), Operand::Imm32(imm)) => {
                if !IImmediate::is_valid(imm) {
                    self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                    self.assembler
                        .and(Self::DATA_TEMP_REGISTER, lhs, Self::DATA_TEMP_REGISTER);
                } else {
                    self.assembler.andi(Self::DATA_TEMP_REGISTER, lhs, imm as _);
                }

                self.assembler.mask_register(
                    Self::DATA_TEMP_REGISTER,
                    Self::DATA_TEMP_REGISTER,
                    32,
                );
                self.test_finalize(cond, Self::DATA_TEMP_REGISTER, dest);
            }

            (Operand::Address(address), Operand::Imm32(imm)) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.lwu(
                    Self::MEMORY_TEMP_REGISTER,
                    resolution.base,
                    resolution.offset,
                );

                if !IImmediate::is_valid(imm) {
                    self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                    self.assembler.and(
                        Self::DATA_TEMP_REGISTER,
                        Self::MEMORY_TEMP_REGISTER,
                        Self::DATA_TEMP_REGISTER,
                    );
                } else {
                    self.assembler.andi(
                        Self::DATA_TEMP_REGISTER,
                        Self::MEMORY_TEMP_REGISTER,
                        imm as _,
                    );
                }

                self.test_finalize(cond, Self::DATA_TEMP_REGISTER, dest)
            }

            _ => unreachable!(),
        }
    }

    pub fn test64(&mut self, cond: ResultCondition, lhs: u8, rhs: impl Into<Operand>, dest: u8) {
        match rhs.into() {
            Operand::Imm32(imm) => {
                if IImmediate::is_valid(imm) {
                    self.assembler.andi(dest, lhs, imm as _);
                } else {
                    self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                    self.assembler.and(dest, lhs, Self::DATA_TEMP_REGISTER);
                }

                self.test_finalize(cond, dest, dest);
            }

            Operand::Register(rhs) => {
                self.assembler.and(dest, lhs, rhs);
                self.test_finalize(cond, dest, dest);
            }

            _ => unreachable!(),
        }
    }

    pub fn branch8(
        &mut self,
        cond: RelationalCondition,
        left: impl Into<Operand>,
        right: i32,
    ) -> Jump {
        match left.into() {
            Operand::Address(address) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.lb(
                    Self::MEMORY_TEMP_REGISTER,
                    resolution.base,
                    resolution.offset,
                );

                self.load_immediate32(right, Self::DATA_TEMP_REGISTER);
                self.make_branch(cond, Self::MEMORY_TEMP_REGISTER, Self::DATA_TEMP_REGISTER)
            }

            Operand::AbsoluteAddress(address) => {
                self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                self.assembler
                    .lb(Self::MEMORY_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, 0);
                self.load_immediate32(right, Self::DATA_TEMP_REGISTER);
                self.make_branch(cond, Self::MEMORY_TEMP_REGISTER, Self::DATA_TEMP_REGISTER)
            }

            _ => unreachable!(),
        }
    }

    pub fn branch32(
        &mut self,
        cond: RelationalCondition,
        lhs: impl Into<Operand>,
        rhs: impl Into<Operand>,
    ) -> Jump {
        match (lhs.into(), rhs.into()) {
            (Operand::Register(lhs), Operand::Register(rhs)) => {
                self.assembler
                    .sign_extend(Self::DATA_TEMP_REGISTER, lhs, 32);
                self.assembler
                    .sign_extend(Self::MEMORY_TEMP_REGISTER, rhs, 32);

                self.make_branch(cond, Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER)
            }

            (Operand::Register(lhs), Operand::Imm32(imm)) => {
                self.assembler
                    .sign_extend(Self::DATA_TEMP_REGISTER, lhs, 32);
                self.load_immediate32(imm, Self::MEMORY_TEMP_REGISTER);

                self.make_branch(cond, Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER)
            }

            (Operand::Register(lhs), Operand::Address(address)) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.lw(
                    Self::MEMORY_TEMP_REGISTER,
                    resolution.base,
                    resolution.offset,
                );

                self.assembler
                    .sign_extend(Self::DATA_TEMP_REGISTER, lhs, 32);
                self.make_branch(cond, Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER)
            }

            (Operand::Address(address), Operand::Register(rhs)) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler
                    .lw(Self::DATA_TEMP_REGISTER, resolution.base, resolution.offset);

                self.assembler
                    .sign_extend(Self::MEMORY_TEMP_REGISTER, rhs, 32);
                self.make_branch(cond, Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER)
            }

            (Operand::Address(address), Operand::Imm32(imm)) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler
                    .lw(Self::DATA_TEMP_REGISTER, resolution.base, resolution.offset);

                self.load_immediate32(imm, Self::MEMORY_TEMP_REGISTER);
                self.make_branch(cond, Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER)
            }

            (Operand::AbsoluteAddress(address), Operand::Register(rhs)) => {
                self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                self.assembler
                    .lw(Self::MEMORY_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, 0);

                self.assembler
                    .sign_extend(Self::DATA_TEMP_REGISTER, rhs, 32);
                self.make_branch(cond, Self::MEMORY_TEMP_REGISTER, Self::DATA_TEMP_REGISTER)
            }

            (Operand::BaseIndex(address), Operand::Imm32(imm)) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.lw(
                    Self::MEMORY_TEMP_REGISTER,
                    resolution.base,
                    resolution.offset,
                );

                self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                self.make_branch(cond, Self::MEMORY_TEMP_REGISTER, Self::DATA_TEMP_REGISTER)
            }

            _ => unreachable!(),
        }
    }

    pub fn branch64(
        &mut self,
        cond: RelationalCondition,
        lhs: impl Into<Operand>,
        rhs: impl Into<Operand>,
    ) -> Jump {
        match (lhs.into(), rhs.into()) {
            (Operand::Register(lhs), Operand::Register(rhs)) => {
                self.make_branch(cond, Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER)
            }

            (Operand::Register(lhs), Operand::Imm32(imm)) => {
                self.load_immediate32(imm, Self::MEMORY_TEMP_REGISTER);

                self.make_branch(cond, Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER)
            }

            (Operand::Register(lhs), Operand::Address(address)) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.ld(
                    Self::MEMORY_TEMP_REGISTER,
                    resolution.base,
                    resolution.offset,
                );

                self.make_branch(cond, Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER)
            }

            (Operand::Address(address), Operand::Register(rhs)) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler
                    .ld(Self::DATA_TEMP_REGISTER, resolution.base, resolution.offset);

                self.make_branch(cond, Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER)
            }

            (Operand::Address(address), Operand::Imm32(imm)) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler
                    .ld(Self::DATA_TEMP_REGISTER, resolution.base, resolution.offset);

                self.load_immediate32(imm, Self::MEMORY_TEMP_REGISTER);
                self.make_branch(cond, Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER)
            }

            (Operand::AbsoluteAddress(address), Operand::Register(rhs)) => {
                self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                self.assembler
                    .ld(Self::MEMORY_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, 0);

                self.make_branch(cond, Self::MEMORY_TEMP_REGISTER, Self::DATA_TEMP_REGISTER)
            }

            (Operand::BaseIndex(address), Operand::Imm32(imm)) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.ld(
                    Self::MEMORY_TEMP_REGISTER,
                    resolution.base,
                    resolution.offset,
                );

                self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                self.make_branch(cond, Self::MEMORY_TEMP_REGISTER, Self::DATA_TEMP_REGISTER)
            }
            (Operand::Register(lhs), Operand::Imm64(rhs)) => {
                self.load_immediate64(rhs, Self::MEMORY_TEMP_REGISTER);
                self.make_branch(cond, lhs, Self::MEMORY_TEMP_REGISTER)
            }

            (Operand::Address(address), Operand::Imm64(imm)) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.ld(
                    Self::MEMORY_TEMP_REGISTER,
                    resolution.base,
                    resolution.offset,
                );
                self.load_immediate64(imm, Self::DATA_TEMP_REGISTER);
                self.make_branch(cond, Self::MEMORY_TEMP_REGISTER, Self::DATA_TEMP_REGISTER)
            }

            (Operand::Address(left), Operand::Address(right)) => {
                let left_resolution = self.resolve_address(left, Self::MEMORY_TEMP_REGISTER);
                self.assembler.ld(
                    Self::DATA_TEMP_REGISTER,
                    left_resolution.base,
                    left_resolution.offset,
                );

                let right_resolution = self.resolve_address(right, Self::MEMORY_TEMP_REGISTER);

                self.assembler.ld(
                    Self::MEMORY_TEMP_REGISTER,
                    right_resolution.base,
                    right_resolution.offset,
                );

                self.make_branch(cond, Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER)
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
        match dest.into() {
            Operand::Address(address) => match src.into() {
                Operand::Imm32(imm) => {
                    let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                    self.assembler.lw(
                        Self::MEMORY_TEMP_REGISTER,
                        resolution.base,
                        resolution.offset,
                    );

                    if cond == ResultCondition::Overflow {
                        let branch = self.branch_for_arithmetic_overflow::<32>(
                            ArithmeticOperation::Addition,
                            Self::DATA_TEMP_REGISTER,
                            Ok(imm),
                            Self::DATA_TEMP_REGISTER,
                        );

                        let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                        self.assembler.sw(
                            resolution.base,
                            Self::DATA_TEMP_REGISTER,
                            resolution.offset,
                        );
                        return branch;
                    }

                    if IImmediate::is_valid(imm) {
                        self.assembler.addiw(
                            Self::DATA_TEMP_REGISTER,
                            Self::DATA_TEMP_REGISTER,
                            imm,
                        );
                    } else {
                        self.load_immediate32(imm, Self::MEMORY_TEMP_REGISTER);
                        self.assembler.addw(
                            Self::DATA_TEMP_REGISTER,
                            Self::DATA_TEMP_REGISTER,
                            Self::MEMORY_TEMP_REGISTER,
                        );
                    }

                    let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                    self.assembler
                        .sw(resolution.base, Self::DATA_TEMP_REGISTER, resolution.offset);
                    self.branch_test_finalize(cond, Self::DATA_TEMP_REGISTER)
                }
                _ => unreachable!(),
            },

            Operand::AbsoluteAddress(address) => match src.into() {
                Operand::Imm32(imm) => {
                    self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                    self.assembler
                        .lw(Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, 0);

                    if cond == ResultCondition::Overflow {
                        let branch = self.branch_for_arithmetic_overflow::<32>(
                            ArithmeticOperation::Addition,
                            Self::DATA_TEMP_REGISTER,
                            Ok(imm),
                            Self::DATA_TEMP_REGISTER,
                        );

                        self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                        self.assembler
                            .sw(Self::MEMORY_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 0);
                        return branch;
                    }

                    if !IImmediate::is_valid(imm) {
                        self.load_immediate32(imm, Self::MEMORY_TEMP_REGISTER);
                        self.assembler.addw(
                            Self::DATA_TEMP_REGISTER,
                            Self::DATA_TEMP_REGISTER,
                            Self::MEMORY_TEMP_REGISTER,
                        );
                    } else {
                        self.assembler.addiw(
                            Self::DATA_TEMP_REGISTER,
                            Self::DATA_TEMP_REGISTER,
                            imm,
                        );
                    }

                    self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                    self.assembler
                        .sw(Self::MEMORY_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 0);
                    self.branch_test_finalize(cond, Self::DATA_TEMP_REGISTER)
                }

                _ => unreachable!(),
            },

            Operand::Register(reg) => {
                match src.into() {
                    Operand::Address(address) => {
                        let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                        self.assembler.lw(
                            Self::MEMORY_TEMP_REGISTER,
                            resolution.base,
                            resolution.offset,
                        );

                        if cond == ResultCondition::Overflow {
                            return self.branch_for_arithmetic_overflow::<32>(
                                ArithmeticOperation::Addition,
                                reg,
                                Err(Self::MEMORY_TEMP_REGISTER),
                                reg,
                            );
                        }

                        self.assembler.addw(
                            Self::DATA_TEMP_REGISTER,
                            reg,
                            Self::MEMORY_TEMP_REGISTER,
                        );
                        //self.assembler.mask_register(reg, Self::DATA_TEMP_REGISTER, 32);
                        self.branch_test_finalize(cond, Self::DATA_TEMP_REGISTER)
                    }

                    op => self.branch_add32_rrr(cond, reg, op, reg),
                }
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
            Operand::Imm32(imm) => {
                if cond == ResultCondition::Overflow {
                    return self.branch_for_arithmetic_overflow::<32>(
                        ArithmeticOperation::Addition,
                        op1,
                        Ok(imm),
                        dest,
                    );
                }

                if !IImmediate::is_valid(imm) {
                    self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                    self.assembler
                        .addw(Self::DATA_TEMP_REGISTER, op1, Self::DATA_TEMP_REGISTER);
                } else {
                    self.assembler.addiw(Self::DATA_TEMP_REGISTER, op1, imm);
                }

                //self.assembler.mask_register(dest, Self::DATA_TEMP_REGISTER, 32);
                self.branch_test_finalize(cond, Self::DATA_TEMP_REGISTER)
            }

            Operand::Register(op2) => {
                if cond == ResultCondition::Overflow {
                    return self.branch_for_arithmetic_overflow::<32>(
                        ArithmeticOperation::Addition,
                        op1,
                        Err(op2),
                        dest,
                    );
                }

                self.assembler.addw(Self::DATA_TEMP_REGISTER, op1, op2);
                //self.assembler.mask_register(dest, Self::DATA_TEMP_REGISTER, 32);
                self.branch_test_finalize(cond, Self::DATA_TEMP_REGISTER)
            }

            _ => unreachable!(),
        }
    }

    pub fn branch_add64(
        &mut self,
        cond: ResultCondition,
        src: impl Into<Operand>,
        dest: u8,
    ) -> Jump {
        self.branch_add64_rrr(cond, dest, src, dest)
    }

    pub fn branch_add64_rrr(
        &mut self,
        cond: ResultCondition,
        op1: u8,
        op2: impl Into<Operand>,
        dest: u8,
    ) -> Jump {
        match op2.into() {
            Operand::Imm32(imm) => {
                if cond == ResultCondition::Overflow {
                    return self.branch_for_arithmetic_overflow::<64>(
                        ArithmeticOperation::Addition,
                        op1,
                        Ok(imm),
                        dest,
                    );
                }

                if !IImmediate::is_valid(imm) {
                    self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                    self.assembler.add(dest, op1, Self::DATA_TEMP_REGISTER);
                } else {
                    self.assembler.addi(dest, op1, imm);
                }

                self.branch_test_finalize(cond, dest)
            }

            Operand::Register(op2) => {
                if cond == ResultCondition::Overflow {
                    return self.branch_for_arithmetic_overflow::<64>(
                        ArithmeticOperation::Addition,
                        op1,
                        Err(op2),
                        dest,
                    );
                }

                self.assembler.add(dest, op1, op2);
                self.branch_test_finalize(cond, dest)
            }
            _ => unreachable!(),
        }
    }

    pub fn branch_sub32(
        &mut self,
        cond: ResultCondition,
        src: impl Into<Operand>,
        dest: u8,
    ) -> Jump {
        self.branch_sub32_rrr(cond, dest, src, dest)
    }

    pub fn branch_sub32_rrr(
        &mut self,
        cond: ResultCondition,
        op1: u8,
        op2: impl Into<Operand>,
        dest: u8,
    ) -> Jump {
        match op2.into() {
            Operand::Imm32(imm) => self.branch_add32_rrr(cond, op1, Operand::Imm32(-imm), dest),

            Operand::Register(op2) => {
                if cond == ResultCondition::Overflow {
                    return self.branch_for_arithmetic_overflow::<32>(
                        ArithmeticOperation::Subtraction,
                        op1,
                        Err(op2),
                        dest,
                    );
                }

                self.assembler.subw(dest, op1, op2);
                self.branch_test_finalize(cond, dest)
            }
            _ => unreachable!(),
        }
    }

    pub fn branch_sub64(
        &mut self,
        cond: ResultCondition,
        src: impl Into<Operand>,
        dest: u8,
    ) -> Jump {
        self.branch_sub64_rrr(cond, dest, src, dest)
    }

    pub fn branch_sub64_rrr(
        &mut self,
        cond: ResultCondition,
        op1: u8,
        op2: impl Into<Operand>,
        dest: u8,
    ) -> Jump {
        match op2.into() {
            Operand::Imm32(imm) => self.branch_add64_rrr(cond, op1, Operand::Imm32(-imm), dest),

            Operand::Register(op2) => {
                if cond == ResultCondition::Overflow {
                    return self.branch_for_arithmetic_overflow::<64>(
                        ArithmeticOperation::Subtraction,
                        op1,
                        Err(op2),
                        dest,
                    );
                }

                self.assembler.sub(dest, op1, op2);
                self.branch_test_finalize(cond, dest)
            }
            _ => unreachable!(),
        }
    }

    pub fn branch_mul32(
        &mut self,
        cond: ResultCondition,
        src: impl Into<Operand>,
        dest: u8,
    ) -> Jump {
        self.branch_mul32_rrr(cond, dest, src, dest)
    }

    pub fn branch_mul32_rrr(
        &mut self,
        cond: ResultCondition,
        op1: u8,
        op2: impl Into<Operand>,
        dest: u8,
    ) -> Jump {
        match op2.into() {
            Operand::Imm32(imm) => {
                if cond == ResultCondition::Overflow {
                    return self.branch_for_arithmetic_overflow::<32>(
                        ArithmeticOperation::Multiplication,
                        op1,
                        Ok(imm),
                        dest,
                    );
                }

                self.assembler
                    .sign_extend(Self::MEMORY_TEMP_REGISTER, op1, 32);
                self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                self.assembler.mul(
                    Self::DATA_TEMP_REGISTER,
                    Self::MEMORY_TEMP_REGISTER,
                    Self::DATA_TEMP_REGISTER,
                );
                self.assembler
                    .mask_register(dest, Self::DATA_TEMP_REGISTER, 32);
                self.branch_test_finalize(cond, Self::DATA_TEMP_REGISTER)
            }

            Operand::Register(op2) => {
                if cond == ResultCondition::Overflow {
                    return self.branch_for_arithmetic_overflow::<32>(
                        ArithmeticOperation::Multiplication,
                        op1,
                        Err(op2),
                        dest,
                    );
                }

                self.assembler
                    .sign_extend(Self::MEMORY_TEMP_REGISTER, op1, 32);
                self.assembler
                    .sign_extend(Self::DATA_TEMP_REGISTER, op2, 32);
                self.assembler.mul(
                    Self::DATA_TEMP_REGISTER,
                    Self::MEMORY_TEMP_REGISTER,
                    Self::DATA_TEMP_REGISTER,
                );
                self.assembler
                    .mask_register(dest, Self::DATA_TEMP_REGISTER, 32);

                self.branch_test_finalize(cond, Self::DATA_TEMP_REGISTER)
            }
            _ => unreachable!(),
        }
    }

    pub fn branch_mul64(
        &mut self,
        cond: ResultCondition,
        src: impl Into<Operand>,
        dest: u8,
    ) -> Jump {
        self.branch_mul32_rrr(cond, dest, src, dest)
    }

    pub fn branch_mul64_rrr(
        &mut self,
        cond: ResultCondition,
        op1: u8,
        op2: impl Into<Operand>,
        dest: u8,
    ) -> Jump {
        match op2.into() {
            Operand::Imm32(imm) => {
                if cond == ResultCondition::Overflow {
                    return self.branch_for_arithmetic_overflow::<32>(
                        ArithmeticOperation::Multiplication,
                        op1,
                        Ok(imm),
                        dest,
                    );
                }

                self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                self.assembler.mul(dest, op1, Self::DATA_TEMP_REGISTER);
                self.branch_test_finalize(cond, dest)
            }

            Operand::Register(op2) => {
                if cond == ResultCondition::Overflow {
                    return self.branch_for_arithmetic_overflow::<64>(
                        ArithmeticOperation::Multiplication,
                        op1,
                        Err(op2),
                        dest,
                    );
                }

                self.assembler.mul(dest, op1, op2);
                self.branch_test_finalize(cond, dest)
            }
            _ => unreachable!(),
        }
    }

    pub fn branch_neg32(&mut self, cond: ResultCondition, src_dest: u8) -> Jump {
        self.branch_sub32_rrr(cond, zero, src_dest, src_dest)
    }

    pub fn branch_neg64(&mut self, cond: ResultCondition, src_dest: u8) -> Jump {
        self.branch_sub64_rrr(cond, zero, src_dest, src_dest)
    }

    pub fn branch_test8(
        &mut self,
        cond: ResultCondition,
        address: impl Into<Operand>,
        imm: i32,
    ) -> Jump {
        match address.into() {
            Operand::Address(address) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.lbu(
                    Self::MEMORY_TEMP_REGISTER,
                    resolution.base,
                    resolution.offset,
                );

                if !IImmediate::is_valid(imm) {
                    self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                    self.assembler.and(
                        Self::DATA_TEMP_REGISTER,
                        Self::MEMORY_TEMP_REGISTER,
                        Self::DATA_TEMP_REGISTER,
                    );
                } else {
                    self.assembler
                        .andi(Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, imm);
                }

                self.assembler
                    .sign_extend(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 8);

                self.branch_test_finalize(cond, Self::DATA_TEMP_REGISTER)
            }

            Operand::AbsoluteAddress(address) => {
                self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                self.assembler
                    .lbu(Self::MEMORY_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, 0);

                if !IImmediate::is_valid(imm) {
                    self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                    self.assembler.and(
                        Self::DATA_TEMP_REGISTER,
                        Self::MEMORY_TEMP_REGISTER,
                        Self::DATA_TEMP_REGISTER,
                    );
                } else {
                    self.assembler
                        .andi(Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, imm);
                }

                self.assembler
                    .sign_extend(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 8);
                self.branch_test_finalize(cond, Self::DATA_TEMP_REGISTER)
            }

            Operand::BaseIndex(address) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.lbu(
                    Self::MEMORY_TEMP_REGISTER,
                    resolution.base,
                    resolution.offset,
                );

                if !IImmediate::is_valid(imm) {
                    self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                    self.assembler.and(
                        Self::DATA_TEMP_REGISTER,
                        Self::MEMORY_TEMP_REGISTER,
                        Self::DATA_TEMP_REGISTER,
                    );
                } else {
                    self.assembler
                        .andi(Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, imm);
                }

                self.assembler
                    .sign_extend(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 8);

                self.branch_test_finalize(cond, Self::DATA_TEMP_REGISTER)
            }
            Operand::ExtendedAddress(address) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.lbu(
                    Self::MEMORY_TEMP_REGISTER,
                    resolution.base,
                    resolution.offset,
                );

                if !IImmediate::is_valid(imm) {
                    self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                    self.assembler.and(
                        Self::DATA_TEMP_REGISTER,
                        Self::MEMORY_TEMP_REGISTER,
                        Self::DATA_TEMP_REGISTER,
                    );
                } else {
                    self.assembler
                        .andi(Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, imm);
                }

                self.assembler
                    .sign_extend(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 8);

                self.branch_test_finalize(cond, Self::DATA_TEMP_REGISTER)
            }

            _ => unreachable!(),
        }
    }

    pub fn branch_test16(
        &mut self,
        cond: ResultCondition,
        address: impl Into<Operand>,
        imm: i32,
    ) -> Jump {
        match address.into() {
            Operand::Address(address) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.lbu(
                    Self::MEMORY_TEMP_REGISTER,
                    resolution.base,
                    resolution.offset,
                );

                if !IImmediate::is_valid(imm) {
                    self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                    self.assembler.and(
                        Self::DATA_TEMP_REGISTER,
                        Self::MEMORY_TEMP_REGISTER,
                        Self::DATA_TEMP_REGISTER,
                    );
                } else {
                    self.assembler
                        .andi(Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, imm);
                }

                self.assembler
                    .sign_extend(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 16);

                self.branch_test_finalize(cond, Self::DATA_TEMP_REGISTER)
            }

            Operand::AbsoluteAddress(address) => {
                self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                self.assembler
                    .lbu(Self::MEMORY_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, 0);

                if !IImmediate::is_valid(imm) {
                    self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                    self.assembler.and(
                        Self::DATA_TEMP_REGISTER,
                        Self::MEMORY_TEMP_REGISTER,
                        Self::DATA_TEMP_REGISTER,
                    );
                } else {
                    self.assembler
                        .andi(Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, imm);
                }

                self.assembler
                    .sign_extend(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 16);
                self.branch_test_finalize(cond, Self::DATA_TEMP_REGISTER)
            }

            Operand::BaseIndex(address) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.lbu(
                    Self::MEMORY_TEMP_REGISTER,
                    resolution.base,
                    resolution.offset,
                );

                if !IImmediate::is_valid(imm) {
                    self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                    self.assembler.and(
                        Self::DATA_TEMP_REGISTER,
                        Self::MEMORY_TEMP_REGISTER,
                        Self::DATA_TEMP_REGISTER,
                    );
                } else {
                    self.assembler
                        .andi(Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, imm);
                }

                self.assembler
                    .sign_extend(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 16);

                self.branch_test_finalize(cond, Self::DATA_TEMP_REGISTER)
            }
            Operand::ExtendedAddress(address) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.lbu(
                    Self::MEMORY_TEMP_REGISTER,
                    resolution.base,
                    resolution.offset,
                );

                if !IImmediate::is_valid(imm) {
                    self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                    self.assembler.and(
                        Self::DATA_TEMP_REGISTER,
                        Self::MEMORY_TEMP_REGISTER,
                        Self::DATA_TEMP_REGISTER,
                    );
                } else {
                    self.assembler
                        .andi(Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, imm);
                }

                self.assembler
                    .sign_extend(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 16);

                self.branch_test_finalize(cond, Self::DATA_TEMP_REGISTER)
            }

            _ => unreachable!(),
        }
    }

    pub fn branch_test32(
        &mut self,
        cond: ResultCondition,
        lhs: impl Into<Operand>,
        rhs: impl Into<Operand>,
    ) -> Jump {
        match (lhs.into(), rhs.into()) {
            (Operand::Register(lhs), Operand::Register(rhs)) => {
                self.assembler
                    .zero_extend(Self::DATA_TEMP_REGISTER, lhs, 32);
                self.assembler
                    .and(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, rhs);
                self.assembler
                    .sign_extend(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 32);
                self.branch_test_finalize(cond, Self::DATA_TEMP_REGISTER)
            }

            (Operand::Register(lhs), Operand::Imm32(imm)) => {
                self.assembler
                    .zero_extend(Self::DATA_TEMP_REGISTER, lhs, 32);
                if !IImmediate::is_valid(imm) {
                    self.load_immediate32(imm, Self::MEMORY_TEMP_REGISTER);
                    self.assembler.and(
                        Self::DATA_TEMP_REGISTER,
                        Self::DATA_TEMP_REGISTER,
                        Self::MEMORY_TEMP_REGISTER,
                    );
                } else {
                    self.assembler
                        .andi(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, imm);
                }

                self.assembler
                    .sign_extend(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 32);
                self.branch_test_finalize(cond, Self::DATA_TEMP_REGISTER)
            }

            (Operand::Address(address), Operand::Imm32(imm)) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.lwu(
                    Self::MEMORY_TEMP_REGISTER,
                    resolution.base,
                    resolution.offset,
                );

                if !IImmediate::is_valid(imm) {
                    self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                    self.assembler.and(
                        Self::DATA_TEMP_REGISTER,
                        Self::MEMORY_TEMP_REGISTER,
                        Self::DATA_TEMP_REGISTER,
                    );
                } else {
                    self.assembler
                        .andi(Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, imm);
                }

                self.assembler
                    .sign_extend(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 32);
                self.branch_test_finalize(cond, Self::DATA_TEMP_REGISTER)
            }

            (Operand::AbsoluteAddress(address), Operand::Imm32(imm)) => {
                self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                self.assembler
                    .lwu(Self::MEMORY_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, 0);

                if !IImmediate::is_valid(imm) {
                    self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                    self.assembler.and(
                        Self::DATA_TEMP_REGISTER,
                        Self::MEMORY_TEMP_REGISTER,
                        Self::DATA_TEMP_REGISTER,
                    );
                } else {
                    self.assembler
                        .andi(Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, imm);
                }

                self.assembler
                    .sign_extend(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 32);
                self.branch_test_finalize(cond, Self::DATA_TEMP_REGISTER)
            }

            _ => unreachable!(),
        }
    }

    pub fn branch_test64(
        &mut self,
        cond: ResultCondition,
        lhs: impl Into<Operand>,
        rhs: impl Into<Operand>,
    ) -> Jump {
        match (lhs.into(), rhs.into()) {
            (Operand::Register(lhs), Operand::Register(rhs)) => {
                self.assembler
                    .and(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, rhs);
                self.branch_test_finalize(cond, Self::DATA_TEMP_REGISTER)
            }

            (Operand::Register(lhs), Operand::Imm32(imm)) => {
                if !IImmediate::is_valid(imm) {
                    self.load_immediate32(imm, Self::MEMORY_TEMP_REGISTER);
                    self.assembler
                        .and(Self::DATA_TEMP_REGISTER, lhs, Self::MEMORY_TEMP_REGISTER);
                } else {
                    self.assembler.andi(Self::DATA_TEMP_REGISTER, lhs, imm);
                }

                self.branch_test_finalize(cond, Self::DATA_TEMP_REGISTER)
            }

            (Operand::Register(lhs), Operand::Imm64(imm)) => {
                if !IImmediate::is_valid(imm) {
                    self.load_immediate64(imm, Self::MEMORY_TEMP_REGISTER);
                    self.assembler
                        .and(Self::DATA_TEMP_REGISTER, lhs, Self::MEMORY_TEMP_REGISTER);
                } else {
                    self.assembler
                        .andi(Self::DATA_TEMP_REGISTER, lhs, imm as i32);
                }

                self.branch_test_finalize(cond, Self::DATA_TEMP_REGISTER)
            }

            (Operand::Address(address), Operand::Register(rhs)) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.ld(
                    Self::MEMORY_TEMP_REGISTER,
                    resolution.base,
                    resolution.offset,
                );
                self.assembler
                    .and(Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, rhs);
                self.branch_test_finalize(cond, Self::DATA_TEMP_REGISTER)
            }

            (Operand::Address(address), Operand::Imm32(imm)) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.ld(
                    Self::MEMORY_TEMP_REGISTER,
                    resolution.base,
                    resolution.offset,
                );

                if !IImmediate::is_valid(imm) {
                    self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                    self.assembler.and(
                        Self::DATA_TEMP_REGISTER,
                        Self::MEMORY_TEMP_REGISTER,
                        Self::DATA_TEMP_REGISTER,
                    );
                } else {
                    self.assembler
                        .andi(Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, imm);
                }

                self.branch_test_finalize(cond, Self::DATA_TEMP_REGISTER)
            }

            (Operand::AbsoluteAddress(address), Operand::Imm32(imm)) => {
                self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                self.assembler
                    .ld(Self::MEMORY_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, 0);

                if !IImmediate::is_valid(imm) {
                    self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                    self.assembler.and(
                        Self::DATA_TEMP_REGISTER,
                        Self::MEMORY_TEMP_REGISTER,
                        Self::DATA_TEMP_REGISTER,
                    );
                } else {
                    self.assembler
                        .andi(Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, imm);
                }

                self.branch_test_finalize(cond, Self::DATA_TEMP_REGISTER)
            }

            _ => unreachable!(),
        }
    }

    pub fn move_with_patch(&mut self, src: impl Into<Operand>, dest: u8) -> DataLabelPtr {
        match src.into() {
            Operand::Imm32(imm) => {
                let imml = ImmediateLoader::new_placeholder(imm as i64);
                let label = DataLabelPtr::new(self);
                imml.move_into(&mut self.assembler, dest);
                label
            }

            Operand::Imm64(imm) => {
                let imml = ImmediateLoader::new_placeholder(imm);
                let label = DataLabelPtr::new(self);
                imml.move_into(&mut self.assembler, dest);
                label
            }

            _ => unreachable!(),
        }
    }

    pub fn store_ptr_with_patch(&mut self, initial_value: isize, address: Address) -> DataLabelPtr {
        let imml = ImmediateLoader::new_placeholder(initial_value as i64);
        let label = DataLabelPtr::new(self);
        imml.move_into(&mut self.assembler, Self::DATA_TEMP_REGISTER);
        let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
        self.assembler
            .sd(resolution.base, Self::DATA_TEMP_REGISTER, resolution.offset);
        label
    }

    pub fn branch32_with_patch(
        &mut self,
        cond: RelationalCondition,
        address: Address,
        initial_right_value: i32,
    ) -> (Jump, DataLabelPtr) {
        let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
        self.assembler.lw(
            Self::MEMORY_TEMP_REGISTER,
            resolution.base,
            resolution.offset,
        );
        let data_label = self.move_with_patch(
            Operand::Imm32(initial_right_value),
            Self::DATA_TEMP_REGISTER,
        );
        (
            self.make_branch(cond, Self::MEMORY_TEMP_REGISTER, Self::DATA_TEMP_REGISTER),
            data_label,
        )
    }

    pub fn branch_ptr_with_patch(
        &mut self,
        cond: RelationalCondition,
        lhs: impl Into<Operand>,
        initial_right_value: isize,
    ) -> (Jump, DataLabelPtr) {
        match lhs.into() {
            Operand::Address(address) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.ld(
                    Self::MEMORY_TEMP_REGISTER,
                    resolution.base,
                    resolution.offset,
                );
                let data_label =
                    self.move_with_patch(initial_right_value as i64, Self::DATA_TEMP_REGISTER);
                (
                    self.make_branch(cond, Self::MEMORY_TEMP_REGISTER, Self::DATA_TEMP_REGISTER),
                    data_label,
                )
            }

            Operand::Register(lhs) => {
                let data_label =
                    self.move_with_patch(initial_right_value as i64, Self::DATA_TEMP_REGISTER);
                (
                    self.make_branch(cond, lhs, Self::DATA_TEMP_REGISTER),
                    data_label,
                )
            }

            _ => unreachable!(),
        }
    }

    pub fn patchable_branch64(
        &mut self,
        cond: RelationalCondition,
        reg: u8,
        right: impl Into<Operand>,
    ) -> PatchableJump {
        PatchableJump(self.branch64(cond, reg, right))
    }

    pub fn branch_float(&mut self, cond: DoubleCondition, lhs: u8, rhs: u8) -> Jump {
        self.branch_fp::<32, false>(cond, lhs, rhs)
    }

    pub fn branch_double(&mut self, cond: DoubleCondition, lhs: u8, rhs: u8) -> Jump {
        self.branch_fp::<64, false>(cond, lhs, rhs)
    }

    pub fn branch_double_non_zero(&mut self, reg: u8, _: u8) -> Jump {
        self.assembler
            .fcvt_si2fp::<64>(Self::FP_TEMP_REGISTER, zero, FPRoundingMode::DYN);
        self.branch_fp::<64, true>(
            DoubleCondition::NotEqualAndOrdered,
            reg,
            Self::FP_TEMP_REGISTER,
        )
    }

    pub fn branch_double_zero_or_nan(&mut self, reg: u8, _: u8) -> Jump {
        self.assembler
            .fcvt_si2fp::<64>(Self::FP_TEMP_REGISTER, zero, FPRoundingMode::DYN);
        self.branch_fp::<64, true>(
            DoubleCondition::EqualOrUnordered,
            reg,
            Self::FP_TEMP_REGISTER,
        )
    }

    pub fn branch_truncate_double_to_int32(
        &mut self,
        src: u8,
        dest: u8,
        branch_if_truncate_successful: bool,
    ) -> Jump {
        self.assembler
            .fcvt_fp2si::<64>(dest, src, FPRoundingMode::RTZ);
        self.assembler
            .sign_extend(Self::DATA_TEMP_REGISTER, dest, 32);
        if branch_if_truncate_successful {
            self.assembler
                .sltu(Self::DATA_TEMP_REGISTER, zero, Self::DATA_TEMP_REGISTER);
        } else {
            self.assembler
                .sltiu(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 1);
        }

        self.assembler.mask_register(dest, dest, 32);
        self.make_branch(
            RelationalCondition::NotEqual,
            Self::DATA_TEMP_REGISTER,
            zero,
        )
    }

    pub fn branch_convert_double_to_int32(
        &mut self,
        src: u8,
        dest: u8,
        failure_cases: &mut JumpList,
        _: u8,
        neg_zero_check: bool,
    ) {
        self.assembler
            .fcvt_fp2si::<64>(Self::DATA_TEMP_REGISTER, src, FPRoundingMode::DYN);
        self.assembler.fcvt_si2fp::<64>(
            Self::FP_TEMP_REGISTER,
            Self::DATA_TEMP_REGISTER,
            FPRoundingMode::DYN,
        );
        self.assembler
            .mask_register(dest, Self::DATA_TEMP_REGISTER, 32);
        failure_cases.push(self.branch_fp::<64, false>(
            DoubleCondition::NotEqualOrUnordered,
            src,
            Self::FP_TEMP_REGISTER,
        ));

        if neg_zero_check {
            let result_is_non_zero = self.make_branch(
                RelationalCondition::NotEqual,
                Self::DATA_TEMP_REGISTER,
                zero,
            );
            self.assembler.fmv_fp2i::<64>(Self::DATA_TEMP_REGISTER, src);
            failure_cases.push(self.make_branch(
                RelationalCondition::LessThan,
                Self::DATA_TEMP_REGISTER,
                zero,
            ));
            result_is_non_zero.link(self);
        }
    }

    pub fn get_effective_address(&mut self, address: BaseIndex, dest: u8) {
        let resolution = self.resolve_address(address, dest);
        self.assembler
            .addi(dest, resolution.base, resolution.offset);
    }

    pub fn load_float(&mut self, address: impl Into<Operand>, dest: u8) {
        match address.into() {
            Operand::Address(address) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.flw(dest, resolution.base, resolution.offset);
            }

            Operand::BaseIndex(address) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.flw(dest, resolution.base, resolution.offset);
            }

            Operand::AbsoluteAddress(address) => {
                self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                self.assembler.flw(dest, Self::MEMORY_TEMP_REGISTER, 0);
            }

            _ => unreachable!(),
        }
    }

    pub fn load_double(&mut self, address: impl Into<Operand>, dest: u8) {
        match address.into() {
            Operand::Address(address) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.fld(dest, resolution.base, resolution.offset);
            }

            Operand::BaseIndex(address) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.fld(dest, resolution.base, resolution.offset);
            }

            Operand::AbsoluteAddress(address) => {
                self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                self.assembler.fld(dest, Self::MEMORY_TEMP_REGISTER, 0);
            }

            _ => unreachable!(),
        }
    }

    pub fn store_float(&mut self, src: u8, address: impl Into<Operand>) {
        match address.into() {
            Operand::Address(address) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.fsw(resolution.base, src, resolution.offset);
            }

            Operand::BaseIndex(address) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.fsw(resolution.base, src, resolution.offset);
            }

            Operand::AbsoluteAddress(address) => {
                self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                self.assembler.fsw(Self::MEMORY_TEMP_REGISTER, src, 0);
            }

            _ => unreachable!(),
        }
    }

    pub fn store_double(&mut self, src: u8, address: impl Into<Operand>) {
        match address.into() {
            Operand::Address(address) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.fsd(resolution.base, src, resolution.offset);
            }

            Operand::BaseIndex(address) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.fsd(resolution.base, src, resolution.offset);
            }

            Operand::AbsoluteAddress(address) => {
                self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                self.assembler.fsd(Self::MEMORY_TEMP_REGISTER, src, 0);
            }

            _ => unreachable!(),
        }
    }

    pub fn add_float(&mut self, op1: u8, op2: u8, dest: u8) {
        self.assembler.fadd::<32>(dest, op1, op2);
    }

    pub fn add_float_rr(&mut self, src: u8, dest: u8) {
        self.assembler.fadd::<32>(dest, dest, src);
    }

    pub fn add_double(&mut self, op1: u8, op2: u8, dest: u8) {
        self.assembler.fadd::<64>(dest, op1, op2);
    }

    pub fn add_double_rr(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::AbsoluteAddress(address) => {
                self.load_double(address, Self::FP_TEMP_REGISTER);
                self.assembler
                    .fadd::<64>(dest, dest, Self::FP_TEMP_REGISTER);
            }

            Operand::Register(reg) => {
                self.assembler.fadd::<64>(dest, dest, reg);
            }

            _ => unreachable!(),
        }
    }

    pub fn sub_float(&mut self, op1: u8, op2: u8, dest: u8) {
        self.assembler.fsub::<32>(dest, op1, op2);
    }

    pub fn sub_float_rr(&mut self, src: u8, dest: u8) {
        self.assembler.fsub::<32>(dest, dest, src);
    }

    pub fn sub_double(&mut self, op1: u8, op2: u8, dest: u8) {
        self.assembler.fsub::<64>(dest, op1, op2);
    }

    pub fn sub_double_rr(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::AbsoluteAddress(address) => {
                self.load_double(address, Self::FP_TEMP_REGISTER);
                self.assembler
                    .fsub::<64>(dest, dest, Self::FP_TEMP_REGISTER);
            }

            Operand::Register(reg) => {
                self.assembler.fsub::<64>(dest, dest, reg);
            }

            _ => unreachable!(),
        }
    }

    pub fn mul_float(&mut self, op1: u8, op2: u8, dest: u8) {
        self.assembler.fmul::<32>(dest, op1, op2);
    }

    pub fn mul_float_rr(&mut self, src: u8, dest: u8) {
        self.assembler.fmul::<32>(dest, dest, src);
    }

    pub fn mul_double(&mut self, op1: u8, op2: u8, dest: u8) {
        self.assembler.fmul::<64>(dest, op1, op2);
    }

    pub fn mul_double_rr(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::AbsoluteAddress(address) => {
                self.load_double(address, Self::FP_TEMP_REGISTER);
                self.assembler
                    .fmul::<64>(dest, dest, Self::FP_TEMP_REGISTER);
            }

            Operand::Register(reg) => {
                self.assembler.fmul::<64>(dest, dest, reg);
            }

            Operand::Address(address) => {
                self.load_double(address, Self::FP_TEMP_REGISTER);
                self.assembler
                    .fmul::<64>(dest, dest, Self::FP_TEMP_REGISTER);
            }

            _ => unreachable!(),
        }
    }

    pub fn div_float(&mut self, op1: u8, op2: u8, dest: u8) {
        self.assembler.fdiv::<32>(dest, op1, op2);
    }

    pub fn div_float_rr(&mut self, src: u8, dest: u8) {
        self.assembler.fdiv::<32>(dest, dest, src);
    }

    pub fn div_double(&mut self, op1: u8, op2: u8, dest: u8) {
        self.assembler.fdiv::<64>(dest, op1, op2);
    }

    pub fn div_double_rr(&mut self, src: u8, dest: u8) {
        self.assembler.fdiv::<64>(dest, dest, src)
    }

    pub fn sqrt_float(&mut self, src: u8, dest: u8) {
        self.assembler.fsqrt::<32>(dest, src);
    }

    pub fn sqrt_double(&mut self, src: u8, dest: u8) {
        self.assembler.fsqrt::<64>(dest, src);
    }

    pub fn abs_float(&mut self, src: u8, dest: u8) {
        self.assembler.fsgnjx::<32>(dest, src, src);
    }

    pub fn abs_double(&mut self, src: u8, dest: u8) {
        self.assembler.fsgnjx::<64>(dest, src, src);
    }

    pub fn ceil_float(&mut self, src: u8, dest: u8) {
        self.round_fp::<32>(src, dest, FPRoundingMode::RUP)
    }

    pub fn ceil_double(&mut self, src: u8, dest: u8) {
        self.round_fp::<64>(src, dest, FPRoundingMode::RUP)
    }

    pub fn floor_float(&mut self, src: u8, dest: u8) {
        self.round_fp::<32>(src, dest, FPRoundingMode::RDN)
    }

    pub fn floor_double(&mut self, src: u8, dest: u8) {
        self.round_fp::<64>(src, dest, FPRoundingMode::RDN)
    }

    pub fn round_to_nearest_int_float(&mut self, src: u8, dest: u8) {
        self.round_fp::<32>(src, dest, FPRoundingMode::RNE)
    }

    pub fn round_to_nearest_int_double(&mut self, src: u8, dest: u8) {
        self.round_fp::<64>(src, dest, FPRoundingMode::RNE)
    }

    pub fn round_toward_zero_float(&mut self, src: u8, dest: u8) {
        self.round_fp::<32>(src, dest, FPRoundingMode::RTZ)
    }

    pub fn round_toward_zero_double(&mut self, src: u8, dest: u8) {
        self.round_fp::<64>(src, dest, FPRoundingMode::RTZ)
    }

    pub fn and_float(&mut self, op1: u8, op2: u8, dest: u8) {
        self.assembler.fmv_fp2i::<32>(Self::DATA_TEMP_REGISTER, op1);
        self.assembler
            .fmv_fp2i::<32>(Self::MEMORY_TEMP_REGISTER, op2);
        self.assembler.and(
            Self::DATA_TEMP_REGISTER,
            Self::DATA_TEMP_REGISTER,
            Self::MEMORY_TEMP_REGISTER,
        );
        self.assembler
            .fmv_i2fp::<32>(dest, Self::DATA_TEMP_REGISTER);
    }

    pub fn and_double(&mut self, op1: u8, op2: u8, dest: u8) {
        self.assembler.fmv_fp2i::<64>(Self::DATA_TEMP_REGISTER, op1);
        self.assembler
            .fmv_fp2i::<64>(Self::MEMORY_TEMP_REGISTER, op2);
        self.assembler.and(
            Self::DATA_TEMP_REGISTER,
            Self::DATA_TEMP_REGISTER,
            Self::MEMORY_TEMP_REGISTER,
        );
        self.assembler
            .fmv_i2fp::<64>(dest, Self::DATA_TEMP_REGISTER);
    }

    pub fn or_float(&mut self, op1: u8, op2: u8, dest: u8) {
        self.assembler.fmv_fp2i::<32>(Self::DATA_TEMP_REGISTER, op1);
        self.assembler
            .fmv_fp2i::<32>(Self::MEMORY_TEMP_REGISTER, op2);
        self.assembler.or(
            Self::DATA_TEMP_REGISTER,
            Self::DATA_TEMP_REGISTER,
            Self::MEMORY_TEMP_REGISTER,
        );
        self.assembler
            .fmv_i2fp::<32>(dest, Self::DATA_TEMP_REGISTER);
    }

    pub fn or_double(&mut self, op1: u8, op2: u8, dest: u8) {
        self.assembler.fmv_fp2i::<64>(Self::DATA_TEMP_REGISTER, op1);
        self.assembler
            .fmv_fp2i::<64>(Self::MEMORY_TEMP_REGISTER, op2);
        self.assembler.or(
            Self::DATA_TEMP_REGISTER,
            Self::DATA_TEMP_REGISTER,
            Self::MEMORY_TEMP_REGISTER,
        );
        self.assembler
            .fmv_i2fp::<64>(dest, Self::DATA_TEMP_REGISTER);
    }

    pub fn negate_float(&mut self, src: u8, dest: u8) {
        self.assembler.fsgnjn::<32>(dest, src, src);
    }

    pub fn negate_double(&mut self, src: u8, dest: u8) {
        self.assembler.fsgnjn::<64>(dest, src, src);
    }

    pub fn compare_float(&mut self, cond: DoubleCondition, lhs: u8, rhs: u8, dest: u8) {
        self.compare_fp::<32>(cond, lhs, rhs, dest);
    }

    pub fn compare_double(&mut self, cond: DoubleCondition, lhs: u8, rhs: u8, dest: u8) {
        self.compare_fp::<64>(cond, lhs, rhs, dest);
    }

    pub fn convert_int32_to_float(&mut self, src: u8, dest: u8) {
        self.assembler
            .fcvt_si2fp::<32>(dest, src, FPRoundingMode::DYN)
    }

    pub fn convert_int32_to_double(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Imm32(imm) => {
                self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                self.assembler
                    .fcvt_si2fp::<64>(dest, Self::DATA_TEMP_REGISTER, FPRoundingMode::DYN)
            }

            Operand::Register(src) => {
                self.assembler
                    .fcvt_si2fp::<64>(dest, src, FPRoundingMode::DYN)
            }
            _ => unreachable!(),
        }
    }

    pub fn convert_int64_to_float(&mut self, src: u8, dest: u8) {
        self.assembler
            .fcvt_si2fp::<32>(dest, src, FPRoundingMode::DYN)
    }

    pub fn convert_int64_to_double(&mut self, src: u8, dest: u8) {
        self.assembler
            .fcvt_si2fp::<64>(dest, src, FPRoundingMode::DYN)
    }

    pub fn convert_uint64_to_float(&mut self, src: u8, dest: u8) {
        self.assembler
            .fcvt_ui2fp::<32>(dest, src, FPRoundingMode::DYN)
    }

    pub fn convert_uint64_to_double(&mut self, src: u8, dest: u8) {
        self.assembler
            .fcvt_ui2fp::<64>(dest, src, FPRoundingMode::DYN)
    }

    pub fn convert_float_to_double(&mut self, src: u8, dest: u8) {
        self.assembler.fmv_fp2i::<32>(Self::DATA_TEMP_REGISTER, src);
        self.assembler
            .fmv_i2fp::<64>(dest, Self::DATA_TEMP_REGISTER);
        self.assembler
            .fcvt_fp2fp::<64, 32>(dest, dest, FPRoundingMode::DYN);
    }

    pub fn convert_double_to_float(&mut self, src: u8, dest: u8) {
        self.assembler
            .fcvt_fp2fp::<32, 64>(dest, src, FPRoundingMode::DYN);
    }

    pub fn truncate_float_to_int32(&mut self, src: u8, dest: u8) {
        self.assembler
            .fcvt_fp2si::<32>(dest, src, FPRoundingMode::RTZ);
        self.assembler.mask_register(dest, dest, 32);
    }

    pub fn truncate_float_to_uint32(&mut self, src: u8, dest: u8) {
        self.assembler
            .fcvt_fp2ui::<32>(dest, src, FPRoundingMode::RTZ);
        self.assembler.mask_register(dest, dest, 32);
    }

    pub fn truncate_float_to_int64(&mut self, src: u8, dest: u8) {
        self.assembler
            .fcvt_fp2si::<32>(dest, src, FPRoundingMode::RTZ);
    }

    pub fn truncate_float_to_uint64_rr(&mut self, src: u8, dest: u8) {
        self.assembler
            .fcvt_fp2ui::<32>(dest, src, FPRoundingMode::RTZ);
    }

    pub fn truncate_float_to_uint64(&mut self, src: u8, dest: u8, _: u8, _: u8) {
        self.assembler
            .fcvt_fp2ui::<32>(dest, src, FPRoundingMode::RTZ);
    }

    pub fn truncate_double_to_int32(&mut self, src: u8, dest: u8) {
        self.assembler
            .fcvt_fp2si::<64>(dest, src, FPRoundingMode::RTZ);
        self.assembler.mask_register(dest, dest, 32);
    }

    pub fn truncate_double_to_uint32(&mut self, src: u8, dest: u8) {
        self.assembler
            .fcvt_fp2ui::<64>(dest, src, FPRoundingMode::RTZ);
        self.assembler.mask_register(dest, dest, 32);
    }

    pub fn truncate_double_to_int64(&mut self, src: u8, dest: u8) {
        self.assembler
            .fcvt_fp2si::<64>(dest, src, FPRoundingMode::RTZ);
    }

    pub fn truncate_double_to_uint64(&mut self, src: u8, dest: u8) {
        self.assembler
            .fcvt_fp2ui::<64>(dest, src, FPRoundingMode::RTZ);
    }

    pub fn memory_fence(&mut self) {
        self.assembler
            .fence(&[MemoryOperation::RW], &[MemoryOperation::RW]);
    }

    pub fn store_fence(&mut self) {
        self.assembler
            .fence(&[MemoryOperation::W], &[MemoryOperation::RW]);
    }

    pub fn load_fence(&mut self) {
        self.assembler
            .fence(&[MemoryOperation::R], &[MemoryOperation::RW]);
    }

    pub fn branch_atomic_weak_cas_impl<const BIT_SIZE: usize>(
        &mut self,
        is_failure: bool,
        expected_and_clobbered: u8,
        new_value: u8,
        address: BaseIndex,
    ) -> JumpList {
        // There's no 8-bit or 16-bit load-reserved and store-conditional instructions in RISC-V,
        // so we have to implement the operations through the 32-bit versions, with a limited amount
        // of usable registers.
        let mut failure = JumpList::new();
        // We clobber the expected-value register with the XOR difference between the expected
        // and the new value, also clipping the result to the desired number of bits.
        self.assembler
            .xor(expected_and_clobbered, expected_and_clobbered, new_value);
        // The BaseIndex address is resolved into the memory temp. The address is aligned to the 4-byte
        // boundary, and the remainder is used to calculate the shift amount for the exact position
        // in the 32-bit word where the target bit pattern is located.
        let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
        self.assembler.addi(
            Self::MEMORY_TEMP_REGISTER,
            resolution.base,
            resolution.offset,
        );
        self.assembler
            .andi(Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, 0b11);
        self.assembler.andi(
            Self::MEMORY_TEMP_REGISTER,
            Self::MEMORY_TEMP_REGISTER,
            !0b11,
        );
        self.assembler
            .slli(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 3);
        self.assembler
            .addi(Self::MEMORY_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, 32);

        // The XOR value in the expected-value register is shifted into the appropriate position in
        // the upper half of the register. The shift value is OR-ed into the lower half.
        self.assembler.sll(
            expected_and_clobbered,
            expected_and_clobbered,
            Self::DATA_TEMP_REGISTER,
        );
        self.assembler.or(
            expected_and_clobbered,
            expected_and_clobbered,
            Self::DATA_TEMP_REGISTER,
        );

        // The 32-bit value is loaded through the load-reserve instruction, and then shifted into the
        // upper 32 bits of the register. XOR against the expected-value register will, in the upper
        // 32 bits of the register, produce the 32-bit word with the expected value replaced by the new one.
        self.assembler.lrw(
            Self::DATA_TEMP_REGISTER,
            Self::MEMORY_TEMP_REGISTER,
            &[MemoryAccess::Acquire],
        );
        self.assembler
            .slli(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 32);
        self.assembler.xor(
            expected_and_clobbered,
            Self::DATA_TEMP_REGISTER,
            expected_and_clobbered,
        );
        // We still have to validate that the expected value, after XOR, matches the new one. The upper
        // 32 bits of the expected-value register are shifted by the pre-prepared shift amount stored
        // in the lower half of that same register. This works becasue the shift amount is read only from
        // the bottom 6 bits of the shift-amount register. XOR-ing against the new-value register and shifting
        // back left should leave is with a zero value, in which case the expected-value bit pattern matched
        // the one that was loaded from memory. If non-zero, the failure branch is taken.
        self.assembler.srl(
            Self::DATA_TEMP_REGISTER,
            expected_and_clobbered,
            expected_and_clobbered,
        );
        self.assembler.xor(
            Self::DATA_TEMP_REGISTER,
            Self::DATA_TEMP_REGISTER,
            new_value,
        );
        self.assembler.slli(
            Self::DATA_TEMP_REGISTER,
            Self::DATA_TEMP_REGISTER,
            64 - BIT_SIZE as u32,
        );
        failure.push(self.make_branch(
            RelationalCondition::NotEqual,
            Self::DATA_TEMP_REGISTER,
            zero,
        ));

        // The corresponding store-conditional remains. The 32-bit word, containing the new value after
        // the XOR, is located in the upper 32 bits of the expected-value register. That can be shifted
        // down and then used in the store-conditional instruction.
        self.assembler
            .srli(expected_and_clobbered, expected_and_clobbered, 32);
        self.assembler.scw(
            Self::DATA_TEMP_REGISTER,
            Self::MEMORY_TEMP_REGISTER,
            expected_and_clobbered,
            &[MemoryAccess::AcquireRelease],
        );

        // On successful store, the temp register will have a zero value, and a non-zero value otherwise.
        // Branches are produced accordingly.
        if is_failure {
            failure.push(self.make_branch(
                RelationalCondition::NotEqual,
                Self::DATA_TEMP_REGISTER,
                zero,
            ));
            failure
        } else {
            let success =
                self.make_branch(RelationalCondition::Equal, Self::DATA_TEMP_REGISTER, zero);
            failure.link(self);
            let mut ls = JumpList::new();
            ls.push(success);
            ls
        }
    }

    pub fn breakpoint(&mut self) {
        self.assembler.ebreak();
    }

    pub fn set_carry(&mut self, _: u8) {}

    pub fn call_op(&mut self, op: Option<impl Into<Operand>>) -> Call {
        if let Some(op) = op {
            match op.into() {
                Operand::Register(reg) => {
                    self.assembler.jalr(x1, reg, 0);
                    Call::new(self.assembler.label(), Call::NONE)
                }

                Operand::Address(address) => {
                    let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                    self.assembler
                        .ld(Self::DATA_TEMP_REGISTER, resolution.base, resolution.offset);

                    self.assembler.jalr(x1, Self::DATA_TEMP_REGISTER, 0);

                    Call::new(self.assembler.label(), Call::NONE)
                }

                Operand::AbsoluteAddress(address) => {
                    self.load_immediate64(address.ptr as _, Self::DATA_TEMP_REGISTER);
                    self.assembler.jalr(x1, Self::DATA_TEMP_REGISTER, 0);

                    Call::new(self.assembler.label(), Call::NONE)
                }

                Operand::Imm64(imm) => {
                    self.load_immediate64(imm, Self::DATA_TEMP_REGISTER);
                    self.assembler.jalr(x1, Self::DATA_TEMP_REGISTER, 0);

                    Call::new(self.assembler.label(), Call::NONE)
                }

                _ => unreachable!(),
            }
        } else {
            let label = self.assembler.label();
            self.assembler.pointer_call_placeholder(|asm| {
                asm.addi(Self::DATA_TEMP_REGISTER, zero, 0);
                asm.jalr(x1, Self::DATA_TEMP_REGISTER, 0);
            });

            Call::new(label, Call::LINKABLE)
        }
    }

    pub fn push(&mut self, src: u8) {
        self.assembler.addi(sp, sp, -8);
        self.assembler.sd(sp, src, 0);
    }

    pub fn push_pair(&mut self, src1: u8, src2: u8) {
        self.assembler.addi(sp, sp, -16);
        self.assembler.sd(sp, src1, 0);
        self.assembler.sd(sp, src2, 8);
    }

    pub fn pop(&mut self, dest: u8) {
        self.assembler.ld(dest, sp, 0);
        self.assembler.addi(sp, sp, 8);
    }

    pub fn pop_pair(&mut self, dest1: u8, dest2: u8) {
        self.assembler.ld(dest1, sp, 0);
        self.assembler.ld(dest2, sp, 8);
        self.assembler.addi(sp, sp, 16);
    }

    pub fn move_conditionally32(
        &mut self,
        cond: RelationalCondition,
        left: u8,
        right: u8,
        src: u8,
        dest: u8,
    ) {
        self.assembler
            .sign_extend(Self::DATA_TEMP_REGISTER, left, 32);
        self.assembler
            .sign_extend(Self::MEMORY_TEMP_REGISTER, right, 32);

        self.branch_for_move_conditionally(
            Self::invert(cond),
            Self::DATA_TEMP_REGISTER,
            Self::MEMORY_TEMP_REGISTER,
            8,
        );
        self.assembler.addi(dest, src, 0);
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
        self.assembler
            .sign_extend(Self::DATA_TEMP_REGISTER, left, 32);
        match right.into() {
            Operand::Register(right) => {
                self.assembler
                    .sign_extend(Self::MEMORY_TEMP_REGISTER, right, 32);
            }

            Operand::Imm32(imm) => self.load_immediate32(imm, Self::MEMORY_TEMP_REGISTER),

            _ => unreachable!(),
        }

        self.branch_for_move_conditionally(
            cond,
            Self::DATA_TEMP_REGISTER,
            Self::MEMORY_TEMP_REGISTER,
            12,
        );
        self.assembler.addi(dest, then_case, 0);
        self.assembler.jal(zero, 8);
        self.assembler.addi(dest, else_case, 0);
    }

    pub fn move_conditionally64(
        &mut self,
        cond: RelationalCondition,
        left: u8,
        right: u8,
        src: u8,
        dest: u8,
    ) {
        self.branch_for_move_conditionally(Self::invert(cond), left, right, 8);
        self.assembler.addi(dest, src, 0);
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
        let right = match right.into() {
            Operand::Register(right) => right,

            Operand::Imm32(imm) => {
                self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                Self::DATA_TEMP_REGISTER
            }

            _ => unreachable!(),
        };

        self.branch_for_move_conditionally(cond, left, right, 12);
        self.assembler.addi(dest, then_case, 0);
        self.assembler.jal(zero, 8);
        self.assembler.addi(dest, else_case, 0);
    }

    pub fn move_conditionally_float(
        &mut self,
        cond: DoubleCondition,
        lhs: u8,
        rhs: u8,
        src: u8,
        dest: u8,
    ) {
        let invcond_branch = self.branch_fp::<32, true>(cond, lhs, rhs);
        self.assembler.addi(dest, src, 0);
        invcond_branch.link(self);
    }

    pub fn move_conditionally_float_then_else(
        &mut self,
        cond: DoubleCondition,
        lhs: u8,
        rhs: u8,
        then_case: u8,
        else_case: u8,
        dest: u8,
    ) {
        let invcond_branch = self.branch_fp::<32, true>(cond, lhs, rhs);
        self.assembler.addi(dest, then_case, 0);
        let end = self.jump();
        invcond_branch.link(self);
        self.assembler.addi(dest, else_case, 0);
        end.link(self);
    }

    pub fn move_conditionally_double(
        &mut self,
        cond: DoubleCondition,
        lhs: u8,
        rhs: u8,
        src: u8,
        dest: u8,
    ) {
        let invcond_branch = self.branch_fp::<64, true>(cond, lhs, rhs);
        self.assembler.addi(dest, src, 0);
        invcond_branch.link(self);
    }

    pub fn move_conditionally_double_then_else(
        &mut self,
        cond: DoubleCondition,
        lhs: u8,
        rhs: u8,
        then_case: u8,
        else_case: u8,
        dest: u8,
    ) {
        let invcond_branch = self.branch_fp::<64, true>(cond, lhs, rhs);
        self.assembler.addi(dest, then_case, 0);
        let end = self.jump();
        invcond_branch.link(self);
        self.assembler.addi(dest, else_case, 0);
        end.link(self);
    }

    pub fn move_conditionally_test32(
        &mut self,
        cond: ResultCondition,
        left: u8,
        right: impl Into<Operand>,
        src: u8,
        dest: u8,
    ) {
        let right = match right.into() {
            Operand::Register(right) => right,

            Operand::Imm32(imm) => {
                self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                Self::DATA_TEMP_REGISTER
            }

            _ => unreachable!(),
        };

        self.assembler.and(Self::DATA_TEMP_REGISTER, left, right);
        self.assembler
            .sign_extend(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 32);
        self.test_finalize(cond, Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER);
        self.assembler.beq(Self::DATA_TEMP_REGISTER, zero, 8);
        self.assembler.addi(dest, src, 0);
    }

    pub fn move_conditionally_test32_then_else(
        &mut self,
        cond: ResultCondition,
        left: u8,
        right: impl Into<Operand>,
        then_case: u8,
        else_case: u8,
        dest: u8,
    ) {
        let right = match right.into() {
            Operand::Register(right) => right,

            Operand::Imm32(imm) => {
                self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                Self::DATA_TEMP_REGISTER
            }

            _ => unreachable!(),
        };

        self.assembler.and(Self::DATA_TEMP_REGISTER, left, right);
        self.assembler
            .sign_extend(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 32);
        self.test_finalize(cond, Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER);
        self.assembler.beq(Self::DATA_TEMP_REGISTER, zero, 12);
        self.assembler.addi(dest, then_case, 0);
        self.assembler.jal(zero, 8);
        self.assembler.addi(dest, else_case, 0);
    }

    pub fn move_conditionally_test64(
        &mut self,
        cond: ResultCondition,
        left: u8,
        right: impl Into<Operand>,
        src: u8,
        dest: u8,
    ) {
        let right = match right.into() {
            Operand::Register(right) => right,

            Operand::Imm32(imm) => {
                self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                Self::DATA_TEMP_REGISTER
            }

            _ => unreachable!(),
        };

        self.assembler.and(Self::DATA_TEMP_REGISTER, left, right);
        self.test_finalize(cond, Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER);
        self.assembler.beq(Self::DATA_TEMP_REGISTER, zero, 8);
        self.assembler.addi(dest, src, 0);
    }

    pub fn move_conditionally_test64_then_else(
        &mut self,
        cond: ResultCondition,
        left: u8,
        right: impl Into<Operand>,
        then_case: u8,
        else_case: u8,
        dest: u8,
    ) {
        let right = match right.into() {
            Operand::Register(right) => right,

            Operand::Imm32(imm) => {
                self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                Self::DATA_TEMP_REGISTER
            }

            _ => unreachable!(),
        };

        self.assembler.and(Self::DATA_TEMP_REGISTER, left, right);
        self.test_finalize(cond, Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER);
        self.assembler.beq(Self::DATA_TEMP_REGISTER, zero, 12);
        self.assembler.addi(dest, then_case, 0);
        self.assembler.jal(zero, 8);
        self.assembler.addi(dest, else_case, 0);
    }

    pub fn move_double_conditionally32(
        &mut self,
        cond: RelationalCondition,
        lhs: u8,
        rhs: impl Into<Operand>,
        then_case: u8,
        else_case: u8,
        dest: u8,
    ) {
        self.assembler
            .sign_extend(Self::DATA_TEMP_REGISTER, lhs, 32);

        match rhs.into() {
            Operand::Register(rhs) => {
                self.assembler
                    .sign_extend(Self::MEMORY_TEMP_REGISTER, rhs, 32);
            }

            Operand::Imm32(imm) => {
                self.load_immediate32(imm, Self::MEMORY_TEMP_REGISTER);
            }

            _ => unreachable!(),
        }

        self.branch_for_move_conditionally(
            Self::invert(cond),
            Self::DATA_TEMP_REGISTER,
            Self::MEMORY_TEMP_REGISTER,
            12,
        );
        self.assembler.fsgnj::<64>(dest, then_case, then_case);
        self.assembler.jal(zero, 8);
        self.assembler.fsgnj::<64>(dest, else_case, else_case);
    }

    pub fn move_double_conditionally64(
        &mut self,
        cond: RelationalCondition,
        lhs: u8,
        rhs: impl Into<Operand>,
        then_case: u8,
        else_case: u8,
        dest: u8,
    ) {
        let rhs = match rhs.into() {
            Operand::Register(rhs) => rhs,

            Operand::Imm32(imm) => {
                self.load_immediate32(imm, Self::MEMORY_TEMP_REGISTER);
                Self::MEMORY_TEMP_REGISTER
            }

            _ => unreachable!(),
        };

        self.branch_for_move_conditionally(Self::invert(cond), lhs, rhs, 12);
        self.assembler.fsgnj::<64>(dest, then_case, then_case);
        self.assembler.jal(zero, 8);
        self.assembler.fsgnj::<64>(dest, else_case, else_case);
    }

    pub fn move_double_conditionally_float(
        &mut self,
        cond: DoubleCondition,
        lhs: u8,
        rhs: u8,
        then_case: u8,
        else_case: u8,
        dest: u8,
    ) {
        let invcond_branch = self.branch_fp::<32, true>(cond, lhs, rhs);
        self.assembler.fsgnj::<64>(dest, then_case, then_case);
        let end = self.jump();
        invcond_branch.link(self);
        self.assembler.fsgnj::<64>(dest, else_case, else_case);
        end.link(self);
    }

    pub fn move_double_conditionally_double(
        &mut self,
        cond: DoubleCondition,
        lhs: u8,
        rhs: u8,
        then_case: u8,
        else_case: u8,
        dest: u8,
    ) {
        let invcond_branch = self.branch_fp::<64, true>(cond, lhs, rhs);
        self.assembler.fsgnj::<64>(dest, then_case, then_case);
        let end = self.jump();
        invcond_branch.link(self);
        self.assembler.fsgnj::<64>(dest, else_case, else_case);
        end.link(self);
    }

    pub fn move_double_conditionally_test32(
        &mut self,
        cond: ResultCondition,
        lhs: u8,
        rhs: impl Into<Operand>,
        then_case: u8,
        else_case: u8,
        dest: u8,
    ) {
        let rhs = match rhs.into() {
            Operand::Register(rhs) => {
                self.assembler
                    .sign_extend(Self::MEMORY_TEMP_REGISTER, rhs, 32);
                Self::MEMORY_TEMP_REGISTER
            }

            Operand::Imm32(imm) => {
                self.load_immediate32(imm, Self::MEMORY_TEMP_REGISTER);
                Self::MEMORY_TEMP_REGISTER
            }

            _ => unreachable!(),
        };

        self.assembler
            .sign_extend(Self::DATA_TEMP_REGISTER, lhs, 32);

        self.assembler.and(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, rhs);
        self.test_finalize(cond, Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER);
        
        self.assembler.beq(Self::DATA_TEMP_REGISTER, zero, 12);
        self.assembler.fsgnj::<64>(dest, then_case, then_case);
        self.assembler.jal(zero, 8);
        self.assembler.fsgnj::<64>(dest, else_case, else_case);
    }

    pub fn move_double_conditionally_test64(
        &mut self,
        cond: ResultCondition,
        lhs: u8,
        rhs: impl Into<Operand>,
        then_case: u8,
        else_case: u8,
        dest: u8,
    ) {
        let rhs = match rhs.into() {
            Operand::Register(rhs) => rhs,

            Operand::Imm32(imm) => {
                self.load_immediate32(imm, Self::MEMORY_TEMP_REGISTER);
                Self::MEMORY_TEMP_REGISTER
            }

            _ => unreachable!(),
        };

        self.assembler.and(Self::DATA_TEMP_REGISTER, lhs, rhs);
        self.test_finalize(cond, Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER);
        
        self.assembler.beq(Self::DATA_TEMP_REGISTER, zero, 12);
        self.assembler.fsgnj::<64>(dest, then_case, then_case);
        self.assembler.jal(zero, 8);
        self.assembler.fsgnj::<64>(dest, else_case, else_case);
    }
}

enum ArithmeticOperation {
    Addition,
    Subtraction,
    Multiplication,
}

struct AddressResolution {
    base: u8,
    offset: i32,
}

// private methods
impl MacroAssemblerRISCV64 {
    fn load_immediate32(&mut self, imm: i32, dest: u8) {
        ImmediateLoader::new(imm as i64).move_into(&mut self.assembler, dest);
    }

    fn load_immediate64(&mut self, imm: i64, dest: u8) {
        ImmediateLoader::new(imm).move_into(&mut self.assembler, dest);
    }

    fn resolve_address(
        &mut self,
        address: impl Into<Operand>,
        destination: u8,
    ) -> AddressResolution {
        match address.into() {
            Operand::BaseIndex(address) => {
                if address.offset != 0 {
                    if is_valid32::<12>(address.offset) {
                        if address.scale != Scale::TimesOne {
                            self.assembler
                                .slli(destination, address.index, address.scale as _);
                            self.assembler.add(destination, address.base, destination);
                        } else {
                            self.assembler.add(destination, address.base, address.index);
                        }

                        return AddressResolution {
                            base: destination,
                            offset: address.offset,
                        };
                    }

                    if address.scale != Scale::TimesOne {
                        let scale = address.scale as u32;
                        let upper_offset = address.offset >> scale;
                        let lower_offset = address.offset & ((1 << scale) - 1);

                        if !is_valid32::<12>(upper_offset) {
                            let imml = ImmediateLoader::new(upper_offset as _);
                            imml.move_into(&mut self.assembler, destination);
                            self.assembler.add(destination, address.index, destination);
                        } else {
                            self.assembler
                                .addi(destination, address.index, upper_offset);
                        }

                        self.assembler.slli(destination, destination, scale);
                        self.assembler.ori(destination, destination, lower_offset);
                    } else {
                        let imml = ImmediateLoader::new(address.offset as _);
                        imml.move_into(&mut self.assembler, destination);
                        self.assembler.add(destination, address.base, destination);
                    }

                    self.assembler.add(destination, address.base, destination);

                    return AddressResolution {
                        base: destination,
                        offset: 0,
                    };
                }

                if address.scale != Scale::TimesOne {
                    self.assembler
                        .slli(destination, address.index, address.scale as _);
                    self.assembler.add(destination, address.base, destination);
                } else {
                    self.assembler.add(destination, address.base, address.index);
                }

                AddressResolution {
                    base: destination,
                    offset: 0,
                }
            }

            Operand::Address(address) => {
                if is_valid32::<12>(address.offset) {
                    return AddressResolution {
                        base: address.base,
                        offset: address.offset,
                    };
                }

                let mut value = address.offset as u32;

                if (value & (1 << 11)) != 0 {
                    value += 1 << 12;
                }

                self.assembler.lui(destination, value as i32);
                self.assembler
                    .addi(destination, destination, (value & ((1 << 12) - 1)) as _);
                self.assembler.add(destination, address.base, destination);

                AddressResolution {
                    base: destination,
                    offset: 0,
                }
            }

            Operand::ExtendedAddress(address) => {
                if is_valid64::<12>(address.offset as i64) {
                    return AddressResolution {
                        base: address.base,
                        offset: address.offset as _,
                    };
                }

                let imml = ImmediateLoader::new(address.offset as _);
                imml.move_into(&mut self.assembler, destination);
                self.assembler.add(destination, address.base, destination);

                AddressResolution {
                    base: destination,
                    offset: 0,
                }
            }

            _ => unreachable!(),
        }
    }

    fn make_branch(&mut self, condition: RelationalCondition, lhs: u8, rhs: u8) -> Jump {
        let label = self.assembler.label();

        self.assembler.branch_placeholder(|asm| match condition {
            RelationalCondition::Equal => {
                asm.beq(lhs, rhs, 0);
            }

            RelationalCondition::NotEqual => {
                asm.bne(lhs, rhs, 0);
            }

            RelationalCondition::LessThan => {
                asm.blt(lhs, rhs, 0);
            }

            RelationalCondition::LessThanOrEqual => {
                asm.bge(lhs, rhs, 0);
            }

            RelationalCondition::GreaterThan => {
                asm.blt(rhs, lhs, 0);
            }

            RelationalCondition::GreaterThanOrEqual => {
                asm.bge(lhs, rhs, 0);
            }

            RelationalCondition::Above => {
                asm.bltu(rhs, lhs, 0);
            }

            RelationalCondition::AboveOrEqual => {
                asm.bgeu(lhs, rhs, 0);
            }

            RelationalCondition::Below => {
                asm.bltu(lhs, rhs, 0);
            }

            RelationalCondition::BelowOrEqual => {
                asm.bgeu(rhs, lhs, 0);
            }
        });

        Jump::new(label)
    }

    fn branch_test_finalize(&mut self, cond: ResultCondition, src: u8) -> Jump {
        match cond {
            ResultCondition::Overflow => {
                unreachable!()
            }
            ResultCondition::Signed => self.make_branch(RelationalCondition::LessThan, src, zero),

            ResultCondition::Zero => self.make_branch(RelationalCondition::Equal, src, zero),

            ResultCondition::NonZero => self.make_branch(RelationalCondition::NotEqual, src, zero),

            ResultCondition::PositiveOrZero => {
                self.make_branch(RelationalCondition::GreaterThanOrEqual, src, zero)
            }
        }
    }

    fn branch_for_arithmetic_overflow<const BITSIZE: usize>(
        &mut self,
        op: ArithmeticOperation,
        op1: u8,
        op2: Result<i32, u8>,
        dest: u8,
    ) -> Jump {
        if BITSIZE == 32 {
            self.assembler
                .sign_extend(Self::DATA_TEMP_REGISTER, op1, 32);

            match op2 {
                Ok(imm) => {
                    self.load_immediate32(imm, Self::MEMORY_TEMP_REGISTER);
                }
                Err(reg) => {
                    self.assembler
                        .sign_extend(Self::MEMORY_TEMP_REGISTER, reg, 32);
                }
            }

            if dest == Self::DATA_TEMP_REGISTER || dest == Self::MEMORY_TEMP_REGISTER {
                let other_temp = if dest == Self::DATA_TEMP_REGISTER {
                    Self::MEMORY_TEMP_REGISTER
                } else {
                    Self::DATA_TEMP_REGISTER
                };

                match op {
                    ArithmeticOperation::Addition => {
                        self.assembler.add(
                            dest,
                            Self::DATA_TEMP_REGISTER,
                            Self::MEMORY_TEMP_REGISTER,
                        );
                    }
                    ArithmeticOperation::Multiplication => {
                        self.assembler.mul(
                            dest,
                            Self::DATA_TEMP_REGISTER,
                            Self::MEMORY_TEMP_REGISTER,
                        );
                    }
                    ArithmeticOperation::Subtraction => {
                        self.assembler.sub(
                            dest,
                            Self::DATA_TEMP_REGISTER,
                            Self::MEMORY_TEMP_REGISTER,
                        );
                    }
                }

                self.assembler.sign_extend(other_temp, dest, 32);
                self.assembler.xor(other_temp, dest, other_temp);
                //self.assembler.mask_register(dest, dest, 32);

                return self.make_branch(RelationalCondition::NotEqual, other_temp, zero);
            }

            match op {
                ArithmeticOperation::Addition => {
                    self.assembler.add(
                        Self::DATA_TEMP_REGISTER,
                        Self::DATA_TEMP_REGISTER,
                        Self::MEMORY_TEMP_REGISTER,
                    );
                }
                ArithmeticOperation::Multiplication => {
                    self.assembler.mul(
                        Self::DATA_TEMP_REGISTER,
                        Self::DATA_TEMP_REGISTER,
                        Self::MEMORY_TEMP_REGISTER,
                    );
                }
                ArithmeticOperation::Subtraction => {
                    self.assembler.sub(
                        Self::DATA_TEMP_REGISTER,
                        Self::DATA_TEMP_REGISTER,
                        Self::MEMORY_TEMP_REGISTER,
                    );
                }
            }

            self.assembler
                .mask_register(dest, Self::DATA_TEMP_REGISTER, 32);
            self.assembler
                .sign_extend(Self::MEMORY_TEMP_REGISTER, dest, 32);

            return self.make_branch(
                RelationalCondition::NotEqual,
                Self::DATA_TEMP_REGISTER,
                Self::MEMORY_TEMP_REGISTER,
            );
        }

        let rop2 = if let Ok(imm) = op2 {
            self.load_immediate32(imm, Self::MEMORY_TEMP_REGISTER);
            Self::MEMORY_TEMP_REGISTER
        } else if let Err(reg) = op2 {
            reg
        } else {
            unreachable!()
        };

        match op {
            ArithmeticOperation::Addition => {
                if op1 == dest && rop2 == dest {
                    self.assembler.slli(Self::MEMORY_TEMP_REGISTER, dest, 1);
                    self.assembler
                        .xor(Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, dest);
                    self.mov(Self::MEMORY_TEMP_REGISTER, dest);
                } else {
                    self.assembler.xor(Self::DATA_TEMP_REGISTER, op1, rop2);
                    self.assembler
                        .xori(Self::MEMORY_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, -1);

                    self.assembler.add(dest, op1, rop2);
                    self.assembler.xor(
                        Self::MEMORY_TEMP_REGISTER,
                        if op1 == dest { rop2 } else { op1 },
                        Self::MEMORY_TEMP_REGISTER,
                    );
                    self.assembler.and(
                        Self::MEMORY_TEMP_REGISTER,
                        Self::MEMORY_TEMP_REGISTER,
                        Self::DATA_TEMP_REGISTER,
                    );
                }

                self.make_branch(
                    RelationalCondition::LessThan,
                    Self::DATA_TEMP_REGISTER,
                    zero,
                )
            }

            ArithmeticOperation::Subtraction => {
                if op1 == dest && rop2 == dest {
                    self.mov(zero, dest);
                    self.mov(zero, Self::DATA_TEMP_REGISTER);
                } else {
                    self.assembler.xor(Self::DATA_TEMP_REGISTER, op1, rop2);

                    self.assembler.sub(dest, op1, rop2);

                    if op1 == dest {
                        self.assembler.xor(Self::MEMORY_TEMP_REGISTER, rop2, dest);
                        self.assembler.xori(
                            Self::MEMORY_TEMP_REGISTER,
                            Self::MEMORY_TEMP_REGISTER,
                            -1,
                        );
                    } else {
                        self.assembler.xor(Self::MEMORY_TEMP_REGISTER, op1, dest);
                    }

                    self.assembler.and(
                        Self::DATA_TEMP_REGISTER,
                        Self::DATA_TEMP_REGISTER,
                        Self::MEMORY_TEMP_REGISTER,
                    );
                }

                self.make_branch(
                    RelationalCondition::LessThan,
                    Self::DATA_TEMP_REGISTER,
                    zero,
                )
            }

            ArithmeticOperation::Multiplication => {
                self.assembler.mulh(Self::DATA_TEMP_REGISTER, op1, rop2);
                self.assembler.mul(dest, op1, rop2);
                self.assembler.srai(Self::MEMORY_TEMP_REGISTER, dest, 0x3f);

                self.make_branch(
                    RelationalCondition::NotEqual,
                    Self::DATA_TEMP_REGISTER,
                    Self::MEMORY_TEMP_REGISTER,
                )
            }
        }
    }

    fn branch_for_move_conditionally(
        &mut self,
        cond: RelationalCondition,
        lhs: u8,
        rhs: u8,
        offset: i32,
    ) {
        match cond {
            RelationalCondition::Equal => self.assembler.beq(lhs, rhs, offset),
            RelationalCondition::NotEqual => self.assembler.bne(lhs, rhs, offset),
            RelationalCondition::Above => self.assembler.bltu(rhs, lhs, offset),
            RelationalCondition::AboveOrEqual => self.assembler.bgeu(lhs, rhs, offset),
            RelationalCondition::Below => self.assembler.bltu(lhs, rhs, offset),
            RelationalCondition::BelowOrEqual => self.assembler.bgeu(rhs, lhs, offset),
            RelationalCondition::GreaterThan => self.assembler.blt(rhs, lhs, offset),
            RelationalCondition::GreaterThanOrEqual => self.assembler.bge(lhs, rhs, offset),
            RelationalCondition::LessThan => self.assembler.blt(lhs, rhs, offset),
            RelationalCondition::LessThanOrEqual => self.assembler.bge(rhs, lhs, offset),
        }
    }

    fn compare_finalize(&mut self, cond: RelationalCondition, lhs: u8, rhs: u8, dest: u8) {
        match cond {
            RelationalCondition::Equal => {
                self.assembler.xor(dest, lhs, rhs);
                self.assembler.sltiu(dest, dest, 1);
            }

            RelationalCondition::NotEqual => {
                self.assembler.xor(dest, lhs, rhs);
                self.assembler.sltu(dest, zero, dest);
            }

            RelationalCondition::Above => {
                self.assembler.sltu(dest, rhs, lhs);
            }

            RelationalCondition::AboveOrEqual => {
                self.assembler.sltu(dest, lhs, rhs);
                self.assembler.xori(dest, dest, 1);
            }

            RelationalCondition::Below => {
                self.assembler.sltu(dest, lhs, rhs);
            }

            RelationalCondition::BelowOrEqual => {
                self.assembler.sltu(dest, rhs, lhs);
                self.assembler.xori(dest, dest, 1);
            }

            RelationalCondition::GreaterThan => {
                self.assembler.slt(dest, rhs, lhs);
            }

            RelationalCondition::GreaterThanOrEqual => {
                self.assembler.slt(dest, lhs, rhs);
                self.assembler.xori(dest, dest, 1);
            }

            RelationalCondition::LessThan => {
                self.assembler.slt(dest, lhs, rhs);
            }

            RelationalCondition::LessThanOrEqual => {
                self.assembler.slt(dest, rhs, lhs);
                self.assembler.xori(dest, dest, 1);
            }
        }
    }

    fn test_finalize(&mut self, cond: ResultCondition, src: u8, dest: u8) {
        match cond {
            ResultCondition::Overflow
            | ResultCondition::Signed
            | ResultCondition::PositiveOrZero => {
                unreachable!()
            }

            ResultCondition::Zero => {
                self.assembler.sltiu(dest, src, 1);
            }

            ResultCondition::NonZero => {
                self.assembler.sltu(dest, zero, src);
            }
        }
    }

    fn branch_fp<const FP_SIZE: usize, const INVERT: bool>(
        &mut self,
        cond: DoubleCondition,
        lhs: u8,
        rhs: u8,
    ) -> Jump {
        let mut unordered_jumps = JumpList::new();

        self.assembler
            .fclass::<FP_SIZE>(Self::DATA_TEMP_REGISTER, lhs);
        self.assembler.andi(
            Self::DATA_TEMP_REGISTER,
            Self::DATA_TEMP_REGISTER,
            0b1100000000,
        );
        unordered_jumps.push(self.make_branch(
            RelationalCondition::NotEqual,
            Self::DATA_TEMP_REGISTER,
            zero,
        ));

        self.assembler
            .fclass::<FP_SIZE>(Self::DATA_TEMP_REGISTER, rhs);
        self.assembler.andi(
            Self::DATA_TEMP_REGISTER,
            Self::DATA_TEMP_REGISTER,
            0b1100000000,
        );
        unordered_jumps.push(self.make_branch(
            RelationalCondition::NotEqual,
            Self::DATA_TEMP_REGISTER,
            zero,
        ));

        match cond {
            DoubleCondition::EqualAndOrdered | DoubleCondition::EqualOrUnordered => {
                self.assembler
                    .feq::<FP_SIZE>(Self::DATA_TEMP_REGISTER, lhs, rhs);
            }

            DoubleCondition::NotEqualAndOrdered | DoubleCondition::NotEqualOrUnordered => {
                self.assembler
                    .feq::<FP_SIZE>(Self::DATA_TEMP_REGISTER, lhs, rhs);
                self.assembler
                    .xori(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 1);
            }

            DoubleCondition::GreaterThanAndOrdered | DoubleCondition::GreaterThanOrUnordered => {
                self.assembler
                    .flt::<FP_SIZE>(Self::DATA_TEMP_REGISTER, rhs, lhs);
            }

            DoubleCondition::GreaterThanOrEqualAndOrdered
            | DoubleCondition::GreaterThanOrEqualOrUnordered => {
                self.assembler
                    .fle::<FP_SIZE>(Self::DATA_TEMP_REGISTER, rhs, lhs);
                self.assembler
                    .xori(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 1);
            }

            DoubleCondition::LessThanAndOrdered | DoubleCondition::LessThanOrUnordered => {
                self.assembler
                    .flt::<FP_SIZE>(Self::DATA_TEMP_REGISTER, lhs, rhs);
            }

            DoubleCondition::LessThanOrEqualAndOrdered
            | DoubleCondition::LessThanOrEqualOrUnordered => {
                self.assembler
                    .fle::<FP_SIZE>(Self::DATA_TEMP_REGISTER, lhs, rhs);
            }
        }

        let end = self.jump();

        unordered_jumps.link(self);

        match cond {
            DoubleCondition::EqualAndOrdered
            | DoubleCondition::NotEqualAndOrdered
            | DoubleCondition::GreaterThanAndOrdered
            | DoubleCondition::GreaterThanOrEqualAndOrdered
            | DoubleCondition::LessThanAndOrdered
            | DoubleCondition::LessThanOrEqualAndOrdered => {
                self.assembler.addi(Self::DATA_TEMP_REGISTER, zero, 0);
            }

            _ => {
                self.assembler.addi(Self::DATA_TEMP_REGISTER, zero, 1);
            }
        }

        end.link(self);

        self.make_branch(
            if INVERT {
                RelationalCondition::Equal
            } else {
                RelationalCondition::NotEqual
            },
            Self::DATA_TEMP_REGISTER,
            zero,
        )
    }

    fn round_fp<const FP_SIZE: usize>(&mut self, src: u8, dest: u8, rm: FPRoundingMode) {
        let mut end = JumpList::new();

        // Test the given source register for NaN condition. If detected, it should be
        // propagated to the destination register.
        self.assembler
            .fclass::<FP_SIZE>(Self::DATA_TEMP_REGISTER, src);
        self.assembler.andi(
            Self::DATA_TEMP_REGISTER,
            Self::DATA_TEMP_REGISTER,
            0b1100000000,
        );
        let not_nan = self.make_branch(RelationalCondition::Equal, Self::DATA_TEMP_REGISTER, zero);

        self.assembler.feq::<FP_SIZE>(dest, src, src);
        end.push(self.jump());

        not_nan.link(self);
        self.assembler
            .fsgnjx::<FP_SIZE>(Self::FP_TEMP_REGISTER, src, src);

        // Compare the absolute source value with the maximum representable integer value.
        // Rounding is only possible if the absolute source value is smaller.
        if FP_SIZE == 32 {
            self.assembler
                .addi(Self::DATA_TEMP_REGISTER, zero, 0b10010111);
            self.assembler
                .slli(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 23);
            self.assembler
                .fmv_i2fp::<FP_SIZE>(Self::FP_TEMP_REGISTER2, Self::DATA_TEMP_REGISTER);
        } else {
            self.assembler
                .addi(Self::DATA_TEMP_REGISTER, zero, 0b10000110100);
            self.assembler
                .slli(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 52);
            self.assembler
                .fmv_i2fp::<FP_SIZE>(Self::FP_TEMP_REGISTER2, Self::DATA_TEMP_REGISTER);
        }

        self.assembler.flt::<FP_SIZE>(
            Self::DATA_TEMP_REGISTER,
            Self::FP_TEMP_REGISTER,
            Self::FP_TEMP_REGISTER2,
        );
        let not_roundable =
            self.make_branch(RelationalCondition::Equal, Self::DATA_TEMP_REGISTER, zero);

        let dealiased_src = if src == dest {
            self.assembler
                .fsgnj::<FP_SIZE>(Self::FP_TEMP_REGISTER, src, src);
            Self::FP_TEMP_REGISTER
        } else {
            src
        };
        // Rounding can now be done by roundtripping through a general-purpose register
        // with the desired rounding mode applied.
        if FP_SIZE == 32 {
            self.assembler
                .fcvt_fp2si::<FP_SIZE>(Self::DATA_TEMP_REGISTER, dealiased_src, rm);
            self.assembler
                .fcvt_si2fp::<FP_SIZE>(dest, Self::DATA_TEMP_REGISTER, rm);
        } else {
            self.assembler
                .fcvt_fp2si::<FP_SIZE>(Self::DATA_TEMP_REGISTER, dealiased_src, rm);
            self.assembler
                .fcvt_si2fp::<FP_SIZE>(dest, Self::DATA_TEMP_REGISTER, rm);
        }

        self.assembler.fsgnj::<FP_SIZE>(dest, dest, dealiased_src);
        end.push(self.jump());

        not_roundable.link(self);
        // If not roundable, the value should still be moved over into the destination register.
        if src != dest {
            self.assembler.fsgnj::<FP_SIZE>(dest, src, src);
        }

        end.link(self);
    }

    fn compare_fp<const FP_SIZE: usize>(
        &mut self,
        cond: DoubleCondition,
        lhs: u8,
        rhs: u8,
        dest: u8,
    ) {
        let mut unordered_jumps = JumpList::new();

        // Detect any NaN values that could still yield a positive comparison, depending on the condition.
        self.assembler
            .fclass::<FP_SIZE>(Self::DATA_TEMP_REGISTER, lhs);
        self.assembler.andi(
            Self::DATA_TEMP_REGISTER,
            Self::DATA_TEMP_REGISTER,
            0b1100000000,
        );
        unordered_jumps.push(self.make_branch(
            RelationalCondition::Equal,
            Self::DATA_TEMP_REGISTER,
            zero,
        ));

        self.assembler
            .fclass::<FP_SIZE>(Self::DATA_TEMP_REGISTER, rhs);
        self.assembler.andi(
            Self::DATA_TEMP_REGISTER,
            Self::DATA_TEMP_REGISTER,
            0b1100000000,
        );
        unordered_jumps.push(self.make_branch(
            RelationalCondition::Equal,
            Self::DATA_TEMP_REGISTER,
            zero,
        ));

        match cond {
            DoubleCondition::EqualAndOrdered | DoubleCondition::EqualOrUnordered => {
                self.assembler.feq::<FP_SIZE>(dest, lhs, rhs);
            }

            DoubleCondition::NotEqualAndOrdered | DoubleCondition::NotEqualOrUnordered => {
                self.assembler.feq::<FP_SIZE>(dest, lhs, rhs);
                self.assembler.xori(dest, dest, 1);
            }

            DoubleCondition::GreaterThanAndOrdered | DoubleCondition::GreaterThanOrUnordered => {
                self.assembler.flt::<FP_SIZE>(dest, rhs, lhs);
            }

            DoubleCondition::GreaterThanOrEqualAndOrdered
            | DoubleCondition::GreaterThanOrEqualOrUnordered => {
                self.assembler.fle::<FP_SIZE>(dest, rhs, lhs);
            }

            DoubleCondition::LessThanAndOrdered | DoubleCondition::LessThanOrUnordered => {
                self.assembler.flt::<FP_SIZE>(dest, lhs, rhs);
            }

            DoubleCondition::LessThanOrEqualAndOrdered
            | DoubleCondition::LessThanOrEqualOrUnordered => {
                self.assembler.fle::<FP_SIZE>(dest, lhs, rhs);
            }
        }

        let end = self.jump();

        unordered_jumps.link(self);

        match cond {
            DoubleCondition::EqualAndOrdered
            | DoubleCondition::NotEqualAndOrdered
            | DoubleCondition::GreaterThanAndOrdered
            | DoubleCondition::GreaterThanOrEqualAndOrdered
            | DoubleCondition::LessThanAndOrdered
            | DoubleCondition::LessThanOrEqualAndOrdered => {
                self.assembler.addi(dest, zero, 0);
            }

            DoubleCondition::EqualOrUnordered
            | DoubleCondition::NotEqualOrUnordered
            | DoubleCondition::GreaterThanOrUnordered
            | DoubleCondition::GreaterThanOrEqualOrUnordered
            | DoubleCondition::LessThanOrUnordered
            | DoubleCondition::LessThanOrEqualOrUnordered => {
                self.assembler.addi(dest, zero, 1);
            }
        }

        end.link(self);
    }
}
