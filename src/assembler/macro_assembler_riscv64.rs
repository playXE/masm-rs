use std::ops::{Deref, DerefMut};

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

    pub fn ret(&mut self) {
        self.assembler.jalr(zero, x1, 0);
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
                    self.assembler.lw(Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, 0);
                    self.assembler.addi(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, imm);
                    self.assembler.sw(Self::MEMORY_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 0);
                    return;
                }

                self.assembler.lw(Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, 0);
                self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                self.assembler.add(Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, Self::DATA_TEMP_REGISTER);
                
                self.load_immediate64(address.ptr as _, Self::MEMORY_TEMP_REGISTER);
                self.assembler.sw(Self::MEMORY_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 0);
            }

            (Operand::Imm32(imm), Operand::Address(address)) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);

                if IImmediate::is_valid(imm) {
                    self.assembler.lw(Self::DATA_TEMP_REGISTER, resolution.base, resolution.offset);
                    self.assembler.addi(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, imm);
                    self.assembler.sw(resolution.base, Self::DATA_TEMP_REGISTER, resolution.offset);
                    return;
                }

                self.assembler.lw(Self::MEMORY_TEMP_REGISTER, resolution.base, resolution.offset);
                self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                self.assembler.add(Self::DATA_TEMP_REGISTER, Self::MEMORY_TEMP_REGISTER, Self::DATA_TEMP_REGISTER);
            
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.sw(resolution.base, Self::DATA_TEMP_REGISTER, resolution.offset);
            }

            (Operand::Address(address), Operand::Register(dest)) => {
                let resolution = self.resolve_address(address, Self::MEMORY_TEMP_REGISTER);
                self.assembler.lw(Self::DATA_TEMP_REGISTER, resolution.base, resolution.offset);
                self.assembler.addw(dest, Self::DATA_TEMP_REGISTER, zero);
                self.assembler.mask_register(dest, dest, 32);
            }

            _ => unreachable!(),
        }
    }

    pub fn add32_rrr(&mut self, lhs: impl Into<Operand>, rhs: impl Into<Operand>, dest: impl Into<Operand>) {
        let lhs = lhs.into();
        let rhs = rhs.into();
        let dest = dest.into();

        match (lhs, rhs, dest) {
            (Operand::Register(lhs), Operand::Register(rhs), Operand::Register(dst)) => {
                self.assembler.addw(dst, lhs, rhs);
                self.assembler.mask_register(dst, dst, 32);
            }

            (Operand::Imm32(imm), Operand::Register(op2), Operand::Register(dst)) => {
                if IImmediate::is_valid(imm) {
                    self.assembler.addiw(dst, op2, imm as i32);
                    self.assembler.mask_register(dst, dst, 32);
                    return;
                }

                self.load_immediate32(imm, Self::DATA_TEMP_REGISTER);
                self.assembler.addw(dst, op2, Self::DATA_TEMP_REGISTER);
                self.assembler.mask_register(dst, dst, 32);
            }

            _ => unreachable!(),
        }
    }

    pub fn mov(&mut self, src: impl Into<Operand>, dst: impl Into<Operand>) {
        match dst.into() {
            Operand::Register(dest) => match src.into() {
                Operand::Register(src) => {
                    self.assembler.addi(dest, src, 0);
                }

                Operand::Imm32(imm) => {
                    self.load_immediate32(imm, dest);
                    self.assembler.mask_register(dest, dest, 32);
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

    pub fn jump(&mut self) -> Jump {
        let label = self.assembler.label();

        self.assembler.jump_placeholder(|asm| {
            asm.jal(zero, 0)
        });

        Jump::new(label)
    }

    pub fn breakpoint(&mut self) {}

    pub unsafe fn link_call(_: *mut u8, _: Call, _: *const u8) {}
    pub unsafe fn link_jump(_: *mut u8, _: Jump, _: *const u8) {}
    pub unsafe fn link_pointer(_: *mut u8, _: AssemblerLabel, _: *const u8) {}
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
                self.assembler.mask_register(dest, dest, 32);

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

    fn branch_fp<const FP_SIZE: usize, const INVERT: bool>(&mut self, cond: DoubleCondition, lhs: u8, rhs: u8) -> Jump {
        let mut unordered_jumps = JumpList::new();

        self.assembler.fclass::<FP_SIZE>(Self::DATA_TEMP_REGISTER, lhs);
        self.assembler.andi(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 0b1100000000);
        unordered_jumps.push(self.make_branch(RelationalCondition::NotEqual, Self::DATA_TEMP_REGISTER, zero));

        self.assembler.fclass::<FP_SIZE>(Self::DATA_TEMP_REGISTER, rhs);
        self.assembler.andi(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 0b1100000000);
        unordered_jumps.push(self.make_branch(RelationalCondition::NotEqual, Self::DATA_TEMP_REGISTER, zero));

        match cond {
            DoubleCondition::EqualAndOrdered
            | DoubleCondition::EqualOrUnordered => {
                self.assembler.feq::<FP_SIZE>(Self::DATA_TEMP_REGISTER, lhs, rhs);
            }

            DoubleCondition::NotEqualAndOrdered
            | DoubleCondition::NotEqualOrUnordered => {
                self.assembler.feq::<FP_SIZE>(Self::DATA_TEMP_REGISTER, lhs, rhs);
                self.assembler.xori(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 1);
            }

            DoubleCondition::GreaterThanAndOrdered
            | DoubleCondition::GreaterThanOrUnordered => {
                self.assembler.flt::<FP_SIZE>(Self::DATA_TEMP_REGISTER, rhs, lhs);
            }

            DoubleCondition::GreaterThanOrEqualAndOrdered
            | DoubleCondition::GreaterThanOrEqualOrUnordered => {
                self.assembler.fle::<FP_SIZE>(Self::DATA_TEMP_REGISTER, rhs, lhs);
                self.assembler.xori(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 1);
            }

            DoubleCondition::LessThanAndOrdered
            | DoubleCondition::LessThanOrUnordered => {
                self.assembler.flt::<FP_SIZE>(Self::DATA_TEMP_REGISTER, lhs, rhs);
            }

            DoubleCondition::LessThanOrEqualAndOrdered
            | DoubleCondition::LessThanOrEqualOrUnordered => {
                self.assembler.fle::<FP_SIZE>(Self::DATA_TEMP_REGISTER, lhs, rhs);
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

        self.make_branch(if INVERT {
            RelationalCondition::Equal
        } else {
            RelationalCondition::NotEqual
        }, Self::DATA_TEMP_REGISTER, zero)
    }

    fn round_fp<const FP_SIZE: usize>(&mut self, src: u8, dest: u8, rm: FPRoundingMode) {
        let mut end = JumpList::new();

        // Test the given source register for NaN condition. If detected, it should be
        // propagated to the destination register.
        self.assembler.fclass::<FP_SIZE>(Self::DATA_TEMP_REGISTER, src);
        self.assembler.andi(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 0b1100000000);
        let not_nan = self.make_branch(RelationalCondition::Equal, Self::DATA_TEMP_REGISTER, zero);

        self.assembler.feq::<FP_SIZE>(dest, src, src);
        end.push(self.jump());

        not_nan.link(self);
        self.assembler.fsgnjx::<FP_SIZE>(Self::FP_TEMP_REGISTER, src, src);

        // Compare the absolute source value with the maximum representable integer value.
        // Rounding is only possible if the absolute source value is smaller.
        if FP_SIZE == 32 {
            self.assembler.addi(Self::DATA_TEMP_REGISTER, zero, 0b10010111);
            self.assembler.slli(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 23);
            self.assembler.fmv_i2fp::<FP_SIZE>(Self::FP_TEMP_REGISTER2, Self::DATA_TEMP_REGISTER);
        } else {
            self.assembler.addi(Self::DATA_TEMP_REGISTER, zero, 0b10000110100);
            self.assembler.slli(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 52);
            self.assembler.fmv_i2fp::<FP_SIZE>(Self::FP_TEMP_REGISTER2, Self::DATA_TEMP_REGISTER);
        }

        self.assembler.flt::<FP_SIZE>(Self::DATA_TEMP_REGISTER, Self::FP_TEMP_REGISTER, Self::FP_TEMP_REGISTER2);
        let not_roundable = self.make_branch(RelationalCondition::Equal, Self::DATA_TEMP_REGISTER, zero);

        let dealiased_src = if src == dest {
            self.assembler.fsgnj::<FP_SIZE>(Self::FP_TEMP_REGISTER, src, src);
            Self::FP_TEMP_REGISTER
        } else {
            src 
        };
        // Rounding can now be done by roundtripping through a general-purpose register
        // with the desired rounding mode applied.
        if FP_SIZE == 32 {
            self.assembler.fcvt_fp2si::<FP_SIZE>(Self::DATA_TEMP_REGISTER, dealiased_src, rm);
            self.assembler.fcvt_si2fp::<FP_SIZE>(dest, Self::DATA_TEMP_REGISTER, rm);
        } else {
            self.assembler.fcvt_fp2si::<FP_SIZE>(Self::DATA_TEMP_REGISTER, dealiased_src, rm);
            self.assembler.fcvt_si2fp::<FP_SIZE>(dest, Self::DATA_TEMP_REGISTER, rm);
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

    fn compare_fp<const FP_SIZE: usize>(&mut self, cond: DoubleCondition, lhs: u8, rhs: u8, dest: u8) {
        let mut unordered_jumps = JumpList::new();

         // Detect any NaN values that could still yield a positive comparison, depending on the condition.
        self.assembler.fclass::<FP_SIZE>(Self::DATA_TEMP_REGISTER, lhs);
        self.assembler.andi(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 0b1100000000);
        unordered_jumps.push(self.make_branch(RelationalCondition::Equal, Self::DATA_TEMP_REGISTER, zero));

        self.assembler.fclass::<FP_SIZE>(Self::DATA_TEMP_REGISTER, rhs);
        self.assembler.andi(Self::DATA_TEMP_REGISTER, Self::DATA_TEMP_REGISTER, 0b1100000000);
        unordered_jumps.push(self.make_branch(RelationalCondition::Equal, Self::DATA_TEMP_REGISTER, zero));

        match cond {
            DoubleCondition::EqualAndOrdered
            | DoubleCondition::EqualOrUnordered => {
                self.assembler.feq::<FP_SIZE>(dest, lhs, rhs);
            }

            DoubleCondition::NotEqualAndOrdered
            | DoubleCondition::NotEqualOrUnordered => {
                self.assembler.feq::<FP_SIZE>(dest, lhs, rhs);
                self.assembler.xori(dest, dest, 1);
            }

            DoubleCondition::GreaterThanAndOrdered
            | DoubleCondition::GreaterThanOrUnordered => {
                self.assembler.flt::<FP_SIZE>(dest, rhs, lhs);
            }

            DoubleCondition::GreaterThanOrEqualAndOrdered
            | DoubleCondition::GreaterThanOrEqualOrUnordered => {
                self.assembler.fle::<FP_SIZE>(dest, rhs, lhs);
            }

            DoubleCondition::LessThanAndOrdered
            | DoubleCondition::LessThanOrUnordered => {
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
