//! RISC-V 64 bit assembler implementation
#![allow(non_upper_case_globals, non_camel_case_types)]

use std::mem::size_of;

use super::buffer::{AssemblerBuffer, AssemblerLabel};

pub struct RISCV64Assembler {
    pub buffer: AssemblerBuffer,
    pub index_of_last_watchpoint: i32,
    pub index_of_tail_of_last_watchpoint: i32,
}

impl RISCV64Assembler {
    pub fn buffer(&self) -> &AssemblerBuffer {
        &self.buffer
    }

    pub fn buffer_mut(&mut self) -> &mut AssemblerBuffer {
        &mut self.buffer
    }

    pub fn new() -> Self {
        Self {
            buffer: AssemblerBuffer::new(),
            index_of_last_watchpoint: i32::MIN,
            index_of_tail_of_last_watchpoint: i32::MIN,
        }
    }

    pub fn code_size(&self) -> usize {
        self.buffer.code_size()
    }

    pub fn debug_offset(&self) -> usize {
        self.buffer.debug_offset()
    }
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

macro_rules! decl_xmm {
    (@internal $counter: expr, ($id: ident, $l: expr, $x: expr, $y: expr), $($rest: tt)*) => (
        pub const $id: u8 = $counter;
        decl_xmm!(@internal $counter + 1, $($rest)*);
    );
    (@internal $counter: expr,) => (
        pub const INVALID_FPR: u8 = 0xFF;
    );
    (($id: ident, $l: expr, $x: expr, $y: expr), $($rest: tt)*) => (
        decl_xmm!(@internal 0, ($id, $l, $x, $y), $($rest)*);
    );
}
macro_rules! decl_sp {
    (@internal $counter: expr, ($id: ident, $l: expr, $x: expr, $y: expr), $($rest: tt)*) => (
        pub const $id: u8 = $counter;
        decl_sp!(@internal $counter + 1, $($rest)*);
    );
    (@internal $counter: expr,) => (
        pub const INVALID_SP: u8 = 0xFF;
    );
    (($id: ident, $l: expr, $x: expr, $y: expr), $($rest: tt)*) => (
        decl_sp!(@internal 0, ($id, $l, $x, $y), $($rest)*);
    );
}

for_each_gp_register!(decl_gpr);
for_each_fp_register!(decl_xmm);
for_each_sp_register!(decl_sp);

macro_rules! decl_alias {
    ($(($id: ident, $l: expr, $x: ident)),+) => {
        $(
            pub const $id: u8 = $x;
        )*

    };
}

for_each_register_alias!(decl_alias);

macro_rules! cenum {
    ($($id: ident = $val: expr), *) => {
        $(pub const $id: u32 = $val;)*
    };
}

cenum! {
    LOAD = 0b0000011,
    LOAD_FP = 0b0000111,
    MISC_MEM = 0b0001111,
    OP_IMM = 0b0010011,
    AUIPC = 0b0010111,
    OP_IMM_32 = 0b0011011,
    STORE = 0b0100011,
    STORE_FP = 0b0100111,
    AMO = 0b0101111,
    OP = 0b0110011,
    LUI = 0b0110111,
    OP_32 = 0b0111011,
    MADD = 0b1000011,
    MSUB = 0b1000111,
    NMSUB = 0b1001011,
    NMADD = 0b1001111,
    OP_FP = 0b1010011,
    BRANCH = 0b1100011,
    JALR = 0b1100111,
    JAL = 0b1101111,
    SYSTEM = 0b1110011
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[rustfmt::skip]
pub enum FCVTType {
    W, WU,
    L, LU,
    S, D
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[rustfmt::skip]
pub enum FMVType {
    X, W, D
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[rustfmt::skip]
pub enum FPRoundingMode {
    RNE = 0b000,
    RTZ = 0b001,
    RDN = 0b010,    
    RUP = 0b011,
    RMM = 0b100,
    DYN = 0b111
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[rustfmt::skip]
#[repr(u8)]
pub enum MemoryOperation {
    I = 1 << 3,
    O = 1 << 2,
    R = 1 << 1,
    W = 1 << 0,
    RW = Self::R as u8 | Self::W as u8,
    IORW = Self::I as u8 | Self::O as u8 | Self::RW as u8
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[rustfmt::skip]
#[repr(u8)]
pub enum MemoryAccess {
    Acquire = 1 << 1,
    Release = 1 << 0,
    AcquireRelease = Self::Acquire as u8 | Self::Release as u8
}

pub const fn register_value(register_id: u8) -> u32 {
    (register_id as u32) & ((1 << 5) - 1)
}

/// InstructionValue contains the 32-bit instruction value and also provides access into the desired field.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct InstructionValue {
    pub value: u32,
}

impl InstructionValue {
    pub const fn new(value: u32) -> Self {
        Self { value }
    }

    pub const fn field<const FIELD_START: usize, const FIELD_SIZE: usize>(self) -> u32 {
        (self.value >> FIELD_START) & ((1 << FIELD_SIZE) - 1)
    }

    pub const fn opcode(self) -> u32 {
        self.field::<0, 7>()
    }
}

macro_rules! decl_immediate {
    ($name: ident : $immediate_size: expr, $value: item, $ctor: item) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        #[repr(transparent)]
        pub struct $name {
            pub value: u32
        }

        impl $name {
            pub const IMMEDIATE_SIZE: usize = $immediate_size;

            pub fn immediate_mask<T: num::PrimInt + num::NumCast>() -> T {
                if $immediate_size < std::mem::size_of::<u32>() * 8 {
                    T::from(1u64.wrapping_shl($immediate_size as u32)).unwrap() - T::one()
                } else {
                    T::max_value()
                }
            }

            pub fn is_valid<T: num::PrimInt + num::NumCast>(imm_value: T) -> bool {
                let shift = std::mem::size_of::<T>() * 8 - $immediate_size;
                
               
                imm_value == T::from((imm_value.to_i64().unwrap() << shift) >> shift).unwrap()
            }

            pub fn v32<T: num::NumCast>(imm_value: i32) -> T {

                assert!(Self::is_valid(imm_value), "Invalid immediate value: {}", imm_value);
                let imm_value = imm_value as u32;
                let mask: u32 = Self::immediate_mask::<u32>();
                T::from(imm_value & mask).unwrap()
            }

            pub fn v64<T: num::NumCast>(imm_value: i64) -> T {
                assert!(Self::is_valid(imm_value));
                let imm_value = imm_value as u64;
                let mask: u64 = Self::immediate_mask::<u64>();
                T::from(imm_value & mask).unwrap()
            }

            pub const fn field<const FIELD_START: usize, const FIELD_END: usize>(self) -> u32 {
                (self.value >> FIELD_START) & ((1 << (FIELD_END - FIELD_START)) - 1)
            }

            $value

            $ctor
        }

        impl num::ToPrimitive for $name {
            fn to_i64(&self) -> Option<i64> {
                Some(self.value as i64)
            }

            fn to_u64(&self) -> Option<u64> {
                Some(self.value as u64)
            }

            fn to_u32(&self) -> Option<u32> {
                Some(self.value)
            }

            fn to_i32(&self) -> Option<i32> {
                Some(self.value as i32)
            }
        }

        impl num::NumCast for $name {
            fn from<T>(n: T) -> Option<Self>
            where T: num::ToPrimitive
            {
                let n = n.to_u32()?;
                if Self::is_valid(n) {
                    Some(Self::new(n))
                } else {
                    None
                }
            }
        }
    };

    ($name: ident : $immediate_size: expr, $value: item) => {
        decl_immediate!($name: $immediate_size, $value, pub fn new(value: u32) -> Self {
            Self { value }
        });
    };
}

decl_immediate!(IImmediate: 12, pub fn value(insn: InstructionValue) -> i32 {
    let base = insn.field::<20, 12>();
    let imm = base as i32;
    (imm << 20) >> 20
});

decl_immediate!(SImmediate: 12, pub fn value(insn: InstructionValue) -> i32 {
    let base = 0
        | (insn.field::<31, 1>() << 11)
        | (insn.field::<25, 6>() << 5)
        | (insn.field::<8, 4>() << 1)
        | (insn.field::<7, 1>() << 0);

    let imm = base as i32;

    (imm << 20) >> 20
});

decl_immediate!(BImmediate: 13, pub fn value(insn: InstructionValue) -> i32 {
    let base = 0
        | (insn.field::<31, 1>() << 12)
        | (insn.field::<7, 1>() << 11)
        | (insn.field::<25, 6>() << 5)
        | (insn.field::<8, 4>() << 1);

    let imm = base as i32;

    (imm << 19) >> 19
});

decl_immediate!(UImmediate: 32,
    pub fn value(insn: InstructionValue) -> i32 {
        insn.field::<12, 20>() as i32
    },

    pub fn new(value: u32) -> Self {
        Self { value: (value >> 12) << 12 }
    }
);

decl_immediate!(JImmediate: 21, pub fn value(insn: InstructionValue) -> i32 {
    let base = 0
        | (insn.field::<31, 1>() << 20)
        | (insn.field::<12, 8>() << 12)
        | (insn.field::<20, 1>() << 11)
        | (insn.field::<25, 6>() << 5)
        | (insn.field::<21, 4>() << 1);

    let imm = base as i32;

    (imm << 11) >> 11
});

pub struct ImmediateDecomposition {
    pub upper: UImmediate,
    pub lower: IImmediate,
}

impl ImmediateDecomposition {
    pub fn new32(mut immediate: i32) -> Self {
        if (immediate & (1 << 11)) != 0 {
            immediate += 1 << 12;
        }

        Self {
            upper: UImmediate::v32(immediate),
            lower: IImmediate::v32((immediate << 20) >> 20),
        }
    }

    pub fn new64(mut immediate: i64) -> Self {
        if (immediate & (1 << 11)) != 0 {
            immediate += 1 << 12;
        }

        Self {
            upper: UImmediate::v64(immediate),
            lower: IImmediate::v64((immediate << 20) >> 20),
        }
    }
}

impl RISCV64Assembler {}

pub struct RTypeBase<const OPCODE: u32, const F3: u32, const F7: u32>;

impl<const OPCODE: u32, const F3: u32, const F7: u32> RTypeBase<OPCODE, F3, F7> {
    pub const FUNCT3_VALUE: u32 = F3;
    pub const FUNCT7_VALUE: u32 = F7;
    pub const OPCODE_VALUE: u32 = OPCODE;

    pub const fn construct(rd: u8, rs1: u8, rs2: u8) -> u32 {
        let instruction = 0
            | (F7 << 25)
            | (register_value(rs2) << 20)
            | (register_value(rs1) << 15)
            | (F3 << 12)
            | (register_value(rd) << 7)
            | OPCODE as u32;

        instruction
    }

    pub const fn matches(insn: InstructionValue) -> bool {
        OPCODE as u32 == insn.opcode() && F3 == insn.field::<12, 3>() && F7 == insn.field::<25, 7>()
    }

    pub const fn rd(insn: InstructionValue) -> u8 {
        insn.field::<7, 5>() as u8
    }

    pub const fn rs1(insn: InstructionValue) -> u8 {
        insn.field::<15, 5>() as u8
    }

    pub const fn rs2(insn: InstructionValue) -> u8 {
        insn.field::<20, 5>() as u8
    }
}

pub struct RTypeBaseWithRoundingMode<const OPCODE: u32, const F7: u32>;

impl<const OPCODE: u32, const F7: u32> RTypeBaseWithRoundingMode<OPCODE, F7> {
    pub const FUNCT7_VALUE: u32 = F7;
    pub const OPCODE_VALUE: u32 = OPCODE;
    pub const fn construct(rd: u8, rs1: u8, rs2: u8, rm: FPRoundingMode) -> u32 {
        let instruction = 0
            | (F7 << 25)
            | (register_value(rs2) << 20)
            | (register_value(rs1) << 15)
            | ((rm as u32) << 12)
            | (register_value(rd) << 7)
            | OPCODE;

        instruction
    }

    pub fn matches(insn: InstructionValue) -> bool {
        OPCODE == insn.opcode() && F7 == insn.field::<25, 7>()
    }

    pub fn rd(insn: InstructionValue) -> u8 {
        insn.field::<7, 5>() as u8
    }

    pub fn rs1(insn: InstructionValue) -> u8 {
        insn.field::<15, 5>() as u8
    }

    pub fn rs2(insn: InstructionValue) -> u8 {
        insn.field::<20, 5>() as u8
    }

    pub fn rm(insn: InstructionValue) -> u8 {
        insn.field::<12, 3>() as u8
    }
}

pub struct RTypeBaseWithAqRl<const OPCODE: u32, const F3: u32, const F7: u32>;

impl<const OPCODE: u32, const F3: u32, const F7: u32> RTypeBaseWithAqRl<OPCODE, F3, F7> {
    pub const FUNCT3_VALUE: u32 = F3;
    pub const FUNCT7_VALUE: u32 = F7;
    pub const OPCODE_VALUE: u32 = OPCODE;
    pub fn construct(rd: u8, rs1: u8, rs2: u8, access: &[MemoryAccess]) -> u32 {
        let mut aqrl = 0;

        for i in 0..access.len() {
            aqrl |= access[i] as u32;
        }

        let instruction = 0
            | ((F7 | aqrl) << 25)
            | (register_value(rs2) << 20)
            | (register_value(rs1) << 15)
            | (F3 << 12)
            | (register_value(rd) << 7)
            | OPCODE;

        instruction
    }

    pub fn matches(insn: InstructionValue) -> bool {
        OPCODE == insn.opcode() && F3 == insn.field::<12, 3>() && F7 == insn.field::<25, 7>()
    }

    pub fn rd(insn: InstructionValue) -> u8 {
        insn.field::<7, 5>() as u8
    }

    pub fn rs1(insn: InstructionValue) -> u8 {
        insn.field::<15, 5>() as u8
    }

    pub fn rs2(insn: InstructionValue) -> u8 {
        insn.field::<20, 5>() as u8
    }

    pub fn aqrl(insn: InstructionValue) -> u8 {
        insn.field::<25, 2>() as u8
    }
}

pub struct R4TypeBaseWithRoundingMode<const OPCODE: u32, const F2: u32>;

impl<const OPCODE: u32, const F2: u32> R4TypeBaseWithRoundingMode<OPCODE, F2> {
    pub const FUNCT2_VALUE: u32 = F2;
    pub const OPCODE_VALUE: u32 = OPCODE;
    pub const fn construct(rd: u8, rs1: u8, rs2: u8, rs3: u8, rm: FPRoundingMode) -> u32 {
        let instruction = 0
            | (register_value(rs3) << 27)
            | (F2 << 25)
            | (register_value(rs2) << 20)
            | (register_value(rs1) << 15)
            | ((rm as u32) << 12)
            | (register_value(rd) << 7)
            | OPCODE;

        instruction
    }

    pub fn matches(insn: InstructionValue) -> bool {
        OPCODE == insn.opcode() && F2 == insn.field::<25, 2>()
    }

    pub fn rd(insn: InstructionValue) -> u8 {
        insn.field::<7, 5>() as u8
    }

    pub fn rs1(insn: InstructionValue) -> u8 {
        insn.field::<15, 5>() as u8
    }

    pub fn rs2(insn: InstructionValue) -> u8 {
        insn.field::<20, 5>() as u8
    }

    pub fn rs3(insn: InstructionValue) -> u8 {
        insn.field::<27, 5>() as u8
    }

    pub fn rm(insn: InstructionValue) -> u8 {
        insn.field::<12, 3>() as u8
    }
}

pub struct ITypeBase<const OPCODE: u32, const F3: u32>;

impl<const OPCODE: u32, const F3: u32> ITypeBase<OPCODE, F3> {
    pub const FUNCT3_VALUE: u32 = F3;
    pub const OPCODE_VALUE: u32 = OPCODE;
    pub const fn construct(rd: u8, rs1: u8, imm: IImmediate) -> u32 {
        let instruction = 0
            | (imm.field::<0, 12>() << 20)
            | (register_value(rs1) << 15)
            | (F3 << 12)
            | (register_value(rd) << 7)
            | OPCODE;

        instruction
    }

    pub fn matches(insn: InstructionValue) -> bool {
        OPCODE == insn.opcode() && F3 == insn.field::<12, 3>()
    }

    pub fn rd(insn: InstructionValue) -> u8 {
        insn.field::<7, 5>() as u8
    }

    pub fn rs1(insn: InstructionValue) -> u8 {
        insn.field::<15, 5>() as u8
    }
}

pub struct STypeBase<const OPCODE: u32, const F3: u32>;

impl<const OPCODE: u32, const F3: u32> STypeBase<OPCODE, F3> {
    pub const FUNCT3_VALUE: u32 = F3;
    pub const OPCODE_VALUE: u32 = OPCODE;
    pub const fn construct(rs1: u8, rs2: u8, imm: SImmediate) -> u32 {
        let instruction = 0
            | (imm.field::<5, 7>() << 25)
            | (register_value(rs2) << 20)
            | (register_value(rs1) << 15)
            | (F3 << 12)
            | (imm.field::<0, 5>() << 7)
            | OPCODE;

        instruction
    }

    pub const fn matches(insn: InstructionValue) -> bool {
        OPCODE == insn.opcode() && F3 == insn.field::<12, 3>()
    }

    pub const fn rs1(insn: InstructionValue) -> u8 {
        insn.field::<15, 5>() as u8
    }

    pub const fn rs2(insn: InstructionValue) -> u8 {
        insn.field::<20, 5>() as u8
    }
}

pub struct BTypeBase<const OPCODE: u32, const F3: u32>;

impl<const OPCODE: u32, const F3: u32> BTypeBase<OPCODE, F3> {
    pub const FUNCT3_VALUE: u32 = F3;
    pub const OPCODE_VALUE: u32 = OPCODE;
    pub const fn construct(rs1: u8, rs2: u8, imm: BImmediate) -> u32 {
        let instruction = 0
            | (imm.field::<12, 1>() << 31)
            | (imm.field::<5, 6>() << 25)
            | (register_value(rs2) << 20)
            | (register_value(rs1) << 15)
            | (F3 << 12)
            | (imm.field::<1, 4>() << 8)
            | (imm.field::<11, 1>() << 7)
            | OPCODE;
        instruction
    }

    pub const fn matches(insn: InstructionValue) -> bool {
        OPCODE == insn.opcode() && F3 == insn.field::<12, 3>()
    }

    pub const fn rs1(insn: InstructionValue) -> u8 {
        insn.field::<15, 5>() as u8
    }

    pub const fn rs2(insn: InstructionValue) -> u8 {
        insn.field::<20, 5>() as u8
    }
}

pub struct UTypeBase<const OPCODE: u32>;

impl<const OPCODE: u32> UTypeBase<OPCODE> {
    pub const OPCODE_VALUE: u32 = OPCODE;
    pub const fn construct(rd: u8, imm: UImmediate) -> u32 {
        let instruction = 0 | imm.value | (register_value(rd) << 7) | OPCODE;

        instruction
    }

    pub const fn matches(insn: InstructionValue) -> bool {
        OPCODE == insn.opcode()
    }

    pub const fn rd(insn: InstructionValue) -> u8 {
        insn.field::<7, 5>() as u8
    }
}

pub struct JTypeBase<const OPCODE: u32>;

impl<const OPCODE: u32> JTypeBase<OPCODE> {
    pub const OPCODE_VALUE: u32 = OPCODE;
    pub const fn construct(rd: u8, imm: JImmediate) -> u32 {
        let instruction = 0
            | (imm.field::<20, 1>() << 31)
            | (imm.field::<1, 10>() << 21)
            | (imm.field::<11, 1>() << 20)
            | (imm.field::<12, 8>() << 12)
            | (register_value(rd) << 7)
            | OPCODE;

        instruction
    }

    pub const fn matches(insn: InstructionValue) -> bool {
        OPCODE == insn.opcode()
    }

    pub const fn rd(insn: InstructionValue) -> u8 {
        insn.field::<7, 5>() as u8
    }
}

// The following instruction definitions utilize the base instruction structs, in most cases specifying everything
// necessary in the template parameters of the base instruction struct they are inheriting from. For each instruction
// there's also a pretty-print name constant included in the definition, for use by the disassembler.

// RV32I Base Instruction Set

macro_rules! riscv_insns {
    ($($name: ident : $base: ty, $pretty: literal)*) => {
        $(
            pub type $name = $base;

            paste::paste! {
                pub const [<PRETTY_ $name>]: &str = $pretty;
            }

        )*
    };
}

riscv_insns! {
    LUI: UTypeBase<{LUI}>, "lui"
    AUIPC: UTypeBase<{AUIPC}>, "auipc"
    JAL: JTypeBase<{JAL}>, "jal"
    JALR: ITypeBase<{JALR}, 0b000>, "jalr"
    BEQ: BTypeBase<{BRANCH}, 0b000>, "beq"
    BNE: BTypeBase<{BRANCH}, 0b001>, "bne"
    BLT: BTypeBase<{BRANCH}, 0b100>, "blt"
    BGE: BTypeBase<{BRANCH}, 0b101>, "bge"
    BLTU: BTypeBase<{BRANCH}, 0b110>, "bltu"
    BGEU: BTypeBase<{BRANCH}, 0b111>, "bgeu"
    LB: ITypeBase<{LOAD}, 0b000>, "lb"
    LH: ITypeBase<{LOAD}, 0b001>, "lh"
    LW: ITypeBase<{LOAD}, 0b010>, "lw"
    LBU: ITypeBase<{LOAD}, 0b100>, "lbu"
    LHU: ITypeBase<{LOAD}, 0b101>, "lhu"
    SB: STypeBase<{STORE}, 0b000>, "sb"
    SH: STypeBase<{STORE}, 0b001>, "sh"
    SW: STypeBase<{STORE}, 0b010>, "sw"
    ADDI: ITypeBase<{OP_IMM}, 0b000>, "addi"
    SLTI: ITypeBase<{OP_IMM}, 0b010>, "slti"
    SLTIU: ITypeBase<{OP_IMM}, 0b011>, "sltiu"
    XORI: ITypeBase<{OP_IMM}, 0b100>, "xori"
    ORI: ITypeBase<{OP_IMM}, 0b110>, "ori"
    ANDI: ITypeBase<{OP_IMM}, 0b111>, "andi"

}

pub struct SLLI;

pub const PRETTY_SLLI: &str = "slli";

impl SLLI {
    pub const OPCODE_VALUE: u32 = OP_IMM;

    pub fn construct(rd: u8, rs1: u8, shift_amount: u32) -> u32 {
        ITypeBase::<{ OP_IMM }, 0b001>::construct(
            rd,
            rs1,
            IImmediate::v32((0b000000 << 6) | shift_amount as i32),
        )
    }

    pub fn matches(insn: InstructionValue) -> bool {
        ITypeBase::<{ OP_IMM }, 0b001>::matches(insn)
    }

    pub fn rd(insn: InstructionValue) -> u8 {
        ITypeBase::<{ OP_IMM }, 0b001>::rd(insn)
    }

    pub fn rs1(insn: InstructionValue) -> u8 {
        ITypeBase::<{ OP_IMM }, 0b001>::rs1(insn)
    }
}

pub struct SRLI;

pub const PRETTY_SRLI: &str = "srli";

impl SRLI {
    pub fn construct(rd: u8, rs1: u8, shift_amount: u32) -> u32 {
        ITypeBase::<{ OP_IMM }, 0b101>::construct(
            rd,
            rs1,
            IImmediate::v32((0b000000 << 6) | shift_amount as i32),
        )
    }

    pub fn matches(insn: InstructionValue) -> bool {
        ITypeBase::<{ OP_IMM }, 0b101>::matches(insn)
    }

    pub fn rd(insn: InstructionValue) -> u8 {
        ITypeBase::<{ OP_IMM }, 0b101>::rd(insn)
    }

    pub fn rs1(insn: InstructionValue) -> u8 {
        ITypeBase::<{ OP_IMM }, 0b101>::rs1(insn)
    }
}

pub struct SRAI;

pub const PRETTY_SRAI: &str = "srai";

impl SRAI {
    pub fn construct(rd: u8, rs1: u8, shift_amount: u32) -> u32 {
        ITypeBase::<{ OP_IMM }, 0b101>::construct(
            rd,
            rs1,
            IImmediate::v32((0b010000 << 6) | shift_amount as i32),
        )
    }

    pub fn matches(insn: InstructionValue) -> bool {
        ITypeBase::<{ OP_IMM }, 0b101>::matches(insn)
    }

    pub fn rd(insn: InstructionValue) -> u8 {
        ITypeBase::<{ OP_IMM }, 0b101>::rd(insn)
    }

    pub fn rs1(insn: InstructionValue) -> u8 {
        ITypeBase::<{ OP_IMM }, 0b101>::rs1(insn)
    }
}

riscv_insns! {
    ADD: RTypeBase<{OP}, 0b000, 0b0000000>, "add"
    SUB: RTypeBase<{OP}, 0b000, 0b0100000>, "sub"
    SLL: RTypeBase<{OP}, 0b001, 0b0000000>, "sll"
    SLT: RTypeBase<{OP}, 0b010, 0b0000000>, "slt"
    SLTU: RTypeBase<{OP}, 0b011, 0b0000000>, "sltu"
    XOR: RTypeBase<{OP}, 0b100, 0b0000000>, "xor"
    SRL: RTypeBase<{OP}, 0b101, 0b0000000>, "srl"
    SRA: RTypeBase<{OP}, 0b101, 0b0100000>, "sra"
    OR: RTypeBase<{OP}, 0b110, 0b0000000>, "or"
    AND: RTypeBase<{OP}, 0b111, 0b0000000>, "and"
    FENCE: ITypeBase<{MISC_MEM}, 0b000>, "fence"
    ECALL: ITypeBase<{SYSTEM}, 0b000>, "ecall"
    EBREAK: ITypeBase<{SYSTEM}, 0b001>, "ebreak"

    // RV64I Base Instruction Set (in addition to RV32I)
    LWU: ITypeBase<{LOAD}, 0b110>, "lwu"
    LD: ITypeBase<{LOAD}, 0b011>, "ld"
    SD: STypeBase<{STORE}, 0b011>, "sd"
    ADDIW: ITypeBase<{OP_IMM_32}, 0b000>, "addiw"

}

pub struct SLLIW;

pub const PRETTY_SLLIW: &str = "slliw";

impl SLLIW {
    pub fn construct(rd: u8, rs1: u8, shift_amount: u32) -> u32 {
        ITypeBase::<{ OP_IMM_32 }, 0b001>::construct(
            rd,
            rs1,
            IImmediate::v32((0b000000 << 5) | shift_amount as i32),
        )
    }

    pub fn matches(insn: InstructionValue) -> bool {
        ITypeBase::<{ OP_IMM_32 }, 0b001>::matches(insn)
    }

    pub fn rd(insn: InstructionValue) -> u8 {
        ITypeBase::<{ OP_IMM_32 }, 0b001>::rd(insn)
    }

    pub fn rs1(insn: InstructionValue) -> u8 {
        ITypeBase::<{ OP_IMM_32 }, 0b001>::rs1(insn)
    }
}

pub struct SRLIW;

pub const PRETTY_SRLIW: &str = "srliw";

impl SRLIW {
    pub fn construct(rd: u8, rs1: u8, shift_amount: u32) -> u32 {
        ITypeBase::<{ OP_IMM_32 }, 0b101>::construct(
            rd,
            rs1,
            IImmediate::v32((0b000000 << 5) | shift_amount as i32),
        )
    }

    pub fn matches(insn: InstructionValue) -> bool {
        ITypeBase::<{ OP_IMM_32 }, 0b101>::matches(insn)
    }

    pub fn rd(insn: InstructionValue) -> u8 {
        ITypeBase::<{ OP_IMM_32 }, 0b101>::rd(insn)
    }

    pub fn rs1(insn: InstructionValue) -> u8 {
        ITypeBase::<{ OP_IMM_32 }, 0b101>::rs1(insn)
    }
}

pub struct SRAIW;

pub const PRETTY_SRAIW: &str = "sraiw";

impl SRAIW {
    pub fn construct(rd: u8, rs1: u8, shift_amount: u32) -> u32 {
        ITypeBase::<{ OP_IMM_32 }, 0b101>::construct(
            rd,
            rs1,
            IImmediate::v32((0b010000 << 5) | shift_amount as i32),
        )
    }

    pub fn matches(insn: InstructionValue) -> bool {
        ITypeBase::<{ OP_IMM_32 }, 0b101>::matches(insn)
    }

    pub fn rd(insn: InstructionValue) -> u8 {
        ITypeBase::<{ OP_IMM_32 }, 0b101>::rd(insn)
    }

    pub fn rs1(insn: InstructionValue) -> u8 {
        ITypeBase::<{ OP_IMM_32 }, 0b101>::rs1(insn)
    }
}

riscv_insns! {
    ADDW: RTypeBase<{OP_32}, 0b000, 0b0000000>, "addw"
    SUBW: RTypeBase<{OP_32}, 0b000, 0b0100000>, "subw"
    SLLW: RTypeBase<{OP_32}, 0b001, 0b0000000>, "sllw"
    SRLW: RTypeBase<{OP_32}, 0b101, 0b0000000>, "srlw"
    SRAW: RTypeBase<{OP_32}, 0b101, 0b0100000>, "sraw"
    // RV32/RV64 Zifencei Standard Extension
    FENCE_I: ITypeBase<{MISC_MEM}, 0b001>, "fence.i"
    // RV32M Standard Extension
    MUL: RTypeBase<{OP}, 0b000, 0b0000001>, "mul"
    MULH: RTypeBase<{OP}, 0b001, 0b0000001>, "mulh"
    MULHSU: RTypeBase<{OP}, 0b010, 0b0000001>, "mulhsu"
    MULHU: RTypeBase<{OP}, 0b011, 0b0000001>, "mulhu"
    DIV: RTypeBase<{OP}, 0b100, 0b0000001>, "div"
    DIVU: RTypeBase<{OP}, 0b101, 0b0000001>, "divu"
    REM: RTypeBase<{OP}, 0b110, 0b0000001>, "rem"
    REMU: RTypeBase<{OP}, 0b111, 0b0000001>, "remu"
    // RV64M Standard Extension (in addition to RV32M)
    MULW: RTypeBase<{OP_32}, 0b000, 0b0000001>, "mulw"
    DIVW: RTypeBase<{OP_32}, 0b100, 0b0000001>, "divw"
    DIVUW: RTypeBase<{OP_32}, 0b101, 0b0000001>, "divuw"
    REMW: RTypeBase<{OP_32}, 0b110, 0b0000001>, "remw"
    REMUW: RTypeBase<{OP_32}, 0b111, 0b0000001>, "remuw"
    // RV32A Standard Extension
    LR_W: RTypeBaseWithAqRl<{AMO}, 0b010, 0b0001000>, "lr.w"
    SC_W: RTypeBaseWithAqRl<{AMO}, 0b010, 0b0001100>, "sc.w"
    AMOSWAP_W: RTypeBaseWithAqRl<{AMO}, 0b010, 0b0000100>, "amoswap.w"
    AMOADD_W: RTypeBaseWithAqRl<{AMO}, 0b010, 0b0000000>, "amoadd.w"
    AMOXOR_W: RTypeBaseWithAqRl<{AMO}, 0b010, 0b0010000>, "amoxor.w"
    AMOAND_W: RTypeBaseWithAqRl<{AMO}, 0b010, 0b0110000>, "amoand.w"
    AMOOR_W: RTypeBaseWithAqRl<{AMO}, 0b010, 0b0100000>, "amoor.w"
    AMOMIN_W: RTypeBaseWithAqRl<{AMO}, 0b010, 0b1000000>, "amomin.w"
    AMOMAX_W: RTypeBaseWithAqRl<{AMO}, 0b010, 0b1010000>, "amomax.w"
    AMOMINU_W: RTypeBaseWithAqRl<{AMO}, 0b010, 0b1100000>, "amominu.w"
    AMOMAXU_W: RTypeBaseWithAqRl<{AMO}, 0b010, 0b1110000>, "amomaxu.w"

    // RV64A Standard Extension (in addition to RV32A)
    LR_D: RTypeBaseWithAqRl<{AMO}, 0b011, 0b0001000>, "lr.d"
    SC_D: RTypeBaseWithAqRl<{AMO}, 0b011, 0b0001100>, "sc.d"
    AMOSWAP_D: RTypeBaseWithAqRl<{AMO}, 0b011, 0b0000100>, "amoswap.d"
    AMOADD_D: RTypeBaseWithAqRl<{AMO}, 0b011, 0b0000000>, "amoadd.d"
    AMOXOR_D: RTypeBaseWithAqRl<{AMO}, 0b011, 0b0010000>, "amoxor.d"
    AMOAND_D: RTypeBaseWithAqRl<{AMO}, 0b011, 0b0110000>, "amoand.d"
    AMOOR_D: RTypeBaseWithAqRl<{AMO}, 0b011, 0b0100000>, "amoor.d"
    AMOMIN_D: RTypeBaseWithAqRl<{AMO}, 0b011, 0b1000000>, "amomin.d"
    AMOMAX_D: RTypeBaseWithAqRl<{AMO}, 0b011, 0b1010000>, "amomax.d"
    AMOMINU_D: RTypeBaseWithAqRl<{AMO}, 0b011, 0b1100000>, "amominu.d"
    AMOMAXU_D: RTypeBaseWithAqRl<{AMO}, 0b011, 0b1110000>, "amomaxu.d"
}

pub struct FCVTImpl<const RS2: u8, const F7: u32>;

impl<const RS2: u8, const F7: u32> FCVTImpl<RS2, F7> {
    pub const fn construct(rd: u8, rs1: u8, rm: FPRoundingMode) -> u32 {
        RTypeBaseWithRoundingMode::<{ OP_FP }, F7>::construct(rd, rs1, RS2, rm)
    }

    pub fn matches(inst: InstructionValue) -> bool {
        RTypeBaseWithRoundingMode::<{ OP_FP }, F7>::matches(inst)
    }

    pub fn rd(inst: InstructionValue) -> u8 {
        RTypeBaseWithRoundingMode::<{ OP_FP }, F7>::rd(inst)
    }

    pub fn rs1(inst: InstructionValue) -> u8 {
        RTypeBaseWithRoundingMode::<{ OP_FP }, F7>::rs1(inst)
    }

    pub fn rs2(inst: InstructionValue) -> u8 {
        RTypeBaseWithRoundingMode::<{ OP_FP }, F7>::rs2(inst)
    }

    pub fn rm(inst: InstructionValue) -> u8 {
        RTypeBaseWithRoundingMode::<{ OP_FP }, F7>::rm(inst)
    }
}

pub struct FMVImpl<const F7: u32>;

impl<const F7: u32> FMVImpl<F7> {
    pub fn matches(inst: InstructionValue) -> bool {
        RTypeBase::<{ OP_FP }, 0b000, F7>::matches(inst)
    }

    pub const fn construct(rd: u8, rs1: u8) -> u32 {
        RTypeBase::<{ OP_FP }, 0b000, F7>::construct(rd, rs1, 0)
    }

    pub fn rd(inst: InstructionValue) -> u8 {
        RTypeBase::<{ OP_FP }, 0b000, F7>::rd(inst)
    }

    pub fn rs1(inst: InstructionValue) -> u8 {
        RTypeBase::<{ OP_FP }, 0b000, F7>::rs1(inst)
    }
}
// RV32F Standard Extension
riscv_insns! {
    FLW: ITypeBase<{LOAD_FP}, 0b010>, "flw"
    FSW: STypeBase<{STORE_FP}, 0b010>, "fsw"
    FMADD_S: R4TypeBaseWithRoundingMode<{MADD}, 0b00>, "fmadd.s"
    FMSUB_S: R4TypeBaseWithRoundingMode<{MSUB}, 0b00>, "fmsub.s"
    FNMSUB_S: R4TypeBaseWithRoundingMode<{NMSUB}, 0b00>, "fnmsub.s"
    FNMADD_S: R4TypeBaseWithRoundingMode<{NMADD}, 0b00>, "fnmadd.s"
    FADD_S: RTypeBaseWithRoundingMode<{OP_FP}, 0b0000000>, "fadd.s"
    FSUB_S: RTypeBaseWithRoundingMode<{OP_FP}, 0b0000100>, "fsub.s"
    FMUL_S: RTypeBaseWithRoundingMode<{OP_FP}, 0b0001000>, "fmul.s"
    FDIV_S: RTypeBaseWithRoundingMode<{OP_FP}, 0b0001100>, "fdiv.s"
    FSQRT_S: RTypeBaseWithRoundingMode<{OP_FP}, 0b0101100>, "fsqrt.s"
    FSGNJ_S: RTypeBase<{OP_FP}, 0b000, 0b0010000>, "fsgnj.s"
    FSGNJN_S: RTypeBase<{OP_FP}, 0b001, 0b0010000>, "fsgnjn.s"
    FSGNJX_S: RTypeBase<{OP_FP}, 0b010, 0b0010000>, "fsgnjx.s"
    FMIN_S: RTypeBase<{OP_FP}, 0b000, 0b0010100>, "fmin.s"
    FMAX_S: RTypeBase<{OP_FP}, 0b001, 0b0010100>, "fmax.s"
    FCVT_W_S: FCVTImpl<0b00000, 0b1100000>, "fcvt.w.s"
    FCVT_WU_S: FCVTImpl<0b00000, 0b1100001>, "fcvt.wu.s"
    FMV_X_W: FMVImpl<0b1110000>, "fmv.x.w"

    FEQ_S: RTypeBase<{OP_FP}, 0b010, 0b1010000>, "feq.s"
    FLT_S: RTypeBase<{OP_FP}, 0b001, 0b1010000>, "flt.s"
    FLE_S: RTypeBase<{OP_FP}, 0b000, 0b1010000>, "fle.s"

    FCLASS_S: RTypeBase<{OP_FP}, 0b001, 0b1110000>, "fclass.s"
    FCVT_S_W: FCVTImpl<0b00000, 0b1101000>, "fcvt.s.w"
    FCVT_S_WU: FCVTImpl<0b00001, 0b1101000>, "fcvt.s.wu"
    FMV_W_X: FMVImpl<0b1111000>, "fmv.w.x"
    // RV64F Standard Extension (in addition to RV32F)
    FCVT_L_S: FCVTImpl<0b00010, 0b1100000>, "fcvt.l.s"
    FCVT_LU_S: FCVTImpl<0b00011, 0b1100000>, "fcvt.lu.s"
    FCVT_S_L: FCVTImpl<0b00010, 0b1101000>, "fcvt.s.l"
    FCVT_S_LU: FCVTImpl<0b00011, 0b1101000>, "fcvt.s.lu"
    // RV32D Standard Extension
    FLD: ITypeBase<{LOAD_FP}, 0b011>, "fld"
    FSD: STypeBase<{STORE_FP}, 0b011>, "fsd"
    FMADD_D: R4TypeBaseWithRoundingMode<{MADD}, 0b01>, "fmadd.d"
    FMSUB_D: R4TypeBaseWithRoundingMode<{MSUB}, 0b01>, "fmsub.d"
    FNMSUB_D: R4TypeBaseWithRoundingMode<{NMSUB}, 0b01>, "fnmsub.d"
    FNMADD_D: R4TypeBaseWithRoundingMode<{NMADD}, 0b01>, "fnmadd.d"
    FADD_D: RTypeBaseWithRoundingMode<{OP_FP}, 0b0000001>, "fadd.d"
    FSUB_D: RTypeBaseWithRoundingMode<{OP_FP}, 0b0000101>, "fsub.d"
    FMUL_D: RTypeBaseWithRoundingMode<{OP_FP}, 0b0001001>, "fmul.d"
    FDIV_D: RTypeBaseWithRoundingMode<{OP_FP}, 0b0001101>, "fdiv.d"
    FSQRT_D: RTypeBaseWithRoundingMode<{OP_FP}, 0b0101101>, "fsqrt.d"
    FSGNJ_D: RTypeBase<{OP_FP}, 0b000,  0b0010001>, "fsgnj.d"
    FSGNJN_D: RTypeBase<{OP_FP}, 0b001,  0b0010011>, "fsgnjn.d"
    FSGNJX_D: RTypeBase<{OP_FP}, 0b010,  0b0010010>, "fsgnjx.d"
    FMIN_D: RTypeBase<{OP_FP}, 0b000,  0b0010101>, "fmin.d"
    FMAX_D: RTypeBase<{OP_FP}, 0b001,  0b0010101>, "fmax.d"
    FCVT_S_D: FCVTImpl<0b00001, 0b0100000>, "fcvt.s.d"
    FCVT_D_S: FCVTImpl<0b00000, 0b0100001>, "fcvt.d.s"
    FEQ_D: RTypeBase<{OP_FP}, 0b010, 0b1010001>, "feq.d"
    FLT_D: RTypeBase<{OP_FP}, 0b001, 0b1010001>, "flt.d"
    FLE_D: RTypeBase<{OP_FP}, 0b000, 0b1010001>, "fle.d"
    FCLASS_D: RTypeBase<{OP_FP}, 0b001, 0b1110001>, "fclass.d"
    FCVT_W_D: FCVTImpl<0b00000, 0b1100001>, "fcvt.w.d"
    FCVT_WU_D: FCVTImpl<0b00001, 0b1100001>, "fcvt.wu.d"
    FCVT_D_W: FCVTImpl<0b00000, 0b1101001>, "fcvt.d.w"
    FCVT_D_WU: FCVTImpl<0b00001, 0b1101001>, "fcvt.d.wu"

    // RV64D Standard Extension (in addition to RV32D)
    FCVT_L_D: FCVTImpl<0b00010, 0b1100001>, "fcvt.l.d"
    FCVT_LU_D: FCVTImpl<0b00011, 0b1100001>, "fcvt.lu.d"
    FCVT_D_L: FCVTImpl<0b00010, 0b1101001>, "fcvt.d.l"
    FCVT_D_LU: FCVTImpl<0b00011, 0b1101001>, "fcvt.d.lu"
    FMV_X_D: FMVImpl<0b1110001>, "fmv.x.d"
    FMV_D_X: FMVImpl<0b1111001>, "fmv.d.x"
}

impl RISCV64Assembler {
    pub fn first_register() -> u8 {
        x0
    }

    pub const fn last_register() -> u8 {
        x31
    }

    pub fn first_sp_register() -> u8 {
        pc
    }

    pub const fn last_sp_register() -> u8 {
        pc
    }

    pub fn first_fp_register() -> u8 {
        f0
    }

    pub const fn last_fp_register() -> u8 {
        f31
    }

    pub const fn gpr_name(reg: u8) -> &'static str {
        match reg {
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
            x29 => "x29",
            x30 => "x30",
            x31 => "x31",
            _ => "unknown",
        }
    }

    pub fn fpr_name(reg: u8) -> &'static str {
        match reg {
            f0 => "f0",
            f1 => "f1",
            f2 => "f2",
            f3 => "f3",
            f4 => "f4",
            f5 => "f5",
            f6 => "f6",
            f7 => "f7",
            f8 => "f8",
            f9 => "f9",
            f10 => "f10",
            f11 => "f11",
            f12 => "f12",
            f13 => "f13",
            f14 => "f14",
            f15 => "f15",
            f16 => "f16",
            f17 => "f17",
            f18 => "f18",
            f19 => "f19",
            f20 => "f20",
            f21 => "f21",
            f22 => "f22",
            f23 => "f23",
            f24 => "f24",
            f25 => "f25",
            f26 => "f26",
            f27 => "f27",
            f28 => "f28",
            f29 => "f29",
            f30 => "f30",
            f31 => "f31",
            _ => "unknown",
        }
    }

    pub unsafe fn get_relocate_address(code: *mut u8, label: AssemblerLabel) -> *mut u8 {
        code.offset(label.offset() as i32 as isize)
    }

    pub fn get_difference_between_labels(from: AssemblerLabel, to: AssemblerLabel) -> i32 {
        to.offset() as i32 - from.offset() as i32
    }

    pub fn get_call_return_offset(call: AssemblerLabel) -> usize {
        call.offset() as _
    }

    pub fn label_ignoring_watchpoints(&mut self) -> AssemblerLabel {
        self.buffer.label()
    }

    pub fn label_for_watchpoint(&mut self) -> AssemblerLabel {
        let mut label = self.buffer.label();

        if label.offset() as i32 != self.index_of_last_watchpoint {
            label = self.label();
        }

        self.index_of_last_watchpoint = label.offset() as _;
        self.index_of_tail_of_last_watchpoint =
            label.offset() as i32 + Self::max_jump_replacement_size() as i32;
        label
    }

    pub fn label(&mut self) -> AssemblerLabel {
        let mut result = self.buffer.label();

        while (result.offset() as i32) < self.index_of_tail_of_last_watchpoint {
            self.nop();
            result = self.buffer.label();
        }

        result
    }

    pub unsafe fn link_jump_(code: *mut u8, from: AssemblerLabel, to: *mut u8) {
        if !from.is_set() {
            return;
        }

        let location = code.offset(from.offset() as i32 as isize).cast::<u32>();

        if location.read() == LinkJumpImpl::placeholder_insn() {
            LinkJumpImpl::apply(location, to);
            return;
        }

        if location.read() == LinkBranchImpl::placeholder_insn() {
            LinkBranchImpl::apply(location, to);
            return;
        }
    }

    pub unsafe fn link_call(code: *mut u8, from: AssemblerLabel, to: *mut u8) {
        let location = code.offset(from.offset() as i32 as isize).cast::<u32>();
        assert_eq!(location.read(), LinkCallImpl::placeholder_insn());
        LinkCallImpl::apply(location, to);
    }

    pub unsafe fn link_pointer(code: *mut u8, where_: AssemblerLabel, value_ptr: *mut u8) {
        let location = code.offset(where_.offset() as i32 as isize).cast::<u32>();
        PatchPointerImpl::apply(location, value_ptr);
    }

    pub fn link_jump(&mut self, from: AssemblerLabel, to: AssemblerLabel) {
        if !from.is_set() || !to.is_set() {
            return;
        }

        unsafe {
            let location = self
                .buffer
                .data_mut()
                .as_mut_ptr()
                .offset(to.offset() as i32 as isize) as *mut u32;
            Self::link_jump_(self.buffer.data_mut().as_mut_ptr(), from, location.cast());
        }
    }

    pub const fn patchable_jump_size() -> usize {
        size_of::<u32>() * 8
    }

    pub const fn max_jump_replacement_size() -> usize {
        size_of::<u32>() * 8
    }

    pub unsafe fn repatch_int32(_: *mut u8, _: i32) {
        todo!()
    }

    pub unsafe fn repatch_pointer(where_: *mut u8, value_ptr: *mut u8) {
        let location = where_.cast::<u32>();
        PatchPointerImpl::apply(location, value_ptr);
    }

    pub unsafe fn relink_jump(where_: *mut u8, to: *mut u8) {
        let location = where_.cast::<u32>();
        LinkJumpImpl::apply(location, to);
    }

    pub unsafe fn relink_call(where_: *mut u8, to: *mut u8) {
        let location = where_.cast::<u32>();
        LinkCallImpl::apply(location, to);
    }

    pub unsafe fn relink_tail_call(where_: *mut u8, to: *mut u8) {
        Self::relink_jump(where_, to)
    }

    pub unsafe fn replace_with_vm_halt(where_: *mut u8) {
        let location = where_.cast::<u32>();
        location.write(SD::construct(zero, zero, SImmediate::v32(0)))
    }

    pub unsafe fn replace_with_jump(from: *mut u8, to: *mut u8) {
        let location = from.cast::<u32>();
        let offset = to as i64 - from as i64;

        if JImmediate::is_valid(offset) {
            location.write(JAL::construct(zero, JImmediate::v32(offset as i32)));
            return;
        }

        let immediate = ImmediateDecomposition::new64(offset as i64);

        location.write(AUIPC::construct(x30, immediate.upper));
        location
            .add(1)
            .write(JALR::construct(zero, x30, immediate.lower));
    }

    pub unsafe fn revert_jump_replacement_to_patch(from: *mut u8, value_ptr: *mut u8) {
        let location = from.cast::<u32>();
        PatchPointerImpl::apply_with_dest(location, x30, value_ptr);
    }

    pub unsafe fn read_call_target(from: *mut u8) -> *mut u8 {
        let location = from.cast::<u32>();
        PatchPointerImpl::read(location)
    }

    pub unsafe fn read_pointer(where_: *mut u8) -> *mut u8 {
        let location = where_.cast::<u32>();
        PatchPointerImpl::read(location)
    }

    pub unsafe fn replace_with_load(_: *mut u8) {
        todo!()
    }

    pub unsafe fn replace_with_address_computation(_: *mut u8) {
        todo!()
    }

    pub unsafe fn fill_nops(base: *mut u8, size: usize) {
        let ptr = base.cast::<u32>();

        let nop = ADDI::construct(x0, x0, IImmediate::v32(0));

        let mut i = 0;
        let n = size / size_of::<u32>();
        while i < n {
            ptr.add(i).write(nop);
            i += 1;
        }
    }

    pub fn lui(&mut self, rd: u8, imm: i32) {
        self.insn(LUI::construct(rd, UImmediate::v32(imm)));
    }

    pub fn auipc(&mut self, rd: u8, imm: i32) {
        self.insn(AUIPC::construct(rd, UImmediate::v32(imm)));
    }

    pub fn jal(&mut self, rd: u8, imm: i32) {
        self.insn(JAL::construct(rd, JImmediate::v32(imm)));
    }

    pub fn jalr(&mut self, rd: u8, rs1: u8, imm: i32) {
        self.insn(JALR::construct(rd, rs1, IImmediate::v32(imm)));
    }

    pub fn beq(&mut self, rs1: u8, rs2: u8, imm: i32) {
        self.insn(BEQ::construct(rs1, rs2, BImmediate::v32(imm)));
    }

    pub fn bne(&mut self, rs1: u8, rs2: u8, imm: i32) {
        self.insn(BNE::construct(rs1, rs2, BImmediate::v32(imm)));
    }

    pub fn blt(&mut self, rs1: u8, rs2: u8, imm: i32) {
        self.insn(BLT::construct(rs1, rs2, BImmediate::v32(imm)));
    }

    pub fn bge(&mut self, rs1: u8, rs2: u8, imm: i32) {
        self.insn(BGE::construct(rs1, rs2, BImmediate::v32(imm)));
    }

    pub fn bltu(&mut self, rs1: u8, rs2: u8, imm: i32) {
        self.insn(BLTU::construct(rs1, rs2, BImmediate::v32(imm)));
    }

    pub fn bgeu(&mut self, rs1: u8, rs2: u8, imm: i32) {
        self.insn(BGEU::construct(rs1, rs2, BImmediate::v32(imm)));
    }

    pub fn lb(&mut self, rd: u8, rs1: u8, imm: i32) {
        self.insn(LB::construct(rd, rs1, IImmediate::v32(imm)));
    }

    pub fn lh(&mut self, rd: u8, rs1: u8, imm: i32) {
        self.insn(LH::construct(rd, rs1, IImmediate::v32(imm)));
    }

    pub fn lw(&mut self, rd: u8, rs1: u8, imm: i32) {
        self.insn(LW::construct(rd, rs1, IImmediate::v32(imm)));
    }

    pub fn lbu(&mut self, rd: u8, rs1: u8, imm: i32) {
        self.insn(LBU::construct(rd, rs1, IImmediate::v32(imm)));
    }

    pub fn lhu(&mut self, rd: u8, rs1: u8, imm: i32) {
        self.insn(LHU::construct(rd, rs1, IImmediate::v32(imm)));
    }

    pub fn sb(&mut self, rs1: u8, rs2: u8, imm: i32) {
        self.insn(SB::construct(rs1, rs2, SImmediate::v32(imm)));
    }

    pub fn sh(&mut self, rs1: u8, rs2: u8, imm: i32) {
        self.insn(SH::construct(rs1, rs2, SImmediate::v32(imm)));
    }

    pub fn sw(&mut self, rs1: u8, rs2: u8, imm: i32) {
        self.insn(SW::construct(rs1, rs2, SImmediate::v32(imm)));
    }

    pub fn sd(&mut self, rs1: u8, rs2: u8, imm: i32) {
        self.insn(SD::construct(rs1, rs2, SImmediate::v32(imm)));
    }

    pub fn addi(&mut self, rd: u8, rs1: u8, imm: i32) {
        self.insn(ADDI::construct(rd, rs1, IImmediate::v32(imm)));
    }

    pub fn slti(&mut self, rd: u8, rs1: u8, imm: i32) {
        self.insn(SLTI::construct(rd, rs1, IImmediate::v32(imm)));
    }

    pub fn sltiu(&mut self, rd: u8, rs1: u8, imm: i32) {
        self.insn(SLTIU::construct(rd, rs1, IImmediate::v32(imm)));
    }

    pub fn xori(&mut self, rd: u8, rs1: u8, imm: i32) {
        self.insn(XORI::construct(rd, rs1, IImmediate::v32(imm)));
    }

    pub fn ori(&mut self, rd: u8, rs1: u8, imm: i32) {
        self.insn(ORI::construct(rd, rs1, IImmediate::v32(imm)));
    }

    pub fn andi(&mut self, rd: u8, rs1: u8, imm: i32) {
        self.insn(ANDI::construct(rd, rs1, IImmediate::v32(imm)));
    }

    pub fn slli(&mut self, rd: u8, rs1: u8, imm: u32) {
        self.insn(SLLI::construct(
            rd,
            rs1,
            IImmediate::v32(((0b000000 << 6) | imm) as i32),
        ));
    }

    pub fn srli(&mut self, rd: u8, rs1: u8, imm: u32) {
        self.insn(SRLI::construct(
            rd,
            rs1,
            IImmediate::v32(((0b000000 << 6) | imm) as i32),
        ));
    }

    pub fn srai(&mut self, rd: u8, rs1: u8, imm: u32) {
        self.insn(SRAI::construct(
            rd,
            rs1,
            IImmediate::v32(((0b010000 << 6) | imm) as i32),
        ));
    }

    pub fn add(&mut self, rd: u8, rs1: u8, rs2: u8) {
        self.insn(ADD::construct(rd, rs1, rs2));
    }

    pub fn sub(&mut self, rd: u8, rs1: u8, rs2: u8) {
        self.insn(SUB::construct(rd, rs1, rs2));
    }

    pub fn sll(&mut self, rd: u8, rs1: u8, rs2: u8) {
        self.insn(SLL::construct(rd, rs1, rs2));
    }

    pub fn slt(&mut self, rd: u8, rs1: u8, rs2: u8) {
        self.insn(SLT::construct(rd, rs1, rs2));
    }

    pub fn sltu(&mut self, rd: u8, rs1: u8, rs2: u8) {
        self.insn(SLTU::construct(rd, rs1, rs2));
    }

    pub fn xor(&mut self, rd: u8, rs1: u8, rs2: u8) {
        self.insn(XOR::construct(rd, rs1, rs2));
    }

    pub fn srl(&mut self, rd: u8, rs1: u8, rs2: u8) {
        self.insn(SRL::construct(rd, rs1, rs2));
    }

    pub fn sra(&mut self, rd: u8, rs1: u8, rs2: u8) {
        self.insn(SRA::construct(rd, rs1, rs2));
    }

    pub fn or(&mut self, rd: u8, rs1: u8, rs2: u8) {
        self.insn(OR::construct(rd, rs1, rs2));
    }

    pub fn and(&mut self, rd: u8, rs1: u8, rs2: u8) {
        self.insn(AND::construct(rd, rs1, rs2));
    }

    pub fn ecall(&mut self) {
        self.insn(ECALL::construct(zero, zero, IImmediate::v32(0)));
    }

    pub fn ebreak(&mut self) {
        self.insn(EBREAK::construct(zero, zero, IImmediate::v32(1)));
    }

    pub fn addiw(&mut self, rd: u8, rs1: u8, imm: i32) {
        self.insn(ADDIW::construct(rd, rs1, IImmediate::v32(imm)));
    }

    /*pub fn slliw(&mut self, rd: u8, rs1: u8, imm: u32) {
        self.insn(SLLIW::construct(
            rd,
            rs1,
            IImmediate::v32(((0b000000 << 5) | imm) as i32),
        ));
    }

    pub fn srliw(&mut self, rd: u8, rs1: u8, imm: u32) {
        self.insn(SRLIW::construct(
            rd,
            rs1,
            IImmediate::v32(((0b000000 << 5) | imm) as i32),
        ));
    }

    pub fn sraiw(&mut self, rd: u8, rs1: u8, imm: u32) {
        self.insn(SRAIW::construct(
            rd,
            rs1,
            IImmediate::v32(((0b010000 << 5) | imm) as i32),
        ));
    }*/

    pub fn addw(&mut self, rd: u8, rs1: u8, rs2: u8) {
        self.insn(ADDW::construct(rd, rs1, rs2));
    }

    pub fn subw(&mut self, rd: u8, rs1: u8, rs2: u8) {
        self.insn(SUBW::construct(rd, rs1, rs2));
    }

    pub fn sllw(&mut self, rd: u8, rs1: u8, rs2: u8) {
        self.insn(SLLW::construct(rd, rs1, rs2));
    }

    pub fn srlw(&mut self, rd: u8, rs1: u8, rs2: u8) {
        self.insn(SRLW::construct(rd, rs1, rs2));
    }

    pub fn sraw(&mut self, rd: u8, rs1: u8, rs2: u8) {
        self.insn(SRAW::construct(rd, rs1, rs2));
    }

    pub fn mul(&mut self, rd: u8, rs1: u8, rs2: u8) {
        self.insn(MUL::construct(rd, rs1, rs2));
    }

    pub fn mulh(&mut self, rd: u8, rs1: u8, rs2: u8) {
        self.insn(MULH::construct(rd, rs1, rs2));
    }

    pub fn mulhsu(&mut self, rd: u8, rs1: u8, rs2: u8) {
        self.insn(MULHSU::construct(rd, rs1, rs2));
    }

    pub fn mulhu(&mut self, rd: u8, rs1: u8, rs2: u8) {
        self.insn(MULHU::construct(rd, rs1, rs2));
    }

    pub fn div(&mut self, rd: u8, rs1: u8, rs2: u8) {
        self.insn(DIV::construct(rd, rs1, rs2));
    }

    pub fn divu(&mut self, rd: u8, rs1: u8, rs2: u8) {
        self.insn(DIVU::construct(rd, rs1, rs2));
    }

    pub fn rem(&mut self, rd: u8, rs1: u8, rs2: u8) {
        self.insn(REM::construct(rd, rs1, rs2));
    }

    pub fn remu(&mut self, rd: u8, rs1: u8, rs2: u8) {
        self.insn(REMU::construct(rd, rs1, rs2));
    }

    pub fn flw(&mut self, rd: u8, rs1: u8, imm: i32) {
        self.insn(FLW::construct(rd, rs1, IImmediate::v32(imm)));
    }

    pub fn fld(&mut self, rd: u8, rs1: u8, imm: i32) {
        self.insn(FLD::construct(rd, rs1, IImmediate::v32(imm)));
    }

    pub fn fsw(&mut self, rs1: u8, rs2: u8, imm: i32) {
        self.insn(FSW::construct(rs1, rs2, SImmediate::v32(imm)));
    }

    pub fn fsd(&mut self, rs1: u8, rs2: u8, imm: i32) {
        self.insn(FSD::construct(rs1, rs2, SImmediate::v32(imm)));
    }

    pub fn fmadd<const FP_SIZE: usize>(&mut self, rd: u8, rs1: u8, rs2: u8, rs3: u8) {
        if FP_SIZE == 32 {
            self.insn(FMADD_S::construct(rd, rs1, rs2, rs3, FPRoundingMode::DYN));
        } else {
            self.insn(FMADD_D::construct(rd, rs1, rs2, rs3, FPRoundingMode::DYN));
        }
    }

    pub fn fmsub<const FP_SIZE: usize>(&mut self, rd: u8, rs1: u8, rs2: u8, rs3: u8) {
        if FP_SIZE == 32 {
            self.insn(FMSUB_S::construct(rd, rs1, rs2, rs3, FPRoundingMode::DYN));
        } else {
            self.insn(FMSUB_D::construct(rd, rs1, rs2, rs3, FPRoundingMode::DYN));
        }
    }

    pub fn fnmsub<const FP_SIZE: usize>(&mut self, rd: u8, rs1: u8, rs2: u8, rs3: u8) {
        if FP_SIZE == 32 {
            self.insn(FNMSUB_S::construct(rd, rs1, rs2, rs3, FPRoundingMode::DYN));
        } else {
            self.insn(FNMSUB_D::construct(rd, rs1, rs2, rs3, FPRoundingMode::DYN));
        }
    }

    pub fn fnmadd<const FP_SIZE: usize>(&mut self, rd: u8, rs1: u8, rs2: u8, rs3: u8) {
        if FP_SIZE == 32 {
            self.insn(FNMADD_S::construct(rd, rs1, rs2, rs3, FPRoundingMode::DYN));
        } else {
            self.insn(FNMADD_D::construct(rd, rs1, rs2, rs3, FPRoundingMode::DYN));
        }
    }

    pub fn fadd<const FP_SIZE: usize>(&mut self, rd: u8, rs1: u8, rs2: u8) {
        if FP_SIZE == 32 {
            self.insn(FADD_S::construct(rd, rs1, rs2, FPRoundingMode::DYN));
        } else {
            self.insn(FADD_D::construct(rd, rs1, rs2, FPRoundingMode::DYN));
        }
    }

    pub fn fsub<const FP_SIZE: usize>(&mut self, rd: u8, rs1: u8, rs2: u8) {
        if FP_SIZE == 32 {
            self.insn(FSUB_S::construct(rd, rs1, rs2, FPRoundingMode::DYN));
        } else {
            self.insn(FSUB_D::construct(rd, rs1, rs2, FPRoundingMode::DYN));
        }
    }

    pub fn fmul<const FP_SIZE: usize>(&mut self, rd: u8, rs1: u8, rs2: u8) {
        if FP_SIZE == 32 {
            self.insn(FMUL_S::construct(rd, rs1, rs2, FPRoundingMode::DYN));
        } else {
            self.insn(FMUL_D::construct(rd, rs1, rs2, FPRoundingMode::DYN));
        }
    }

    pub fn fdiv<const FP_SIZE: usize>(&mut self, rd: u8, rs1: u8, rs2: u8) {
        if FP_SIZE == 32 {
            self.insn(FDIV_S::construct(rd, rs1, rs2, FPRoundingMode::DYN));
        } else {
            self.insn(FDIV_D::construct(rd, rs1, rs2, FPRoundingMode::DYN));
        }
    }

    pub fn fsqrt<const FP_SIZE: usize>(&mut self, rd: u8, rs1: u8) {
        if FP_SIZE == 32 {
            self.insn(FSQRT_S::construct(rd, rs1, 0, FPRoundingMode::DYN));
        } else {
            self.insn(FSQRT_D::construct(rd, rs1, 0, FPRoundingMode::DYN));
        }
    }

    pub fn fsgnj<const FP_SIZE: usize>(&mut self, rd: u8, rs1: u8, rs2: u8) {
        if FP_SIZE == 32 {
            self.insn(FSGNJ_S::construct(rd, rs1, rs2));
        } else {
            self.insn(FSGNJ_D::construct(rd, rs1, rs2));
        }
    }

    pub fn fsgnjn<const FP_SIZE: usize>(&mut self, rd: u8, rs1: u8, rs2: u8) {
        if FP_SIZE == 32 {
            self.insn(FSGNJN_S::construct(rd, rs1, rs2));
        } else {
            self.insn(FSGNJN_D::construct(rd, rs1, rs2));
        }
    }

    pub fn fsgnjx<const FP_SIZE: usize>(&mut self, rd: u8, rs1: u8, rs2: u8) {
        if FP_SIZE == 32 {
            self.insn(FSGNJX_S::construct(rd, rs1, rs2));
        } else {
            self.insn(FSGNJX_D::construct(rd, rs1, rs2));
        }
    }

    pub fn fmin<const FP_SIZE: usize>(&mut self, rd: u8, rs1: u8, rs2: u8) {
        if FP_SIZE == 32 {
            self.insn(FMIN_S::construct(rd, rs1, rs2));
        } else {
            self.insn(FMIN_D::construct(rd, rs1, rs2));
        }
    }

    pub fn fmax<const FP_SIZE: usize>(&mut self, rd: u8, rs1: u8, rs2: u8) {
        if FP_SIZE == 32 {
            self.insn(FMAX_S::construct(rd, rs1, rs2));
        } else {
            self.insn(FMAX_D::construct(rd, rs1, rs2));
        }
    }

    pub fn feq<const FP_SIZE: usize>(&mut self, rd: u8, rs1: u8, rs2: u8) {
        if FP_SIZE == 32 {
            self.insn(FEQ_S::construct(rd, rs1, rs2));
        } else {
            self.insn(FEQ_D::construct(rd, rs1, rs2));
        }
    }

    pub fn flt<const FP_SIZE: usize>(&mut self, rd: u8, rs1: u8, rs2: u8) {
        if FP_SIZE == 32 {
            self.insn(FLT_S::construct(rd, rs1, rs2));
        } else {
            self.insn(FLT_D::construct(rd, rs1, rs2));
        }
    }

    pub fn fle<const FP_SIZE: usize>(&mut self, rd: u8, rs1: u8, rs2: u8) {
        if FP_SIZE == 32 {
            self.insn(FLE_S::construct(rd, rs1, rs2));
        } else {
            self.insn(FLE_D::construct(rd, rs1, rs2));
        }
    }

    pub fn fclass<const FP_SIZE: usize>(&mut self, rd: u8, rs1: u8) {
        if FP_SIZE == 32 {
            self.insn(FCLASS_S::construct(rd, rs1, 0));
        } else {
            self.insn(FCLASS_D::construct(rd, rs1, 0));
        }
    }

    pub fn fcvt_fp2si<const FP_SIZE: usize>(&mut self, rd: u8, rs1: u8, rm: FPRoundingMode) {
        if FP_SIZE == 32 {
            self.insn(FCVT_W_S::construct(rd, rs1, rm));
        } else {
            self.insn(FCVT_L_S::construct(rd, rs1, rm));
        }
    }

    pub fn fcvt_fp2ui<const FP_SIZE: usize>(&mut self, rd: u8, rs1: u8, rm: FPRoundingMode) {
        if FP_SIZE == 32 {
            self.insn(FCVT_WU_S::construct(rd, rs1, rm));
        } else {
            self.insn(FCVT_LU_S::construct(rd, rs1, rm));
        }
    }

    pub fn fcvt_si2fp<const FP_SIZE: usize>(&mut self, rd: u8, rs1: u8, rm: FPRoundingMode) {
        if FP_SIZE == 32 {
            self.insn(FCVT_S_W::construct(rd, rs1, rm));
        } else {
            self.insn(FCVT_S_L::construct(rd, rs1, rm));
        }
    }

    pub fn fcvt_ui2fp<const FP_SIZE: usize>(&mut self, rd: u8, rs1: u8, rm: FPRoundingMode) {
        if FP_SIZE == 32 {
            self.insn(FCVT_S_WU::construct(rd, rs1, rm));
        } else {
            self.insn(FCVT_S_LU::construct(rd, rs1, rm));
        }
    }

    pub fn fcvt_fp2fp<const FP_SIZE: usize, const FP_SIZE2: usize>(&mut self, rd: u8, rs1: u8, rm: FPRoundingMode) {
        if FP_SIZE == 32 && FP_SIZE2 == 64 {
            self.insn(FCVT_D_S::construct(rd, rs1, rm));
        } else if FP_SIZE == 64 && FP_SIZE2 == 32 {
            self.insn(FCVT_S_D::construct(rd, rs1, rm));
        }
    }

    pub fn fmv_fp2i<const FP_SIZE: usize>(&mut self, rd: u8, rs1: u8) {
        if FP_SIZE == 32 {
            self.insn(FMV_X_W::construct(rd, rs1));
        } else {
            self.insn(FMV_X_D::construct(rd, rs1));
        }
    }

    pub fn fmv_i2fp<const FP_SIZE: usize>(&mut self, rd: u8, rs1: u8) {
        if FP_SIZE == 32 {
            self.insn(FMV_W_X::construct(rd, rs1));
        } else {
            self.insn(FMV_D_X::construct(rd, rs1));
        }
    }

    pub fn fence(&mut self, pred: &[MemoryOperation], succ: &[MemoryAccess]) {
        let mut pred_val = 0;

        for op in pred.iter() {
            pred_val |= *op as u32;
        }

        let mut succ_val = 0;

        for op in succ.iter() {
            succ_val |= *op as u32;
        }

        let immediate = 0
            | (0b0000 << 8)
            | ((pred_val & ((1 << 4) - 1)) << 4)
            | ((succ_val & ((1 << 4) - 1)) << 0);

        self.insn(FENCE::construct(
            zero,
            zero,
            IImmediate::v32(immediate as i32),
        ));
    }

    pub fn lrw(&mut self, rd: u8, rs1: u8, access: &[MemoryAccess]) {
        self.insn(LR_W::construct(rd, rs1, zero, access))
    }

    pub fn scw(&mut self, rd: u8, rs1: u8, rs2: u8, access: &[MemoryAccess]) {
        self.insn(SC_W::construct(rd, rs1, rs2, access))
    }

    pub fn lrd(&mut self, rd: u8, rs1: u8, access: &[MemoryAccess]) {
        self.insn(LR_D::construct(rd, rs1, zero, access))
    }

    pub fn scd(&mut self, rd: u8, rs1: u8, rs2: u8, access: &[MemoryAccess]) {
        self.insn(SC_D::construct(rd, rs1, rs2, access))
    }

    pub fn amoswap(&mut self, rd: u8, rs1: u8, rs2: u8, access: &[MemoryAccess]) {
        self.insn(AMOSWAP_W::construct(rd, rs1, rs2, access))
    }

    pub fn amoor(&mut self, rd: u8, rs1: u8, rs2: u8, access: &[MemoryAccess]) {
        self.insn(AMOOR_W::construct(rd, rs1, rs2, access))
    }

    pub fn amoxor(&mut self, rd: u8, rs1: u8, rs2: u8, access: &[MemoryAccess]) {
        self.insn(AMOXOR_W::construct(rd, rs1, rs2, access))
    }

    pub fn amoand(&mut self, rd: u8, rs1: u8, rs2: u8, access: &[MemoryAccess]) {
        self.insn(AMOAND_W::construct(rd, rs1, rs2, access))
    }

    pub fn amomin(&mut self, rd: u8, rs1: u8, rs2: u8, access: &[MemoryAccess]) {
        self.insn(AMOMIN_W::construct(rd, rs1, rs2, access))
    }

    pub fn amomax(&mut self, rd: u8, rs1: u8, rs2: u8, access: &[MemoryAccess]) {
        self.insn(AMOMAX_W::construct(rd, rs1, rs2, access))
    }

    pub fn amominu(&mut self, rd: u8, rs1: u8, rs2: u8, access: &[MemoryAccess]) {
        self.insn(AMOMINU_W::construct(rd, rs1, rs2, access))
    }

    pub fn amomaxu(&mut self, rd: u8, rs1: u8, rs2: u8, access: &[MemoryAccess]) {
        self.insn(AMOMAXU_W::construct(rd, rs1, rs2, access))
    }

    pub fn amoswapd(&mut self, rd: u8, rs1: u8, rs2: u8, access: &[MemoryAccess]) {
        self.insn(AMOSWAP_D::construct(rd, rs1, rs2, access))
    }

    pub fn amoord(&mut self, rd: u8, rs1: u8, rs2: u8, access: &[MemoryAccess]) {
        self.insn(AMOOR_D::construct(rd, rs1, rs2, access))
    }

    pub fn amoxord(&mut self, rd: u8, rs1: u8, rs2: u8, access: &[MemoryAccess]) {
        self.insn(AMOXOR_D::construct(rd, rs1, rs2, access))
    }

    pub fn amoandd(&mut self, rd: u8, rs1: u8, rs2: u8, access: &[MemoryAccess]) {
        self.insn(AMOAND_D::construct(rd, rs1, rs2, access))
    }

    pub fn amomind(&mut self, rd: u8, rs1: u8, rs2: u8, access: &[MemoryAccess]) {
        self.insn(AMOMIN_D::construct(rd, rs1, rs2, access))
    }

    pub fn amomaxd(&mut self, rd: u8, rs1: u8, rs2: u8, access: &[MemoryAccess]) {
        self.insn(AMOMAX_D::construct(rd, rs1, rs2, access))
    }

    pub fn amominud(&mut self, rd: u8, rs1: u8, rs2: u8, access: &[MemoryAccess]) {
        self.insn(AMOMINU_D::construct(rd, rs1, rs2, access))
    }

    pub fn amomaxud(&mut self, rd: u8, rs1: u8, rs2: u8, access: &[MemoryAccess]) {
        self.insn(AMOMAXU_D::construct(rd, rs1, rs2, access))
    }

    /*pub fn slli(&mut self, rd: u8, rs1: u8, shamt: u32) {
        self.insn(SLLI::construct(rd, rs1, shamt))
    }

    pub fn srli(&mut self, rd: u8, rs1: u8, shamt: u32) {
        self.insn(SRLI::construct(rd, rs1, shamt))
    }

    pub fn srai(&mut self, rd: u8, rs1: u8, shamt: u32) {
        self.insn(SRAI::construct(rd, rs1, shamt))
    }*/

    pub fn slliw(&mut self, rd: u8, rs1: u8, shamt: u32) {
        self.insn(SLLIW::construct(rd, rs1, shamt))
    }

    pub fn srliw(&mut self, rd: u8, rs1: u8, shamt: u32) {
        self.insn(SRLIW::construct(rd, rs1, shamt))
    }

    pub fn sraiw(&mut self, rd: u8, rs1: u8, shamt: u32) {
        self.insn(SRAIW::construct(rd, rs1, shamt))
    }

    pub fn nop(&mut self) {
        self.addi(zero, zero, 0);
    }

    pub fn align(&mut self, alignment: usize) {
        while !self.buffer.is_aligned(alignment) {
            self.nop();
        }
    }

    pub fn mask_register(&mut self, rd: u8, rs: u8, mask: u32) {
        self.slli(rd, rs, 64 - mask);
        self.srli(rd, rd, 64 - mask);
    }

    pub fn sign_extend(&mut self, rd: u8, rs: u8, bits: u32) {
        if bits == 64 {
            return;
        }

        if bits == 32 {
            self.addiw(rd, rs, 0);
            return;
        }

        self.slli(rd, rs, 64 - bits);
        self.srai(rd, rd, 64 - bits);
    }

    pub fn zero_extend(&mut self, rd: u8, rs: u8, bits: u32) {
        if bits == 64 {
            return;
        }

        self.slli(rd, rs, 64 - bits);
        self.srli(rd, rd, 64 - bits);
    }

    pub fn insn(&mut self, insn: u32) {
        self.buffer.put_int(insn as _);
    }

    pub fn jump_placeholder(&mut self, functor: impl FnOnce(&mut Self)) {
        LinkJumpImpl::generate_placeholder(self, functor)
    }

    pub fn branch_placeholder(&mut self, functor: impl FnOnce(&mut Self)) {
        LinkBranchImpl::generate_placeholder(self, functor)
    }

    pub fn pointer_call_placeholder(&mut self, functor: impl FnOnce(&mut Self)) {
        PatchPointerImpl::generate_placeholder(self, functor)
    }

    pub fn near_call_placeholder(&mut self, functor: impl FnOnce(&mut Self)) {
        LinkCallImpl::generate_placeholder(self, functor)
    }
}

pub trait LinkJumpOrCallImpl {
    unsafe fn apply(location: *mut u32, target: *mut u8) {
        let instruction = InstructionValue::new(location.add(1).read());

        let destination = instruction.field::<7, 5>();
        let mut offset = target as i64 - location.add(1) as i64;
        if JImmediate::is_valid(offset) {
            location.write(ADDI::construct(x0, x0, IImmediate::v32(0)));
            location.add(1).write(JAL::construct(
                destination as _,
                JImmediate::v32(offset as i32),
            ));
            return;
        }

        offset += size_of::<u32>() as i64;

        let immediate = ImmediateDecomposition::new64(offset);
        location.write(AUIPC::construct(x30, immediate.upper));
        location
            .add(1)
            .write(JALR::construct(destination as _, x30, immediate.lower));
    }
}

pub struct LinkJumpImpl;

impl LinkJumpOrCallImpl for LinkJumpImpl {}

impl LinkJumpImpl {
    pub fn placeholder_insn() -> u32 {
        ADDI::construct(x0, x0, IImmediate::v32(1))
    }

    pub fn generate_placeholder(
        assembler: &mut RISCV64Assembler,
        functor: impl FnOnce(&mut RISCV64Assembler),
    ) {
        assembler.insn(Self::placeholder_insn());
        functor(assembler);
    }
}

pub struct LinkCallImpl;

impl LinkJumpOrCallImpl for LinkCallImpl {}

impl LinkCallImpl {
    pub fn placeholder_insn() -> u32 {
        ADDI::construct(x0, x0, IImmediate::v32(2))
    }

    pub fn generate_placeholder(
        assembler: &mut RISCV64Assembler,
        functor: impl FnOnce(&mut RISCV64Assembler),
    ) {
        assembler.insn(Self::placeholder_insn());
        functor(assembler);
    }
}

pub struct LinkBranchImpl;

impl LinkBranchImpl {
    pub fn placeholder_insn() -> u32 {
        ADDI::construct(x0, x0, IImmediate::v32(3))
    }

    pub fn generate_placeholder(
        assembler: &mut RISCV64Assembler,
        functor: impl FnOnce(&mut RISCV64Assembler),
    ) {
        let insn_value = Self::placeholder_insn();
        for _ in 0..2 {
            assembler.insn(insn_value);
        }
        functor(assembler);
    }

    pub unsafe fn apply(location: *mut u32, target: *mut u8) {
        let instruction = InstructionValue::new(location.add(2).read());
        assert!(instruction.opcode() == BRANCH);

        let branch_instruction_for_func3 = |funct3, rs1, rs2, imm: BImmediate| match funct3 {
            BEQ::FUNCT3_VALUE => BEQ::construct(rs1, rs2, imm),
            BNE::FUNCT3_VALUE => BNE::construct(rs1, rs2, imm),
            BLT::FUNCT3_VALUE => BLT::construct(rs1, rs2, imm),
            BGE::FUNCT3_VALUE => BGE::construct(rs1, rs2, imm),
            BLTU::FUNCT3_VALUE => BLTU::construct(rs1, rs2, imm),
            BGEU::FUNCT3_VALUE => BGEU::construct(rs1, rs2, imm),
            _ => unreachable!("Invalid funct3 value for patching branch: {}", funct3),
        };

        let lhs = instruction.field::<15, 5>() as u8;
        let rhs = instruction.field::<20, 5>() as u8;

        let mut offset = target as i64 - location.add(2) as i64;

        if BImmediate::is_valid(offset) {
            location.write(ADDI::construct(x0, x0, IImmediate::v32(0)));
            location
                .add(1)
                .write(ADDI::construct(x0, x0, IImmediate::v32(0)));
            location.add(2).write(branch_instruction_for_func3(
                instruction.field::<12, 3>(),
                lhs,
                rhs,
                BImmediate::v32(offset as i32),
            ));

            return;
        }

        if JImmediate::is_valid(offset) {
            location.write(ADDI::construct(x0, x0, IImmediate::v32(0)));
            location.add(1).write(branch_instruction_for_func3(
                instruction.field::<12, 3>() ^ 0b001,
                lhs,
                rhs,
                BImmediate::v32(8),
            ));
            location
                .add(2)
                .write(JAL::construct(x0, JImmediate::v32(offset as i32)));
            return;
        }

        offset += size_of::<u32>() as i64;

        let immediate = ImmediateDecomposition::new64(offset);

        location.write(branch_instruction_for_func3(
            instruction.field::<12, 3>() ^ 0b001,
            lhs,
            rhs,
            BImmediate::v32(12),
        ));

        location
            .add(1)
            .write(AUIPC::construct(x31, immediate.upper));
        location
            .add(2)
            .write(JALR::construct(x0, x30, immediate.lower));
    }
}

pub struct PatchPointerImpl;

impl PatchPointerImpl {
    pub fn placeholder_insn() -> u32 {
        ADDI::construct(x0, x0, IImmediate::v32(4))
    }

    pub fn generate_placeholder(
        assembler: &mut RISCV64Assembler,
        functor: impl FnOnce(&mut RISCV64Assembler),
    ) {
        let insn_value = Self::placeholder_insn();
        for _ in 0..7 {
            assembler.insn(insn_value);
        }
        functor(assembler);
    }

    pub unsafe fn apply(location: *mut u32, value: *mut u8) {
        let instruction = InstructionValue::new(location.add(7).read());
        let valid_location =
            instruction.opcode() == OP_IMM && instruction.field::<12, 3>() == 0b000;
        assert!(valid_location);

        Self::apply_with_dest(location, instruction.field::<7, 5>() as _, value)
    }

    pub unsafe fn apply_with_dest(location: *mut u32, destination: u8, value: *mut u8) {
        let imml = ImmediateLoader::new_placeholder(value as i64);

        for i in 0..imml.opcount {
            let op = imml.ops[imml.opcount - (i + 1)];

            match op.op_type {
                ImmOpType::IImmediate => {
                    location.add(i).write(ADDI::construct(
                        destination,
                        zero,
                        IImmediate::new(op.value),
                    ));
                }

                ImmOpType::LUI => {
                    location
                        .add(i)
                        .write(LUI::construct(destination, UImmediate::new(op.value)));
                }

                ImmOpType::ADDI => {
                    location.add(i).write(ADDI::construct(
                        destination,
                        destination,
                        IImmediate::new(op.value),
                    ));
                }

                ImmOpType::LSHIFT12 => {
                    location
                        .add(i)
                        .write(SLLI::construct(destination, destination, 12));
                }

                ImmOpType::NOP => {
                    location
                        .add(i)
                        .write(ADDI::construct(x0, x0, IImmediate::v32(0)));
                }
            }
        }
    }

    pub unsafe fn read(location: *mut u32) -> *mut u8 {
        let instruction_value = InstructionValue::new(location.add(7).read());

        assert!(
            instruction_value.opcode() == OP_IMM && instruction_value.field::<12, 3>() == 0b000
        );

        let dest = instruction_value.field::<7, 5>() as u8;

        let mut i = 0;

        {
            // Iterate through all NOP instructions generated for the purposes of the placeholder.
            let nop_insn = ADDI::construct(x0, x0, IImmediate::v32(0));

            while i < 8 {
                if location.add(i).read() != nop_insn {
                    break;
                }
                i += 1;
            }
        }

        let mut target = 0;

        while i < 8 {
            let insn = InstructionValue::new(location.add(i).read());
            if insn.opcode() == LUI && insn.field::<7, 5>() == dest as u32 {
                target = UImmediate::value(insn) as i32 as i64;
            } else if insn.opcode() == OP_IMM && insn.field::<12, 3>() == 0b000 {
                target += IImmediate::value(insn) as i64;
            } else {
                assert!(insn.value == SLLI::construct(dest, dest, 12));
                target <<= 12;
            }

            i += 1;
        }

        target as *mut u8
    }
}

pub struct ImmediateLoader {
    ops: [ImmOp; 8],
    opcount: usize,
}

impl ImmediateLoader {
    pub fn new(imm: i64) -> Self {
        let mut this = Self {
            ops: [ImmOp {
                op_type: ImmOpType::NOP,
                value: 0,
            }; 8],
            opcount: 0,
        };
        // If the immediate value fits into the IImmediate mold, we can short-cut to just generating that through a single ADDI.
        if IImmediate::is_valid(imm) {
            
            this.ops[this.opcount] = ImmOp {
                op_type: ImmOpType::IImmediate,
                value: IImmediate::v32::<i32>(imm as i32) as u32,
            };
            this.opcount += 1;
            return this;
        }

        // The immediate is larger than 12 bits, so it has to be loaded through the initial LUI and then additional shift-and-addi pairs.
        // This sequence is generated in reverse. move_into() or other users traverse the sequence accordingly.
        let mut value = imm;

        loop {
            let add_imm = value & ((1 << 12) - 1);
            // The addi will be sign-extending the 12-bit value and adding it to the register-contained value. If the addi-immediate
            // is negative, the remaining immediate has to be increased by 2^12 to offset the subsequent subtraction.
            if add_imm & (1 << 11) != 0 {
                value += 1 << 12;
            }

            this.ops[this.opcount] = ImmOp {
                op_type: ImmOpType::ADDI,
                value: add_imm as u32,
            };

            this.opcount += 1;

            // Shift out the bits incorporated into the just-added addi.
            value = value >> 12;

            // If the remainder of the immediate can fit into a 20-bit immediate, we can generate the LUI instruction that will end up
            // loading the initial higher bits of the desired immediate.
            if is_valid64::<20>(value as _) {
                this.ops[this.opcount] = ImmOp {
                    op_type: ImmOpType::LUI,
                    value: ((value & ((1 << 20) - 1)) << 12) as u32,
                };
                this.opcount += 1;
                break;
            }

            // Otherwise, generate the lshift operation that will make room for lower parts of the immediate value.
            this.ops[this.opcount] = ImmOp {
                op_type: ImmOpType::LSHIFT12,
                value: 0,
            };

            this.opcount += 1;
        }

        this
    }

    pub fn new_placeholder(imm: i64) -> Self {
        let mut this = Self::new(imm);
        // The non-placeholder constructor already generated the necessary operations to load this immediate.
        // This constructor still fills out the remaining potential operations as nops. This enables future patching
        // of these instructions with other immediate-load sequences.
        for i in this.opcount..this.ops.len() {
            this.ops[i] = ImmOp {
                op_type: ImmOpType::NOP,
                value: 0,
            };
        }

        this.opcount = this.ops.len();
        this
    }

    pub fn move_into(&self, assembler: &mut RISCV64Assembler, dest: u8) {
        // This is a helper method that generates the necessary instructions through the RISCV64Assembler infrastructure.
        // Operations are traversed in reverse in order to match the generation process.
        println!("Loading immediate: {:#x} {}", self.ops[0].value, self.opcount);
        for i in 0..self.opcount {
            let op = self.ops[self.opcount - (i + 1)];

            match op.op_type {
                ImmOpType::IImmediate => {
                    assembler.addi(dest, zero, op.value as _);
                }

                ImmOpType::LUI => {
                    assembler.lui(dest, op.value as _);
                }

                ImmOpType::ADDI => {
                    assembler.addi(dest, dest, op.value as _);
                }

                ImmOpType::LSHIFT12 => {
                    assembler.slli(dest, dest, 12);
                }

                ImmOpType::NOP => {
                    assembler.addi(zero, zero, 0);
                }
            }
        }
    }
}

pub fn is_valid64<const N: usize>(imm: i64) -> bool {
    let shift = std::mem::size_of::<i64>() * 8 - N;
    imm == ((imm << shift) >> shift)
}

pub fn is_valid32<const N: usize>(imm: i32) -> bool {
    let shift = std::mem::size_of::<i32>() * 8 - N;
    imm == ((imm << shift) >> shift)
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum ImmOpType {
    IImmediate,
    LUI,
    ADDI,
    LSHIFT12,
    NOP,
}
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ImmOp {
    pub op_type: ImmOpType,
    pub value: u32,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum Condition {
    EQ,
    NE,
    GTU,
    LEU,
    GEU,
    LTU,
    GT,
    LE,
    GE,
    LT,
}

impl Condition {
    pub fn invert(self) -> Self {
        match self {
            Condition::EQ => Condition::NE,
            Condition::NE => Condition::EQ,
            Condition::GTU => Condition::LEU,
            Condition::LEU => Condition::GTU,
            Condition::GEU => Condition::LTU,
            Condition::LTU => Condition::GEU,
            Condition::GT => Condition::LE,
            Condition::LE => Condition::GT,
            Condition::GE => Condition::LT,
            Condition::LT => Condition::GE,
        }
    }
}
