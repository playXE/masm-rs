#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Debug, Hash)]
#[repr(u8)]
pub enum RegisterID {
    EAX,
    ECX,
    EDX,
    EBX,
    ESP,
    EBP,
    ESI,
    EDI,

    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Debug, Hash)]
#[repr(u8)]
pub enum XMMRegisterID {
    XMM0,
    XMM1,
    XMM2,
    XMM3,
    XMM4,
    XMM5,
    XMM6,
    XMM7,
}

pub struct X86Asm {
    formatter: X86AsmFormatter,
}

impl X86Asm {
    pub fn new(x64: bool) -> Self {
        Self {
            formatter: X86AsmFormatter {
                buffer: AsmBuffer::new(),
                x64,
            },
        }
    }

    pub fn nop(&mut self) {
        self.formatter.one_byte_op(OP_NOP);
    }

    pub fn label(&self) -> AsmLabel {
        self.formatter.label()
    }

    pub fn push_r(&mut self, r: RegisterID) {
        self.formatter.one_byte_op_r(OP_PUSH_EAX, r);
    }

    pub fn pop_r(&mut self, r: RegisterID) {
        self.formatter.one_byte_op_r(OP_POP_EAX, r);
    }

    pub fn push_i32(&mut self, imm: i32) {
        self.formatter.one_byte_op(OP_PUSH_Iz);
        self.formatter.buffer.put_int(imm);
    }

    pub fn push_m(&mut self, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op_off(OP_GROUP1A_Ev, GROUP5_OP_PUSH as _, base, offset);
    }

    pub fn pop_m(&mut self, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op_off(OP_GROUP1A_Ev, GROUP1A_OP_POP as _, base, offset);
    }

    pub fn adcl_im(&mut self, imm: i32, addr: u32) {
        if imm as i8 as i32 == imm {
            self.formatter
                .one_byte_op_x86addr(OP_GROUP1_EvIb, GROUP1_OP_ADC as _, addr);
            self.formatter.buffer.put_byte(imm as _);
        } else {
            self.formatter
                .one_byte_op_x86addr(OP_GROUP1_EvIz, GROUP1_OP_ADC as _, addr);
            self.formatter.buffer.put_int(imm as _);
        }
    }

    pub fn addl_rr(&mut self, src: RegisterID, dst: RegisterID) {
        self.formatter.one_byte_op_rm(OP_ADD_EvGv, src as _, dst);
    }

    pub fn addl_mr(&mut self, offset: i32, base: RegisterID, dst: RegisterID) {
        self.formatter
            .one_byte_op_off(OP_ADD_GvEv, dst as _, base, offset);
    }

    pub fn addl_ar(&mut self, addr: u32, dst: RegisterID) {
        self.formatter
            .one_byte_op_x86addr(OP_ADD_GvEv, dst as _, addr);
    }

    pub fn addl_rm(&mut self, src: RegisterID, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op_off(OP_ADD_EvGv, src as _, base, offset);
    }

    pub fn addl_ir(&mut self, imm: i32, dst: RegisterID) {
        if imm as i8 as i32 == imm {
            self.formatter
                .one_byte_op_rm(OP_GROUP1_EvIb, GROUP1_OP_ADD as _, dst);
            self.formatter.buffer.put_byte(imm as _);
        } else {
            self.formatter
                .one_byte_op_rm(OP_GROUP1_EvIz, GROUP1_OP_ADD as _, dst);
            self.formatter.buffer.put_int(imm as _);
        }
    }

    pub fn addl_im(&mut self, imm: i32, offset: i32, base: RegisterID) {
        if imm as i8 as i32 == imm {
            self.formatter
                .one_byte_op_off(OP_GROUP1_EvIb, GROUP1_OP_ADD as _, base, offset);
            self.formatter.buffer.put_byte(imm as _);
        } else {
            self.formatter
                .one_byte_op_off(OP_GROUP1_EvIz, GROUP1_OP_ADD as _, base, offset);
            self.formatter.buffer.put_int(imm as _);
        }
    }
    pub fn addq_rr(&mut self, src: RegisterID, dst: RegisterID) {
        self.formatter.one_byte_op64_rm(OP_ADD_EvGv, src as _, dst);
    }

    pub fn addq_mr(&mut self, offset: i32, base: RegisterID, dst: RegisterID) {
        self.formatter
            .one_byte_op64_off(OP_ADD_GvEv, dst as _, base, offset);
    }

    pub fn addq_rm(&mut self, src: RegisterID, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op64_off(OP_ADD_EvGv, src as _, base, offset);
    }

    pub fn addq_ir(&mut self, imm: i32, dst: RegisterID) {
        if imm as i8 as i32 == imm {
            self.formatter
                .one_byte_op64_rm(OP_GROUP1_EvIb, GROUP1_OP_ADD as _, dst);
            self.formatter.buffer.put_byte(imm as _);
        } else {
            self.formatter
                .one_byte_op64_rm(OP_GROUP1_EvIz, GROUP1_OP_ADD as _, dst);
            self.formatter.buffer.put_int(imm as _);
        }
    }

    pub fn addq_im(&mut self, imm: i32, offset: i32, base: RegisterID) {
        if imm as i8 as i32 == imm {
            self.formatter
                .one_byte_op64_off(OP_GROUP1_EvIb, GROUP1_OP_ADD as _, base, offset);
            self.formatter.buffer.put_byte(imm as _);
        } else {
            self.formatter
                .one_byte_op64_off(OP_GROUP1_EvIz, GROUP1_OP_ADD as _, base, offset);
            self.formatter.buffer.put_int(imm as _);
        }
    }

    pub fn addl_im_addr(&mut self, imm: i32, addr: u32) {
        if imm as i8 as i32 == 0 {
            self.formatter
                .one_byte_op_x86addr(OP_GROUP1_EvIb, GROUP1_OP_ADD as _, addr);
            self.formatter.buffer.put_byte(imm as _);
        } else {
            self.formatter
                .one_byte_op_x86addr(OP_GROUP1_EvIz, GROUP1_OP_ADD as _, addr);
            self.formatter.buffer.put_int(imm as _);
        }
    }
}

use super::assembler_buffer::*;

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Debug, Hash)]
#[repr(u8)]
pub enum ModRmMode {
    NoDisp,
    Disp0,
    Disp8,
    Disp32,
    Register,
}

pub struct X86AsmFormatter {
    buffer: AsmBuffer,
    x64: bool,
}
pub enum Condition {
    O,
    NO,
    B,
    AE,
    E,
    NE,
    BE,
    A,
    S,
    NS,
    P,
    NP,
    L,
    GE,
    LE,
    G,
}
macro_rules! c {
        ($val: ident = $x: expr) => {
            pub const $val: u8= $x;
        };
        ($($val: ident = $x: expr),*) => {
            $(
                c!($val = $x);
            )*
        }
    }

pub const COND_C: Condition = Condition::B;
pub const COND_NC: Condition = Condition::AE;
pub mod OneByteOpcodeId {
    c!(
        OP_ADD_EvGv = 0x01,
        OP_ADD_GvEv = 0x03,
        OP_OR_EvGv = 0x09,
        OP_OR_GvEv = 0x0B,
        OP_2BYTE_ESCAPE = 0x0F,
        OP_AND_EvGv = 0x21,
        OP_AND_GvEv = 0x23,
        OP_SUB_EvGv = 0x29,
        OP_SUB_GvEv = 0x2B,
        PRE_PREDICT_BRANCH_NOT_TAKEN = 0x2E,
        OP_XOR_EvGv = 0x31,
        OP_XOR_GvEv = 0x33,
        OP_CMP_EvGv = 0x39,
        OP_CMP_GvEv = 0x3B,
        PRE_REX = 0x40,
        OP_PUSH_EAX = 0x50,
        OP_POP_EAX = 0x58,
        OP_MOVSXD_GvEv = 0x63,
        PRE_OPERAND_SIZE = 0x66,
        PRE_SSE_66 = 0x66,
        OP_PUSH_Iz = 0x68,
        OP_IMUL_GvEvIz = 0x69,
        OP_GROUP1_EbIb = 0x80,
        OP_GROUP1_EvIz = 0x81,
        OP_GROUP1_EvIb = 0x83,
        OP_TEST_EbGb = 0x84,
        OP_TEST_EvGv = 0x85,
        OP_XCHG_EvGv = 0x87,
        OP_MOV_EbGb = 0x88,
        OP_MOV_EvGv = 0x89,
        OP_MOV_GvEv = 0x8B,
        OP_LEA = 0x8D,
        OP_GROUP1A_Ev = 0x8F,
        OP_NOP = 0x90,
        OP_CDQ = 0x99,
        OP_MOV_EAXOv = 0xA1,
        OP_MOV_OvEAX = 0xA3,
        OP_MOV_EAXIv = 0xB8,
        OP_GROUP2_EvIb = 0xC1,
        OP_RET = 0xC3,
        OP_GROUP11_EvIb = 0xC6,
        OP_GROUP11_EvIz = 0xC7,
        OP_INT3 = 0xCC,
        OP_GROUP2_Ev1 = 0xD1,
        OP_GROUP2_EvCL = 0xD3,
        OP_ESCAPE_DD = 0xDD,
        OP_CALL_rel32 = 0xE8,
        OP_JMP_rel32 = 0xE9,
        PRE_SSE_F2 = 0xF2,
        PRE_SSE_F3 = 0xF3,
        OP_HLT = 0xF4,
        OP_GROUP3_EbIb = 0xF6,
        OP_GROUP3_Ev = 0xF7,
        OP_GROUP3_EvIz = 0xF7, // OP_GROUP3_Ev has an immediate, when instruction is a test.
        OP_GROUP5_Ev = 0xFF
    );
}
pub mod TwoByteOpcodeID {
    c!(
        OP2_MOVSD_VsdWsd = 0x10,
        OP2_MOVSD_WsdVsd = 0x11,
        OP2_MOVSS_VsdWsd = 0x10,
        OP2_MOVSS_WsdVsd = 0x11,
        OP2_CVTSI2SD_VsdEd = 0x2A,
        OP2_CVTTSD2SI_GdWsd = 0x2C,
        OP2_UCOMISD_VsdWsd = 0x2E,
        OP2_ADDSD_VsdWsd = 0x58,
        OP2_MULSD_VsdWsd = 0x59,
        OP2_CVTSD2SS_VsdWsd = 0x5A,
        OP2_CVTSS2SD_VsdWsd = 0x5A,
        OP2_SUBSD_VsdWsd = 0x5C,
        OP2_DIVSD_VsdWsd = 0x5E,
        OP2_SQRTSD_VsdWsd = 0x51,
        OP2_ANDNPD_VpdWpd = 0x55,
        OP2_XORPD_VpdWpd = 0x57,
        OP2_MOVD_VdEd = 0x6E,
        OP2_MOVD_EdVd = 0x7E,
        OP2_JCC_rel32 = 0x80,
        OP_SETCC = 0x90,
        OP2_IMUL_GvEv = 0xAF,
        OP2_MOVZX_GvEb = 0xB6,
        OP2_MOVSX_GvEb = 0xBE,
        OP2_MOVZX_GvEw = 0xB7,
        OP2_MOVSX_GvEw = 0xBF,
        OP2_PEXTRW_GdUdIb = 0xC5,
        OP2_PSLLQ_UdqIb = 0x73,
        OP2_PSRLQ_UdqIb = 0x73,
        OP2_POR_VdqWdq = 0xEB
    );
}
use OneByteOpcodeId::*;
use TwoByteOpcodeID::*;
fn jcc_rel32(c: Condition) -> u8 {
    return OP2_JCC_rel32 + c as u8;
}

fn setcc_opcode(c: Condition) -> u8 {
    return OP_SETCC + c as u8;
}
pub mod GroupOpcodeID {
    c!(
        GROUP1_OP_ADD = 0,
        GROUP1_OP_OR = 1,
        GROUP1_OP_ADC = 2,
        GROUP1_OP_AND = 4,
        GROUP1_OP_SUB = 5,
        GROUP1_OP_XOR = 6,
        GROUP1_OP_CMP = 7,
        GROUP1A_OP_POP = 0,
        GROUP2_OP_ROL = 0,
        GROUP2_OP_ROR = 1,
        GROUP2_OP_RCL = 2,
        GROUP2_OP_RCR = 3,
        GROUP2_OP_SHL = 4,
        GROUP2_OP_SHR = 5,
        GROUP2_OP_SAR = 7,
        GROUP3_OP_TEST = 0,
        GROUP3_OP_NOT = 2,
        GROUP3_OP_NEG = 3,
        GROUP3_OP_IDIV = 7,
        GROUP5_OP_CALLN = 2,
        GROUP5_OP_JMPN = 4,
        GROUP5_OP_PUSH = 6,
        GROUP11_MOV = 0,
        GROUP14_OP_PSLLQ = 6,
        GROUP14_OP_PSRLQ = 2,
        ESCAPE_DD_FSTP_doubleReal = 3
    );
}
use GroupOpcodeID::*;
impl X86AsmFormatter {
    pub const NO_IX: RegisterID = RegisterID::ESP;
    pub const HAS_SIB: RegisterID = RegisterID::ESP;
    pub const NO_BASE: RegisterID = RegisterID::EBP;
    pub const NO_BASE2: RegisterID = RegisterID::R13;
    pub const HAS_SIB2: RegisterID = RegisterID::R12;
    /// Legacy prefix bytes:
    ///
    /// These are emmitted prior to the instruction.
    pub fn prefix(&mut self, pre: u8) {
        self.buffer.put_byte(pre as _);
    }
    /// Word-sized operands / no operand instruction formatters.
    ///
    /// In addition to the opcode, the following operand permutations are supported:
    ///   * None - instruction takes no operands.
    ///   * One register - the low three bits of the RegisterID are added into the opcode.
    ///   * Two registers - encode a register form ModRm (for all ModRm formats, the reg field is passed first, and a GroupOpcodeID may be passed in its place).
    ///   * Three argument ModRM - a register, and a register and an offset describing a memory operand.
    ///   * Five argument ModRM - a register, and a base register, an index, scale, and offset describing a memory operand.
    ///
    /// For 32-bit x86 targets, the address operand may also be provided as a *mut u8.
    /// On 64-bit targets REX prefixes will be planted as necessary, where high numbered registers are used.
    ///
    /// The twoByteOp methods plant two-byte Intel instructions sequences (first opcode byte 0x0F).
    pub fn one_byte_op(&mut self, opcode: u8) {
        self.buffer.put_byte(opcode as _);
    }

    pub fn one_byte_op_r(&mut self, op: u8, r: RegisterID) {
        self.emit_rex_if_needed(0, 0, r as _);
        self.buffer.put_byte(op as _);
    }
    pub fn one_byte_op_rm(&mut self, op: u8, reg: i32, rm: RegisterID) {
        self.emit_rex_if_needed(reg, 0, rm as _);
        self.buffer.put_byte(op as _);
        self.register_modrm(reg, rm);
    }
    pub fn one_byte_op_off(&mut self, op: u8, reg: i32, base: RegisterID, offset: i32) {
        self.emit_rex_if_needed(reg, 0, base as _);
        self.buffer.put_byte(op as _);
        self.memory_modrm(reg, base, offset);
    }
    pub fn one_byte_op_off32(&mut self, op: u8, reg: i32, base: RegisterID, offset: i32) {
        self.emit_rex_if_needed(reg, 0, base as _);
        self.buffer.put_byte(op as _);
        self.memory_modrm_disp32(reg, base, offset);
    }
    pub fn one_byte_op_off8(&mut self, op: u8, reg: i32, base: RegisterID, offset: i32) {
        self.emit_rex_if_needed(reg, 0, base as _);
        self.buffer.put_byte(op as _);
        self.memory_modrm_disp8(reg, base, offset);
    }

    pub fn one_byte_op_scaled(
        &mut self,
        op: u8,
        reg: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
        offset: i32,
    ) {
        self.emit_rex_if_needed(reg, index as _, base as _);
        self.buffer.put_byte(op as _);
        self.memory_modrm_scaled(reg, base, index, scale, offset);
    }

    pub fn one_byte_op_x86addr(&mut self, op: u8, reg: i32, addr: u32) {
        self.buffer.put_byte(op as _);
        self.memory_modrm_addr(reg, addr);
    }

    pub fn two_byte_op(&mut self, op: u8) {
        self.buffer.put_byte(OP_2BYTE_ESCAPE as _);
        self.buffer.put_byte(op as _);
    }
    pub fn two_byte_op_rm(&mut self, op: u8, reg: i32, rm: RegisterID) {
        self.emit_rex_if_needed(reg, 0, rm as _);
        self.buffer.put_byte(OP_2BYTE_ESCAPE as _);
        self.buffer.put_byte(op as _);
        self.register_modrm(reg, rm);
    }

    pub fn two_byte_op_off(&mut self, op: u8, reg: i32, base: RegisterID, offset: i32) {
        self.emit_rex_if_needed(reg, 0, base as _);
        self.buffer.put_byte(OP_2BYTE_ESCAPE as _);
        self.buffer.put_byte(op as _);
        self.memory_modrm(reg, base, offset);
    }
    pub fn two_byte_op_scaled(
        &mut self,
        op: u8,
        reg: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
        offset: i32,
    ) {
        self.emit_rex_if_needed(reg, index as _, base as _);
        self.buffer.put_byte(OP_2BYTE_ESCAPE as _);
        self.buffer.put_byte(op as _);
        self.memory_modrm_scaled(reg, base, index, scale, offset);
    }

    pub fn two_byte_op_x86addr(&mut self, op: u8, reg: i32, addr: u32) {
        self.buffer.put_byte(OP_2BYTE_ESCAPE as _);
        self.buffer.put_byte(op as _);
        self.memory_modrm_addr(reg, addr);
    }
    /// Quad-word-sized operands:
    ///
    /// Used to format 64-bit operantions, planting a REX.w prefix.
    /// When planting d64 or f64 instructions, not requiring a REX.w prefix,
    ///` the normal (non-'64'-postfixed) formatters should be used.
    pub fn one_byte_op64(&mut self, op: u8) {
        self.emit_rexw(0, 0, 0);
        self.buffer.put_byte(op as _);
    }

    pub fn one_byte_op64_r(&mut self, op: u8, reg: i32) {
        self.emit_rexw(0, 0, reg);
        self.buffer.put_byte(op as i8 + (reg & 7) as i8);
    }
    pub fn one_byte_op64_rm(&mut self, op: u8, reg: i32, rm: RegisterID) {
        self.emit_rexw(reg, 0, rm as _);
        self.buffer.put_byte(op as _);
        self.register_modrm(reg, rm);
    }

    pub fn one_byte_op64_off(&mut self, op: u8, reg: i32, base: RegisterID, offset: i32) {
        self.emit_rexw(reg, 0, base as _);
        self.buffer.put_byte(op as _);
        self.memory_modrm(reg, base, offset);
    }
    pub fn one_byte_op64_off32(&mut self, op: u8, reg: i32, base: RegisterID, offset: i32) {
        self.emit_rexw(reg, 0, base as _);
        self.buffer.put_byte(op as _);
        self.memory_modrm_disp32(reg, base, offset);
    }
    pub fn one_byte_op64_off8(&mut self, op: u8, reg: i32, base: RegisterID, offset: i32) {
        self.emit_rexw(reg, 0, base as _);
        self.buffer.put_byte(op as _);
        self.memory_modrm_disp8(reg, base, offset);
    }
    pub fn two_byte_op64_rm(&mut self, op: u8, reg: i32, rm: RegisterID) {
        self.emit_rexw(reg, 0, rm as _);
        self.buffer.put_byte(OP_2BYTE_ESCAPE as _);
        self.buffer.put_byte(op as _);
        self.register_modrm(reg, rm);
    }
    /// Byte-operands:
    ///
    /// These methods format byte operations.  Byte operations differ from the normal
    /// formatters in the circumstances under which they will decide to emit REX prefixes.
    /// These should be used where any register operand signifies a byte register.
    ///
    /// The disctinction is due to the handling of register numbers in the range 4..7 on
    /// x86-64.  These register numbers may either represent the second byte of the first
    /// four registers (ah..bh) or the first byte of the second four registers (spl..dil).
    ///
    /// Since ah..bh cannot be used in all permutations of operands (specifically cannot
    /// be accessed where a REX prefix is present), these are likely best treated as
    /// deprecated.  In order to ensure the correct registers spl..dil are selected a
    /// REX prefix will be emitted for any byte register operand in the range 4..15.
    ///
    /// These formatters may be used in instructions where a mix of operand sizes, in which
    /// case an unnecessary REX will be emitted, for example:
    ///     movzbl %al, %edi
    /// In this case a REX will be planted since edi is 7 (and were this a byte operand
    /// a REX would be required to specify dil instead of bh).  Unneeded REX prefixes will
    /// be silently ignored by the processor.
    ///
    /// Address operands should still be checked using reg_requires_rex(), while byte_reg_requires_rex()
    /// is provided to check byte register operands.
    pub fn one_byte_op8_grm(&mut self, op: u8, group_op: i32, rm: RegisterID) {
        self.emit_rex_if(self.byte_reg_requires_rex(rm as _), 0, 0, rm as _);
        self.buffer.put_byte(op as _);
        self.register_modrm(group_op as _, rm);
    }
    pub fn one_byte_op8_rm(&mut self, op: u8, reg: i32, rm: RegisterID) {
        self.emit_rex_if(
            self.byte_reg_requires_rex(reg) || self.byte_reg_requires_rex(rm as _),
            reg,
            0,
            rm as _,
        );
        self.buffer.put_byte(op as _);
        self.register_modrm(reg, rm);
    }

    pub fn one_byte_op8_scaled(
        &mut self,
        op: u8,
        reg: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
        offset: i32,
    ) {
        self.emit_rex_if(
            self.byte_reg_requires_rex(reg)
                || self.byte_reg_requires_rex(index as _)
                || self.byte_reg_requires_rex(base as _),
            reg,
            index as _,
            base as _,
        );
        self.buffer.put_byte(op as _);
        self.memory_modrm_scaled(reg, base, index, scale, offset);
    }
    pub fn two_byte_op8_rm(&mut self, op: u8, reg: RegisterID, rm: RegisterID) {
        self.emit_rex_if(
            self.byte_reg_requires_rex(reg as _) | self.byte_reg_requires_rex(rm as _),
            reg as _,
            0,
            rm as _,
        );
        self.buffer.put_byte(OP_2BYTE_ESCAPE as _);
        self.buffer.put_byte(op as _);
        self.register_modrm(reg as _, rm);
    }
    pub fn two_byte_op8_grm(&mut self, op: u8, group: i32, rm: RegisterID) {
        self.emit_rex_if(self.byte_reg_requires_rex(rm as _), 0, 0, rm as _);
        self.buffer.put_byte(OP_2BYTE_ESCAPE as _);
        self.buffer.put_byte(op as _);
        self.register_modrm(group, rm);
    }

    pub fn reg_requires_rex(&self, r: i32) -> bool {
        r >= RegisterID::R8 as i32 && self.x64
    }

    pub fn byte_reg_requires_rex(&self, r: i32) -> bool {
        r >= RegisterID::ESP as i32 && self.x64
    }

    pub fn emit_rex(&mut self, w: bool, r: i32, x: i32, b: i32) {
        if self.x64 {
            self.buffer.put_byte(
                PRE_REX as i8
                    | ((w as i32) << 3) as i8
                    | ((r >> 3) << 2) as i8
                    | ((x >> 3) << 1) as i8
                    | (b >> 3) as i8,
            );
        }
    }

    pub fn emit_rexw(&mut self, r: i32, x: i32, b: i32) {
        self.emit_rex(true, r, x, b);
    }

    pub fn emit_rex_if(&mut self, c: bool, r: i32, x: i32, b: i32) {
        if c && self.x64 {
            self.emit_rex(false, r, x, b);
        }
    }
    pub fn emit_rex_if_needed(&mut self, r: i32, x: i32, b: i32) {
        if self.x64 {
            self.emit_rex_if(
                self.reg_requires_rex(r) || self.reg_requires_rex(x) || self.reg_requires_rex(b),
                r,
                x,
                b,
            );
        }
    }

    pub fn put_modrm(&mut self, mode: ModRmMode, reg: i32, rm: RegisterID) {
        self.buffer
            .put_byte(((mode as i8) << 6) | ((reg & 7) << 3) as i8 | (rm as i8 & 7));
    }

    pub fn put_modrm_sib(
        &mut self,
        mode: ModRmMode,
        reg: i32,
        base: RegisterID,
        ix: RegisterID,
        scale: i32,
    ) {
        self.put_modrm(mode, reg, RegisterID::ESP);
        self.buffer
            .put_byte((scale << 6) as i8 | ((ix as i32 & 7) << 3) as i8 | (base as i32 & 7) as i8);
    }

    pub fn register_modrm(&mut self, reg: i32, rm: RegisterID) {
        self.put_modrm(ModRmMode::Register, reg, rm);
    }

    pub fn memory_modrm(&mut self, reg: i32, base: RegisterID, offset: i32) {
        if base == RegisterID::ESP || (self.x64 && base == RegisterID::R12) {
            if offset == 0 {
                self.put_modrm_sib(ModRmMode::NoDisp, reg, base, Self::NO_IX, 0);
            } else if offset as i8 as i32 == offset {
                self.put_modrm_sib(ModRmMode::Disp8, reg, base, Self::NO_IX, 0);
                self.buffer.put_byte(offset as _);
            } else {
                self.put_modrm_sib(ModRmMode::Disp32, reg, base, Self::NO_IX, 0);
                self.buffer.put_int(offset as _);
            }
        } else {
            let c = if self.x64 {
                offset == 0 && base != Self::NO_BASE && base != Self::NO_BASE
            } else {
                offset == 0 && base != Self::NO_BASE
            };
            if c {
                self.put_modrm(ModRmMode::NoDisp, reg, base);
            } else if offset as i8 as i32 == offset {
                self.put_modrm(ModRmMode::Disp8, reg, base);
                self.buffer.put_byte(offset as _);
            } else {
                self.put_modrm(ModRmMode::Disp32, reg, base);
                self.buffer.put_int(offset as _);
            }
        }
    }
    pub fn label(&self) -> AsmLabel {
        self.buffer.label()
    }

    pub fn memory_modrm_disp8(&mut self, reg: i32, base: RegisterID, offset: i32) {
        if base == RegisterID::ESP || (self.x64 && base == RegisterID::R12) {
            self.put_modrm_sib(ModRmMode::Disp8, reg, base, Self::NO_IX, 0);
            self.buffer.put_byte(offset as _);
        } else {
            self.put_modrm(ModRmMode::Disp8, reg, base);
            self.buffer.put_byte(offset as _);
        }
    }
    pub fn memory_modrm_disp32(&mut self, reg: i32, base: RegisterID, offset: i32) {
        if base == RegisterID::ESP || (self.x64 && base == RegisterID::R12) {
            self.put_modrm_sib(ModRmMode::Disp32, reg, base, Self::NO_IX, 0);
            self.buffer.put_byte(offset as _);
        } else {
            self.put_modrm(ModRmMode::Disp32, reg, base);
            self.buffer.put_byte(offset as _);
        }
    }

    pub fn memory_modrm_scaled(
        &mut self,
        reg: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
        offset: i32,
    ) {
        let c = if self.x64 {
            offset == 0 && base != Self::NO_BASE && base != Self::NO_BASE2
        } else {
            offset == 0 && base != Self::NO_BASE
        };
        if c {
            self.put_modrm_sib(ModRmMode::NoDisp, reg, base, index, scale);
        } else if offset as i8 as i32 == offset {
            self.put_modrm_sib(ModRmMode::Disp8, reg, base, index, scale);
            self.buffer.put_byte(offset as _);
        } else {
            self.put_modrm_sib(ModRmMode::Disp32, reg, base, index, scale);
            self.buffer.put_byte(offset as _);
        }
    }

    pub fn memory_modrm_addr(&mut self, reg: i32, addr: u32) {
        self.put_modrm(ModRmMode::NoDisp, reg, Self::NO_BASE);
        self.buffer.put_int(addr as _);
    }
}
