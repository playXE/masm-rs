#![allow(non_upper_case_globals)]

use super::buffer::{AssemblerBuffer, AssemblerLabel, LocalWriter};
use std::ops::{Deref, DerefMut};
pub struct X86Assembler {
    pub formatter: X86InstructionFormatter,
    pub index_of_last_watchpoint: usize,
    pub index_of_tail_of_last_watchpoint: usize,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
#[repr(u8)]
pub enum RoundingType {
    ToNearestWithTiesToEven = 0,
    TowardNegativeInfinity = 1,
    TowardInfinity = 2,
    TowardZero = 3,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
#[repr(u8)]
pub enum PackedCompareCondition {
    EqualAndOrdered = 0,
    LessThanAndOrdered = 1,
    LessThanOrEqualAndOrdered = 2,
    Unordered = 3,
    NotEqualOrUnordered = 4,
    NotLessThanOrUnordered = 5,
    NotLessThanOrEqualOrUnordered = 6,
    Ordered = 7,
    EqualOrUnordered = 8,
    NotGreaterThanOrEqualOrUnordered = 9,
    NotGreaterThanOrUnordered = 10,
    False = 11,
    NotEqualAndOrdered = 12,
    GreaterThanOrEqualAndOrdered = 13,
    GreaterThanAndOrdered = 14,
    True = 15,
}

impl X86Assembler {
    pub fn code_size(&self) -> usize {
        self.formatter.buffer.code_size()
    }

    pub fn buffer(&self) -> &AssemblerBuffer {
        &self.formatter.buffer
    }

    pub fn buffer_mut(&mut self) -> &mut AssemblerBuffer {
        &mut self.formatter.buffer
    }

    pub fn new() -> Self {
        Self {
            formatter: X86InstructionFormatter::new(),
            index_of_last_watchpoint: 0,
            index_of_tail_of_last_watchpoint: 0,
        }
    }

    pub const fn fpr_name(reg: u8) -> &'static str {
        match reg {
            0 => "xmm0",
            1 => "xmm1",
            2 => "xmm2",
            3 => "xmm3",
            4 => "xmm4",
            5 => "xmm5",
            6 => "xmm6",
            7 => "xmm7",
            8 => "xmm8",
            9 => "xmm9",
            10 => "xmm10",
            11 => "xmm11",
            12 => "xmm12",
            13 => "xmm13",
            14 => "xmm14",
            15 => "xmm15",
            _ => "<invalid>",
        }
    }

    pub const fn gpr_name(reg: u8) -> &'static str {
        match reg {
            0 => "rax",
            1 => "rcx",
            2 => "rdx",
            3 => "rbx",
            4 => "rsp",
            5 => "rbp",
            6 => "rsi",
            7 => "rdi",
            8 => "r8",
            9 => "r9",
            10 => "r10",
            11 => "r11",
            12 => "r12",
            13 => "r13",
            14 => "r14",
            15 => "r15",
            _ => "<invalid>",
        }
    }

    pub const fn spr_name(reg: u8) -> &'static str {
        match reg {
            0 => "eflags",
            1 => "rip",
            _ => "<invalid>",
        }
    }

    pub const fn first_fp_register() -> u8 {
        xmm0
    }

    pub const fn last_fp_register() -> u8 {
        xmm15
    }

    pub const fn number_of_fp_registers() -> usize {
        (Self::last_fp_register() - Self::first_fp_register() + 1) as usize
    }

    pub const fn first_sp_register() -> u8 {
        eip
    }

    pub const fn last_sp_register() -> u8 {
        eflags
    }

    pub const fn number_of_sp_registers() -> usize {
        (Self::last_sp_register() - Self::first_sp_register() + 1) as usize
    }

    pub const fn first_register() -> u8 {
        eax
    }

    pub const fn last_register() -> u8 {
        r15
    }

    pub const fn number_of_registers() -> usize {
        (Self::last_register() - Self::first_register() + 1) as usize
    }

    pub fn push_r(&mut self, reg: u8) {
        self.formatter.one_byte_op_r(OP_PUSH_EAX, reg);
    }

    pub fn pop_r(&mut self, reg: u8) {
        self.formatter.one_byte_op_r(OP_POP_EAX, reg);
    }

    pub fn push_i32(&mut self, imm: i32) {
        self.formatter.one_byte_op(OP_PUSH_Iz);
        self.formatter.immediate32(imm);
    }

    pub fn push_m(&mut self, offset: i32, base: u8) {
        self.formatter
            .one_byte_op_mem(OP_GROUP5_Ev, GROUP5_OP_PUSH, base, offset);
    }

    pub fn pop_m(&mut self, offset: i32, base: u8) {
        self.formatter
            .one_byte_op_mem(OP_GROUP1A_Ev, GROUP1A_OP_POP, base, offset);
    }

    pub fn addl_rr(&mut self, src: u8, dst: u8) {
        self.formatter.one_byte_op_rm(OP_ADD_EvGv, src, dst);
    }

    pub fn addl_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter
            .one_byte_op_mem(OP_ADD_GvEv, dst, base, offset);
    }

    pub fn addl_mr_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8, dst: u8) {
        self.formatter
            .one_byte_op_mem_scaled(OP_ADD_GvEv, dst, base, index, scale, offset);
    }

    pub fn addl_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter
            .one_byte_op_mem(OP_ADD_EvGv, src, base, offset);
    }

    pub fn addl_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter
            .one_byte_op_mem_scaled(OP_ADD_EvGv, src, base, index, scale, offset);
    }

    pub fn addb_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter
            .one_byte_op8_mem(OP_ADD_EbGb, src, base, offset);
    }

    pub fn addb_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter
            .one_byte_op8_mem_scaled(OP_ADD_EbGb, src, base, index, scale, offset);
    }

    pub fn addw_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.formatter
            .one_byte_op8_mem(OP_ADD_EvGv, src, base, offset);
    }

    pub fn addw_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.formatter
            .one_byte_op8_mem_scaled(OP_ADD_EvGv, src, base, index, scale, offset);
    }

    pub fn addl_ir(&mut self, imm: i32, dst: u8) {
        if can_sign_extend_8_32(imm) {
            self.formatter
                .one_byte_op_rm(OP_GROUP1_EvIb, GROUP1_OP_ADD, dst);
            self.formatter.immediate8(imm);
        } else {
            if dst == eax {
                self.formatter.one_byte_op(OP_ADD_EAXIv);
            } else {
                self.formatter
                    .one_byte_op_rm(OP_GROUP1_EvIz, GROUP1_OP_ADD, dst);
            }
            self.formatter.immediate32(imm);
        }
    }

    pub fn addl_im(&mut self, imm: i32, offset: i32, base: u8) {
        if can_sign_extend_8_32(imm) {
            self.formatter
                .one_byte_op_mem(OP_GROUP1_EvIb, GROUP1_OP_ADD, base, offset);
            self.formatter.immediate8(imm);
        } else {
            self.formatter
                .one_byte_op_mem(OP_GROUP1_EvIz, GROUP1_OP_ADD, base, offset);
            self.formatter.immediate32(imm);
        }
    }

    pub fn addl_im_scaled(&mut self, imm: i32, offset: i32, base: u8, index: u8, scale: u8) {
        if can_sign_extend_8_32(imm) {
            self.formatter.one_byte_op_mem_scaled(
                OP_GROUP1_EvIb,
                GROUP1_OP_ADD,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.immediate8(imm);
        } else {
            self.formatter.one_byte_op_mem_scaled(
                OP_GROUP1_EvIz,
                GROUP1_OP_ADD,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.immediate32(imm);
        }
    }

    pub fn addb_im(&mut self, imm: i32, offset: i32, base: u8) {
        self.formatter
            .one_byte_op8_mem(OP_GROUP1_EbIb, GROUP1_OP_ADD, base, offset);
        self.formatter.immediate8(imm);
    }

    pub fn addb_im_scaled(&mut self, imm: i32, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.one_byte_op8_mem_scaled(
            OP_GROUP1_EbIb,
            GROUP1_OP_ADD,
            base,
            index,
            scale,
            offset,
        );
        self.formatter.immediate8(imm);
    }

    pub fn addw_im(&mut self, imm: i32, offset: i32, base: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        if can_sign_extend_8_32(imm) {
            self.formatter
                .one_byte_op8_mem(OP_GROUP1_EvIb, GROUP1_OP_ADD, base, offset);
            self.formatter.immediate8(imm);
        } else {
            self.formatter
                .one_byte_op8_mem(OP_GROUP1_EvIz, GROUP1_OP_ADD, base, offset);
            self.formatter.immediate32(imm);
        }
    }

    pub fn addw_im_scaled(&mut self, imm: i32, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        if can_sign_extend_8_32(imm) {
            self.formatter.one_byte_op8_mem_scaled(
                OP_GROUP1_EvIb,
                GROUP1_OP_ADD,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.immediate8(imm);
        } else {
            self.formatter.one_byte_op8_mem_scaled(
                OP_GROUP1_EvIz,
                GROUP1_OP_ADD,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.immediate32(imm);
        }
    }

    pub fn addq_rr(&mut self, src: u8, dst: u8) {
        self.formatter.one_byte_op64_rm(OP_ADD_EvGv, src, dst);
    }

    pub fn addq_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter
            .one_byte_op64_mem(OP_ADD_GvEv, dst, base, offset);
    }

    pub fn addq_mr_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8, dst: u8) {
        self.formatter
            .one_byte_op64_mem_scaled(OP_ADD_GvEv, dst, base, index, scale, offset);
    }

    pub fn addq_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter
            .one_byte_op64_mem(OP_ADD_EvGv, src, base, offset);
    }

    pub fn addq_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter
            .one_byte_op64_mem_scaled(OP_ADD_EvGv, src, base, index, scale, offset);
    }

    pub fn addq_ir(&mut self, imm: i32, dst: u8) {
        if can_sign_extend_8_32(imm) {
            self.formatter
                .one_byte_op64_rm(OP_GROUP1_EvIb, GROUP1_OP_ADD, dst);
            self.formatter.immediate8(imm);
        } else {
            if dst == eax {
                self.formatter.one_byte_op(OP_ADD_EAXIv);
            } else {
                self.formatter
                    .one_byte_op64_rm(OP_GROUP1_EvIz, GROUP1_OP_ADD, dst);
            }

            self.formatter.immediate32(imm);
        }
    }

    pub fn addq_im(&mut self, imm: i32, offset: i32, base: u8) {
        if can_sign_extend_8_32(imm) {
            self.formatter
                .one_byte_op64_mem(OP_GROUP1_EvIb, GROUP1_OP_ADD, base, offset);
            self.formatter.immediate8(imm);
        } else {
            self.formatter
                .one_byte_op64_mem(OP_GROUP1_EvIz, GROUP1_OP_ADD, base, offset);
            self.formatter.immediate32(imm);
        }
    }

    pub fn addq_im_scaled(&mut self, imm: i32, offset: i32, base: u8, index: u8, scale: u8) {
        if can_sign_extend_8_32(imm) {
            self.formatter.one_byte_op64_mem_scaled(
                OP_GROUP1_EvIb,
                GROUP1_OP_ADD,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.immediate8(imm);
        } else {
            self.formatter.one_byte_op64_mem_scaled(
                OP_GROUP1_EvIz,
                GROUP1_OP_ADD,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.immediate32(imm);
        }
    }

    pub fn andl_rr(&mut self, src: u8, dst: u8) {
        self.formatter.one_byte_op8_rm(OP_AND_EvGv, src, dst);
    }

    pub fn andl_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter
            .one_byte_op8_mem(OP_AND_GvEv, dst, base, offset);
    }

    pub fn andl_mr_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8, dst: u8) {
        self.formatter
            .one_byte_op8_mem_scaled(OP_AND_GvEv, dst, base, index, scale, offset);
    }

    pub fn andw_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.andl_mr(offset, base, dst)
    }

    pub fn andw_mr_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8, dst: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.andl_mr_scaled(offset, base, index, scale, dst)
    }

    pub fn andl_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter
            .one_byte_op8_mem(OP_AND_EvGv, src, base, offset);
    }

    pub fn andl_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter
            .one_byte_op8_mem_scaled(OP_AND_EvGv, src, base, index, scale, offset);
    }

    pub fn andw_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.andl_rm(src, offset, base)
    }

    pub fn andw_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.andl_rm_scaled(src, offset, base, index, scale)
    }

    pub fn andb_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter
            .one_byte_op8_mem(OP_AND_EvGb, src, base, offset);
    }

    pub fn andb_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter
            .one_byte_op8_mem_scaled(OP_AND_EvGb, src, base, index, scale, offset);
    }

    pub fn andl_ir(&mut self, imm: i32, dst: u8) {
        if can_sign_extend_8_32(imm) {
            self.formatter
                .one_byte_op8_rm(OP_GROUP1_EvIb, GROUP1_OP_AND, dst);
            self.formatter.immediate8(imm);
        } else {
            self.formatter
                .one_byte_op8_rm(OP_GROUP1_EvIz, GROUP1_OP_AND, dst);
            self.formatter.immediate32(imm);
        }
    }

    pub fn andl_im(&mut self, imm: i32, offset: i32, base: u8) {
        if can_sign_extend_8_32(imm) {
            self.formatter
                .one_byte_op8_mem(OP_GROUP1_EvIb, GROUP1_OP_AND, base, offset);
            self.formatter.immediate8(imm);
        } else {
            self.formatter
                .one_byte_op8_mem(OP_GROUP1_EvIz, GROUP1_OP_AND, base, offset);
            self.formatter.immediate32(imm);
        }
    }

    pub fn andl_im_scaled(&mut self, imm: i32, offset: i32, base: u8, index: u8, scale: u8) {
        if can_sign_extend_8_32(imm) {
            self.formatter.one_byte_op8_mem_scaled(
                OP_GROUP1_EvIb,
                GROUP1_OP_AND,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.immediate8(imm);
        } else {
            self.formatter.one_byte_op8_mem_scaled(
                OP_GROUP1_EvIz,
                GROUP1_OP_AND,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.immediate32(imm);
        }
    }

    pub fn andw_im(&mut self, imm: i32, offset: i32, base: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        if can_sign_extend_8_32(imm) {
            self.formatter
                .one_byte_op8_mem(OP_GROUP1_EvIb, GROUP1_OP_AND, base, offset);
            self.formatter.immediate8(imm);
        } else {
            self.formatter
                .one_byte_op8_mem(OP_GROUP1_EvIz, GROUP1_OP_AND, base, offset);
            self.formatter.immediate16(imm);
        }
    }

    pub fn andb_im(&mut self, imm: i32, offset: i32, base: u8) {
        self.formatter
            .one_byte_op_mem(OP_GROUP1_EbIb, GROUP1_OP_AND, base, offset);
        self.formatter.immediate8(imm);
    }

    pub fn andb_im_scaled(&mut self, imm: i32, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.one_byte_op_mem_scaled(
            OP_GROUP1_EbIb,
            GROUP1_OP_AND,
            base,
            index,
            scale,
            offset,
        );
        self.formatter.immediate8(imm);
    }

    pub fn andq_rr(&mut self, src: u8, dst: u8) {
        self.formatter.one_byte_op64_rm(OP_AND_EvGv, src, dst);
    }

    pub fn andq_ir(&mut self, imm: i32, dst: u8) {
        if can_sign_extend_8_32(imm) {
            self.formatter
                .one_byte_op64_rm(OP_GROUP1_EvIb, GROUP1_OP_AND, dst);
            self.formatter.immediate8(imm);
        } else {
            self.formatter
                .one_byte_op64_rm(OP_GROUP1_EvIz, GROUP1_OP_AND, dst);
            self.formatter.immediate32(imm);
        }
    }

    pub fn andq_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter
            .one_byte_op64_mem(OP_AND_GvEv, dst, base, offset);
    }

    pub fn andq_mr_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8, dst: u8) {
        self.formatter
            .one_byte_op64_mem_scaled(OP_AND_GvEv, dst, base, index, scale, offset);
    }

    pub fn andq_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter
            .one_byte_op64_mem(OP_AND_EvGv, src, base, offset);
    }

    pub fn andq_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter
            .one_byte_op64_mem_scaled(OP_AND_EvGv, src, base, index, scale, offset);
    }

    pub fn andq_im(&mut self, imm: i32, offset: i32, base: u8) {
        if can_sign_extend_8_32(imm) {
            self.formatter
                .one_byte_op64_mem(OP_GROUP1_EvIb, GROUP1_OP_AND, base, offset);
            self.formatter.immediate8(imm);
        } else {
            self.formatter
                .one_byte_op64_mem(OP_GROUP1_EvIz, GROUP1_OP_AND, base, offset);
            self.formatter.immediate32(imm);
        }
    }

    pub fn andq_im_scaled(&mut self, imm: i32, offset: i32, base: u8, index: u8, scale: u8) {
        if can_sign_extend_8_32(imm) {
            self.formatter.one_byte_op64_mem_scaled(
                OP_GROUP1_EvIb,
                GROUP1_OP_AND,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.immediate8(imm);
        } else {
            self.formatter.one_byte_op64_mem_scaled(
                OP_GROUP1_EvIz,
                GROUP1_OP_AND,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.immediate32(imm);
        }
    }

    pub fn dec_r(&mut self, dst: u8) {
        self.formatter
            .one_byte_op64_rm(OP_GROUP5_Ev, GROUP1_OP_OR, dst);
    }

    pub fn decq_r(&mut self, dst: u8) {
        self.formatter
            .one_byte_op64_rm(OP_GROUP5_Ev, GROUP1_OP_OR, dst);
    }

    pub fn illegal_instruction(&mut self) {
        self.formatter.two_byte_op(OP2_UD2);
    }

    pub fn inc_r(&mut self, dst: u8) {
        self.formatter
            .one_byte_op_rm(OP_GROUP5_Ev, GROUP1_OP_ADD, dst);
    }

    pub fn incq_r(&mut self, dst: u8) {
        self.formatter
            .one_byte_op64_rm(OP_GROUP5_Ev, GROUP1_OP_ADD, dst);
    }

    pub fn incq_m(&mut self, offset: i32, base: u8) {
        self.formatter
            .one_byte_op64_mem(OP_GROUP5_Ev, GROUP1_OP_ADD, base, offset);
    }

    pub fn incq_m_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.one_byte_op64_mem_scaled(
            OP_GROUP5_Ev,
            GROUP1_OP_ADD,
            base,
            index,
            scale,
            offset,
        );
    }

    pub fn negl_r(&mut self, dst: u8) {
        self.formatter
            .one_byte_op_rm(OP_GROUP3_Ev, GROUP3_OP_NEG, dst);
    }

    pub fn negq_r(&mut self, dst: u8) {
        self.formatter
            .one_byte_op64_rm(OP_GROUP3_Ev, GROUP3_OP_NEG, dst);
    }

    pub fn negq_m(&mut self, offset: i32, base: u8) {
        self.formatter
            .one_byte_op64_mem(OP_GROUP3_Ev, GROUP3_OP_NEG, base, offset);
    }

    pub fn negq_m_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.one_byte_op64_mem_scaled(
            OP_GROUP3_Ev,
            GROUP3_OP_NEG,
            base,
            index,
            scale,
            offset,
        );
    }

    pub fn negl_m(&mut self, offset: i32, base: u8) {
        self.formatter
            .one_byte_op_mem(OP_GROUP3_Ev, GROUP3_OP_NEG, base, offset);
    }

    pub fn negl_m_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.one_byte_op_mem_scaled(
            OP_GROUP3_Ev,
            GROUP3_OP_NEG,
            base,
            index,
            scale,
            offset,
        );
    }

    pub fn negw_m(&mut self, offset: i32, base: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.formatter
            .one_byte_op_mem(OP_GROUP3_Ev, GROUP3_OP_NEG, base, offset);
    }

    pub fn negw_m_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.formatter.one_byte_op_mem_scaled(
            OP_GROUP3_Ev,
            GROUP3_OP_NEG,
            base,
            index,
            scale,
            offset,
        );
    }

    pub fn negb_m(&mut self, offset: i32, base: u8) {
        self.formatter
            .one_byte_op_mem(OP_GROUP3_Eb, GROUP3_OP_NEG, base, offset);
    }

    pub fn negb_m_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.one_byte_op_mem_scaled(
            OP_GROUP3_Eb,
            GROUP3_OP_NEG,
            base,
            index,
            scale,
            offset,
        );
    }

    pub fn notl_r(&mut self, dst: u8) {
        self.formatter
            .one_byte_op_rm(OP_GROUP3_Ev, GROUP3_OP_NOT, dst);
    }

    pub fn notl_m(&mut self, offset: i32, base: u8) {
        self.formatter
            .one_byte_op_mem(OP_GROUP3_Ev, GROUP3_OP_NOT, base, offset);
    }

    pub fn notl_m_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.one_byte_op_mem_scaled(
            OP_GROUP3_Ev,
            GROUP3_OP_NOT,
            base,
            index,
            scale,
            offset,
        );
    }

    pub fn notw_m(&mut self, offset: i32, base: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.notl_m(offset, base);
    }

    pub fn notw_m_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.notl_m_scaled(offset, base, index, scale);
    }

    pub fn notb_m(&mut self, offset: i32, base: u8) {
        self.formatter
            .one_byte_op_mem(OP_GROUP3_Eb, GROUP3_OP_NOT, base, offset);
    }

    pub fn notb_m_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.one_byte_op_mem_scaled(
            OP_GROUP3_Eb,
            GROUP3_OP_NOT,
            base,
            index,
            scale,
            offset,
        );
    }

    pub fn notq_r(&mut self, dst: u8) {
        self.formatter
            .one_byte_op64_rm(OP_GROUP3_Ev, GROUP3_OP_NOT, dst);
    }

    pub fn notq_m(&mut self, offset: i32, base: u8) {
        self.formatter
            .one_byte_op64_mem(OP_GROUP3_Ev, GROUP3_OP_NOT, base, offset);
    }

    pub fn notq_m_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.one_byte_op64_mem_scaled(
            OP_GROUP3_Ev,
            GROUP3_OP_NOT,
            base,
            index,
            scale,
            offset,
        );
    }

    pub fn orl_rr(&mut self, src: u8, dst: u8) {
        self.formatter.one_byte_op_rm(OP_OR_EvGv, src, dst);
    }

    pub fn orl_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter
            .one_byte_op_mem(OP_OR_GvEv, dst, base, offset);
    }

    pub fn orl_mr_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8, dst: u8) {
        self.formatter
            .one_byte_op_mem_scaled(OP_OR_GvEv, dst, base, index, scale, offset);
    }

    pub fn orl_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter
            .one_byte_op_mem(OP_OR_EvGv, src, base, offset);
    }

    pub fn orl_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter
            .one_byte_op_mem_scaled(OP_OR_EvGv, src, base, index, scale, offset);
    }

    pub fn orw_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.orl_rm(src, offset, base);
    }

    pub fn orw_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.orl_rm_scaled(src, offset, base, index, scale);
    }

    pub fn orb_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter
            .one_byte_op_mem(OP_OR_EvGb, src, base, offset);
    }

    pub fn orb_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter
            .one_byte_op_mem_scaled(OP_OR_EvGb, src, base, index, scale, offset);
    }

    pub fn orl_ir(&mut self, imm: i32, dst: u8) {
        if can_sign_extend_8_32(imm) {
            self.formatter
                .one_byte_op_rm(OP_GROUP1_EvIb, GROUP1_OP_OR, dst);
            self.formatter.immediate8(imm);
        } else {
            if dst == eax {
                self.formatter.one_byte_op(OP_OR_EAXIv);
            } else {
                self.formatter
                    .one_byte_op_rm(OP_GROUP1_EvIz, GROUP1_OP_OR, dst);
            }

            self.formatter.immediate32(imm);
        }
    }

    pub fn orl_im(&mut self, imm: i32, offset: i32, base: u8) {
        if can_sign_extend_8_32(imm) {
            self.formatter
                .one_byte_op_mem(OP_GROUP1_EvIb, GROUP1_OP_OR, base, offset);
            self.formatter.immediate8(imm);
        } else {
            self.formatter
                .one_byte_op_mem(OP_GROUP1_EvIz, GROUP1_OP_OR, base, offset);
            self.formatter.immediate32(imm);
        }
    }

    pub fn orl_im_scaled(&mut self, imm: i32, offset: i32, base: u8, index: u8, scale: u8) {
        if can_sign_extend_8_32(imm) {
            self.formatter.one_byte_op_mem_scaled(
                OP_GROUP1_EvIb,
                GROUP1_OP_OR,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.immediate8(imm);
        } else {
            self.formatter.one_byte_op_mem_scaled(
                OP_GROUP1_EvIz,
                GROUP1_OP_OR,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.immediate32(imm);
        }
    }

    pub fn orw_im(&mut self, imm: i32, offset: i32, base: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        if can_sign_extend_8_32(imm) {
            self.formatter
                .one_byte_op_mem(OP_GROUP1_EvIb, GROUP1_OP_OR, base, offset);
            self.formatter.immediate8(imm);
        } else {
            self.formatter
                .one_byte_op_mem(OP_GROUP1_EvIz, GROUP1_OP_OR, base, offset);
            self.formatter.immediate16(imm);
        }
    }

    pub fn orw_im_scaled(&mut self, imm: i32, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        if can_sign_extend_8_32(imm) {
            self.formatter.one_byte_op_mem_scaled(
                OP_GROUP1_EvIb,
                GROUP1_OP_OR,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.immediate8(imm);
        } else {
            self.formatter.one_byte_op_mem_scaled(
                OP_GROUP1_EvIz,
                GROUP1_OP_OR,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.immediate16(imm);
        }
    }

    pub fn orb_im(&mut self, imm: i32, offset: i32, base: u8) {
        self.formatter
            .one_byte_op_mem(OP_GROUP1_EbIb, GROUP1_OP_OR, base, offset);
        self.formatter.immediate8(imm);
    }

    pub fn orb_im_scaled(&mut self, imm: i32, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.one_byte_op_mem_scaled(
            OP_GROUP1_EbIb,
            GROUP1_OP_OR,
            base,
            index,
            scale,
            offset,
        );
        self.formatter.immediate8(imm);
    }

    pub fn orq_rr(&mut self, src: u8, dst: u8) {
        self.formatter.one_byte_op64_rm(OP_OR_EvGv, src, dst);
    }

    pub fn orq_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter
            .one_byte_op64_mem(OP_OR_GvEv, dst, base, offset);
    }

    pub fn orq_mr_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8, dst: u8) {
        self.formatter
            .one_byte_op64_mem_scaled(OP_OR_GvEv, dst, base, index, scale, offset);
    }

    pub fn orq_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter
            .one_byte_op64_mem(OP_OR_EvGv, src, base, offset);
    }

    pub fn orq_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter
            .one_byte_op64_mem_scaled(OP_OR_EvGv, src, base, index, scale, offset);
    }

    pub fn orq_im(&mut self, imm: i32, offset: i32, base: u8) {
        if can_sign_extend_8_32(imm) {
            self.formatter
                .one_byte_op64_mem(OP_GROUP1_EvIb, GROUP1_OP_OR, base, offset);
            self.formatter.immediate8(imm);
        } else {
            self.formatter
                .one_byte_op64_mem(OP_GROUP1_EvIz, GROUP1_OP_OR, base, offset);
            self.formatter.immediate32(imm);
        }
    }

    pub fn orq_im_scaled(&mut self, imm: i32, offset: i32, base: u8, index: u8, scale: u8) {
        if can_sign_extend_8_32(imm) {
            self.formatter.one_byte_op64_mem_scaled(
                OP_GROUP1_EvIb,
                GROUP1_OP_OR,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.immediate8(imm);
        } else {
            self.formatter.one_byte_op64_mem_scaled(
                OP_GROUP1_EvIz,
                GROUP1_OP_OR,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.immediate32(imm);
        }
    }

    pub fn orq_ir(&mut self, imm: i32, dst: u8) {
        if can_sign_extend_8_32(imm) {
            self.formatter
                .one_byte_op64_rm(OP_GROUP1_EvIb, GROUP1_OP_OR, dst);
            self.formatter.immediate8(imm);
        } else {
            if dst == eax {
                self.formatter.one_byte_op64(OP_OR_EAXIv);
            } else {
                self.formatter
                    .one_byte_op64_rm(OP_GROUP1_EvIz, GROUP1_OP_OR, dst);
            }

            self.formatter.immediate32(imm);
        }
    }

    pub fn subl_rr(&mut self, src: u8, dst: u8) {
        self.formatter.one_byte_op_rm(OP_SUB_EvGv, src, dst);
    }

    pub fn subl_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter
            .one_byte_op_mem(OP_SUB_GvEv, dst, base, offset);
    }

    pub fn subl_mr_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8, dst: u8) {
        self.formatter
            .one_byte_op_mem_scaled(OP_SUB_GvEv, dst, base, index, scale, offset);
    }

    pub fn subl_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter
            .one_byte_op_mem(OP_SUB_EvGv, src, base, offset);
    }

    pub fn subl_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter
            .one_byte_op_mem_scaled(OP_SUB_EvGv, src, base, index, scale, offset);
    }

    pub fn subw_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.formatter
            .one_byte_op_mem(OP_SUB_EvGv, src, base, offset);
    }

    pub fn subw_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.formatter
            .one_byte_op_mem_scaled(OP_SUB_EvGv, src, base, index, scale, offset);
    }

    pub fn subb_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.formatter
            .one_byte_op_mem(OP_SUB_EvGb, src, base, offset);
    }

    pub fn subb_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.formatter
            .one_byte_op_mem_scaled(OP_SUB_EvGb, src, base, index, scale, offset);
    }

    pub fn subl_ir(&mut self, imm: i32, dst: u8) {
        if can_sign_extend_8_32(imm) {
            self.formatter
                .two_byte_op_rm(OP_GROUP1_EvIb, GROUP1_OP_SUB, dst);
            self.formatter.immediate8(imm);
        } else {
            if dst == eax {
                self.formatter.one_byte_op(OP_SUB_EAXIv);
            } else {
                self.formatter
                    .two_byte_op_rm(OP_GROUP1_EvIz, GROUP1_OP_SUB, dst);
            }

            self.formatter.immediate32(imm);
        }
    }

    pub fn subl_im(&mut self, imm: i32, offset: i32, base: u8) {
        if can_sign_extend_8_32(imm) {
            self.formatter
                .one_byte_op_mem(OP_GROUP1_EvIb, GROUP1_OP_SUB, base, offset);
            self.formatter.immediate8(imm);
        } else {
            self.formatter
                .one_byte_op_mem(OP_GROUP1_EvIz, GROUP1_OP_SUB, base, offset);
            self.formatter.immediate32(imm);
        }
    }

    pub fn subl_im_scaled(&mut self, imm: i32, offset: i32, base: u8, index: u8, scale: u8) {
        if can_sign_extend_8_32(imm) {
            self.formatter.one_byte_op_mem_scaled(
                OP_GROUP1_EvIb,
                GROUP1_OP_SUB,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.immediate8(imm);
        } else {
            self.formatter.one_byte_op_mem_scaled(
                OP_GROUP1_EvIz,
                GROUP1_OP_SUB,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.immediate32(imm);
        }
    }

    pub fn subw_im(&mut self, imm: i32, offset: i32, base: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        if can_sign_extend_8_32(imm) {
            self.formatter
                .one_byte_op_mem(OP_GROUP1_EvIb, GROUP1_OP_SUB, base, offset);
            self.formatter.immediate8(imm);
        } else {
            self.formatter
                .one_byte_op_mem(OP_GROUP1_EvIz, GROUP1_OP_SUB, base, offset);
            self.formatter.immediate16(imm);
        }
    }

    pub fn subw_im_scaled(&mut self, imm: i32, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        if can_sign_extend_8_32(imm) {
            self.formatter.one_byte_op_mem_scaled(
                OP_GROUP1_EvIb,
                GROUP1_OP_SUB,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.immediate8(imm);
        } else {
            self.formatter.one_byte_op_mem_scaled(
                OP_GROUP1_EvIz,
                GROUP1_OP_SUB,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.immediate16(imm);
        }
    }

    pub fn subb_im(&mut self, imm: i32, offset: i32, base: u8) {
        self.formatter
            .one_byte_op_mem(OP_GROUP1_EbIb, GROUP1_OP_SUB, base, offset);
        self.formatter.immediate8(imm);
    }

    pub fn subb_im_scaled(&mut self, imm: i32, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.one_byte_op_mem_scaled(
            OP_GROUP1_EbIb,
            GROUP1_OP_SUB,
            base,
            index,
            scale,
            offset,
        );
        self.formatter.immediate8(imm);
    }

    pub fn subq_rr(&mut self, src: u8, dst: u8) {
        self.formatter.one_byte_op64_rm(OP_SUB_EvGv, src, dst);
    }

    pub fn subq_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter
            .one_byte_op64_mem(OP_SUB_GvEv, dst, base, offset);
    }

    pub fn subq_mr_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8, dst: u8) {
        self.formatter
            .one_byte_op64_mem_scaled(OP_SUB_GvEv, dst, base, index, scale, offset);
    }

    pub fn subq_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter
            .one_byte_op64_mem(OP_SUB_EvGv, src, base, offset);
    }

    pub fn subq_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter
            .one_byte_op64_mem_scaled(OP_SUB_EvGv, src, base, index, scale, offset);
    }

    pub fn subq_ir(&mut self, imm: i32, dst: u8) {
        if can_sign_extend_8_32(imm) {
            self.formatter
                .one_byte_op64_rm(OP_GROUP1_EvIb, GROUP1_OP_SUB, dst);
            self.formatter.immediate8(imm);
        } else {
            if dst == eax {
                self.formatter.one_byte_op64(OP_SUB_EAXIv);
            } else {
                self.formatter
                    .one_byte_op64_rm(OP_GROUP1_EvIz, GROUP1_OP_SUB, dst);
            }
            self.formatter.immediate32(imm);
        }
    }

    pub fn subq_im(&mut self, imm: i32, offset: i32, base: u8) {
        if can_sign_extend_8_32(imm) {
            self.formatter
                .two_byte_op64_mem(OP_GROUP1_EvIb, GROUP1_OP_SUB, base, offset);
            self.formatter.immediate8(imm);
        } else {
            self.formatter
                .two_byte_op64_mem(OP_GROUP1_EvIz, GROUP1_OP_SUB, base, offset);
            self.formatter.immediate32(imm);
        }
    }

    pub fn subq_im_scaled(&mut self, imm: i32, offset: i32, base: u8, index: u8, scale: u8) {
        if can_sign_extend_8_32(imm) {
            self.formatter.two_byte_op64_mem_scaled(
                OP_GROUP1_EvIb,
                GROUP1_OP_SUB,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.immediate8(imm);
        } else {
            self.formatter.two_byte_op64_mem_scaled(
                OP_GROUP1_EvIz,
                GROUP1_OP_SUB,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.immediate32(imm);
        }
    }

    pub fn xorl_rr(&mut self, src: u8, dst: u8) {
        self.formatter.one_byte_op_rm(OP_XOR_EvGv, src, dst);
    }

    pub fn xorl_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter
            .one_byte_op_mem(OP_XOR_GvEv, dst, base, offset);
    }

    pub fn xorl_mr_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8, dst: u8) {
        self.formatter
            .one_byte_op_mem_scaled(OP_XOR_GvEv, dst, base, index, scale, offset);
    }

    pub fn xorl_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter
            .one_byte_op_mem(OP_XOR_EvGv, src, base, offset);
    }

    pub fn xorl_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter
            .one_byte_op_mem_scaled(OP_XOR_EvGv, src, base, index, scale, offset);
    }

    pub fn xorl_im(&mut self, imm: i32, offset: i32, base: u8) {
        if can_sign_extend_8_32(imm) {
            self.formatter
                .one_byte_op_mem(OP_GROUP1_EvIb, GROUP1_OP_XOR, base, offset);
            self.formatter.immediate8(imm);
        } else {
            self.formatter
                .one_byte_op_mem(OP_GROUP1_EvIz, GROUP1_OP_XOR, base, offset);
            self.formatter.immediate32(imm);
        }
    }

    pub fn xorl_im_scaled(&mut self, imm: i32, offset: i32, base: u8, index: u8, scale: u8) {
        if can_sign_extend_8_32(imm) {
            self.formatter.one_byte_op_mem_scaled(
                OP_GROUP1_EvIb,
                GROUP1_OP_XOR,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.immediate8(imm);
        } else {
            self.formatter.one_byte_op_mem_scaled(
                OP_GROUP1_EvIz,
                GROUP1_OP_XOR,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.immediate32(imm);
        }
    }

    pub fn xorw_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.xorl_rm(src, offset, base)
    }

    pub fn xorw_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.xorl_rm_scaled(src, offset, base, index, scale)
    }

    pub fn xorw_im(&mut self, imm: i32, offset: i32, base: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        if can_sign_extend_8_32(imm) {
            self.formatter
                .one_byte_op_mem(OP_GROUP1_EvIb, GROUP1_OP_XOR, base, offset);
            self.formatter.immediate8(imm);
        } else {
            self.formatter
                .one_byte_op_mem(OP_GROUP1_EvIz, GROUP1_OP_XOR, base, offset);
            self.formatter.immediate16(imm);
        }
    }

    pub fn xorb_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter
            .one_byte_op_mem(OP_XOR_EvGb, src, base, offset);
    }

    pub fn xorb_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter
            .one_byte_op_mem_scaled(OP_XOR_EvGb, src, base, index, scale, offset);
    }

    pub fn xorb_im(&mut self, imm: i32, offset: i32, base: u8) {
        self.formatter
            .one_byte_op_mem(OP_GROUP1_EbIb, GROUP1_OP_XOR, base, offset);
        self.formatter.immediate8(imm);
    }

    pub fn xorb_im_scaled(&mut self, imm: i32, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.one_byte_op_mem_scaled(
            OP_GROUP1_EbIb,
            GROUP1_OP_XOR,
            base,
            index,
            scale,
            offset,
        );
        self.formatter.immediate8(imm);
    }

    pub fn xorl_ir(&mut self, imm: i32, dst: u8) {
        if can_sign_extend_8_32(imm) {
            self.formatter
                .one_byte_op_rm(OP_GROUP1_EvIb, GROUP1_OP_XOR, dst);
            self.formatter.immediate8(imm);
        } else {
            self.formatter
                .one_byte_op_rm(OP_GROUP1_EvIz, GROUP1_OP_XOR, dst);
            self.formatter.immediate32(imm);
        }
    }

    pub fn xorq_rr(&mut self, src: u8, dst: u8) {
        self.formatter.two_byte_op64_rm(OP_XOR_EvGv, src, dst);
    }

    pub fn xorq_ir(&mut self, imm: i32, dst: u8) {
        if can_sign_extend_8_32(imm) {
            self.formatter
                .one_byte_op64_rm(OP_GROUP1_EvIb, GROUP1_OP_XOR, dst);
            self.formatter.immediate8(imm);
        } else {
            self.formatter
                .one_byte_op64_rm(OP_GROUP1_EvIz, GROUP1_OP_XOR, dst);
            self.formatter.immediate32(imm);
        }
    }

    pub fn xorq_im(&mut self, imm: i32, offset: i32, base: u8) {
        if can_sign_extend_8_32(imm) {
            self.formatter
                .one_byte_op64_mem(OP_GROUP1_EvIb, GROUP1_OP_XOR, base, offset);
            self.formatter.immediate8(imm);
        } else {
            self.formatter
                .one_byte_op64_mem(OP_GROUP1_EvIz, GROUP1_OP_XOR, base, offset);
            self.formatter.immediate32(imm);
        }
    }

    pub fn xorq_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter
            .one_byte_op64_mem(OP_XOR_EvGv, src, base, offset);
    }

    pub fn xorq_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter
            .one_byte_op64_mem_scaled(OP_XOR_EvGv, src, base, index, scale, offset);
    }

    pub fn xorq_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter
            .one_byte_op64_mem(OP_XOR_GvEv, dst, base, offset);
    }

    pub fn xorq_mr_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8, dst: u8) {
        self.formatter
            .one_byte_op64_mem_scaled(OP_XOR_GvEv, dst, base, index, scale, offset);
    }

    pub fn lzcnt_rr(&mut self, src: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter.two_byte_op_rm(OP2_LZCNT, dst, src);
    }

    pub fn lzcnt_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter.two_byte_op_mem(OP2_LZCNT, dst, base, offset);
    }

    pub fn lzcntq_rr(&mut self, src: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter.two_byte_op64_rm(OP2_LZCNT, dst, src);
    }

    pub fn lzcntq_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op64_mem(OP2_LZCNT, dst, base, offset);
    }

    pub fn bsr_rr(&mut self, src: u8, dst: u8) {
        self.formatter.two_byte_op_rm(OP2_BSR, dst, src);
    }

    pub fn bsr_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter.two_byte_op_mem(OP2_BSR, dst, base, offset);
    }

    pub fn bsrq_rr(&mut self, src: u8, dst: u8) {
        self.formatter.two_byte_op64_rm(OP2_BSR, dst, src);
    }

    pub fn bsrq_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter.two_byte_op64_mem(OP2_BSR, dst, base, offset);
    }

    pub fn bswapl_r(&mut self, dst: u8) {
        self.formatter.two_byte_op_r(OP2_BSWAP, dst);
    }

    pub fn bswapq_r(&mut self, dst: u8) {
        self.formatter.two_byte_op64_r(OP2_BSWAP, dst);
    }

    pub fn tzcnt_rr(&mut self, src: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter.two_byte_op_rm(OP2_TZCNT, dst, src);
    }

    pub fn tzcntq_rr(&mut self, src: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter.two_byte_op64_rm(OP2_TZCNT, dst, src);
    }

    pub fn bsf_rr(&mut self, src: u8, dst: u8) {
        self.formatter.two_byte_op_rm(OP2_BSF, dst, src);
    }

    pub fn bsfq_rr(&mut self, src: u8, dst: u8) {
        self.formatter.two_byte_op64_rm(OP2_BSF, dst, src);
    }

    pub fn btrq_rr(&mut self, src: u8, dst: u8) {
        self.formatter.two_byte_op64_rm(OP2_BTR, dst, src);
    }

    pub fn popcnt_rr(&mut self, src: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter.two_byte_op_rm(OP2_POPCNT, dst, src);
    }

    pub fn popcnt_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op_mem(OP2_POPCNT, dst, base, offset);
    }

    pub fn popcntq_rr(&mut self, src: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter.two_byte_op64_rm(OP2_POPCNT, dst, src);
    }

    pub fn popcntq_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op64_mem(OP2_POPCNT, dst, base, offset);
    }

    fn shift_instruction32<const OP: u8>(&mut self, imm: i32, dst: u8) {
        if imm == 1 {
            self.formatter.one_byte_op_rm(OP_GROUP2_Ev1, OP, dst);
        } else {
            self.formatter.one_byte_op_rm(OP_GROUP2_EvIb, OP, dst);
            self.formatter.immediate8(imm);
        }
    }

    fn shift_instruction16<const OP: u8>(&mut self, imm: i32, dst: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        if imm == 1 {
            self.formatter.one_byte_op_rm(OP_GROUP2_Ev1, OP, dst);
        } else {
            self.formatter.one_byte_op_rm(OP_GROUP2_EvIb, OP, dst);
            self.formatter.immediate8(imm);
        }
    }

    pub fn sarl_i8r(&mut self, imm: i32, dst: u8) {
        self.shift_instruction32::<GROUP2_OP_SAR>(imm, dst);
    }

    pub fn sarl_clr(&mut self, dst: u8) {
        self.formatter
            .one_byte_op_rm(OP_GROUP2_EvCL, GROUP2_OP_SAR, dst);
    }

    pub fn shrl_i8r(&mut self, imm: i32, dst: u8) {
        self.shift_instruction32::<GROUP2_OP_SHR>(imm, dst);
    }

    pub fn shrl_clr(&mut self, dst: u8) {
        self.formatter
            .one_byte_op_rm(OP_GROUP2_EvCL, GROUP2_OP_SHR, dst);
    }

    pub fn shll_i8r(&mut self, imm: i32, dst: u8) {
        self.shift_instruction32::<GROUP2_OP_SHL>(imm, dst);
    }

    pub fn shll_clr(&mut self, dst: u8) {
        self.formatter
            .one_byte_op_rm(OP_GROUP2_EvCL, GROUP2_OP_SHL, dst);
    }

    pub fn rorl_i8r(&mut self, imm: i32, dst: u8) {
        self.shift_instruction32::<GROUP2_OP_ROR>(imm, dst);
    }

    pub fn rorl_clr(&mut self, dst: u8) {
        self.formatter
            .one_byte_op_rm(OP_GROUP2_EvCL, GROUP2_OP_ROR, dst);
    }

    pub fn roll_i8r(&mut self, imm: i32, dst: u8) {
        self.shift_instruction32::<GROUP2_OP_ROL>(imm, dst);
    }

    pub fn roll_clr(&mut self, dst: u8) {
        self.formatter
            .one_byte_op_rm(OP_GROUP2_EvCL, GROUP2_OP_ROL, dst);
    }

    pub fn rolw_i8r(&mut self, imm: i32, dst: u8) {
        self.shift_instruction16::<GROUP2_OP_ROR>(imm, dst);
    }

    fn shift_instruction64<const OP: u8>(&mut self, imm: i32, dst: u8) {
        if imm == 1 {
            self.formatter.one_byte_op64_rm(OP_GROUP2_Ev1, OP, dst);
        } else {
            self.formatter.one_byte_op64_rm(OP_GROUP2_EvIb, OP, dst);
            self.formatter.immediate8(imm);
        }
    }

    pub fn sarq_i8r(&mut self, imm: i32, dst: u8) {
        self.shift_instruction64::<GROUP2_OP_SAR>(imm, dst);
    }

    pub fn sarq_clr(&mut self, dst: u8) {
        self.formatter
            .one_byte_op64_rm(OP_GROUP2_EvCL, GROUP2_OP_SAR, dst);
    }

    pub fn shrq_i8r(&mut self, imm: i32, dst: u8) {
        self.shift_instruction64::<GROUP2_OP_SHR>(imm, dst);
    }

    pub fn shrq_clr(&mut self, dst: u8) {
        self.formatter
            .one_byte_op64_rm(OP_GROUP2_EvCL, GROUP2_OP_SHR, dst);
    }

    pub fn shlq_i8r(&mut self, imm: i32, dst: u8) {
        self.shift_instruction64::<GROUP2_OP_SHL>(imm, dst);
    }

    pub fn shlq_clr(&mut self, dst: u8) {
        self.formatter
            .one_byte_op64_rm(OP_GROUP2_EvCL, GROUP2_OP_SHL, dst);
    }

    pub fn rorq_i8r(&mut self, imm: i32, dst: u8) {
        self.shift_instruction64::<GROUP2_OP_ROR>(imm, dst);
    }

    pub fn rorq_clr(&mut self, dst: u8) {
        self.formatter
            .one_byte_op64_rm(OP_GROUP2_EvCL, GROUP2_OP_ROR, dst);
    }

    pub fn rolq_i8r(&mut self, imm: i32, dst: u8) {
        self.shift_instruction64::<GROUP2_OP_ROL>(imm, dst);
    }

    pub fn rolq_clr(&mut self, dst: u8) {
        self.formatter
            .one_byte_op64_rm(OP_GROUP2_EvCL, GROUP2_OP_ROL, dst);
    }

    pub fn imull_rr(&mut self, src: u8, dst: u8) {
        self.formatter.two_byte_op_rm(OP2_IMUL_GvEv, dst, src);
    }

    pub fn imulq_rr(&mut self, src: u8, dst: u8) {
        self.formatter.two_byte_op64_rm(OP2_IMUL_GvEv, dst, src);
    }

    pub fn imull_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter
            .two_byte_op_mem(OP2_IMUL_GvEv, dst, base, offset)
    }

    pub fn imull_i32r(&mut self, src: u8, imm: i32, dst: u8) {
        self.formatter.one_byte_op_rm(OP_IMUL_GvEvIz, dst, src);
        self.formatter.immediate32(imm);
    }

    pub fn divl_r(&mut self, dst: u8) {
        self.formatter
            .one_byte_op_rm(OP_GROUP3_Ev, GROUP3_OP_DIV, dst);
    }

    pub fn idivl_r(&mut self, dst: u8) {
        self.formatter
            .one_byte_op_rm(OP_GROUP3_Ev, GROUP3_OP_IDIV, dst);
    }

    pub fn idivq_r(&mut self, dst: u8) {
        self.formatter
            .one_byte_op64_rm(OP_GROUP3_Ev, GROUP3_OP_IDIV, dst);
    }

    pub fn divq_r(&mut self, dst: u8) {
        self.formatter
            .one_byte_op64_rm(OP_GROUP3_Ev, GROUP3_OP_DIV, dst);
    }

    pub fn cmpl_rr(&mut self, src: u8, dst: u8) {
        self.formatter.one_byte_op_rm(OP_CMP_EvGv, dst, src);
    }

    pub fn cmpl_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter
            .one_byte_op_mem(OP_CMP_EvGv, src, base, offset)
    }

    pub fn cmpl_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter
            .one_byte_op_mem(OP_CMP_GvEv, dst, base, offset)
    }

    pub fn cmpl_ir(&mut self, imm: i32, dst: u8) {
        if can_sign_extend_8_32(imm) {
            self.formatter
                .one_byte_op_rm(OP_GROUP1_EvIb, GROUP1_OP_CMP, dst);
            self.formatter.immediate8(imm);
        } else {
            if dst == eax {
                self.formatter.one_byte_op(OP_CMP_EAXIv);
            } else {
                self.formatter
                    .one_byte_op_rm(OP_GROUP1_EvIz, GROUP1_OP_CMP, dst);
            }

            self.formatter.immediate32(imm);
        }
    }

    pub fn cmpl_ir_force32(&mut self, imm: i32, dst: u8) {
        self.formatter
            .one_byte_op_rm(OP_GROUP1_EvIz, GROUP1_OP_CMP, dst);
        self.formatter.immediate32(imm);
    }

    pub fn cmpl_im(&mut self, imm: i32, offset: i32, base: u8) {
        if can_sign_extend_8_32(imm) {
            self.formatter
                .one_byte_op_mem(OP_GROUP1_EvIb, GROUP1_OP_CMP, base, offset);
            self.formatter.immediate8(imm);
        } else {
            self.formatter
                .one_byte_op_mem(OP_GROUP1_EvIz, GROUP1_OP_CMP, base, offset);
            self.formatter.immediate32(imm);
        }
    }

    pub fn cmpb_im(&mut self, imm: i32, offset: i32, base: u8) {
        self.formatter
            .one_byte_op_mem(OP_GROUP1_EbIb, GROUP1_OP_CMP, base, offset);
        self.formatter.immediate8(imm);
    }

    pub fn cmpb_im_scaled(&mut self, imm: i32, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.one_byte_op_mem_scaled(
            OP_GROUP1_EbIb,
            GROUP1_OP_CMP,
            base,
            index,
            scale,
            offset,
        );
        self.formatter.immediate8(imm);
    }

    pub fn cmpl_im_scaled(&mut self, imm: i32, offset: i32, base: u8, index: u8, scale: u8) {
        if can_sign_extend_8_32(imm) {
            self.formatter.one_byte_op_mem_scaled(
                OP_GROUP1_EvIb,
                GROUP1_OP_CMP,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.immediate8(imm);
        } else {
            self.formatter.one_byte_op_mem_scaled(
                OP_GROUP1_EvIz,
                GROUP1_OP_CMP,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.immediate32(imm);
        }
    }

    pub fn cmpl_im_force32(&mut self, imm: i32, offset: i32, base: u8) {
        self.formatter
            .one_byte_op_mem(OP_GROUP1_EvIz, GROUP1_OP_CMP, base, offset);
        self.formatter.immediate32(imm);
    }

    pub fn cmpq_rr(&mut self, src: u8, dst: u8) {
        self.formatter.one_byte_op64_rm(OP_CMP_EvGv, dst, src);
    }

    pub fn cmpq_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter
            .one_byte_op64_mem(OP_CMP_EvGv, src, base, offset)
    }

    pub fn cmpq_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter
            .one_byte_op64_mem(OP_CMP_GvEv, dst, base, offset)
    }

    pub fn cmpq_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter
            .one_byte_op64_mem_scaled(OP_CMP_EvGv, src, base, index, scale, offset)
    }

    pub fn cmpq_ir(&mut self, imm: i32, dst: u8) {
        if can_sign_extend_8_32(imm) {
            self.formatter
                .one_byte_op64_rm(OP_GROUP1_EvIb, GROUP1_OP_CMP, dst);
            self.formatter.immediate8(imm);
        } else {
            if dst == eax {
                self.formatter.one_byte_op64(OP_CMP_EAXIv);
            } else {
                self.formatter
                    .one_byte_op64_rm(OP_GROUP1_EvIz, GROUP1_OP_CMP, dst);
            }

            self.formatter.immediate32(imm);
        }
    }

    pub fn cmpq_im(&mut self, imm: i32, offset: i32, base: u8) {
        if can_sign_extend_8_32(imm) {
            self.formatter
                .one_byte_op64_mem(OP_GROUP1_EvIb, GROUP1_OP_CMP, base, offset);
            self.formatter.immediate8(imm);
        } else {
            self.formatter
                .one_byte_op64_mem(OP_GROUP1_EvIz, GROUP1_OP_CMP, base, offset);
            self.formatter.immediate32(imm);
        }
    }

    pub fn cmpq_im_scaled(&mut self, imm: i32, offset: i32, base: u8, index: u8, scale: u8) {
        if can_sign_extend_8_32(imm) {
            self.formatter.one_byte_op64_mem_scaled(
                OP_GROUP1_EvIb,
                GROUP1_OP_CMP,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.immediate8(imm);
        } else {
            self.formatter.one_byte_op64_mem_scaled(
                OP_GROUP1_EvIz,
                GROUP1_OP_CMP,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.immediate32(imm);
        }
    }

    pub fn cmpw_ir(&mut self, imm: i32, dst: u8) {
        if can_sign_extend_8_32(imm) {
            self.formatter.prefix(PRE_OPERAND_SIZE);
            self.formatter
                .one_byte_op_rm(OP_GROUP1_EvIb, GROUP1_OP_CMP, dst);
            self.formatter.immediate8(imm);
        } else {
            self.formatter.prefix(PRE_OPERAND_SIZE);
            self.formatter
                .one_byte_op_rm(OP_GROUP1_EvIz, GROUP1_OP_CMP, dst);
            self.formatter.immediate16(imm);
        }
    }

    pub fn cmpw_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.formatter
            .one_byte_op_mem_scaled(OP_CMP_EvGv, src, base, index, scale, offset)
    }

    pub fn cmpw_im(&mut self, imm: i32, offset: i32, base: u8) {
        if can_sign_extend_8_32(imm) {
            self.formatter.prefix(PRE_OPERAND_SIZE);
            self.formatter
                .one_byte_op_mem(OP_GROUP1_EvIb, GROUP1_OP_CMP, base, offset);
            self.formatter.immediate8(imm);
        } else {
            self.formatter.prefix(PRE_OPERAND_SIZE);
            self.formatter
                .one_byte_op_mem(OP_GROUP1_EvIz, GROUP1_OP_CMP, base, offset);
            self.formatter.immediate16(imm);
        }
    }

    pub fn cmpw_im_scaled(&mut self, imm: i32, offset: i32, base: u8, index: u8, scale: u8) {
        if can_sign_extend_8_32(imm) {
            self.formatter.prefix(PRE_OPERAND_SIZE);
            self.formatter.one_byte_op_mem_scaled(
                OP_GROUP1_EvIb,
                GROUP1_OP_CMP,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.immediate8(imm);
        } else {
            self.formatter.prefix(PRE_OPERAND_SIZE);
            self.formatter.one_byte_op_mem_scaled(
                OP_GROUP1_EvIz,
                GROUP1_OP_CMP,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.immediate16(imm);
        }
    }

    pub fn testl_rr(&mut self, src: u8, dst: u8) {
        self.formatter.one_byte_op_rm(OP_TEST_EvGv, src, dst);
    }

    pub fn testl_i32r(&mut self, imm: i32, dst: u8) {
        if dst == eax {
            self.formatter.one_byte_op(OP_TEST_EAXIv);
        } else {
            self.formatter
                .one_byte_op_rm(OP_GROUP3_EvIz, GROUP3_OP_TEST, dst);
        }

        self.formatter.immediate32(imm);
    }

    pub fn testl_i32m(&mut self, imm: i32, offset: i32, base: u8) {
        self.formatter
            .one_byte_op_mem(OP_GROUP3_EvIz, GROUP3_OP_TEST, base, offset);
        self.formatter.immediate32(imm);
    }

    pub fn testb_rr(&mut self, src: u8, dst: u8) {
        self.formatter.one_byte_op_rm(OP_TEST_EbGb, src, dst);
    }

    pub fn testb_im(&mut self, imm: i32, offset: i32, base: u8) {
        self.formatter
            .one_byte_op_mem(OP_GROUP3_EbIb, GROUP3_OP_TEST, base, offset);
        self.formatter.immediate8(imm);
    }

    pub fn testb_im_scaled(&mut self, imm: i32, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.one_byte_op_mem_scaled(
            OP_GROUP3_EbIb,
            GROUP3_OP_TEST,
            base,
            index,
            scale,
            offset,
        );
        self.formatter.immediate8(imm);
    }

    pub fn testl_i32m_scaled(&mut self, imm: i32, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.one_byte_op_mem_scaled(
            OP_GROUP3_EvIz,
            GROUP3_OP_TEST,
            base,
            index,
            scale,
            offset,
        );
        self.formatter.immediate32(imm);
    }

    pub fn testq_rr(&mut self, src: u8, dst: u8) {
        self.formatter.one_byte_op64_rm(OP_TEST_EvGv, src, dst);
    }

    pub fn testq_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter
            .one_byte_op64_mem(OP_TEST_EvGv, src, base, offset);
    }

    pub fn testq_i32r(&mut self, imm: i32, dst: u8) {
        if dst == eax {
            self.formatter.one_byte_op64(OP_TEST_EAXIv);
        } else {
            self.formatter
                .one_byte_op64_rm(OP_GROUP3_EvIz, GROUP3_OP_TEST, dst);
        }

        self.formatter.immediate32(imm);
    }

    pub fn testq_i32m(&mut self, imm: i32, offset: i32, base: u8) {
        self.formatter
            .one_byte_op64_mem(OP_GROUP3_EvIz, GROUP3_OP_TEST, base, offset);
        self.formatter.immediate32(imm);
    }

    pub fn testq_i32m_scaled(&mut self, imm: i32, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.one_byte_op64_mem_scaled(
            OP_GROUP3_EvIz,
            GROUP3_OP_TEST,
            base,
            index,
            scale,
            offset,
        );
        self.formatter.immediate32(imm);
    }

    pub fn testw_rr(&mut self, src: u8, dst: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.formatter.one_byte_op_rm(OP_TEST_EvGv, src, dst);
    }

    pub fn testw_im(&mut self, imm: i32, offset: i32, base: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.formatter
            .one_byte_op_mem(OP_GROUP3_EvIz, GROUP3_OP_TEST, base, offset);
        self.formatter.immediate16(imm);
    }

    pub fn testw_im_scaled(&mut self, imm: i32, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.formatter.one_byte_op_mem_scaled(
            OP_GROUP3_EvIz,
            GROUP3_OP_TEST,
            base,
            index,
            scale,
            offset,
        );
        self.formatter.immediate16(imm);
    }

    pub fn testb_i8r(&mut self, imm: i32, dst: u8) {
        if dst == eax {
            self.formatter.one_byte_op(OP_TEST_EAXIv);
        } else {
            self.formatter
                .one_byte_op_rm(OP_GROUP3_EbIb, GROUP3_OP_TEST, dst);
        }

        self.formatter.immediate8(imm);
    }

    pub fn bt_ir(&mut self, bit_offset: i32, test_value: u8) {
        self.formatter
            .two_byte_op_rm(OP2_GROUP_BT_EvIb, GROUP_BT_OP_BT, test_value);
        self.formatter.immediate8(bit_offset);
    }

    pub fn bt_im(&mut self, bit_offset: i32, offset: i32, base: u8) {
        self.formatter
            .two_byte_op_mem(OP2_GROUP_BT_EvIb, GROUP_BT_OP_BT, base, offset);
        self.formatter.immediate8(bit_offset);
    }

    pub fn bt_rr(&mut self, bit_offset: u8, test_value: u8) {
        self.formatter
            .two_byte_op_rm(OP2_BT_EvEv, bit_offset, test_value);
    }

    pub fn bit_rm(&mut self, bit_offset: u8, offset: i32, base: u8) {
        self.formatter
            .two_byte_op_mem(OP2_BT_EvEv, bit_offset, base, offset);
    }

    pub fn btw_ir(&mut self, bit_offset: i32, test_value: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.formatter
            .two_byte_op64_rm(OP2_GROUP_BT_EvIb, GROUP_BT_OP_BT, test_value);
        self.formatter.immediate8(bit_offset);
    }

    pub fn btw_im(&mut self, bit_offset: i32, offset: i32, base: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.formatter
            .two_byte_op64_mem(OP2_GROUP_BT_EvIb, GROUP_BT_OP_BT, base, offset);
        self.formatter.immediate8(bit_offset);
    }

    pub fn btw_rr(&mut self, bit_offset: u8, test_value: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.formatter
            .two_byte_op64_rm(OP2_BT_EvEv, bit_offset, test_value);
    }

    pub fn btw_rm(&mut self, bit_offset: u8, offset: i32, base: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.formatter
            .two_byte_op64_mem(OP2_BT_EvEv, bit_offset, base, offset);
    }

    pub fn setcc_r(&mut self, cc: Condition, dst: u8) {
        self.formatter.two_byte_op8_rm(setcc_opcode(cc), 0, dst);
    }

    pub fn sete_r(&mut self, dst: u8) {
        self.formatter
            .two_byte_op8_rm(setcc_opcode(Condition::E), 0, dst);
    }

    pub fn setz_r(&mut self, dst: u8) {
        self.sete_r(dst);
    }

    pub fn setne_r(&mut self, dst: u8) {
        self.formatter
            .two_byte_op8_rm(setcc_opcode(Condition::NE), 0, dst);
    }

    pub fn setnz_r(&mut self, dst: u8) {
        self.setne_r(dst);
    }

    pub fn setnp_r(&mut self, dst: u8) {
        self.formatter
            .two_byte_op8_rm(setcc_opcode(Condition::NP), 0, dst);
    }

    pub fn setp_r(&mut self, dst: u8) {
        self.formatter
            .two_byte_op8_rm(setcc_opcode(Condition::P), 0, dst);
    }

    // various mov ops:

    pub fn cdq(&mut self) {
        self.formatter.one_byte_op(OP_CDQ);
    }

    pub fn cqo(&mut self) {
        self.formatter.one_byte_op64(OP_CDQ);
    }

    pub fn fstps(&mut self, offset: i32, base: u8) {
        self.formatter
            .one_byte_op_mem(OP_ESCAPE_D9, ESCAPE_D9_FSTP_singleReal, base, offset);
    }

    pub fn fstpl(&mut self, offset: i32, base: u8) {
        self.formatter
            .one_byte_op_mem(OP_ESCAPE_DD, ESCAPE_DD_FSTP_doubleReal, base, offset);
    }

    pub fn xchgl_rr(&mut self, src: u8, dst: u8) {
        if src == eax {
            self.formatter.one_byte_op_r(OP_XCHG_EAX, dst);
        } else if dst == eax {
            self.formatter.one_byte_op_r(OP_XCHG_EAX, src);
        } else {
            self.formatter.one_byte_op_rm(OP_XCHG_EvGv, src, dst);
        }
    }

    pub fn xchgb_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter
            .one_byte_op_mem(OP_XCHG_EvGb, src, base, offset);
    }

    pub fn xchgb_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter
            .one_byte_op_mem_scaled(OP_XCHG_EvGb, src, base, index, scale, offset);
    }

    pub fn xchgw_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.formatter
            .one_byte_op_mem(OP_XCHG_EvGv, src, base, offset);
    }

    pub fn xchgw_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.formatter
            .one_byte_op_mem_scaled(OP_XCHG_EvGv, src, base, index, scale, offset);
    }

    pub fn xchgl_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter
            .one_byte_op_mem(OP_XCHG_EvGv, src, base, offset);
    }

    pub fn xchgl_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter
            .one_byte_op_mem_scaled(OP_XCHG_EvGv, src, base, index, scale, offset);
    }

    pub fn xchgq_rr(&mut self, src: u8, dst: u8) {
        if src == eax {
            self.formatter.one_byte_op64_r(OP_XCHG_EAX, dst);
        } else if dst == eax {
            self.formatter.one_byte_op64_r(OP_XCHG_EAX, src);
        } else {
            self.formatter.one_byte_op64_rm(OP_XCHG_EvGv, src, dst);
        }
    }

    pub fn xchgq_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter
            .one_byte_op64_mem(OP_XCHG_EvGv, src, base, offset);
    }

    pub fn xchgq_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter
            .one_byte_op64_mem_scaled(OP_XCHG_EvGv, src, base, index, scale, offset);
    }

    pub fn pinsrb_i8r(&mut self, lane_index: u8, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .three_byte_op_rm(OP2_3BYTE_ESCAPE_3A, OP3_PINSRB_VdqRdqpIb, vd, rn);
        self.formatter.immediate8(lane_index as _)
    }

    pub fn pinsrw_i8r(&mut self, lane_index: u8, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter.two_byte_op_rm(OP2_PINSRW_VdqRdqp, vd, rn);
        self.formatter.immediate8(lane_index as _)
    }

    pub fn pinsrd_i8rr(&mut self, lane_index: u8, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .three_byte_op_rm(OP2_3BYTE_ESCAPE_3A, OP3_PINSRQ_VdqEqbIb, vd, rn);
        self.formatter.immediate8(lane_index as _)
    }

    pub fn pinsrq_i8rr(&mut self, lane_index: u8, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .three_byte_op_rm(OP2_3BYTE_ESCAPE_3A, OP3_PINSRQ_VdqEqbIb, vd, rn);
        self.formatter.immediate8(lane_index as _)
    }

    pub fn insertps_i8rr(&mut self, lane_index: u8, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .three_byte_op_rm(OP2_3BYTE_ESCAPE_3A, OP3_INSERTPS_VpsUpsIb, vd, rn);
        self.formatter.immediate8((lane_index << 4) as _);
    }

    pub fn unpcklpd_rr(&mut self, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter.two_byte_op_rm(OP2_UNPCKLPD_VpdWpd, vd, rn);
    }

    pub fn pextrb_i8rr(&mut self, lane_index: u8, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .three_byte_op_rm(OP2_3BYTE_ESCAPE_3A, OP3_PEXTRB_MbVdqIb, vd, rn);
        self.formatter.immediate8(lane_index as _)
    }

    pub fn pextrw_i8rr(&mut self, lane_index: u8, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .three_byte_op_rm(OP2_3BYTE_ESCAPE_3A, OP3_PEXTRW_MwVdqIb, vd, rn);
        self.formatter.immediate8(lane_index as _)
    }

    pub fn pextrd_i8rr(&mut self, lane_index: u8, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .three_byte_op_rm(OP2_3BYTE_ESCAPE_3A, OP3_PEXTRD_EyVdqIb, vd, rn);
        self.formatter.immediate8(lane_index as _)
    }

    pub fn pextrq_i8rr(&mut self, lane_index: u8, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .three_byte_op_rm(OP2_3BYTE_ESCAPE_3A, OP3_PEXTRQ_EyVdqIb, vd, rn);
        self.formatter.immediate8(lane_index as _)
    }

    pub fn pshufd_i8rr(&mut self, lane_index: u8, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter.two_byte_op_rm(OP2_PSHUFD_VdqWdqIb, vd, rn);
        self.formatter.immediate8(lane_index as _)
    }

    pub fn pshufb_rr(&mut self, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .three_byte_op_rm(OP2_3BYTE_ESCAPE_38, OP3_PSHUFB_VdqWdq, vd, rn);
    }

    pub fn pshuflw_i8rr(&mut self, control_bits: u8, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter.two_byte_op_rm(OP2_PSHUFLW_VdqWdqIb, vd, rn);
        self.formatter.immediate8(control_bits as _)
    }

    pub fn pshufhw_rr(&mut self, control_bits: u8, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter.two_byte_op_rm(OP2_PSHUFHW_VdqWdqIb, vd, rn);
        self.formatter.immediate8(control_bits as _)
    }

    pub fn punpcklqdq_rr(&mut self, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter.two_byte_op_rm(OP2_PUNPCKLQDQ_VdqWdq, vd, rn);
    }

    pub fn shufps_i8rr(&mut self, lane_index: u8, rn: u8, vd: u8) {
        self.formatter.two_byte_op_rm(OP2_SHUFPS_VpdWpdIb, vd, rn);
        self.formatter.immediate8(lane_index as _)
    }

    pub fn shufpd_i8rr(&mut self, lane_index: u8, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter.two_byte_op_rm(OP2_SHUFPD_VpdWpdIb, vd, rn);
        self.formatter.immediate8(lane_index as _)
    }

    pub fn paddsb_rr(&mut self, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter.two_byte_op_rm(OP2_PADDSB_VdqWdq, vd, rn);
    }

    pub fn paddusb_rr(&mut self, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter.two_byte_op_rm(OP2_PADDUSB_VdqWdq, vd, rn);
    }

    pub fn paddusw_rr(&mut self, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter.two_byte_op_rm(OP2_PADDUSW_VdqWdq, vd, rn);
    }

    pub fn psubsb_rr(&mut self, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter.two_byte_op_rm(OP2_PSUBSB_VdqWdq, vd, rn);
    }

    pub fn psubusb_rr(&mut self, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter.two_byte_op_rm(OP2_PSUBUSB_VdqWdq, vd, rn);
    }

    pub fn psubusw_rr(&mut self, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter.two_byte_op_rm(OP2_PSUBUSW_VdqWdq, vd, rn);
    }

    pub fn psubsw_rr(&mut self, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter.two_byte_op_rm(OP2_PSUBSW_VdqWdq, vd, rn);
    }

    pub fn pmaxsb_rr(&mut self, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .three_byte_op_rm(OP2_3BYTE_ESCAPE_38, OP3_PMAXSB_VdqWdq, vd, rn);
    }

    pub fn pmaxsw_rr(&mut self, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter.two_byte_op_rm(OP2_PMAXSW_VdqWdq, vd, rn);
    }

    pub fn pmaxsd_rr(&mut self, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .three_byte_op_rm(OP2_3BYTE_ESCAPE_38, OP3_PMAXSD_VdqWdq, vd, rn);
    }

    pub fn pmaxub_rr(&mut self, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter.two_byte_op_rm(OP2_PMAXUB_VdqWdq, vd, rn);
    }

    pub fn pmaxuw_rr(&mut self, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .three_byte_op_rm(OP2_3BYTE_ESCAPE_38, OP3_PMAXUW_VdqWdq, vd, rn);
    }

    pub fn pmaxud_rr(&mut self, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .three_byte_op_rm(OP2_3BYTE_ESCAPE_38, OP3_PMAXUD_VdqWdq, vd, rn);
    }

    pub fn pminsb_rr(&mut self, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .three_byte_op_rm(OP2_3BYTE_ESCAPE_38, OP3_PMINSB_VdqWdq, vd, rn);
    }

    pub fn pminsw_rr(&mut self, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter.two_byte_op_rm(OP2_PMINSW_VdqWdq, vd, rn);
    }

    pub fn pminsd_rr(&mut self, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .three_byte_op_rm(OP2_3BYTE_ESCAPE_38, OP3_PMINSD_VdqWdq, vd, rn);
    }

    pub fn pminub_rr(&mut self, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter.two_byte_op_rm(OP2_PMINUB_VdqWdq, vd, rn);
    }

    pub fn pminuw_rr(&mut self, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .three_byte_op_rm(OP2_3BYTE_ESCAPE_38, OP3_PMINUW_VdqWdq, vd, rn);
    }

    pub fn pminud_rr(&mut self, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .three_byte_op_rm(OP2_3BYTE_ESCAPE_38, OP3_PMINUD_VdqWdq, vd, rn);
    }

    pub fn pavgb_rr(&mut self, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter.two_byte_op_rm(OP2_PAVGB_VdqWdq, vd, rn);
    }

    pub fn pavgw_rr(&mut self, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter.two_byte_op_rm(OP2_PAVGW_VdqWdq, vd, rn);
    }

    pub fn pabsb_rr(&mut self, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .three_byte_op_rm(OP2_3BYTE_ESCAPE_38, OP3_PABSB_VdqWdq, vd, rn);
    }

    pub fn pabsw_rr(&mut self, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .three_byte_op_rm(OP2_3BYTE_ESCAPE_38, OP3_PABSW_VdqWdq, vd, rn);
    }

    pub fn pabsd_rr(&mut self, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .three_byte_op_rm(OP2_3BYTE_ESCAPE_38, OP3_PABSD_VdqWdq, vd, rn);
    }

    pub fn pxor_rr(&mut self, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter.two_byte_op_rm(OP2_PXOR_VdqWdq, vd, rn);
    }

    pub fn pblendw_i8rr(&mut self, imm8: u8, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .three_byte_op_rm(OP2_3BYTE_ESCAPE_3A, OP3_VPBLENDW_VxHxWxIb, vd, rn);
        self.formatter.immediate8(imm8 as _);
    }

    pub fn addps_rr(&mut self, rn: u8, vd: u8) {
        self.formatter.two_byte_op_rm(OP2_ADDPS_VpsWps, vd, rn);
    }

    pub fn psubd_rr(&mut self, rn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter.two_byte_op_rm(OP2_PSUBD_VdqWdq, vd, rn);
    }

    pub fn cvtdq2ps_rr(&mut self, vn: u8, vd: u8) {
        self.formatter.two_byte_op_rm(OP2_CVTDQ2PS_VsdWsd, vd, vn);
    }

    pub fn cvtdq2pd_rr(&mut self, vn: u8, vd: u8) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter.two_byte_op_rm(OP2_CVTDQ2PS_VsdWsd, vd, vn);
    }

    pub fn packsswb_rr(&mut self, xmm_2: u8, xmm_1: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .two_byte_op_rm(OP2_PACKSSWB_VdqWdq, xmm_1, xmm_2);
    }

    pub fn packuswb_rr(&mut self, upper: u8, dest: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .two_byte_op_rm(OP2_PACKUSWB_VdqWdq, dest, upper);
    }

    pub fn packssdw_r(&mut self, xmm_2: u8, xmm_1: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .two_byte_op_rm(OP2_PACKSSDW_VdqWdq, xmm_1, xmm_2);
    }

    pub fn packusdw_rr(&mut self, xmm_2: u8, xmm_1: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .three_byte_op_rm(OP2_3BYTE_ESCAPE_38, OP3_VPACKUSDW_VxHxWx, xmm_1, xmm_2);
    }

    pub fn pmovsxbw(&mut self, xmm_2: u8, xmm_1: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .three_byte_op_rm(OP2_3BYTE_ESCAPE_38, OP3_VPMOVSXBW_VxUx, xmm_1, xmm_2);
    }

    pub fn pmovzxbw(&mut self, xmm_2: u8, xmm_1: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .three_byte_op_rm(OP2_3BYTE_ESCAPE_38, OP3_VPMOVZXBW_VxUx, xmm_1, xmm_2);
    }

    pub fn pmovzxwd(&mut self, xmm_2: u8, xmm_1: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .three_byte_op_rm(OP2_3BYTE_ESCAPE_38, OP3_VPMOVZXWD_VxUx, xmm_1, xmm_2);
    }

    pub fn pmovsxwd(&mut self, xmm_2: u8, xmm_1: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .three_byte_op_rm(OP2_3BYTE_ESCAPE_38, OP3_VPMOVSXWD_VxUx, xmm_1, xmm_2);
    }

    pub fn pmovsxdq(&mut self, xmm_2: u8, xmm_1: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .three_byte_op_rm(OP2_3BYTE_ESCAPE_38, OP3_VPMOVSXDQ_VxUx, xmm_1, xmm_2);
    }

    pub fn pmovzxdq(&mut self, xmm_2: u8, xmm_1: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .three_byte_op_rm(OP2_3BYTE_ESCAPE_38, OP3_VPMOVZXDQ_VxUx, xmm_1, xmm_2);
    }

    pub fn movl_rr(&mut self, src: u8, dst: u8) {
        self.formatter.one_byte_op_rm(OP_MOV_EvGv, src, dst);
    }

    pub fn movl_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter
            .one_byte_op_mem(OP_MOV_EvGv, src, base, offset)
    }

    pub fn movl_rm_disp32(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter
            .one_byte_op_mem_disp32(OP_MOV_EvGv, src, base, offset)
    }

    pub fn movl_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter
            .one_byte_op_mem_scaled(OP_MOV_EvGv, src, base, index, scale, offset)
    }

    pub fn movl_meax(&mut self, addr: *const u8) {
        self.formatter.one_byte_op(OP_MOV_EAXOv);
        #[cfg(target_pointer_width = "32")]
        {
            self.formatter.immediate32(addr as _);
        }
        #[cfg(target_pointer_width = "64")]
        {
            self.formatter.immediate64(addr as _);
        }
    }

    pub fn movl_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter
            .one_byte_op_mem(OP_MOV_GvEv, dst, base, offset)
    }

    pub fn movl_mr_addr(&mut self, offset: u32, dst: u8) {
        self.formatter.one_byte_op_addr(OP_MOV_GvEv, dst, offset);
    }

    pub fn movl_rm_addr(&mut self, src: u8, offset: u32) {
        self.formatter.one_byte_op_addr(OP_MOV_EvGv, src, offset);
    }

    pub fn movl_mr_disp32(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter
            .one_byte_op_mem_disp32(OP_MOV_GvEv, dst, base, offset)
    }

    pub fn movl_mr_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8, dst: u8) {
        self.formatter
            .one_byte_op_mem_scaled(OP_MOV_GvEv, dst, base, index, scale, offset)
    }

    pub fn movl_mr_disp8(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter
            .one_byte_op_mem_disp8(OP_MOV_GvEv, dst, base, offset)
    }

    pub fn movl_i32r(&mut self, imm: i32, dst: u8) {
        self.formatter.one_byte_op_r(OP_MOV_EAXIv, dst);
        self.formatter.immediate32(imm);
    }

    pub fn movl_i32m(&mut self, imm: i32, offset: i32, base: u8) {
        self.formatter
            .one_byte_op_mem(OP_GROUP11_EvIz, GROUP11_MOV, base, offset);
        self.formatter.immediate32(imm);
    }

    pub fn movl_i32m_scaled(&mut self, imm: i32, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.one_byte_op_mem_scaled(
            OP_GROUP11_EvIz,
            GROUP11_MOV,
            base,
            index,
            scale,
            offset,
        );
        self.formatter.immediate32(imm);
    }

    pub fn movb_i8m(&mut self, imm: i32, offset: i32, base: u8) {
        self.formatter
            .one_byte_op8_mem(OP_GROUP11_EvIb, GROUP11_MOV, base, offset);
        self.formatter.immediate8(imm);
    }

    pub fn movb_i8m_scaled(&mut self, imm: i32, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.one_byte_op8_mem_scaled(
            OP_GROUP11_EvIb,
            GROUP11_MOV,
            base,
            index,
            scale,
            offset,
        );
        self.formatter.immediate8(imm);
    }

    pub fn movb_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter
            .one_byte_op8_mem(OP_MOV_EbGb, src, base, offset)
    }

    pub fn movb_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter
            .one_byte_op8_mem_scaled(OP_MOV_EbGb, src, base, index, scale, offset)
    }

    pub fn movw_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.formatter
            .one_byte_op8_mem(OP_MOV_EvGv, src, base, offset)
    }

    pub fn movw_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.formatter
            .one_byte_op8_mem_scaled(OP_MOV_EvGv, src, base, index, scale, offset)
    }

    pub fn movw_im(&mut self, imm: i32, offset: i32, base: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.formatter
            .one_byte_op_mem(OP_GROUP11_EvIz, GROUP11_MOV, base, offset);
        self.formatter.immediate16(imm);
    }

    pub fn movw_im_scaled(&mut self, imm: i32, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.formatter.one_byte_op_mem_scaled(
            OP_GROUP11_EvIz,
            GROUP11_MOV,
            base,
            index,
            scale,
            offset,
        );
        self.formatter.immediate16(imm);
    }

    pub fn movl_eaxm(&mut self, addr: *const u8) {
        self.formatter.one_byte_op(OP_MOV_OvEAX);
        #[cfg(target_pointer_width = "32")]
        {
            self.formatter.immediate32(addr as _);
        }
        #[cfg(target_pointer_width = "64")]
        {
            self.formatter.immediate64(addr as _);
        }
    }

    pub fn movq_rr(&mut self, src: u8, dst: u8) {
        self.formatter.one_byte_op64_rm(OP_MOV_GvEv, dst, src);
    }

    pub fn movq_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter
            .one_byte_op64_mem(OP_MOV_EvGv, src, base, offset)
    }

    pub fn movq_rm_disp32(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter
            .one_byte_op64_mem_disp32(OP_MOV_EvGv, src, base, offset)
    }

    pub fn movq_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter
            .one_byte_op64_mem_scaled(OP_MOV_EvGv, src, base, index, scale, offset)
    }

    pub fn movq_rm_addr(&mut self, src: u8, offset: i32) {
        self.formatter
            .one_byte_op64_addr(OP_MOV_EvGv, src, offset as _)
    }

    pub fn movq_meax(&mut self, addr: *const u8) {
        self.formatter.one_byte_op64(OP_MOV_EAXOv);

        {
            self.formatter.immediate64(addr as _);
        }
    }

    pub fn movq_eaxm(&mut self, addr: *const u8) {
        self.formatter.one_byte_op64(OP_MOV_OvEAX);

        {
            self.formatter.immediate64(addr as _);
        }
    }

    pub fn movq_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter
            .one_byte_op64_mem(OP_MOV_GvEv, dst, base, offset)
    }

    pub fn movq_mr_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8, dst: u8) {
        self.formatter
            .one_byte_op64_mem_scaled(OP_MOV_GvEv, dst, base, index, scale, offset)
    }

    pub fn movq_mr_addr(&mut self, offset: i32, dst: u8) {
        self.formatter
            .one_byte_op64_addr(OP_MOV_GvEv, dst, offset as _)
    }

    pub fn movq_mr_disp32(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter
            .one_byte_op64_mem_disp32(OP_MOV_GvEv, dst, base, offset)
    }

    pub fn movq_mr_disp8(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter
            .one_byte_op64_mem_disp8(OP_MOV_GvEv, dst, base, offset)
    }

    pub fn movq_i32m(&mut self, imm: i32, offset: i32, base: u8) {
        self.formatter
            .one_byte_op64_mem(OP_GROUP11_EvIz, GROUP11_MOV, base, offset);
        self.formatter.immediate32(imm);
    }

    pub fn movq_i32m_scaled(&mut self, imm: i32, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.one_byte_op64_mem_scaled(
            OP_GROUP11_EvIz,
            GROUP11_MOV,
            base,
            index,
            scale,
            offset,
        );
        self.formatter.immediate32(imm);
    }

    pub fn movq_i64r(&mut self, imm: i64, dst: u8) {
        self.formatter.one_byte_op64_r(OP_MOV_EAXIv, dst);
        self.formatter.immediate64(imm);
    }

    pub fn mov_i32r(&mut self, imm: i32, dst: u8) {
        self.formatter
            .one_byte_op64_rm(OP_GROUP11_EvIz, GROUP11_MOV, dst);
        self.formatter.immediate32(imm);
    }

    pub fn movsxd_rr(&mut self, src: u8, dst: u8) {
        self.formatter.one_byte_op64_rm(OP_MOVSXD_GvEv, dst, src);
    }

    pub fn movzwl_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter
            .two_byte_op_mem(OP2_MOVZX_GvEw, dst, base, offset);
    }

    pub fn movzwl_mr_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8, dst: u8) {
        self.formatter
            .two_byte_op_mem_scaled(OP2_MOVZX_GvEw, dst, base, index, scale, offset);
    }

    pub fn movswl_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter
            .two_byte_op_mem(OP2_MOVSX_GvEw, dst, base, offset);
    }

    pub fn movswl_mr_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8, dst: u8) {
        self.formatter
            .two_byte_op_mem_scaled(OP2_MOVSX_GvEw, dst, base, index, scale, offset);
    }

    pub fn movzbl_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter
            .two_byte_op_mem(OP2_MOVZX_GvEb, dst, base, offset);
    }

    pub fn movzbl_mr_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8, dst: u8) {
        self.formatter
            .two_byte_op_mem_scaled(OP2_MOVZX_GvEb, dst, base, index, scale, offset);
    }

    pub fn movsbl_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter
            .two_byte_op_mem(OP2_MOVSX_GvEb, dst, base, offset);
    }

    pub fn movsbl_mr_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8, dst: u8) {
        self.formatter
            .two_byte_op_mem_scaled(OP2_MOVSX_GvEb, dst, base, index, scale, offset);
    }

    pub fn movzbl_rr(&mut self, src: u8, dst: u8) {
        // In 64-bit, this may cause an unnecessary REX to be planted (if the dst register
        // is in the range ESP-EDI, and the src would not have required a REX).  Unneeded
        // REX prefixes are defined to be silently ignored by the processor.
        self.formatter.two_byte_op8_rm(OP2_MOVZX_GvEb, dst, src);
    }

    pub fn movsbl_rr(&mut self, src: u8, dst: u8) {
        self.formatter.two_byte_op8_rm(OP2_MOVSX_GvEb, dst, src);
    }

    pub fn movsbq_rr(&mut self, src: u8, dst: u8) {
        self.formatter.two_byte_op64_rm(OP2_MOVSX_GvEb, dst, src);
    }

    pub fn movzwl_rr(&mut self, src: u8, dst: u8) {
        self.formatter.two_byte_op8_rm(OP2_MOVZX_GvEw, dst, src);
    }

    pub fn movswl_rr(&mut self, src: u8, dst: u8) {
        self.formatter.two_byte_op8_rm(OP2_MOVSX_GvEw, dst, src);
    }

    pub fn movswq_rr(&mut self, src: u8, dst: u8) {
        self.formatter.two_byte_op64_rm(OP2_MOVSX_GvEw, dst, src);
    }

    pub fn cmovl_rr(&mut self, cond: Condition, src: u8, dst: u8) {
        self.formatter.two_byte_op_rm(cmovcc(cond), dst, src);
    }

    pub fn cmovl_mr(&mut self, cond: Condition, offset: i32, base: u8, dst: u8) {
        self.formatter
            .two_byte_op_mem(cmovcc(cond), dst, base, offset);
    }

    pub fn cmovl_mr_scaled(
        &mut self,
        cond: Condition,
        offset: i32,
        base: u8,
        index: u8,
        scale: u8,
        dst: u8,
    ) {
        self.formatter
            .two_byte_op_mem_scaled(cmovcc(cond), dst, base, index, scale, offset);
    }

    pub fn cmovel_rr(&mut self, src: u8, dst: u8) {
        self.formatter
            .two_byte_op8_rm(cmovcc(Condition::E), dst, src);
    }

    pub fn cmovnel_rr(&mut self, src: u8, dst: u8) {
        self.formatter
            .two_byte_op8_rm(cmovcc(Condition::NE), dst, src);
    }

    pub fn cmovpl_rr(&mut self, src: u8, dst: u8) {
        self.formatter
            .two_byte_op8_rm(cmovcc(Condition::P), dst, src);
    }

    pub fn cmovnpl_rr(&mut self, src: u8, dst: u8) {
        self.formatter
            .two_byte_op8_rm(cmovcc(Condition::NP), dst, src);
    }

    pub fn cmovq_rr(&mut self, cond: Condition, src: u8, dst: u8) {
        self.formatter.two_byte_op64_rm(cmovcc(cond), dst, src);
    }

    pub fn cmovq_mr(&mut self, cond: Condition, offset: i32, base: u8, dst: u8) {
        self.formatter
            .two_byte_op64_mem(cmovcc(cond), dst, base, offset);
    }

    pub fn cmovq_mr_scaled(
        &mut self,
        cond: Condition,
        offset: i32,
        base: u8,
        index: u8,
        scale: u8,
        dst: u8,
    ) {
        self.formatter
            .two_byte_op64_mem_scaled(cmovcc(cond), dst, base, index, scale, offset);
    }

    pub fn cmoveq_rr(&mut self, src: u8, dst: u8) {
        self.formatter
            .two_byte_op64_rm(cmovcc(Condition::E), dst, src);
    }

    pub fn cmoveq_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter
            .two_byte_op64_mem(cmovcc(Condition::E), dst, base, offset);
    }

    pub fn cmovneq_rr(&mut self, src: u8, dst: u8) {
        self.formatter
            .two_byte_op64_rm(cmovcc(Condition::NE), dst, src);
    }

    pub fn cmovneq_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter
            .two_byte_op64_mem(cmovcc(Condition::NE), dst, base, offset);
    }

    pub fn cmovpq_rr(&mut self, src: u8, dst: u8) {
        self.formatter
            .two_byte_op64_rm(cmovcc(Condition::P), dst, src);
    }

    pub fn cmovnpq_rr(&mut self, src: u8, dst: u8) {
        self.formatter
            .two_byte_op64_rm(cmovcc(Condition::NP), dst, src);
    }

    pub fn leal_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter.one_byte_op_mem(OP_LEA, dst, base, offset);
    }

    pub fn leal_mr_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8, dst: u8) {
        self.formatter
            .one_byte_op_mem_scaled(OP_LEA, dst, base, index, scale, offset);
    }

    pub fn leaq_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter.one_byte_op64_mem(OP_LEA, dst, base, offset);
    }

    pub fn leaq_mr_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8, dst: u8) {
        self.formatter
            .one_byte_op64_mem_scaled(OP_LEA, dst, base, index, scale, offset);
    }

    pub fn call(&mut self) -> AssemblerLabel {
        self.formatter.one_byte_op(OP_CALL_rel32);
        self.formatter.immediate_rel32()
    }

    pub fn call_r(&mut self, dst: u8) -> AssemblerLabel {
        self.formatter
            .one_byte_op_rm(OP_GROUP5_Ev, GROUP5_OP_CALLN, dst);
        self.formatter.label()
    }

    pub fn call_m(&mut self, offset: i32, base: u8) {
        self.formatter
            .one_byte_op_mem(OP_GROUP5_Ev, GROUP5_OP_CALLN, base, offset);
    }

    pub fn jmp(&mut self) -> AssemblerLabel {
        self.formatter.one_byte_op(OP_JMP_rel32);
        self.formatter.immediate_rel32()
    }

    // Return a AssemblerLabel so we have a label to the jump, so we can use this
    // To make a tail recursive call on x86-64.  The MacroAssembler
    // really shouldn't wrap this as a Jump, since it can't be linked. :-/
    pub fn jmp_r(&mut self, dst: u8) -> AssemblerLabel {
        self.formatter
            .one_byte_op_rm(OP_GROUP5_Ev, GROUP5_OP_JMPN, dst);
        self.formatter.label()
    }

    pub fn jmp_m(&mut self, offset: i32, base: u8) {
        self.formatter
            .one_byte_op_mem(OP_GROUP5_Ev, GROUP5_OP_JMPN, base, offset);
    }

    pub fn jmp_m_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.one_byte_op_mem_scaled(
            OP_GROUP5_Ev,
            GROUP5_OP_JMPN,
            base,
            index,
            scale,
            offset,
        );
    }

    pub fn jne(&mut self) -> AssemblerLabel {
        self.formatter.two_byte_op(jcc_rel32(Condition::NE));
        self.formatter.immediate_rel32()
    }

    pub fn jnz(&mut self) -> AssemblerLabel {
        self.formatter.two_byte_op(jcc_rel32(Condition::NE));
        self.formatter.immediate_rel32()
    }

    pub fn je(&mut self) -> AssemblerLabel {
        self.formatter.two_byte_op(jcc_rel32(Condition::E));
        self.formatter.immediate_rel32()
    }

    pub fn jz(&mut self) -> AssemblerLabel {
        self.formatter.two_byte_op(jcc_rel32(Condition::E));
        self.formatter.immediate_rel32()
    }

    pub fn jl(&mut self) -> AssemblerLabel {
        self.formatter.two_byte_op(jcc_rel32(Condition::L));
        self.formatter.immediate_rel32()
    }

    pub fn jb(&mut self) -> AssemblerLabel {
        self.formatter.two_byte_op(jcc_rel32(Condition::B));
        self.formatter.immediate_rel32()
    }

    pub fn jle(&mut self) -> AssemblerLabel {
        self.formatter.two_byte_op(jcc_rel32(Condition::LE));
        self.formatter.immediate_rel32()
    }

    pub fn jbe(&mut self) -> AssemblerLabel {
        self.formatter.two_byte_op(jcc_rel32(Condition::BE));
        self.formatter.immediate_rel32()
    }

    pub fn jge(&mut self) -> AssemblerLabel {
        self.formatter.two_byte_op(jcc_rel32(Condition::GE));
        self.formatter.immediate_rel32()
    }

    pub fn jg(&mut self) -> AssemblerLabel {
        self.formatter.two_byte_op(jcc_rel32(Condition::G));
        self.formatter.immediate_rel32()
    }

    pub fn ja(&mut self) -> AssemblerLabel {
        self.formatter.two_byte_op(jcc_rel32(Condition::A));
        self.formatter.immediate_rel32()
    }

    pub fn jae(&mut self) -> AssemblerLabel {
        self.formatter.two_byte_op(jcc_rel32(Condition::AE));
        self.formatter.immediate_rel32()
    }

    pub fn jo(&mut self) -> AssemblerLabel {
        self.formatter.two_byte_op(jcc_rel32(Condition::O));
        self.formatter.immediate_rel32()
    }

    pub fn jnp(&mut self) -> AssemblerLabel {
        self.formatter.two_byte_op(jcc_rel32(Condition::NP));
        self.formatter.immediate_rel32()
    }

    pub fn jp(&mut self) -> AssemblerLabel {
        self.formatter.two_byte_op(jcc_rel32(Condition::P));
        self.formatter.immediate_rel32()
    }

    pub fn js(&mut self) -> AssemblerLabel {
        self.formatter.two_byte_op(jcc_rel32(Condition::S));
        self.formatter.immediate_rel32()
    }

    pub fn jcc(&mut self, condition: Condition) -> AssemblerLabel {
        self.formatter.two_byte_op(jcc_rel32(condition));
        self.formatter.immediate_rel32()
    }

    // SSE Operations:

    pub fn addsd_rr(&mut self, dst: u8, src: u8) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter.two_byte_op_rm(OP2_ADDSD_VsdWsd, dst, src);
    }

    pub fn addsd_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_mem(OP2_ADDSD_VsdWsd, dst, base, offset);
    }

    pub fn addsd_mr_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_mem_scaled(OP2_ADDSD_VsdWsd, dst, base, index, scale, offset);
    }

    pub fn addss_rr(&mut self, dst: u8, src: u8) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter.two_byte_op_rm(OP2_ADDSD_VsdWsd, dst, src);
    }

    pub fn addss_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op_mem(OP2_ADDSD_VsdWsd, dst, base, offset);
    }

    pub fn addss_mr_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op_mem_scaled(OP2_ADDSD_VsdWsd, dst, base, index, scale, offset);
    }

    pub fn cvtsi2sd_rr(&mut self, dst: u8, src: u8) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter.two_byte_op_rm(OP2_CVTSI2SD_VsdEd, dst, src);
    }

    pub fn cvtsi2ss_rr(&mut self, dst: u8, src: u8) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter.two_byte_op_rm(OP2_CVTSI2SD_VsdEd, dst, src);
    }

    pub fn cvtsi2sdq_rr(&mut self, dst: u8, src: u8) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op64_rm(OP2_CVTSI2SD_VsdEd, dst, src);
    }

    pub fn cvtsi2ssq_rr(&mut self, dst: u8, src: u8) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op64_rm(OP2_CVTSI2SD_VsdEd, dst, src);
    }

    pub fn cvtsi2sdq_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op64_mem(OP2_CVTSI2SD_VsdEd, dst, base, offset);
    }

    pub fn cvtsi2ssq_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op64_mem(OP2_CVTSI2SD_VsdEd, dst, base, offset);
    }

    pub fn cvtsi2sd_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_mem(OP2_CVTSI2SD_VsdEd, dst, base, offset);
    }

    pub fn cvtsi2ss_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op_mem(OP2_CVTSI2SD_VsdEd, dst, base, offset);
    }

    pub fn cvttsd2si_rr(&mut self, src: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter.two_byte_op_rm(OP2_CVTTSD2SI_GdWsd, dst, src);
    }

    pub fn cvttss2si_rr(&mut self, src: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter.two_byte_op_rm(OP2_CVTTSD2SI_GdWsd, dst, src);
    }

    pub fn cvtss2siq_rr(&mut self, src: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter.two_byte_op_rm(OP2_CVTTSD2SI_GdWsd, dst, src);
    }

    pub fn cvtsd2ss_rr(&mut self, src: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter.two_byte_op_rm(OP2_CVTSD2SS_VsdWsd, dst, src);
    }

    pub fn cvtsd2ss_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_mem(OP2_CVTSD2SS_VsdWsd, dst, base, offset);
    }

    pub fn cvtss2sd_rr(&mut self, src: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter.two_byte_op_rm(OP2_CVTSD2SS_VsdWsd, dst, src);
    }

    pub fn cvtss2sd_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op_mem(OP2_CVTSD2SS_VsdWsd, dst, base, offset);
    }

    pub fn cvttsd2siq_rr(&mut self, src: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op64_rm(OP2_CVTTSD2SI_GdWsd, dst, src);
    }

    pub fn movd_f2r(&mut self, src: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter.two_byte_op_rm(OP2_MOVD_EdVd, dst, src);
    }

    pub fn movd_r2f(&mut self, src: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter.two_byte_op_rm(OP2_MOVD_VdEd, dst, src);
    }

    pub fn movddup_rr(&mut self, src: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter.two_byte_op_rm(OP2_MOVDDUP_VqWq, dst, src);
    }

    pub fn movmskpd_rr(&mut self, src: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter.two_byte_op_rm(OP2_MOVMSKPD_VdEd, dst, src);
    }

    pub fn movq_f2r(&mut self, src: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter.two_byte_op64_rm(OP2_MOVD_VdEd, dst, src);
    }

    pub fn movq_r2f(&mut self, src: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter.two_byte_op64_rm(OP2_MOVD_EdVd, dst, src);
    }

    pub fn movapd_rr(&mut self, src: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter.two_byte_op_rm(OP2_MOVAPD_VpdWpd, dst, src);
    }

    pub fn movaps_rr(&mut self, src: u8, dst: u8) {
        self.formatter.two_byte_op_rm(OP2_MOVAPD_VpdWpd, dst, src);
    }

    pub fn movhlps_rr(&mut self, src: u8, dst: u8) {
        self.formatter.two_byte_op_rm(OP2_MOVHLPS_VqUq, dst, src);
    }

    pub fn movsd_rr(&mut self, src: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter.two_byte_op_rm(OP2_MOVSD_VsdWsd, dst, src);
    }

    pub fn movsd_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_mem(OP2_MOVSD_WsdVsd, src, base, offset);
    }

    pub fn movsd_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_mem_scaled(OP2_MOVSD_WsdVsd, src, base, index, scale, offset);
    }

    pub fn movss_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op_mem(OP2_MOVSS_WsdVsd, src, base, offset);
    }

    pub fn movss_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op_mem_scaled(OP2_MOVSS_WsdVsd, src, base, index, scale, offset);
    }

    pub fn movsd_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_mem(OP2_MOVSD_VsdWsd, dst, base, offset);
    }

    pub fn movsd_mr_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_mem_scaled(OP2_MOVSD_VsdWsd, dst, base, index, scale, offset);
    }

    pub fn movss_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op_mem(OP2_MOVSS_VsdWsd, dst, base, offset);
    }

    pub fn movss_mr_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op_mem_scaled(OP2_MOVSS_VsdWsd, dst, base, index, scale, offset);
    }

    pub fn movshdup_rr(&mut self, src: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter.two_byte_op_rm(OP2_MOVSHDUP_VqWq, dst, src);
    }

    pub fn movsldup_rr(&mut self, src: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter.two_byte_op_rm(OP2_MOVSLDUP_VqWq, dst, src);
    }

    pub fn mulsd_rr(&mut self, src: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter.two_byte_op_rm(OP2_MULSD_VsdWsd, dst, src);
    }

    pub fn mulsd_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_mem(OP2_MULSD_VsdWsd, dst, base, offset);
    }

    pub fn mulsd_mr_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_mem_scaled(OP2_MULSD_VsdWsd, dst, base, index, scale, offset);
    }

    pub fn mulss_rr(&mut self, src: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter.two_byte_op_rm(OP2_MULSD_VsdWsd, dst, src);
    }

    pub fn mulss_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op_mem(OP2_MULSD_VsdWsd, dst, base, offset);
    }

    pub fn mulss_mr_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op_mem_scaled(OP2_MULSD_VsdWsd, dst, base, index, scale, offset);
    }

    pub fn pextrw_irr(&mut self, which_word: i32, src: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter.two_byte_op_rm(OP2_PEXTRW_GdUdIb, dst, src);
        self.formatter.immediate8(which_word);
    }

    pub fn psllq_i8r(&mut self, shift: i32, dst: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .two_byte_op_rm(OP2_PSLLQ_UdqIb, GROUP14_OP_PSLLQ, dst);
        self.formatter.immediate8(shift);
    }

    pub fn psrld_i8r(&mut self, shift: i32, dst: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .two_byte_op_rm(OP2_PSRLD_UdqIb, GROUP14_OP_PSRLQ, dst);
        self.formatter.immediate8(shift);
    }

    pub fn prslq_i8r(&mut self, shift: i32, dst: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .two_byte_op_rm(OP2_PSRLQ_UdqIb, GROUP14_OP_PSRLQ, dst);
        self.formatter.immediate8(shift);
    }

    pub fn por_rr(&mut self, src: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter.two_byte_op_rm(OP2_POR_VdqWdq, dst, src);
    }

    pub fn subsd_rr(&mut self, src: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter.two_byte_op_rm(OP2_SUBSD_VsdWsd, dst, src);
    }

    pub fn subsd_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_mem(OP2_SUBSD_VsdWsd, dst, base, offset);
    }

    pub fn subsd_mr_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_mem_scaled(OP2_SUBSD_VsdWsd, dst, base, index, scale, offset);
    }

    pub fn subss_rr(&mut self, src: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter.two_byte_op_rm(OP2_SUBSD_VsdWsd, dst, src);
    }

    pub fn subss_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op_mem(OP2_SUBSD_VsdWsd, dst, base, offset);
    }

    pub fn subss_mr_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op_mem_scaled(OP2_SUBSD_VsdWsd, dst, base, index, scale, offset);
    }

    pub fn ucomisd_rr(&mut self, src: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter.two_byte_op_rm(OP2_UCOMISD_VsdWsd, dst, src);
    }

    pub fn ucomisd_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .two_byte_op_mem(OP2_UCOMISD_VsdWsd, dst, base, offset);
    }

    pub fn ucomisd_mr_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .two_byte_op_mem_scaled(OP2_UCOMISD_VsdWsd, dst, base, index, scale, offset);
    }

    pub fn ucomiss_rr(&mut self, src: u8, dst: u8) {
        self.formatter.two_byte_op_rm(OP2_UCOMISD_VsdWsd, dst, src);
    }

    pub fn ucomiss_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter
            .two_byte_op_mem(OP2_UCOMISD_VsdWsd, dst, base, offset);
    }

    pub fn ucomiss_mr_scaled(&mut self, offset: i32, base: u8, index: u8, scale: u8, dst: u8) {
        self.formatter
            .two_byte_op_mem_scaled(OP2_UCOMISD_VsdWsd, dst, base, index, scale, offset);
    }

    pub fn divsd_rr(&mut self, src: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter.two_byte_op_rm(OP2_DIVSD_VsdWsd, dst, src);
    }

    pub fn divsd_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_mem(OP2_DIVSD_VsdWsd, dst, base, offset);
    }

    pub fn divss_rr(&mut self, src: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter.two_byte_op_rm(OP2_DIVSD_VsdWsd, dst, src);
    }

    pub fn divss_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op_mem(OP2_DIVSD_VsdWsd, dst, base, offset);
    }

    pub fn andps_rr(&mut self, src: u8, dst: u8) {
        self.formatter.two_byte_op_rm(OP2_ANDPS_VpsWps, dst, src);
    }

    pub fn orps_rr(&mut self, src: u8, dst: u8) {
        self.formatter.two_byte_op_rm(OP2_ORPS_VpsWps, dst, src);
    }

    pub fn xorps_rr(&mut self, src: u8, dst: u8) {
        self.formatter.two_byte_op_rm(OP2_XORPS_VpsWps, dst, src);
    }

    pub fn xorpd_rr(&mut self, src: u8, dst: u8) {
        if src == dst {
            self.xorps_rr(src, dst);
        } else {
            self.formatter.prefix(PRE_SSE_66);
            self.formatter.two_byte_op_rm(OP2_XORPD_VpdWpd, dst, src);
        }
    }

    pub fn andnpd_rr(&mut self, src: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter.two_byte_op_rm(OP2_ANDNPD_VpdWpd, dst, src);
    }

    pub fn sqrtsd_rr(&mut self, src: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter.two_byte_op_rm(OP2_SQRTSD_VsdWsd, dst, src);
    }

    pub fn sqrtsd_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_mem(OP2_SQRTSD_VsdWsd, dst, base, offset);
    }

    pub fn sqrtss_rr(&mut self, src: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter.two_byte_op_rm(OP2_SQRTSS_VssWss, dst, src);
    }

    pub fn sqrtss_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op_mem(OP2_SQRTSS_VssWss, dst, base, offset);
    }

    pub fn roundss_rr(&mut self, src: u8, dst: u8, rounding: RoundingType) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .three_byte_op_rm(OP2_3BYTE_ESCAPE_3A, OP3_ROUNDSS_VssWssIb, dst, src);
        self.formatter.immediate8(rounding as _);
    }

    pub fn roundss_mr(&mut self, offset: i32, base: u8, dst: u8, rounding: RoundingType) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter.three_byte_op_mem(
            OP2_3BYTE_ESCAPE_3A,
            OP3_ROUNDSS_VssWssIb,
            dst,
            base,
            offset,
        );
        self.formatter.immediate8(rounding as _);
    }

    pub fn roundsd_rr(&mut self, src: u8, dst: u8, rounding: RoundingType) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .three_byte_op_rm(OP2_3BYTE_ESCAPE_3A, OP3_ROUNDSD_VsdWsdIb, dst, src);
        self.formatter.immediate8(rounding as _);
    }

    pub fn roundsd_mr(&mut self, offset: i32, base: u8, dst: u8, rounding: RoundingType) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter.three_byte_op_mem(
            OP2_3BYTE_ESCAPE_3A,
            OP3_ROUNDSD_VsdWsdIb,
            dst,
            base,
            offset,
        );
        self.formatter.immediate8(rounding as _);
    }

    // Misc operations:

    pub fn int3(&mut self) {
        self.formatter.one_byte_op(OP_INT3);
    }

    pub fn is_int3(addr: &u8) -> bool {
        *addr == OP_INT3
    }

    pub fn ret(&mut self) {
        self.formatter.one_byte_op(OP_RET);
    }

    pub fn predict_not_taken(&mut self) {
        self.formatter.prefix(PRE_PREDICT_BRANCH_NOT_TAKEN);
    }

    pub fn lock(&mut self) {
        self.formatter.prefix(PRE_LOCK);
    }

    // Causes the memory access in the next instruction to be offset by %gs. Usually you use
    // this with a 32-bit absolute address load. That "address" ends up being the offset to
    // %gs. This prefix is ignored by lea. Getting the value of %gs is hard - you can pretty
    // much just use it as a secret offset.

    pub fn gs(&mut self) {
        self.formatter.prefix(PRE_GS);
    }

    pub fn cmpxchgb_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter
            .two_byte_op8_mem(OP2_CMPXCHGb, src, base, offset);
    }

    pub fn cmpxchgb_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter
            .two_byte_op8_mem_scaled(OP2_CMPXCHGb, src, base, index, scale, offset);
    }

    pub fn cmpxchgw_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.formatter
            .two_byte_op_mem(OP2_CMPXCHG, src, base, offset);
    }

    pub fn cmpxchgw_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.formatter
            .two_byte_op_mem_scaled(OP2_CMPXCHG, src, base, index, scale, offset);
    }

    pub fn cmpxchgl_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter
            .two_byte_op_mem(OP2_CMPXCHG, src, base, offset);
    }

    pub fn cmpxchgl_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter
            .two_byte_op_mem_scaled(OP2_CMPXCHG, src, base, index, scale, offset);
    }

    pub fn cmpxchgq_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter
            .two_byte_op64_mem(OP2_CMPXCHG, src, base, offset);
    }

    pub fn cmpxchgq_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter
            .two_byte_op64_mem_scaled(OP2_CMPXCHG, src, base, index, scale, offset);
    }

    pub fn xaddb_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter
            .two_byte_op8_mem(OP2_XADDb, src, base, offset);
    }

    pub fn xaddb_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter
            .two_byte_op8_mem_scaled(OP2_XADDb, src, base, index, scale, offset);
    }

    pub fn xaddw_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.formatter.two_byte_op_mem(OP2_XADD, src, base, offset);
    }

    pub fn xaddw_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.formatter
            .two_byte_op_mem_scaled(OP2_XADD, src, base, index, scale, offset);
    }

    pub fn xaddl_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter.two_byte_op_mem(OP2_XADD, src, base, offset);
    }

    pub fn xaddl_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter
            .two_byte_op_mem_scaled(OP2_XADD, src, base, index, scale, offset);
    }

    pub fn xaddq_rm(&mut self, src: u8, offset: i32, base: u8) {
        self.formatter
            .two_byte_op64_mem(OP2_XADD, src, base, offset);
    }

    pub fn xaddq_rm_scaled(&mut self, src: u8, offset: i32, base: u8, index: u8, scale: u8) {
        self.formatter
            .two_byte_op64_mem_scaled(OP2_XADD, src, base, index, scale, offset);
    }

    pub fn lfence(&mut self) {
        self.formatter
            .three_byte_op(OP2_3BYTE_ESCAPE_AE, OP3_LFENCE);
    }

    pub fn mfence(&mut self) {
        self.formatter
            .three_byte_op(OP2_3BYTE_ESCAPE_AE, OP3_MFENCE);
    }

    pub fn sfence(&mut self) {
        self.formatter
            .three_byte_op(OP2_3BYTE_ESCAPE_AE, OP3_SFENCE);
    }

    pub fn rdtsc(&mut self) {
        self.formatter.two_byte_op(OP2_RDTSC);
    }

    pub fn pause(&mut self) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter.one_byte_op(OP_PAUSE);
    }

    pub fn cpuid(&mut self) {
        self.formatter.two_byte_op(OP2_CPUID);
    }

    // AVX methods.
    //
    // Our AVX methods follow our rule of using AT&T syntax, i.e. reverse order of Intel syntax.
    // So, for example,
    //
    //    VEX.128.0F.WIG 14 /r VUNPCKLPS xmm1,xmm2, xmm3/m128
    //
    // will have following three methods potentially.
    //     - vunpcklps_rrr(XMMRegisterID xmm3, XMMRegisterID xmm2, XMMRegisterID xmm1)
    //     - vunpcklps_mrr(int offset, RegisterID base, XMMRegisterID xmm2, XMMRegisterID xmm1)
    //     - vunpcklps_mrr(int offset, RegisterID base, RegisterID index, int scale, XMMRegisterID xmm2, XMMRegisterID xmm1)
    //
    // For detailed Instruction encoding, read Intel SDM 2.3, "INTEL ADVANCED VECTOR EXTENSIONS (INTEL AVX)".
    // And 3.1.1.2 Opcode Column in the Instruction Summary Table (Instructions with VEX prefix)
    //
    // And the way to read the instruction is
    //
    // VEX.128.0F.WIG 14 /r VUNPCKLPS xmm1,xmm2, xmm3/m128
    //     - 128 bit AVX
    //     - only 0F, so PRE_SSE_00 prefix
    //     - only 0F, so two byte opcode
    //     - WIG => W is ignored, this is necessary to use two-byte VEX prefix. If W needs to have 0 or 1, then we need to use three byte VEX prefix.
    //     - 14 => opcode second byte (0F 14 is opcode)
    //     - /r => ModR/M contains register
    //     - VUNPCKLPS is instruction name
    //     - xmm1 is destination
    //     - xmm2 and xmm3 are operands
    //     - /r says that vvvv in VEX prefix is representing a register operand and an r/m operand.
    //
    // VEX.128.66.0F3A.W0 14 /r ib VPEXTRB reg/m8, xmm2, imm8
    //     - 128 bit AVX
    //     - 66, so PRE_SSE_66 prefix
    //     - 0F3A, so three byte opcode
    //     - W0 => W is 0
    //     - 14 => opcode third byte (0F 3A 14 is opcode)
    //     - /r => ModR/M contains register
    //     - ib => one byte imm before opcode.
    //     - VPEXTRB is instruction name
    //     - xmm1 is destination
    //     - xmm2 and xmm3 are operands
    //
    //  See also https://stackoverflow.com/questions/15017659/how-to-read-the-intel-opcode-notation
    //
    //  AVX instruction format is the following
    //
    //     [Prefixes] [VEX] OPCODE ModR/M [SIB] [DISP]
    //
    //  Prefixes are typically empty (LOCK etc. and they are invalid).
    //  How operand is mapped to these fields are defined per instruction.

    pub fn vzeroupper(&mut self) {
        self.formatter
            .vex_nds_lig_two_byte_op(PRE_SSE_00, VexW::W0, OP2_VZEROUPPER);
    }

    pub fn vpinsrb_i8mrr(&mut self, lane_index: u8, offset: i32, base: u8, xmm_2: u8, xmm_1: u8) {
        self.formatter.vex_nds_lig_three_byte_op_mem(
            PRE_SSE_66,
            VexImpliedBytes::ThreeBytesOp3A,
            VexW::W0,
            OP3_PINSRB_VdqRdqpIb,
            xmm_1,
            xmm_2,
            base,
            offset,
        );

        self.formatter.immediate8(lane_index as _);
    }

    pub fn vpinsrb_i8rrr(&mut self, lane_index: u8, rm: u8, xmm_2: u8, xmm_1: u8) {
        self.formatter.vex_nds_lig_three_byte_op(
            PRE_SSE_66,
            VexImpliedBytes::ThreeBytesOp3A,
            VexW::W0,
            OP3_PINSRB_VdqRdqpIb,
            xmm_1,
            xmm_2,
            rm,
        );

        self.formatter.immediate8(lane_index as _);
    }

    pub fn vpinsrw_i8mrr(&mut self, lane_index: u8, offset: i32, base: u8, xmm_2: u8, xmm_1: u8) {
        self.formatter.vex_nds_lig_two_byte_op_mem(
            PRE_SSE_66,
            VexW::W0,
            OP2_PINSRW_VdqRdqp,
            xmm_1,
            xmm_2,
            base,
            offset,
        );

        self.formatter.immediate8(lane_index as _);
    }

    pub fn vpinsrw_i8rrr(&mut self, lane_index: u8, rm: u8, xmm_2: u8, xmm_1: u8) {
        self.formatter.vex_nds_lig_two_byte_op_rr(
            PRE_SSE_66,
            VexW::W0,
            OP2_PINSRW_VdqRdqp,
            xmm_1,
            xmm_2,
            rm,
        );

        self.formatter.immediate8(lane_index as _);
    }

    pub fn vpinsrd_i8mrr(&mut self, lane_index: u8, offset: i32, base: u8, xmm_2: u8, xmm_1: u8) {
        self.formatter.vex_nds_lig_three_byte_op_mem(
            PRE_SSE_66,
            VexImpliedBytes::ThreeBytesOp3A,
            VexW::W1,
            OP3_PINSRD_VdqEdIb,
            xmm_1,
            xmm_2,
            base,
            offset,
        );

        self.formatter.immediate8(lane_index as _);
    }

    pub fn vpinsrd_i8rrr(&mut self, lane_index: u8, rm: u8, xmm_2: u8, xmm_1: u8) {
        self.formatter.vex_nds_lig_three_byte_op(
            PRE_SSE_66,
            VexImpliedBytes::ThreeBytesOp3A,
            VexW::W1,
            OP3_PINSRD_VdqEdIb,
            xmm_1,
            xmm_2,
            rm,
        );

        self.formatter.immediate8(lane_index as _);
    }

    pub fn vpinsrq_i8mrr(&mut self, lane_index: u8, offset: i32, base: u8, xmm_2: u8, xmm_1: u8) {
        self.formatter.vex_nds_lig_three_byte_op_mem(
            PRE_SSE_66,
            VexImpliedBytes::ThreeBytesOp3A,
            VexW::W1,
            OP3_PINSRQ_VdqEqbIb,
            xmm_1,
            xmm_2,
            base,
            offset,
        );

        self.formatter.immediate8(lane_index as _);
    }

    pub fn vpinsrq_i8rrr(&mut self, lane_index: u8, rm: u8, xmm_2: u8, xmm_1: u8) {
        self.formatter.vex_nds_lig_three_byte_op(
            PRE_SSE_66,
            VexImpliedBytes::ThreeBytesOp3A,
            VexW::W1,
            OP3_PINSRQ_VdqEqbIb,
            xmm_1,
            xmm_2,
            rm,
        );

        self.formatter.immediate8(lane_index as _);
    }

    pub fn vinsertps_i8rrr(&mut self, lane_index: u8, rm: u8, xmm_2: u8, xmm_1: u8) {
        self.formatter.vex_nds_lig_three_byte_op(
            PRE_SSE_66,
            VexImpliedBytes::ThreeBytesOp3A,
            VexW::W0,
            OP3_INSERTPS_VpsUpsIb,
            xmm_1,
            xmm_2,
            rm,
        );

        self.formatter.immediate8(lane_index as _);
    }

    pub fn vmovddup_rr(&mut self, src: u8, dst: u8) {
        self.formatter.vex_nds_lig_two_byte_op_rr(
            PRE_SSE_66,
            VexW::W0,
            OP2_MOVDDUP_VqWq,
            dst,
            0,
            src,
        );
    }

    pub fn vmovddup_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter.vex_nds_lig_two_byte_op_mem(
            PRE_SSE_66,
            VexW::W0,
            OP2_MOVDDUP_VqWq,
            dst,
            0,
            base,
            offset,
        );
    }

    pub fn vmovapd_rr(&mut self, src: u8, dst: u8) {
        self.formatter.vex_nds_lig_two_byte_op_rr(
            PRE_SSE_66,
            VexW::W0,
            OP2_MOVAPD_VpdWpd,
            dst,
            0,
            src,
        );
    }

    pub fn vbroadcastss_mr(&mut self, offset: i32, base: u8, dst: u8) {
        self.formatter.vex_nds_lig_three_byte_op_mem(
            PRE_SSE_66,
            VexImpliedBytes::ThreeBytesOp38,
            VexW::W0,
            OP3_VBROADCASTSS_VxWd,
            dst,
            0,
            base,
            offset,
        );
    }

    pub fn vpunpcklbw_rrr(&mut self, xmm_3: u8, xmm_2: u8, xmm_1: u8) {
        self.formatter.vex_nds_lig_two_byte_op_rr(
            PRE_SSE_66,
            VexW::W0,
            OP2_PUNPCKLBW_VdqWdq,
            xmm_1,
            xmm_3,
            xmm_2,
        );
    }

    pub fn vpunpckhbw_rrr(&mut self, xmm_3: u8, xmm_2: u8, xmm_1: u8) {
        self.formatter.vex_nds_lig_two_byte_op_rr(
            PRE_SSE_66,
            VexW::W0,
            OP2_PUNPCKHBW_VdqWdq,
            xmm_1,
            xmm_3,
            xmm_2,
        );
    }

    pub fn vpunpcklqdq_rrr(&mut self, xmm_3: u8, xmm_2: u8, xmm_1: u8) {
        self.formatter.vex_nds_lig_two_byte_op_rr(
            PRE_SSE_66,
            VexW::W0,
            OP2_PUNPCKLQDQ_VdqWdq,
            xmm_1,
            xmm_3,
            xmm_2,
        );
    }

    pub fn vunpcklps_rrr(&mut self, xmm_3: u8, xmm_2: u8, xmm_1: u8) {
        self.formatter.vex_nds_lig_two_byte_op_rr(
            PRE_SSE_00,
            VexW::W0,
            OP2_UNPCKLPD_VpdWpd,
            xmm_1,
            xmm_3,
            xmm_2,
        );
    }

    pub fn vunpcklpd_rrr(&mut self, xmm_3: u8, xmm_2: u8, xmm_1: u8) {
        self.formatter.vex_nds_lig_two_byte_op_rr(
            PRE_SSE_66,
            VexW::W0,
            OP2_UNPCKLPD_VpdWpd,
            xmm_1,
            xmm_3,
            xmm_2,
        );
    }

    pub fn vpextrb_i8rr(&mut self, lane_index: u8, vn: u8, rd: u8) {
        self.formatter.vex_nds_lig_three_byte_op(
            PRE_SSE_66,
            VexImpliedBytes::ThreeBytesOp3A,
            VexW::W0,
            OP3_PEXTRB_MbVdqIb,
            vn,
            0,
            rd,
        );

        self.formatter.immediate8(lane_index as _);
    }

    pub fn vpextrb_i8rm(&mut self, lane_index: u8, src: u8, offset: i32, base: u8) {
        self.formatter.vex_nds_lig_three_byte_op_mem(
            PRE_SSE_66,
            VexImpliedBytes::ThreeBytesOp3A,
            VexW::W0,
            OP3_PEXTRB_MbVdqIb,
            src,
            0,
            base,
            offset,
        );

        self.formatter.immediate8(lane_index as _);
    }

    pub fn vpextrw_i8rr(&mut self, lane_index: u8, vn: u8, rd: u8) {
        self.formatter.vex_nds_lig_three_byte_op(
            PRE_SSE_66,
            VexImpliedBytes::ThreeBytesOp3A,
            VexW::W0,
            OP3_PEXTRW_MwVdqIb,
            vn,
            0,
            rd,
        );

        self.formatter.immediate8(lane_index as _);
    }

    pub fn vpextrw_i8rm(&mut self, lane_index: u8, src: u8, offset: i32, base: u8) {
        self.formatter.vex_nds_lig_three_byte_op_mem(
            PRE_SSE_66,
            VexImpliedBytes::ThreeBytesOp3A,
            VexW::W0,
            OP3_PEXTRW_MwVdqIb,
            src,
            0,
            base,
            offset,
        );

        self.formatter.immediate8(lane_index as _);
    }

    pub fn vpextrd_i8rr(&mut self, lane_index: u8, vn: u8, rd: u8) {
        self.formatter.vex_nds_lig_three_byte_op(
            PRE_SSE_66,
            VexImpliedBytes::ThreeBytesOp3A,
            VexW::W0,
            OP3_PEXTRD_EyVdqIb,
            vn,
            0,
            rd,
        );

        self.formatter.immediate8(lane_index as _);
    }

    pub fn vpextrd_i8rm(&mut self, lane_index: u8, src: u8, offset: i32, base: u8) {
        self.formatter.vex_nds_lig_three_byte_op_mem(
            PRE_SSE_66,
            VexImpliedBytes::ThreeBytesOp3A,
            VexW::W0,
            OP3_PEXTRD_EyVdqIb,
            src,
            0,
            base,
            offset,
        );

        self.formatter.immediate8(lane_index as _);
    }

    pub fn vpextrq_i8rr(&mut self, lane_index: u8, vn: u8, rd: u8) {
        self.formatter.vex_nds_lig_three_byte_op(
            PRE_SSE_66,
            VexImpliedBytes::ThreeBytesOp3A,
            VexW::W1,
            OP3_PEXTRQ_EyVdqIb,
            vn,
            0,
            rd,
        );

        self.formatter.immediate8(lane_index as _);
    }

    pub fn vpshufb_rrr(&mut self, vm: u8, vn: u8, vd: u8) {
        self.formatter.vex_nds_lig_three_byte_op(
            PRE_SSE_66,
            VexImpliedBytes::ThreeBytesOp38,
            VexW::W0,
            OP3_PSHUFB_VdqWdq,
            vd,
            vn,
            vm,
        );
    }

    pub fn vshufps_i8rrr(&mut self, control_bits: u8, xmm_3: u8, xmm_2: u8, xmm_1: u8) {
        self.formatter.vex_nds_lig_two_byte_op_rr(
            PRE_SSE_00,
            VexW::W0,
            OP2_SHUFPS_VpdWpdIb,
            xmm_1,
            xmm_2,
            xmm_3,
        );

        self.formatter.immediate8(control_bits as _);
    }

    pub fn vshufpd_i8rrr(&mut self, control_bits: u8, xmm_3: u8, xmm_2: u8, xmm_1: u8) {
        self.formatter.vex_nds_lig_two_byte_op_rr(
            PRE_SSE_66,
            VexW::W0,
            OP2_SHUFPD_VpdWpdIb,
            xmm_1,
            xmm_2,
            xmm_3,
        );

        self.formatter.immediate8(control_bits as _);
    }

    pub fn vpshuflw_i8rr(&mut self, control_bits: u8, xmm_2: u8, xmm_1: u8) {
        self.formatter.vex_nds_lig_two_byte_op_rr(
            PRE_SSE_F2,
            VexW::W0,
            OP2_PSHUFLW_VdqWdqIb,
            xmm_1,
            0,
            xmm_2,
        );

        self.formatter.immediate8(control_bits as _);
    }

    pub fn vpshufd_i8rr(&mut self, control_bits: u8, xmm_2: u8, xmm_1: u8) {
        self.formatter.vex_nds_lig_two_byte_op_rr(
            PRE_SSE_66,
            VexW::W0,
            OP2_PSHUFD_VdqWdqIb,
            xmm_1,
            0,
            xmm_2,
        );

        self.formatter.immediate8(control_bits as _);
    }

    pub fn vpaddsb_rrr(&mut self, right: u8, left: u8, vd: u8) {
        self.formatter.vex_nds_lig_wig_commutative_two_byte_op(
            PRE_SSE_66,
            OP2_PADDSB_VdqWdq,
            vd,
            left,
            right,
        );
    }

    pub fn vpaddusb_rrr(&mut self, right: u8, left: u8, vd: u8) {
        self.formatter.vex_nds_lig_wig_commutative_two_byte_op(
            PRE_SSE_66,
            OP2_PADDUSB_VdqWdq,
            vd,
            left,
            right,
        );
    }

    pub fn vpaddsw_rrr(&mut self, right: u8, left: u8, vd: u8) {
        self.formatter.vex_nds_lig_wig_commutative_two_byte_op(
            PRE_SSE_66,
            OP2_PADDSW_VdqWdq,
            vd,
            left,
            right,
        );
    }

    pub fn vpaddusw_rrr(&mut self, right: u8, left: u8, vd: u8) {
        self.formatter.vex_nds_lig_wig_commutative_two_byte_op(
            PRE_SSE_66,
            OP2_PADDUSW_VdqWdq,
            vd,
            left,
            right,
        );
    }

    pub fn vpsubsb_rrr(&mut self, right: u8, left: u8, vd: u8) {
        self.formatter.vex_nds_lig_wig_commutative_two_byte_op(
            PRE_SSE_66,
            OP2_PSUBSB_VdqWdq,
            vd,
            left,
            right,
        );
    }

    pub fn vpsubusb_rrr(&mut self, right: u8, left: u8, vd: u8) {
        self.formatter.vex_nds_lig_wig_commutative_two_byte_op(
            PRE_SSE_66,
            OP2_PSUBUSB_VdqWdq,
            vd,
            left,
            right,
        );
    }

    pub fn vpsubsw_rrr(&mut self, right: u8, left: u8, vd: u8) {
        self.formatter.vex_nds_lig_wig_commutative_two_byte_op(
            PRE_SSE_66,
            OP2_PSUBSW_VdqWdq,
            vd,
            left,
            right,
        );
    }

    pub fn vpsubusw_rrr(&mut self, right: u8, left: u8, vd: u8) {
        self.formatter.vex_nds_lig_wig_commutative_two_byte_op(
            PRE_SSE_66,
            OP2_PSUBUSW_VdqWdq,
            vd,
            left,
            right,
        );
    }

    pub fn vpmaxsb_rrr(&mut self, right: u8, left: u8, vd: u8) {
        self.formatter.vex_nds_lig_three_byte_op(
            PRE_SSE_66,
            VexImpliedBytes::ThreeBytesOp38,
            VexW::W0,
            OP3_PMAXSB_VdqWdq,
            vd,
            left,
            right,
        );
    }

    pub fn vpmaxsw_rrr(&mut self, right: u8, left: u8, vd: u8) {
        self.formatter.vex_nds_lig_two_byte_op_rr(
            PRE_SSE_66,
            VexW::W0,
            OP2_PMAXSW_VdqWdq,
            vd,
            left,
            right,
        );
    }

    pub fn vpmaxsd_rrr(&mut self, right: u8, left: u8, vd: u8) {
        self.formatter.vex_nds_lig_three_byte_op(
            PRE_SSE_66,
            VexImpliedBytes::ThreeBytesOp38,
            VexW::W0,
            OP3_PMAXSD_VdqWdq,
            vd,
            left,
            right,
        );
    }

    pub fn vpmaxub_rrr(&mut self, right: u8, left: u8, vd: u8) {
        self.formatter.vex_nds_lig_two_byte_op_rr(
            PRE_SSE_66,
            VexW::W0,
            OP2_PMAXUB_VdqWdq,
            vd,
            left,
            right,
        );
    }

    pub fn vpmaxuw_rrr(&mut self, right: u8, left: u8, vd: u8) {
        self.formatter.vex_nds_lig_three_byte_op(
            PRE_SSE_66,
            VexImpliedBytes::ThreeBytesOp38,
            VexW::W0,
            OP3_PMAXUW_VdqWdq,
            vd,
            left,
            right,
        );
    }

    pub fn vpmaxud_rrr(&mut self, right: u8, left: u8, vd: u8) {
        self.formatter.vex_nds_lig_three_byte_op(
            PRE_SSE_66,
            VexImpliedBytes::ThreeBytesOp38,
            VexW::W1,
            OP3_PMAXUD_VdqWdq,
            vd,
            left,
            right,
        );
    }

    pub fn vpminsb_rrr(&mut self, right: u8, left: u8, vd: u8) {
        self.formatter.vex_nds_lig_three_byte_op(
            PRE_SSE_66,
            VexImpliedBytes::ThreeBytesOp38,
            VexW::W0,
            OP3_PMINSB_VdqWdq,
            vd,
            left,
            right,
        );
    }

    pub fn vpminsw_rrr(&mut self, right: u8, left: u8, vd: u8) {
        self.formatter.vex_nds_lig_two_byte_op_rr(
            PRE_SSE_66,
            VexW::W0,
            OP2_PMINSW_VdqWdq,
            vd,
            left,
            right,
        );
    }

    pub fn vpminsd_rrr(&mut self, right: u8, left: u8, vd: u8) {
        self.formatter.vex_nds_lig_three_byte_op(
            PRE_SSE_66,
            VexImpliedBytes::ThreeBytesOp38,
            VexW::W0,
            OP3_PMINSD_VdqWdq,
            vd,
            left,
            right,
        );
    }

    pub fn vpminub_rrr(&mut self, right: u8, left: u8, vd: u8) {
        self.formatter.vex_nds_lig_two_byte_op_rr(
            PRE_SSE_66,
            VexW::W0,
            OP2_PMINUB_VdqWdq,
            vd,
            left,
            right,
        );
    }

    pub fn vpminuw_rrr(&mut self, right: u8, left: u8, vd: u8) {
        self.formatter.vex_nds_lig_three_byte_op(
            PRE_SSE_66,
            VexImpliedBytes::ThreeBytesOp38,
            VexW::W0,
            OP3_PMINUW_VdqWdq,
            vd,
            left,
            right,
        );
    }

    pub fn vpminud_rrr(&mut self, right: u8, left: u8, vd: u8) {
        self.formatter.vex_nds_lig_three_byte_op(
            PRE_SSE_66,
            VexImpliedBytes::ThreeBytesOp38,
            VexW::W1,
            OP3_PMINUD_VdqWdq,
            vd,
            left,
            right,
        );
    }

    pub fn vmaxps_rrr(&mut self, xmm_3: u8, xmm_2: u8, xmm_1: u8) {
        self.formatter.vex_nds_lig_wig_two_byte_op(
            PRE_SSE_66,
            OP2_MAXPS_VpsWps,
            xmm_1,
            xmm_2,
            xmm_3,
        );
    }

    pub fn vmaxpd_rrr(&mut self, xmm_3: u8, xmm_2: u8, xmm_1: u8) {
        self.formatter.vex_nds_lig_wig_two_byte_op(
            PRE_SSE_66,
            OP2_MAXPD_VpdWpd,
            xmm_1,
            xmm_2,
            xmm_3,
        );
    }

    pub fn vminps_rrr(&mut self, xmm_3: u8, xmm_2: u8, xmm_1: u8) {
        self.formatter.vex_nds_lig_wig_two_byte_op(
            PRE_SSE_66,
            OP2_MINPS_VpsWps,
            xmm_1,
            xmm_2,
            xmm_3,
        );
    }

    pub fn vminpd_rrr(&mut self, xmm_3: u8, xmm_2: u8, xmm_1: u8) {
        self.formatter.vex_nds_lig_wig_two_byte_op(
            PRE_SSE_66,
            OP2_MINPD_VpdWpd,
            xmm_1,
            xmm_2,
            xmm_3,
        );
    }

    pub fn vpavgb_rrr(&mut self, right: u8, left: u8, vd: u8) {
        self.formatter
            .vex_nds_lig_wig_two_byte_op(PRE_SSE_66, OP2_PAVGB_VdqWdq, vd, left, right);
    }

    pub fn vpavgw_rrr(&mut self, right: u8, left: u8, vd: u8) {
        self.formatter
            .vex_nds_lig_wig_two_byte_op(PRE_SSE_66, OP2_PAVGW_VdqWdq, vd, left, right);
    }

    pub fn vpabsb_rr(&mut self, vn: u8, vd: u8) {
        self.formatter.vex_nds_lig_three_byte_op(
            PRE_SSE_66,
            VexImpliedBytes::ThreeBytesOp38,
            VexW::W0,
            OP3_PABSB_VdqWdq,
            vd,
            0,
            vn,
        );
    }

    pub fn vpabsw_rr(&mut self, vn: u8, vd: u8) {
        self.formatter.vex_nds_lig_three_byte_op(
            PRE_SSE_66,
            VexImpliedBytes::ThreeBytesOp38,
            VexW::W0,
            OP3_PABSW_VdqWdq,
            vd,
            0,
            vn,
        );
    }

    pub fn vpabsd_rr(&mut self, vn: u8, vd: u8) {
        self.formatter.vex_nds_lig_three_byte_op(
            PRE_SSE_66,
            VexImpliedBytes::ThreeBytesOp38,
            VexW::W0,
            OP3_PABSD_VdqWdq,
            vd,
            0,
            vn,
        );
    }

    pub fn vpxor_rrr(&mut self, right: u8, left: u8, vd: u8) {
        self.formatter
            .vex_nds_lig_wig_two_byte_op(PRE_SSE_66, OP2_PXOR_VdqWdq, vd, left, right);
    }

    pub fn vpsubq_rrr(&mut self, right: u8, left: u8, vd: u8) {
        self.formatter
            .vex_nds_lig_wig_two_byte_op(PRE_SSE_66, OP2_PSUBQ_VdqWdq, vd, left, right);
    }

    pub fn vblendvpd_rrrr(&mut self, xmm_4: u8, xmm_3: u8, xmm_2: u8, xmm_1: u8) {
        self.formatter.vex_nds_lig_three_byte_op(
            PRE_SSE_66,
            VexImpliedBytes::ThreeBytesOp3A,
            VexW::W0,
            OP3_BLENDVPD_VpdWpdXMM0,
            xmm_1,
            xmm_2,
            xmm_3,
        );
        self.formatter.immediate8((xmm_4 as i32) << 4);
    }

    pub fn vpmulhrsw_rrr(&mut self, xmm_3: u8, xmm_2: u8, xmm_1: u8) {
        self.formatter.vex_nds_lig_three_byte_op(
            PRE_SSE_66,
            VexImpliedBytes::ThreeBytesOp38,
            VexW::W0,
            OP3_ROUNDSD_VsdWsdIb,
            xmm_1,
            xmm_2,
            xmm_3,
        );
    }

    pub fn vaddps_rrr(&mut self, left: u8, right: u8, dest: u8) {
        self.formatter
            .vex_nds_lig_wig_two_byte_op(PRE_SSE_66, OP2_ADDPS_VpsWps, dest, left, right);
    }

    pub fn vaddpd_rrr(&mut self, left: u8, right: u8, dest: u8) {
        self.formatter
            .vex_nds_lig_wig_two_byte_op(PRE_SSE_66, OP2_ADDPD_VpdWpd, dest, left, right);
    }

    pub fn vpaddb_rrr(&mut self, left: u8, right: u8, dest: u8) {
        self.formatter.vex_nds_lig_wig_commutative_two_byte_op(
            PRE_SSE_66,
            OP2_PADDB_VdqWdq,
            dest,
            left,
            right,
        );
    }

    pub fn vpaddw_rrr(&mut self, left: u8, right: u8, dest: u8) {
        self.formatter.vex_nds_lig_wig_commutative_two_byte_op(
            PRE_SSE_66,
            OP2_PADDW_VdqWdq,
            dest,
            left,
            right,
        );
    }

    pub fn vpaddd_rrr(&mut self, left: u8, right: u8, dest: u8) {
        self.formatter.vex_nds_lig_wig_commutative_two_byte_op(
            PRE_SSE_66,
            OP2_PADDD_VdqWdq,
            dest,
            left,
            right,
        );
    }

    pub fn vpaddq_rrr(&mut self, left: u8, right: u8, dest: u8) {
        self.formatter.vex_nds_lig_wig_commutative_two_byte_op(
            PRE_SSE_66,
            OP2_PADDQ_VdqWdq,
            dest,
            left,
            right,
        );
    }

    pub fn vsubps_mrr(&mut self, offset: i32, base: u8, left: u8, dest: u8) {
        self.formatter.vex_nds_lig_wig_two_byte_op_mem(
            PRE_SSE_00,
            OP2_SUBPS_VpsWps,
            dest,
            left,
            base,
            offset,
        );
    }

    pub fn vsubps_rrr(&mut self, left: u8, right: u8, dest: u8) {
        self.formatter
            .vex_nds_lig_wig_two_byte_op(PRE_SSE_66, OP2_SUBPS_VpsWps, dest, left, right);
    }

    pub fn vsubpd_rrr(&mut self, left: u8, right: u8, dest: u8) {
        self.formatter
            .vex_nds_lig_wig_two_byte_op(PRE_SSE_66, OP2_SUBPD_VpdWpd, dest, left, right);
    }

    pub fn vpsubb_rrr(&mut self, left: u8, right: u8, dest: u8) {
        self.formatter
            .vex_nds_lig_wig_two_byte_op(PRE_SSE_66, OP2_PSUBB_VdqWdq, dest, left, right);
    }

    pub fn vpsubw_rrr(&mut self, left: u8, right: u8, dest: u8) {
        self.formatter
            .vex_nds_lig_wig_two_byte_op(PRE_SSE_66, OP2_PSUBW_VdqWdq, dest, left, right);
    }

    pub fn vpsubd_rrr(&mut self, left: u8, right: u8, dest: u8) {
        self.formatter
            .vex_nds_lig_wig_two_byte_op(PRE_SSE_66, OP2_PSUBD_VdqWdq, dest, left, right);
    }

    pub fn vmulps_rrr(&mut self, left: u8, right: u8, dest: u8) {
        self.formatter.vex_nds_lig_wig_commutative_two_byte_op(
            PRE_SSE_00,
            OP2_MULPS_VpsWps,
            dest,
            left,
            right,
        );
    }

    pub fn vmulpd_rrr(&mut self, left: u8, right: u8, dest: u8) {
        self.formatter.vex_nds_lig_wig_commutative_two_byte_op(
            PRE_SSE_66,
            OP2_MULPD_VpdWpd,
            dest,
            left,
            right,
        );
    }

    pub fn vpmullw_rrr(&mut self, left: u8, right: u8, dest: u8) {
        self.formatter.vex_nds_lig_wig_commutative_two_byte_op(
            PRE_SSE_66,
            OP2_PMULLW_VdqWdq,
            dest,
            left,
            right,
        );
    }

    pub fn vpmulld_rrr(&mut self, left: u8, right: u8, dest: u8) {
        self.formatter.vex_nds_lig_three_byte_op(
            PRE_SSE_66,
            VexImpliedBytes::ThreeBytesOp38,
            VexW::W0,
            OP3_PMULLD_VdqWdq,
            dest,
            left,
            right,
        );
    }

    pub fn vdivps_rrr(&mut self, left: u8, right: u8, dest: u8) {
        self.formatter
            .vex_nds_lig_wig_two_byte_op(PRE_SSE_00, OP2_DIVPS_VpsWps, dest, left, right);
    }

    pub fn vdivpd_rrr(&mut self, left: u8, right: u8, dest: u8) {
        self.formatter
            .vex_nds_lig_wig_two_byte_op(PRE_SSE_66, OP2_DIVPD_VpdWpd, dest, left, right);
    }

    pub fn vdivsd_rrr(&mut self, left: u8, right: u8, dest: u8) {
        self.formatter
            .vex_nds_lig_wig_two_byte_op(PRE_SSE_F2, OP2_DIVSD_VsdWsd, dest, left, right);
    }

    pub fn vdivsd_mrr(&mut self, offset: i32, base: u8, left: u8, dest: u8) {
        self.formatter.vex_nds_lig_wig_two_byte_op_mem(
            PRE_SSE_F2,
            OP2_DIVSD_VsdWsd,
            dest,
            left,
            base,
            offset,
        );
    }

    pub fn vdivss_rrr(&mut self, left: u8, right: u8, dest: u8) {
        self.formatter
            .vex_nds_lig_wig_two_byte_op(PRE_SSE_F3, OP2_DIVSS_VpsWps, dest, left, right);
    }

    pub fn vdivss_mrr(&mut self, offset: i32, base: u8, left: u8, dest: u8) {
        self.formatter.vex_nds_lig_wig_two_byte_op_mem(
            PRE_SSE_F3,
            OP2_DIVSS_VpsWps,
            dest,
            left,
            base,
            offset,
        );
    }

    pub fn vroundsd_i8rrr(&mut self, rounding: RoundingType, src1: u8, src2: u8, dest: u8) {
        self.formatter.vex_nds_lig_three_byte_op(
            PRE_SSE_66,
            VexImpliedBytes::ThreeBytesOp3A,
            VexW::W0,
            OP3_ROUNDSD_VsdWsdIb,
            dest,
            src2,
            src1,
        );
        self.formatter.immediate8(rounding as i32);
    }

    pub fn vroundsd_i8mrr(
        &mut self,
        rounding: RoundingType,
        offset: i32,
        base: u8,
        src: u8,
        dest: u8,
    ) {
        self.formatter.vex_nds_lig_three_byte_op_mem(
            PRE_SSE_66,
            VexImpliedBytes::ThreeBytesOp3A,
            VexW::W0,
            OP3_ROUNDSD_VsdWsdIb,
            dest,
            src,
            base,
            offset,
        );
        self.formatter.immediate8(rounding as i32);
    }

    pub fn vroundss_i8rrr(&mut self, rounding: RoundingType, src1: u8, src2: u8, dest: u8) {
        self.formatter.vex_nds_lig_three_byte_op(
            PRE_SSE_66,
            VexImpliedBytes::ThreeBytesOp3A,
            VexW::W0,
            OP3_ROUNDSS_VssWssIb,
            dest,
            src2,
            src1,
        );
        self.formatter.immediate8(rounding as i32);
    }

    pub fn vroundss_i8mrr(
        &mut self,
        rounding: RoundingType,
        offset: i32,
        base: u8,
        src: u8,
        dest: u8,
    ) {
        self.formatter.vex_nds_lig_three_byte_op_mem(
            PRE_SSE_66,
            VexImpliedBytes::ThreeBytesOp3A,
            VexW::W0,
            OP3_ROUNDSS_VssWssIb,
            dest,
            src,
            base,
            offset,
        );
        self.formatter.immediate8(rounding as i32);
    }

    pub fn label_for_watchpoint(&mut self) -> AssemblerLabel {
        let mut result = self.formatter.label();

        if result.offset() != self.index_of_tail_of_last_watchpoint as u32 {
            result = self.formatter.label();
        }

        self.index_of_last_watchpoint = result.offset() as _;
        self.index_of_tail_of_last_watchpoint =
            result.offset() as usize + Self::max_jump_replacement_size();

        result
    }

    pub fn label_ignoring_watchpoints(&mut self) -> AssemblerLabel {
        self.formatter.label()
    }

    pub fn label(&mut self) -> AssemblerLabel {
        let mut result = self.formatter.label();

        while result.offset() < self.index_of_tail_of_last_watchpoint as u32 {
            self.nop();
            result = self.formatter.label();
        }

        result
    }

    pub fn align(&mut self, alignment: usize) -> AssemblerLabel {
        while !self.formatter.is_aligned(alignment) {
            self.formatter.one_byte_op(OP_HLT);
        }

        self.formatter.label()
    }

    // Linking & patching:
    //
    // 'link' and 'patch' methods are for use on unprotected code - such as the code
    // within the AssemblerBuffer, and code being patched by the patch buffer.  Once
    // code has been finalized it is (platform support permitting) within a non-
    // writable region of memory; to modify the code in an execute-only execuable
    // pool the 'repatch' and 'relink' methods should be used.
    unsafe fn set_pointer(where_: *mut u8, value: *mut u8) {
        where_.cast::<*mut u8>().sub(1).write_unaligned(value);
    }

    unsafe fn set_int32(where_: *mut u8, value: i32) {
        where_.cast::<i32>().sub(1).write_unaligned(value);
    }

    unsafe fn set_rel32(from: *mut u8, to: *mut u8) {
        let offset = to.offset_from(from) as i32;
        Self::set_int32(from, offset);
    }

    pub fn nop(&mut self) {
        self.formatter.one_byte_op(OP_NOP);
    }

    pub fn debug_offset(&self) -> usize {
        self.formatter.debug_offset()
    }

    pub unsafe fn replace_with_jump(instruction_start: *mut u8, to: *mut u8) {
        let ptr = instruction_start;
        let dst_ptr = to;
        let distance = dst_ptr as isize - (ptr as isize + 5);

        ptr.write_unaligned(OP_JMP_rel32);
        ptr.add(1).cast::<i32>().write_unaligned(distance as i32);
    }

    pub unsafe fn replace_with_hlt(instruction_start: *mut u8) {
        let ptr = instruction_start;
        ptr.write_unaligned(OP_HLT);
    }

    pub unsafe fn read_pointer(instruction_start: *mut u8) -> *mut u8 {
        let ptr = instruction_start;
        ptr.cast::<*mut u8>().read_unaligned()
    }

    pub unsafe fn repatch_pointer(where_: *mut u8, to: *mut u8) {
        Self::set_pointer(where_, to)
    }

    pub unsafe fn repatch_int32(where_: *mut u8, value: i32) {
        Self::set_int32(where_, value)
    }

    pub unsafe fn relink_jump(from: *mut u8, to: *mut u8) {
        Self::set_rel32(from, to)
    }

    pub unsafe fn relink_tail_call(from: *mut u8, to: *mut u8) {
        Self::set_rel32(from, to)
    }

    pub unsafe fn relink_call(from: *mut u8, to: *mut u8) {
        Self::set_rel32(from, to)
    }

    pub unsafe fn link_pointer(code: *mut u8, where_: AssemblerLabel, to: *mut u8) {
        Self::set_pointer(code.add(where_.offset() as _), to)
    }

    pub unsafe fn link_call(code: *mut u8, where_: AssemblerLabel, to: *mut u8) {
        Self::set_rel32(code.add(where_.offset() as _), to)
    }

    pub unsafe fn link_tail_call(code: *mut u8, where_: AssemblerLabel, to: *mut u8) {
        Self::set_rel32(code.add(where_.offset() as _), to)
    }

    pub unsafe fn link_jump_(code: *mut u8, where_: AssemblerLabel, to: *mut u8) {
        Self::set_rel32(code.add(where_.offset() as _), to)
    }

    pub fn link_jump(&mut self, from: AssemblerLabel, to: AssemblerLabel) {
        let code = self.formatter.data_mut().as_mut_ptr();

        unsafe {
            Self::set_rel32(code.add(from.offset() as _), code.add(to.offset() as _));
        }
    }

    pub unsafe fn revert_jump_to_cmpl_im_force32(
        instruction_start: *mut u8,
        imm: i32,
        _offset: i32,
        dst: u8,
    ) {
        let opcode_bytes = 1;
        let modrm_bytes = 1;

        let ptr = instruction_start;

        ptr.write(OP_GROUP11_EvIz);
        ptr.add(1)
            .write((MOD_RM_MEM_NO_DISP << 6) | (GROUP1_OP_CMP << 3) | dst);

        let as_bytes = imm.to_ne_bytes();

        for i in opcode_bytes + modrm_bytes..Self::max_jump_replacement_size() {
            ptr.add(i).write(as_bytes[i - opcode_bytes - modrm_bytes]);
        }
    }

    pub unsafe fn revert_jump_to_cmpl_ir_force32(instruction_start: *mut u8, imm: i32, dst: u8) {
        let opcode_bytes = 1;
        let modrm_bytes = 1;

        let ptr = instruction_start;

        ptr.write(OP_GROUP11_EvIz);
        ptr.add(1)
            .write((MOD_RM_REG << 6) | (GROUP1_OP_CMP << 3) | dst);

        let as_bytes = imm.to_ne_bytes();

        for i in opcode_bytes + modrm_bytes..Self::max_jump_replacement_size() {
            ptr.add(i).write(as_bytes[i - opcode_bytes - modrm_bytes]);
        }
    }

    pub unsafe fn revert_jump_to_movq_i64r(instruction_start: *mut u8, imm: i64, dst: u8) {
        let instruction_size = 10;
        let rex_bytes = 1;
        let opcode_bytes = 1;

        let ptr = instruction_start;

        ptr.write(PRE_REX | (1 << 3) | (dst >> 3));
        ptr.add(1).write(OP_MOV_EAXIv | (dst & 7));

        let as_bytes = imm.to_ne_bytes();

        for i in rex_bytes + opcode_bytes..instruction_size {
            ptr.add(i).write(as_bytes[i - rex_bytes - opcode_bytes]);
        }
    }

    pub unsafe fn revert_jump_to_movq_i32r(instruction_start: *mut u8, imm: i64, dst: u8) {
        let instruction_size = 6;
        let rex_bytes = 1;
        let opcode_bytes = 1;

        let ptr = instruction_start;

        ptr.write(PRE_REX | (dst >> 3));
        ptr.add(1).write(OP_MOV_EAXIv | (dst & 7));

        let as_bytes = imm.to_ne_bytes();

        for i in rex_bytes + opcode_bytes..instruction_size {
            ptr.add(i).write(as_bytes[i - rex_bytes - opcode_bytes]);
        }
    }

    #[allow(unused_mut)]
    pub unsafe fn fill_nops(base: *mut u8, mut size: usize) {
        #[cfg(target_pointer_width = "64")]
        {
            const NOPS: [&'static [u8]; 10] = [
                &[0x90],
                // xchg %ax,%ax
                &[0x66, 0x90],
                // nopl (%[re]ax)
                &[0x0f, 0x1f, 0x00],
                // nopl 8(%[re]ax)
                &[0x0f, 0x1f, 0x40, 0x08],
                // nopl 8(%[re]ax,%[re]ax,1)
                &[0x0f, 0x1f, 0x44, 0x00, 0x08],
                // nopw 8(%[re]ax,%[re]ax,1)
                &[0x66, 0x0f, 0x1f, 0x44, 0x00, 0x08],
                // nopl 512(%[re]ax)
                &[0x0f, 0x1f, 0x80, 0x00, 0x02, 0x00, 0x00],
                // nopl 512(%[re]ax,%[re]ax,1)
                &[0x0f, 0x1f, 0x84, 0x00, 0x00, 0x02, 0x00, 0x00],
                // nopw 512(%[re]ax,%[re]ax,1)
                &[0x66, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x02, 0x00, 0x00],
                // nopw %cs:512(%[re]ax,%[re]ax,1)
                &[0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x02, 0x00, 0x00],
            ];

            let mut ptr = base;

            while size != 0 {
                let nop_size = size.min(15);
                let num_prefixes = if nop_size <= 10 { 0 } else { nop_size - 10 };

                for _ in 0..=num_prefixes {
                    ptr.write(0x66);
                    ptr = ptr.add(1);
                }

                let nop_rest = nop_size - num_prefixes;

                for i in 0..nop_rest {
                    ptr.write(NOPS[nop_rest - 1][i]);
                    ptr = ptr.add(1);
                }

                size -= nop_size;
            }
        }

        #[cfg(target_pointer_width = "32")]
        {
            std::ptr::write_bytes(base, OP_NOP, size);
        }
    }

    pub fn get_difference_between_labels(a: AssemblerLabel, b: AssemblerLabel) -> i32 {
        let a = a.offset() as i32;
        let b = b.offset() as i32;
        a - b
    }

    pub fn get_relocate_address(code: *mut u8, label: AssemblerLabel) -> *mut u8 {
        unsafe { code.add(label.offset() as _) }
    }

    pub fn get_call_return_offset(call: AssemblerLabel) -> usize {
        call.offset as _
    }

    pub unsafe fn replace_with_address_computation(mut instruction_start: *mut u8) {
        #[cfg(target_pointer_width = "64")]
        if (instruction_start.read() & !15) == PRE_REX {
            instruction_start = instruction_start.add(1);
        }

        match instruction_start.read() {
            OP_MOV_GvEv => {
                instruction_start.write(OP_LEA);
            }
            OP_LEA => {}
            _ => unreachable!(),
        }
    }

    pub unsafe fn replace_with_load(mut instruction_start: *mut u8) {
        #[cfg(target_pointer_width = "64")]
        if (instruction_start.read() & !15) == PRE_REX {
            instruction_start = instruction_start.add(1);
        }

        match instruction_start.read() {
            OP_MOV_GvEv => {}
            OP_LEA => {
                instruction_start.write(OP_MOV_GvEv);
            }
            _ => unreachable!(),
        }
    }

    pub fn max_jump_replacement_size() -> usize {
        5
    }

    pub fn patchable_jump_size() -> usize {
        5
    }
}

pub struct X86InstructionFormatter {
    pub buffer: AssemblerBuffer,
}

const fn can_sign_extend_8_32(value: i32) -> bool {
    value == value as i8 as i32
}

pub const MOD_RM_MEM_NO_DISP: u8 = 0;
pub const MOD_RM_MEM_DISP8: u8 = 1 << 6;
pub const MOD_RM_MEM_DISP32: u8 = 2 << 6;
pub const MOD_RM_REG: u8 = 3 << 6;

impl X86InstructionFormatter {
    pub const MAX_INSTRUCTION_SIZE: usize = 16;

    pub fn new() -> Self {
        Self {
            buffer: AssemblerBuffer::new(),
        }
    }

    pub fn prefix(&mut self, pre: u8) {
        self.buffer.put_byte(pre as _);
    }

    pub fn byte_reg_requires_rex(reg: u8) -> bool {
        reg >= esp
    }

    pub fn byte_reg_requires_rex_2(reg: u8, reg2: u8) -> bool {
        Self::byte_reg_requires_rex(reg | reg2)
    }

    pub fn reg_requires_rex(reg: u8) -> bool {
        reg >= r8
    }

    pub fn reg_requires_rex_2(reg: u8, reg2: u8) -> bool {
        Self::reg_requires_rex(reg | reg2)
    }

    pub fn reg_requires_rex_3(reg: u8, reg2: u8, reg3: u8) -> bool {
        Self::reg_requires_rex(reg | reg2 | reg3)
    }

    pub fn one_byte_op(&mut self, op: u8) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);
        writer.put_byte_unchecked(op as i8);
    }

    pub fn one_byte_op_r(&mut self, op: u8, reg: u8) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);
        writer.emit_rex_if_needed(0, 0, reg);
        writer.put_byte_unchecked((op + (reg & 7)) as i8);
    }

    pub fn one_byte_op_rm(&mut self, op: u8, reg: u8, rm: u8) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);
        writer.emit_rex_if_needed(reg, 0, rm);
        writer.put_byte_unchecked(op as _);
        writer.register_modrm(reg, rm);
    }

    pub fn one_byte_op_mem(&mut self, op: u8, reg: u8, base: u8, offset: i32) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);
        writer.emit_rex_if_needed(reg, 0, base);
        writer.put_byte_unchecked(op as _);
        writer.memory_modrm(reg, base, offset);
    }

    pub fn one_byte_op_mem_disp32(&mut self, op: u8, reg: u8, base: u8, offset: i32) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);
        writer.emit_rex_if_needed(reg, 0, base);
        writer.put_byte_unchecked(op as _);
        writer.memory_modrm_disp32(reg, base, offset);
    }

    pub fn one_byte_op_mem_disp8(&mut self, op: u8, reg: u8, base: u8, offset: i32) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);
        writer.emit_rex_if_needed(reg, 0, base);
        writer.put_byte_unchecked(op as _);
        writer.memory_modrm_disp8(reg, base, offset);
    }

    pub fn one_byte_op_mem_scaled(
        &mut self,
        op: u8,
        reg: u8,
        base: u8,
        index: u8,
        scale: u8,
        offset: i32,
    ) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);
        writer.emit_rex_if_needed(reg, index, base);
        writer.put_byte_unchecked(op as _);
        writer.memory_modrm_scaled(reg, base, index, scale, offset);
    }

    pub fn one_byte_op_addr(&mut self, op: u8, reg: u8, addr: u32) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);
        writer.put_byte_unchecked(op as _);
        writer.memory_modrm_addr(reg, addr);
    }

    pub fn two_byte_op(&mut self, op: u8) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);
        writer.put_byte_unchecked(OP_2BYTE_ESCAPE as _);
        writer.put_byte_unchecked(op as _);
    }

    pub fn two_byte_op_r(&mut self, op: u8, reg: u8) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);
        writer.emit_rex_if_needed(0, 0, reg);
        writer.put_byte_unchecked(OP_2BYTE_ESCAPE as _);
        writer.put_byte_unchecked((op + (reg & 7)) as i8);
    }

    pub fn two_byte_op_rm(&mut self, op: u8, reg: u8, rm: u8) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);
        writer.emit_rex_if_needed(reg, 0, rm);
        writer.put_byte_unchecked(OP_2BYTE_ESCAPE as _);
        writer.put_byte_unchecked(op as _);
        writer.register_modrm(reg, rm);
    }

    pub fn two_byte_op_mem(&mut self, op: u8, reg: u8, base: u8, offset: i32) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);
        writer.emit_rex_if_needed(reg, 0, base);
        writer.put_byte_unchecked(OP_2BYTE_ESCAPE as _);
        writer.put_byte_unchecked(op as _);
        writer.memory_modrm(reg, base, offset);
    }

    pub fn two_byte_op_mem_scaled(
        &mut self,
        op: u8,
        reg: u8,
        base: u8,
        index: u8,
        scale: u8,
        offset: i32,
    ) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);
        writer.emit_rex_if_needed(reg, index, base);
        writer.put_byte_unchecked(OP_2BYTE_ESCAPE as _);
        writer.put_byte_unchecked(op as _);
        writer.memory_modrm_scaled(reg, base, index, scale, offset);
    }

    pub fn two_byte_op_addr(&mut self, op: u8, reg: u8, addr: u32) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);
        writer.put_byte_unchecked(OP_2BYTE_ESCAPE as _);
        writer.put_byte_unchecked(op as _);
        writer.memory_modrm_addr(reg, addr);
    }

    pub fn vex_nds_lig_wig_two_byte_op(&mut self, simd_prefix: u8, op: u8, dest: u8, a: u8, b: u8) {
        self.vex_nds_lig_two_byte_op_rr(simd_prefix, VexW::W0, op, dest, a, b)
    }

    pub fn vex_nds_lig_wig_two_byte_op_mem(
        &mut self,
        simd_prefix: u8,
        op: u8,
        dest: u8,
        a: u8,
        b: u8,
        offset: i32,
    ) {
        self.vex_nds_lig_two_byte_op_mem(simd_prefix, VexW::W0, op, dest, a, b, offset)
    }

    pub fn vex_nds_lig_wig_two_byte_op_mem_scaled(
        &mut self,
        simd_prefix: u8,
        op: u8,
        dest: u8,
        a: u8,
        offset: i32,
        base: u8,
        index: u8,
        scale: u8,
    ) {
        self.vex_nds_lig_two_byte_op_mem_scaled(
            simd_prefix,
            VexW::W0,
            op,
            dest,
            a,
            base,
            index,
            scale,
            offset,
        )
    }

    pub fn vex_nds_lig_two_byte_op(&mut self, simd_prefix: u8, vexw: VexW, op: u8) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);

        if vexw == VexW::W1 {
            writer.three_bytes_vex_nds2(simd_prefix, VexImpliedBytes::TwoBytes, vexw, 0, 0, 0);
        } else {
            writer.two_bytes_vex(simd_prefix, 0, 0);
        }

        writer.put_byte_unchecked(op as _);
    }

    pub fn vex_nds_lig_two_byte_op_rr(
        &mut self,
        simd_prefix: u8,
        vexw: VexW,
        op: u8,
        dest: u8,
        a: u8,
        b: u8,
    ) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);

        if Self::reg_requires_rex(b) || vexw == VexW::W1 {
            writer.three_bytes_vex_nds2(simd_prefix, VexImpliedBytes::TwoBytes, vexw, dest, a, b);
        } else {
            writer.two_bytes_vex(simd_prefix, a, dest);
        }

        writer.put_byte_unchecked(op as _);
        writer.register_modrm(dest, b);
    }

    pub fn vex_nds_lig_two_byte_op_mem(
        &mut self,
        simd_prefix: u8,
        vexw: VexW,
        op: u8,
        dest: u8,
        a: u8,
        base: u8,
        offset: i32,
    ) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);

        if Self::reg_requires_rex(base) || vexw == VexW::W1 {
            writer.three_bytes_vex_nds2(
                simd_prefix,
                VexImpliedBytes::TwoBytes,
                vexw,
                dest,
                a,
                base,
            );
        } else {
            writer.two_bytes_vex(simd_prefix, a, dest);
        }

        writer.put_byte_unchecked(op as _);
        writer.memory_modrm(dest, base, offset);
    }

    pub fn vex_nds_lig_two_byte_op_mem_scaled(
        &mut self,
        simd_prefix: u8,
        vexw: VexW,
        op: u8,
        dest: u8,
        a: u8,
        base: u8,
        index: u8,
        scale: u8,
        offset: i32,
    ) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);

        if Self::reg_requires_rex(base) || Self::reg_requires_rex(index) || vexw == VexW::W1 {
            writer.three_bytes_vex_nds2(
                simd_prefix,
                VexImpliedBytes::TwoBytes,
                vexw,
                dest,
                a,
                base,
            );
        } else {
            writer.two_bytes_vex(simd_prefix, a, dest);
        }

        writer.put_byte_unchecked(op as _);
        writer.memory_modrm_scaled(dest, base, index, scale, offset);
    }

    pub fn vex_nds_lig_wig_commutative_two_byte_op(
        &mut self,
        simd_prefix: u8,
        op: u8,
        dest: u8,
        mut a: u8,
        mut b: u8,
    ) {
        if Self::reg_requires_rex(b) {
            std::mem::swap(&mut a, &mut b);
        }

        self.vex_nds_lig_two_byte_op_rr(simd_prefix, VexW::W0, op, dest, a, b)
    }

    pub fn vex_nds_lig_three_byte_op(
        &mut self,
        simd_prefix: u8,
        implied_bytes: VexImpliedBytes,
        vexw: VexW,
        op: u8,
        dest: u8,
        a: u8,
        b: u8,
    ) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);

        writer.three_bytes_vex_maybe_256(
            false,
            simd_prefix,
            implied_bytes,
            vexw == VexW::W1,
            dest,
            0,
            a,
            b,
        );
        writer.put_byte_unchecked(op as _);
        writer.register_modrm(dest, b);
    }

    pub fn vex_nds_lig_three_byte_op_mem(
        &mut self,
        simd_prefix: u8,
        implied_bytes: VexImpliedBytes,
        vexw: VexW,
        op: u8,
        dest: u8,
        a: u8,
        base: u8,
        offset: i32,
    ) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);

        writer.three_bytes_vex_maybe_256(
            false,
            simd_prefix,
            implied_bytes,
            vexw == VexW::W1,
            dest,
            0,
            a,
            base,
        );
        writer.put_byte_unchecked(op as _);
        writer.memory_modrm(dest, base, offset);
    }

    pub fn three_byte_op(&mut self, two_byte_prefix: u8, op: u8) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);

        writer.put_byte_unchecked(OP_2BYTE_ESCAPE as _);
        writer.put_byte_unchecked(two_byte_prefix as _);
        writer.put_byte_unchecked(op as _);
    }

    pub fn three_byte_op_rm(&mut self, two_byte_prefix: u8, op: u8, dest: u8, src: u8) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);

        writer.put_byte_unchecked(OP_2BYTE_ESCAPE as _);
        writer.put_byte_unchecked(two_byte_prefix as _);
        writer.put_byte_unchecked(op as _);
        writer.register_modrm(dest, src);
    }

    pub fn three_byte_op_mem(
        &mut self,
        two_byte_prefix: u8,
        op: u8,
        dest: u8,
        base: u8,
        offset: i32,
    ) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);

        writer.put_byte_unchecked(OP_2BYTE_ESCAPE as _);
        writer.put_byte_unchecked(two_byte_prefix as _);
        writer.put_byte_unchecked(op as _);
        writer.memory_modrm(dest, base, offset);
    }

    // Quad-word-sized operands:
    //
    // Used to format 64-bit operantions, planting a REX.w prefix.
    // When planting d64 or f64 instructions, not requiring a REX.w prefix,
    // the normal (non-'64'-postfixed) formatters should be used.

    pub fn one_byte_op64(&mut self, op: u8) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);
        writer.emit_rexw(0, 0, 0);
        writer.put_byte_unchecked(op as _);
    }

    pub fn one_byte_op64_r(&mut self, op: u8, reg: u8) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);
        writer.emit_rexw(0, 0, reg);
        writer.put_byte_unchecked((op + (reg & 7)) as _);
    }

    pub fn one_byte_op64_rm(&mut self, op: u8, reg: u8, rm: u8) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);
        writer.emit_rexw(reg, 0, rm);
        writer.put_byte_unchecked(op as _);
        writer.register_modrm(reg, rm);
    }

    pub fn one_byte_op64_mem(&mut self, op: u8, reg: u8, base: u8, offset: i32) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);
        writer.emit_rexw(reg, 0, base);
        writer.put_byte_unchecked(op as _);
        writer.memory_modrm(reg, base, offset);
    }

    pub fn one_byte_op64_mem_disp32(&mut self, op: u8, reg: u8, base: u8, offset: i32) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);
        writer.emit_rexw(reg, 0, base);
        writer.put_byte_unchecked(op as _);
        writer.memory_modrm_disp32(reg, base, offset);
    }

    pub fn one_byte_op64_mem_disp8(&mut self, op: u8, reg: u8, base: u8, offset: i32) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);
        writer.emit_rexw(reg, 0, base);
        writer.put_byte_unchecked(op as _);
        writer.memory_modrm_disp8(reg, base, offset);
    }

    pub fn one_byte_op64_mem_scaled(
        &mut self,
        op: u8,
        reg: u8,
        base: u8,
        index: u8,
        scale: u8,
        offset: i32,
    ) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);
        writer.emit_rexw(reg, index, base);
        writer.put_byte_unchecked(op as _);
        writer.memory_modrm_scaled(reg, base, index, scale, offset);
    }

    pub fn one_byte_op64_addr(&mut self, op: u8, reg: u8, addr: u32) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);
        writer.emit_rexw(reg, 0, 0);
        writer.put_byte_unchecked(op as _);
        writer.memory_modrm_addr(reg, addr);
    }

    pub fn two_byte_op64_r(&mut self, op: u8, reg: u8) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);
        writer.emit_rexw(0, 0, 0);
        writer.put_byte_unchecked(OP_2BYTE_ESCAPE as _);
        writer.put_byte_unchecked((op + (reg & 7)) as _);
    }

    pub fn two_byte_op64_rm(&mut self, op: u8, reg: u8, rm: u8) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);
        writer.emit_rexw(reg, 0, rm);
        writer.put_byte_unchecked(OP_2BYTE_ESCAPE as _);
        writer.put_byte_unchecked(op as _);
        writer.register_modrm(reg, rm);
    }

    pub fn two_byte_op64_mem(&mut self, op: u8, reg: u8, base: u8, offset: i32) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);
        writer.emit_rexw(reg, 0, base);
        writer.put_byte_unchecked(OP_2BYTE_ESCAPE as _);
        writer.put_byte_unchecked(op as _);
        writer.memory_modrm(reg, base, offset);
    }

    pub fn two_byte_op64_mem_scaled(
        &mut self,
        op: u8,
        reg: u8,
        base: u8,
        index: u8,
        scale: u8,
        offset: i32,
    ) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);
        writer.emit_rexw(reg, index, base);
        writer.put_byte_unchecked(OP_2BYTE_ESCAPE as _);
        writer.put_byte_unchecked(op as _);
        writer.memory_modrm_scaled(reg, base, index, scale, offset);
    }

    pub fn three_byte_op64(&mut self, two_byte_prefix: u8, op: u8, reg: u8, rm: u8) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);
        writer.emit_rexw(reg, 0, rm);
        writer.put_byte_unchecked(OP_2BYTE_ESCAPE as _);
        writer.put_byte_unchecked(two_byte_prefix as _);
        writer.put_byte_unchecked(op as _);
        writer.register_modrm(reg, rm);
    }

    // Byte-operands:
    //
    // These methods format byte operations.  Byte operations differ from the normal
    // formatters in the circumstances under which they will decide to emit REX prefixes.
    // These should be used where any register operand signifies a byte register.
    //
    // The disctinction is due to the handling of register numbers in the range 4..7 on
    // x86-64.  These register numbers may either represent the second byte of the first
    // four registers (ah..bh) or the first byte of the second four registers (spl..dil).
    //
    // Since ah..bh cannot be used in all permutations of operands (specifically cannot
    // be accessed where a REX prefix is present), these are likely best treated as
    // deprecated.  In order to ensure the correct registers spl..dil are selected a
    // REX prefix will be emitted for any byte register operand in the range 4..15.
    //
    // These formatters may be used in instructions where a mix of operand sizes, in which
    // case an unnecessary REX will be emitted, for example:
    //     movzbl %al, %edi
    // In this case a REX will be planted since edi is 7 (and were this a byte operand
    // a REX would be required to specify dil instead of bh).  Unneeded REX prefixes will
    // be silently ignored by the processor.
    //
    // Address operands should still be checked using regRequiresRex(), while byteRegRequiresRex()
    // is provided to check byte register operands.

    pub fn one_byte_op8_r(&mut self, op: u8, group: u8, rm: u8) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);
        writer.emit_rex_if(Self::byte_reg_requires_rex(rm), 0, 0, rm);
        writer.put_byte_unchecked(op as _);
        writer.register_modrm(group, rm);
    }

    pub fn one_byte_op8_rm(&mut self, op: u8, reg: u8, rm: u8) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);
        writer.emit_rex_if(Self::byte_reg_requires_rex_2(reg, rm), reg, 0, rm);
        writer.put_byte_unchecked(op as _);
        writer.register_modrm(reg, rm);
    }

    pub fn one_byte_op8_mem(&mut self, op: u8, reg: u8, base: u8, offset: i32) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);
        writer.emit_rex_if(Self::byte_reg_requires_rex(reg), reg, 0, base);
        writer.put_byte_unchecked(op as _);
        writer.memory_modrm(reg, base, offset);
    }

    pub fn one_byte_op8_mem_scaled(
        &mut self,
        op: u8,
        reg: u8,
        base: u8,
        index: u8,
        scale: u8,
        offset: i32,
    ) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);
        writer.emit_rex_if(Self::byte_reg_requires_rex(reg), reg, index, base);
        writer.put_byte_unchecked(op as _);
        writer.memory_modrm_scaled(reg, base, index, scale, offset);
    }

    pub fn two_byte_op8_rm(&mut self, op: u8, reg: u8, rm: u8) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);
        writer.emit_rex_if(Self::byte_reg_requires_rex_2(reg, rm), reg, 0, rm);
        writer.put_byte_unchecked(OP_2BYTE_ESCAPE as _);
        writer.put_byte_unchecked(op as _);
        writer.register_modrm(reg, rm);
    }

    pub fn two_byte_op8_r(&mut self, op: u8, group: u8, rm: u8) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);
        writer.emit_rex_if(Self::byte_reg_requires_rex(rm), 0, 0, rm);
        writer.put_byte_unchecked(OP_2BYTE_ESCAPE as _);
        writer.put_byte_unchecked(op as _);
        writer.register_modrm(group, rm);
    }

    pub fn two_byte_op8_mem(&mut self, op: u8, reg: u8, base: u8, offset: i32) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);
        writer.emit_rex_if(Self::byte_reg_requires_rex(reg), reg, 0, base);
        writer.put_byte_unchecked(OP_2BYTE_ESCAPE as _);
        writer.put_byte_unchecked(op as _);
        writer.memory_modrm(reg, base, offset);
    }

    pub fn two_byte_op8_mem_scaled(
        &mut self,
        op: u8,
        reg: u8,
        base: u8,
        index: u8,
        scale: u8,
        offset: i32,
    ) {
        let mut writer = SingleInstructionBufferWriter::new(&mut self.buffer);
        writer.emit_rex_if(Self::byte_reg_requires_rex(reg), reg, index, base);
        writer.put_byte_unchecked(OP_2BYTE_ESCAPE as _);
        writer.put_byte_unchecked(op as _);
        writer.memory_modrm_scaled(reg, base, index, scale, offset);
    }

    // Immediates:
    //
    // An immediate should be appended where appropriate after an op has been emitted.
    // The writes are unchecked since the opcode formatters above will have ensured space.

    pub fn immediate8(&mut self, imm: i32) {
        self.buffer.put_byte_unchecked(imm as _)
    }

    pub fn immediate16(&mut self, imm: i32) {
        self.buffer.put_u16_unchecked(imm as _)
    }

    pub fn immediate32(&mut self, imm: i32) {
        self.buffer.put_u32_unchecked(imm as _)
    }

    pub fn immediate64(&mut self, imm: i64) {
        self.buffer.put_u64_unchecked(imm as _)
    }

    pub fn immediate_rel32(&mut self) -> AssemblerLabel {
        self.buffer.put_u32_unchecked(0);
        self.label()
    }

    pub fn label(&mut self) -> AssemblerLabel {
        self.buffer.label()
    }

    pub fn code_size(&self) -> usize {
        self.buffer.code_size()
    }

    pub fn is_aligned(&self, alignment: usize) -> bool {
        self.buffer.is_aligned(alignment)
    }

    pub fn data(&self) -> &[u8] {
        self.buffer.data()
    }

    pub fn data_mut(&mut self) -> &mut [u8] {
        self.buffer.data_mut()
    }

    pub fn debug_offset(&self) -> usize {
        self.buffer.debug_offset()
    }
}

pub struct SingleInstructionBufferWriter<'a> {
    local: LocalWriter<'a>,
}

impl<'a> Deref for SingleInstructionBufferWriter<'a> {
    type Target = LocalWriter<'a>;

    fn deref(&self) -> &Self::Target {
        &self.local
    }
}

impl<'a> DerefMut for SingleInstructionBufferWriter<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.local
    }
}

impl<'a> SingleInstructionBufferWriter<'a> {
    pub fn new(buffer: &'a mut AssemblerBuffer) -> Self {
        Self {
            local: LocalWriter::new(buffer, X86InstructionFormatter::MAX_INSTRUCTION_SIZE),
        }
    }

    pub const NO_BASE: u8 = ebp;
    pub const HAS_SIB: u8 = esp;
    pub const NO_INDEX: u8 = esp;

    pub const NO_BASE2: u8 = r13;
    pub const HAS_SIB2: u8 = r12;

    pub fn emit_rex(&mut self, w: bool, r: u8, x: u8, b: u8) {
        self.put_byte_unchecked(
            (PRE_REX | (w as u8) << 3 | (r >> 3) << 2 | (x >> 3) << 1 | (b >> 3)) as _,
        );
    }

    pub fn emit_rexw(&mut self, r: u8, x: u8, b: u8) {
        self.emit_rex(true, r, x, b);
    }

    pub fn emit_rex_if(&mut self, w: bool, r: u8, x: u8, b: u8) {
        if w {
            self.emit_rex(false, r, x, b);
        }
    }

    pub fn emit_rex_if_needed(&mut self, r: u8, x: u8, b: u8) {
        self.emit_rex_if(
            X86InstructionFormatter::reg_requires_rex_3(r, x, b),
            r,
            x,
            b,
        );
    }

    pub fn put_modrm(&mut self, mode: u8, reg: u8, rm: u8) {
        self.put_byte_unchecked((mode | ((reg & 7) << 3) | (rm & 7)) as _);
    }

    pub fn put_modrm_sib(&mut self, mode: u8, reg: u8, base: u8, index: u8, scale: u8) {
        self.put_modrm(mode, reg, Self::HAS_SIB);
        self.put_byte_unchecked((scale << 6 | ((index & 7) << 3) | (base & 7)) as _);
    }

    pub fn register_modrm(&mut self, reg: u8, rm: u8) {
        self.put_modrm(MOD_RM_REG, reg, rm);
    }

    pub fn memory_modrm(&mut self, reg: u8, base: u8, offset: i32) {
        if base == Self::HAS_SIB || base == Self::HAS_SIB2 {
            if offset == 0 {
                self.put_modrm_sib(MOD_RM_MEM_NO_DISP, reg, base, Self::NO_INDEX, 0);
            } else if can_sign_extend_8_32(offset) {
                self.put_modrm_sib(MOD_RM_MEM_DISP8, reg, base, Self::NO_INDEX, 0);
                self.put_byte_unchecked(offset as _);
            } else {
                self.put_modrm_sib(MOD_RM_MEM_DISP32, reg, base, Self::NO_INDEX, 0);
                self.put_int_unchecked(offset);
            }
        } else {
            if offset == 0 && base != Self::NO_BASE && base != Self::NO_BASE2 {
                self.put_modrm(MOD_RM_MEM_NO_DISP, reg, base);
            } else if can_sign_extend_8_32(offset) {
                self.put_modrm(MOD_RM_MEM_DISP8, reg, base);
                self.put_byte_unchecked(offset as _);
            } else {
                self.put_modrm(MOD_RM_MEM_DISP32, reg, base);
                self.put_int_unchecked(offset);
            }
        }
    }

    pub fn memory_modrm_disp8(&mut self, reg: u8, base: u8, offset: i32) {
        if base == Self::HAS_SIB || base == Self::HAS_SIB2 {
            self.put_modrm_sib(MOD_RM_MEM_DISP8, reg, base, Self::NO_INDEX, 0);
            self.put_byte_unchecked(offset as _);
        } else {
            self.put_modrm(MOD_RM_MEM_DISP8, reg, base);
            self.put_byte_unchecked(offset as _);
        }
    }

    pub fn memory_modrm_disp32(&mut self, reg: u8, base: u8, offset: i32) {
        if base == Self::HAS_SIB || base == Self::HAS_SIB2 {
            self.put_modrm_sib(MOD_RM_MEM_DISP32, reg, base, Self::NO_INDEX, 0);
            self.put_int_unchecked(offset);
        } else {
            self.put_modrm(MOD_RM_MEM_DISP32, reg, base);
            self.put_int_unchecked(offset);
        }
    }

    pub fn memory_modrm_scaled(&mut self, reg: u8, base: u8, index: u8, scale: u8, offset: i32) {
        if offset == 0 {
            self.put_modrm_sib(MOD_RM_MEM_NO_DISP, reg, base, index, scale);
        } else if can_sign_extend_8_32(offset) {
            self.put_modrm_sib(MOD_RM_MEM_DISP8, reg, base, index, scale);
            self.put_byte_unchecked(offset as _);
        } else {
            self.put_modrm_sib(MOD_RM_MEM_DISP32, reg, base, index, scale);
            self.put_int_unchecked(offset);
        }
    }

    pub fn memory_modrm_addr(&mut self, reg: u8, addr: u32) {
        self.put_modrm_sib(MOD_RM_MEM_NO_DISP, reg, Self::NO_BASE, Self::NO_INDEX, 0);
        self.put_int_unchecked(addr as _);
    }

    pub fn two_bytes_vex_maybe_256(&mut self, is_vex_256: bool, simd_prefix: u8, r: u8, vvvv: u8) {
        let first_byte = VexPrefix::TwoBytes as u8; // 0xc5

        let mut second_byte = 0;

        second_byte |= (!X86InstructionFormatter::reg_requires_rex(r) as u8) << 7;
        second_byte |= (!vvvv & 0xf) << 3;
        second_byte |= (is_vex_256 as u8) << 2;
        second_byte |= vex_encoded_simd_prefix(simd_prefix);

        self.put_byte_unchecked(first_byte as _);
        self.put_byte_unchecked(second_byte as _);
    }

    pub fn two_bytes_vex(&mut self, simd_prefix: u8, in_op_reg: u8, r: u8) {
        self.two_bytes_vex_maybe_256(false, simd_prefix, r, in_op_reg);
    }

    pub fn three_bytes_vex_maybe_256(
        &mut self,
        is_vex_256: bool,
        simd_prefix: u8,
        implied_bytes: VexImpliedBytes,
        is_w1: bool,
        r: u8,
        x: u8,
        b: u8,
        vvvv: u8,
    ) {
        let first_byte = VexPrefix::ThreeBytes as u8; // 0xc4

        // R:    REX.R in 1’s complement (inverted) form 1: Same as REX.R=0 (must be 1 in 32-bit mode) 0: Same as REX.R=1 (64-bit mode only)
        // X:    REX.X in 1’s complement (inverted) form 1: Same as REX.X=0 (must be 1 in 32-bit mode) 0: Same as REX.X=1 (64-bit mode only)
        // B:    REX.B in 1’s complement (inverted) form 1: Same as REX.B=0 (Ignored in 32-bit mode). 0: Same as REX.B=1 (64-bit mode only)
        // m-mmmm:
        //     00000: Reserved for future use (will #UD)
        //     00001: implied 0F leading opcode byte
        //     00010: implied 0F 38 leading opcode bytes
        //     00011: implied 0F 3A leading opcode bytes
        //     00100-11111: Reserved for future use (will #UD)
        let mut second_byte = 0;

        second_byte |= (!X86InstructionFormatter::reg_requires_rex(r) as u8) << 7;
        second_byte |= (!X86InstructionFormatter::reg_requires_rex(x) as u8) << 6;
        second_byte |= (!X86InstructionFormatter::reg_requires_rex(b) as u8) << 5;
        second_byte |= implied_bytes as u8;

        let mut third_byte = 0;

        third_byte |= (is_w1 as u8) << 7;
        third_byte |= (!vvvv & 0xf) << 3;
        third_byte |= (is_vex_256 as u8) << 2;
        third_byte |= vex_encoded_simd_prefix(simd_prefix);

        self.put_byte_unchecked(first_byte as _);
        self.put_byte_unchecked(second_byte as _);
        self.put_byte_unchecked(third_byte as _);
    }

    pub fn three_bytes_vex_nds(
        &mut self,
        simd_prefix: u8,
        implied_bytes: VexImpliedBytes,
        vexw: VexW,
        r: u8,
        in_op_reg: u8,
        x: u8,
        b: u8,
    ) {
        self.three_bytes_vex_maybe_256(
            false,
            simd_prefix,
            implied_bytes,
            vexw == VexW::W1,
            r,
            x,
            b,
            in_op_reg,
        )
    }

    pub fn three_bytes_vex_nds2(
        &mut self,
        simd_prefix: u8,
        implied_bytes: VexImpliedBytes,
        vexw: VexW,
        r: u8,
        in_op_reg: u8,
        b: u8,
    ) {
        self.three_bytes_vex_maybe_256(
            false,
            simd_prefix,
            implied_bytes,
            vexw == VexW::W1,
            r,
            0,
            b,
            in_op_reg,
        )
    }
}

pub fn vex_encoded_simd_prefix(prefix: u8) -> u8 {
    match prefix {
        PRE_SSE_66 => 0b01,
        PRE_SSE_F3 => 0b10,
        PRE_SSE_F2 => 0b11,
        _ => 0b00,
    }
}

macro_rules! cenum {
    ($($id: ident = $val: expr), *) => {
        $(pub const $id: u8 = $val;)*
    };
}

cenum! {
    OP_ADD_EbGb                     = 0x00,
        PRE_SSE_00                      = 0x00,
        OP_ADD_EvGv                     = 0x01,
        OP_ADD_GvEv                     = 0x03,
        OP_ADD_EAXIv                    = 0x05,
        OP_OR_EvGb                      = 0x08,
        OP_OR_EvGv                      = 0x09,
        OP_OR_GvEv                      = 0x0B,
        OP_OR_EAXIv                     = 0x0D,
        OP_2BYTE_ESCAPE                 = 0x0F,
        OP_AND_EvGb                     = 0x20,
        OP_AND_EvGv                     = 0x21,
        OP_AND_GvEv                     = 0x23,
        OP_SUB_EvGb                     = 0x28,
        OP_SUB_EvGv                     = 0x29,
        OP_SUB_GvEv                     = 0x2B,
        OP_SUB_EAXIv                    = 0x2D,
        PRE_PREDICT_BRANCH_NOT_TAKEN    = 0x2E,
        OP_XOR_EvGb                     = 0x30,
        OP_XOR_EvGv                     = 0x31,
        OP_XOR_GvEv                     = 0x33,
        OP_XOR_EAXIv                    = 0x35,
        OP_CMP_EvGv                     = 0x39,
        OP_CMP_GvEv                     = 0x3B,
        OP_CMP_EAXIv                    = 0x3D,
        PRE_REX                         = 0x40,
        OP_PUSH_EAX                     = 0x50,
        OP_POP_EAX                      = 0x58,
        OP_MOVSXD_GvEv                  = 0x63,
        PRE_GS                          = 0x65,
        PRE_OPERAND_SIZE                = 0x66,
        PRE_SSE_66                      = 0x66,
        OP_PUSH_Iz                      = 0x68,
        OP_IMUL_GvEvIz                  = 0x69,
        OP_GROUP1_EbIb                  = 0x80,
        OP_GROUP1_EvIz                  = 0x81,
        OP_GROUP1_EvIb                  = 0x83,
        OP_TEST_EbGb                    = 0x84,
        OP_TEST_EvGv                    = 0x85,
        OP_XCHG_EvGb                    = 0x86,
        OP_XCHG_EvGv                    = 0x87,
        OP_MOV_EbGb                     = 0x88,
        OP_MOV_EvGv                     = 0x89,
        OP_MOV_GvEv                     = 0x8B,
        OP_LEA                          = 0x8D,
        OP_GROUP1A_Ev                   = 0x8F,
        OP_NOP                          = 0x90,
        OP_XCHG_EAX                     = 0x90,
        OP_PAUSE                        = 0x90,
        OP_CDQ                          = 0x99,
        OP_MOV_EAXOv                    = 0xA1,
        OP_MOV_OvEAX                    = 0xA3,
        OP_TEST_ALIb                    = 0xA8,
        OP_TEST_EAXIv                   = 0xA9,
        OP_MOV_EAXIv                    = 0xB8,
        OP_GROUP2_EvIb                  = 0xC1,
        OP_RET                          = 0xC3,
        OP_GROUP11_EvIb                 = 0xC6,
        OP_GROUP11_EvIz                 = 0xC7,
        OP_INT3                         = 0xCC,
        OP_GROUP2_Ev1                   = 0xD1,
        OP_GROUP2_EvCL                  = 0xD3,
        OP_ESCAPE_D9                    = 0xD9,
        OP_ESCAPE_DD                    = 0xDD,
        OP_CALL_rel32                   = 0xE8,
        OP_JMP_rel32                    = 0xE9,
        PRE_LOCK                        = 0xF0,
        PRE_SSE_F2                      = 0xF2,
        PRE_SSE_F3                      = 0xF3,
        OP_HLT                          = 0xF4,
        OP_GROUP3_Eb                    = 0xF6,
        OP_GROUP3_EbIb                  = 0xF6,
        OP_GROUP3_Ev                    = 0xF7,
        OP_GROUP3_EvIz                  = 0xF7, // OP_GROUP3_Ev has an immediate, when instruction is a test.
        OP_GROUP5_Ev                    = 0xFF
}

cenum! {
    OP2_UD2                         = 0xB,
        OP2_MOVSD_VsdWsd                = 0x10,
        OP2_MOVUPS_VsdWsd               = 0x10,
        OP2_MOVSS_VsdWsd                = 0x10,
        OP2_MOVSD_WsdVsd                = 0x11,
        OP2_MOVUPS_WsdVsd               = 0x11,
        OP2_MOVSS_WsdVsd                = 0x11,
        OP2_MOVDDUP_VqWq                = 0x12,
        OP2_MOVHLPS_VqUq                = 0x12,
        OP2_MOVSLDUP_VqWq               = 0x12,
        OP2_UNPCKLPD_VpdWpd             = 0x14,
        OP2_UNPCKHPD_VpdWpd             = 0x15,
        OP2_MOVSHDUP_VqWq               = 0x16,
        OP2_MOVAPD_VpdWpd               = 0x28,
        OP2_MOVAPS_VpsWps               = 0x28,
        OP2_MOVAPS_WpsVps               = 0x29,
        OP2_CVTSI2SS_VssEs              = 0x2A,
        OP2_CVTSI2SD_VsdEd              = 0x2A,
        OP2_CVTTSD2SI_GdWsd             = 0x2C,
        OP2_CVTTSS2SI_GdWsd             = 0x2C,
        OP2_UCOMISS_VssWss              = 0x2E,
        OP2_UCOMISD_VsdWsd              = 0x2E,
        OP2_RDTSC                       = 0x31,
        OP2_3BYTE_ESCAPE_38             = 0x38,
        OP2_3BYTE_ESCAPE_3A             = 0x3A,
        OP2_CMOVCC                      = 0x40,
        OP2_MOVMSKPD_VdEd               = 0x50,
        OP2_SQRTSD_VsdWsd               = 0x51,
        OP2_SQRTSS_VssWss               = 0x51,
        OP2_ANDPS_VpsWps                = 0x54,
        OP2_ANDPD_VpdWpd                = 0x54,
        OP2_ANDNPS_VpsWps               = 0x55,
        OP2_ANDNPD_VpdWpd               = 0x55,
        OP2_ORPS_VpsWps                 = 0x56,
        OP2_ORPD_VpdWpd                 = 0x56,
        OP2_XORPS_VpsWps                = 0x57,
        OP2_XORPD_VpdWpd                = 0x57,
        OP2_ADDSD_VsdWsd                = 0x58,
        OP2_MULSD_VsdWsd                = 0x59,
        OP2_CVTSD2SS_VsdWsd             = 0x5A,
        OP2_CVTPS2PD_VsdWsd             = 0x5A,
        OP2_CVTSS2SD_VsdWsd             = 0x5A,
        OP2_CVTPD2PS_VsdWsd             = 0x5A,
        OP2_CVTDQ2PS_VsdWsd             = 0x5B,
        OP2_SUBSD_VsdWsd                = 0x5C,
        OP2_MINPS_VpsWps                = 0x5D,
        OP2_MINPD_VpdWpd                = 0x5D,
        OP2_DIVSD_VsdWsd                = 0x5E,
        OP2_MAXPS_VpsWps                = 0x5F,
        OP2_MAXPD_VpdWpd                = 0x5F,
        OP2_PUNPCKLBW_VdqWdq            = 0x60,
        OP2_PACKSSWB_VdqWdq             = 0x63,
        OP2_PACKUSWB_VdqWdq             = 0x67,
        OP2_PUNPCKHBW_VdqWdq            = 0x68,
        OP2_PACKSSDW_VdqWdq             = 0x6B,
        OP2_PUNPCKLQDQ_VdqWdq           = 0x6C,
        OP2_MOVD_VdEd                   = 0x6E,
        OP2_MOVQ_PqQq                   = 0x6E,
        OP2_MOVDQA_VdqWdq               = 0x6F,
        OP2_PSHUFD_VdqWdqIb             = 0x70,
        OP2_PSHUFLW_VdqWdqIb            = 0x70,
        OP2_PSHUFHW_VdqWdqIb            = 0x70,
        OP2_PSLLW_UdqIb                 = 0x71,
        OP2_PSRLW_UdqIb                 = 0x71,
        OP2_PSRAW_UdqIb                 = 0x71,
        OP2_PSLLD_UdqIb                 = 0x72,
        OP2_PSRLD_UdqIb                 = 0x72,
        OP2_PSRAD_UdqIb                 = 0x72,
        OP2_PSLLQ_UdqIb                 = 0x73,
        OP2_PSRLQ_UdqIb                 = 0x73,
        OP2_VZEROUPPER                  = 0x77,
        OP2_MOVD_EdVd                   = 0x7E,
        OP2_MOVQ_QqPq                   = 0x7E,
        OP2_JCC_rel32                   = 0x80,
        OP_SETCC                        = 0x90,
        OP2_CPUID                       = 0xA2,
        OP2_BT_EvEv                     = 0xA3,
        OP2_3BYTE_ESCAPE_AE             = 0xAE,
        OP2_IMUL_GvEv                   = 0xAF,
        OP2_CMPXCHGb                    = 0xB0,
        OP2_CMPXCHG                     = 0xB1,
        OP2_BTR                         = 0xB3,
        OP2_MOVZX_GvEb                  = 0xB6,
        OP2_POPCNT                      = 0xB8,
        OP2_GROUP_BT_EvIb               = 0xBA,
        OP2_BSF                         = 0xBC,
        OP2_TZCNT                       = 0xBC,
        OP2_BSR                         = 0xBD,
        OP2_LZCNT                       = 0xBD,
        OP2_MOVSX_GvEb                  = 0xBE,
        OP2_MOVZX_GvEw                  = 0xB7,
        OP2_MOVSX_GvEw                  = 0xBF,
        OP2_XADDb                       = 0xC0,
        OP2_XADD                        = 0xC1,
        OP2_PINSRW_VdqRdqp              = 0xC4,
        OP2_PEXTRW_GdUdIb               = 0xC5,
        OP2_SHUFPD_VpdWpdIb             = 0xC6,
        OP2_SHUFPS_VpdWpdIb             = 0xC6,
        OP2_BSWAP                       = 0xC8,
        OP2_PSUBUSB_VdqWdq              = 0xD8,
        OP2_PSUBUSW_VdqWdq              = 0xD9,
        OP2_VPAND_VxHxWx                = 0xDB,
        OP2_PADDUSB_VdqWdq              = 0xDC,
        OP2_PADDUSW_VdqWdq              = 0xDD,
        OP2_PAVGB_VdqWdq                = 0xE0,
        OP2_PAVGW_VdqWdq                = 0xE3,
        OP2_CVTDQ2PD_VdqWdq             = 0xE6,
        OP2_CVTDPD2DQ_VdqWdq            = 0xE6,
        OP2_PSUBSB_VdqWdq               = 0xE8,
        OP2_PSUBSW_VdqWdq               = 0xE9,
        OP2_POR_VdqWdq                  = 0xEB,
        OP2_PADDSB_VdqWdq               = 0xEC,
        OP2_PADDSW_VdqWdq               = 0xED,
        OP2_PXOR_VdqWdq                 = 0xEF,
        OP2_PADDB_VdqWdq                = 0xFC,
        OP2_PADDW_VdqWdq                = 0xFD,
        OP2_PADDD_VdqWdq                = 0xFE,
        OP2_PADDQ_VdqWdq                = 0xD4,
        OP2_PSUBB_VdqWdq                = 0xF8,
        OP2_PSUBW_VdqWdq                = 0xF9,
        OP2_PSUBD_VdqWdq                = 0xFA,
        OP2_PSUBQ_VdqWdq                = 0xFB,
        OP2_PMULLW_VdqWdq               = 0xD5,
        OP2_PMOVMSKB_GdqpUdq            = 0xD7,
        OP2_ADDPS_VpsWps                = 0x58,
        OP2_ADDPD_VpdWpd                = 0x58,
        OP2_SUBPS_VpsWps                = 0x5C,
        OP2_SUBPD_VpdWpd                = 0x5C,
        OP2_MULPS_VpsWps                = 0x59,
        OP2_MULPD_VpdWpd                = 0x59,
        OP2_DIVPS_VpsWps                = 0x5E,
        OP2_DIVPD_VpdWpd                = 0x5E,
        OP2_DIVSS_VpsWps                = 0x5E,
        OP2_SQRTPS_VpsWps               = 0x51,
        OP2_SQRTPD_VpdWpd               = 0x51,
        OP2_PMADDWD_VdqWdq              = 0xF5,
        OP2_VPSLLD_VxHxWx               = 0x72,
        OP2_PCMPEQB_VdqWdq              = 0x74,
        OP2_PCMPEQW_VdqWdq              = 0x75,
        OP2_PCMPEQD_VdqWdq              = 0x76,
        OP2_PCMPGTB_VdqWdq              = 0x64,
        OP2_PCMPGTW_VdqWdq              = 0x65,
        OP2_PCMPGTD_VdqWdq              = 0x66,
        OP2_CMPPS_VpsWpsIb              = 0xC2,
        OP2_CMPPD_VpdWpdIb              = 0xC2,
        OP2_PMAXSW_VdqWdq               = 0xEE,
        OP2_PMAXUB_VdqWdq               = 0xDE,
        OP2_PMINSW_VdqWdq               = 0xEA,
        OP2_PMINUB_VdqWdq               = 0xDA,
        OP2_PSRLW_VdqWdq                = 0xD1,
        OP2_PSRLD_VdqWdq                = 0xD2,
        OP2_PSRLQ_VdqWdq                = 0xD3,
        OP2_PSRAW_VdqWdq                = 0xE1,
        OP2_PSRAD_VdqWdq                = 0xE2,
        OP2_PSLLW_VdqWdq                = 0xF1,
        OP2_PSLLD_VdqWdq                = 0xF2,
        OP2_PSLLQ_VdqWdq                = 0xF3,
        OP2_PMOVMSKB_EqWdq              = 0xD7,
        OP2_MOVMSKPS_EqWps              = 0x50,
        OP2_MOVMSKPD_EqWpd              = 0x50
}

cenum! {
    OP3_PSHUFB_VdqWdq       = 0x00,
        OP3_PMADDUBSW_VpdWpd    = 0x04,
        OP3_ROUNDPD_MbVdqIb     = 0x09,
        OP3_ROUNDSS_VssWssIb    = 0x0A,
        OP3_ROUNDSD_VsdWsdIb    = 0x0B,
        OP3_VPBLENDW_VxHxWxIb   = 0x0E,
        OP3_PEXTRB_MbVdqIb      = 0x14,
        OP3_PEXTRW_MwVdqIb      = 0x15,
        OP3_PEXTRD_EyVdqIb      = 0x16,
        OP3_PEXTRQ_EyVdqIb      = 0x16,
        OP3_EXTRACTPS_EdVdqIb   = 0x17,
        OP3_VBROADCASTSS_VxWd   = 0x18,
        OP3_VPMOVSXBW_VxUx      = 0x20,
        OP3_PABSB_VdqWdq        = 0x1C,
        OP3_PABSW_VdqWdq        = 0x1D,
        OP3_PABSD_VdqWdq        = 0x1E,
        OP3_INSERTPS_VpsUpsIb   = 0x21,
        OP3_PINSRB_VdqRdqpIb    = 0x20,
        OP3_PINSRD_VdqEdIb      = 0x22,
        OP3_PINSRQ_VdqEqbIb     = 0x22,
        OP3_VPMOVSXWD_VxUx      = 0x23,
        OP3_VPMOVSXDQ_VxUx      = 0x25,
        OP3_VPACKUSDW_VxHxWx    = 0x2B,
        OP3_VPMOVZXBW_VxUx      = 0x30,
        OP3_VPMOVZXWD_VxUx      = 0x33,
        OP3_VPMOVZXDQ_VxUx      = 0x35,
        OP3_BLENDVPD_VpdWpdXMM0 = 0x4B,
        OP3_LFENCE              = 0xE8,
        OP3_MFENCE              = 0xF0,
        OP3_SFENCE              = 0xF8,
        OP3_ROUNDPS_VpsWpsIb    = 0x08,
        OP3_ROUNDPD_VpdWpdIb    = 0x09,
        OP3_PMULLD_VdqWdq       = 0x40,
        OP3_PCMPEQQ_VdqWdq      = 0x29,
        OP3_PCMPGTQ_VdqWdq      = 0x37,
        OP3_PMINSB_VdqWdq       = 0x38,
        OP3_PMINSD_VdqWdq       = 0x39,
        OP3_PMINUW_VdqWdq       = 0x3A,
        OP3_PMINUD_VdqWdq       = 0x3B,
        OP3_PMAXSB_VdqWdq       = 0x3C,
        OP3_PMAXSD_VdqWdq       = 0x3D,
        OP3_PMAXUW_VdqWdq       = 0x3E,
        OP3_PMAXUD_VdqWdq       = 0x3F,
        OP3_PTEST_VdqWdq        = 0x17
}

cenum! {
    GROUP1_OP_ADD = 0,
    GROUP1_OP_OR  = 1,
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
    GROUP3_OP_NOT  = 2,
    GROUP3_OP_NEG  = 3,
    GROUP3_OP_DIV = 6,
    GROUP3_OP_IDIV = 7,

    GROUP5_OP_CALLN = 2,
    GROUP5_OP_JMPN  = 4,
    GROUP5_OP_PUSH  = 6,

    GROUP11_MOV = 0,

    GROUP14_OP_PSLLD = 6,
    GROUP14_OP_PSLLQ = 6,
    GROUP14_OP_PSRAQ = 4,
    GROUP14_OP_PSRLQ = 2,

    ESCAPE_D9_FSTP_singleReal = 3,
    ESCAPE_DD_FSTP_doubleReal = 3,

    GROUP_BT_OP_BT = 4
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[repr(u8)]
pub enum VexImpliedBytes {
    TwoBytes = 0b01,
    ThreeBytesOp38 = 0b10,
    ThreeBytesOp3A = 0b11,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[repr(u8)]
pub enum VexPrefix {
    TwoBytes = 0xC5,
    ThreeBytes = 0xC4,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[repr(u8)]
pub enum VexW {
    W0 = 0,
    W1 = 1,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[repr(u8)]
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

impl Condition {
    pub const C: Condition = Condition::B;
    pub const NC: Condition = Condition::AE;
}

pub fn cmovcc(cond: Condition) -> u8 {
    OP2_CMOVCC + cond as u8
}

pub fn jcc_rel32(cond: Condition) -> u8 {
    OP2_JCC_rel32 + cond as u8
}

pub fn setcc_opcode(cond: Condition) -> u8 {
    OP_SETCC + cond as u8
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
