#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
#[repr(u8)]
pub enum Rounding {
    ToNearestWithTiesToEven = 0,
    TowardNegativeInfiniti = 1,
    TowardInfiniti = 2,
    TowardZero = 3,
}

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
    XMM8,
    XMM9,
    XMM10,
    XMM11,
    XMM12,
    XMM13,
    XMM14,
    XMM15,
}

impl Into<RegisterID> for XMMRegisterID {
    fn into(self) -> RegisterID {
        unsafe { std::mem::transmute(self) }
    }
}

pub struct X86Asm {
    pub formatter: X86AsmFormatter,
    index_of_last_watchpoint: i32,
    index_of_tail_last_watchpoint: i32,
}

impl X86Asm {
    pub fn data(&self) -> &[u8] {
        &self.formatter.buffer.storage
    }
    pub fn new(x64: bool) -> Self {
        Self {
            formatter: X86AsmFormatter {
                buffer: AsmBuffer::new(),
                x64,
            },
            index_of_last_watchpoint: 0,
            index_of_tail_last_watchpoint: 0,
        }
    }

    pub fn nop(&mut self) {
        self.formatter.one_byte_op(OP_NOP);
    }

    pub fn label(&mut self) -> AsmLabel {
        let mut result = self.formatter.label();
        while result.offset() < self.index_of_tail_last_watchpoint as u32 {
            self.nop();
            result = self.formatter.label();
        }
        result
    }

    pub fn label_for_watchpoint(&mut self) -> AsmLabel {
        let mut res = self.formatter.label();
        if res.offset() as i32 != self.index_of_last_watchpoint {
            res = self.label();
        }
        self.index_of_last_watchpoint = res.offset() as _;
        self.index_of_tail_last_watchpoint = res.offset() as i32 + 5;
        res
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
    pub fn addl_mr_scaled(
        &mut self,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
        dst: RegisterID,
    ) {
        self.formatter
            .one_byte_op_scaled(OP_ADD_GvEv, dst as _, base, index, scale, offset);
    }

    pub fn addl_ar(&mut self, addr: u32, dst: RegisterID) {
        self.formatter
            .one_byte_op_x86addr(OP_ADD_GvEv, dst as _, addr);
    }

    pub fn addl_rm(&mut self, src: RegisterID, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op_off(OP_ADD_EvGv, src as _, base, offset);
    }

    pub fn addl_rm_scaled(
        &mut self,
        src: RegisterID,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
    ) {
        self.formatter
            .one_byte_op_scaled(OP_ADD_EvGv, src as _, base, index, scale, offset);
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
    pub fn addb_rm(&mut self, src: RegisterID, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op8_off(OP_ADD_EbGb, src as _, base, offset);
    }

    pub fn addb_rm_scaled(
        &mut self,
        src: RegisterID,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
    ) {
        self.formatter
            .one_byte_op8_scaled(OP_ADD_EbGb, src as _, base, index, scale, offset);
    }

    pub fn addb_im(&mut self, imm: i8, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op8_off(OP_GROUP1_EbIb, GROUP1_OP_ADD as _, base, offset);
        self.formatter.buffer.put_byte(imm);
    }

    pub fn addb_im_scaled(
        &mut self,
        imm: i8,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
    ) {
        self.formatter.one_byte_op8_scaled(
            OP_GROUP1_EbIb,
            GROUP1_OP_ADD as _,
            base,
            index,
            scale,
            offset,
        );
        self.formatter.buffer.put_byte(imm);
    }

    pub fn addw_rm(&mut self, src: RegisterID, offset: i32, base: RegisterID) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.formatter
            .one_byte_op8_off(OP_ADD_EvGv, src as _, base, offset);
    }

    pub fn addw_rm_scaled(
        &mut self,
        src: RegisterID,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
    ) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.formatter
            .one_byte_op8_scaled(OP_ADD_EvGv, src as _, base, index, scale, offset);
    }

    pub fn addw_im(&mut self, imm: i16, offset: i32, base: RegisterID) {
        if imm as i8 as i16 == imm {
            self.formatter
                .one_byte_op8_off(OP_GROUP1_EvIb, GROUP1_OP_ADD as _, base, offset);
            self.formatter.buffer.put_byte(imm as _);
        } else {
            self.formatter
                .one_byte_op8_off(OP_GROUP1_EvIz, GROUP1_OP_ADD as _, base, offset);
            self.formatter.buffer.put_short(imm as _);
        }
    }

    pub fn addw_im_scaled(
        &mut self,
        imm: i16,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
    ) {
        if imm as i8 as i16 == imm {
            self.formatter.one_byte_op8_scaled(
                OP_GROUP1_EvIb,
                GROUP1_OP_ADD as _,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.buffer.put_byte(imm as _);
        } else {
            self.formatter.one_byte_op8_scaled(
                OP_GROUP1_EvIz,
                GROUP1_OP_ADD as _,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.buffer.put_short(imm as _);
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

    pub fn andl_rr(&mut self, src: RegisterID, dst: RegisterID) {
        self.formatter.one_byte_op_rm(OP_AND_EvGv, src as _, dst);
    }

    pub fn andl_mr(&mut self, offset: i32, base: RegisterID, dst: RegisterID) {
        self.formatter
            .one_byte_op_off(OP_AND_GvEv, dst as _, base, offset);
    }

    pub fn andl_rm(&mut self, src: RegisterID, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op_off(OP_AND_EvGv, src as _, base, offset);
    }

    pub fn andl_ir(&mut self, imm: i32, dst: RegisterID) {
        if imm as i8 as i32 == imm {
            self.formatter
                .one_byte_op_rm(OP_GROUP1_EvIb, GROUP1_OP_AND as _, dst);
            self.formatter.buffer.put_byte(imm as _);
        } else {
            self.formatter
                .one_byte_op_rm(OP_GROUP1_EvIz, GROUP1_OP_AND as _, dst);
            self.formatter.buffer.put_int(imm as _);
        }
    }
    pub fn andl_im(&mut self, imm: i32, offset: i32, base: RegisterID) {
        if imm as i8 as i32 == imm {
            self.formatter
                .one_byte_op_off(OP_GROUP1_EvIb, GROUP1_OP_AND as _, base, offset);
            self.formatter.buffer.put_byte(imm as _);
        } else {
            self.formatter
                .one_byte_op_off(OP_GROUP1_EvIz, GROUP1_OP_AND as _, base, offset);
            self.formatter.buffer.put_int(imm as _);
        }
    }

    pub fn andq_rr(&mut self, src: RegisterID, dst: RegisterID) {
        self.formatter.one_byte_op64_rm(OP_AND_EvGv, src as _, dst);
    }

    pub fn andq_ir(&mut self, imm: i32, dst: RegisterID) {
        if imm as i8 as i32 == imm {
            self.formatter
                .one_byte_op64_rm(OP_GROUP1_EvIb, GROUP1_OP_AND as _, dst);
            self.formatter.buffer.put_byte(imm as _);
        } else {
            self.formatter
                .one_byte_op64_rm(OP_GROUP1_EvIz, GROUP1_OP_AND as _, dst);
            self.formatter.buffer.put_int(imm as _);
        }
    }

    pub fn negl_r(&mut self, dst: RegisterID) {
        self.formatter
            .one_byte_op_rm(OP_GROUP3_Ev, GROUP3_OP_NEG as _, dst);
    }

    pub fn negq_r(&mut self, dst: RegisterID) {
        self.formatter
            .one_byte_op64_rm(OP_GROUP3_Ev, GROUP3_OP_NEG as _, dst);
    }

    pub fn negl_m(&mut self, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op_off(OP_GROUP3_Ev, GROUP3_OP_NEG as _, base, offset);
    }
    pub fn notl_r(&mut self, dst: RegisterID) {
        self.formatter
            .one_byte_op_rm(OP_GROUP3_Ev, GROUP3_OP_NOT as _, dst);
    }

    pub fn notl_m(&mut self, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op_off(OP_GROUP3_Ev, GROUP3_OP_NOT as _, base, offset);
    }

    pub fn orl_rr(&mut self, src: RegisterID, dst: RegisterID) {
        self.formatter.one_byte_op_rm(OP_OR_EvGv, src as _, dst);
    }

    pub fn orl_mr(&mut self, offset: i32, base: RegisterID, dst: RegisterID) {
        self.formatter
            .one_byte_op_off(OP_OR_EvGv, dst as _, base, offset);
    }
    pub fn orl_mr_scaled(
        &mut self,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
        dst: RegisterID,
    ) {
        self.formatter
            .one_byte_op_scaled(OP_OR_EvGv, dst as _, base, index, scale, offset);
    }

    pub fn orl_rm(&mut self, src: RegisterID, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op_off(OP_OR_GvEv, src as _, base, offset);
    }
    pub fn orl_rm_scaled(
        &mut self,
        src: RegisterID,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
    ) {
        self.formatter
            .one_byte_op_scaled(OP_OR_GvEv, src as _, base, index, scale, offset);
    }

    pub fn orl_ir(&mut self, imm: i32, dst: RegisterID) {
        if imm as i8 as i32 == imm {
            self.formatter
                .one_byte_op_rm(OP_GROUP1_EvIb, GROUP1_OP_OR as _, dst);
            self.formatter.buffer.put_byte(imm as _);
        } else {
            self.formatter
                .one_byte_op_rm(OP_GROUP1_EvIz, GROUP1_OP_OR as _, dst);
            self.formatter.buffer.put_int(imm as _);
        }
    }

    pub fn orl_im(&mut self, imm: i32, offset: i32, base: RegisterID) {
        if imm as i8 as i32 == imm {
            self.formatter
                .one_byte_op_off(OP_GROUP1_EvIb, GROUP1_OP_OR as _, base, offset);
            self.formatter.buffer.put_byte(imm as _);
        } else {
            self.formatter
                .one_byte_op_off(OP_GROUP1_EvIz, GROUP1_OP_OR as _, base, offset);
            self.formatter.buffer.put_int(imm as _);
        }
    }

    pub fn orq_rr(&mut self, src: RegisterID, dst: RegisterID) {
        self.formatter.one_byte_op64_rm(OP_OR_EvGv, src as _, dst);
    }

    pub fn orq_ir(&mut self, imm: i32, dst: RegisterID) {
        if imm as i8 as i32 == imm {
            self.formatter
                .one_byte_op64_rm(OP_GROUP1_EvIb, GROUP1_OP_OR as _, dst);
            self.formatter.buffer.put_byte(imm as _);
        } else {
            self.formatter
                .one_byte_op64_rm(OP_GROUP1_EvIz, GROUP1_OP_OR as _, dst);
            self.formatter.buffer.put_int(imm as _);
        }
    }
    pub fn subl_rr(&mut self, src: RegisterID, dst: RegisterID) {
        self.formatter.one_byte_op_rm(OP_SUB_EvGv, src as _, dst);
    }

    pub fn subl_mr(&mut self, offset: i32, base: RegisterID, dst: RegisterID) {
        self.formatter
            .one_byte_op_off(OP_SUB_EvGv, dst as _, base, offset);
    }

    pub fn subl_rm(&mut self, src: RegisterID, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op_off(OP_SUB_GvEv, src as _, base, offset);
    }

    pub fn subl_ir(&mut self, imm: i32, dst: RegisterID) {
        if imm as i8 as i32 == imm {
            self.formatter
                .one_byte_op_rm(OP_GROUP1_EvIb, GROUP1_OP_SUB as _, dst);
            self.formatter.buffer.put_byte(imm as _);
        } else {
            self.formatter
                .one_byte_op_rm(OP_GROUP1_EvIz, GROUP1_OP_SUB as _, dst);
            self.formatter.buffer.put_int(imm as _);
        }
    }

    pub fn subl_im(&mut self, imm: i32, offset: i32, base: RegisterID) {
        if imm as i8 as i32 == imm {
            self.formatter
                .one_byte_op_off(OP_GROUP1_EvIb, GROUP1_OP_SUB as _, base, offset);
            self.formatter.buffer.put_byte(imm as _);
        } else {
            self.formatter
                .one_byte_op_off(OP_GROUP1_EvIz, GROUP1_OP_SUB as _, base, offset);
            self.formatter.buffer.put_int(imm as _);
        }
    }
    pub fn subq_rr(&mut self, src: RegisterID, dst: RegisterID) {
        self.formatter.one_byte_op64_rm(OP_SUB_EvGv, src as _, dst);
    }

    pub fn subq_ir(&mut self, imm: i32, dst: RegisterID) {
        if imm as i8 as i32 == imm {
            self.formatter
                .one_byte_op64_rm(OP_GROUP1_EvIb, GROUP1_OP_SUB as _, dst);
            self.formatter.buffer.put_byte(imm as _);
        } else {
            self.formatter
                .one_byte_op64_rm(OP_GROUP1_EvIz, GROUP1_OP_SUB as _, dst);
            self.formatter.buffer.put_int(imm as _);
        }
    }
    pub fn xorl_rr(&mut self, src: RegisterID, dst: RegisterID) {
        self.formatter.one_byte_op_rm(OP_XOR_EvGv, src as _, dst);
    }

    pub fn xorl_mr(&mut self, offset: i32, base: RegisterID, dst: RegisterID) {
        self.formatter
            .one_byte_op_off(OP_XOR_EvGv, dst as _, base, offset);
    }

    pub fn xorl_rm(&mut self, src: RegisterID, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op_off(OP_XOR_GvEv, src as _, base, offset);
    }

    pub fn xorl_ir(&mut self, imm: i32, dst: RegisterID) {
        if imm as i8 as i32 == imm {
            self.formatter
                .one_byte_op_rm(OP_GROUP1_EvIb, GROUP1_OP_XOR as _, dst);
            self.formatter.buffer.put_byte(imm as _);
        } else {
            self.formatter
                .one_byte_op_rm(OP_GROUP1_EvIz, GROUP1_OP_XOR as _, dst);
            self.formatter.buffer.put_int(imm as _);
        }
    }

    pub fn xorl_im(&mut self, imm: i32, offset: i32, base: RegisterID) {
        if imm as i8 as i32 == imm {
            self.formatter
                .one_byte_op_off(OP_GROUP1_EvIb, GROUP1_OP_XOR as _, base, offset);
            self.formatter.buffer.put_byte(imm as _);
        } else {
            self.formatter
                .one_byte_op_off(OP_GROUP1_EvIz, GROUP1_OP_XOR as _, base, offset);
            self.formatter.buffer.put_int(imm as _);
        }
    }

    pub fn xorq_rr(&mut self, src: RegisterID, dst: RegisterID) {
        self.formatter.one_byte_op64_rm(OP_XOR_EvGv, src as _, dst);
    }

    pub fn xorq_ir(&mut self, imm: i32, dst: RegisterID) {
        if imm as i8 as i32 == imm {
            self.formatter
                .one_byte_op64_rm(OP_GROUP1_EvIb, GROUP1_OP_XOR as _, dst);
            self.formatter.buffer.put_byte(imm as _);
        } else {
            self.formatter
                .one_byte_op64_rm(OP_GROUP1_EvIz, GROUP1_OP_XOR as _, dst);
            self.formatter.buffer.put_int(imm as _);
        }
    }

    pub fn rorq_i8r(&mut self, imm: i8, dst: RegisterID) {
        if imm == 1 {
            self.formatter
                .one_byte_op64_rm(OP_GROUP2_Ev1, GROUP2_OP_ROR as _, dst);
        } else {
            self.formatter
                .one_byte_op64_rm(OP_GROUP2_EvIb, GROUP2_OP_ROR as _, dst);
            self.formatter.buffer.put_byte(imm);
        }
    }

    pub fn sarq_clr(&mut self, dst: RegisterID) {
        self.formatter
            .one_byte_op64_rm(OP_GROUP2_EvCL, GROUP2_OP_SAR as _, dst);
    }

    pub fn sarq_i8r(&mut self, imm: i8, dst: RegisterID) {
        if imm == 1 {
            self.formatter
                .one_byte_op64_rm(OP_GROUP2_Ev1, GROUP2_OP_SAR as _, dst);
        } else {
            self.formatter
                .one_byte_op64_rm(OP_GROUP2_EvIb, GROUP2_OP_SAR as _, dst);
            self.formatter.buffer.put_byte(imm);
        }
    }

    pub fn shrq_i8r(&mut self, imm: i8, dst: RegisterID) {
        if imm == 1 {
            self.formatter
                .one_byte_op64_rm(OP_GROUP2_Ev1, GROUP2_OP_SHR as _, dst);
        } else {
            self.formatter
                .one_byte_op64_rm(OP_GROUP2_EvIb, GROUP2_OP_SHR as _, dst);
            self.formatter.buffer.put_byte(imm);
        }
    }

    pub fn shrq_clr(&mut self, dst: RegisterID) {
        self.formatter
            .one_byte_op64_rm(OP_GROUP2_EvCL, GROUP2_OP_SHR as _, dst);
    }

    pub fn shlq_i8r(&mut self, imm: i8, dst: RegisterID) {
        if imm == 1 {
            self.formatter
                .one_byte_op64_rm(OP_GROUP2_Ev1, GROUP2_OP_SHL as _, dst);
        } else {
            self.formatter
                .one_byte_op64_rm(OP_GROUP2_EvIb, GROUP2_OP_SHL as _, dst);
            self.formatter.buffer.put_byte(imm);
        }
    }

    pub fn sarl_i8r(&mut self, imm: i8, dst: RegisterID) {
        if imm == 1 {
            self.formatter
                .one_byte_op_rm(OP_GROUP2_Ev1, GROUP2_OP_SAR as _, dst);
        } else {
            self.formatter
                .one_byte_op_rm(OP_GROUP2_EvIb, GROUP2_OP_SAR as _, dst);
            self.formatter.buffer.put_byte(imm);
        }
    }
    pub fn sarl_clr(&mut self, dst: RegisterID) {
        self.formatter
            .one_byte_op_rm(OP_GROUP2_EvCL, GROUP2_OP_SAR as _, dst);
    }

    pub fn shrl_i8r(&mut self, imm: i8, dst: RegisterID) {
        if imm == 1 {
            self.formatter
                .one_byte_op_rm(OP_GROUP2_Ev1, GROUP2_OP_SHR as _, dst);
        } else {
            self.formatter
                .one_byte_op_rm(OP_GROUP2_EvIb, GROUP2_OP_SHR as _, dst);
            self.formatter.buffer.put_byte(imm);
        }
    }
    pub fn shrl_clr(&mut self, dst: RegisterID) {
        self.formatter
            .one_byte_op_rm(OP_GROUP2_EvCL, GROUP2_OP_SHR as _, dst);
    }
    pub fn shll_i8r(&mut self, imm: i8, dst: RegisterID) {
        if imm == 1 {
            self.formatter
                .one_byte_op_rm(OP_GROUP2_Ev1, GROUP2_OP_SHL as _, dst);
        } else {
            self.formatter
                .one_byte_op_rm(OP_GROUP2_EvIb, GROUP2_OP_SHL as _, dst);
            self.formatter.buffer.put_byte(imm);
        }
    }
    pub fn shll_clr(&mut self, dst: RegisterID) {
        self.formatter
            .one_byte_op_rm(OP_GROUP2_EvCL, GROUP2_OP_SHL as _, dst);
    }

    pub fn imull_rr(&mut self, src: RegisterID, dst: RegisterID) {
        self.formatter.one_byte_op_rm(OP2_IMUL_GvEv, dst as _, src);
    }

    pub fn imull_mr(&mut self, offset: i32, base: RegisterID, dst: RegisterID) {
        self.formatter
            .one_byte_op_off(OP2_IMUL_GvEv, dst as _, base, offset);
    }

    pub fn imulq_rr(&mut self, src: RegisterID, dst: RegisterID) {
        self.formatter
            .one_byte_op64_rm(OP2_IMUL_GvEv, dst as _, src);
    }

    pub fn imull_i32r(&mut self, src: RegisterID, value: i32, dst: RegisterID) {
        self.formatter.one_byte_op_rm(OP_IMUL_GvEvIz, dst as _, src);
        self.formatter.buffer.put_int(value);
    }

    pub fn divl_r(&mut self, dst: RegisterID) {
        self.formatter
            .one_byte_op_rm(OP_GROUP3_Ev, GROUP3_OP_DIV as _, dst);
    }
    pub fn idivl_r(&mut self, dst: RegisterID) {
        self.formatter
            .one_byte_op_rm(OP_GROUP3_Ev, GROUP3_OP_IDIV as _, dst);
    }
    pub fn divq_r(&mut self, dst: RegisterID) {
        self.formatter
            .one_byte_op64_rm(OP_GROUP3_Ev, GROUP3_OP_DIV as _, dst);
    }
    pub fn idivq_r(&mut self, dst: RegisterID) {
        self.formatter
            .one_byte_op64_rm(OP_GROUP3_Ev, GROUP3_OP_IDIV as _, dst);
    }
    pub fn cmpl_rr(&mut self, src: RegisterID, dst: RegisterID) {
        self.formatter.one_byte_op_rm(OP_CMP_EvGv, src as _, dst);
    }

    pub fn cmpl_rm(&mut self, src: RegisterID, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op_off(OP_CMP_EvGv, src as _, base, offset);
    }
    pub fn cmpl_mr(&mut self, src: RegisterID, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op_off(OP_CMP_GvEv, src as _, base, offset);
    }

    pub fn cmpl_ir(&mut self, imm: i32, dst: RegisterID) {
        if imm as i8 as i32 == imm {
            self.formatter
                .one_byte_op_rm(OP_GROUP1_EvIb, GROUP1_OP_CMP as _, dst);
            self.formatter.buffer.put_byte(imm as _);
        } else {
            if dst == RegisterID::EAX {
                self.formatter.one_byte_op(OP_CMP_EAXIv);
            } else {
                self.formatter
                    .one_byte_op_rm(OP_GROUP1_EvIz, GROUP1_OP_CMP as _, dst);
            }
            self.formatter.buffer.put_int(imm);
        }
    }

    pub fn cmpl_ir_force32(&mut self, imm: i32, dst: RegisterID) {
        self.formatter
            .one_byte_op_rm(OP_GROUP1_EvIz, GROUP1_OP_CMP as _, dst);
        self.formatter.buffer.put_int(imm);
    }

    pub fn cmpl_im(&mut self, imm: i32, offset: i32, base: RegisterID) {
        if imm as i8 as i32 == imm {
            self.formatter
                .one_byte_op_off(OP_GROUP1_EvIb, GROUP1_OP_CMP as _, base, offset);
            self.formatter.buffer.put_byte(imm as _);
        } else {
            self.formatter
                .one_byte_op_off(OP_GROUP1_EvIz, GROUP1_OP_CMP as _, base, offset);
            self.formatter.buffer.put_int(imm as _);
        }
    }
    pub fn cmpb_im(&mut self, imm: i32, offset: i32, base: RegisterID) {
        if imm as i8 as i32 == imm {
            self.formatter
                .one_byte_op_off(OP_GROUP1_EbIb, GROUP1_OP_CMP as _, base, offset);
            self.formatter.buffer.put_byte(imm as _);
        } else {
            panic!("Imm8 expected");
        }
    }
    pub fn cmpb_im_scaled(
        &mut self,
        imm: i32,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
    ) {
        if imm as i8 as i32 == imm {
            self.formatter.one_byte_op_scaled(
                OP_GROUP1_EbIb,
                GROUP1_OP_CMP as _,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.buffer.put_byte(imm as _);
        } else {
            panic!("Imm8 expected");
        }
    }
    pub fn cmpl_im_scaled(
        &mut self,
        imm: i32,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
    ) {
        if imm as i8 as i32 == imm {
            self.formatter.one_byte_op_scaled(
                OP_GROUP1_EvIb,
                GROUP1_OP_CMP as _,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.buffer.put_byte(imm as _);
        } else {
            self.formatter.one_byte_op_scaled(
                OP_GROUP1_EvIz,
                GROUP1_OP_CMP as _,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.buffer.put_int(imm as _);
        }
    }
    pub fn cmpl_im_force32(&mut self, imm: i32, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op_off(OP_GROUP1_EvIz, GROUP1_OP_CMP as _, base, offset);
        self.formatter.buffer.put_int(imm as _);
    }

    pub fn cmpq_rr(&mut self, src: RegisterID, dst: RegisterID) {
        self.formatter.one_byte_op64_rm(OP_CMP_EvGv, src as _, dst);
    }

    pub fn cmpq_rm(&mut self, src: RegisterID, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op64_off(OP_CMP_EvGv, src as _, base, offset);
    }
    pub fn cmpq_mr(&mut self, src: RegisterID, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op64_off(OP_CMP_GvEv, src as _, base, offset);
    }

    pub fn cmpq_ir(&mut self, imm: i32, dst: RegisterID) {
        if imm as i8 as i32 == imm {
            self.formatter
                .one_byte_op64_rm(OP_GROUP1_EvIb, GROUP1_OP_CMP as _, dst);
            self.formatter.buffer.put_byte(imm as _);
        } else {
            if dst == RegisterID::EAX {
                self.formatter.one_byte_op64(OP_CMP_EAXIv);
            } else {
                self.formatter
                    .one_byte_op64_rm(OP_GROUP1_EvIz, GROUP1_OP_CMP as _, dst);
            }
            self.formatter.buffer.put_int(imm);
        }
    }

    pub fn cmpq_ir_force32(&mut self, imm: i32, dst: RegisterID) {
        self.formatter
            .one_byte_op64_rm(OP_GROUP1_EvIz, GROUP1_OP_CMP as _, dst);
        self.formatter.buffer.put_int(imm);
    }

    pub fn cmpq_im(&mut self, imm: i32, offset: i32, base: RegisterID) {
        if imm as i8 as i32 == imm {
            self.formatter
                .one_byte_op64_off(OP_GROUP1_EvIb, GROUP1_OP_CMP as _, base, offset);
            self.formatter.buffer.put_byte(imm as _);
        } else {
            self.formatter
                .one_byte_op64_off(OP_GROUP1_EvIz, GROUP1_OP_CMP as _, base, offset);
            self.formatter.buffer.put_int(imm as _);
        }
    }
    pub fn cmpq_im_scaled(
        &mut self,
        imm: i32,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
    ) {
        if imm as i8 as i32 == imm {
            self.formatter.one_byte_op64_scaled(
                OP_GROUP1_EvIb,
                GROUP1_OP_CMP as _,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.buffer.put_byte(imm as _);
        } else {
            self.formatter.one_byte_op64_scaled(
                OP_GROUP1_EvIz,
                GROUP1_OP_CMP as _,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.buffer.put_int(imm as _);
        }
    }
    pub fn cmpq_im_force32(&mut self, imm: i32, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op64_off(OP_GROUP1_EvIz, GROUP1_OP_CMP as _, base, offset);
        self.formatter.buffer.put_int(imm as _);
    }

    pub fn cmpw_ir(&mut self, imm: i32, dst: RegisterID) {
        if imm as i8 as i32 == imm {
            self.formatter.prefix(PRE_OPERAND_SIZE);
            self.formatter
                .one_byte_op_rm(OP_GROUP1_EvIb, GROUP1_OP_CMP as _, dst);
            self.formatter.buffer.put_byte(imm as _);
        } else {
            self.formatter.prefix(PRE_OPERAND_SIZE);
            self.formatter
                .one_byte_op_rm(OP_GROUP1_EvIz, GROUP1_OP_CMP as _, dst);
            self.formatter.buffer.put_int(imm as _);
        }
    }

    pub fn cmpw_im(
        &mut self,
        imm: i32,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
    ) {
        if imm as i8 as i32 == imm {
            self.formatter.prefix(PRE_OPERAND_SIZE);
            self.formatter.one_byte_op_scaled(
                OP_GROUP1_EvIb,
                GROUP1_OP_CMP as _,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.buffer.put_byte(imm as _);
        } else {
            self.formatter.prefix(PRE_OPERAND_SIZE);
            self.formatter.one_byte_op_scaled(
                OP_GROUP1_EvIz,
                GROUP1_OP_CMP as _,
                base,
                index,
                scale,
                offset,
            );
            self.formatter.buffer.put_int(imm as _);
        }
    }

    pub fn cmpw_rm(
        &mut self,
        src: RegisterID,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
    ) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.formatter
            .one_byte_op_scaled(OP_CMP_EvGv, src as _, base, index, scale, offset);
    }

    pub fn testl_rr(&mut self, src: RegisterID, dst: RegisterID) {
        self.formatter.one_byte_op_rm(OP_TEST_EvGv, src as _, dst);
    }

    pub fn testl_i32r(&mut self, imm: i32, dst: RegisterID) {
        if dst == RegisterID::EAX {
            self.formatter.one_byte_op(OP_TEST_EAXIv);
        } else {
            self.formatter
                .one_byte_op_rm(OP_GROUP3_EvIz, GROUP3_OP_TEST as _, dst);
        }
        self.formatter.buffer.put_int(imm);
    }

    pub fn testl_i32m(&mut self, imm: i32, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op_off(OP_GROUP3_EvIz, GROUP3_OP_TEST as _, base, offset);
        self.formatter.buffer.put_int(imm);
    }

    pub fn testb_rr(&mut self, src: RegisterID, dst: RegisterID) {
        self.formatter.one_byte_op8_rm(OP_TEST_EbGb, src as _, dst);
    }

    pub fn testb_im(&mut self, imm: i32, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op_off(OP_GROUP3_EbIb, GROUP3_OP_TEST as _, base, offset);
        self.formatter.buffer.put_byte(imm as _);
    }

    pub fn testl_i32m_scaled(
        &mut self,
        imm: i32,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
    ) {
        self.formatter.one_byte_op_scaled(
            OP_GROUP3_EvIz,
            GROUP3_OP_TEST as _,
            base,
            index,
            scale,
            offset,
        );
        self.formatter.buffer.put_int(imm);
    }

    pub fn testq_rr(&mut self, src: RegisterID, dst: RegisterID) {
        self.formatter.one_byte_op64_rm(OP_TEST_EvGv, src as _, dst);
    }

    pub fn testq_rm(&mut self, src: RegisterID, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op64_off(OP_TEST_EvGv, src as _, base, offset);
    }

    pub fn testq_i32r(&mut self, imm: i32, dst: RegisterID) {
        if dst == RegisterID::EAX {
            self.formatter.one_byte_op64(OP_TEST_EAXIv);
        } else {
            self.formatter
                .one_byte_op64_rm(OP_GROUP3_EvIz, GROUP3_OP_TEST as _, dst);
        }
        self.formatter.buffer.put_int(imm);
    }
    pub fn testq_i32m(&mut self, imm: i32, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op64_off(OP_GROUP3_EvIz, GROUP3_OP_TEST as _, base, offset);
        self.formatter.buffer.put_int(imm);
    }
    pub fn testb_i8r(&mut self, imm: i8, dst: RegisterID) {
        if dst == RegisterID::EAX {
            self.formatter.one_byte_op(OP_TEST_ALIb);
        } else {
            self.formatter
                .one_byte_op8_rm(OP_GROUP3_EbIb, GROUP3_OP_TEST as _, dst);
        }
        self.formatter.buffer.put_byte(imm);
    }
    pub fn bt_ir(&mut self, offset: i8, value: RegisterID) {
        self.formatter
            .two_byte_op_rm(OP2_GROUP_BT_EvIb, GROUP_BT_OP_BT as _, value);
        self.formatter.buffer.put_byte(offset);
    }
    pub fn bt_rr(&mut self, offset: RegisterID, value: RegisterID) {
        self.formatter
            .two_byte_op_rm(OP2_BT_EvEv, offset as _, value);
    }

    pub fn setcc_r(&mut self, cond: Condition, dst: RegisterID) {
        self.formatter.two_byte_op8_rm(setcc_opcode(cond), 0, dst);
    }

    pub fn sete_r(&mut self, dst: RegisterID) {
        self.setcc_r(Condition::E, dst);
    }

    pub fn setz_r(&mut self, dst: RegisterID) {
        self.sete_r(dst);
    }

    pub fn cdq(&mut self) {
        self.formatter.one_byte_op(OP_CDQ);
    }

    pub fn cqo(&mut self) {
        self.formatter.one_byte_op64(OP_CDQ);
    }

    pub fn fstps(&mut self, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op_off(OP_ESCAPE_D9, ESCAPE_D9_FSTP_singleReal as _, base, offset);
    }
    pub fn fstpl(&mut self, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op_off(OP_ESCAPE_DD, ESCAPE_DD_FSTP_doubleReal as _, base, offset);
    }

    pub fn xchgl_rr(&mut self, src: RegisterID, dst: RegisterID) {
        if src == RegisterID::EAX {
            self.formatter.one_byte_op_r(OP_XCHG_EAX, dst);
        } else if dst == RegisterID::EAX {
            self.formatter.one_byte_op_r(OP_XCHG_EAX, src);
        } else {
            self.formatter.one_byte_op_rm(OP_XCHG_EvGv, src as _, dst);
        }
    }

    pub fn xchgb_rm(&mut self, src: RegisterID, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op8_off(OP_XOR_EvGb, src as _, base, offset);
    }

    pub fn xchgb_rm_scaled(
        &mut self,
        src: RegisterID,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
    ) {
        self.formatter
            .one_byte_op_scaled(OP_XOR_EvGb, src as _, base, index, scale, offset);
    }

    pub fn xchgw_rm(&mut self, src: RegisterID, offset: i32, base: RegisterID) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.formatter
            .one_byte_op_off(OP_XOR_EvGb, src as _, base, offset);
    }

    pub fn xchgw_rm_scaled(
        &mut self,
        src: RegisterID,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
    ) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.formatter
            .one_byte_op_scaled(OP_XOR_EvGb, src as _, base, index, scale, offset);
    }
    pub fn xchgl_rm(&mut self, src: RegisterID, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op_off(OP_XOR_EvGb, src as _, base, offset);
    }

    pub fn xchgl_rm_scaled(
        &mut self,
        src: RegisterID,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
    ) {
        self.formatter
            .one_byte_op_scaled(OP_XOR_EvGb, src as _, base, index, scale, offset);
    }

    pub fn xchgq_rr(&mut self, src: RegisterID, dst: RegisterID) {
        if src == RegisterID::EAX {
            self.formatter.one_byte_op64_r(OP_XCHG_EAX, dst as _);
        } else if dst == RegisterID::EAX {
            self.formatter.one_byte_op64_r(OP_XCHG_EAX, src as _);
        } else {
            self.formatter.one_byte_op64_rm(OP_XCHG_EvGv, src as _, dst);
        }
    }
    pub fn xchgq_rm(&mut self, src: RegisterID, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op64_off(OP_XOR_EvGb, src as _, base, offset);
    }

    pub fn xchgq_rm_scaled(
        &mut self,
        src: RegisterID,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
    ) {
        self.formatter
            .one_byte_op64_scaled(OP_XOR_EvGb, src as _, base, index, scale, offset);
    }

    pub fn movl_rr(&mut self, src: RegisterID, dst: RegisterID) {
        self.formatter.one_byte_op_rm(OP_MOV_EvGv, src as _, dst);
    }

    pub fn movl_rm(&mut self, src: RegisterID, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op_off(OP_MOV_EvGv, src as _, base, offset);
    }

    pub fn movl_rm_disp32(&mut self, src: RegisterID, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op_off32(OP_MOV_EvGv, src as _, base, offset);
    }

    pub fn movl_rm_scaled(
        &mut self,
        src: RegisterID,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
    ) {
        self.formatter
            .one_byte_op_scaled(OP_MOV_EvGv, src as _, base, index, scale, offset);
    }

    pub fn movl_meax(&mut self, addr: usize) {
        self.formatter.one_byte_op(OP_MOV_EAXOv);
        if self.formatter.x64 {
            self.formatter.buffer.put_long(addr as _);
        } else {
            self.formatter.buffer.put_int(addr as _);
        }
    }
    pub fn movl_mr(&mut self, offset: i32, base: RegisterID, dst: RegisterID) {
        self.formatter
            .one_byte_op_off(OP_MOV_GvEv, dst as _, base, offset);
    }
    pub fn movl_mr_disp32(&mut self, offset: i32, base: RegisterID, dst: RegisterID) {
        self.formatter
            .one_byte_op_off32(OP_MOV_GvEv, dst as _, base, offset);
    }
    pub fn movl_mr_disp8(&mut self, offset: i32, base: RegisterID, dst: RegisterID) {
        self.formatter
            .one_byte_op_off8(OP_MOV_GvEv, dst as _, base, offset);
    }

    pub fn movl_mr_scaled(
        &mut self,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
        dst: RegisterID,
    ) {
        self.formatter
            .one_byte_op_scaled(OP_MOV_GvEv, dst as _, base, index, scale, offset);
    }

    pub fn movl_i32r(&mut self, imm: i32, dst: RegisterID) {
        self.formatter.one_byte_op_r(OP_MOV_EAXIv, dst);
        self.formatter.buffer.put_int(imm);
    }

    pub fn movl_i32m(&mut self, imm: i32, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op_off(OP_GROUP11_EvIz, GROUP11_MOV as _, base, offset);
        self.formatter.buffer.put_int(imm);
    }

    pub fn movl_i32m_scaled(
        &mut self,
        imm: i32,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
    ) {
        self.formatter.one_byte_op_scaled(
            OP_GROUP11_EvIz,
            GROUP11_MOV as _,
            base,
            index,
            scale,
            offset,
        );
        self.formatter.buffer.put_int(imm);
    }

    pub fn movb_i8m(&mut self, imm: i8, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op_off(OP_GROUP11_EvIb, GROUP11_MOV as _, base, offset);
        self.formatter.buffer.put_byte(imm);
    }

    pub fn movb_i8m_scaled(
        &mut self,
        imm: i8,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
    ) {
        self.formatter.one_byte_op_scaled(
            OP_GROUP11_EvIb,
            GROUP11_MOV as _,
            base,
            index,
            scale,
            offset,
        );
        self.formatter.buffer.put_byte(imm);
    }
    pub fn movb_rm(&mut self, src: RegisterID, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op_off(OP_MOV_EbGb, src as _, base, offset);
    }

    pub fn movb_rm_scaled(
        &mut self,
        src: RegisterID,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
    ) {
        self.formatter
            .one_byte_op_scaled(OP_MOV_EbGb, src as _, base, index, scale, offset);
    }
    pub fn movw_rm(&mut self, src: RegisterID, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op8_off(OP_MOV_EbGb, src as _, base, offset);
    }

    pub fn movw_rm_scaled(
        &mut self,
        src: RegisterID,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
    ) {
        self.formatter
            .one_byte_op8_scaled(OP_MOV_EbGb, src as _, base, index, scale, offset);
    }

    pub fn movw_im(&mut self, imm: i16, offset: i32, base: RegisterID) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.formatter
            .one_byte_op_off(OP_GROUP11_EvIz, GROUP11_MOV as _, base, offset);
        self.formatter.buffer.put_short(imm);
    }
    pub fn movw_im_scaled(
        &mut self,
        imm: i16,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
    ) {
        self.formatter.prefix(PRE_OPERAND_SIZE);
        self.formatter.one_byte_op_scaled(
            OP_GROUP11_EvIz,
            GROUP11_MOV as _,
            base,
            index,
            scale,
            offset,
        );
        self.formatter.buffer.put_short(imm);
    }

    pub fn movl_eaxm(&mut self, addr: usize) {
        self.formatter.one_byte_op(OP_MOV_OvEAX);
        if self.formatter.x64 {
            self.formatter.buffer.put_long(addr as _);
        } else {
            self.formatter.buffer.put_int(addr as _);
        }
    }
    pub fn movq_rr(&mut self, src: RegisterID, dst: RegisterID) {
        self.formatter.one_byte_op64_rm(OP_MOV_EvGv, src as _, dst);
    }

    pub fn movq_rm(&mut self, src: RegisterID, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op64_off(OP_MOV_EvGv, src as _, base, offset);
    }

    pub fn movq_rm_disp32(&mut self, src: RegisterID, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op64_off32(OP_MOV_EvGv, src as _, base, offset);
    }

    pub fn movq_rm_scaled(
        &mut self,
        src: RegisterID,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
    ) {
        self.formatter
            .one_byte_op64_scaled(OP_MOV_EvGv, src as _, base, index, scale, offset);
    }

    pub fn movq_meax(&mut self, addr: usize) {
        self.formatter.one_byte_op64(OP_MOV_EAXOv);

        self.formatter.buffer.put_long(addr as _);
    }
    pub fn movq_mr(&mut self, offset: i32, base: RegisterID, dst: RegisterID) {
        self.formatter
            .one_byte_op64_off(OP_MOV_GvEv, dst as _, base, offset);
    }
    pub fn movq_mr_disp32(&mut self, offset: i32, base: RegisterID, dst: RegisterID) {
        self.formatter
            .one_byte_op64_off32(OP_MOV_GvEv, dst as _, base, offset);
    }
    pub fn movq_mr_disp8(&mut self, offset: i32, base: RegisterID, dst: RegisterID) {
        self.formatter
            .one_byte_op64_off8(OP_MOV_GvEv, dst as _, base, offset);
    }

    pub fn movq_mr_scaled(
        &mut self,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
        dst: RegisterID,
    ) {
        self.formatter
            .one_byte_op64_scaled(OP_MOV_GvEv, dst as _, base, index, scale, offset);
    }

    pub fn mov_i32r(&mut self, imm: i32, dst: RegisterID) {
        self.formatter
            .one_byte_op64_rm(OP_GROUP11_EvIz, GROUP11_MOV as _, dst as _);
        self.formatter.buffer.put_int(imm);
    }

    pub fn movq_i32m(&mut self, imm: i32, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op64_off(OP_GROUP11_EvIz, GROUP11_MOV as _, base, offset);
        self.formatter.buffer.put_int(imm);
    }

    pub fn movq_i32m_scaled(
        &mut self,
        imm: i32,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
    ) {
        self.formatter.one_byte_op64_scaled(
            OP_GROUP11_EvIz,
            GROUP11_MOV as _,
            base,
            index,
            scale,
            offset,
        );
        self.formatter.buffer.put_int(imm);
    }

    pub fn movq_i64r(&mut self, imm: i64, dst: RegisterID) {
        self.formatter.one_byte_op64_r(OP_MOV_EAXIv, dst as _);
        self.formatter.buffer.put_long(imm);
    }

    pub fn movsxd_rr(&mut self, src: RegisterID, dst: RegisterID) {
        self.formatter
            .one_byte_op64_rm(OP_MOVSXD_GvEv, dst as _, src);
    }

    pub fn movzwl_mr(&mut self, offset: i32, base: RegisterID, dst: RegisterID) {
        self.formatter
            .two_byte_op_off(OP2_MOVZX_GvEw, dst as _, base, offset);
    }
    pub fn movzwl_mr_scaled(
        &mut self,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
        dst: RegisterID,
    ) {
        self.formatter
            .two_byte_op_scaled(OP2_MOVZX_GvEw, dst as _, base, index, scale, offset);
    }

    pub fn movswl_mr(&mut self, offset: i32, base: RegisterID, dst: RegisterID) {
        self.formatter
            .two_byte_op_off(OP2_MOVSX_GvEw, dst as _, base, offset);
    }
    pub fn movswl_mr_scaled(
        &mut self,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
        dst: RegisterID,
    ) {
        self.formatter
            .two_byte_op_scaled(OP2_MOVZX_GvEb, dst as _, base, index, scale, offset);
    }
    pub fn movzbl_mr(&mut self, offset: i32, base: RegisterID, dst: RegisterID) {
        self.formatter
            .two_byte_op_off(OP2_MOVZX_GvEb, dst as _, base, offset);
    }
    pub fn movsbl_mr(&mut self, offset: i32, base: RegisterID, dst: RegisterID) {
        self.formatter
            .two_byte_op_off(OP2_MOVSX_GvEb, dst as _, base, offset);
    }
    pub fn movsbl_mr_scaled(
        &mut self,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
        dst: RegisterID,
    ) {
        self.formatter
            .two_byte_op_scaled(OP2_MOVSX_GvEb, dst as _, base, index, scale, offset);
    }

    pub fn movzbl_mr_scaled(
        &mut self,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
        dst: RegisterID,
    ) {
        self.formatter
            .two_byte_op_scaled(OP2_MOVZX_GvEw, dst as _, base, index, scale, offset);
    }
    pub fn movzbl_rr(&mut self, src: RegisterID, dst: RegisterID) {
        // In 64-bit, this may cause an unnecessary REX to be planted (if the dst register
        // is in the range ESP-EDI, and the src would not have required a REX).  Unneeded
        // REX prefixes are defined to be silently ignored by the processor.
        self.formatter.two_byte_op_rm(OP2_MOVZX_GvEb, dst as _, src);
    }

    pub fn movsbl_rr(&mut self, src: RegisterID, dst: RegisterID) {
        self.formatter.two_byte_op_rm(OP2_MOVSX_GvEb, dst as _, src);
    }
    pub fn movzwl_rr(&mut self, src: RegisterID, dst: RegisterID) {
        self.formatter.two_byte_op_rm(OP2_MOVZX_GvEw, dst as _, src);
    }
    pub fn movswl_rr(&mut self, src: RegisterID, dst: RegisterID) {
        self.formatter.two_byte_op_rm(OP2_MOVSX_GvEw, dst as _, src);
    }

    pub fn cmovl_rr(&mut self, cond: Condition, src: RegisterID, dst: RegisterID) {
        self.formatter.two_byte_op_rm(cmovcc(cond), dst as _, src);
    }

    pub fn cmovl_mr(&mut self, cond: Condition, offset: i32, base: RegisterID, dst: RegisterID) {
        self.formatter
            .two_byte_op_off(cmovcc(cond), dst as _, base, offset);
    }

    pub fn cmovl_mr_scaled(
        &mut self,
        cond: Condition,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
        dst: RegisterID,
    ) {
        self.formatter
            .two_byte_op_scaled(cmovcc(cond), dst as _, base, index, scale, offset);
    }
    pub fn cmovq_mr(&mut self, cond: Condition, offset: i32, base: RegisterID, dst: RegisterID) {
        self.formatter
            .two_byte_op64_off(cmovcc(cond), dst as _, base, offset);
    }

    pub fn cmovq_mr_scaled(
        &mut self,
        cond: Condition,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
        dst: RegisterID,
    ) {
        self.formatter
            .two_byte_op64_scaled(cmovcc(cond), dst as _, base, index, scale, offset);
    }

    pub fn cmovq_rr(&mut self, cond: Condition, src: RegisterID, dst: RegisterID) {
        self.formatter.two_byte_op64_rm(cmovcc(cond), dst as _, src);
    }

    pub fn leal_mr(&mut self, offset: i32, base: RegisterID, dst: RegisterID) {
        self.formatter
            .one_byte_op_off(OP_LEA, dst as _, base, offset);
    }
    pub fn leal_mr_scaled(
        &mut self,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
        dst: RegisterID,
    ) {
        self.formatter
            .one_byte_op_scaled(OP_LEA, dst as _, base, index, scale, offset);
    }

    pub fn leaq_mr(&mut self, offset: i32, base: RegisterID, dst: RegisterID) {
        self.formatter
            .one_byte_op64_off(OP_LEA, dst as _, base, offset);
    }
    pub fn leaq_mr_scaled(
        &mut self,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
        dst: RegisterID,
    ) {
        self.formatter
            .one_byte_op64_scaled(OP_LEA, dst as _, base, index, scale, offset);
    }

    pub fn inc_r(&mut self, dst: RegisterID) {
        self.formatter
            .one_byte_op_rm(OP_GROUP5_Ev, GROUP1_OP_ADD as _, dst);
    }

    pub fn incq_r(&mut self, dst: RegisterID) {
        self.formatter
            .one_byte_op64_rm(OP_GROUP5_Ev, GROUP1_OP_ADD as _, dst);
    }
    pub fn incq_m(&mut self, base: RegisterID, offset: i32) {
        self.formatter
            .one_byte_op64_off(OP_GROUP5_Ev, GROUP1_OP_ADD as _, base, offset);
    }

    pub fn incq_m_scaled(&mut self, offset: i32, base: RegisterID, index: RegisterID, scale: i32) {
        self.formatter.one_byte_op64_scaled(
            OP_GROUP5_Ev,
            GROUP1_OP_ADD as _,
            base,
            index,
            scale,
            offset,
        );
    }
    pub fn call_rel(&mut self) -> AsmLabel {
        self.formatter.one_byte_op(OP_CALL_rel32);
        self.formatter.buffer.put_int(0);
        self.formatter.label()
    }

    pub fn call_r(&mut self, r: RegisterID) -> AsmLabel {
        self.formatter
            .one_byte_op_rm(OP_GROUP5_Ev, GROUP5_OP_CALLN as _, r);
        self.formatter.label()
    }

    pub fn call_m(&mut self, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op_off(OP_GROUP5_Ev, GROUP5_OP_CALLN as _, base, offset);
    }

    pub fn jmp(&mut self) -> AsmLabel {
        self.formatter.one_byte_op(OP_JMP_rel32);
        self.formatter.buffer.put_int(0);
        self.formatter.label()
    }
    // Return a AssemblerLabel so we have a label to the jump, so we can use this
    // To make a tail recursive call on x86-64.  The MacroAssembler
    // really shouldn't wrap this as a Jump, since it can't be linked. :-/
    pub fn jmp_r(&mut self, r: RegisterID) -> AsmLabel {
        self.formatter
            .one_byte_op_rm(OP_GROUP5_Ev, GROUP5_OP_JMPN as _, r);
        self.formatter.label()
    }

    pub fn jmp_m(&mut self, offset: i32, base: RegisterID) {
        self.formatter
            .one_byte_op_off(OP_GROUP5_Ev, GROUP5_OP_JMPN as _, base, offset);
    }

    pub fn jcc(&mut self, c: Condition) -> AsmLabel {
        self.formatter.two_byte_op(jcc_rel32(c));
        self.formatter.buffer.put_int(0);
        self.formatter.label()
    }

    pub fn int3(&mut self) {
        self.formatter.one_byte_op(OP_INT3);
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
    pub fn gs(&mut self) {
        self.formatter.prefix(PRE_GS);
    }

    pub fn cpuid(&mut self) {
        self.formatter.two_byte_op(OP2_CPUID);
    }

    pub fn addsd_rr(&mut self, src: XMMRegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_rm(OP2_ADDSD_VsdWsd, dst as i32, src.into());
    }

    pub fn addsd_mr(&mut self, offset: i32, base: RegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_off(OP2_ADDSD_VsdWsd, dst as i32, base, offset);
    }

    pub fn addsd_mr_scaled(
        &mut self,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
        dst: XMMRegisterID,
    ) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_scaled(OP2_ADDSD_VsdWsd, dst as i32, base, index, scale, offset);
    }

    pub fn addss_rr(&mut self, src: XMMRegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op_rm(OP2_ADDSD_VsdWsd, dst as i32, src.into());
    }

    pub fn addss_mr(&mut self, offset: i32, base: RegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op_off(OP2_ADDSD_VsdWsd, dst as i32, base, offset);
    }

    pub fn addss_mr_scaled(
        &mut self,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
        dst: XMMRegisterID,
    ) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op_scaled(OP2_ADDSD_VsdWsd, dst as i32, base, index, scale, offset);
    }
    pub fn cvtsi2sd_rr(&mut self, src: RegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_rm(OP2_CVTSI2SD_VsdEd, dst as _, src);
    }

    pub fn cvtsi2ss_rr(&mut self, src: RegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_rm(OP2_CVTSI2SD_VsdEd, dst as _, src);
    }

    pub fn cvtsi2sdq_rr(&mut self, src: RegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op64_rm(OP2_CVTSI2SD_VsdEd, dst as _, src);
    }
    pub fn cvtsi2ssq_rr(&mut self, src: RegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op64_rm(OP2_CVTSI2SD_VsdEd, dst as _, src);
    }

    pub fn cvtsi2sdq_mr(&mut self, offset: i32, base: RegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op64_off(OP2_CVTSI2SD_VsdEd, dst as _, base, offset);
    }
    pub fn cvtsi2sd_mr(&mut self, offset: i32, base: RegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_off(OP2_CVTSI2SD_VsdEd, dst as _, base, offset);
    }
    pub fn cvtsi2ss_mr(&mut self, offset: i32, base: RegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op_off(OP2_CVTSI2SD_VsdEd, dst as _, base, offset);
    }

    pub fn cvttsd2si_rr(&mut self, src: XMMRegisterID, dst: RegisterID) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_rm(OP2_CVTTSD2SI_GdWsd, dst as _, src.into());
    }
    pub fn cvttss2si_rr(&mut self, src: XMMRegisterID, dst: RegisterID) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op_rm(OP2_CVTTSS2SI_GdWsd, dst as _, src.into());
    }
    pub fn cvttss2siq_rr(&mut self, src: XMMRegisterID, dst: RegisterID) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op64_rm(OP2_CVTTSS2SI_GdWsd, dst as _, src.into());
    }

    pub fn cvtsd2ss_rr(&mut self, src: XMMRegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_rm(OP2_CVTSD2SS_VsdWsd, dst as _, src.into());
    }

    pub fn cvtsd2ss_mr(&mut self, offset: i32, base: RegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_off(OP2_CVTSD2SS_VsdWsd, dst as _, base, offset);
    }
    pub fn cvtss2sd_rr(&mut self, src: XMMRegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op_rm(OP2_CVTSS2SD_VsdWsd, dst as _, src.into());
    }

    pub fn cvtss2sd_mr(&mut self, offset: i32, base: RegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_off(OP2_CVTSS2SD_VsdWsd, dst as _, base, offset);
    }

    pub fn cvttsd2siq_rr(&mut self, src: XMMRegisterID, dst: RegisterID) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op64_rm(OP2_CVTTSD2SI_GdWsd, dst as _, src.into());
    }

    pub fn movd_f2i_rr(&mut self, src: XMMRegisterID, dst: RegisterID) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter.two_byte_op_rm(OP2_MOVD_EdVd, src as _, dst);
    }

    pub fn movd_i2f_rr(&mut self, src: RegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter.two_byte_op_rm(OP2_MOVD_VdEd, dst as _, src);
    }

    pub fn movmskpd_rr(&mut self, src: XMMRegisterID, dst: RegisterID) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .two_byte_op64_rm(OP2_MOVMSKPD_VdEd, dst as _, src.into());
    }

    pub fn movq_f2i_rr(&mut self, src: XMMRegisterID, dst: RegisterID) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .two_byte_op64_rm(OP2_MOVD_EdVd, src as _, dst);
    }

    pub fn movq_i2f_rr(&mut self, src: RegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .two_byte_op64_rm(OP2_MOVD_VdEd, dst as _, src);
    }

    pub fn movapd_rr(&mut self, src: XMMRegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .two_byte_op_rm(OP2_MOVAPD_VpdWpd, dst as _, src.into());
    }

    pub fn movaps_rr(&mut self, src: XMMRegisterID, dst: XMMRegisterID) {
        self.formatter
            .two_byte_op_rm(OP2_MOVAPS_VpdWpd, dst as _, src.into());
    }

    pub fn movsd_rr(&mut self, src: XMMRegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_rm(OP2_MOVSD_VsdWsd, dst as _, src.into());
    }

    pub fn movsd_rm(&mut self, src: XMMRegisterID, offset: i32, base: RegisterID) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_off(OP2_MOVSD_WsdVsd, src as _, base, offset);
    }

    pub fn movsd_rm_scaled(
        &mut self,
        src: XMMRegisterID,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
    ) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_scaled(OP2_MOVSD_WsdVsd, src as _, base, index, scale, offset);
    }
    pub fn movss_rr(&mut self, src: XMMRegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op_rm(OP2_MOVSD_VsdWsd, dst as _, src.into());
    }

    pub fn movss_rm(&mut self, src: XMMRegisterID, offset: i32, base: RegisterID) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op_off(OP2_MOVSD_WsdVsd, src as _, base, offset);
    }

    pub fn movss_rm_scaled(
        &mut self,
        src: XMMRegisterID,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
    ) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op_scaled(OP2_MOVSD_WsdVsd, src as _, base, index, scale, offset);
    }

    pub fn movsd_mr_addr(&mut self, addr: u32, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_x86addr(OP2_MOVSD_VsdWsd, dst as _, addr);
    }
    pub fn movsd_mr(&mut self, offset: i32, base: RegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_off(OP2_MOVSD_VsdWsd, dst as _, base, offset);
    }

    pub fn movsd_mr_scaled(
        &mut self,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
        dst: XMMRegisterID,
    ) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_scaled(OP2_MOVSD_VsdWsd, dst as _, base, index, scale, offset);
    }
    pub fn movss_mr(&mut self, offset: i32, base: RegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op_off(OP2_MOVSD_VsdWsd, dst as _, base, offset);
    }
    pub fn movss_mr_addr(&mut self, addr: u32, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op_x86addr(OP2_MOVSD_VsdWsd, dst as _, addr);
    }
    pub fn movss_mr_scaled(
        &mut self,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
        dst: XMMRegisterID,
    ) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op_scaled(OP2_MOVSD_VsdWsd, dst as _, base, index, scale, offset);
    }

    pub fn mulsd_rr(&mut self, src: XMMRegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_rm(OP2_MULSD_VsdWsd, dst as _, src.into());
    }

    pub fn mulsd_mr(&mut self, offset: i32, base: RegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_off(OP2_MULSD_VsdWsd, dst as _, base, offset);
    }
    pub fn mulsd_mr_scaled(
        &mut self,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
        dst: XMMRegisterID,
    ) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_scaled(OP2_MULSD_VsdWsd, dst as _, base, index, scale, offset);
    }
    pub fn mulss_rr(&mut self, src: XMMRegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op_rm(OP2_MULSD_VsdWsd, dst as _, src.into());
    }

    pub fn mulss_mr(&mut self, offset: i32, base: RegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op_off(OP2_MULSD_VsdWsd, dst as _, base, offset);
    }
    pub fn mulss_mr_scaled(
        &mut self,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
        dst: XMMRegisterID,
    ) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op_scaled(OP2_MULSD_VsdWsd, dst as _, base, index, scale, offset);
    }

    pub fn subsd_rr(&mut self, src: XMMRegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_rm(OP2_SUBSD_VsdWsd, dst as _, src.into());
    }

    pub fn subsd_mr(&mut self, offset: i32, base: RegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_off(OP2_SUBSD_VsdWsd, dst as _, base, offset);
    }
    pub fn subsd_mr_scaled(
        &mut self,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
        dst: XMMRegisterID,
    ) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_scaled(OP2_SUBSD_VsdWsd, dst as _, base, index, scale, offset);
    }
    pub fn subss_rr(&mut self, src: XMMRegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op_rm(OP2_SUBSD_VsdWsd, dst as _, src.into());
    }

    pub fn subss_mr(&mut self, offset: i32, base: RegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op_off(OP2_SUBSD_VsdWsd, dst as _, base, offset);
    }
    pub fn subss_mr_scaled(
        &mut self,
        offset: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
        dst: XMMRegisterID,
    ) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op_scaled(OP2_SUBSD_VsdWsd, dst as _, base, index, scale, offset);
    }

    pub fn ucomisd_rr(&mut self, src: XMMRegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .two_byte_op_rm(OP2_UCOMISD_VsdWsd, dst as _, src.into());
    }

    pub fn ucomisd_mr(&mut self, offset: i32, base: RegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .two_byte_op_off(OP2_UCOMISD_VsdWsd, dst as _, base, offset);
    }
    pub fn ucomiss_rr(&mut self, src: XMMRegisterID, dst: XMMRegisterID) {
        self.formatter
            .two_byte_op_rm(OP2_UCOMISD_VsdWsd, dst as _, src.into());
    }

    pub fn ucomiss_mr(&mut self, offset: i32, base: RegisterID, dst: XMMRegisterID) {
        self.formatter
            .two_byte_op_off(OP2_UCOMISD_VsdWsd, dst as _, base, offset);
    }

    pub fn divsd_rr(&mut self, src: XMMRegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_rm(OP2_DIVSD_VsdWsd, dst as _, src.into());
    }

    pub fn divsd_mr(&mut self, offset: i32, base: RegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_off(OP2_DIVSD_VsdWsd, dst as _, base, offset);
    }
    pub fn divss_rr(&mut self, src: XMMRegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op_rm(OP2_DIVSD_VsdWsd, dst as _, src.into());
    }

    pub fn divss_mr(&mut self, offset: i32, base: RegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op_off(OP2_DIVSD_VsdWsd, dst as _, base, offset);
    }

    pub fn andps_rr(&mut self, src: XMMRegisterID, dst: XMMRegisterID) {
        self.formatter
            .two_byte_op_rm(OP2_ANDPS_VpdWpd, dst as _, src.into());
    }

    pub fn orps_rr(&mut self, src: XMMRegisterID, dst: XMMRegisterID) {
        self.formatter
            .two_byte_op_rm(OP2_ORPS_VpdWpd, dst as _, src.into());
    }

    pub fn xorps_rr(&mut self, src: XMMRegisterID, dst: XMMRegisterID) {
        self.formatter
            .two_byte_op_rm(OP2_XORPD_VpdWpd, dst as _, src.into());
    }

    pub fn xorpd_rr(&mut self, src: XMMRegisterID, dst: XMMRegisterID) {
        if src == dst {
            self.xorps_rr(src, dst);
            return;
        }
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .two_byte_op_rm(OP2_XORPD_VpdWpd, dst as _, src.into());
    }

    pub fn andnpd_rr(&mut self, src: XMMRegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter
            .two_byte_op_rm(OP2_ANDNPD_VpdWpd, dst as _, src.into());
    }

    pub fn sqrtsd_rr(&mut self, src: XMMRegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_rm(OP2_SQRTSD_VsdWsd, dst as _, src.into());
    }

    pub fn sqrtsd_mr(&mut self, offset: i32, base: RegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_F2);
        self.formatter
            .two_byte_op_off(OP2_SQRTSD_VsdWsd, dst as _, base, offset);
    }

    pub fn sqrtss_rr(&mut self, src: XMMRegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op_rm(OP2_SQRTSD_VsdWsd, dst as _, src.into());
    }

    pub fn sqrtss_mr(&mut self, offset: i32, base: RegisterID, dst: XMMRegisterID) {
        self.formatter.prefix(PRE_SSE_F3);
        self.formatter
            .two_byte_op_off(OP2_SQRTSD_VsdWsd, dst as _, base, offset);
    }

    pub fn roundss_rr(&mut self, src: XMMRegisterID, dst: XMMRegisterID, rounding: Rounding) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter.three_byte_op_rm(
            OP2_3BYTE_ESCAPE_3A,
            OP3_ROUNDSS_VssWssIb,
            dst as _,
            src.into(),
        );
        self.formatter.buffer.put_byte(rounding as i8);
    }

    pub fn roundsd_rr(&mut self, src: XMMRegisterID, dst: XMMRegisterID, rounding: Rounding) {
        self.formatter.prefix(PRE_SSE_66);
        self.formatter.three_byte_op_rm(
            OP2_3BYTE_ESCAPE_3A,
            OP3_ROUNDSD_VssWssIb,
            dst as _,
            src.into(),
        );
        self.formatter.buffer.put_byte(rounding as i8);
    }

    pub fn set_int32(where_: *mut u8, value: i32) {
        crate::utils::unaligned_store((where_ as usize - 4) as *mut u8, value);
    }

    pub fn set_int8(where_: *mut u8, value: i8) {
        crate::utils::unaligned_store((where_ as usize - 1) as *mut u8, value);
    }

    pub fn set_pointer(where_: *mut u8, value: *mut u8) {
        unsafe {
            crate::utils::unaligned_store::<*mut u8>(
                where_.cast::<*mut u8>().offset(-1).cast(),
                value,
            );
        }
    }

    pub fn set_rel32(from: *mut u8, to: *mut u8) {
        let offset = to as isize - from as isize;
        assert_eq!(offset as i32 as isize, offset);
        Self::set_int32(from, offset as i32);
    }

    pub fn get_reloc_addr(code: *mut u8, label: AsmLabel) -> *mut u8 {
        assert!(label.is_set());
        return (code as usize + label.offset() as usize) as *mut u8;
    }
    pub fn get_call_return_offset(call: AsmLabel) -> u32 {
        call.offset()
    }

    pub fn replace_with_addr_computation(start: *mut u8, x64: bool) {
        unsafe {
            let mut ptr = start;
            if x64 {
                if (*ptr & !15) == PRE_REX {
                    ptr = ptr.offset(1);
                }
            }
            match *ptr {
                OP_MOV_GvEv => {
                    *ptr = OP_LEA;
                }
                OP_LEA => {}
                _ => unreachable!(),
            }
        }
    }
    pub fn replace_with_load(start: *mut u8, x64: bool) {
        unsafe {
            let mut ptr = start;
            if x64 {
                if (*ptr & !15) == PRE_REX {
                    ptr = ptr.offset(1);
                }
            }
            match *ptr {
                OP_MOV_GvEv => {}
                OP_LEA => {
                    *ptr = OP_MOV_GvEv;
                }
                _ => unreachable!(),
            }
        }
    }

    pub fn revert_jump_to_movq_i64r(start: *mut u8, imm: i64, dst: RegisterID) {
        let instruction_size = 10; // REX.W MOV IMM64
        let rex_bytes = 1;
        let opcode_bytes = 1;
        let ptr = start;
        unsafe {
            *ptr = PRE_REX | (1 << 3) | (dst as i32 >> 3) as u8;
            *ptr.offset(1) = OP_MOV_EAXIv | (dst as i32 & 7) as u8;
            let bytes = imm.to_ne_bytes();
            for i in rex_bytes + opcode_bytes..instruction_size {
                *ptr.offset(i as _) = bytes[i - rex_bytes - opcode_bytes];
            }
        }
    }
    pub fn revert_jump_to_movl_i32r(start: *mut u8, imm: i32, dst: RegisterID) {
        let instruction_size = 6; // REX.W MOV IMM64
        let rex_bytes = 1;
        let opcode_bytes = 1;
        let ptr = start;
        unsafe {
            *ptr = PRE_REX | (dst as i32 >> 3) as u8;
            *ptr.offset(1) = OP_MOV_EAXIv | (dst as i32 & 7) as u8;
            let bytes = imm.to_ne_bytes();
            for i in rex_bytes + opcode_bytes..instruction_size {
                *ptr.offset(i as _) = bytes[i - rex_bytes - opcode_bytes];
            }
        }
    }
    pub fn align(&mut self, alignment: usize) -> AsmLabel {
        while !self.formatter.buffer.is_aligned(alignment) {
            self.formatter.one_byte_op(OP_HLT);
        }
        self.label()
    }
    pub fn replace_with_jump(start: *mut u8, to: *mut u8) {
        unsafe {
            let dist = to as usize - (start as usize + 5);
            crate::utils::unaligned_store(start, OP_JMP_rel32);
            crate::utils::unaligned_store(start.offset(1), dist);
        }
    }
    pub fn link_jump(&mut self, from: AsmLabel, to: AsmLabel) {
        let code = self.formatter.buffer.storage.as_mut_ptr();
        Self::set_rel32(
            (code as usize + from.offset() as usize) as *mut u8,
            (code as usize + to.offset() as usize) as *mut u8,
        );
    }
    pub fn link_pointer_or_call(code: *mut u8, label: AsmLabel, value: *mut u8) {
        Self::set_pointer((code as usize + label.offset() as usize) as *mut u8, value);
    }

    pub fn slink_jump(code: *mut u8, from: AsmLabel, to: *mut u8) {
        Self::set_rel32((code as usize + from.offset() as usize) as *mut u8, to);
    }

    pub fn repatch_pointer(where_: *mut u8, value: *mut u8) {
        Self::set_pointer(where_, value);
    }

    pub fn revert_jump_to_cmpl_im_force32(start: *mut u8, imm: i32, dst: RegisterID) {
        let op_bytes: usize = 1;
        let modrm_bytes: usize = 1;
        let ptr = start;

        unsafe {
            *ptr = OP_GROUP1_EvIz;
            *ptr.offset(1) =
                ((ModRmMode::Register as i32) << 6) as u8 | (GROUP1_OP_CMP << 3) | dst as u8;
            let bytes = imm.to_ne_bytes();
            for i in op_bytes + modrm_bytes..5 {
                *ptr.offset(i as _) = bytes[i - op_bytes - modrm_bytes];
            }
        }
    }
}

use super::assembler_buffer::*;

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Debug, Hash)]
#[repr(u8)]
pub enum ModRmMode {
    NoDisp = 0,
    Disp8 = 1 << 6,
    Disp32 = 2 << 6,
    Register = 3 << 6,
}

pub struct X86AsmFormatter {
    pub buffer: AsmBuffer,
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
        OP_ADD_EbGb = 0x00,
        OP_ADD_EvGv = 0x01,
        OP_ADD_GvEv = 0x03,
        OP_ADD_EAXIv = 0x05,
        OP_OR_EvGb = 0x08,
        OP_OR_EvGv = 0x09,
        OP_OR_GvEv = 0x0B,
        OP_OR_EAXIv = 0x0D,
        OP_2BYTE_ESCAPE = 0x0F,
        OP_AND_EvGb = 0x20,
        OP_AND_EvGv = 0x21,
        OP_AND_GvEv = 0x23,
        OP_SUB_EvGb = 0x28,
        OP_SUB_EvGv = 0x29,
        OP_SUB_GvEv = 0x2B,
        OP_SUB_EAXIv = 0x2D,
        PRE_PREDICT_BRANCH_NOT_TAKEN = 0x2E,
        OP_XOR_EvGb = 0x30,
        OP_XOR_EvGv = 0x31,
        OP_XOR_GvEv = 0x33,
        OP_XOR_EAXIv = 0x35,
        OP_CMP_EvGv = 0x39,
        OP_CMP_GvEv = 0x3B,
        OP_CMP_EAXIv = 0x3D,
        PRE_REX = 0x40,
        OP_PUSH_EAX = 0x50,
        OP_POP_EAX = 0x58,
        OP_MOVSXD_GvEv = 0x63,
        PRE_GS = 0x65,
        PRE_OPERAND_SIZE = 0x66,
        PRE_SSE_66 = 0x66,
        OP_PUSH_Iz = 0x68,
        OP_IMUL_GvEvIz = 0x69,
        OP_GROUP1_EbIb = 0x80,
        OP_GROUP1_EvIz = 0x81,
        OP_GROUP1_EvIb = 0x83,
        OP_TEST_EbGb = 0x84,
        OP_TEST_EvGv = 0x85,
        OP_XCHG_EvGb = 0x86,
        OP_XCHG_EvGv = 0x87,
        OP_MOV_EbGb = 0x88,
        OP_MOV_EvGv = 0x89,
        OP_MOV_GvEv = 0x8B,
        OP_LEA = 0x8D,
        OP_GROUP1A_Ev = 0x8F,
        OP_NOP = 0x90,
        OP_XCHG_EAX = 0x90,
        OP_PAUSE = 0x90,
        OP_CDQ = 0x99,
        OP_MOV_EAXOv = 0xA1,
        OP_MOV_OvEAX = 0xA3,
        OP_TEST_ALIb = 0xA8,
        OP_TEST_EAXIv = 0xA9,
        OP_MOV_EAXIv = 0xB8,
        OP_GROUP2_EvIb = 0xC1,
        OP_RET = 0xC3,
        OP_GROUP11_EvIb = 0xC6,
        OP_GROUP11_EvIz = 0xC7,
        OP_INT3 = 0xCC,
        OP_GROUP2_Ev1 = 0xD1,
        OP_GROUP2_EvCL = 0xD3,
        OP_ESCAPE_D9 = 0xD9,
        OP_ESCAPE_DD = 0xDD,
        OP_CALL_rel32 = 0xE8,
        OP_JMP_rel32 = 0xE9,
        PRE_LOCK = 0xF0,
        PRE_SSE_F2 = 0xF2,
        PRE_SSE_F3 = 0xF3,
        OP_HLT = 0xF4,
        OP_GROUP3_Eb = 0xF6,
        OP_GROUP3_EbIb = 0xF6,
        OP_GROUP3_Ev = 0xF7,
        OP_GROUP3_EvIz = 0xF7, // OP_GROUP3_Ev has an immediate, when instruction is a test.
        OP_GROUP5_Ev = 0xFF
    );
}
pub mod TwoByteOpcodeID {
    c!(
        OP2_UD2 = 0xB,
        OP2_MOVSD_VsdWsd = 0x10,
        OP2_MOVSD_WsdVsd = 0x11,
        OP2_MOVSS_VsdWsd = 0x10,
        OP2_MOVSS_WsdVsd = 0x11,
        OP2_MOVAPD_VpdWpd = 0x28,
        OP2_MOVAPS_VpdWpd = 0x28,
        OP2_CVTSI2SD_VsdEd = 0x2A,
        OP2_CVTTSD2SI_GdWsd = 0x2C,
        OP2_CVTTSS2SI_GdWsd = 0x2C,
        OP2_UCOMISD_VsdWsd = 0x2E,
        OP2_RDTSC = 0x31,
        OP2_3BYTE_ESCAPE_3A = 0x3A,
        OP2_CMOVCC = 0x40,
        OP2_ADDSD_VsdWsd = 0x58,
        OP2_MULSD_VsdWsd = 0x59,
        OP2_CVTSD2SS_VsdWsd = 0x5A,
        OP2_CVTSS2SD_VsdWsd = 0x5A,
        OP2_SUBSD_VsdWsd = 0x5C,
        OP2_DIVSD_VsdWsd = 0x5E,
        OP2_MOVMSKPD_VdEd = 0x50,
        OP2_SQRTSD_VsdWsd = 0x51,
        OP2_ANDPS_VpdWpd = 0x54,
        OP2_ANDNPD_VpdWpd = 0x55,
        OP2_ORPS_VpdWpd = 0x56,
        OP2_XORPD_VpdWpd = 0x57,
        OP2_MOVD_VdEd = 0x6E,
        OP2_MOVD_EdVd = 0x7E,
        OP2_JCC_rel32 = 0x80,
        OP_SETCC = 0x90,
        OP2_CPUID = 0xA2,
        OP2_3BYTE_ESCAPE_AE = 0xAE,
        OP2_IMUL_GvEv = 0xAF,
        OP2_CMPXCHGb = 0xB0,
        OP2_CMPXCHG = 0xB1,
        OP2_BTR = 0xB3,
        OP2_MOVZX_GvEb = 0xB6,
        OP2_POPCNT = 0xB8,
        OP2_GROUP_BT_EvIb = 0xBA,
        OP2_BT_EvEv = 0xA3,
        OP2_BSF = 0xBC,
        OP2_TZCNT = 0xBC,
        OP2_BSR = 0xBD,
        OP2_LZCNT = 0xBD,
        OP2_MOVSX_GvEb = 0xBE,
        OP2_MOVZX_GvEw = 0xB7,
        OP2_MOVSX_GvEw = 0xBF,
        OP2_XADDb = 0xC0,
        OP2_XADD = 0xC1,
        OP2_PEXTRW_GdUdIb = 0xC5,
        OP2_BSWAP = 0xC8,
        OP2_PSLLQ_UdqIb = 0x73,
        OP2_PSRLQ_UdqIb = 0x73,
        OP2_POR_VdqWdq = 0xEB,
        OP3_ROUNDSS_VssWssIb = 0x0A,
        OP3_ROUNDSD_VssWssIb = 0x0B,
        OP3_LFENCE = 0xe8,
        OP3_MFENCE = 0xf0,
        OP3_SFENCE = 0xf8
    );
}
use OneByteOpcodeId::*;
use TwoByteOpcodeID::*;
fn jcc_rel32(c: Condition) -> u8 {
    return OP2_JCC_rel32 + c as u8;
}

fn cmovcc(c: Condition) -> u8 {
    OP2_CMOVCC + c as u8
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
        GROUP3_OP_DIV = 6,
        GROUP3_OP_IDIV = 7,
        GROUP5_OP_CALLN = 2,
        GROUP5_OP_JMPN = 4,
        GROUP5_OP_PUSH = 6,
        GROUP11_MOV = 0,
        GROUP14_OP_PSLLQ = 6,
        GROUP14_OP_PSRLQ = 2,
        ESCAPE_D9_FSTP_singleReal = 3,
        ESCAPE_DD_FSTP_doubleReal = 3,
        GROUP_BT_OP_BT = 4
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

    pub fn one_byte_op64_scaled(
        &mut self,
        op: u8,
        reg: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
        offset: i32,
    ) {
        self.emit_rexw(reg, index as _, base as _);
        self.buffer.put_byte(op as _);
        self.memory_modrm_scaled(reg, base, index, scale, offset);
    }

    pub fn one_byte_op64_addr(&mut self, op: u8, reg: i32, addr: u32) {
        self.emit_rexw(reg, 0, 0);
        self.buffer.put_byte(op as _);
        self.memory_modrm_addr(reg, addr);
    }
    pub fn two_byte_op64_rm(&mut self, op: u8, reg: i32, rm: RegisterID) {
        self.emit_rexw(reg, 0, rm as _);
        self.buffer.put_byte(OP_2BYTE_ESCAPE as _);
        self.buffer.put_byte(op as _);
        self.register_modrm(reg, rm);
    }

    pub fn two_byte_op64_off(&mut self, op: u8, reg: i32, base: RegisterID, offset: i32) {
        self.emit_rexw(reg, 0, base as _);
        self.buffer.put_byte(OP_2BYTE_ESCAPE as _);
        self.buffer.put_byte(op as _);
        self.memory_modrm(reg, base, offset);
    }

    pub fn two_byte_op64_scaled(
        &mut self,
        op: u8,
        reg: i32,
        base: RegisterID,
        index: RegisterID,
        scale: i32,
        offset: i32,
    ) {
        self.emit_rexw(reg, index as _, base as _);
        self.buffer.put_byte(OP_2BYTE_ESCAPE as _);
        self.buffer.put_byte(op as _);
        self.memory_modrm_scaled(reg, base, index, scale, offset);
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
    pub fn one_byte_op8_off(&mut self, op: u8, reg: i32, base: RegisterID, offset: i32) {
        self.emit_rex_if(
            self.byte_reg_requires_rex(base as _) | self.byte_reg_requires_rex(reg),
            reg,
            0,
            base as _,
        );
        self.buffer.put_byte(op as _);
        self.memory_modrm(reg, base, offset);
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
    pub fn two_byte_op8_rm(&mut self, op: u8, reg: i32, rm: RegisterID) {
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

    pub fn two_byte_op8_off(&mut self, op: u8, reg: i32, base: RegisterID, offset: i32) {
        self.emit_rex_if(
            self.byte_reg_requires_rex(reg) | self.byte_reg_requires_rex(base as _),
            reg,
            0,
            base as _,
        );
        self.buffer.put_byte(OP_2BYTE_ESCAPE as _);
        self.buffer.put_byte(op as _);
        self.memory_modrm(reg, base, offset);
    }

    pub fn two_byte_op8_scaled(
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
                || self.byte_reg_requires_rex(base as _)
                || self.byte_reg_requires_rex(index as _),
            reg,
            index as _,
            base as _,
        );
        self.buffer.put_byte(OP_2BYTE_ESCAPE as _);
        self.buffer.put_byte(op as _);
        self.memory_modrm_scaled(reg, base, index, scale, offset);
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
            .put_byte((mode as i8) | ((reg & 7) << 3) as i8 | (rm as i8 & 7));
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
        if !self.x64 {
            self.put_modrm(ModRmMode::NoDisp, reg, Self::NO_BASE);
        } else {
            self.put_modrm_sib(ModRmMode::NoDisp, reg, Self::NO_BASE, Self::NO_IX, 0);
        }
        self.buffer.put_int(addr as _);
    }

    pub fn three_byte_op(&mut self, prefix: u8, opcode: u8) {
        self.buffer.put_byte(OP_2BYTE_ESCAPE as _);
        self.buffer.put_byte(prefix as _);
        self.buffer.put_byte(opcode as _);
    }

    pub fn three_byte_op_rm(&mut self, prefix: u8, op: u8, reg: i32, rm: RegisterID) {
        self.emit_rex_if_needed(reg as _, 0, rm as _);
        self.buffer.put_byte(OP_2BYTE_ESCAPE as _);
        self.buffer.put_byte(prefix as _);
        self.buffer.put_byte(op as _);
        self.register_modrm(reg, rm);
    }

    pub fn three_byte_op_disp(
        &mut self,
        prefix: u8,
        op: u8,
        reg: i32,
        base: RegisterID,
        disp: i32,
    ) {
        self.emit_rex_if_needed(reg, 0, base as _);
        self.buffer.put_byte(OP_2BYTE_ESCAPE as _);
        self.buffer.put_byte(prefix as _);
        self.buffer.put_byte(op as _);
        self.memory_modrm(reg, base, disp);
    }
}
