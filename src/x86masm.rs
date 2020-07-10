use crate::{assembler_buffer::*, x86_assembler::*, *};

pub const DOUBLE_CONDITION_BIT_SPECIAL: u8 = 0x20;
pub const DOUBLE_CONDITION_BIT_INVERT: u8 = 0x10;
pub const DOUBLE_CONDITION_BITS: u8 = 0x10 | 0x20;
pub const REPATCH_OFFSET_CALL_R11: usize = 3;
#[derive(Copy, Clone, PartialEq, Eq, Default)]
pub struct DataLabelPtr {
    label: AsmLabel,
}

impl DataLabelPtr {
    pub fn new(masm: &mut MacroAssemblerX86) -> Self {
        Self {
            label: masm.asm.label(),
        }
    }

    pub fn asm_label(&self) -> AsmLabel {
        self.label
    }

    pub fn is_set(&self) -> bool {
        self.label.is_set()
    }
}

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
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
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
#[repr(u8)]
pub enum ResultCondition {
    Overflow = Condition::O as u8,
    Signed = Condition::S as u8,
    PositiveOrZero = Condition::NS as u8,
    Zero = Condition::E as u8,
    NonZero = Condition::NE as u8,
}
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
#[repr(u8)]
pub enum FpCondition {
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

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Default)]
pub struct Label {
    label: AsmLabel,
}

impl Label {
    pub fn asm_label(&self) -> AsmLabel {
        self.label
    }
}

pub const SCRATCH_REG: RegisterID = RegisterID::R11;
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Default)]
pub struct Jump {
    label: AsmLabel,
}

impl Jump {
    pub fn new(l: AsmLabel) -> Self {
        Self { label: l }
    }
    pub fn link(&self, asm: &mut MacroAssemblerX86) {
        let lbl = asm.asm.label();
        asm.asm.link_jump(self.label, lbl);
    }
    pub fn link_to(&self, asm: &mut MacroAssemblerX86, to: Label) {
        asm.asm.link_jump(self.label, to.label);
    }

    pub fn label(&self) -> Label {
        Label { label: self.label }
    }
}

pub struct PatchableJump {
    pub j: Jump,
}

impl PatchableJump {
    pub fn new(j: Jump) -> Self {
        Self { j }
    }
}

pub enum Mem {
    // rbp + val1
    Local(i32),

    // reg1 + val1
    Base(RegisterID, i32),

    // reg1 + reg2 * val1 + val2
    Index(RegisterID, RegisterID, i32, i32),

    // reg1 * val1 + val2
    Offset(RegisterID, i32, i32),
}

pub enum Scale {
    TimesOne,
    TimesTwo,
    TimesFour,
    TimesEight,
}

pub fn with_swapped_register(
    original: RegisterID,
    left: RegisterID,
    right: RegisterID,
) -> RegisterID {
    if original == left {
        right
    } else if original == right {
        left
    } else {
        original
    }
}

#[derive(Default)]

pub struct JumpList {
    pub jumps: Vec<Jump>,
}

impl JumpList {
    pub fn new() -> Self {
        Self {
            jumps: Vec::with_capacity(2),
        }
    }
    pub fn link(&mut self, asm: &mut MacroAssemblerX86) {
        for j in self.jumps.iter() {
            j.link(asm);
        }
    }
    pub fn link_to(&mut self, asm: &mut MacroAssemblerX86, to: Label) {
        for j in self.jumps.iter() {
            j.link_to(asm, to);
        }
    }

    pub fn push(&mut self, j: Jump) {
        self.jumps.push(j);
    }
}

pub struct MacroAssemblerX86 {
    pub asm: X86Asm,
    pub link_tasks: Vec<Box<dyn FnOnce(&mut LinkBuffer<Self>)>>,
    x64: bool,
}

impl MacroAssemblerX86 {
    pub fn new(x64: bool) -> Self {
        Self {
            x64,
            link_tasks: vec![],
            asm: X86Asm::new(x64),
        }
    }
    pub fn add_link_task(&mut self, task: Box<dyn FnOnce(&mut LinkBuffer<Self>)>) {
        self.link_tasks.push(task);
    }
    pub fn pad_before_patch(&mut self) {
        self.asm.label();
    }

    pub fn move_with_patch_ptr(&mut self, initial_value: usize, dest: RegisterID) -> DataLabelPtr {
        self.asm.movq_i64r(initial_value as _, dest);
        DataLabelPtr::new(self)
    }
    pub fn load32(&mut self, mem: Mem, dest: RegisterID) {
        match mem {
            Mem::Base(base, offset) => {
                self.asm.movl_mr(offset, base, dest);
            }
            Mem::Local(ix) => {
                self.asm.movl_mr(ix, RegisterID::EBP, dest);
            }
            Mem::Index(base, index, scale, offset) => {
                self.asm.movl_mr_scaled(offset, base, index, scale, dest);
            }
            _ => unreachable!(),
        }
    }

    pub fn store32(&mut self, src: RegisterID, dest: Mem) {
        match dest {
            Mem::Base(base, offset) => self.asm.movl_rm(src, offset, base),
            Mem::Local(ix) => self.asm.movl_rm(src, ix, RegisterID::EBP),
            Mem::Index(base, index, scale, offset) => {
                self.asm.movl_rm_scaled(src, offset, base, index, scale)
            }
            _ => unreachable!(),
        }
    }

    pub fn store32_imm(&mut self, imm: i32, dest: Mem) {
        match dest {
            Mem::Base(base, offset) => self.asm.movl_i32m(imm, offset, base),
            Mem::Local(ix) => self.asm.movl_i32m(imm, ix, RegisterID::EBP),
            Mem::Index(base, index, scale, offset) => {
                self.asm.movl_i32m_scaled(imm, offset, base, index, scale)
            }
            _ => unreachable!(),
        }
    }

    pub fn store16(&mut self, src: RegisterID, dest: Mem) {
        match dest {
            Mem::Base(base, offset) => self.asm.movw_rm(src, offset, base),
            Mem::Local(ix) => self.asm.movw_rm(src, ix, RegisterID::EBP),
            Mem::Index(base, index, scale, offset) => {
                self.asm.movw_rm_scaled(src, offset, base, index, scale)
            }
            _ => unreachable!(),
        }
    }
    pub fn store16_imm(&mut self, imm: i16, dest: Mem) {
        match dest {
            Mem::Base(base, offset) => self.asm.movw_im(imm, offset, base),
            Mem::Local(ix) => self.asm.movw_im(imm, ix, RegisterID::EBP),
            Mem::Index(base, index, scale, offset) => {
                self.asm.movw_im_scaled(imm, offset, base, index, scale)
            }
            _ => unreachable!(),
        }
    }

    pub fn store8(&mut self, src: RegisterID, dest: Mem) {
        match dest {
            Mem::Base(base, offset) => self.asm.movb_rm(src, offset, base),
            Mem::Local(ix) => self.asm.movb_rm(src, ix, RegisterID::EBP),
            Mem::Index(base, index, scale, offset) => {
                self.asm.movb_rm_scaled(src, offset, base, index, scale)
            }
            _ => unreachable!(),
        }
    }
    pub fn store8_imm(&mut self, imm: i8, dest: Mem) {
        match dest {
            Mem::Base(base, offset) => self.asm.movb_i8m(imm, offset, base),
            Mem::Local(ix) => self.asm.movb_i8m(imm, ix, RegisterID::EBP),
            Mem::Index(base, index, scale, offset) => {
                self.asm.movb_i8m_scaled(imm, offset, base, index, scale)
            }
            _ => unreachable!(),
        }
    }
    pub fn load16(&mut self, mem: Mem, dest: RegisterID) {
        match mem {
            Mem::Base(base, offset) => {
                self.asm.movzwl_mr(offset, base, dest);
            }
            Mem::Local(ix) => {
                self.asm.movzwl_mr(ix, RegisterID::EBP, dest);
            }
            Mem::Index(base, index, scale, offset) => {
                self.asm.movzwl_mr_scaled(offset, base, index, scale, dest);
            }
            _ => unreachable!(),
        }
    }

    pub fn load8(&mut self, mem: Mem, dest: RegisterID) {
        match mem {
            Mem::Base(base, offset) => {
                self.asm.movzbl_mr(offset, base, dest);
            }
            Mem::Local(ix) => {
                self.asm.movzbl_mr(ix, RegisterID::EBP, dest);
            }
            Mem::Index(base, index, scale, offset) => {
                self.asm.movzbl_mr_scaled(offset, base, index, scale, dest);
            }
            _ => unreachable!(),
        }
    }

    pub fn load8_sign_extend_to_32(&mut self, mem: Mem, dest: RegisterID) {
        match mem {
            Mem::Base(base, offset) => {
                self.asm.movsbl_mr(offset, base, dest);
            }
            Mem::Local(ix) => {
                self.asm.movsbl_mr(ix, RegisterID::EBP, dest);
            }
            Mem::Index(base, index, scale, offset) => {
                self.asm.movsbl_mr_scaled(offset, base, index, scale, dest);
            }
            _ => unreachable!(),
        }
    }

    pub fn add32_rr(&mut self, src: RegisterID, dst: RegisterID) {
        self.asm.addl_rr(src, dst);
    }

    pub fn add32_imm(&mut self, imm: i32, dst: RegisterID) {
        if imm == 1 {
            self.asm.inc_r(dst);
        } else {
            self.asm.addl_ir(imm, dst);
        }
    }

    pub fn add32_im(&mut self, imm: i32, mem: Mem) {
        match mem {
            Mem::Base(base, off) => {
                self.asm.addl_im(imm, off, base);
            }
            Mem::Local(ix) => {
                self.asm.addl_im(imm, ix, RegisterID::EBP);
            }
            _ => unimplemented!(),
        }
    }

    pub fn add8_im(&mut self, src: i8, dst: Mem) {
        match dst {
            Mem::Base(base, off) => {
                self.asm.addb_im(src, off, base);
            }
            Mem::Local(off) => {
                self.asm.addb_im(src, off, RegisterID::EBP);
            }
            Mem::Index(base, index, scale, offset) => {
                self.asm.addb_im_scaled(src, offset, base, index, scale);
            }
            _ => unimplemented!(),
        }
    }

    pub fn add16_im(&mut self, src: i16, dst: Mem) {
        match dst {
            Mem::Base(base, off) => {
                self.asm.addw_im(src, off, base);
            }
            Mem::Local(ix) => {
                self.asm.addw_im(src, ix, RegisterID::EBP);
            }

            Mem::Index(base, index, scale, offset) => {
                self.asm.addw_im_scaled(src, offset, base, index, scale);
            }
            _ => unimplemented!(),
        }
    }

    pub fn add32_rm(&mut self, src: RegisterID, dst: Mem) {
        match dst {
            Mem::Base(base, off) => {
                self.asm.addl_rm(src, off, base);
            }
            Mem::Index(base, index, scale, offset) => {
                self.asm.addl_rm_scaled(src, offset, base, index, scale);
            }
            Mem::Local(ix) => {
                self.asm.addl_rm(src, ix, RegisterID::EBP);
            }
            _ => unimplemented!(),
        }
    }

    pub fn add32_mr(&mut self, src: Mem, dst: RegisterID) {
        match src {
            Mem::Base(base, off) => {
                self.asm.addl_mr(off, base, dst);
            }
            Mem::Index(base, index, scale, offset) => {
                self.asm.addl_mr_scaled(offset, base, index, scale, dst);
            }
            Mem::Local(ix) => {
                self.asm.addl_mr(ix, RegisterID::EBP, dst);
            }
            _ => unimplemented!(),
        }
    }

    pub fn add32(&mut self, a: RegisterID, b: RegisterID, dst: RegisterID) {
        self.x86_lea32(Mem::Index(a, b, Scale::TimesOne as i32, 0), dst);
    }

    pub fn add32i(&mut self, imm: i32, b: RegisterID, dst: RegisterID) {
        if imm == 0 {
            self.zero_extend_32_to_ptr(b, dst);
        } else {
            if b == dst {
                self.add32_imm(imm, dst);
            } else {
                self.asm.leal_mr(imm, b, dst);
            }
        }
    }

    fn x86_lea32(&mut self, mem: Mem, dest: RegisterID) {
        match mem {
            Mem::Index(base, index, scale, offset) => {
                if scale == 0 && offset == 0 {
                    if base == dest {
                        self.add32_rr(index, dest);
                        return;
                    }
                    if index == dest {
                        self.add32_rr(base, dest);
                    }
                }
                self.asm.leal_mr_scaled(offset, base, index, scale, dest);
            }
            _ => unreachable!(),
        }
    }

    pub fn zero_extend_32_to_ptr(&mut self, src: RegisterID, dst: RegisterID) {
        if self.x64 {
            self.asm.movl_rr(src, dst);
        } else {
            self.move_rr(src, dst);
        }
    }
    pub fn zero_extend_i32_to_ptr(&mut self, src: i32, dst: RegisterID) {
        if self.x64 {
            self.asm.movl_i32r(src, dst);
        } else {
            self.move_i64(src as _, dst);
        }
    }
    pub fn move_i32(&mut self, imm: i32, dst: RegisterID) {
        if imm == 0 {
            self.asm.xorl_rr(dst, dst);
        } else {
            self.asm.movl_i32r(imm, dst);
        }
    }

    pub fn move_i64(&mut self, imm: i64, dst: RegisterID) {
        if self.x64 {
            if imm == 0 {
                self.asm.xorq_rr(dst, dst);
            } else {
                self.asm.movq_i64r(imm, dst);
            }
        } else {
            panic!();
        }
    }

    pub fn move32_if_needed(&mut self, src: RegisterID, dest: RegisterID) {
        if src == dest {
            return;
        }
        self.asm.movl_rr(src, dest);
    }

    pub fn move_fp_to_gp(&mut self, src: XMMRegisterID, dest: RegisterID) {
        self.asm.movq_f2i_rr(src, dest);
    }

    pub fn move_gp_to_fp(&mut self, src: RegisterID, dest: XMMRegisterID) {
        self.asm.movq_i2f_rr(src, dest);
    }
    pub fn move_rr(&mut self, src: RegisterID, dst: RegisterID) {
        if src != dst {
            if self.x64 {
                self.asm.movq_rr(src, dst);
            } else {
                self.asm.movl_rr(src, dst);
            }
        }
    }

    pub fn and32(&mut self, op1: RegisterID, op2: RegisterID, dest: RegisterID) {
        if op1 == dest {
            self.asm.andl_rr(op2, dest);
        } else {
            self.move32_if_needed(op2, dest);
            self.asm.andl_rr(op1, dest);
        }
    }

    pub fn and32_imm(&mut self, imm: i32, op1: RegisterID, dest: RegisterID) {
        self.move32_if_needed(op1, dest);
        self.asm.andl_ir(imm, dest);
    }
    pub fn lshift32_rr(&mut self, shift_amount: RegisterID, dest: RegisterID) {
        if shift_amount == RegisterID::ECX {
            self.asm.shll_clr(dest);
        } else {
            self.swap_gp(shift_amount, RegisterID::ECX);
            self.asm.shll_clr(if dest == RegisterID::ECX {
                shift_amount
            } else {
                dest
            });
            self.swap_gp(shift_amount, RegisterID::ECX);
        }
    }
    pub fn lshift32_ir(&mut self, imm: i32, dest: RegisterID) {
        self.asm.shll_i8r(imm as _, dest);
    }
    pub fn lshift32(&mut self, src: RegisterID, shift_amount: RegisterID, dest: RegisterID) {
        self.move32_if_needed(src, dest);
        self.lshift32_rr(shift_amount, dest);
    }

    pub fn lshift32_imm(&mut self, src: RegisterID, imm: i32, dest: RegisterID) {
        self.move32_if_needed(src, dest);
        self.lshift32_ir(imm, dest);
    }

    pub fn mul32_rr(&mut self, src: RegisterID, dest: RegisterID) {
        self.asm.imull_rr(src, dest);
    }

    pub fn mul32(&mut self, src1: RegisterID, src2: RegisterID, dest: RegisterID) {
        if src2 == dest {
            self.asm.imull_rr(src1, dest);
            return;
        }
        self.move32_if_needed(src1, dest);
        self.asm.imull_rr(src2, dest);
    }

    pub fn mul32_mr(&mut self, src: Mem, dest: RegisterID) {
        match src {
            Mem::Base(base, offset) => {
                self.asm.imull_mr(offset, base, dest);
            }
            Mem::Local(ix) => {
                self.asm.imull_mr(ix, RegisterID::EBP, dest);
            }
            _ => unreachable!(),
        }
    }

    pub fn mul32_mem(&mut self, op1: Mem, op2: RegisterID, dest: RegisterID) {
        if op2 == dest {
            self.mul32_mr(op1, dest);
        } else {
            match op1 {
                Mem::Base(b, ..) if b == dest => {
                    self.load32(op1, dest);
                    self.mul32_rr(op2, dest);
                }
                Mem::Base(..) => {
                    self.zero_extend_32_to_ptr(op2, dest);
                    self.mul32_mr(op1, dest);
                }
                _ => unreachable!(),
            }
        }
    }

    pub fn mul32_imm(&mut self, imm: i32, src: RegisterID, dest: RegisterID) {
        self.asm.imull_i32r(src, imm, dest);
    }

    pub fn x86div32(&mut self, denominator: RegisterID) {
        self.asm.idivl_r(denominator);
    }

    pub fn x86div32_rr(&mut self, eax: RegisterID, edx: RegisterID, denominator: RegisterID) {
        assert!(eax == RegisterID::EAX);
        assert!(edx == RegisterID::EDX);
        self.x86div32(denominator);
    }
    pub fn x86udiv32(&mut self, denominator: RegisterID) {
        self.asm.divl_r(denominator);
    }
    pub fn x86udiv32_rr(&mut self, eax: RegisterID, edx: RegisterID, denominator: RegisterID) {
        assert!(eax == RegisterID::EAX);
        assert!(edx == RegisterID::EDX);
        self.x86div32(denominator);
    }

    pub fn neg32_r(&mut self, dest: RegisterID) {
        self.asm.negl_r(dest);
    }

    pub fn neg32(&mut self, src: RegisterID, dest: RegisterID) {
        self.move32_if_needed(src, dest);
        self.neg32_r(dest);
    }

    pub fn neg32_mem(&mut self, mem: Mem) {
        match mem {
            Mem::Base(base, offset) => {
                self.asm.negl_m(offset, base);
            }
            Mem::Local(ix) => {
                self.asm.negl_m(ix, RegisterID::EBP);
            }
            _ => unreachable!(),
        }
    }

    pub fn or32_rr(&mut self, src: RegisterID, dst: RegisterID) {
        self.asm.orl_rr(src, dst);
    }

    pub fn or32(&mut self, op1: RegisterID, op2: RegisterID, dest: RegisterID) {
        if op1 == dest {
            self.or32_rr(op2, dest);
        } else {
            self.move32_if_needed(op2, dest);
            self.or32_rr(op1, dest);
        }
    }
    pub fn or32_mem(&mut self, mem: Mem, op2: RegisterID, dest: RegisterID) {
        if op2 == dest {
            self.or32_mr(mem, dest);
        } else {
            match mem {
                Mem::Base(b, ..) if b == dest => {
                    self.load32(mem, dest);
                    self.or32_rr(op2, dest);
                }
                _ => {
                    self.zero_extend_32_to_ptr(op2, dest);
                    self.or32_mr(mem, dest);
                }
            }
        }
    }
    pub fn or32_imm(&mut self, imm: i32, op2: RegisterID, dest: RegisterID) {
        self.move32_if_needed(op2, dest);
        self.asm.orl_ir(imm, dest);
    }
    pub fn or32_rm(&mut self, src: RegisterID, mem: Mem) {
        match mem {
            Mem::Base(base, offset) => {
                self.asm.orl_rm(src, offset, base);
            }
            Mem::Local(ix) => {
                self.asm.orl_rm(src, ix, RegisterID::EBP);
            }
            Mem::Index(base, index, scale, offset) => {
                self.asm.orl_rm_scaled(src, offset, base, index, scale);
            }
            _ => unreachable!(),
        }
    }
    pub fn or32_mr(&mut self, mem: Mem, dest: RegisterID) {
        match mem {
            Mem::Base(base, offset) => {
                self.asm.orl_mr(offset, base, dest);
            }
            Mem::Local(ix) => {
                self.asm.orl_mr(ix, RegisterID::EBP, dest);
            }
            Mem::Index(base, index, scale, offset) => {
                self.asm.orl_mr_scaled(offset, base, index, scale, dest);
            }
            _ => unreachable!(),
        }
    }

    pub fn swap_gp(&mut self, reg1: RegisterID, reg2: RegisterID) {
        if reg1 != reg2 {
            if self.x64 {
                self.asm.xchgq_rr(reg1, reg2);
            } else {
                self.asm.xchgl_rr(reg1, reg2);
            }
        }
    }
    pub fn rshift32_rr(&mut self, shift_amount: RegisterID, dest: RegisterID) {
        if shift_amount == RegisterID::ECX {
            self.asm.sarl_clr(dest);
        } else {
            self.swap_gp(shift_amount, RegisterID::ECX);
            self.asm.sarl_clr(if dest == RegisterID::ECX {
                shift_amount
            } else {
                dest
            });
            self.swap_gp(shift_amount, RegisterID::ECX);
        }
    }

    pub fn rshift32(&mut self, src: RegisterID, shift_amount: RegisterID, dest: RegisterID) {
        self.move32_if_needed(src, dest);
        self.rshift32_rr(shift_amount, dest);
    }

    pub fn rshift32_ir(&mut self, imm: i8, dest: RegisterID) {
        self.asm.sarl_i8r(imm, dest);
    }

    pub fn rshift32_imm(&mut self, src: RegisterID, imm: i8, dest: RegisterID) {
        self.move32_if_needed(src, dest);
        self.rshift32_ir(imm, dest);
    }

    pub fn urshift32_rr(&mut self, shift_amount: RegisterID, dest: RegisterID) {
        if shift_amount == RegisterID::ECX {
            self.asm.shrl_clr(dest);
        } else {
            self.swap_gp(shift_amount, RegisterID::ECX);
            self.asm.shrl_clr(if dest == RegisterID::ECX {
                shift_amount
            } else {
                dest
            });
            self.swap_gp(shift_amount, RegisterID::ECX);
        }
    }

    pub fn urshift32(&mut self, src: RegisterID, shift_amount: RegisterID, dest: RegisterID) {
        self.move32_if_needed(src, dest);
        self.rshift32_rr(shift_amount, dest);
    }

    pub fn urshift32_ir(&mut self, imm: i8, dest: RegisterID) {
        self.asm.shrl_i8r(imm, dest);
    }

    pub fn urshift32_imm(&mut self, src: RegisterID, imm: i8, dest: RegisterID) {
        self.move32_if_needed(src, dest);
        self.rshift32_ir(imm, dest);
    }

    pub fn sub32_rr(&mut self, src: RegisterID, dest: RegisterID) {
        self.asm.subl_rr(src, dest);
    }

    pub fn sub32(&mut self, src1: RegisterID, src2: RegisterID, dest: RegisterID) {
        if dest == src2 {
            self.neg32_r(dest);
            self.add32_rr(src1, dest);
            return;
        }
        self.move_rr(src1, dest);
        self.sub32_rr(src2, dest);
    }

    pub fn sub32_imm(&mut self, imm: i32, dest: RegisterID) {
        self.asm.subl_ir(imm, dest);
    }

    pub fn xor32_rr(&mut self, src: RegisterID, dest: RegisterID) {
        self.asm.xorl_rr(src, dest);
    }

    pub fn xor32(&mut self, op1: RegisterID, op2: RegisterID, dest: RegisterID) {
        if op1 == op2 {
            self.move_i32(0, dest);
        } else if op1 == dest {
            self.xor32_rr(op2, dest);
        } else {
            self.move32_if_needed(op2, dest);
            self.xor32_rr(op1, dest);
        }
    }

    pub fn xor32_imm(&mut self, imm: i32, op2: RegisterID, dest: RegisterID) {
        self.move32_if_needed(op2, dest);
        self.asm.xorl_ir(imm, dest);
    }

    pub fn not32(&mut self, r: RegisterID) {
        self.asm.notl_r(r);
    }

    pub fn sqrt_double(&mut self, src: XMMRegisterID, dest: XMMRegisterID) {
        self.asm.sqrtsd_rr(src, dest);
    }

    pub fn sqrt_float(&mut self, src: XMMRegisterID, dest: XMMRegisterID) {
        self.asm.sqrtss_rr(src, dest);
    }

    pub fn abs_double(&mut self, src: XMMRegisterID, dest: XMMRegisterID) {
        static NEGATIVE_ZERO_CONST: f64 = -1.0;
        self.load_double_at_addr(&NEGATIVE_ZERO_CONST, dest);
        self.asm.andnpd_rr(src, dest);
    }

    pub fn negate_double(&mut self, src: XMMRegisterID, dest: XMMRegisterID) {
        static NEGATIVE_ZERO_CONST: f64 = -1.0;
        self.load_double_at_addr(&NEGATIVE_ZERO_CONST, dest);
        self.asm.xorpd_rr(src, dest);
    }
    pub fn ceil_double(&mut self, src: XMMRegisterID, dest: XMMRegisterID) {
        self.asm.roundsd_rr(src, dest, Rounding::TowardInfiniti);
    }
    pub fn ceil_float(&mut self, src: XMMRegisterID, dest: XMMRegisterID) {
        self.asm.roundss_rr(src, dest, Rounding::TowardInfiniti);
    }
    pub fn floor_double(&mut self, src: XMMRegisterID, dest: XMMRegisterID) {
        self.asm
            .roundsd_rr(src, dest, Rounding::TowardNegativeInfiniti);
    }
    pub fn floor_float(&mut self, src: XMMRegisterID, dest: XMMRegisterID) {
        self.asm
            .roundss_rr(src, dest, Rounding::TowardNegativeInfiniti);
    }
    pub fn load_double_at_addr(&mut self, addr: *const f64, dest: XMMRegisterID) {
        if !self.x64 {
            self.asm.movsd_mr_addr(addr as u32, dest);
        } else {
            self.move_i64(addr as _, SCRATCH_REG);
            self.load_double(Mem::Base(SCRATCH_REG, 0), dest);
        }
    }

    pub fn load_double(&mut self, mem: Mem, dest: XMMRegisterID) {
        match mem {
            Mem::Base(base, offset) => {
                self.asm.movsd_mr(offset, base, dest);
            }
            Mem::Local(ix) => {
                self.asm.movsd_mr(ix, RegisterID::EBP, dest);
            }
            Mem::Index(base, index, scale, offset) => {
                self.asm.movsd_mr_scaled(offset, base, index, scale, dest);
            }
            _ => unreachable!(),
        }
    }
    pub fn load_float_at_addr(&mut self, addr: *const f64, dest: XMMRegisterID) {
        if !self.x64 {
            self.asm.movss_mr_addr(addr as u32, dest);
        } else {
            self.move_i64(addr as _, SCRATCH_REG);
            self.load_float(Mem::Base(SCRATCH_REG, 0), dest);
        }
    }

    pub fn load_float(&mut self, mem: Mem, dest: XMMRegisterID) {
        match mem {
            Mem::Base(base, offset) => {
                self.asm.movsd_mr(offset, base, dest);
            }
            Mem::Local(ix) => {
                self.asm.movsd_mr(ix, RegisterID::EBP, dest);
            }
            Mem::Index(base, index, scale, offset) => {
                self.asm.movsd_mr_scaled(offset, base, index, scale, dest);
            }
            _ => unreachable!(),
        }
    }

    pub fn convert_float_to_double(&mut self, src: XMMRegisterID, dest: XMMRegisterID) {
        self.asm.cvtss2sd_rr(src, dest);
    }

    pub fn convert_double_to_float(&mut self, src: XMMRegisterID, dest: XMMRegisterID) {
        self.asm.cvtsd2ss_rr(src, dest);
    }

    pub fn add_double_rr(&mut self, src: XMMRegisterID, dst: XMMRegisterID) {
        self.add_double(src, dst, dst);
    }

    pub fn add_double(&mut self, op1: XMMRegisterID, op2: XMMRegisterID, dest: XMMRegisterID) {
        if op1 == dest {
            self.asm.addsd_rr(op2, dest);
        } else {
            self.move_fp_double(op2, dest);
            self.asm.addsd_rr(op1, dest);
        }
    }

    pub fn add_float(&mut self, op1: XMMRegisterID, op2: XMMRegisterID, dest: XMMRegisterID) {
        if op1 == dest {
            self.asm.addss_rr(op2, dest);
        } else {
            self.move_fp_double(op2, dest);
            self.asm.addss_rr(op1, dest);
        }
    }

    pub fn div_double_rr(&mut self, src: XMMRegisterID, dest: XMMRegisterID) {
        self.asm.divsd_rr(src, dest);
    }

    pub fn div_double(&mut self, op1: XMMRegisterID, op2: XMMRegisterID, dest: XMMRegisterID) {
        self.move_fp_double(op1, dest);
        self.div_double_rr(op2, dest);
    }

    pub fn div_float_rr(&mut self, src: XMMRegisterID, dest: XMMRegisterID) {
        self.asm.divss_rr(src, dest);
    }

    pub fn div_float(&mut self, op1: XMMRegisterID, op2: XMMRegisterID, dest: XMMRegisterID) {
        self.move_fp_double(op1, dest);
        self.div_float_rr(op2, dest);
    }

    pub fn sub_double(&mut self, op1: XMMRegisterID, op2: XMMRegisterID, dest: XMMRegisterID) {
        self.move_fp_double(op1, dest);
        self.asm.subsd_rr(op2, dest);
    }

    pub fn sub_double_rr(&mut self, src: XMMRegisterID, dest: XMMRegisterID) {
        self.sub_double(src, dest, dest);
    }
    pub fn sub_float(&mut self, op1: XMMRegisterID, op2: XMMRegisterID, dest: XMMRegisterID) {
        self.move_fp_double(op1, dest);
        self.asm.subss_rr(op2, dest);
    }

    pub fn sub_float_rr(&mut self, src: XMMRegisterID, dest: XMMRegisterID) {
        self.sub_float(src, dest, dest);
    }

    pub fn mul_float(&mut self, op1: XMMRegisterID, op2: XMMRegisterID, dest: XMMRegisterID) {
        if op1 == dest {
            self.asm.mulss_rr(op2, dest);
        } else {
            self.move_fp_double(op2, dest);
            self.asm.mulss_rr(op1, dest);
        }
    }

    pub fn mul_float_rr(&mut self, src: XMMRegisterID, dest: XMMRegisterID) {
        self.mul_float(src, dest, dest);
    }
    pub fn mul_double(&mut self, op1: XMMRegisterID, op2: XMMRegisterID, dest: XMMRegisterID) {
        if op1 == dest {
            self.asm.mulss_rr(op2, dest);
        } else {
            self.move_fp_double(op2, dest);
            self.asm.mulsd_rr(op1, dest);
        }
    }

    pub fn mul_double_rr(&mut self, src: XMMRegisterID, dest: XMMRegisterID) {
        self.mul_double(src, dest, dest);
    }
    pub fn convert_int32_to_double(&mut self, src: RegisterID, dest: XMMRegisterID) {
        self.asm.cvtsi2sd_rr(src, dest);
    }
    pub fn convert_int32_to_float(&mut self, src: RegisterID, dest: XMMRegisterID) {
        self.asm.cvtsi2ss_rr(src, dest);
    }

    pub fn convert_int32_to_double_mem(&mut self, src: Mem, dest: XMMRegisterID) {
        match src {
            Mem::Base(base, offset) => {
                self.asm.cvtsi2sd_mr(offset, base, dest);
            }
            _ => unreachable!(),
        }
    }
    pub fn convert_int32_to_float_mem(&mut self, src: Mem, dest: XMMRegisterID) {
        match src {
            Mem::Base(base, offset) => {
                self.asm.cvtsi2ss_mr(offset, base, dest);
            }
            _ => unreachable!(),
        }
    }

    pub fn compare_double(
        &mut self,
        cond: FpCondition,
        left: XMMRegisterID,
        right: XMMRegisterID,
        dest: RegisterID,
    ) {
        self.fp_compare(cond, left, right, dest, |this, arg1, arg2| {
            this.asm.ucomisd_rr(arg1, arg2);
        });
    }
    pub fn compare_float(
        &mut self,
        cond: FpCondition,
        left: XMMRegisterID,
        right: XMMRegisterID,
        dest: RegisterID,
    ) {
        self.fp_compare(cond, left, right, dest, |this, arg1, arg2| {
            this.asm.ucomiss_rr(arg1, arg2);
        });
    }

    fn jump_after_fp_cmp(
        &mut self,
        cond: FpCondition,
        left: XMMRegisterID,
        right: XMMRegisterID,
    ) -> Jump {
        if cond == FpCondition::EqualAndOrdered {
            if left == right {
                return Jump::new(self.asm.jcc(Condition::NP));
            }
            let is_unordered = Jump::new(self.asm.jcc(Condition::P));
            let result = Jump::new(self.asm.jcc(Condition::E));
            is_unordered.link(self);
            return result;
        }
        if cond == FpCondition::NotEqualOrUnordered {
            if left == right {
                return Jump::new(self.asm.jcc(Condition::P));
            }
            let is_unordered = Jump::new(self.asm.jcc(Condition::P));
            let is_equal = Jump::new(self.asm.jcc(Condition::E));
            is_unordered.link(self);
            let result = self.jump();
            is_equal.link(self);
            return result;
        }
        return Jump::new(
            self.asm
                .jcc(unsafe { std::mem::transmute(cond as u8 & !DOUBLE_CONDITION_BITS) }),
        );
    }

    pub fn ret(&mut self) {
        self.asm.ret();
    }
    pub fn compare8(&mut self, cond: RelationalCondition, left: Mem, right: i32, dest: RegisterID) {
        match left {
            Mem::Base(base, off) => {
                self.asm.cmpb_im(right, off, base);
                self.set32(unsafe { std::mem::transmute(cond) }, dest);
            }
            _ => unreachable!(),
        }
    }

    pub fn compare32(
        &mut self,
        cond: RelationalCondition,
        left: RegisterID,
        right: RegisterID,
        dest: RegisterID,
    ) {
        self.asm.cmpl_rr(right, left);
        self.set32(unsafe { std::mem::transmute(cond) }, dest);
    }

    pub fn compare32_imm(
        &mut self,
        cond: RelationalCondition,
        left: RegisterID,
        right: i32,
        dest: RegisterID,
    ) {
        if right == 0 {}
        self.asm.cmpl_ir(right, left);
        self.set32(unsafe { std::mem::transmute(cond) }, dest);
    }
    pub fn compare64(
        &mut self,
        cond: RelationalCondition,
        left: RegisterID,
        right: RegisterID,
        dest: RegisterID,
    ) {
        self.asm.cmpq_rr(right, left);
        self.set32(unsafe { std::mem::transmute(cond) }, dest);
    }

    pub fn compare64_imm(
        &mut self,
        cond: RelationalCondition,
        left: RegisterID,
        right: i32,
        dest: RegisterID,
    ) {
        if right == 0 {}
        self.asm.cmpq_ir(right, left);
        self.set32(unsafe { std::mem::transmute(cond) }, dest);
    }
    pub fn test8(&mut self, cond: RelationalCondition, addr: Mem, mask: i32, dest: RegisterID) {
        let (base, off) = match addr {
            Mem::Base(base, off) => (base, off),
            _ => unreachable!(),
        };
        if mask as i8 == -1 {
            self.asm.cmpb_im(-1, off, base);
        } else {
            self.asm.testb_im(mask, off, base);
        }
        self.set32(unsafe { std::mem::transmute(cond) }, dest);
    }

    pub fn test32(
        &mut self,
        cond: RelationalCondition,
        reg: RegisterID,
        mask: RegisterID,
        dest: RegisterID,
    ) {
        self.asm.testl_rr(reg, mask);
        self.set32(unsafe { std::mem::transmute(cond) }, dest);
    }

    pub fn test64(
        &mut self,
        cond: RelationalCondition,
        reg: RegisterID,
        mask: RegisterID,
        dest: RegisterID,
    ) {
        self.asm.testq_rr(reg, mask);
        self.set32(unsafe { std::mem::transmute(cond) }, dest);
    }
    pub fn trap_safepoint(&mut self, page_addr: usize) {
        if self.x64 {
            self.move_i64(page_addr as _, SCRATCH_REG);
            self.asm.movl_i32m(0, 0, SCRATCH_REG);
        } else {
            self.push(RegisterID::EAX);
            self.move_i32(page_addr as _, RegisterID::EAX);
            self.asm.movl_i32m(0, 0, SCRATCH_REG);
            self.pop(RegisterID::EAX);
        }
    }
    pub fn near_tail_call(&mut self) -> Call {
        return Call::new(self.asm.jmp(), CallFlags::LinkableNearTail as _);
    }

    pub fn near_call(&mut self) -> Call {
        return Call::new(self.asm.call_rel(), CallFlags::LinkableNear as _);
    }

    pub fn call_mem(&mut self, mem: Mem) {
        match mem {
            Mem::Base(base, offset) => {
                self.asm.call_m(offset, base);
            }
            _ => {
                if self.x64 {
                    self.load64(mem, SCRATCH_REG);
                } else {
                    self.load32(mem, SCRATCH_REG);
                }
                self.asm.call_r(SCRATCH_REG);
            }
        }
    }
    pub fn branch(&mut self, cond: RelationalCondition) -> Jump {
        Jump::new(self.asm.jcc(unsafe { std::mem::transmute(cond) }))
    }
    pub fn branch32(
        &mut self,
        cond: RelationalCondition,
        left: RegisterID,
        right: RegisterID,
    ) -> Jump {
        self.asm.cmpl_rr(right, left);
        self.branch(cond)
    }
    pub fn branch32_imm(&mut self, cond: RelationalCondition, imm: i32, right: RegisterID) -> Jump {
        self.asm.cmpl_ir(imm, right);
        self.branch(cond)
    }

    pub fn branch_add32_rr(
        &mut self,
        cond: ResultCondition,
        src: RegisterID,
        dest: RegisterID,
    ) -> Jump {
        self.add32_rr(src, dest);
        Jump::new(self.asm.jcc(unsafe { std::mem::transmute(cond) }))
    }
    pub fn branch_add32(
        &mut self,
        cond: ResultCondition,
        src1: RegisterID,
        src2: RegisterID,
        dest: RegisterID,
    ) -> Jump {
        if src1 == dest {
            return self.branch_add32_rr(cond, src2, dest);
        } else {
            self.move32_if_needed(src2, dest);
            self.branch_add32_rr(cond, src1, dest)
        }
    }
    pub fn branch_add32_imm(&mut self, cond: ResultCondition, imm: i32, dest: RegisterID) -> Jump {
        self.add32_imm(imm, dest);
        Jump::new(self.asm.jcc(unsafe { std::mem::transmute(cond) }))
    }
    pub fn branch_sub32(
        &mut self,
        cond: ResultCondition,
        src: RegisterID,
        dest: RegisterID,
    ) -> Jump {
        self.sub32_rr(src, dest);
        Jump::new(self.asm.jcc(unsafe { std::mem::transmute(cond) }))
    }

    pub fn branch_sub32_imm(&mut self, cond: ResultCondition, imm: i32, dest: RegisterID) -> Jump {
        self.sub32_imm(imm, dest);
        Jump::new(self.asm.jcc(unsafe { std::mem::transmute(cond) }))
    }
    pub fn branch64_test(
        &mut self,
        cond: ResultCondition,
        reg: RegisterID,
        mask: RegisterID,
    ) -> Jump {
        self.asm.testq_rr(reg, mask);
        Jump::new(self.asm.jcc(unsafe { std::mem::transmute(cond) }))
    }

    pub fn branch64_test_imm64(
        &mut self,
        cond: ResultCondition,
        reg: RegisterID,
        mask: i64,
    ) -> Jump {
        self.move_i64(mask, SCRATCH_REG);
        self.branch64_test(cond, reg, SCRATCH_REG)
    }

    pub fn branch64_test_imm32(
        &mut self,
        cond: ResultCondition,
        reg: RegisterID,
        mask: i32,
    ) -> Jump {
        self.asm.testq_i32r(mask, reg);
        Jump::new(self.asm.jcc(unsafe { std::mem::transmute(cond) }))
    }
    pub fn branch64(
        &mut self,
        cond: RelationalCondition,
        left: RegisterID,
        right: RegisterID,
    ) -> Jump {
        self.asm.cmpq_rr(right, left);
        self.branch(cond)
    }
    pub fn branch64_imm32(
        &mut self,
        cond: RelationalCondition,
        imm: i32,
        right: RegisterID,
    ) -> Jump {
        self.asm.cmpq_ir(imm, right);
        self.branch(cond)
    }
    pub fn branch64_imm64(
        &mut self,
        cond: RelationalCondition,
        left: RegisterID,
        right: i64,
    ) -> Jump {
        if cond == RelationalCondition::Equal || cond == RelationalCondition::NotEqual && right == 0
        {
            self.asm.testq_rr(left, left);
            return Jump::new(self.asm.jcc(unsafe { std::mem::transmute(cond) }));
        }
        self.move_i64(right, SCRATCH_REG);
        self.branch64(cond, left, SCRATCH_REG)
    }
    pub fn jump(&mut self) -> Jump {
        Jump::new(self.asm.jmp())
    }
    pub fn label(&mut self) -> Label {
        Label {
            label: self.asm.label(),
        }
    }
    pub fn set32(&mut self, cond: Condition, dest: RegisterID) {
        if !self.x64 {
            self.asm.xchgl_rr(dest, RegisterID::EAX);
            self.asm.setcc_r(cond, RegisterID::EAX);
            self.asm.movzbl_rr(RegisterID::EAX, RegisterID::EAX);
            self.asm.xchgl_rr(dest, RegisterID::EAX);
            return;
        }
        self.asm.setcc_r(cond, dest);
        self.asm.movzbl_rr(dest, dest);
    }

    fn fp_compare(
        &mut self,
        cond: FpCondition,
        left: XMMRegisterID,
        right: XMMRegisterID,
        dest: RegisterID,
        mut compare: impl FnMut(&mut Self, XMMRegisterID, XMMRegisterID),
    ) {
        if (cond as u8 & DOUBLE_CONDITION_BIT_SPECIAL) != 0 {
            if cond == FpCondition::EqualAndOrdered {
                if left == right {
                    compare(self, right, left);
                    self.set32(Condition::NP, dest);
                    return;
                }
                self.move_i32(0, dest);
                compare(self, right, left);
                let is_unordered = self.asm.jcc(Condition::P);
                self.set32(Condition::E, dest);
                Jump {
                    label: is_unordered,
                }
                .link(self);
                return;
            }
            if cond == FpCondition::NotEqualOrUnordered {
                if left == right {
                    compare(self, right, left);
                    self.set32(Condition::P, dest);
                    return;
                }
                self.move_i32(1, dest);
                compare(self, right, left);
                let is_unordered = self.asm.jcc(Condition::NE);
                self.set32(Condition::E, dest);
                Jump {
                    label: is_unordered,
                }
                .link(self);
            }
        }
        if (cond as u8 & DOUBLE_CONDITION_BIT_INVERT) != 0 {
            compare(self, left, right);
        } else {
            compare(self, right, left);
        }
        self.set32(
            unsafe { std::mem::transmute(cond as u8 & !DOUBLE_CONDITION_BITS) },
            dest,
        );
    }
    pub fn swap_fp(&mut self, reg1: XMMRegisterID, reg2: XMMRegisterID) {
        todo!("{:?} swap {:?}", reg1, reg2);
    }

    pub fn move_fp_double(&mut self, src: XMMRegisterID, dst: XMMRegisterID) {
        if src != dst {
            self.asm.movaps_rr(src, dst);
        }
    }

    pub fn add32_to_addr(&mut self, imm: i32, addr: usize) {
        self.move_i64(addr as _, SCRATCH_REG);
        self.add32_im(imm, Mem::Base(SCRATCH_REG, 0));
    }

    pub fn load8_addr(&mut self, addr: usize, dest: RegisterID) {
        self.move_i64(addr as _, dest);
        self.load8(Mem::Base(dest, 0), dest);
    }

    pub fn load16_addr(&mut self, addr: usize, dest: RegisterID) {
        self.move_i64(addr as _, dest);
        self.load16(Mem::Base(dest, 0), dest);
    }

    pub fn load32_addr(&mut self, addr: usize, dest: RegisterID) {
        if dest == RegisterID::EAX {
            self.asm.movl_meax(addr);
        } else {
            self.move_i64(addr as _, dest);
            self.load32(Mem::Base(dest, 0), dest);
        }
    }

    pub fn convert_int32_to_double_imm(&mut self, imm: i32, dst: XMMRegisterID) {
        self.move_i32(imm, SCRATCH_REG);
        self.convert_int32_to_double(SCRATCH_REG, dst);
    }

    pub fn store32_addr(&mut self, source: RegisterID, addr: usize) {
        if source == RegisterID::EAX {
            self.asm.movl_eaxm(addr as _);
        } else {
            self.move_i64(addr as _, SCRATCH_REG);
            self.store32(source, Mem::Base(SCRATCH_REG, 0));
        }
    }

    pub fn store8_addr(&mut self, source: RegisterID, addr: usize) {
        self.move_i64(addr as _, SCRATCH_REG);
        self.store8(source, Mem::Base(SCRATCH_REG, 0));
    }

    pub fn load64(&mut self, mem: Mem, dest: RegisterID) {
        match mem {
            Mem::Base(base, offset) => {
                self.asm.movq_mr(offset, base, dest);
            }
            Mem::Local(ix) => {
                self.asm.movq_mr(ix, RegisterID::EBP, dest);
            }
            Mem::Index(base, index, scale, offset) => {
                self.asm.movq_mr_scaled(offset, base, index, scale, dest);
            }
            _ => unreachable!(),
        }
    }

    pub fn load64_addr(&mut self, addr: usize, dest: RegisterID) {
        if dest == RegisterID::EAX {
            self.asm.movq_meax(addr);
        } else {
            self.move_i64(addr as _, dest);
            self.load64(Mem::Base(dest, 0), dest);
        }
    }

    pub fn store64(&mut self, src: RegisterID, dest: Mem) {
        match dest {
            Mem::Base(base, offset) => self.asm.movq_rm(src, offset, base),
            Mem::Local(ix) => self.asm.movq_rm(src, ix, RegisterID::EBP),
            Mem::Index(base, index, scale, offset) => {
                self.asm.movq_rm_scaled(src, offset, base, index, scale)
            }
            _ => unreachable!(),
        }
    }

    pub fn store64_imm32(&mut self, imm: i32, dest: Mem) {
        match dest {
            Mem::Base(base, offset) => self.asm.movq_i32m(imm, offset, base),
            Mem::Local(ix) => self.asm.movq_i32m(imm, ix, RegisterID::EBP),
            Mem::Index(base, index, scale, offset) => {
                self.asm.movq_i32m_scaled(imm, offset, base, index, scale)
            }
            _ => unreachable!(),
        }
    }
    pub fn store64_imm64(&mut self, imm: i64, dest: Mem) {
        if imm as i32 as i64 == imm {
            self.store64_imm32(imm as _, dest);
            return;
        }
        self.move_i64(imm, SCRATCH_REG);
        self.store64(SCRATCH_REG, dest);
    }

    pub fn sub64(&mut self, src: RegisterID, dest: RegisterID) {
        self.asm.subq_rr(src, dest);
    }

    pub fn sub64_imm32(&mut self, imm: i32, dest: RegisterID) {
        self.asm.subq_ir(imm, dest);
    }

    pub fn sub64_imm64(&mut self, imm: i64, dest: RegisterID) {
        self.move_i64(imm, SCRATCH_REG);
        self.asm.subq_rr(SCRATCH_REG, dest);
    }
    pub fn call_6args(&mut self) -> Call {
        if self.x64 {
            #[cfg(target_family = "windows")]
            {
                // JIT relies on the CallerFrame (frame pointer) being put on the stack,
                // On Win64 we need to manually copy the frame pointer to the stack, since MSVC may not maintain a frame pointer on 64-bit.
                // See http://msdn.microsoft.com/en-us/library/9z1stfyw.aspx where it's stated that rbp MAY be used as a frame pointer.
                self.store64(RegisterID::EAX, Mem::Base(RegisterID::EBP, -16));
                // On Windows we need to copy the arguments that don't fit in registers to the stack location where the callee expects to find them.
                // We don't know the number of arguments at this point, so the arguments (5, 6, ...) should always be copied.

                // Copy argument 5
                self.load64(
                    Mem::Base(RegisterID::ESP, 4 * std::mem::size_of::<u64>() as i32),
                    SCRATCH_REG,
                );
                self.store64(
                    SCRATCH_REG,
                    Mem::Base(RegisterID::ESP, -4 * std::mem::size_of::<u64>() as i32),
                );
                // Copy argument 6
                self.load64(
                    Mem::Base(RegisterID::ESP, 5 * std::mem::size_of::<u64>() as i32),
                    SCRATCH_REG,
                );
                self.store64(
                    SCRATCH_REG,
                    Mem::Base(RegisterID::ESP, -3 * std::mem::size_of::<u64>() as i32),
                );
                // We also need to allocate the shadow space on the stack for the 4 parameter registers.
                // Also, we should allocate 16 bytes for the frame pointer, and return address (not populated).
                // In addition, we need to allocate 16 bytes for two more parameters, since the call can have up to 6 parameters.
                self.sub64_imm32(8 * std::mem::size_of::<i64>() as i32, RegisterID::ESP);
            }
            let _ = self.move_with_patch_ptr(0, SCRATCH_REG);
            let result = Call::new(self.asm.call_r(SCRATCH_REG), CallFlags::Linkable as _);
            #[cfg(target_family = "windows")]
            {
                self.add64_imm32(8 * 8, RegisterID::ESP, RegisterID::ESP);
            }
            return result;
        } else {
            return Call::new(self.asm.call_rel(), CallFlags::Linkable as _);
        }
    }
    pub fn far_jump_r(&mut self, target: RegisterID) {
        self.asm.jmp_r(target);
    }

    pub fn far_jump(&mut self, addr: usize) {
        if self.x64 {
            self.move_i64(addr as _, SCRATCH_REG);
            self.asm.jmp_m(0, SCRATCH_REG);
        } else {
            self.asm.jmp_maddr(addr);
        }
    }
    pub fn call_ptr(&mut self, ptr: *const u8) {
        if self.x64 {
            self.move_i64(ptr as _, SCRATCH_REG);
        } else {
            self.move_i32(ptr as _, SCRATCH_REG);
        }
        self.asm.call_r(SCRATCH_REG);
    }
    pub fn call_ptr_repatch(&mut self, initial: *const u8) -> Call {
        let _lbl = self.move_with_patch_ptr(initial as _, SCRATCH_REG);
        Call::new(self.asm.call_r(SCRATCH_REG), CallFlags::Linkable as _)
    }
    pub fn add64(&mut self, a: RegisterID, b: RegisterID, dest: RegisterID) {
        self.x86_lea64(Mem::Index(a, b, 0, 0), dest);
    }

    pub fn add64_rr(&mut self, src: RegisterID, dest: RegisterID) {
        self.asm.addq_rr(src, dest);
    }
    pub fn add64_imm64(&mut self, imm: i64, dest: RegisterID) {
        self.move_i64(imm, SCRATCH_REG);
        self.add64_rr(SCRATCH_REG, dest);
    }
    pub fn add64_imm32(&mut self, imm: i32, src: RegisterID, dest: RegisterID) {
        self.asm.leaq_mr(imm, src, dest);
    }

    pub fn x86_lea64(&mut self, index: Mem, dest: RegisterID) {
        match index {
            Mem::Index(base, index, scale, offset) => {
                if scale == 0 && offset == 0 {
                    if base == dest {
                        self.asm.addq_rr(index, dest);
                        return;
                    }
                    if index == dest {
                        self.asm.addq_rr(base, dest);
                        return;
                    }
                }
                self.asm.leaq_mr_scaled(offset, base, index, scale, dest);
            }
            _ => unreachable!(),
        }
    }

    pub fn tail_recursive_call64(&mut self) -> Call {
        let _ = self.move_with_patch_ptr(0, SCRATCH_REG);
        let new_jump = Jump::new(self.asm.jmp_r(SCRATCH_REG));
        return Call::from_tail_jump(new_jump.label);
    }

    pub fn make_tail_recurisive_call64(&mut self, old_jump: Jump) -> Call {
        old_jump.link(self);
        let _ = self.move_with_patch_ptr(0, SCRATCH_REG);
        let new_jump = Jump::new(self.asm.jmp_r(SCRATCH_REG));
        return Call::from_tail_jump(new_jump.label);
    }

    pub fn and64_rr(&mut self, src: RegisterID, dest: RegisterID) {
        self.asm.andq_rr(src, dest);
    }

    pub fn and64_ir(&mut self, imm: i32, dest: RegisterID) {
        self.asm.andq_ir(imm, dest);
    }
    pub fn and64(&mut self, op1: RegisterID, op2: RegisterID, dest: RegisterID) {
        if op1 == op2 && op1 != dest && op2 != dest {
            self.move_rr(op1, dest);
        } else if op1 == dest {
            self.and64_rr(op2, dest);
        } else {
            self.move_rr(op2, dest);
            self.and64_rr(op1, dest);
        }
    }

    pub fn lshift64_imm(&mut self, imm: i8, dest: RegisterID) {
        self.asm.shlq_i8r(imm, dest);
    }

    pub fn lshift64(&mut self, src: RegisterID, dest: RegisterID) {
        if src == RegisterID::ECX {
            self.asm.shlq_clr(dest);
        } else {
            assert_ne!(src, dest);
            self.swap_gp(src, RegisterID::ECX);
            self.asm
                .shlq_clr(if dest == RegisterID::ECX { src } else { dest });
            self.swap_gp(src, RegisterID::ECX);
        }
    }
    pub fn rshift64_imm(&mut self, imm: i8, dest: RegisterID) {
        self.asm.sarq_i8r(imm, dest);
    }

    pub fn rshift64(&mut self, src: RegisterID, dest: RegisterID) {
        if src == RegisterID::ECX {
            self.asm.sarq_clr(dest);
        } else {
            assert_ne!(src, dest);
            self.swap_gp(src, RegisterID::ECX);
            self.asm
                .sarq_clr(if dest == RegisterID::ECX { src } else { dest });
            self.swap_gp(src, RegisterID::ECX);
        }
    }
    pub fn urshift64_imm(&mut self, imm: i8, dest: RegisterID) {
        self.asm.shrq_i8r(imm, dest);
    }

    pub fn urshift64(&mut self, src: RegisterID, dest: RegisterID) {
        if src == RegisterID::ECX {
            self.asm.sarq_clr(dest);
        } else {
            assert_ne!(src, dest);
            self.swap_gp(src, RegisterID::ECX);
            self.asm
                .shrq_clr(if dest == RegisterID::ECX { src } else { dest });
            self.swap_gp(src, RegisterID::ECX);
        }
    }

    pub fn mul64_rr(&mut self, src: RegisterID, dest: RegisterID) {
        self.asm.imulq_rr(src, dest);
    }
    pub fn mul64(&mut self, src1: RegisterID, src2: RegisterID, dest: RegisterID) {
        if src2 == dest {
            self.mul64_rr(src1, dest);
            return;
        }
        self.move_rr(src1, dest);
        self.mul64_rr(src2, dest);
    }

    pub fn x86_cvt_to_quad_word64(&mut self) {
        self.asm.cqo();
    }

    pub fn x86div64(&mut self, dest: RegisterID) {
        self.asm.idivq_r(dest);
    }
    pub fn x86udiv64(&mut self, denominator: RegisterID) {
        self.asm.divq_r(denominator);
    }
    pub fn neg64(&mut self, src: RegisterID, dest: RegisterID) {
        self.move_rr(src, dest);
        self.asm.negq_r(dest);
    }

    pub fn or64_rr(&mut self, src: RegisterID, dest: RegisterID) {
        self.asm.orq_rr(src, dest);
    }

    pub fn or64_i32r(&mut self, imm: i32, dest: RegisterID) {
        self.asm.orq_ir(imm, dest);
    }

    pub fn or64_i64r(&mut self, imm: i64, dest: RegisterID) {
        if imm <= i32::MAX as i64 && imm >= i32::MIN as i64 {
            self.or64_i32r(imm as _, dest);
            return;
        }
        self.move_i64(imm, SCRATCH_REG);
        self.or64_rr(SCRATCH_REG, dest);
    }

    pub fn or64(&mut self, op1: RegisterID, op2: RegisterID, dest: RegisterID) {
        if op1 == op2 {
            self.move_rr(op1, dest);
        } else if op1 == dest {
            self.or64_rr(op2, dest);
        } else {
            self.move_rr(op2, dest);
            self.or64_rr(op1, dest);
        }
    }

    pub fn or64_imm32(&mut self, imm: i32, src: RegisterID, dest: RegisterID) {
        self.move_rr(src, dest);
        self.or64_i32r(imm, dest);
    }
    pub fn or64_imm64(&mut self, imm: i64, src: RegisterID, dest: RegisterID) {
        self.move_rr(src, dest);
        self.or64_i64r(imm, dest);
    }

    pub fn xor64_rr(&mut self, src: RegisterID, dest: RegisterID) {
        self.asm.xorq_rr(src, dest);
    }

    pub fn xor64(&mut self, op1: RegisterID, op2: RegisterID, dest: RegisterID) {
        if op1 == op2 {
            self.move_i32(0, dest);
        } else if op1 == dest {
            self.xor64_rr(op2, dest);
        } else {
            self.move_rr(op2, dest);
            self.xor64_rr(op1, dest);
        }
    }

    pub fn xor64_imm32(&mut self, imm: i32, src_dest: RegisterID) {
        self.asm.xorq_ir(imm, src_dest);
    }

    pub fn xor64_imm64(&mut self, imm: i64, src_dest: RegisterID) {
        self.move_i64(imm, SCRATCH_REG);
        self.asm.xorq_rr(SCRATCH_REG, src_dest);
    }

    pub fn not64(&mut self, src_dest: RegisterID) {
        self.asm.notq_r(src_dest);
    }

    pub fn not64_mem(&mut self, src_dest: Mem) {
        match src_dest {
            Mem::Base(base, off) => {
                self.asm.notq_m(off, base);
            }
            _ => unreachable!(),
        }
    }
    pub fn cmov(&mut self, cond: Condition, src: RegisterID, dest: RegisterID) {
        if self.x64 {
            self.asm.cmovq_rr(cond, src, dest);
        } else {
            self.asm.cmovl_rr(cond, src, dest);
        }
    }

    pub fn cmov32(&mut self, cond: Condition, src: RegisterID, dest: RegisterID) {
        self.asm.cmovl_rr(cond, src, dest);
    }

    pub fn push(&mut self, src: RegisterID) {
        self.asm.push_r(src);
    }

    pub fn pop(&mut self, dest: RegisterID) {
        self.asm.pop_r(dest);
    }

    pub fn function_prologue(&mut self, size: i32) {
        //self.move_rr(RegisterID::EBP, RegisterID::EAX);
        self.asm.push_r(RegisterID::EBP);
        self.move_rr(RegisterID::ESP, RegisterID::EBP);
        if size != 0 {
            if self.x64 {
                self.sub64_imm32(size, RegisterID::ESP);
            } else {
                self.sub32_imm(size, RegisterID::ESP);
            }
        }
    }

    pub fn function_epilogue(&mut self) {
        self.move_rr(RegisterID::EBP, RegisterID::ESP);
        self.asm.pop_r(RegisterID::EBP);
    }
    pub fn register_for_arg(&mut self, arg: usize) -> RegisterID {
        #[cfg(windows)]
        {
            match arg {
                0 => RegisterID::ECX,
                1 => RegisterID::EDX,
                2 => RegisterID::R8,
                3 => RegisterID::R9,
                _ => unreachable!(),
            }
        }
        #[cfg(unix)]
        {
            match arg {
                0 => RegisterID::EDI,
                1 => RegisterID::ESI,
                2 => RegisterID::EDX,
                3 => RegisterID::ECX,
                4 => RegisterID::R8,
                5 => RegisterID::R9,
                _ => unreachable!(),
            }
        }
    }

    pub fn arg_stack_addr(&self, arg: usize) -> Mem {
        let offset = arg as i32 - ARG_IN_REG_COUNT as i32;
        return Mem::Base(RegisterID::ESP, offset * (if self.x64 { 8 } else { 4 }));
    }
    pub fn prepare_call_with_arg_count(&mut self, argc: usize) {
        //self.push(RegisterID::EAX);
        #[cfg(windows)]
        {
            self.push(RegisterID::ECX);
            //self.push(RegisterID::EDX);
            self.push(RegisterID::R8);
            self.push(RegisterID::R9);
            self.push(RegisterID::R10);
            self.push(RegisterID::R11);
        }
        let mut argc_on_stack = 0;
        if argc > ARG_IN_REG_COUNT {
            argc_on_stack = argc - ARG_IN_REG_COUNT * (if self.x64 { 8 } else { 4 });
            argc_on_stack = argc_on_stack + 4 * (if self.x64 { 8 } else { 4 });
            //return address
        }
        #[cfg(windows)]
        {
            argc_on_stack += 40;
        }
        if argc_on_stack != 0 {
            if !self.x64 {
                self.sub32_imm(argc_on_stack as _, RegisterID::ESP);
            } else {
                self.sub64_imm32(argc_on_stack as _, RegisterID::ESP);
            }
        }
    }

    pub fn call_r(&mut self, r: RegisterID) {
        self.asm.call_r(r);
    }
    pub fn call_ptr_argc(&mut self, ptr: *const u8, argc: usize) {
        if self.x64 {
            self.move_i64(ptr as _, SCRATCH_REG);
        } else {
            self.move_i32(ptr as _, SCRATCH_REG);
        }
        self.asm.call_r(SCRATCH_REG);
        if argc > ARG_IN_REG_COUNT {
            let argc_on_stack = argc - ARG_IN_REG_COUNT * (if self.x64 { 8 } else { 4 });
            let argc_on_stack = argc_on_stack + 4 * (if self.x64 { 8 } else { 4 }); //return address
            if !self.x64 {
                self.add32_imm(argc_on_stack as _, RegisterID::ESP);
            } else {
                self.add64_imm32(argc_on_stack as _, RegisterID::ESP, RegisterID::ESP);
            }
        }
        #[cfg(windows)]
        {
            self.add64_imm32(40, RegisterID::ESP, RegisterID::ESP);
        }
    }
    pub fn call_ptr_repatch_argc(&mut self, initial: *const u8, argc: usize) -> Call {
        let _lbl = self.move_with_patch_ptr(initial as _, SCRATCH_REG);
        let c = Call::new(self.asm.call_r(SCRATCH_REG), CallFlags::Linkable as _);
        if argc > ARG_IN_REG_COUNT {
            let argc_on_stack = argc - ARG_IN_REG_COUNT * (if self.x64 { 8 } else { 4 });
            let argc_on_stack = argc_on_stack + 4 * (if self.x64 { 8 } else { 4 }); //return address
            if !self.x64 {
                self.add32_imm(argc_on_stack as _, RegisterID::ESP);
            } else {
                self.add64_imm32(argc_on_stack as _, RegisterID::ESP, RegisterID::ESP);
            }
        }
        #[cfg(windows)]
        {
            self.add64_imm32(40, RegisterID::ESP, RegisterID::ESP);
        }
        c
    }
    pub fn call_reg(&mut self, r: RegisterID, argc: usize) {
        let _ = if self.x64 {
            self.asm.call_r(r);
            if argc > ARG_IN_REG_COUNT {
                let argc_on_stack = argc - ARG_IN_REG_COUNT * (if self.x64 { 8 } else { 4 });
                let argc_on_stack = argc_on_stack + 4 * (if self.x64 { 8 } else { 4 }); //return address
                if !self.x64 {
                    self.add32_imm(argc_on_stack as _, RegisterID::ESP);
                } else {
                    self.add64_imm32(argc_on_stack as _, RegisterID::ESP, RegisterID::ESP);
                }
            }
            #[cfg(windows)]
            {
                self.add64_imm32(40, RegisterID::ESP, RegisterID::ESP);
            }
        } else {
            let argc_on_stack = argc - ARG_IN_REG_COUNT * (if self.x64 { 8 } else { 4 });
            let argc_on_stack = argc_on_stack + 4 * (if self.x64 { 8 } else { 4 }); //return address
            self.asm.call_r(r);
            self.add32_imm(argc_on_stack as i32 + 40, RegisterID::ESP);
        };
        #[cfg(windows)]
        {
            self.pop(RegisterID::R11);
            self.pop(RegisterID::R10);
            self.pop(RegisterID::R9);
            self.pop(RegisterID::R8);
            //self.pop(RegisterID::EDX);
            self.pop(RegisterID::ECX);
        }
        //self.pop(RegisterID::EAX);
    }
    pub fn call(&mut self, argc: usize) -> Call {
        let call = if self.x64 {
            let _ = self.move_with_patch_ptr(0, SCRATCH_REG);
            let result = Call::new(self.asm.call_r(SCRATCH_REG), CallFlags::Linkable as _);
            if argc > ARG_IN_REG_COUNT {
                let argc_on_stack = argc - ARG_IN_REG_COUNT * (if self.x64 { 8 } else { 4 });
                let argc_on_stack = argc_on_stack + 4 * (if self.x64 { 8 } else { 4 }); //return address
                if !self.x64 {
                    self.add32_imm(argc_on_stack as _, RegisterID::ESP);
                } else {
                    self.add64_imm32(argc_on_stack as _, RegisterID::ESP, RegisterID::ESP);
                }
            }
            #[cfg(windows)]
            {
                self.add64_imm32(40, RegisterID::ESP, RegisterID::ESP);
            }
            result
        } else {
            let argc_on_stack = argc - ARG_IN_REG_COUNT * (if self.x64 { 8 } else { 4 });
            let argc_on_stack = argc_on_stack + 4 * (if self.x64 { 8 } else { 4 }); //return address
            let c = Call::new(self.asm.call_rel(), CallFlags::Linkable as _);
            self.add32_imm(argc_on_stack as i32 + 40, RegisterID::ESP);
            c
        };
        #[cfg(windows)]
        {
            self.pop(RegisterID::R11);
            self.pop(RegisterID::R10);
            self.pop(RegisterID::R9);
            self.pop(RegisterID::R8);
            //self.pop(RegisterID::EDX);
            self.pop(RegisterID::ECX);
        }
        //self.pop(RegisterID::EAX);
        call
    }
    pub fn pass_int32_as_arg(&mut self, value: i32, arg: usize) {
        if arg < ARG_IN_REG_COUNT {
            let r = self.register_for_arg(arg);
            self.move_i32(value, r);
        } else {
            self.store32_imm(value, self.arg_stack_addr(arg));
        }
    }
    pub fn pass_ptr_as_arg(&mut self, value: usize, arg: usize) {
        if arg < ARG_IN_REG_COUNT {
            let r = self.register_for_arg(arg);
            if !self.x64 {
                self.move_i32(value as _, r);
            } else {
                self.move_i64(value as _, r);
            }
        } else {
            if !self.x64 {
                self.store32_imm(value as _, self.arg_stack_addr(arg));
            } else {
                self.store64_imm64(value as _, self.arg_stack_addr(arg));
            }
        }
    }

    pub fn pass_reg_as_arg(&mut self, src: RegisterID, arg: usize) {
        if arg < ARG_IN_REG_COUNT {
            let r = self.register_for_arg(arg);
            self.move_rr(src, r);
        } else {
            if self.x64 {
                self.store64(src, self.arg_stack_addr(arg));
            } else {
                self.store32(src, self.arg_stack_addr(arg));
            }
            //self.store32_imm(value, self.arg_stack_addr(arg));
        }
    }
}
impl super::MacroAssemblerBase for MacroAssemblerX86 {
    fn link_call(code: *mut u8, call: Call, func: *const u8, flags: u8) {
        if (flags & CallFlags::Near as u8) == 0 {
            X86Asm::link_pointer_or_call(
                code,
                call.label
                    .label_at_offset(-(REPATCH_OFFSET_CALL_R11 as i32)),
                func as *mut u8,
            );
            return;
        } else if (flags & CallFlags::Tail as u8) != 0 {
            X86Asm::slink_jump(code, call.label, func as *mut u8);
        } else {
            X86Asm::slink_jump(code, call.label, func as *mut u8);
        }
    }
    fn link_jump(code: *mut u8, jump: AsmLabel, label: AsmLabel) {
        X86Asm::slink_jump2(code, jump, label)
    }
    fn link_jump_ptr(code: *mut u8, from: AsmLabel, to: *const u8) {
        unsafe { X86Asm::set_rel32(code.offset(from.0 as _), to as *mut u8) }
    }
    fn link_pointer(code: *mut u8, label: assembler_buffer::AsmLabel, value: *mut u8) {
        X86Asm::link_pointer_or_call(code, label, value);
    }
    fn get_linker_addr(code: *mut u8, label: AsmLabel) -> *mut u8 {
        X86Asm::get_reloc_addr(code, label)
    }
    fn finalize(self) -> Vec<u8> {
        self.asm.formatter.buffer.storage
    }
}

pub enum CallConv {
    CDecl,
    SystemV,
    Windows64,
}

#[cfg(windows)]
pub const ARG_IN_REG_COUNT: usize = 4;
#[cfg(unix)]
pub const ARG_IN_REG_COUNT: usize = 6;

use linkbuffer::*;
impl LinkBuffer<MacroAssemblerX86> {
    pub fn allocate(&mut self, masm: &mut MacroAssemblerX86) {
        if self.code.is_null() == false {
            if masm.asm.data().len() > self.size {
                return;
            }
            let nops_to_fill = self.size - masm.asm.data().len();
            for _ in 0..nops_to_fill {
                masm.asm.nop();
            }
            self.did_allocate = true;
            return;
        }
        let mut init_size = masm.asm.data().len();

        while init_size % 8 != 0 {
            masm.asm.nop();
            init_size = masm.asm.data().len();
        }

        self.did_allocate = true;
        self.exec_mem = crate::MEM_ALLOC
            .lock()
            .unwrap()
            .allocate(init_size, 8)
            .unwrap();
        self.code = self.exec_mem;
        self.size = init_size;
        self.did_allocate = true;
    }
    pub fn from_masm(masm: &mut MacroAssemblerX86) -> Self {
        let mut l = Self::new(std::ptr::null_mut());
        l.link_code(masm);

        l
    }

    pub fn link_code(&mut self, masm: &mut MacroAssemblerX86) {
        masm.asm.label();
        self.allocate(masm);
        if !self.did_allocate {
            return;
        }
        let buffer = masm.asm.data().as_ptr();
        unsafe {
            std::ptr::copy_nonoverlapping(buffer, self.code, masm.asm.data().len());
        }
        std::mem::swap(&mut self.link_tasks, &mut masm.link_tasks);
        Memory::set_rwx_mem(self.code, self.size);
    }
}
