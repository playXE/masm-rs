use super::{
    abstract_macro_assembler::{
        Address, BaseIndex, ConvertibleLoadLabel, DataLabel32, DataLabelCompact, DataLabelPtr,
        Extend, Jump, Operand, PatchableJump, Scale, Call,
    },
    macro_assembler_x86_common::*,
    x86assembler::*,
};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum ClearBitsAttribute {
    OkToClobberMask,
    MustPreserveMask,
}

pub const fn can_sign_extend_32_64(x: i64) -> bool {
    x == (x as i32) as i64
}

impl MacroAssemblerX86Common {

    pub fn call(&mut self) -> Call {
        let label = self.move_with_patch(0i64, Self::SCRATCH_REGISTER);
        let result = Call::new(self.assembler.call_r(Self::SCRATCH_REGISTER), Call::LINKABLE);
        assert!(result.label.offset - label.label.offset == 3);
        result
    }

    pub fn add64(&mut self, src: impl Into<Operand>, dst: impl Into<Operand>) {
        match (src.into(), dst.into()) {
            (Operand::Register(src), Operand::Register(dest)) => {
                self.assembler.addq_rr(src, dest);
            }

            (Operand::Address(src), Operand::Register(dest)) => {
                self.assembler.addq_mr(src.offset, src.base, dest);
            }

            (Operand::BaseIndex(src), Operand::Register(dest)) => {
                self.assembler.addq_mr_scaled(
                    src.offset,
                    src.base,
                    src.index,
                    src.scale as _,
                    dest,
                );
            }

            (Operand::Register(src), Operand::Address(dest)) => {
                self.assembler.addq_rm(src, dest.offset, dest.base);
            }

            (Operand::Register(src), Operand::BaseIndex(dest)) => {
                self.assembler.addq_rm_scaled(
                    src,
                    dest.offset,
                    dest.base,
                    dest.index,
                    dest.scale as _,
                );
            }

            (Operand::AbsoluteAddress(src), Operand::Register(dest)) => {
                self.mov(src, Self::SCRATCH_REGISTER);
                self.add64(Address::new(Self::SCRATCH_REGISTER, 0), dest);
            }

            (Operand::Imm32(imm), Operand::Register(src_dest)) => {
                if imm == 1 {
                    self.assembler.incq_r(src_dest);
                } else {
                    self.assembler.addq_ir(imm, src_dest);
                }
            }

            (Operand::Imm64(imm), Operand::Register(src_dest)) => {
                if imm == 1 {
                    self.assembler.incq_r(src_dest);
                } else {
                    self.mov(imm, Self::SCRATCH_REGISTER);
                    self.add64(Self::SCRATCH_REGISTER, src_dest);
                }
            }

            (Operand::Imm32(imm), Operand::Address(addr)) => {
                if imm == 1 {
                    self.assembler.incq_m(addr.offset, addr.base);
                } else {
                    self.assembler.addq_im(imm, addr.offset, addr.base);
                }
            }

            (Operand::Imm64(imm), Operand::Address(addr)) => {
                if imm == 1 {
                    self.assembler.incq_m(addr.offset, addr.base);
                } else {
                    self.mov(imm, Self::SCRATCH_REGISTER);
                    self.add64(Self::SCRATCH_REGISTER, addr);
                }
            }

            (Operand::Imm32(imm), Operand::BaseIndex(addr)) => {
                if imm == 1 {
                    self.assembler.incq_m_scaled(
                        addr.offset,
                        addr.base,
                        addr.index,
                        addr.scale as _,
                    );
                } else {
                    self.assembler.addq_im_scaled(
                        imm,
                        addr.offset,
                        addr.base,
                        addr.index,
                        addr.scale as _,
                    );
                }
            }

            (op1, op2) => unreachable!("add64: {:?}, {:?}", op1, op2),
        }
    }

    pub fn and64(&mut self, src: impl Into<Operand>, dst: impl Into<Operand>) {
        match (src.into(), dst.into()) {
            (Operand::Register(src), Operand::Register(dest)) => {
                self.assembler.andq_rr(src, dest);
            }

            (Operand::Register(src), Operand::Address(dest)) => {
                self.assembler.andq_rm(src, dest.offset, dest.base);
            }

            (Operand::Register(src), Operand::BaseIndex(dest)) => {
                self.assembler.andq_rm(src, dest.offset, dest.base);
            }

            (Operand::Imm32(imm), Operand::Register(src_dest)) => {
                self.assembler.andq_ir(imm, src_dest);
            }

            (Operand::Imm64(imm), Operand::Register(src_dest)) => {
                self.mov(imm, Self::SCRATCH_REGISTER);
                self.and64(Self::SCRATCH_REGISTER, src_dest);
            }

            (Operand::Imm32(imm), Operand::Address(addr)) => {
                self.assembler.andq_im(imm, addr.offset, addr.base);
            }

            (Operand::Imm64(imm), Operand::Address(addr)) => {
                self.mov(imm, Self::SCRATCH_REGISTER);
                self.and64(Self::SCRATCH_REGISTER, addr);
            }

            (Operand::Imm32(imm), Operand::BaseIndex(addr)) => {
                self.assembler.andq_im_scaled(
                    imm,
                    addr.offset,
                    addr.base,
                    addr.index,
                    addr.scale as _,
                );
            }

            (Operand::Address(src), Operand::Register(dest)) => {
                self.assembler.andq_mr(src.offset, src.base, dest);
            }

            (Operand::BaseIndex(src), Operand::Register(dest)) => {
                self.assembler.andq_mr_scaled(
                    src.offset,
                    src.base,
                    src.index,
                    src.scale as _,
                    dest,
                );
            }

            (op1, op2) => unreachable!("and64: {:?}, {:?}", op1, op2),
        }
    }

    pub fn and64_rrr(&mut self, op1: u8, op2: u8, dest: u8) {
        if op1 == op2 && op1 != dest && op2 != dest {
            self.mov(op1, dest);
        } else if op1 == dest {
            self.and64(op2, dest);
        } else {
            self.mov(op2, dest);
            self.and64(op1, dest);
        }
    }

    pub fn x86_lea64(&mut self, index: BaseIndex, dest: u8) {
        if index.scale as u8 == 0 && index.offset == 0 {
            if index.base == dest {
                self.add64(index.index, dest);
                return;
            }

            if index.index == dest {
                self.add64(index.base, dest);
                return;
            }
        }

        self.assembler.leaq_mr_scaled(
            index.offset,
            index.base,
            index.index,
            index.scale as _,
            dest,
        );
    }

    pub fn get_effective_address(&mut self, addr: BaseIndex, dest: u8) {
        self.x86_lea64(addr, dest)
    }

    pub fn add64_rrr(&mut self, a: impl Into<Operand>, b: impl Into<Operand>, dest: u8) {
        match (a.into(), b.into()) {
            (Operand::Register(a), Operand::Register(b)) => {
                self.x86_lea64(BaseIndex::new(a, b, Scale::TimesOne, 0, Extend::None), dest)
            }

            (Operand::Imm32(imm), Operand::Register(b)) => self.assembler.leaq_mr(imm, b, dest),

            (a, b) => unreachable!("add64_rrr: {:?}, {:?}", a, b),
        }
    }

    pub fn add_ptr_no_flags(&mut self, imm: i32, src_dest: u8) {
        self.assembler.leal_mr(imm, src_dest, src_dest);
    }

    pub fn count_leading_zeros64(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(src) => {
                self.assembler.lzcntq_rr(src, dest);
            }

            Operand::Address(addr) => {
                self.assembler.lzcntq_mr(addr.offset, addr.base, dest);
            }

            src => unreachable!("count_leading_zeros64: {:?}", src),
        }
    }

    pub fn count_trailing_zeros64(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(src) => {
                self.assembler.tzcntq_rr(src, dest);
            }

            src => unreachable!("count_trailing_zeros64: {:?}", src),
        }
    }

    pub fn clear_bit64(&mut self, bit_to_clear: u8, dst: u8) {
        self.assembler.btrq_rr(bit_to_clear, dst);
    }

    pub fn clear_bits64_with_mask(
        &mut self,
        mask: u8,
        dest: u8,
        mask_preservation: ClearBitsAttribute,
    ) {
        self.not64(mask);
        self.assembler.andq_rr(mask, dest);
        if mask_preservation == ClearBitsAttribute::MustPreserveMask {
            self.not64(mask);
        }
    }

    pub fn clear_bits64_with_mask_rr(
        &mut self,
        src: u8,
        mask: u8,
        dest: u8,
        mask_preservation: ClearBitsAttribute,
    ) {
        self.mov(src, dest);
        self.clear_bits64_with_mask(mask, dest, mask_preservation);
    }

    pub fn count_population64(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(src) => {
                self.assembler.popcntq_rr(src, dest);
            }

            Operand::Address(addr) => {
                self.assembler.popcntq_mr(addr.offset, addr.base, dest);
            }

            src => unreachable!("count_population64: {:?}", src),
        }
    }

    pub fn lshift64(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(src) => {
                if src == ecx {
                    self.assembler.shlq_clr(dest);
                } else {
                    assert!(src != dest);

                    self.swap(src, ecx);
                    self.assembler
                        .shlq_clr(if dest == ecx { src } else { dest });
                    self.swap(src, ecx);
                }
            }

            Operand::Imm32(imm) => {
                self.assembler.shlq_i8r(imm, dest);
            }

            src => unreachable!("lshift64: {:?}", src),
        }
    }

    pub fn lshift64_rrr(
        &mut self,
        src: impl Into<Operand>,
        shift_amount: impl Into<Operand>,
        dest: u8,
    ) {
        match (src.into(), shift_amount.into()) {
            (Operand::Register(src), Operand::Imm32(imm)) => {
                if src == dest {
                    self.lshift32(imm, dest);
                } else {
                    self.mov(src, dest);
                    self.lshift32(imm, dest);
                }
            }

            (Operand::Address(src), Operand::Register(shift_amount)) => {
                if shift_amount == dest {
                    self.mov(shift_amount, Self::SCRATCH_REGISTER);
                    self.load64(src, dest);
                    self.lshift64(Self::SCRATCH_REGISTER, dest);
                } else {
                    self.load64(src, dest);
                    self.lshift64(shift_amount, dest);
                }
            }

            (Operand::Register(src), Operand::Register(shift_amount)) => {
                if shift_amount == dest {
                    self.mov(shift_amount, Self::SCRATCH_REGISTER);
                    self.mov(src, dest);
                    self.lshift64(Self::SCRATCH_REGISTER, dest);
                } else {
                    self.mov(src, dest);
                    self.lshift64(shift_amount, dest);
                }
            }

            (src, shift_amount) => unreachable!("lshift64_rrr: {:?}, {:?}", src, shift_amount),
        }
    }

    pub fn rshift64(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(src) => {
                if src == ecx {
                    self.assembler.sarq_clr(dest);
                } else {
                    assert!(src != dest);

                    self.swap(src, ecx);
                    self.assembler
                        .sarq_clr(if dest == ecx { src } else { dest });
                    self.swap(src, ecx);
                }
            }

            Operand::Imm32(imm) => {
                self.assembler.sarq_i8r(imm, dest);
            }

            src => unreachable!("rshift64: {:?}", src),
        }
    }

    pub fn rshift64_rrr(
        &mut self,
        src: impl Into<Operand>,
        shift_amount: impl Into<Operand>,
        dest: u8,
    ) {
        match (src.into(), shift_amount.into()) {
            (Operand::Register(src), Operand::Imm32(imm)) => {
                if src == dest {
                    self.rshift32(imm, dest);
                } else {
                    self.mov(src, dest);
                    self.rshift32(imm, dest);
                }
            }

            (Operand::Address(src), Operand::Register(shift_amount)) => {
                if shift_amount == dest {
                    self.mov(shift_amount, Self::SCRATCH_REGISTER);
                    self.load64(src, dest);
                    self.rshift64(Self::SCRATCH_REGISTER, dest);
                } else {
                    self.load64(src, dest);
                    self.rshift64(shift_amount, dest);
                }
            }

            (Operand::Register(src), Operand::Register(shift_amount)) => {
                if shift_amount == dest {
                    self.mov(shift_amount, Self::SCRATCH_REGISTER);
                    self.mov(src, dest);
                    self.rshift64(Self::SCRATCH_REGISTER, dest);
                } else {
                    self.mov(src, dest);
                    self.rshift64(shift_amount, dest);
                }
            }

            (src, shift_amount) => unreachable!("rshift64_rrr: {:?}, {:?}", src, shift_amount),
        }
    }

    pub fn urshift64(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(src) => {
                if src == ecx {
                    self.assembler.shrq_clr(dest);
                } else {
                    assert!(src != dest);

                    self.swap(src, ecx);
                    self.assembler
                        .shrq_clr(if dest == ecx { src } else { dest });
                    self.swap(src, ecx);
                }
            }

            Operand::Imm32(imm) => {
                self.assembler.shrq_i8r(imm, dest);
            }

            src => unreachable!("urshift64: {:?}", src),
        }
    }

    pub fn urshift64_rrr(
        &mut self,
        src: impl Into<Operand>,
        shift_amount: impl Into<Operand>,
        dest: u8,
    ) {
        match (src.into(), shift_amount.into()) {
            (Operand::Register(src), Operand::Imm32(imm)) => {
                if src == dest {
                    self.urshift32(imm, dest);
                } else {
                    self.mov(src, dest);
                    self.urshift32(imm, dest);
                }
            }

            (Operand::Address(src), Operand::Register(shift_amount)) => {
                if shift_amount == dest {
                    self.mov(shift_amount, Self::SCRATCH_REGISTER);
                    self.load64(src, dest);
                    self.urshift64(Self::SCRATCH_REGISTER, dest);
                } else {
                    self.load64(src, dest);
                    self.urshift64(shift_amount, dest);
                }
            }

            (Operand::Register(src), Operand::Register(shift_amount)) => {
                if shift_amount == dest {
                    self.mov(shift_amount, Self::SCRATCH_REGISTER);
                    self.mov(src, dest);
                    self.urshift64(Self::SCRATCH_REGISTER, dest);
                } else {
                    self.mov(src, dest);
                    self.urshift64(shift_amount, dest);
                }
            }

            (src, shift_amount) => unreachable!("urshift64_rrr: {:?}, {:?}", src, shift_amount),
        }
    }

    pub fn rotate_right64(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(src) => {
                if src == ecx {
                    self.assembler.rorq_clr(dest);
                } else {
                    assert!(src != dest);

                    self.swap(src, ecx);
                    self.assembler
                        .rorq_clr(if dest == ecx { src } else { dest });
                    self.swap(src, ecx);
                }
            }

            Operand::Imm32(imm) => {
                self.assembler.rorq_i8r(imm, dest);
            }

            src => unreachable!("rotate_right64: {:?}", src),
        }
    }

    pub fn rotate_right64_rrr(
        &mut self,
        src: impl Into<Operand>,
        shift_amount: impl Into<Operand>,
        dest: u8,
    ) {
        match (src.into(), shift_amount.into()) {
            (Operand::Register(src), Operand::Imm32(imm)) => {
                if src == dest {
                    self.rotate_right32(imm, dest);
                } else {
                    self.mov(src, dest);
                    self.rotate_right32(imm, dest);
                }
            }

            (Operand::Address(src), Operand::Register(shift_amount)) => {
                if shift_amount == dest {
                    self.mov(shift_amount, Self::SCRATCH_REGISTER);
                    self.load64(src, dest);
                    self.rotate_right64(Self::SCRATCH_REGISTER, dest);
                } else {
                    self.load64(src, dest);
                    self.rotate_right64(shift_amount, dest);
                }
            }

            (Operand::Register(src), Operand::Register(shift_amount)) => {
                if shift_amount == dest {
                    self.mov(shift_amount, Self::SCRATCH_REGISTER);
                    self.mov(src, dest);
                    self.rotate_right64(Self::SCRATCH_REGISTER, dest);
                } else {
                    self.mov(src, dest);
                    self.rotate_right64(shift_amount, dest);
                }
            }

            (src, shift_amount) => {
                unreachable!("rotate_right64_rrr: {:?}, {:?}", src, shift_amount)
            }
        }
    }

    pub fn rotate_left64(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(src) => {
                if src == ecx {
                    self.assembler.rolq_clr(dest);
                } else {
                    assert!(src != dest);

                    self.swap(src, ecx);
                    self.assembler
                        .rolq_clr(if dest == ecx { src } else { dest });
                    self.swap(src, ecx);
                }
            }

            Operand::Imm32(imm) => {
                self.assembler.rolq_i8r(imm, dest);
            }

            src => unreachable!("rotate_left64: {:?}", src),
        }
    }

    pub fn rotate_left64_rrr(
        &mut self,
        src: impl Into<Operand>,
        shift_amount: impl Into<Operand>,
        dest: u8,
    ) {
        match (src.into(), shift_amount.into()) {
            (Operand::Register(src), Operand::Imm32(imm)) => {
                if src == dest {
                    self.rotate_left32(imm, dest);
                } else {
                    self.mov(src, dest);
                    self.rotate_left32(imm, dest);
                }
            }

            (Operand::Address(src), Operand::Register(shift_amount)) => {
                if shift_amount == dest {
                    self.mov(shift_amount, Self::SCRATCH_REGISTER);
                    self.load64(src, dest);
                    self.rotate_left64(Self::SCRATCH_REGISTER, dest);
                } else {
                    self.load64(src, dest);
                    self.rotate_left64(shift_amount, dest);
                }
            }

            (Operand::Register(src), Operand::Register(shift_amount)) => {
                if shift_amount == dest {
                    self.mov(shift_amount, Self::SCRATCH_REGISTER);
                    self.mov(src, dest);
                    self.rotate_left64(Self::SCRATCH_REGISTER, dest);
                } else {
                    self.mov(src, dest);
                    self.rotate_left64(shift_amount, dest);
                }
            }

            (src, shift_amount) => unreachable!("rotate_left64_rrr: {:?}, {:?}", src, shift_amount),
        }
    }

    pub fn mul64(&mut self, src: u8, dest: u8) {
        self.assembler.imulq_rr(src, dest);
    }

    pub fn mul64_rrr(&mut self, src1: u8, src2: u8, dest: u8) {
        if src2 == dest {
            self.assembler.imulq_rr(src1, dest);
            return;
        }

        self.mov(src1, dest);
        self.assembler.imulq_rr(src2, dest);
    }

    pub fn x86_convert_to_quad_word64(&mut self) {
        self.assembler.cqo();
    }

    pub fn x86div64(&mut self, denominator: u8) {
        self.assembler.idivq_r(denominator);
    }

    pub fn x86udiv64(&mut self, denominator: u8) {
        self.assembler.divq_r(denominator);
    }

    pub fn neg64(&mut self, src_dest: impl Into<Operand>) {
        match src_dest.into() {
            Operand::Register(src_dest) => {
                self.assembler.negq_r(src_dest);
            }

            Operand::Address(addr) => {
                self.assembler.negq_m(addr.offset, addr.base);
            }

            Operand::BaseIndex(addr) => {
                self.assembler
                    .negq_m_scaled(addr.offset, addr.base, addr.index, addr.scale as _);
            }

            src_dest => unreachable!("neg64: {:?}", src_dest),
        }
    }

    pub fn or64(&mut self, src: impl Into<Operand>, dest: impl Into<Operand>) {
        match (src.into(), dest.into()) {
            (Operand::Register(src), Operand::Register(dest)) => {
                self.assembler.orq_rr(src, dest);
            }

            (Operand::Register(src), Operand::Address(dest)) => {
                self.assembler.orq_rm(src, dest.offset, dest.base);
            }

            (Operand::Register(src), Operand::BaseIndex(dest)) => {
                self.assembler.orq_rm_scaled(
                    src,
                    dest.offset,
                    dest.base,
                    dest.index,
                    dest.scale as _,
                );
            }

            (Operand::Address(src), Operand::Register(dest)) => {
                self.assembler.orq_mr(src.offset, src.base, dest);
            }

            (Operand::BaseIndex(src), Operand::Register(dest)) => {
                self.assembler
                    .orq_mr_scaled(src.offset, src.base, src.index, src.scale as _, dest);
            }

            (Operand::Imm32(imm), Operand::Register(dest)) => {
                self.assembler.orq_ir(imm, dest);
            }

            (Operand::Imm32(imm), Operand::Address(dest)) => {
                self.assembler.orq_im(imm, dest.offset, dest.base);
            }

            (Operand::Imm32(imm), Operand::BaseIndex(dest)) => {
                self.assembler.orq_im_scaled(
                    imm,
                    dest.offset,
                    dest.base,
                    dest.index,
                    dest.scale as _,
                );
            }

            (Operand::Imm64(imm), Operand::Register(dest)) => {
                if imm <= i32::MAX as i64 && imm >= i32::MIN as i64 {
                    self.assembler.orq_ir(imm as i32, dest);
                } else {
                    self.mov(imm, Self::SCRATCH_REGISTER);
                    self.assembler.orq_rr(Self::SCRATCH_REGISTER, dest);
                }
            }

            (src, dest) => unreachable!("or64: {:?}, {:?}", src, dest),
        }
    }

    pub fn or64_rrr(&mut self, src1: impl Into<Operand>, src2: u8, dest: u8) {
        match src1.into() {
            Operand::Imm32(imm) => {
                self.mov(src2, dest);
                self.or64(imm, dest);
            }

            Operand::Register(src1) => {
                if src1 == dest {
                    self.or64(src2, dest);
                } else {
                    self.mov(src1, dest);
                    self.or64(src2, dest);
                }
            }

            op => unreachable!("or64_rrr: {:?}", op),
        }
    }

    pub fn sub64(&mut self, src: impl Into<Operand>, dest: impl Into<Operand>) {
        match (src.into(), dest.into()) {
            (Operand::Register(src), Operand::Register(dest)) => {
                self.assembler.subq_rr(src, dest);
            }

            (Operand::Register(src), Operand::Address(dest)) => {
                self.assembler.subq_rm(src, dest.offset, dest.base);
            }

            (Operand::Register(src), Operand::BaseIndex(dest)) => {
                self.assembler.subq_rm_scaled(
                    src,
                    dest.offset,
                    dest.base,
                    dest.index,
                    dest.scale as _,
                );
            }

            (Operand::Address(src), Operand::Register(dest)) => {
                self.assembler.subq_mr(src.offset, src.base, dest);
            }

            (Operand::BaseIndex(src), Operand::Register(dest)) => {
                self.assembler.subq_mr_scaled(
                    src.offset,
                    src.base,
                    src.index,
                    src.scale as _,
                    dest,
                );
            }

            (Operand::Imm32(imm), Operand::Register(dest)) => {
                if imm == 1 {
                    self.assembler.decq_r(dest);
                    return;
                }
                self.assembler.subq_ir(imm, dest);
            }

            (Operand::Imm32(imm), Operand::Address(dest)) => {
                self.assembler.subq_im(imm, dest.offset, dest.base);
            }

            (Operand::Imm32(imm), Operand::BaseIndex(dest)) => {
                self.assembler.subq_im_scaled(
                    imm,
                    dest.offset,
                    dest.base,
                    dest.index,
                    dest.scale as _,
                );
            }

            (Operand::Imm64(imm), Operand::Register(dest)) => {
                if imm == 1 {
                    self.assembler.decq_r(dest);
                    return;
                }
                if imm <= i32::MAX as i64 && imm >= i32::MIN as i64 {
                    self.assembler.subq_ir(imm as i32, dest);
                } else {
                    self.mov(imm, Self::SCRATCH_REGISTER);
                    self.assembler.subq_rr(Self::SCRATCH_REGISTER, dest);
                }
            }

            (src, dest) => unreachable!("sub64: {:?}, {:?}", src, dest),
        }
    }

    pub fn sub64_rrr(&mut self, a: u8, b: impl Into<Operand>, dest: u8) {
        match b.into() {
            Operand::Imm32(imm) => {
                self.mov(a, dest);
                self.sub64(imm, dest);
            }

            Operand::Register(b) => {
                if b != dest {
                    self.mov(a, dest);
                    self.sub64(b, dest);
                } else if a != b {
                    self.neg64(b);
                    self.add64(a, b);
                } else {
                    self.mov(0i32, dest);
                }
            }

            op => unreachable!("sub64_rrr: {:?}", op),
        }
    }

    pub fn xor64(&mut self, src: impl Into<Operand>, dest: impl Into<Operand>) {
        match (src.into(), dest.into()) {
            (Operand::Register(src), Operand::Register(dest)) => {
                self.assembler.xorq_rr(src, dest);
            }

            (Operand::Register(src), Operand::Address(dest)) => {
                self.assembler.xorq_rm(src, dest.offset, dest.base);
            }

            (Operand::Register(src), Operand::BaseIndex(dest)) => {
                self.assembler.xorq_rm_scaled(
                    src,
                    dest.offset,
                    dest.base,
                    dest.index,
                    dest.scale as _,
                );
            }

            (Operand::Address(src), Operand::Register(dest)) => {
                self.assembler.xorq_mr(src.offset, src.base, dest);
            }

            (Operand::BaseIndex(src), Operand::Register(dest)) => {
                self.assembler.xorq_mr_scaled(
                    src.offset,
                    src.base,
                    src.index,
                    src.scale as _,
                    dest,
                );
            }

            (Operand::Imm32(imm), Operand::Register(dest)) => {
                self.assembler.xorq_ir(imm, dest);
            }

            (Operand::Imm32(imm), Operand::Address(dest)) => {
                self.assembler.xorq_im(imm, dest.offset, dest.base);
            }

            (Operand::Imm64(imm), Operand::Register(dest)) => {
                if imm <= i32::MAX as i64 && imm >= i32::MIN as i64 {
                    self.assembler.xorq_ir(imm as i32, dest);
                } else {
                    self.mov(imm, Self::SCRATCH_REGISTER);
                    self.assembler.xorq_rr(Self::SCRATCH_REGISTER, dest);
                }
            }

            (src, dest) => unreachable!("xor64: {:?}, {:?}", src, dest),
        }
    }

    pub fn xor64_rrr(&mut self, a: impl Into<Operand>, b: u8, dest: u8) {
        match a.into() {
            Operand::Register(a) => {
                if a == b {
                    self.mov(0i32, dest);
                } else if a == dest {
                    self.xor64(b, dest);
                } else {
                    self.mov(b, dest);
                    self.xor64(a, dest);
                }
            }

            Operand::Imm32(imm) => {
                self.mov(b, dest);
                self.xor64(imm, dest);
            }

            Operand::Imm64(imm) => {
                self.mov(b, dest);
                self.xor64(imm, dest);
            }

            op => unreachable!("xor64_rrr: {:?}", op),
        }
    }

    pub fn not64(&mut self, src_dest: impl Into<Operand>) {
        match src_dest.into() {
            Operand::Register(src_dest) => {
                self.assembler.notq_r(src_dest);
            }

            Operand::Address(addr) => {
                self.assembler.notq_m(addr.offset, addr.base);
            }

            Operand::BaseIndex(addr) => {
                self.assembler
                    .notq_m_scaled(addr.offset, addr.base, addr.index, addr.scale as _);
            }

            src_dest => unreachable!("not64: {:?}", src_dest),
        }
    }

    pub fn zero_extend8_to_64(&mut self, src: u8, dest: u8) {
        self.assembler.movsbq_rr(src, dest)
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

    pub fn load64_with_address_offset_patch(&mut self, address: Address, dest: u8) -> DataLabel32 {
        self.pad_before_patch();
        self.assembler
            .movq_mr_disp32(address.offset, address.base, dest);
        DataLabel32::new(self)
    }

    pub fn load64_with_compact_address_offset_patch(
        &mut self,
        address: Address,
        dest: u8,
    ) -> DataLabelCompact {
        self.pad_before_patch();
        self.assembler
            .movq_mr_disp8(address.offset, address.base, dest);
        DataLabelCompact::new(self)
    }

    pub fn load64(&mut self, address: impl Into<Operand>, dest: u8) {
        match address.into() {
            Operand::Address(addr) => {
                self.assembler.movq_mr(addr.offset, addr.base, dest);
            }

            Operand::BaseIndex(addr) => {
                self.assembler.movq_mr_scaled(
                    addr.offset,
                    addr.base,
                    addr.index,
                    addr.scale as _,
                    dest,
                );
            }

            Operand::AbsoluteAddress(addr) => {
                if dest == eax {
                    self.assembler.movq_meax(addr.ptr);
                } else {
                    self.mov(addr, dest);
                    self.load64(Address::new(dest, 0), dest);
                }
            }

            address => unreachable!("load64: {:?}", address),
        }
    }

    pub fn store64(&mut self, src: impl Into<Operand>, dest: impl Into<Operand>) {
        match (src.into(), dest.into()) {
            (Operand::Register(src), Operand::Address(dest)) => {
                self.assembler.movq_rm(src, dest.offset, dest.base);
            }

            (Operand::Register(src), Operand::BaseIndex(dest)) => {
                self.assembler.movq_rm_scaled(
                    src,
                    dest.offset,
                    dest.base,
                    dest.index,
                    dest.scale as _,
                );
            }

            (Operand::Register(src), Operand::AbsoluteAddress(addr)) => {
                if src == eax {
                    self.assembler.movl_eaxm(addr.ptr);
                } else {
                    self.mov(addr, Self::SCRATCH_REGISTER);
                    self.store64(src, Address::new(Self::SCRATCH_REGISTER, 0));
                }
            }

            (Operand::Imm32(imm), Operand::Address(dest)) => {
                self.assembler.movq_i32m(imm, dest.offset, dest.base);
            }

            (Operand::Imm32(imm), Operand::BaseIndex(dest)) => {
                self.assembler.movq_i32m_scaled(
                    imm,
                    dest.offset,
                    dest.base,
                    dest.index,
                    dest.scale as _,
                );
            }

            (Operand::Imm64(imm), Operand::AbsoluteAddress(addr)) => {
                if can_sign_extend_32_64(imm) {
                    self.mov(addr, Self::SCRATCH_REGISTER);
                    self.store64(imm as i32, Address::new(Self::SCRATCH_REGISTER, 0));
                } else {
                    self.mov(imm, Self::SCRATCH_REGISTER);
                    self.swap(Self::SCRATCH_REGISTER, eax);
                    self.assembler.movq_eaxm(addr.ptr);
                    self.swap(Self::SCRATCH_REGISTER, eax);
                }
            }

            (Operand::AbsoluteAddress(addr), Operand::Address(address)) => {
                self.mov(addr, Self::SCRATCH_REGISTER);
                self.store64(Self::SCRATCH_REGISTER, address);
            }

            (Operand::AbsoluteAddress(addr), Operand::BaseIndex(address)) => {
                self.mov(addr, Self::SCRATCH_REGISTER);
                self.store64(Self::SCRATCH_REGISTER, address);
            }

            (Operand::Imm64(imm), Operand::BaseIndex(address)) => {
                self.mov(imm, Self::SCRATCH_REGISTER);
                self.assembler.movq_rm_scaled(
                    Self::SCRATCH_REGISTER,
                    address.offset,
                    address.base,
                    address.index,
                    address.scale as _,
                );
            }

            (src, dest) => unreachable!("store64: {:?}, {:?}", src, dest),
        }
    }

    pub fn store_pair64(&mut self, src1: u8, src2: u8, dest: u8, offset: i32) {
        self.store64(src1, Address::new(dest, offset));
        self.store64(src2, Address::new(dest, offset + 8));
    }

    pub fn transfer32(&mut self, src: Address, dest: Address) {
        self.load32(src, Self::SCRATCH_REGISTER);
        self.store32(Self::SCRATCH_REGISTER, dest);
    }

    pub fn transfer64(&mut self, src: Address, dest: Address) {
        self.load64(src, Self::SCRATCH_REGISTER);
        self.store64(Self::SCRATCH_REGISTER, dest);
    }

    pub fn transfer_ptr(&mut self, src: Address, dest: Address) {
        self.load64(src, Self::SCRATCH_REGISTER);
        self.store64(Self::SCRATCH_REGISTER, dest);
    }

    pub fn swap64(&mut self, src: u8, dest: impl Into<Operand>) {
        match dest.into() {
            Operand::Register(dest) => {
                self.assembler.xchgq_rr(src, dest);
            }

            Operand::Address(dest) => {
                self.assembler.xchgq_rm(src, dest.offset, dest.base);
            }

            op => unreachable!("swap64: {:?}", op),
        }
    }

    /// Swaps the contents of two XMM registers.
    ///
    /// NOTE: This is a hacky implementation that uses xmm7 as a temporary register.
    pub fn swap_double(&mut self, reg1: u8, reg2: u8) {
        if reg1 == reg2 {
            return;
        }
        // FIXME: This is kinda a hack since we don't use xmm7 as a temp.
        self.move_double(reg1, xmm7);
        self.move_double(reg2, reg1);
        self.move_double(xmm7, reg2);
    }

    pub fn store64_with_address_offset_patch(&mut self, src: u8, address: Address) -> DataLabel32 {
        self.pad_before_patch();
        self.assembler
            .movq_rm_disp32(src, address.offset, address.base);
        DataLabel32::new(self)
    }

    pub fn move32_to_float(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(src) => self.assembler.movd_r2f(src, dest),
            Operand::Imm32(imm) => {
                self.mov(imm, Self::SCRATCH_REGISTER);
                self.assembler.movd_r2f(Self::SCRATCH_REGISTER, dest);
            }

            op => unreachable!("move32_to_float: {:?}", op),
        }
    }

    pub fn move64_to_double(&mut self, src: impl Into<Operand>, dest: u8) {
        match src.into() {
            Operand::Register(src) => self.assembler.movq_r2f(src, dest),
            Operand::Imm64(imm) => {
                self.mov(imm, Self::SCRATCH_REGISTER);
                self.assembler.movq_r2f(Self::SCRATCH_REGISTER, dest);
            }

            op => unreachable!("move64_to_double: {:?}", op),
        }
    }

    pub fn move_double_to64(&mut self, src: u8, dest: u8) {
        self.assembler.movq_f2r(src, dest);
    }

    pub fn compare64(
        &mut self,
        cond: RelationalCondition,
        left: u8,
        right: impl Into<Operand>,
        dest: u8,
    ) {
        match right.into() {
            Operand::Imm32(imm) => {
                if imm == 0 {
                    if let Some(result_condition) = self.compute_compare_to_zero_into_test(cond) {
                        self.test64(result_condition, left, left, dest);
                        return;
                    }
                }

                self.assembler.cmpq_ir(imm, left);
                self.set32(cond.x86_condition(), dest);
            }

            Operand::Register(right) => {
                self.assembler.cmpq_rr(right, left);
                self.set32(cond.x86_condition(), dest);
            }

            op => unreachable!("compare64: {:?}, {:?}, {:?}", cond, left, op),
        }
    }

    pub fn test64(&mut self, cond: ResultCondition, reg: u8, mask: impl Into<Operand>, dest: u8) {
        match mask.into() {
            Operand::Imm32(imm) => {
                if imm == -1 {
                    self.assembler.testq_rr(reg, reg);
                } else if (imm & !0x7f) == 0 {
                    self.assembler.testb_i8r(imm, reg);
                } else {
                    self.assembler.testq_i32r(imm, reg);
                }

                self.set32(cond.x86_condition(), dest);
            }

            Operand::Register(mask) => {
                self.assembler.testq_rr(mask, reg);
                self.set32(cond.x86_condition(), dest);
            }

            op => unreachable!("test64: {:?}, {:?}, {:?}", cond, reg, op),
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
                self.assembler.cmpq_rr(right, left);
                Jump::new(self.assembler.jcc(cond.x86_condition()))
            }

            (Operand::Register(left), Operand::Imm32(imm)) => {
                if imm == 0 {
                    if let Some(result_condition) = self.compute_compare_to_zero_into_test(cond) {
                        return self.branch_test64(result_condition, left, left);
                    }
                }
                self.assembler.cmpq_ir(imm, left);
                Jump::new(self.assembler.jcc(cond.x86_condition()))
            }

            (Operand::Register(left), Operand::Imm64(imm)) => {
                if (cond == RelationalCondition::Equal || cond == RelationalCondition::NotEqual)
                    && imm == 0
                {
                    self.assembler.testq_rr(left, left);
                    return Jump::new(self.assembler.jcc(cond.x86_condition()));
                }

                self.mov(imm, Self::SCRATCH_REGISTER);
                self.branch64(cond, left, Self::SCRATCH_REGISTER)
            }

            (Operand::Register(left), Operand::Address(right)) => {
                self.assembler.cmpq_mr(right.offset, right.base, left);
                Jump::new(self.assembler.jcc(cond.x86_condition()))
            }

            (Operand::AbsoluteAddress(left), Operand::Register(right)) => {
                self.mov(left, Self::SCRATCH_REGISTER);
                self.branch64(cond, Address::new(Self::SCRATCH_REGISTER, 0), right)
            }

            (Operand::Address(left), Operand::Register(right)) => {
                self.assembler.cmpq_rm(right, left.offset, left.base);
                Jump::new(self.assembler.jcc(cond.x86_condition()))
            }

            (Operand::Address(left), Operand::Imm32(imm)) => {
                self.assembler.cmpq_im(imm, left.offset, left.base);
                Jump::new(self.assembler.jcc(cond.x86_condition()))
            }

            (Operand::Address(left), Operand::Imm64(imm)) => {
                self.mov(imm, Self::SCRATCH_REGISTER);
                self.branch64(
                    cond,
                    Address::new(left.base, left.offset),
                    Self::SCRATCH_REGISTER,
                )
            }

            (Operand::BaseIndex(left), Operand::Register(right)) => {
                self.assembler.cmpq_rm_scaled(
                    right,
                    left.offset,
                    left.base,
                    left.index,
                    left.scale as _,
                );
                Jump::new(self.assembler.jcc(cond.x86_condition()))
            }

            (Operand::Address(left), Operand::Address(right)) => {
                self.load64(right, Self::SCRATCH_REGISTER);
                self.branch64(cond, left, Self::SCRATCH_REGISTER)
            }

            (left, right) => unreachable!("branch64: {:?}, {:?}, {:?}", cond, left, right),
        }
    }

    pub fn branch_test64(
        &mut self,
        cond: ResultCondition,
        reg: impl Into<Operand>,
        mask: impl Into<Operand>,
    ) -> Jump {
        match (reg.into(), mask.into()) {
            (Operand::Register(reg), Operand::Imm32(imm)) => {
                if imm == -1 {
                    self.assembler.testq_rr(reg, reg);
                } else if (imm & !0x7f) == 0 {
                    self.assembler.testb_i8r(imm, reg);
                } else {
                    self.assembler.testq_i32r(imm, reg);
                }

                Jump::new(self.assembler.jcc(cond.x86_condition()))
            }

            (Operand::Register(reg), Operand::Register(mask)) => {
                self.assembler.testq_rr(mask, reg);
                Jump::new(self.assembler.jcc(cond.x86_condition()))
            }

            (Operand::Register(reg), Operand::Imm64(mask)) => {
                self.mov(mask, Self::SCRATCH_REGISTER);
                self.branch_test64(cond, reg, Self::SCRATCH_REGISTER)
            }

            (Operand::Address(address), Operand::Imm32(imm)) => {
                if imm == -1 {
                    self.assembler.cmpq_im(0, address.offset, address.base);
                } else {
                    self.assembler.testq_i32m(imm, address.offset, address.base);
                }

                Jump::new(self.assembler.jcc(cond.x86_condition()))
            }

            (Operand::Address(address), Operand::Register(mask)) => {
                self.assembler.testq_rm(mask, address.offset, address.base);
                Jump::new(self.assembler.jcc(cond.x86_condition()))
            }

            (Operand::BaseIndex(address), Operand::Imm32(imm)) => {
                if imm == -1 {
                    self.assembler.cmpq_im_scaled(
                        0,
                        address.offset,
                        address.base,
                        address.index,
                        address.scale as _,
                    );
                } else {
                    self.assembler.testq_i32m_scaled(
                        imm,
                        address.offset,
                        address.base,
                        address.index,
                        address.scale as _,
                    );
                }

                Jump::new(self.assembler.jcc(cond.x86_condition()))
            }

            (reg, mask) => unreachable!("branch_test64: {:?}, {:?}, {:?}", cond, reg, mask),
        }
    }

    pub fn branch_test_bit64(
        &mut self,
        cond: ResultCondition,
        test_value: impl Into<Operand>,
        bit: impl Into<Operand>,
    ) -> Jump {
        match (test_value.into(), bit.into()) {
            (Operand::Register(test_value), Operand::Imm32(bit)) => {
                self.assembler.btw_ir(bit, test_value);

                if cond == ResultCondition::NotZero {
                    return Jump::new(self.assembler.jb());
                } else if cond == ResultCondition::Zero {
                    return Jump::new(self.assembler.jae());
                } else {
                    unreachable!("branch_test_bit64: {:?}, {:?}, {:?}", cond, test_value, bit)
                }
            }

            (Operand::Register(test_value), Operand::Register(bit)) => {
                self.assembler.btw_rr(bit, test_value);
                if cond == ResultCondition::NotZero {
                    return Jump::new(self.assembler.jb());
                } else if cond == ResultCondition::Zero {
                    return Jump::new(self.assembler.jae());
                } else {
                    unreachable!("branch_test_bit64: {:?}, {:?}, {:?}", cond, test_value, bit)
                }
            }

            (Operand::Address(test_value), Operand::Imm32(bit)) => {
                self.assembler
                    .btw_im(bit, test_value.offset, test_value.base);
                if cond == ResultCondition::NotZero {
                    return Jump::new(self.assembler.jb());
                } else if cond == ResultCondition::Zero {
                    return Jump::new(self.assembler.jae());
                } else {
                    unreachable!("branch_test_bit64: {:?}, {:?}, {:?}", cond, test_value, bit)
                }
            }

            (test_value, bit) => {
                unreachable!("branch_test_bit64: {:?}, {:?}, {:?}", cond, test_value, bit)
            }
        }
    }

    pub fn branch_add64(
        &mut self,
        cond: ResultCondition,
        src: impl Into<Operand>,
        dest: impl Into<Operand>,
    ) -> Jump {
        self.add64(src, dest);
        Jump::new(self.assembler.jcc(cond.x86_condition()))
    }

    pub fn branch_add64_rrr(
        &mut self,
        cond: ResultCondition,
        src1: impl Into<Operand>,
        src2: impl Into<Operand>,
        dest: u8,
    ) -> Jump {
        match (src1.into(), src2.into()) {
            (Operand::Register(src1), Operand::Register(src2)) => {
                if src1 == dest {
                    return self.branch_add64(cond, src2, dest);
                }

                self.mov(src2, dest);
                self.branch_add64(cond, src1, dest)
            }

            (Operand::Address(op1), Operand::Register(op2))
            | (Operand::Register(op2), Operand::Address(op1)) => {
                if op2 == dest {
                    return self.branch_add64(cond, op1, dest);
                }

                if op1.base == dest {
                    self.load32(op1, dest);
                    return self.branch_add64(cond, op2, dest);
                }

                self.mov(op2, dest);
                self.branch_add64(cond, op1, dest)
            }

            (src1, src2) => unreachable!(
                "branch_add64_rrr: {:?}, {:?}, {:?}, {:?}",
                cond, src1, src2, dest
            ),
        }
    }

    pub fn branch_mul64(&mut self, cond: ResultCondition, src: u8, dest: u8) -> Jump {
        self.mul64(src, dest);
        if cond != ResultCondition::Overflow {
            self.assembler.testq_rr(dest, dest);
        }

        Jump::new(self.assembler.jcc(cond.x86_condition()))
    }

    pub fn branch_mul64_rrr(
        &mut self,
        cond: ResultCondition,
        src1: u8,
        src2: u8,
        dest: u8,
    ) -> Jump {
        if src1 == dest {
            return self.branch_mul64(cond, src2, dest);
        }

        self.mov(src2, dest);
        self.branch_mul64(cond, src1, dest)
    }

    pub fn branch_sub64(
        &mut self,
        cond: ResultCondition,
        src: impl Into<Operand>,
        dest: u8,
    ) -> Jump {
        self.sub64(src, dest);
        Jump::new(self.assembler.jcc(cond.x86_condition()))
    }

    pub fn branch_sub64_rrr(
        &mut self,
        cond: ResultCondition,
        src1: u8,
        src2: i32,
        dest: u8,
    ) -> Jump {
        self.mov(src1, dest);
        self.branch_sub64(cond, src2, dest)
    }

    pub fn move_conditionally64(
        &mut self,
        cond: RelationalCondition,
        left: u8,
        right: u8,
        src: u8,
        dest: u8,
    ) {
        self.assembler.cmpq_rr(right, left);
        self.cmov(cond.x86_condition(), src, dest);
    }

    pub fn move_conditionally64_then_else(
        &mut self,
        cond: RelationalCondition,
        left: u8,
        right: u8,
        then_case: u8,
        mut else_case: u8,
        dest: u8,
    ) {
        self.assembler.cmpq_rr(right, left);
        if then_case != dest && else_case != dest {
            self.mov(else_case, dest);
            else_case = dest;
        }

        if else_case == dest {
            self.cmov(cond.x86_condition(), then_case, dest)
        } else {
            self.cmov(Self::invert(cond).x86_condition(), else_case, dest)
        }
    }

    pub fn move_conditionally64_imm_then_else(
        &mut self,
        cond: RelationalCondition,
        left: u8,
        right: i32,
        then_case: u8,
        mut else_case: u8,
        dest: u8,
    ) {
        self.assembler.cmpq_ir(right, left);
        if then_case != dest && else_case != dest {
            self.mov(else_case, dest);
            else_case = dest;
        }

        if else_case == dest {
            self.cmov(cond.x86_condition(), then_case, dest)
        } else {
            self.cmov(Self::invert(cond).x86_condition(), else_case, dest)
        }
    }

    pub fn move_conditionally_test64(
        &mut self,
        cond: ResultCondition,
        left: u8,
        right: u8,
        src: u8,
        dest: u8,
    ) {
        self.assembler.testq_rr(right, left);
        self.cmov(cond.x86_condition(), src, dest);
    }

    pub fn move_conditionally_test64_then_else(
        &mut self,
        cond: ResultCondition,
        left: u8,
        right: u8,
        then_case: u8,
        mut else_case: u8,
        dest: u8,
    ) {
        self.assembler.testq_rr(right, left);
        if then_case != dest && else_case != dest {
            self.mov(else_case, dest);
            else_case = dest;
        }

        if else_case == dest {
            self.cmov(cond.x86_condition(), then_case, dest)
        } else {
            self.cmov(Self::invert_result(cond).x86_condition(), else_case, dest)
        }
    }

    pub fn move_conditionally_test64_imm(
        &mut self,
        cond: ResultCondition,
        left: u8,
        right: i32,
        src: u8,
        dest: u8,
    ) {
        if right == -1 {
            self.assembler.testq_rr(left, left);
        } else if (right & !0x7f) == 0 {
            self.assembler.testb_i8r(right, left);
        } else {
            self.assembler.testq_i32r(right, left);
        }

        self.cmov(cond.x86_condition(), src, dest);
    }

    pub fn move_conditionally_test64_imm_then_else(
        &mut self,
        cond: ResultCondition,
        left: u8,
        right: i32,
        then_case: u8,
        mut else_case: u8,
        dest: u8,
    ) {
        if right == -1 {
            self.assembler.testq_rr(left, left);
        } else if (right & !0x7f) == 0 {
            self.assembler.testb_i8r(right, left);
        } else {
            self.assembler.testq_i32r(right, left);
        }

        if then_case != dest && else_case != dest {
            self.mov(else_case, dest);
            else_case = dest;
        }

        if else_case == dest {
            self.cmov(cond.x86_condition(), then_case, dest)
        } else {
            self.cmov(Self::invert_result(cond).x86_condition(), else_case, dest)
        }
    }

    pub fn convertible_load_ptr(&mut self, address: Address, dest: u8) -> ConvertibleLoadLabel {
        let result = ConvertibleLoadLabel::new(self);
        self.assembler.movq_mr(address.offset, address.base, dest);
        result
    }

    pub fn move_with_patch(&mut self, imm: impl Into<Operand>, dest: u8) -> DataLabelPtr {
        self.pad_before_patch();
        match imm.into() {
            Operand::Imm32(imm) => {
                self.assembler.movq_i64r(imm as i64, dest);
            }
            Operand::Imm64(imm) => {
                self.assembler.movq_i64r(imm, dest);
            }

            Operand::AbsoluteAddress(addr) => {
                self.assembler.movq_i64r(addr.ptr as i64, dest);
            }

            op => unreachable!("Unexpected operand: {:?}", op),
        }

        DataLabelPtr::new(self)
    }

    pub fn branch_ptr_with_patch(
        &mut self,
        cond: RelationalCondition,
        left: impl Into<Operand>,
        initial_right_value: isize,
    ) -> (Jump, DataLabelPtr) {
        let data_label = self.move_with_patch(initial_right_value as i64, Self::SCRATCH_REGISTER);

        (
            self.branch64(cond, left, Self::SCRATCH_REGISTER),
            data_label,
        )
    }

    pub fn branch32_with_patch(
        &mut self,
        cond: RelationalCondition,
        left: impl Into<Operand>,
        initial_right_value: i32,
    ) -> (Jump, DataLabelPtr) {
        self.pad_before_patch();
        self.assembler
            .movl_i32r(initial_right_value, Self::SCRATCH_REGISTER);
        let data_label = DataLabelPtr::new(self);
        (
            self.branch32(cond, left, Self::SCRATCH_REGISTER),
            data_label,
        )
    }

    pub fn store_ptr_with_patch(&mut self, initial_value: isize, address: Address) -> DataLabelPtr {
        let label = self.move_with_patch(initial_value as i64, Self::SCRATCH_REGISTER);
        self.store64(Self::SCRATCH_REGISTER, address);
        label
    }

    pub fn patchable_branch64(
        &mut self,
        cond: RelationalCondition,
        reg: u8,
        right: impl Into<Operand>,
    ) -> PatchableJump {
        self.pad_before_patch();
        PatchableJump(self.branch64(cond, reg, right))
    }

    pub fn xchg64(&mut self, reg: u8, dest: impl Into<Operand>) {
        match dest.into() {
            Operand::Register(dest) => self.assembler.xchgq_rr(reg, dest),
            Operand::Address(address) => self.assembler.xchgq_rm(reg, address.offset, address.base),
            op => unreachable!("Unexpected operand: {:?}", op),
        }
    }

    pub fn load_from_tls64(&mut self, offset: u32, dst: u8) {
        self.assembler.gs();
        self.assembler.movq_mr_addr(offset as _, dst)
    }

    pub fn store_to_tls64(&mut self, offset: u32, src: u8) {
        self.assembler.gs();
        self.assembler.movq_rm_addr(src, offset as _)
    }

    pub fn truncate_double_to_uint64(&mut self, src: u8, dest: u8) {
        self.assembler.cvttsd2siq_rr(src, dest)
    }

    pub fn truncate_double_to_int64(&mut self, src: u8, dest: u8) {
        self.assembler.cvttsd2siq_rr(src, dest)
    }

    pub fn truncate_float_to_uint32(&mut self, src: u8, dest: u8) {
        self.assembler.cvtss2siq_rr(src, dest)
    }

    pub fn truncate_float_to_int64(&mut self, src: u8, dest: u8) {
        self.assembler.cvtss2siq_rr(src, dest)
    }
}
