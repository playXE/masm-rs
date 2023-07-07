pub const fn is_uint12(x: i32) -> bool {
    (x & !0xfff) == 0
}

pub const fn is_int9(x: i32) -> bool {
    x == ((x << 23) >> 23)
}

pub const fn is_int<const BITS: usize>(t: i32) -> bool {
    t == ((t << (32 - BITS)) >> (32 - BITS))
}

pub const fn is_valid_scaled_uimm12<const DATASIZE: i32>(offset: i32) -> bool {
    let max_pimm = 4095 * (DATASIZE / 8);

    if offset < 0 {
        return false;
    } else if offset > max_pimm {
        return false;
    }

    if (offset & ((DATASIZE / 8) - 1)) != 0 {
        return false;
    }

    true
}

pub const fn is_valid_signed_imm9(x: i32) -> bool {
    is_int9(x)
}

pub const fn is_valid_signed_imm7(x: i32, alignment_shift_amount: i32) -> bool {
    const DISALLOWED_HIGH_BITS: i32 = 32 - 7;

    let shifted_value = x >> alignment_shift_amount;
    let fits_in_7bits =
        shifted_value == (shifted_value << DISALLOWED_HIGH_BITS) >> DISALLOWED_HIGH_BITS;
    let has_correct_alignment = x == shifted_value << alignment_shift_amount;

    fits_in_7bits && has_correct_alignment
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct ARM64LogicalImmediate(pub i32);

impl ARM64LogicalImmediate {
    pub fn create32(mut value: u32) -> Self {
        if value == 0 || !value == 0 {
            return ARM64LogicalImmediate(-1);
        }

        // First look for a 32-bit pattern, then for repeating 16-bit
        // patterns, 8-bit, 4-bit, and finally 2-bit.
        let mut hsb = 0;
        let mut lsb = 0;
        let mut inverted = false;
        
        if Self::find_bit_range::<32>(value as u64, &mut hsb, &mut lsb, &mut inverted) {
            return Self(Self::encode_logical_immediate::<32>(hsb, lsb, inverted));
        }
       
        if (value & 0xffff) != (value >> 16) {
            return ARM64LogicalImmediate(-1);
        }

        value &= 0xffff;

        if Self::find_bit_range::<16>(value as _, &mut hsb, &mut lsb, &mut inverted) {
            return Self(Self::encode_logical_immediate::<16>(hsb, lsb, inverted));
        }

        if (value & 0xff) != (value >> 8) {
            return ARM64LogicalImmediate(-1);
        }

        value &= 0xff;

        if Self::find_bit_range::<8>(value as _, &mut hsb, &mut lsb, &mut inverted) {
            return Self(Self::encode_logical_immediate::<8>(hsb, lsb, inverted));
        }

        if (value & 0xf) != (value >> 4) {
            return ARM64LogicalImmediate(-1);
        }

        value &= 0xf;

        if Self::find_bit_range::<4>(value as _, &mut hsb, &mut lsb, &mut inverted) {
            return Self(Self::encode_logical_immediate::<4>(hsb, lsb, inverted));
        }

        if (value & 0x3) != (value >> 2) {
            return ARM64LogicalImmediate(-1);
        }

        value &= 0x3;

        if Self::find_bit_range::<2>(value as _, &mut hsb, &mut lsb, &mut inverted) {
            return Self(Self::encode_logical_immediate::<2>(hsb, lsb, inverted));
        }

        ARM64LogicalImmediate(-1)
    }

    pub fn create64(value: u64) -> Self {
        if value == 0 || !value == 0 {
            return ARM64LogicalImmediate(-1);
        }

        let mut hsb = 0;
        let mut lsb = 0;
        let mut inverted = false;

        if Self::find_bit_range::<64>(value, &mut hsb, &mut lsb, &mut inverted) {
            return Self(Self::encode_logical_immediate::<64>(hsb, lsb, inverted));
        }

        if value as u32 == (value >> 32) as u32 {
            return Self::create32(value as u32);
        }

        ARM64LogicalImmediate(-1)
    }

    pub fn value(&self) -> i32 {
        self.0
    }

    pub fn is_valid(&self) -> bool {
        self.0 != -1
    }

    pub fn is_64bit(&self) -> bool {
        (self.0 & (1 << 12)) != 0
    }

    /// Generate a mask with bits in the range hsb..0 set, for example:
    ///   hsb:63 = 0xffffffffffffffff
    ///   hsb:42 = 0x000007ffffffffff
    ///   hsb: 0 = 0x0000000000000001
    const fn mask(hsb: usize) -> u64 {
        0xffffffffffffffffu64 >> (63 - hsb)
    }

    fn partial_hsb<const N: usize>(value: &mut u64, result: &mut usize) {
        if (*value & (0xffffffffffffffff << N)) != 0 {
            *result += N;
            *value >>= N;
        }
    }
    /// Find the bit number of the highest bit set in a non-zero value, for example:
    ///   0x8080808080808080 = hsb:63
    ///   0x0000000000000001 = hsb: 0
    ///   0x000007ffffe00000 = hsb:42
    fn highest_set_bit(mut value: u64) -> usize {
        let mut hsb = 0;
        Self::partial_hsb::<32>(&mut value, &mut hsb);
        Self::partial_hsb::<16>(&mut value, &mut hsb);
        Self::partial_hsb::<8>(&mut value, &mut hsb);
        Self::partial_hsb::<4>(&mut value, &mut hsb);
        Self::partial_hsb::<2>(&mut value, &mut hsb);
        Self::partial_hsb::<1>(&mut value, &mut hsb);

        hsb
    }

    fn find_bit_range<const WIDTH: usize>(
        mut value: u64,
        hsb: &mut usize,
        lsb: &mut usize,
        inverted: &mut bool,
    ) -> bool {
        assert!((value & Self::mask(WIDTH as usize - 1)) != 0);
        assert!(value != Self::mask(WIDTH as usize - 1));
        assert!((value & !Self::mask(WIDTH as usize - 1)) == 0);
        // Detect cases where the top bit is set; if so, flip all the bits & set invert.
        // This halves the number of patterns we need to look for.
        let msb = 1 << (WIDTH - 1);

        *inverted = (value & msb) != 0;
        if *inverted {
            value ^= Self::mask(WIDTH - 1);
        }

        *hsb = Self::highest_set_bit(value);
        value ^= Self::mask(*hsb);

        // Find the highest set bit in value, generate a corresponding mask & flip all
        // bits under it.
        if value == 0 {
            // If this cleared the value, then the range hsb..0 was all set.
            *lsb = 0;
            return true;
        }
        // Try making one more mask, and flipping the bits!
        *lsb = Self::highest_set_bit(value);
        value ^= Self::mask(*lsb);

        if value == 0 {
            // Success - but lsb actually points to the hsb of a third range - add one
            // to get to the lsb of the mid range.
            *lsb += 1;
            return true;
        }

        false
    }

    fn encode_logical_immediate<const WIDTH: usize>(hsb: usize, lsb: usize, inverted: bool) -> i32 {
        let mut imm_n = 0;
        let mut imm_s = 0;
        let imm_r;

        // For 64-bit values this is easy - just set immN to true, and imms just
        // contains the bit number of the highest set bit of the set range. For
        // values with narrower widths, these are encoded by a leading set of
        // one bits, followed by a zero bit, followed by the remaining set of bits
        // being the high bit of the range. For a 32-bit immediate there are no
        // leading one bits, just a zero followed by a five bit number. For a
        // 16-bit immediate there is one one bit, a zero bit, and then a four bit
        // bit-position, etc.
        if WIDTH == 64 {
            imm_n = 1;
        } else {
            imm_s = 63 & !(WIDTH + WIDTH - 1)
        }

        if inverted {
            // if width is 64 & hsb is 62, then we have a value something like:
            //   0x80000000ffffffff (in this case with lsb 32).
            // The ror should be by 1, imms (effectively set width minus 1) is
            // 32. Set width is full width minus cleared width.
            imm_r = (WIDTH - 1) - hsb;
            imm_s |= (WIDTH - ((hsb - lsb) + 1)) - 1;
        } else {
            // if width is 64 & hsb is 62, then we have a value something like:
            //   0x7fffffff00000000 (in this case with lsb 32).
            // The value is effectively rol'ed by lsb, which is equivalent to
            // a ror by width - lsb (or 0, in the case where lsb is 0). imms
            // is hsb - lsb.
            imm_r = (WIDTH - lsb) & (WIDTH - 1);
            imm_s |= hsb - lsb;
        }

        (imm_n << 12 | imm_r << 6 | imm_s) as i32
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(C)]
pub enum SIMDLane {
    V128 = 0,
    I8X16,
    I16X8,
    I32X4,
    I64X2,
    F32X4,
    F64X2,
}

impl SIMDLane {
    pub const fn element_byte_size(self) -> i32 {
        match self {
            Self::V128 => 16,
            Self::I8X16 => 1,
            Self::I16X8 => 2,
            Self::I32X4 => 4,
            Self::I64X2 => 8,
            Self::F32X4 => 4,
            Self::F64X2 => 8,
        }
    }

    pub const fn narrowed(self) -> Self {
        match self {
            Self::I16X8 => Self::I8X16,
            Self::I32X4 => Self::I16X8,
            Self::I64X2 => Self::I32X4,
            Self::F64X2 => Self::F32X4,
            _ => unreachable!(),
        }
    }

    pub fn promoted(self) -> Self {
        match self {
            Self::I8X16 => Self::I16X8,
            Self::I16X8 => Self::I32X4,
            Self::I32X4 => Self::I64X2,
            Self::F32X4 => Self::F64X2,
            _ => unreachable!(),
        }
    }

    pub fn scalar_type_is_integral(self) -> bool {
        match self {
            Self::I8X16 | Self::I16X8 | Self::I32X4 | Self::I64X2 => true,
            _ => false,
        }
    }

    pub fn scalar_type_is_floating_point(self) -> bool {
        match self {
            Self::F32X4 | Self::F64X2 => true,
            _ => false,
        }
    }

    pub fn element_count(self) -> usize {
        match self {
            Self::V128 => unreachable!(),
            Self::I8X16 => 16,
            Self::I16X8 => 8,
            Self::I32X4 => 4,
            Self::I64X2 => 2,
            Self::F32X4 => 4,
            Self::F64X2 => 2,
        }
    }
}
