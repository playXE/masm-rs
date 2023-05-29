use cfg_if::cfg_if;



cfg_if! {
    if #[cfg(target_arch="x86_64")] {

        use crate::assembler::x86assembler::*;
        pub const NUMBER_OF_REGISTERS: usize = 6;
        pub const NUMBER_OF_ARGUMENT_REGISTERS: usize = 8;

        pub const FPREG_T0: u8 = xmm0;
        pub const FPREG_T1: u8 = xmm1;
        pub const FPREG_T2: u8 = xmm2;
        pub const FPREG_T3: u8 = xmm3;
        pub const FPREG_T4: u8 = xmm4;
        pub const FPREG_T5: u8 = xmm5;

        pub const ARGUMENT_FPR0: u8 = xmm0;
        pub const ARGUMENT_FPR1: u8 = xmm1;
        pub const ARGUMENT_FPR2: u8 = xmm2;
        pub const ARGUMENT_FPR3: u8 = xmm3;
        pub const ARGUMENT_FPR4: u8 = xmm4;
        pub const ARGUMENT_FPR5: u8 = xmm5;
        pub const ARGUMENT_FPR6: u8 = xmm6;
        pub const ARGUMENT_FPR7: u8 = xmm7;

        pub const RETURN_VALUE_FPR: u8 = xmm0;

        pub const NON_PRESERVED_NON_ARGUMENT_FPR0: u8 = xmm8;

        pub const fn to_argument_register(i: usize) -> u8 {
            match i {
                0 => ARGUMENT_FPR0,
                1 => ARGUMENT_FPR1,
                2 => ARGUMENT_FPR2,
                3 => ARGUMENT_FPR3,
                4 => ARGUMENT_FPR4,
                5 => ARGUMENT_FPR5,
                6 => ARGUMENT_FPR6,
                7 => ARGUMENT_FPR7,
                _ => panic!("Invalid argument register index"),
            }
        }
    } else if #[cfg(target_arch="riscv64")] {
        use crate::assembler::riscv64assembler::*;

        pub const FPREG_T0: u8 = f10;
        pub const FPREG_T1: u8 = f11;
        pub const FPREG_T2: u8 = f12;
        pub const FPREG_T3: u8 = f13;
        pub const FPREG_T4: u8 = f14;
        pub const FPREG_T5: u8 = f15;
        pub const FPREG_T6: u8 = f16;
        pub const FPREG_T7: u8 = f17;
        pub const FPREG_T8: u8 = f0;
        pub const FPREG_T9: u8 = f1;
        pub const FPREG_T10: u8 = f2;
        pub const FPREG_T11: u8 = f3;
        pub const FPREG_T12: u8 = f4;
        pub const FPREG_T13: u8 = f5;
        pub const FPREG_T14: u8 = f6;
        pub const FPREG_T15: u8 = f7;
        pub const FPREG_T16: u8 = f28;
        pub const FPREG_T17: u8 = f29;

        pub const FPREG_CS0: u8 = f8;
        pub const FPREG_CS1: u8 = f9;
        pub const FPREG_CS2: u8 = f18;
        pub const FPREG_CS3: u8 = f19;
        pub const FPREG_CS4: u8 = f20;
        pub const FPREG_CS5: u8 = f21;
        pub const FPREG_CS6: u8 = f22;
        pub const FPREG_CS7: u8 = f23;
        pub const FPREG_CS8: u8 = f24;
        pub const FPREG_CS9: u8 = f25;
        pub const FPREG_CS10: u8 = f26;
        pub const FPREG_CS11: u8 = f27;

        pub const ARGUMENT_FPR0: u8 = f10;
        pub const ARGUMENT_FPR1: u8 = f11;
        pub const ARGUMENT_FPR2: u8 = f12;
        pub const ARGUMENT_FPR3: u8 = f13;
        pub const ARGUMENT_FPR4: u8 = f14;
        pub const ARGUMENT_FPR5: u8 = f15;
        pub const ARGUMENT_FPR6: u8 = f16;
        pub const ARGUMENT_FPR7: u8 = f17;

        pub const RETURN_VALUE_FPR: u8 = f10;
        pub const NON_PRESERVED_NON_ARGUMENT_FPR0: u8 = f11;

        pub const NUMBER_OF_ARGUMENT_REGISTERS: usize = 8;

        pub const fn to_argument_register(i: usize) -> u8 {
            match i {
                0 => ARGUMENT_FPR0,
                1 => ARGUMENT_FPR1,
                2 => ARGUMENT_FPR2,
                3 => ARGUMENT_FPR3,
                4 => ARGUMENT_FPR4,
                5 => ARGUMENT_FPR5,
                6 => ARGUMENT_FPR6,
                7 => ARGUMENT_FPR7,
                _ => panic!("Invalid argument register index"),
            }
        }
    }
}