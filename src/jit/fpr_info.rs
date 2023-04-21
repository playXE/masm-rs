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

    }
}