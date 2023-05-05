//! Calling convention information provider
use cfg_if::cfg_if;


cfg_if! {
    if #[cfg(target_arch="x86_64")]
    {
        use crate::assembler::x86assembler::*;
        pub const NUMBER_OF_REGISTERS: usize = 10;
        cfg_if! {
            if #[cfg(windows)]
            {
                pub const NUMBER_OF_ARGUMENT_REGISTERS: usize = 4;
                pub const NUMBER_OF_CALLEE_SAVED_REGISTERS: usize = 7;
            } else {
                pub const NUMBER_OF_ARGUMENT_REGISTERS: usize = 6;
                pub const NUMBER_OF_CALLEE_SAVED_REGISTERS: usize = 5;
            }
        }

            pub const CALL_FRAME_REGISTER: u8 = ebp;
            pub const NUMBER_TAG_REGISTER: u8 = r14;
            pub const NOT_CELL_MASK_REGISTER: u8 = r15;
            pub const CONSTANTS_REGISTER: u8 = r13;

            pub const T0: u8 = eax;

            cfg_if!{
                if #[cfg(not(windows))]
                {
                    pub const T1: u8 = esi;
                    pub const T2: u8 = edx;
                    pub const T3: u8 = ecx;
                    pub const T4: u8 = r8;
                    pub const T5: u8 = r10;
                    pub const T6: u8 = edi;
                    pub const T7: u8 = r9;
                } else {
                    pub const T1: u8 = edx;
                    pub const T2: u8 = r8;
                    pub const T3: u8 = r9;
                    pub const T4: u8 = r10;
                    pub const T5: u8 = ecx;
                }
            }

            pub const CS0: u8 = ebx;

            cfg_if! {
                if #[cfg(not(windows))] {
                    pub const CS1: u8 = r12;
                    pub const CS2: u8 = r13;
                    pub const CS3: u8 = r14;
                    pub const CS4: u8 = r15;
                } else {
                    pub const CS1: u8 = esi;
                    pub const CS2: u8 = edi;
                    pub const CS3: u8 = r12;
                    pub const CS4: u8 = r13;
                    pub const CS5: u8 = r14;
                    pub const CS6: u8 = r15;
                }
            }

            cfg_if! {
                if #[cfg(not(windows))] {
                    pub const ARGUMENT_GPR0: u8 = edi;
                    pub const ARGUMENT_GPR1: u8 = esi;
                    pub const ARGUMENT_GPR2: u8 = edx;
                    pub const ARGUMENT_GPR3: u8 = ecx;
                    pub const ARGUMENT_GPR4: u8 = r8;
                    pub const ARGUMENT_GPR5: u8 = r9;
                } else {
                    pub const ARGUMENT_GPR0: u8 = ecx;
                    pub const ARGUMENT_GPR1: u8 = edx;
                    pub const ARGUMENT_GPR2: u8 = r8;
                    pub const ARGUMENT_GPR3: u8 = r9;
                }
            }

            pub const NON_ARG_GPR0: u8 = r10;
            pub const NON_ARG_GPR1: u8 = eax;

            pub const RETURN_VALUE_GPR: u8 = eax;
            pub const RETURN_VALUE_GPR2: u8 = edx; // T1 or T2
            pub const NON_PRESERVED_NON_RETURN_GPR: u8 = r10;
            pub const NON_PRESERVED_NON_ARGUMENT_GPR0: u8 = r10;
            pub const NON_PRESERVED_NON_ARGUMENT_GPR1: u8 = eax;
            pub const WASM_SCRATCH_GPR0: u8 = eax;

            cfg_if! {
                if #[cfg(not(windows))] {
                    pub const WASM_SCRATCH_GPR1: u8 = r10;
                }
            }

            pub const PATCHPOINT_SCRATCH_REGISTER: u8 = r11;

            pub const fn to_register(index: usize) -> u8 {
                #[cfg(not(windows))]
                const REGISTER_FOR_INDEX: [u8; NUMBER_OF_REGISTERS] = [
                    T0, T1, T2, T3, T4, T5, T6, T7, CS0, CS1
                ];

                #[cfg(windows)]
                const REGISTER_FOR_INDEX: [u8; NUMBER_OF_REGISTERS] = [
                    T0, T1, T2, T3, T4, T5, CS0, CS1, CS2, CS3
                ];

                REGISTER_FOR_INDEX[index]
            }

            pub const fn to_argument_register(index: usize) -> u8 {
                #[cfg(not(windows))]
                const REGISTER_FOR_INDEX: [u8; NUMBER_OF_ARGUMENT_REGISTERS] = [
                    ARGUMENT_GPR0, ARGUMENT_GPR1, ARGUMENT_GPR2, ARGUMENT_GPR3, ARGUMENT_GPR4, ARGUMENT_GPR5
                ];

                #[cfg(windows)]
                const REGISTER_FOR_INDEX: [u8; NUMBER_OF_ARGUMENT_REGISTERS] = [
                    ARGUMENT_GPR0, ARGUMENT_GPR1, ARGUMENT_GPR2, ARGUMENT_GPR3
                ];

                REGISTER_FOR_INDEX[index]
            }

            pub const fn to_index(reg: u8) -> usize {
                #[cfg(not(windows))]
                const INDEX_FOR_REGISTER: [usize; 16] = [
                    0, 3, 2, 8, usize::MAX, usize::MAX, 1, 6, 4, 7 ,5, usize::MAX, 9, usize::MAX, usize::MAX, usize::MAX
                ];

                #[cfg(windows)]
                const INDEX_FOR_REGISTER: [usize; 16] = [
                    0, 5, 1, 6, usize::MAX, usize::MAX, 7, 8, 2, 3 ,4, usize::MAX, 9, usize::MAX, usize::MAX, usize::MAX
                ];

                INDEX_FOR_REGISTER[reg as usize] as usize
            }

            pub const fn reserved_registers() -> &'static [u8] {
                &[r11, NUMBER_TAG_REGISTER, NOT_CELL_MASK_REGISTER]
            }

    } else if #[cfg(target_arch="riscv64")] {
        use crate::assembler::riscv64assembler::*;
        pub const CALL_FRAME_REGISTER: u8 = fp;
        pub const NUMBER_TAG_REGISTER: u8 = x25;
        pub const NOT_CELL_MASK_REGISTER: u8 = x26;
        pub const CONSTANTS_REGISTER: u8 = x24;

        pub const T0: u8 = x10;
        pub const T1: u8 = x11;
        pub const T2: u8 = x12;
        pub const T3: u8 = x13;
        pub const T4: u8 = x14;
        pub const T5: u8 = x15;
        pub const T6: u8 = x16;
        pub const T7: u8 = x17;
        pub const T8: u8 = x5;
        pub const T9: u8 = x6;
        pub const T10: u8 = x7;
        pub const T11: u8 = x28;
        pub const T12: u8 = x29;

        pub const CS0: u8 = x9;
        pub const CS1: u8 = x18;
        pub const CS2: u8 = x19;
        pub const CS3: u8 = x20;
        pub const CS4: u8 = x21;
        pub const CS5: u8 = x22;
        pub const CS6: u8 = x23;
        pub const CS7: u8 = x24;
        pub const CS8: u8 = x25;
        pub const CS9: u8 = x26;
        pub const CS10: u8 = x27;

        pub const ARGUMENT_GPR0: u8 = x10;
        pub const ARGUMENT_GPR1: u8 = x11;
        pub const ARGUMENT_GPR2: u8 = x12;
        pub const ARGUMENT_GPR3: u8 = x13;
        pub const ARGUMENT_GPR4: u8 = x14;
        pub const ARGUMENT_GPR5: u8 = x15;
        pub const ARGUMENT_GPR6: u8 = x16;
        pub const ARGUMENT_GPR7: u8 = x17;
        
        pub const NON_ARG_GPR0: u8 = x5;
        pub const NON_ARG_GPR1: u8 = x6;

        pub const RETURN_VALUE_GPR: u8 = x10;
        pub const RETURN_VALUE_GPR2: u8 = x11; 

        pub const NON_PRESERVED_NON_RETURN_GPR: u8 = x12;
        pub const NON_PRESERVED_NON_ARGUMENT_GPR0: u8 = x5;
        pub const NON_PRESERVED_NON_ARGUMENT_GPR1: u8 = x6;

        pub const PATCHPOINT_SCRATCH_REGISTER: u8 = x30;
    } else {
        compile_error!("Unsupported architecture")
    }
}
