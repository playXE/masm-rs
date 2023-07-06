#[macro_export]
macro_rules! for_each_gp_register {
    ($m: path) => {
        $m! {
            (eax, "rax", 0, 0),
            (ecx, "rcx", 0, 0),
            (edx, "rdx", 0, 0),
            (ebx, "rbx", 0, 1),
            (esp, "rsp", 0, 0),
            (ebp, "rbp", 0, 1),
            (esi, "rsi", 0, 0),
            (edi, "rdi", 0, 0),
            (r8,  "r8",  0, 0),
            (r9,  "r9",  0, 0),
            (r10, "r10", 0, 0),
            (r11, "r11", 0, 0),
            (r12, "r12", 0, 1),
            (r13, "r13", 0, 1),
            (r14, "r14", 0, 1),
            (r15, "r15", 0, 1),
        }
    };
}

#[macro_export]
macro_rules! for_each_fp_register {
    ($m: path) => {
        $m! {
            (xmm0, "xmm0", 0, 0),
            (xmm1, "xmm1", 0, 0),
            (xmm2, "xmm2", 0, 0),
            (xmm3, "xmm3", 0, 0),
            (xmm4, "xmm4", 0, 0),
            (xmm5, "xmm5", 0, 0),
            (xmm6, "xmm6", 0, 0),
            (xmm7, "xmm7", 0, 0),
            (xmm8, "xmm8", 0, 0),
            (xmm9, "xmm9", 0, 0),
            (xmm10, "xmm10", 0, 0),
            (xmm11, "xmm11", 0, 0),
            (xmm12, "xmm12", 0, 0),
            (xmm13, "xmm13", 0, 0),
            (xmm14, "xmm14", 0, 0),
            (xmm15, "xmm15", 0, 0),
        }
    };
}

#[macro_export]
macro_rules! for_each_sp_register {
    ($m: path) => {
        $m! {
            (eip, "eip", 0, 0),
            (eflags, "eflags", 0, 0),
        }
    };
}
