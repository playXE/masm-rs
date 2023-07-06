#[macro_export]
macro_rules! for_each_gp_register {
    ($m: path) => {
        #[cfg(not(target_vendor = "apple"))]
        $m! {
            (x0,  "x0",  0, 0),
            (x1,  "x1",  0, 0),
            (x2,  "x2",  0, 0),
            (x3,  "x3",  0, 0),
            (x4,  "x4",  0, 0),
            (x5,  "x5",  0, 0),
            (x6,  "x6",  0, 0),
            (x7,  "x7",  0, 0),
            /* Indirect result location register. */
            (x8,  "x8",  0, 0),
            /* Temporary registers. */
            (x9,  "x9",  0, 0),
            (x10, "x10", 0, 0),
            (x11, "x11", 0, 0),
            (x12, "x12", 0, 0),
            (x13, "x13", 0, 0),
            (x14, "x14", 0, 0),
            (x15, "x15", 0, 0),
            /* Intra-procedure-call scratch registers (temporary). */
            (x16, "x16", 0, 0),
            (x17, "x17", 0, 0),
            /* Platform Register (temporary). */
            (x18, "x18", 1, 0),
            /* Callee-saved. */
            (x19, "x19", 0, 1),
            (x20, "x20", 0, 1),
            (x21, "x21", 0, 1),
            (x22, "x22", 0, 1),
            (x23, "x23", 0, 1),
            (x24, "x24", 0, 1),
            (x25, "x25", 0, 1),
            (x26, "x26", 0, 1),
            (x27, "x27", 0, 1),
            (x28, "x28", 0, 1),
            /* Special. */
            (fp,  "fp",  0, 1),
            (lr,  "lr",  1, 0),
            (sp,  "sp",  0, 0),
        }
        #[cfg(target_vendor = "apple")]
        $m! {
            (x0,  "x0",  0, 0),
            (x1,  "x1",  0, 0),
            (x2,  "x2",  0, 0),
            (x3,  "x3",  0, 0),
            (x4,  "x4",  0, 0),
            (x5,  "x5",  0, 0),
            (x6,  "x6",  0, 0),
            (x7,  "x7",  0, 0),
            /* Indirect result location register. */
            (x8,  "x8",  0, 0),
            /* Temporary registers. */
            (x9,  "x9",  0, 0),
            (x10, "x10", 0, 0),
            (x11, "x11", 0, 0),
            (x12, "x12", 0, 0),
            (x13, "x13", 0, 0),
            (x14, "x14", 0, 0),
            (x15, "x15", 0, 0),
            /* Intra-procedure-call scratch registers (temporary). */
            (x16, "x16", 0, 0),
            (x17, "x17", 0, 0),
            /* Platform Register (temporary). */
            (x18, "x18", 0, 0),
            /* Callee-saved. */
            (x19, "x19", 0, 1),
            (x20, "x20", 0, 1),
            (x21, "x21", 0, 1),
            (x22, "x22", 0, 1),
            (x23, "x23", 0, 1),
            (x24, "x24", 0, 1),
            (x25, "x25", 0, 1),
            (x26, "x26", 0, 1),
            (x27, "x27", 0, 1),
            (x28, "x28", 0, 1),
            /* Special. */
            (fp,  "fp",  0, 1),
            (lr,  "lr",  1, 0),
            (sp,  "sp",  0, 0),
        }
    };
}

#[macro_export]
macro_rules! for_each_register_alias {
    ($m: path) => {
        $m! {
            (ip0, "ip0", x16),
            (ip1, "ip1", x17),
            (x29, "x29", fp),
            (x30, "x30", lr),
            (zr,  "zr",  0x3f),
        }
    };
}

#[macro_export]
macro_rules! for_each_sp_register {
    ($m: path) => {
        $m! {
            (pc, "pc"),
            (nzcv, "nzcv"),
            (fpsr, "fpsr"),
        }
    };
}

#[macro_export]
macro_rules! for_each_fp_register {
    ($m: path) => {
        $m! {
            /* Parameter/result registers. */
            (q0,  "q0",  0, 0),
            (q1,  "q1",  0, 0),
            (q2,  "q2",  0, 0),
            (q3,  "q3",  0, 0),
            (q4,  "q4",  0, 0),
            (q5,  "q5",  0, 0),
            (q6,  "q6",  0, 0),
            (q7,  "q7",  0, 0),
            /* Callee-saved (up to 64-bits only!). */
            (q8,  "q8",  0, 1),
            (q9,  "q9",  0, 1),
            (q10, "q10", 0, 1),
            (q11, "q11", 0, 1),
            (q12, "q12", 0, 1),
            (q13, "q13", 0, 1),
            (q14, "q14", 0, 1),
            (q15, "q15", 0, 1),
            /* Temporary registers. */
            (q16, "q16", 0, 0),
            (q17, "q17", 0, 0),
            (q18, "q18", 0, 0),
            (q19, "q19", 0, 0),
            (q20, "q20", 0, 0),
            (q21, "q21", 0, 0),
            (q22, "q22", 0, 0),
            (q23, "q23", 0, 0),
            (q24, "q24", 0, 0),
            (q25, "q25", 0, 0),
            (q26, "q26", 0, 0),
            (q27, "q27", 0, 0),
            (q28, "q28", 0, 0),
            (q29, "q29", 0, 0),
            (q30, "q30", 0, 0),
            (q31, "q31", 0, 0),
        }
    };
}
