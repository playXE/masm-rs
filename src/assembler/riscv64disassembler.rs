use std::fmt::Write;

use super::riscv64assembler::*;

pub fn gp_register_name(value: u8) -> &'static str {
    if value < 32 {
        match value {
            0 => "x0",
            1 => "x1",
            2 => "x2",
            3 => "x3",
            4 => "x4",
            5 => "x5",
            6 => "x6",
            7 => "x7",
            8 => "x8",
            9 => "x9",
            10 => "x10",
            11 => "x11",
            12 => "x12",
            13 => "x13",
            14 => "x14",
            15 => "x15",
            16 => "x16",
            17 => "x17",
            18 => "x18",
            19 => "x19",
            20 => "x20",
            21 => "x21",
            22 => "x22",
            23 => "x23",
            24 => "x24",
            25 => "x25",
            26 => "x26",
            27 => "x27",
            28 => "x28",
            29 => "x29",
            30 => "x30",
            31 => "x31",
            _ => unreachable!(),
        }
    } else {
        "<unknown>"
    }
}

pub fn fp_register_name(value: u8) -> &'static str {
    if value < 32 {
        match value {
            0 => "f0",
            1 => "f1",
            2 => "f2",
            3 => "f3",
            4 => "f4",
            5 => "f5",
            6 => "f6",
            7 => "f7",
            8 => "f8",
            9 => "f9",
            10 => "f10",
            11 => "f11",
            12 => "f12",
            13 => "f13",
            14 => "f14",
            15 => "f15",
            16 => "f16",
            17 => "f17",
            18 => "f18",
            19 => "f19",
            20 => "f20",
            21 => "f21",
            22 => "f22",
            23 => "f23",
            24 => "f24",
            25 => "f25",
            26 => "f26",
            27 => "f27",
            28 => "f28",
            29 => "f29",
            30 => "f30",
            31 => "f31",
            _ => unreachable!(),
        }
    } else {
        "<unknown>"
    }
}

pub fn rounding_mode(value: u8) -> &'static str {
    if value == FPRoundingMode::RNE as u8 {
        "rne"
    } else if value == FPRoundingMode::RTZ as u8 {
        "rtz"
    } else if value == FPRoundingMode::RDN as u8 {
        "rdn"
    } else if value == FPRoundingMode::RUP as u8 {
        "rup"
    } else if value == FPRoundingMode::RMM as u8 {
        "rmm"
    } else {
        "<unknown>"
    }
}

pub fn memory_operation_flags(value: u8) -> String {
    if value != 0 {
        format!(
            "{}{}{}{}",
            if (value & MemoryOperation::I as u8) != 0 {
                "i"
            } else {
                ""
            },
            if (value & MemoryOperation::R as u8) != 0 {
                "r"
            } else {
                ""
            },
            if (value & MemoryOperation::W as u8) != 0 {
                "w"
            } else {
                ""
            },
            if (value & MemoryOperation::O as u8) != 0 {
                "o"
            } else {
                ""
            },
        )
    } else {
        "<none>".to_string()
    }
}

pub fn aqrl_flags(value: u8) -> String {
    match value {
        0 => "".to_string(),
        x if x == MemoryAccess::Acquire as u8 => ".aq".to_string(),

        x if x == MemoryAccess::Release as u8 => ".rl".to_string(),

        x if x == MemoryAccess::AcquireRelease as u8 => ".aqrl".to_string(),

        _ => "<unknown>".to_string(),
    }
}

pub struct RTypeDefaultFormatting;

impl RTypeDefaultFormatting {
    pub fn disassemble(insn: InstructionValue) -> String {
        macro_rules! disasm {
            ($($name: ident), *) => {
                match insn.opcode() {
                    $(_ if $name::matches(insn) => {
                        paste::paste! {
                            format!("{} {}, {}, {}", [<PRETTY_ $name>], gp_register_name($name::rd(insn)), gp_register_name($name::rs1(insn)), gp_register_name($name::rs2(insn)))
                        }
                    })*

                    _ => "<invalid>".to_string()
                }
            };
        }

        disasm! {
            ADD, SUB,
            SLT, SLTU,
            SLL, SRL, SRA,
            XOR, OR, AND,
            ADDW, SUBW,
            SLLW, SRLW, SRAW,
            FSGNJ_S, FSGNJ_D,
            FSGNJN_S, FSGNJN_D,
            FSGNJX_S, FSGNJX_D,
            FMIN_S, FMIN_D,
            FMAX_S, FMAX_D,
            FEQ_S, FEQ_D,
            FLT_S, FLT_D,
            FLE_S, FLE_D,
            MUL, MULH, MULHSU, MULHU,
            DIV, DIVU, REM, REMU,
            MULW, DIVW, DIVUW, REMW, REMUW
        }
    }

    pub fn matches(insn: InstructionValue) -> bool {
        macro_rules! matches {
            ($($name: ident), *) => {
                match insn.opcode() {
                    $(_ if $name::matches(insn) => true,)*
                    _ => false
                }
            };
        }

        matches! {
            ADD, SUB,
            SLT, SLTU,
            SLL, SRL, SRA,
            XOR, OR, AND,
            ADDW, SUBW,
            SLLW, SRLW, SRAW,
            FSGNJ_S, FSGNJ_D,
            FSGNJN_S, FSGNJN_D,
            FSGNJX_S, FSGNJX_D,
            FMIN_S, FMIN_D,
            FMAX_S, FMAX_D,
            FEQ_S, FEQ_D,
            FLT_S, FLT_D,
            FLE_S, FLE_D,
            MUL, MULH, MULHSU, MULHU,
            DIV, DIVU, REM, REMU,
            MULW, DIVW, DIVUW, REMW, REMUW
        }
    }
}

pub struct RTypeR2Formatting;

impl RTypeR2Formatting {
    pub fn disassemble(insn: InstructionValue) -> String {
        if FMV_X_W::matches(insn) {
            format!(
                "fmv.x.w {}, {}",
                gp_register_name(FMV_X_W::rd(insn)),
                fp_register_name(FMV_X_W::rs1(insn))
            )
        } else if FMV_W_X::matches(insn) {
            format!(
                "fmv.w.x {}, {}",
                fp_register_name(FMV_W_X::rd(insn)),
                gp_register_name(FMV_W_X::rs1(insn))
            )
        } else if FCLASS_S::matches(insn) {
            format!(
                "fclass.s {}, {}",
                gp_register_name(FCLASS_S::rd(insn)),
                fp_register_name(FCLASS_S::rs1(insn))
            )
        } else if FCLASS_D::matches(insn) {
            format!(
                "fclass.d {}, {}",
                gp_register_name(FCLASS_D::rd(insn)),
                fp_register_name(FCLASS_D::rs1(insn))
            )
        } else if FMV_X_D::matches(insn) {
            format!(
                "fmv.x.d {}, {}",
                gp_register_name(FMV_X_D::rd(insn)),
                fp_register_name(FMV_X_D::rs1(insn))
            )
        } else if FMV_D_X::matches(insn) {
            format!(
                "fmv.d.x {}, {}",
                fp_register_name(FMV_D_X::rd(insn)),
                gp_register_name(FMV_D_X::rs1(insn))
            )
        } else {
            "<invalid>".to_string()
        }
    }

    pub fn matches(insn: InstructionValue) -> bool {
        FMV_X_W::matches(insn)
            || FMV_W_X::matches(insn)
            || FCLASS_S::matches(insn)
            || FCLASS_D::matches(insn)
            || FMV_X_D::matches(insn)
            || FMV_D_X::matches(insn)
    }
}

pub struct RTypeWithRoundingModeDefaultFormatting;

impl RTypeWithRoundingModeDefaultFormatting {
    pub fn disassemble(insn: InstructionValue) -> String {
        macro_rules! disasm {
            ($($name: ident),*) => {
                match insn.opcode() {
                    $(_ if $name::matches(insn) => {
                        let rm = $name::rm(insn);
                        paste::paste! {
                            format!("{} {}, {}, {}{}{}", [<PRETTY_ $name>], fp_register_name($name::rd(insn)), fp_register_name($name::rs1(insn)), fp_register_name($name::rs2(insn)),

                            if rm == FPRoundingMode::DYN as u8 { "" } else { ", rm:" },
                            if rm == FPRoundingMode::DYN as u8 { "" } else { rounding_mode(rm) }

                        )
                        }
                    })*

                    _ => "<invalid>".to_string()
                }
            };
        }

        disasm!(FADD_S, FADD_D, FSUB_S, FSUB_D, FMUL_S, FMUL_D, FDIV_S, FDIV_D)
    }

    pub fn matches(insn: InstructionValue) -> bool {
        macro_rules! matches {
            ($($name: ident), *) => {
                match insn.opcode() {
                    $(_ if $name::matches(insn) => true,)*
                    _ => false
                }
            };
        }

        matches!(FADD_S, FADD_D, FSUB_S, FSUB_D, FMUL_S, FMUL_D, FDIV_S, FDIV_D)
    }
}

pub struct RTypeWithRoundingModeFSQRTFormatting;

impl RTypeWithRoundingModeFSQRTFormatting {
    pub fn disassemble(insn: InstructionValue) -> String {
        macro_rules! disasm {
            ($($name: ident),*) => {
                match insn.opcode() {
                    $(_ if $name::matches(insn) => {
                        let rm = $name::rm(insn);
                        paste::paste! {
                            format!("{} {}, {}{}{}", [<PRETTY_ $name>], fp_register_name($name::rd(insn)), fp_register_name($name::rs1(insn)),

                            if rm == FPRoundingMode::DYN as u8 { "" } else { ", rm:" },
                            if rm == FPRoundingMode::DYN as u8 { "" } else { rounding_mode(rm) }

                        )
                        }
                    })*

                    _ => "<invalid>".to_string()
                }
            };
        }

        disasm!(FSQRT_S, FSQRT_D)
    }

    pub fn matches(insn: InstructionValue) -> bool {
        macro_rules! matches {
            ($($name: ident), *) => {
                match insn.opcode() {
                    $(_ if $name::matches(insn) => true,)*
                    _ => false
                }
            };
        }

        matches!(FSQRT_S, FSQRT_D)
    }
}

pub struct RTypeWithRoundingModeFCVTFormatting;

impl RTypeWithRoundingModeFCVTFormatting {
    pub fn disassemble(insn: InstructionValue) -> String {
        macro_rules! disasm {
            (
                f2i: ($($f2i: ident),*),
                f2f: ($($f2f: ident),*),
                i2f: ($($i2f: ident),*)
            ) => {
                match insn.opcode() {
                    $(_ if $f2i::matches(insn) => {
                        let rm = $f2i::rm(insn);
                        paste::paste! {
                            format!("{} {}, {}{}{}", [<PRETTY_ $f2i>], gp_register_name($f2i::rd(insn)), fp_register_name($f2i::rs1(insn)),

                            if rm == FPRoundingMode::DYN as u8 { "" } else { ", rm:" },
                            if rm == FPRoundingMode::DYN as u8 { "" } else { rounding_mode(rm) }

                        )
                        }
                    })*

                    $(_ if $f2f::matches(insn) => {
                        let rm = $f2f::rm(insn);
                        paste::paste! {
                            format!("{} {}, {}{}{}", [<PRETTY_ $f2f>], fp_register_name($f2f::rd(insn)), fp_register_name($f2f::rs1(insn)),

                            if rm == FPRoundingMode::DYN as u8 { "" } else { ", rm:" },
                            if rm == FPRoundingMode::DYN as u8 { "" } else { rounding_mode(rm) }

                        )
                        }
                    })*

                    $(_ if $i2f::matches(insn) => {
                        let rm = $i2f::rm(insn);
                        paste::paste! {
                            format!("{} {}, {}{}{}", [<PRETTY_ $i2f>], fp_register_name($i2f::rd(insn)), gp_register_name($i2f::rs1(insn)),

                            if rm == FPRoundingMode::DYN as u8 { "" } else { ", rm:" },
                            if rm == FPRoundingMode::DYN as u8 { "" } else { rounding_mode(rm) }

                        )
                        }
                    })*

                    _ => "<invalid>".to_string()
                }
            };
        }

        disasm! {
            f2i: (FCVT_L_S, FCVT_LU_S, FCVT_W_S, FCVT_WU_S, FCVT_L_D, FCVT_LU_D, FCVT_W_D, FCVT_WU_D),
            f2f: (FCVT_S_D, FCVT_D_S),
            i2f: (FCVT_W_S, FCVT_WU_S, FCVT_D_S, FCVT_D_WU)
        }
    }

    pub fn matches(insn: InstructionValue) -> bool {
        macro_rules! matches {
            (
                f2i: ($($f2i: ident),*),
                f2f: ($($f2f: ident),*),
                i2f: ($($i2f: ident),*)
            ) => {
                match insn.opcode() {
                    $(_ if $f2i::matches(insn) => true,)*
                    $(_ if $f2f::matches(insn) => true,)*
                    $(_ if $i2f::matches(insn) => true,)*
                    _ => false
                }
            };
        }

        matches! {
            f2i: (FCVT_L_S, FCVT_LU_S, FCVT_W_S, FCVT_WU_S, FCVT_L_D, FCVT_LU_D, FCVT_W_D, FCVT_WU_D),
            f2f: (FCVT_S_D, FCVT_D_S),
            i2f: (FCVT_W_S, FCVT_WU_S, FCVT_D_S, FCVT_D_WU)
        }
    }
}

pub struct RTypeWithAqRlDefaultFormatting;

impl RTypeWithAqRlDefaultFormatting {
    pub fn disassemble(insn: InstructionValue) -> String {
        macro_rules! disasm {
            ($($name: ident),*) => {
                match insn.opcode() {
                    $(_ if $name::matches(insn) => {
                        paste::paste! {
                            format!("{}{} {}, {}, {}", [<PRETTY_ $name>], aqrl_flags($name::aqrl(insn)), gp_register_name($name::rd(insn)), gp_register_name($name::rs1(insn)), gp_register_name($name::rs2(insn)))
                        }
                    })*

                    _ => "<invalid>".to_string()
                }
            };
        }

        disasm!(
            SC_W, SC_D, AMOSWAP_W, AMOSWAP_D, AMOADD_W, AMOADD_D, AMOXOR_W, AMOXOR_D, AMOAND_W,
            AMOAND_D, AMOOR_W, AMOOR_D, AMOMIN_W, AMOMIN_D, AMOMAX_W, AMOMAX_D, AMOMINU_W,
            AMOMINU_D, AMOMAXU_W, AMOMAXU_D
        )
    }

    pub fn matches(insn: InstructionValue) -> bool {
        macro_rules! matches {
            ($($name: ident),*) => {
                match insn.opcode() {
                    $(_ if $name::matches(insn) => true,)*
                    _ => false
                }
            };
        }

        matches!(
            SC_W, SC_D, AMOSWAP_W, AMOSWAP_D, AMOADD_W, AMOADD_D, AMOXOR_W, AMOXOR_D, AMOAND_W,
            AMOAND_D, AMOOR_W, AMOOR_D, AMOMIN_W, AMOMIN_D, AMOMAX_W, AMOMAX_D, AMOMINU_W,
            AMOMINU_D, AMOMAXU_W, AMOMAXU_D
        )
    }
}

pub struct RTypeWithAqRlLRFormatting;

impl RTypeWithAqRlLRFormatting {
    pub fn disassemble(insn: InstructionValue) -> String {
        macro_rules! disasm {
            ($($name: ident),*) => {
                match insn.opcode() {
                    $(_ if $name::matches(insn) => {
                        paste::paste! {
                            format!("{}{} {}, {}", [<PRETTY_ $name>], aqrl_flags($name::aqrl(insn)), gp_register_name($name::rd(insn)), gp_register_name($name::rs1(insn)))
                        }
                    })*

                    _ => "<invalid>".to_string()
                }
            };
        }

        disasm!(LR_W, LR_D)
    }

    pub fn matches(insn: InstructionValue) -> bool {
        macro_rules! matches {
            ($($name: ident),*) => {
                match insn.opcode() {
                    $(_ if $name::matches(insn) => true,)*
                    _ => false
                }
            };
        }

        matches!(LR_W, LR_D)
    }
}

pub struct R4TypeWithRoundingModeDefaultFormatting;

impl R4TypeWithRoundingModeDefaultFormatting {
    pub fn disassemble(insn: InstructionValue) -> String {
        macro_rules! disasm {
            ($($name: ident),*) => {
                match insn.opcode() {
                    $(_ if $name::matches(insn) => {
                        let rm = $name::rm(insn);
                        paste::paste! {
                            format!("{} {}, {}, {}, {}{}{}", [<PRETTY_ $name>], fp_register_name($name::rd(insn)), fp_register_name($name::rs1(insn)), fp_register_name($name::rs2(insn)), fp_register_name($name::rs3(insn)),

                            if rm == FPRoundingMode::DYN as u8 { "" } else { ", rm:" },
                            if rm == FPRoundingMode::DYN as u8 { "" } else { rounding_mode(rm) }

                        )
                        }
                    })*

                    _ => "<invalid>".to_string()
                }
            };
        }

        disasm!(FMADD_S, FMADD_D, FMSUB_S, FMSUB_D, FNMSUB_S, FNMSUB_D, FNMADD_S, FNMADD_D)
    }

    pub fn matches(insn: InstructionValue) -> bool {
        macro_rules! matches {
            ($($name: ident),*) => {
                match insn.opcode() {
                    $(_ if $name::matches(insn) => true,)*
                    _ => false
                }
            };
        }

        matches!(FMADD_S, FMADD_D, FMSUB_S, FMSUB_D, FNMSUB_S, FNMSUB_D, FNMADD_S, FNMADD_D)
    }
}

pub struct ITypeDefaultFormatting;

impl ITypeDefaultFormatting {
    pub fn disassemble(insn: InstructionValue) -> String {
        macro_rules! disasm {
            ($($name: ident),*) => {
                match insn.opcode() {
                    $(_ if $name::matches(insn) => {
                        paste::paste! {
                            format!("{} {}, {}, {}", [<PRETTY_ $name>], gp_register_name($name::rd(insn)), gp_register_name($name::rs1(insn)), IImmediate::value(insn))
                        }
                    })*

                    _ => "<invalid>".to_string()
                }
            };
        }

        disasm!(ADDI, SLTI, SLTIU, XORI, ORI, ANDI, SLLI, SRLI, SRAI, ADDIW, SLLIW, SRLIW, SRAIW)
    }

    pub fn matches(insn: InstructionValue) -> bool {
        macro_rules! matches {
            ($($name: ident),*) => {
                match insn.opcode() {
                    $(_ if $name::matches(insn) => true,)*
                    _ => false
                }
            };
        }

        matches!(ADDI, SLTI, SLTIU, XORI, ORI, ANDI, SLLI, SRLI, SRAI, ADDIW, SLLIW, SRLIW, SRAIW)
    }
}

pub struct ITypeImmediateAsOffsetFormatting;

impl ITypeImmediateAsOffsetFormatting {
    pub fn disassemble(insn: InstructionValue) -> String {
        macro_rules! disasm {
            ($($name: ident),*) => {
                match insn.opcode() {
                    $(_ if $name::matches(insn) => {
                        paste::paste! {
                            format!("{} {}, {}({})", [<PRETTY_ $name>], gp_register_name($name::rd(insn)), IImmediate::value(insn), gp_register_name($name::rs1(insn)))
                        }
                    })*

                    _ => "<invalid>".to_string()
                }
            };
        }

        disasm!(JALR, LB, LBU, LH, LHU, LW, LWU, LD, FLW, FLD)
    }

    pub fn matches(insn: InstructionValue) -> bool {
        macro_rules! matches {
            ($($name: ident),*) => {
                match insn.opcode() {
                    $(_ if $name::matches(insn) => true,)*
                    _ => false
                }
            };
        }

        matches!(JALR, LB, LBU, LH, LHU, LW, LWU, LD, FLW, FLD)
    }
}

pub struct STypeDefaultFormatting;

impl STypeDefaultFormatting {
    pub fn disassemble(insn: InstructionValue) -> String {
        macro_rules! disasm {
            ($($name: ident),*) => {
                match insn.opcode() {
                    $(_ if $name::matches(insn) => {
                        paste::paste! {
                            format!("{} {}, {}({})", [<PRETTY_ $name>], gp_register_name($name::rs2(insn)), SImmediate::value(insn), gp_register_name($name::rs1(insn)))
                        }
                    })*

                    _ => "<invalid>".to_string()
                }
            };
        }

        disasm!(SB, SH, SW, SD, FSW, FSD)
    }

    pub fn matches(insn: InstructionValue) -> bool {
        macro_rules! matches {
            ($($name: ident),*) => {
                match insn.opcode() {
                    $(_ if $name::matches(insn) => true,)*
                    _ => false
                }
            };
        }

        matches!(SB, SH, SW, SD, FSW, FSD)
    }
}

pub struct BTypeDefaultFormatting;

impl BTypeDefaultFormatting {
    pub fn disassemble(insn: InstructionValue) -> String {
        macro_rules! disasm {
            ($($name: ident),*) => {
                match insn.opcode() {
                    $(_ if $name::matches(insn) => {
                        paste::paste! {
                            format!("{} {}, {}, {}", [<PRETTY_ $name>], gp_register_name($name::rs1(insn)), gp_register_name($name::rs2(insn)), BImmediate::value(insn))
                        }
                    })*

                    _ => "<invalid>".to_string()
                }
            };
        }

        disasm!(BEQ, BNE, BLT, BGE, BLTU, BGEU)
    }

    pub fn matches(insn: InstructionValue) -> bool {
        macro_rules! matches {
            ($($name: ident),*) => {
                match insn.opcode() {
                    $(_ if $name::matches(insn) => true,)*
                    _ => false
                }
            };
        }

        matches!(BEQ, BNE, BLT, BGE, BLTU, BGEU)
    }
}

pub struct UTypeDefaultFormatting;

impl UTypeDefaultFormatting {
    pub fn disassemble(insn: InstructionValue) -> String {
        macro_rules! disasm {
            ($($name: ident),*) => {
                match insn.opcode() {
                    $(_ if $name::matches(insn) => {
                        paste::paste! {
                            format!("{} {}, {}", [<PRETTY_ $name>], gp_register_name($name::rd(insn)), UImmediate::value(insn))
                        }
                    })*

                    _ => "<invalid>".to_string()
                }
            };
        }

        disasm!(LUI, AUIPC)
    }

    pub fn matches(insn: InstructionValue) -> bool {
        macro_rules! matches {
            ($($name: ident),*) => {
                match insn.opcode() {
                    $(_ if $name::matches(insn) => true,)*
                    _ => false
                }
            };
        }

        matches!(LUI, AUIPC)
    }
}

pub struct JTypeDefaultFormatting;

impl JTypeDefaultFormatting {
    pub fn disassemble(insn: InstructionValue) -> String {
        macro_rules! disasm {
            ($($name: ident),*) => {
                match insn.opcode() {
                    $(_ if $name::matches(insn) => {
                        paste::paste! {
                            format!("{} {}, {}", [<PRETTY_ $name>], gp_register_name($name::rd(insn)), JImmediate::value(insn))
                        }
                    })*

                    _ => "<invalid>".to_string()
                }
            };
        }

        disasm!(JAL)
    }

    pub fn matches(insn: InstructionValue) -> bool {
        macro_rules! matches {
            ($($name: ident),*) => {
                match insn.opcode() {
                    $(_ if $name::matches(insn) => true,)*
                    _ => false
                }
            };
        }

        matches!(JAL)
    }
}

pub struct FenceInstructionFormatting;

impl FenceInstructionFormatting {
    pub fn disassemble(insn: InstructionValue) -> String {
        let imm_value = IImmediate::value(insn);
        let pred_flags = memory_operation_flags(((imm_value >> 4) & ((1 << 4) - 1)) as _);
        let succ_flags = memory_operation_flags((imm_value & ((1 << 4) - 1)) as _);

        format!("fence {}, {}", pred_flags, succ_flags)
    }

    pub fn matches(insn: InstructionValue) -> bool {
        FENCE::matches(insn)
    }
}

pub struct FenceIInstructionFormatting;

impl FenceIInstructionFormatting {
    pub fn disassemble(_insn: InstructionValue) -> String {
        "fence.i".to_string()
    }

    pub fn matches(insn: InstructionValue) -> bool {
        FENCE_I::matches(insn)
    }
}

pub struct EnvironmentInstructionFormatting;

impl EnvironmentInstructionFormatting {
    pub fn disassemble(insn: InstructionValue) -> String {
        if ECALL::matches(insn) {
            "ecall".to_string()
        } else if EBREAK::matches(insn) {
            "ebreak".to_string()
        } else {
            "<invalid>".to_string()
        }
    }

    pub fn matches(insn: InstructionValue) -> bool {
        ECALL::matches(insn) || EBREAK::matches(insn)
    }
}

pub fn disassemble_opcode(insn: InstructionValue) -> String {
    macro_rules! disasm {
        ($($name: ident),*) => {
            match insn.opcode() {
                $(_ if $name::matches(insn) => {
                    paste::paste! {
                        $name::disassemble(insn)
                    }
                })*

                _ => "<invalid>".to_string()
            }
        };
    }

    disasm!(
        RTypeDefaultFormatting,
        RTypeR2Formatting,
        RTypeWithRoundingModeDefaultFormatting,
        RTypeWithRoundingModeFSQRTFormatting,
        RTypeWithRoundingModeFCVTFormatting,
        RTypeWithAqRlDefaultFormatting,
        RTypeWithAqRlLRFormatting,
        R4TypeWithRoundingModeDefaultFormatting,
        ITypeDefaultFormatting,
        ITypeImmediateAsOffsetFormatting,
        STypeDefaultFormatting,
        BTypeDefaultFormatting,
        UTypeDefaultFormatting,
        JTypeDefaultFormatting,
        FenceInstructionFormatting,
        FenceIInstructionFormatting,
        EnvironmentInstructionFormatting
    )
}

pub fn try_to_disassemble(
    code: &[u8],
    prefix: &str,
    ip: u64,
    out: &mut dyn Write,
) -> Result<(), std::fmt::Error> {
    let mut i = 0;
    while i < code.len() {
        let insn = InstructionValue::new(u32::from_le_bytes([
            code[i],
            code[i + 1],
            code[i + 2],
            code[i + 3],
        ]));
        let opcode = disassemble_opcode(insn);
        writeln!(
            out,
            "{}{:016x}: <{:08x}> {}",
            prefix,
            ip + i as u64,
            insn.value,
            opcode
        )?;
        i += 4;
    }

    Ok(())
}
