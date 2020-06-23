#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Debug, Hash)]
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
}

pub struct X86Asm {}

use super::assembler_buffer::*;

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Debug, Hash)]
#[repr(u8)]
pub enum ModRmMode {
    NoDisp,
    Disp0,
    Disp8,
    Disp32,
    Register,
}

pub struct X86AsmFormatter {
    buffer: AsmBuffer,
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
        OP_ADD_EvGv                     = 0x01,
        OP_ADD_GvEv                     = 0x03,
        OP_OR_EvGv                      = 0x09,
        OP_OR_GvEv                      = 0x0B,
        OP_2BYTE_ESCAPE                 = 0x0F,
        OP_AND_EvGv                     = 0x21,
        OP_AND_GvEv                     = 0x23,
        OP_SUB_EvGv                     = 0x29,
        OP_SUB_GvEv                     = 0x2B,
        PRE_PREDICT_BRANCH_NOT_TAKEN    = 0x2E,
        OP_XOR_EvGv                     = 0x31,
        OP_XOR_GvEv                     = 0x33,
        OP_CMP_EvGv                     = 0x39,
        OP_CMP_GvEv                     = 0x3B,
        PRE_REX                         = 0x40,
        OP_PUSH_EAX                     = 0x50,
        OP_POP_EAX                      = 0x58,
        OP_MOVSXD_GvEv                  = 0x63,
        PRE_OPERAND_SIZE                = 0x66,
        PRE_SSE_66                      = 0x66,
        OP_PUSH_Iz                      = 0x68,
        OP_IMUL_GvEvIz                  = 0x69,
        OP_GROUP1_EbIb                  = 0x80,
        OP_GROUP1_EvIz                  = 0x81,
        OP_GROUP1_EvIb                  = 0x83,
        OP_TEST_EbGb                    = 0x84,
        OP_TEST_EvGv                    = 0x85,
        OP_XCHG_EvGv                    = 0x87,
        OP_MOV_EbGb                     = 0x88,
        OP_MOV_EvGv                     = 0x89,
        OP_MOV_GvEv                     = 0x8B,
        OP_LEA                          = 0x8D,
        OP_GROUP1A_Ev                   = 0x8F,
        OP_NOP                          = 0x90,
        OP_CDQ                          = 0x99,
        OP_MOV_EAXOv                    = 0xA1,
        OP_MOV_OvEAX                    = 0xA3,
        OP_MOV_EAXIv                    = 0xB8,
        OP_GROUP2_EvIb                  = 0xC1,
        OP_RET                          = 0xC3,
        OP_GROUP11_EvIb                 = 0xC6,
        OP_GROUP11_EvIz                 = 0xC7,
        OP_INT3                         = 0xCC,
        OP_GROUP2_Ev1                   = 0xD1,
        OP_GROUP2_EvCL                  = 0xD3,
        OP_ESCAPE_DD                    = 0xDD,
        OP_CALL_rel32                   = 0xE8,
        OP_JMP_rel32                    = 0xE9,
        PRE_SSE_F2                      = 0xF2,
        PRE_SSE_F3                      = 0xF3,
        OP_HLT                          = 0xF4,
        OP_GROUP3_EbIb                  = 0xF6,
        OP_GROUP3_Ev                    = 0xF7,
        OP_GROUP3_EvIz                  = 0xF7, // OP_GROUP3_Ev has an immediate, when instruction is a test. 
        OP_GROUP5_Ev                    = 0xFF
    );
}
impl X86AsmFormatter {}
