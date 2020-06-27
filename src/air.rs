//! ## Assembly intermediate language
//! Simple IR for doing codegeneration easy.

use crate::machine_masm::*;
use std::collections::HashMap;

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum AIRType {
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
}
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum AIROp {
    Load8,
    Load16,
    Load32,
    Load64,
    Store8,
    Store16,
    Store32,
    Store64,

    Jump,
    FarJump,
    Branch32,
    Branch64,
    Compare32,
    Compare64,

    Call,
    TailCall,

    Add32,
    Sub32,
    Div32,
    Mul32,
    Rem32,

    Add64,
    Sub64,
    Div64,
    Mul64,
    Rem64,

    LShift32,
    RShift32,
    URShift32,
    LShift64,
    RShift64,
    URShift64,

    And32,
    And64,
    Or32,
    Or64,
}
#[derive(Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum AIRNode {
    None,
    Op(AIROp, Box<AIRNode>, Box<AIRNode>, Box<AIRNode>),
    Value(u32),
    Imm8(i8),
    Imm16(i16),
    Imm32(i32),
    Imm64(i64),
    Nodes(Box<Vec<AIRNode>>),
    Gpr(RegisterID),
    Fpr(XMMRegisterID),
}
