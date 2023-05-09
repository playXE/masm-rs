use macroassembler::assembler::abstract_macro_assembler::AbsoluteAddress;
use macroassembler::assembler::link_buffer::LinkBuffer;

use macroassembler::jit::{helpers::AssemblyHelpers, gpr_info::*, fpr_info::*};
use macroassembler::assembler::*;

extern "C" fn square(x: i32) -> i32 {
    println!("Square: {}", x);
    x * x
}

fn main() {
    let mut asm = TargetMacroAssembler::new();

    let x = ARGUMENT_GPR0;
    let y = ARGUMENT_GPR1;

   
    
    asm.mov(1i32, T3);
    asm.mov(0i32, T4);
    asm.move_conditionally_double_then_else(DoubleCondition::EqualAndOrdered, ARGUMENT_FPR0, ARGUMENT_FPR1, T3, T4, RETURN_VALUE_GPR);
    asm.ret();
    
    
    let mut lb = LinkBuffer::from_macro_assembler(&mut asm);

    let mut out = String::new();

    let code = lb.finalize_with_disassembly(true, "", &mut out).unwrap();

    println!("{}", out);

    let func: extern "C" fn(f64, f64) -> i32 = unsafe { std::mem::transmute(code.start()) };

    println!("{}", func(2.0, 4.0));
}