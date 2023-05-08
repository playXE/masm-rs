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

   
    
    asm.convert_int32_to_double(ARGUMENT_GPR0, FPREG_T0);
    asm.convert_int32_to_double(ARGUMENT_GPR1, FPREG_T1);
    asm.div_double(FPREG_T0, FPREG_T1, FPREG_T0);
    asm.ret();
    
    
    let mut lb = LinkBuffer::from_macro_assembler(&mut asm);

    let mut out = String::new();

    let code = lb.finalize_with_disassembly(true, "", &mut out).unwrap();

    println!("{}", out);

    let func: extern "C" fn(i32, i32) -> f64 = unsafe { std::mem::transmute(code.start()) };

    println!("{}", func(2, 4));
}