
use macroassembler::assembler::macro_assembler_arm64::RelationalCondition;
use macroassembler::assembler::{macro_assembler_arm64::MacroAssemblerARM64, link_buffer::LinkBuffer};
use macroassembler::jit::gpr_info::*;

fn iter_fac(x: i32) -> i32 {
    let mut result = 1;
    for i in 1..=x {
        result *= i;
    }
    result
}

fn main() {
    let mut masm = MacroAssemblerARM64::new();

    // iterative factorial
    let result = T1;
    let i = T2;
    let x = ARGUMENT_GPR0;

    masm.mov(1i32, result);
    masm.mov(1i32, i);
    
    let loop_start = masm.label();
    let br = masm.branch32(RelationalCondition::GreaterThan, i, x);
    masm.mul32(i, result);  
    masm.add32(1i32, i);
    masm.jump().link_to(&mut masm, loop_start);

    br.link(&mut masm);
    masm.mov(result, RETURN_VALUE_GPR);
    masm.ret();

    let mut link = LinkBuffer::from_macro_assembler(&mut masm).unwrap();

    let mut out = String::new();
    let code = link.finalize_with_disassembly(true, "", &mut out).unwrap();

    println!("Code: {}", out);

    let f: extern "C" fn(i32) -> i32 = unsafe { std::mem::transmute(code.start()) };

    println!("{}", f(5));
}