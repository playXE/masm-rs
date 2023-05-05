use macroassembler::assembler::abstract_macro_assembler::AbsoluteAddress;
use macroassembler::assembler::link_buffer::LinkBuffer;
use macroassembler::jit::{helpers::AssemblyHelpers, gpr_info::*};
use macroassembler::assembler::*;

extern "C" fn square(x: i32) -> i32 {
    println!("Square: {}", x);
    x * x
}

fn main() {
    let mut asm = TargetMacroAssembler::new();


    
    asm.emit_function_prologue();
    asm.call_op(Some(AbsoluteAddress::new(square as *const u8)));
    asm.emit_function_epilogue();
    asm.ret();

    /*let number = ARGUMENT_GPR0;
    let result = ARGUMENT_GPR1;
    let i = ARGUMENT_GPR2;
    asm.mov(1i32, result);
    asm.mov(1i32, i);


    let loop_start = asm.label();
    let br = asm.branch32(RelationalCondition::GreaterThan, i, number);
    asm.mul32(i, result);
    asm.add32(1i32, i);
    asm.jump().link_to(&mut asm, loop_start);
    let end = asm.label();
    br.link_to(&mut asm, end);
    asm.mov(result, RETURN_VALUE_GPR);
    asm.ret();*/
    
    let mut lb = LinkBuffer::from_macro_assembler(&mut asm);

    let mut out = String::new();

    let code = lb.finalize_with_disassembly(true, "", &mut out).unwrap();

    println!("{}", out);

    let func: extern "C" fn(i32) -> i32 = unsafe { std::mem::transmute(code.start()) };

    println!("{}", func(5));
}