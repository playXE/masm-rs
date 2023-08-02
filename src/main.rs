
use macroassembler::assembler::TargetMacroAssembler;
use macroassembler::assembler::abstract_macro_assembler::AbsoluteAddress;
use macroassembler::assembler::arm64assembler::zr;
use macroassembler::assembler::macro_assembler_arm64::RelationalCondition;
use macroassembler::assembler::{macro_assembler_arm64::MacroAssemblerARM64, link_buffer::LinkBuffer};
use macroassembler::jit::gpr_info::*;
use macroassembler::jit::helpers::AssemblyHelpers;

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
    masm.emit_function_prologue();
    masm.mov(5i32, ARGUMENT_GPR0);
    masm.call_op(Some(AbsoluteAddress::new(iter_fac as _)));
    masm.emit_function_epilogue();
    masm.ret();

    let mut link = LinkBuffer::from_macro_assembler(&mut masm).unwrap();

    let mut out = String::new();
    let code = link.finalize_with_disassembly(true, "", &mut out).unwrap();

    println!("Code: {}", out);

    let f: extern "C" fn() -> u64 = unsafe { std::mem::transmute(code.start()) };
    println!("{:p}", iter_fac as *const u8);
    println!("{}", f());
    drop(code);
    println!("hi");

    
}