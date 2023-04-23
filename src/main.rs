use std::mem::transmute;

use macroassembler::assembler::abstract_macro_assembler::Address;
use macroassembler::assembler::macro_assembler_riscv64::MacroAssemblerRISCV64;
use macroassembler::assembler::riscv64assembler::*;
use macroassembler::assembler::riscv64disassembler::try_to_disassemble;
use macroassembler::jit::allocate_executable_memory;
fn main() {

    let mut asm = MacroAssemblerRISCV64::new();

    asm.add32(42i32, Address::new(x10, 0));
    asm.add32_rrr(1i32, x11, x10);
    asm.ret();

    let code = asm.assembler.buffer().data();

    
    let mut out = String::new();

    try_to_disassemble(code, "", 0, &mut out).unwrap();
    println!("{}", out);
    let code = allocate_executable_memory(128, 128);

    let start = code.start() as *mut u8;

    unsafe {
        std::ptr::copy_nonoverlapping(asm.assembler.buffer().data().as_ptr(), start, asm.assembler.buffer().data().len());

        let func: extern "C" fn(&mut i32, i32) -> i32 = std::mem::transmute(start);
        let mut x = 0;

        let y = func(&mut x, 4);
        
        println!("{} {}", y, x);

    }
}