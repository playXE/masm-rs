use macroassembler::assembler::riscv64assembler::*;
use macroassembler::assembler::riscv64disassembler::try_to_disassemble;
use macroassembler::jit::allocate_executable_memory;
fn main() {
    let mut asm = RISCV64Assembler::new();
    
    let imml = ImmediateLoader::new(0xdeadbeef);

    imml.move_into(&mut asm, x10);
    asm.jalr(zero, x1, 0);

    let code = asm.buffer().data();

    let mut out = std::io::stdout();

    try_to_disassemble(code, "", 0, &mut out).unwrap();

    let code = allocate_executable_memory(32, 32);

    let start = code.start() as *mut u8;

    unsafe {
        std::ptr::copy_nonoverlapping(asm.buffer().data().as_ptr(), start, asm.buffer().data().len());

        let func = std::mem::transmute::<*mut u8, extern "C" fn() -> u32>(start);

        println!("Result: {:#x}", func());
    }
}