use macroassembler::assembler::riscv64assembler::*;
use macroassembler::assembler::riscv64disassembler::try_to_disassemble;
fn main() {
    let mut asm = RISCV64Assembler::new();
    
    asm.fadd::<32>(f0, f1, f2);
    asm.add(x10, x11, x10);
    asm.jalr(x0, x0, 0);


    let code = asm.buffer().data();

    let mut out = std::io::stdout();

    try_to_disassemble(code, "", 0, &mut out).unwrap();
}