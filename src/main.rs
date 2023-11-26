use macroassembler::{assembler::{TargetMacroAssembler, link_buffer::LinkBuffer}, jit::gpr_info::{ARGUMENT_GPR0, ARGUMENT_GPR1, RETURN_VALUE_GPR}};

fn main() {
    let mut asm = TargetMacroAssembler::new();

    asm.comment("main:");
    asm.add32_rrr(ARGUMENT_GPR0, ARGUMENT_GPR1, RETURN_VALUE_GPR);
    asm.ret();

    let mut lb = LinkBuffer::from_macro_assembler(&mut asm).unwrap();

    let mut out = String::new();

    let _ = lb.finalize_with_disassembly(true, "main", &mut out).unwrap();

    println!("{}", out);
}