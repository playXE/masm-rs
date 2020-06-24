use capstone::prelude::*;
use masm::x86_assembler::*;

fn main() {
    let mut asm = X86Asm::new(true);
    asm.leal_mr_scaled(0, RegisterID::EDI, RegisterID::ESI, 0, RegisterID::EAX);
    asm.movl_rr(RegisterID::EDI, RegisterID::EAX);
    asm.addl_rr(RegisterID::ESI, RegisterID::EAX);
    asm.ret();
    let cs = Capstone::new()
        .x86()
        .mode(arch::x86::ArchMode::Mode64)
        .syntax(arch::x86::ArchSyntax::Intel)
        .detail(true)
        .build()
        .expect("Failed to create Capstone object");
    let insns = cs.disasm_all(asm.data(), 0x0);
    for i in insns.unwrap().iter() {
        println!("{}", i);
    }
}
