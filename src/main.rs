use capstone::prelude::*;
use masm::x86_assembler::*;
use masm::x86masm::MacroAssemblerX86;

fn main() {
    let mut masm = MacroAssemblerX86::new(true);
    let j = masm.jump();
    masm.ret();
    j.link(&mut masm);
    masm.add32(RegisterID::EDI, RegisterID::ESI, RegisterID::EAX);
    masm.ret();
    let cs = Capstone::new()
        .x86()
        .mode(arch::x86::ArchMode::Mode64)
        .syntax(arch::x86::ArchSyntax::Intel)
        .detail(true)
        .build()
        .expect("Failed to create Capstone object");
    let insns = cs.disasm_all(masm.asm.data(), 0x0);
    for i in insns.unwrap().iter() {
        println!("{}", i);
    }
}
