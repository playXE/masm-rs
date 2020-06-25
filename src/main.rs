use capstone::prelude::*;
use masm::linkbuffer::*;
use masm::x86_assembler::*;
use masm::x86masm::MacroAssemblerX86;
use masm::MacroAssemblerBase;
#[inline(never)]
unsafe extern "C" fn foo() {
    println!("Hello from JIT!");
}

fn main() {
    let mut masm = MacroAssemblerX86::new(true);
    masm.asm.push_r(RegisterID::EBP);
    masm.move_rr(RegisterID::ESP, RegisterID::EBP);
    let c = masm.call64();
    masm.move_rr(RegisterID::EBP, RegisterID::ESP);
    masm.asm.pop_r(RegisterID::EBP);
    masm.ret();
    let code = masm.finalize();
    let cs = Capstone::new()
        .x86()
        .mode(arch::x86::ArchMode::Mode64)
        .syntax(arch::x86::ArchSyntax::Intel)
        .detail(true)
        .build()
        .expect("Failed to create Capstone object");
    let insns = cs.disasm_all(&code, 0x0);
    for i in insns.unwrap().iter() {
        println!("{}", i);
    }
    let mut memory = Memory::new();
    let ptr = memory.allocate(code.len(), 8).unwrap();
    unsafe {
        std::ptr::copy_nonoverlapping(code.as_ptr(), ptr, code.len());
        let buffer = LinkBuffer::<MacroAssemblerX86>::new(ptr);
        buffer.link_call(c.label, foo as *const u8);
        memory.set_readable_and_executable();
        let cs = Capstone::new()
            .x86()
            .mode(arch::x86::ArchMode::Mode64)
            .syntax(arch::x86::ArchSyntax::Intel)
            .detail(true)
            .build()
            .expect("Failed to create Capstone object");
        let insns = cs.disasm_all(std::slice::from_raw_parts(ptr, code.len()), ptr as _);
        for i in insns.unwrap().iter() {
            println!("{}", i);
        }
        let f: fn() = std::mem::transmute(ptr);
        f();
    }
}
