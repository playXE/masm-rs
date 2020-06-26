use capstone::prelude::*;
use masm::linkbuffer::*;
use masm::x86_assembler::*;
use masm::x86masm::*;
use masm::MacroAssemblerBase;
#[inline(never)]
unsafe extern "C" fn foo() {
    println!("Hello from JIT!");
}

fn main() {
    let mut masm = MacroAssemblerX86::new(true);
    masm.function_prologue(0);
    masm.push(RegisterID::EBX);
    masm.move_rr(RegisterID::EDI, RegisterID::EBX);
    masm.move_i32(2, RegisterID::EDI);
    let br = masm.branch32(
        RelationalCondition::LessThan,
        RegisterID::EDI,
        RegisterID::EBX,
    );
    masm.move_i32(1, RegisterID::EAX);
    masm.move_rr(RegisterID::EBX, RegisterID::EDI);
    masm.sub32(RegisterID::EDI, RegisterID::EAX, RegisterID::EDI);
    let call = masm.call();
    masm.mul32_rr(RegisterID::EBX, RegisterID::EAX);
    let epilog_jump = masm.jump();

    br.link(&mut masm);
    masm.move_i32(1, RegisterID::EAX);
    epilog_jump.link(&mut masm);
    masm.pop(RegisterID::EBX);
    masm.function_epilogue();
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
        buffer.link_call(call, ptr);
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
        let f: fn(i32) -> i32 = std::mem::transmute(ptr);
        println!("{}", f(5));
    }
}
