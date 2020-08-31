use masm::linkbuffer::LinkBuffer;
use masm::x86_assembler::RegisterID;
use masm::x86masm::{MacroAssemblerX86, RelationalCondition};
#[cfg(not(target_arch = "x86_64"))]
compile_error!("Example only for x86_64!");
use capstone::prelude::*;
fn main() {
    let mut masm = MacroAssemblerX86::new(true);

    masm.function_prologue(0);
    masm.move_i32(1, RegisterID::EAX);
    let arg = masm.register_for_arg(0);
    let br = masm.branch32_imm(RelationalCondition::LessThanOrEqual, 1, arg); // if arg <= 1 then return 1
    masm.push(RegisterID::EBX);
    masm.move_rr(arg, RegisterID::EBX);
    masm.sub32_imm(1, RegisterID::EDI);
    let call = masm.call(1); // fac(arg-1)

    masm.mul32(RegisterID::EAX, RegisterID::EBX, RegisterID::EAX); // fac(arg-1)*arg
    masm.pop(RegisterID::EBX);
    br.link(&mut masm); // return 1;
    masm.function_epilogue();
    masm.ret();

    let buffer = LinkBuffer::from_masm(&mut masm);
    buffer.link_call(call, buffer.code);

    let code = unsafe { std::slice::from_raw_parts(buffer.code, masm.asm.data().len()) };
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

    let fun: extern "C" fn(i32) -> i32 = unsafe { std::mem::transmute(buffer.code) };

    println!("{}", fun(5));
}
