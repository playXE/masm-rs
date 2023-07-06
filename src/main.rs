use jit_allocator::JitAllocatorOptions;
use macroassembler::assembler::arm64assembler::*;
use capstone::prelude::*;
fn main() {
    let mut asm = ARM64Assembler::new();

    asm.add::<32, false>(x0, x1, x0);
    asm.ret(lr);

    let cs = Capstone::new()
        .arm64()
        .mode(arch::arm64::ArchMode::Arm)
        .build()
        .unwrap();

    let insns = cs.disasm_all(asm.buffer().data(), 0x0).unwrap();

    for i in insns.iter() {
        println!("{}", i);
    }

    let mut opts = JitAllocatorOptions::default();
    opts.use_dual_mapping = true;
    let mut alloc = jit_allocator::JitAllocator::new(opts);

    let (rx, rw) = alloc.alloc(asm.code_size()).unwrap();

    unsafe {
        std::ptr::copy_nonoverlapping(asm.buffer().data().as_ptr(), rw, asm.code_size());
    }

    let f: extern "C" fn(i32, i32) -> i32 = unsafe { std::mem::transmute(rx) };

    println!("{} + {} = {}", 42, 2, f(42, 2));
}
