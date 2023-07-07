

use macroassembler::assembler::{arm64assembler::*, assembler_common::{ARM64LogicalImmediate}};
use capstone::prelude::*;
fn main() {
    let mut asm = ARM64Assembler::new();

    asm.movz::<32>(x0, 42, 0);
    asm.ret(lr);

    let cs = Capstone::new()
        .arm64()
        .mode(arch::arm64::ArchMode::Arm)
        .build()
        .unwrap();


    for insn in asm.buffer().data().chunks(4) {
        for val in insn.iter() {
            print!("{:02x} ", val);
        }

        println!();
    }

    let insns = cs.disasm_all(asm.buffer().data(), 0x0).unwrap();

    for i in insns.iter() {
        println!("{}", i);
    }

    /*let mut opts = JitAllocatorOptions::default();
    opts.use_dual_mapping = true;
    let mut alloc = jit_allocator::JitAllocator::new(opts);

    let (rx, rw) = alloc.alloc(asm.code_size()).unwrap();

    unsafe {
        std::ptr::copy_nonoverlapping(asm.buffer().data().as_ptr(), rw, asm.code_size());
    }*/

}
