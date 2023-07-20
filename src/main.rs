use capstone::prelude::BuildsCapstone;
use macroassembler::assembler::arm64assembler::*;

fn main() {
    let mut asm = ARM64Assembler::new();
    
    asm.movz::<32>(x0, 42, 0);
    asm.ret(lr);
  
    let mut alloc = jit_allocator::JitAllocator::new(Default::default());

    unsafe {
        let cs = capstone::Capstone::new().arm64().mode(capstone::arch::arm64::ArchMode::Arm).build().unwrap();
        let insns = cs.disasm_all(asm.buffer().data(), 0).unwrap();

        for i in insns.iter() {
            println!("{}", i);
        }
        let (rx, rw) = alloc.alloc(asm.code_size()).unwrap();

        rw.copy_from_nonoverlapping(asm.buffer().data().as_ptr(), asm.code_size());
        jit_allocator::flush_instruction_cache(rx, asm.code_size());

        let func: extern "C" fn() -> i32 = std::mem::transmute(rx);

        println!("Result: {}", func());
    }
}