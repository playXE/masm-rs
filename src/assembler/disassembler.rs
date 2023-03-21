use capstone::prelude::*;

use super::assembly_comments::AssemblyCommentsRegistry;

pub fn try_to_disassemble<W: std::fmt::Write>(code: *const u8, size: usize, prefix: &str, out: &mut W) -> std::fmt::Result {
    let cs; 

    #[cfg(target_arch="x86_64")]
    {
        cs = Capstone::new().x86()
            .mode(arch::x86::ArchMode::Mode64)
            .syntax(arch::x86::ArchSyntax::Intel)
            .detail(true)
            .build()
            .expect("failed to create Capstone object");
    }

    unsafe {
        let code = std::slice::from_raw_parts(code, size);

        let insns = cs.disasm_all(code, code.as_ptr() as _).expect("failed to disassemble");

        for insn in insns.iter() {
            write!(out, "{}0x{:x}: {} {}", prefix, insn.address(), insn.mnemonic().unwrap(), insn.op_str().unwrap())?;
            if let Some(comment) = AssemblyCommentsRegistry::singleton().comment(insn.address() as _) {
                write!(out, "; {}\n", comment)?;
            } else {
                write!(out, "\n")?;
            }
        }
    }

    Ok(())
}