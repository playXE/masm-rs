#[allow(unused_unsafe)]
pub unsafe fn try_to_disassemble<W: std::fmt::Write>(
    code: *const u8,
    size: usize,
    prefix: &str,
    out: &mut W,
) -> std::fmt::Result {
    
    #[cfg(target_arch = "x86_64")]
    unsafe {
        use super::assembly_comments::AssemblyCommentsRegistry;

        use iced_x86::*;
        let code = std::slice::from_raw_parts(code, size);
        let mut decoder = Decoder::with_ip(64, code, code.as_ptr() as _, DecoderOptions::NONE);

        let mut formatter = GasFormatter::new();
        formatter
            .options_mut()
            .set_gas_show_mnemonic_size_suffix(true);
        formatter.options_mut().set_uppercase_hex(false);
        for instruction in &mut decoder {
            let mut output = String::new();
            formatter.format(&instruction, &mut output);
            if let Some(comment) =
                AssemblyCommentsRegistry::singleton().comment(instruction.ip() as _)
            {
                writeln!(out, "; {}", comment)?;
            }
            write!(out, "{}0x{:x}: {}\n", prefix, instruction.ip(), output)?;

            /*if let Some(comment) = AssemblyCommentsRegistry::singleton().comment(instruction.ip() as _) {
                write!(out, "; {}\n", comment)?;
            } else {
                write!(out, "\n")?;
            }*/
        }
    }

    #[cfg(target_arch = "riscv64")]
    unsafe {
        super::riscv64disassembler::try_to_disassemble(
            std::slice::from_raw_parts(code, size),
            prefix,
            code as u64,
            out,
        )?;
    }

    #[cfg(target_arch="aarch64")]
    {
        use capstone::prelude::*;

        let cs = Capstone::new()
            .arm64()
            .mode(arch::arm64::ArchMode::Arm)
            .detail(true)
            .build()
            .expect("failed to create Capstone object");

        let code = std::slice::from_raw_parts(code, size);
        
        let insns = cs.disasm_all(code, code.as_ptr() as _).expect("failed to disassemble");

        for insn in insns.iter() {
            write!(out, "{}0x{:x}: {} {}\n", prefix, insn.address(), insn.mnemonic().unwrap(), insn.op_str().unwrap())?;
        }
    }

    Ok(())
}
