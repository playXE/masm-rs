mod compile;
use compile::BfJIT;
use pico_args::*;
use std::ffi::OsStr;
use std::path::PathBuf;
fn parse_path(s: &OsStr) -> Result<PathBuf, &'static str> {
    Ok(s.into())
}
fn main() {
    let mut args = Arguments::from_env();

    let disasm = args.contains(["-d", "--disasm"]);
    let opt = args.contains(["-O", "--optimize"]);
    let input = args
        .opt_value_from_os_str(["-i", "--input"], parse_path)
        .unwrap();

    if input.is_none() {
        println!(
            "No input provided. To compile brainfuck program run `brainfuck -i <program_name>"
        );
        return;
    }
    let input = input.unwrap();
    let jit = BfJIT::new(compile::CGContext {
        opt_level: opt as u8,
        getchar: getchar_default,
        putchar: putchar_default,
    });
    let compile_start = std::time::Instant::now();
    let (code, size) = jit.translate(&std::fs::read_to_string(input.to_str().unwrap()).unwrap());
    let compile_end = compile_start.elapsed();
    if disasm {
        use capstone::prelude::*;
        let code = unsafe { std::slice::from_raw_parts(code, size) };
        println!("Disassembly for program '{}'", input.display());
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
    }

    let mut mem: Vec<u8> = vec![0; 1048576];
    let mem_ptr = mem.as_mut_ptr();
    unsafe {
        let fun: unsafe extern "C" fn(*mut u8) = std::mem::transmute(code);
        let start = std::time::Instant::now();
        fun(mem_ptr);
        let end = start.elapsed();

        println!(
            "Compiled in {}ms({}ns), executed in {}ms({}ns)",
            compile_end.as_millis(),
            compile_end.as_nanos(),
            end.as_millis(),
            end.as_nanos()
        );
    }
}
use std::io::{Read, Write};
pub unsafe extern "C" fn putchar_default(x: u8) {
    let mut out = ::std::io::stdout();
    out.write_all(&[x]).unwrap();
    out.flush().unwrap();
}

pub unsafe extern "C" fn getchar_default() -> u8 {
    let mut buf: [u8; 1] = [0; 1];
    ::std::io::stdin().read_exact(&mut buf).unwrap();
    buf[0]
}
