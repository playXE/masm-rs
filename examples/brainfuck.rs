
use std::io::Read;
use std::io::Write;

use macroassembler::assembler::abstract_macro_assembler::AbsoluteAddress;
use macroassembler::assembler::abstract_macro_assembler::Address;
use macroassembler::assembler::abstract_macro_assembler::Jump;
use macroassembler::assembler::abstract_macro_assembler::Label;
use macroassembler::assembler::link_buffer::LinkBuffer;
use macroassembler::assembler::macro_assembler_x86_common::*;
use macroassembler::assembler::x86assembler::*;
use macroassembler::wtf::executable_memory_handle::CodeRef;

pub struct BfJIT {
    ctx: CGContext,
}
pub struct CGContext {
    pub opt_level: u8,
}
#[derive(Copy, Clone, Debug)]
enum Token {
    Forward(u32),
    Backward(u32),
    Add(u8),
    Sub(u8),
    Output,
    Input,
    LoopBegin,
    LoopEnd,

    LoopToZero,
    LoopToAdd,
}

impl BfJIT {
    pub fn new(ctx: CGContext) -> Self {
        Self { ctx }
    }

    pub fn translate(&self, disasm: bool, input: &str) -> CodeRef {
        let mut tokens: Vec<Token> = vec![];
        let mut chars = input.chars().peekable();
        loop {
            let c = if let Some(c) = chars.next() { c } else { break };
            match c {
                '>' => {
                    let mut n: u32 = 1;
                    if self.ctx.opt_level > 0 {
                        while chars.peek() == Some(&'>') {
                            n += 1;
                            chars.next().unwrap();
                        }
                    }
                    tokens.push(Token::Forward(n));
                }
                '<' => {
                    let mut n: u32 = 1;
                    if self.ctx.opt_level > 0 {
                        while chars.peek() == Some(&'<') {
                            n += 1;
                            chars.next().unwrap();
                        }
                    }
                    tokens.push(Token::Backward(n));
                }
                '+' => {
                    let mut n: u8 = 1;
                    if self.ctx.opt_level > 0 {
                        while chars.peek() == Some(&'+') {
                            n += 1;
                            chars.next().unwrap();
                        }
                    }
                    tokens.push(Token::Add(n));
                }
                '-' => {
                    let mut n: u8 = 1;
                    if self.ctx.opt_level > 0 {
                        while chars.peek() == Some(&'-') {
                            n += 1;
                            chars.next().unwrap();
                        }
                    }
                    tokens.push(Token::Sub(n));
                }
                '.' => tokens.push(Token::Output),
                ',' => tokens.push(Token::Input),
                '[' => tokens.push(Token::LoopBegin),
                ']' => tokens.push(Token::LoopEnd),
                _ => {}
            };
        }
        if self.ctx.opt_level > 0 {
            tokens = self.opt_inst_combine(&tokens);
        }

        self.do_translate(disasm, &tokens)
    }

    fn opt_inst_combine(&self, tokens: &[Token]) -> Vec<Token> {
        let mut ret: Vec<Token> = vec![];
        let mut i: usize = 0;
        loop {
            if i >= tokens.len() {
                break;
            }
            match tokens[i..] {
                [Token::LoopBegin, Token::Sub(1), Token::LoopEnd, ..] => {
                    ret.push(Token::LoopToZero);
                    i += 3;
                }
                [Token::LoopBegin, Token::Sub(1), Token::Forward(1), Token::Add(1), Token::Backward(1), Token::LoopEnd, ..] =>
                {
                    ret.push(Token::LoopToAdd);
                    i += 6;
                }
                _ => {
                    ret.push(tokens[i]);
                    i += 1;
                }
            }
        }
        ret
    }

    fn do_translate(&self, disasm: bool, input: &[Token]) -> CodeRef {
        let mut jmps_to_end: Vec<(Label, Jump)> = vec![];

        let mut masm = MacroAssemblerX86Common::new();
      
        for t in input {
            match *t {
                Token::Forward(n) => {
                    masm.comment(format!("forward {}", n));
                    masm.add64(n as i32, edi);
                }
                Token::Backward(n) => {
                    masm.comment(format!("backward {}", n));
                    masm.sub64(n as i32, edi);
                }
                Token::Add(n) => {
                    masm.comment(format!("add {}", n));
                    masm.add8(n as i32, Address::new(edi, 0));
                }
                Token::Sub(n) => {
                    masm.comment(format!("sub {}", n));
                    masm.sub8(n as i32, Address::new(edi, 0));
                }

                Token::Output => {
                    masm.comment("output");
                    masm.assembler.push_r(edi);
                    masm.load8_signed_extend_to_32(Address::new(edi, 0), edi);
                    masm.call_op(Some(AbsoluteAddress::new(putchar as _)));
                    masm.assembler.pop_r(edi);
                }

                Token::Input => {
                    masm.comment("input");
                    masm.assembler.push_r(edi);
                    masm.call_op(Some(AbsoluteAddress::new(getchr as _)));
                    masm.assembler.pop_r(edi);
                    masm.store8(eax, Address::new(edi, 0));
                }

                Token::LoopBegin => {
                    masm.comment("loop begin");
                    let jend = masm.branch8(RelationalCondition::Equal, Address::new(edi, 0), 0);
                    let start = masm.label();

                    jmps_to_end.push((start, jend));
                }

                Token::LoopEnd => {
                    
                    masm.comment("loop end");
                    let (start, jend) = jmps_to_end.pop().unwrap();
                   
                    masm.load8_signed_extend_to_32(Address::new(edi, 0), eax);
                    
                    let j = masm.branch32(RelationalCondition::NotEqual, eax, 0i32);
                    j.link_to(&mut masm, start);
                    jend.link(&mut masm);
                }

                Token::LoopToZero => {
                    masm.comment("loop to zero");
                    masm.store8(0i32, Address::new(edi, 0));
                }

                Token::LoopToAdd => {
                    masm.comment("loop to add");
                    masm.load8(Address::new(edi, 0), esi);
                    masm.add32(esi, Address::new(edi, 1));
                    masm.store8(0i32, Address::new(edi, 0));
                }
            }
        }
        
        masm.ret();
        assert!(jmps_to_end.is_empty());
        let mut buffer = LinkBuffer::from_macro_assembler(&mut masm);

        let mut fmt = String::new();

        let code = buffer.finalize_with_disassembly(disasm, "brainfuck", &mut fmt).unwrap();

        println!("{}", fmt);

        code
    }
}


extern "C" fn putchar(x: u8) {
    let mut out = ::std::io::stdout();
    out.write_all(&[x]).unwrap();
    out.flush().unwrap();
}
extern "C" fn getchr() -> u8 {
    let mut buf = [0u8; 1];
    std::io::stdin().read_exact(&mut buf).unwrap();
    buf[0]
}


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
    let jit = BfJIT::new(CGContext {
        opt_level: opt as u8,
    });
    let compile_start = std::time::Instant::now();

    let code = jit.translate(disasm, &std::fs::read_to_string(input).unwrap());

    println!("{}", code.disassembly());

    println!("Compiled in {:.2}ms", compile_start.elapsed().as_micros() as f64 / 1000.0);

    let fun = unsafe {
        std::mem::transmute::<_, extern "C" fn(*mut u8)>(code.start())
    };
    let mut mem = vec![0u8; 100 * 1024];
    fun(mem.as_mut_ptr());

    drop(code);
    
}
