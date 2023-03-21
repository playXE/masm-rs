
use macroassembler::assembler::TargetMacroAssembler;
use macroassembler::assembler::abstract_macro_assembler::Jump;
use macroassembler::assembler::abstract_macro_assembler::Label;
use macroassembler::assembler::link_buffer::LinkBuffer;
use macroassembler::assembler::macro_assembler_x86_common::*;
use macroassembler::assembler::x86assembler::*;

pub struct BfJIT {
    ctx: CGContext,
}
pub struct CGContext {
    pub putchar: unsafe extern "C" fn(x: u8),
    pub getchar: unsafe extern "C" fn() -> u8,
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

    pub fn translate(&self, input: &str) -> (*const u8, usize) {
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

        self.do_translate(&tokens)
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

    fn do_translate(&self, input: &[Token]) -> (*const u8, usize) {
        let mut jmps_to_end: Vec<(Label, Jump)> = vec![];

        let mut masm = MacroAssemblerX86Common::new();

        for t in input {
            match *t {
                Token::Forward(n) => masm.add32(n as i32, edi),

                _ => todo!(),
            }
        }

        todo!()
    }
}
fn main() {
    let mut asm = TargetMacroAssembler::new();

    asm.mov(1i32, eax);
    asm.comment("if (n <= 1)");
    let branch= asm.branch64(RelationalCondition::LessThanOrEqual, edi, 1);

    asm.comment("n - 1");
    asm.assembler.push_r(ebx);
    asm.mov(edi, ebx);
    asm.add64_rrr(-1, edi, edi);
    asm.comment("n * factorial(n - 1)");
    let call = asm.call();
    asm.mul64_rrr(eax, ebx, eax);
    
    branch.link(&mut asm);
    asm.ret();

    let mut buffer = LinkBuffer::from_macro_assembler(&mut asm);
    buffer.link_call(call, buffer.entrypoint());

    let mut fmt = String::new();
    let code = buffer.finalize_with_disassembly(true, "factorial(u64) -> u64", &mut fmt).unwrap();

    println!("{}", fmt);

    let fac = unsafe { std::mem::transmute::<_, extern "C" fn(u64) -> u64>(code.start()) };

    println!("{}", fac(5));

}
