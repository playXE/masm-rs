use masm::x86_assembler::RegisterID;
use masm::x86masm::*;
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
        let mut jmp_to_end: Vec<(Label, Jump)> = vec![];
        let mut masm = MacroAssemblerX86::new(true);

        for t in input {
            match *t {
                Token::Forward(n) => masm.add64_imm32(n as _, RegisterID::EDI, RegisterID::EDI),
                Token::Backward(n) => masm.sub64_imm32(n as _, RegisterID::EDI),
                Token::Add(n) => {
                    masm.add8_im(n as _, Mem::Base(RegisterID::EDI, 0));
                }
                Token::Sub(n) => {
                    masm.load8(Mem::Base(RegisterID::EDI, 0), RegisterID::EAX);
                    masm.sub32_imm(n as _, RegisterID::EAX);
                    masm.store8(RegisterID::EAX, Mem::Base(RegisterID::EDI, 0))
                }
                Token::Output => {
                    masm.push(RegisterID::EDI);
                    masm.load8_sign_extend_to_32(Mem::Base(RegisterID::EDI, 0), RegisterID::EDI);
                    masm.call_ptr(self.ctx.putchar as *const u8);
                    masm.pop(RegisterID::EDI);
                }
                Token::Input => {
                    masm.push(RegisterID::EDI);
                    masm.call_ptr(self.ctx.getchar as *const u8);
                    masm.pop(RegisterID::EDI);
                    masm.store8(RegisterID::EAX, Mem::Base(RegisterID::EDI, 0));
                }
                Token::LoopBegin => {
                    masm.load8_sign_extend_to_32(Mem::Base(RegisterID::EDI, 0), RegisterID::EAX);
                    let jend = masm.branch32_imm(RelationalCondition::Equal, 0, RegisterID::EAX);
                    let start = masm.label();
                    jmp_to_end.push((start, jend));
                }
                Token::LoopEnd => {
                    let (start, jend) = jmp_to_end.pop().unwrap();
                    masm.load8_sign_extend_to_32(Mem::Base(RegisterID::EDI, 0), RegisterID::EAX);
                    let j = masm.branch32_imm(RelationalCondition::NotEqual, 0, RegisterID::EAX);
                    j.link_to(&mut masm, start);
                    jend.link(&mut masm);
                }
                Token::LoopToZero => {
                    masm.store8_imm(0, Mem::Base(RegisterID::EDI, 0));
                }
                Token::LoopToAdd => {
                    masm.load8(Mem::Base(RegisterID::EDI, 0), RegisterID::ESI);
                    masm.add32_rm(RegisterID::ESI, Mem::Base(RegisterID::EDI, 1));
                    masm.store8_imm(0, Mem::Base(RegisterID::EDI, 0));
                }
            }
        }

        masm.ret();
        let buffer = masm::linkbuffer::LinkBuffer::from_masm(&mut masm);

        (buffer.code, masm.asm.data().len())
    }
}
