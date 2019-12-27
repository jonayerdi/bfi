use std::collections::VecDeque;
use std::io::{Read, Write};

/**
 * TODOS:
 *  - Allocate registers as left and right shifts happen
 *  - Allow registers to the left of the first one (with VecDeque?)
 *  - Return Result<bool,BrainfuckError> instead of panicking on error
 *  - On error: Dump registers, register, ip, instructions
 *  - Handle over/underflows in Add/Sub operations
 *  - Tests
 */

#[derive(Clone, Copy)]
pub enum Instruction {
    Right,
    Left,
    Add,
    Sub,
    In,
    Out,
    Do,
    While,
}

pub trait Encode {
    fn encode(i: &Instruction) -> Self;
}

impl Encode for u8 {
    fn encode(i: &Instruction) -> Self {
        let c: char = Encode::encode(i);
        c as u8
    }
}

impl Encode for char {
    fn encode(i: &Instruction) -> Self {
        match i {
            Instruction::Right => '>',
            Instruction::Left => '<',
            Instruction::Add => '+',
            Instruction::Sub => '-',
            Instruction::In => ',',
            Instruction::Out => '.',
            Instruction::Do => '[',
            Instruction::While => ']',
        }
    }
}

pub trait Decode {
    fn decode(&self) -> Option<Instruction>;
}

impl Decode for u8 {
    fn decode(&self) -> Option<Instruction> {
        if self.is_ascii() {
            Decode::decode(&(*self as char))
        } else {
            None
        }
    }
}

impl Decode for char {
    fn decode(&self) -> Option<Instruction> {
        match self {
            '>' => Some(Instruction::Right),
            '<' => Some(Instruction::Left),
            '+' => Some(Instruction::Add),
            '-' => Some(Instruction::Sub),
            ',' => Some(Instruction::In),
            '.' => Some(Instruction::Out),
            '[' => Some(Instruction::Do),
            ']' => Some(Instruction::While),
            _ => None,
        }
    }
}

#[allow(dead_code)]
pub fn instructions<I, D>(iter: I) -> Vec<Instruction>
where
    I: Iterator<Item = D>,
    D: Decode,
{
    iter.filter_map(|c| c.decode()).collect()
}

pub struct State<'a, R, W>
where
    R: Read,
    W: Write,
{
    ip: usize,
    register: usize,
    instructions: &'a [Instruction],
    dos: Vec<usize>,
    registers: Vec<u8>,
    input: R,
    output: W,
}

#[allow(dead_code)]
impl<'a, R, W> State<'a, R, W>
where
    R: Read,
    W: Write,
{
    pub fn new(instructions: &'a [Instruction], input: R, output: W) -> Self {
        State {
            ip: 0,
            register: 0,
            instructions,
            dos: Vec::with_capacity(4),
            registers: vec![0; 16],
            input,
            output,
        }
    }
    pub fn is_finished(&self) -> bool {
        self.ip == self.instructions.len()
    }
    pub fn next(&mut self) -> bool {
        if self.is_finished() {
            return true;
        }
        match self.instructions[self.ip] {
            Instruction::Right => self.register += 1,
            Instruction::Left => self.register -= 1,
            Instruction::Add => self.registers[self.register] += 1,
            Instruction::Sub => self.registers[self.register] -= 1,
            Instruction::In => {
                let mut data = [0];
                if self.input.read_exact(&mut data).is_ok() {
                    self.registers[self.register] = data[0];
                } else {
                    panic!("Input error at IP={}", self.ip);
                }
            }
            Instruction::Out => {
                if self
                    .output
                    .write_all(&[self.registers[self.register]])
                    .is_err()
                {
                    panic!("Output error at IP={}", self.ip);
                }
            }
            Instruction::Do => {
                if self.registers[self.register] == 0 {
                    let old_ip = self.ip;
                    let mut dos = 1;
                    while dos != 0 {
                        self.ip += 1;
                        if self.ip == self.instructions.len() {
                            panic!("Found [ without closing ] at IP={}", old_ip);
                        }
                        match self.instructions[self.ip] {
                            Instruction::Do => dos += 1,
                            Instruction::While => dos -= 1,
                            _ => {}
                        }
                    }
                } else {
                    self.dos.push(self.ip);
                }
            }
            Instruction::While => {
                if let Some(ip) = self.dos.pop() {
                    self.ip = ip;
                    return false;
                } else {
                    panic!("Found ] without matching [ at IP={}", self.ip);
                }
            }
        }
        self.ip += 1;
        if self.is_finished() {
            let mut error_msg = String::new();
            while let Some(ip) = self.dos.pop() {
                let msg = format!("Found [ without closing ] at IP={}\n", ip);
                error_msg.push_str(&msg);
            }
            if !error_msg.is_empty() {
                panic!("{}", error_msg);
            }
            true
        } else {
            false
        }
    }
    pub fn run(self) {
        let mut state = self;
        while !state.next() {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        // TODO
    }
}
