use std::collections::VecDeque;
use std::io::{Read, Write};
use std::num::Wrapping;

/**
 * TODOS:
 *  - Tests
 *  - Debugging: IP to line/column, dump registers...
 */

#[derive(Debug)]
pub enum BrainfuckError {
    IOError(std::io::Error),
    MissingClosingBraceError,
    MissingOpeningBraceError,
}

impl std::fmt::Display for BrainfuckError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            BrainfuckError::IOError(io_err) => write!(f, "IO error: {}", io_err),
            BrainfuckError::MissingClosingBraceError => write!(f, "Found [ with no closing ]"),
            BrainfuckError::MissingOpeningBraceError => write!(f, "Found ] with no opening ["),
        }
    }
}

impl std::error::Error for BrainfuckError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            BrainfuckError::IOError(io_err) => Some(io_err),
            _ => None,
        }
    }
}

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
    registers: VecDeque<u8>,
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
            registers: VecDeque::from(vec![0]),
            input,
            output,
        }
    }
    pub fn is_finished(&self) -> bool {
        self.ip == self.instructions.len()
    }
    pub fn next(&mut self) -> Result<bool, BrainfuckError> {
        if self.is_finished() {
            return Ok(true);
        }
        match self.instructions[self.ip] {
            Instruction::Right => {
                self.register += 1;
                if self.register == self.registers.len() {
                    self.registers.push_back(0);
                }
            }
            Instruction::Left => {
                if self.register == 0 {
                    self.registers.push_front(0);
                } else {
                    self.register -= 1;
                }
            }
            Instruction::Add => {
                let register = &mut self.registers[self.register];
                *register = (Wrapping(*register) + Wrapping(1)).0;
            }
            Instruction::Sub => {
                let register = &mut self.registers[self.register];
                *register = (Wrapping(*register) - Wrapping(1)).0;
            }
            Instruction::In => {
                let mut data = [0];
                match self.input.read_exact(&mut data) {
                    Ok(()) => self.registers[self.register] = data[0],
                    Err(io_err) => return Err(BrainfuckError::IOError(io_err)),
                }
            }
            Instruction::Out => {
                if let Err(io_err) = self.output.write_all(&[self.registers[self.register]]) {
                    return Err(BrainfuckError::IOError(io_err));
                }
            }
            Instruction::Do => {
                if self.registers[self.register] == 0 {
                    let mut dos = 1;
                    while dos != 0 {
                        self.ip += 1;
                        if self.is_finished() {
                            return Err(BrainfuckError::MissingClosingBraceError);
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
                    return Ok(false);
                } else {
                    return Err(BrainfuckError::MissingOpeningBraceError);
                }
            }
        }
        self.ip += 1;
        if self.is_finished() {
            if self.dos.is_empty() {
                Ok(true)
            } else {
                Err(BrainfuckError::MissingClosingBraceError)
            }
        } else {
            Ok(false)
        }
    }
    pub fn run(self) -> Result<(), BrainfuckError> {
        let mut state = self;
        while !state.next()? {}
        Ok(())
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
