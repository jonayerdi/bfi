use std::collections::VecDeque;
use std::io::{Read, Write};

/**
 * TODOS:
 *  - Tests
 *  - Debugging: IP to line/column, dump registers...
 */

#[derive(Debug)]
pub enum BrainfuckError {
    IOError(std::io::Error),
    RegisterUnderflowError,
    RegisterOverallocationError,
    OverflowError,
    UnderflowError,
    MissingClosingBraceError,
    MissingOpeningBraceError,
}

impl std::fmt::Display for BrainfuckError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            BrainfuckError::IOError(io_err) => write!(f, "IO error: {}", io_err),
            BrainfuckError::RegisterUnderflowError => write!(f, "Register underflow on left shift"),
            BrainfuckError::RegisterOverallocationError => write!(f, "Too many registers allocated"),
            BrainfuckError::OverflowError => write!(f, "Addition overflow"),
            BrainfuckError::UnderflowError => write!(f, "Substraction underflow"),
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

#[allow(dead_code)]
pub enum LeftShiftMode {
    Underflow,
    Error,
}

impl Default for LeftShiftMode {
    fn default() -> Self { LeftShiftMode::Underflow }
}

#[allow(dead_code)]
pub enum ShiftMode {
    Infinite,
    Finite(usize),
}

impl Default for ShiftMode {
    fn default() -> Self { ShiftMode::Infinite }
}

#[allow(dead_code)]
pub enum OverflowMode {
    Wrap,
    Error,
}

impl Default for OverflowMode {
    fn default() -> Self { OverflowMode::Wrap }
}

#[allow(dead_code)]
pub enum InputErrorMode {
    Value(u8),
    Unchanged,
    Error,
}

impl Default for InputErrorMode {
    fn default() -> Self { InputErrorMode::Value(0) }
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
    left_shift: LeftShiftMode,
    shift: ShiftMode,
    overflow: OverflowMode,
    eof: InputErrorMode,
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
            left_shift: LeftShiftMode::default(),
            shift: ShiftMode::default(),
            overflow: OverflowMode::default(),
            eof: InputErrorMode::default(),
            ip: 0,
            register: 0,
            instructions,
            dos: Vec::with_capacity(4),
            registers: VecDeque::from(vec![0]),
            input,
            output,
        }
    }
    pub fn with_left_shift_mode(self, left_shift: LeftShiftMode) -> Self {
        let mut this = self;
        this.left_shift = left_shift;
        this
    }
    pub fn with_shift_mode(self, shift: ShiftMode) -> Self {
        let mut this = self;
        this.shift = shift;
        this
    }
    pub fn with_overflow_mode(self, overflow: OverflowMode) -> Self {
        let mut this = self;
        this.overflow = overflow;
        this
    }
    pub fn with_eof_mode(self, eof: InputErrorMode) -> Self {
        let mut this = self;
        this.eof = eof;
        this
    }
    pub fn is_finished(&self) -> bool {
        self.ip == self.instructions.len()
    }
    pub fn left(&mut self) -> Result<(), BrainfuckError> {
        if self.register == 0 {
            match self.left_shift {
                LeftShiftMode::Underflow => {
                    match self.shift {
                        ShiftMode::Infinite => {
                            Ok(self.registers.push_front(0))
                        }
                        ShiftMode::Finite(max_register) => {
                            if self.registers.len() < max_register {
                                Ok(self.registers.push_front(0))
                            } else {
                                Err(BrainfuckError::RegisterOverallocationError)
                            }
                        }
                    }
                }
                LeftShiftMode::Error => Err(BrainfuckError::RegisterUnderflowError),
            }
        } else {
            self.register -= 1;
            Ok(())
        }
    }
    pub fn right(&mut self) -> Result<(), BrainfuckError> {
        self.register += 1;
        if self.register == self.registers.len() {
            match self.shift {
                ShiftMode::Infinite => {
                    Ok(self.registers.push_back(0))
                }
                ShiftMode::Finite(max_register) => {
                    if self.register < max_register {
                        Ok(self.registers.push_back(0))
                    } else {
                        self.register -= 1;
                        Err(BrainfuckError::RegisterOverallocationError)
                    }
                }
            }
        } else {
            Ok(())
        }
    }
    pub fn add(&mut self) -> Result<(), BrainfuckError> {
        let register = &mut self.registers[self.register];
        if let Some(result) = register.checked_add(1) {
            *register = result;
            Ok(())
        } else {
            match self.overflow {
                OverflowMode::Wrap => {
                    *register = u8::min_value();
                    Ok(())
                }
                OverflowMode::Error => Err(BrainfuckError::OverflowError),
            }
        }
    }
    pub fn sub(&mut self) -> Result<(), BrainfuckError> {
        let register = &mut self.registers[self.register];
        if let Some(result) = register.checked_sub(1) {
            *register = result;
            Ok(())
        } else {
            match self.overflow {
                OverflowMode::Wrap => {
                    *register = u8::max_value();
                    Ok(())
                }
                OverflowMode::Error => Err(BrainfuckError::UnderflowError),
            }
        }
    }
    pub fn read(&mut self) -> Result<(), BrainfuckError> {
        let register = &mut self.registers[self.register];
        let mut data = [0];
        match self.input.read_exact(&mut data) {
            Ok(()) => {
                *register = data[0];
                Ok(())
            },
            Err(io_err) => match self.eof {
                InputErrorMode::Value(v) => {
                    *register = v;
                    Ok(())
                },
                InputErrorMode::Unchanged => Ok(()),
                InputErrorMode::Error => Err(BrainfuckError::IOError(io_err)),
            },
        }
    }
    pub fn write(&mut self) -> Result<(), BrainfuckError> {
        let register = self.registers[self.register];
        if let Err(io_err) = self.output.write_all(&[register]) {
            Err(BrainfuckError::IOError(io_err))
        } else {
            Ok(())
        }
    }
    pub fn next(&mut self) -> Result<bool, BrainfuckError> {
        if self.is_finished() {
            return Ok(true);
        }
        match self.instructions[self.ip] {
            Instruction::Right => self.right()?,
            Instruction::Left => self.left()?,
            Instruction::Add => self.add()?,
            Instruction::Sub => self.sub()?,
            Instruction::In => self.read()?,
            Instruction::Out => self.write()?,
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
