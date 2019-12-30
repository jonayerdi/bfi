use std::collections::VecDeque;
use std::io::{Read, Write};

/**
 * TODOS:
 *  - dump_state
 *  - Tests
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
            BrainfuckError::RegisterOverallocationError => {
                write!(f, "Too many registers allocated")
            }
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
pub enum NegativeRegisterMode {
    Allow,
    Error,
}

impl Default for NegativeRegisterMode {
    fn default() -> Self {
        NegativeRegisterMode::Allow
    }
}

#[allow(dead_code)]
pub enum RegistersMode {
    Infinite,
    Finite(usize),
}

impl Default for RegistersMode {
    fn default() -> Self {
        RegistersMode::Infinite
    }
}

#[allow(dead_code)]
pub enum OverflowMode {
    Wrap,
    Error,
}

impl Default for OverflowMode {
    fn default() -> Self {
        OverflowMode::Wrap
    }
}

#[allow(dead_code)]
pub enum InputErrorMode {
    Value(u8),
    Unchanged,
    Error,
}

impl Default for InputErrorMode {
    fn default() -> Self {
        InputErrorMode::Value(0)
    }
}

#[derive(Clone, Copy)]
pub enum Instruction {
    Dump, // Special instruction to dump state
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
            Instruction::Dump => '#',
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
    fn decode(&self, dump_state: bool) -> Option<Instruction>;
}

impl Decode for u8 {
    fn decode(&self, dump_state: bool) -> Option<Instruction> {
        if self.is_ascii() {
            Decode::decode(&(*self as char), dump_state)
        } else {
            None
        }
    }
}

impl Decode for char {
    fn decode(&self, dump_state: bool) -> Option<Instruction> {
        match self {
            '>' => Some(Instruction::Right),
            '<' => Some(Instruction::Left),
            '+' => Some(Instruction::Add),
            '-' => Some(Instruction::Sub),
            ',' => Some(Instruction::In),
            '.' => Some(Instruction::Out),
            '[' => Some(Instruction::Do),
            ']' => Some(Instruction::While),
            '#' if dump_state => Some(Instruction::Dump),
            _ => None,
        }
    }
}

#[allow(dead_code)]
pub fn instructions<I, D>(iter: I, dump_state: bool) -> Vec<Instruction>
where
    I: Iterator<Item = D>,
    D: Decode,
{
    iter.filter_map(|c| c.decode(dump_state)).collect()
}

pub struct State<'a, R, W>
where
    R: Read,
    W: Write,
{
    cfg_negative_register: NegativeRegisterMode,
    cfg_registers: RegistersMode,
    cfg_overflow: OverflowMode,
    cfg_input_error: InputErrorMode,
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
            cfg_negative_register: NegativeRegisterMode::default(),
            cfg_registers: RegistersMode::default(),
            cfg_overflow: OverflowMode::default(),
            cfg_input_error: InputErrorMode::default(),
            ip: 0,
            register: 0,
            instructions,
            dos: Vec::with_capacity(4),
            registers: VecDeque::from(vec![0]),
            input,
            output,
        }
    }
    pub fn with_negative_register_mode(self, cfg_negative_register: NegativeRegisterMode) -> Self {
        let mut this = self;
        this.cfg_negative_register = cfg_negative_register;
        this
    }
    pub fn with_registers_mode(self, cfg_registers: RegistersMode) -> Self {
        let mut this = self;
        this.cfg_registers = cfg_registers;
        this
    }

    pub fn with_overflow_mode(self, cfg_overflow: OverflowMode) -> Self {
        let mut this = self;
        this.cfg_overflow = cfg_overflow;
        this
    }
    pub fn with_input_error_mode(self, cfg_input_error: InputErrorMode) -> Self {
        let mut this = self;
        this.cfg_input_error = cfg_input_error;
        this
    }
    pub fn is_finished(&self) -> bool {
        self.ip == self.instructions.len()
    }
    pub fn can_allocate_register(&self) -> bool {
        match self.cfg_registers {
            RegistersMode::Infinite => true,
            RegistersMode::Finite(max_register) => self.registers.len() < max_register,
        }
    }
    pub fn dump_state(&self) -> usize {
        println!();
        // TODO: line and column
        println!("IP={} (line {}, column {})", self.ip, '?', '?');
        print!("Registers: |");
        for (register, value) in self.registers.iter().enumerate() {
            if self.register == register {
                print!("*");
            }
            print!("{:02X}|", value);
        }
        println!();
        self.ip + 1
    }
    pub fn left(&mut self) -> Result<usize, BrainfuckError> {
        if self.register == 0 {
            match self.cfg_negative_register {
                NegativeRegisterMode::Allow => {
                    if self.can_allocate_register() {
                        self.registers.push_front(0); 
                    } else {
                        return Err(BrainfuckError::RegisterOverallocationError);
                    }
                }
                NegativeRegisterMode::Error => return Err(BrainfuckError::RegisterUnderflowError),
            }
        } else {
            self.register -= 1;
        }
        Ok(self.ip + 1)
    }
    pub fn right(&mut self) -> Result<usize, BrainfuckError> {
        self.register += 1;
        if self.register == self.registers.len() {
            if self.can_allocate_register() {
                self.registers.push_back(0);
            } else {
                self.register -= 1;
                return Err(BrainfuckError::RegisterOverallocationError);
            }
        }
        Ok(self.ip + 1)
    }
    pub fn add(&mut self) -> Result<usize, BrainfuckError> {
        let register = &mut self.registers[self.register];
        *register = if let Some(result) = register.checked_add(1) {
            result
        } else {
            match self.cfg_overflow {
                OverflowMode::Wrap => u8::min_value(),
                OverflowMode::Error => return Err(BrainfuckError::OverflowError),
            }
        };
        Ok(self.ip + 1)
    }
    pub fn sub(&mut self) -> Result<usize, BrainfuckError> {
        let register = &mut self.registers[self.register];
        *register = if let Some(result) = register.checked_sub(1) {
            result
        } else {
            match self.cfg_overflow {
                OverflowMode::Wrap => u8::max_value(),
                OverflowMode::Error => return Err(BrainfuckError::UnderflowError),
            }
        };
        Ok(self.ip + 1)
    }
    pub fn read(&mut self) -> Result<usize, BrainfuckError> {
        let register = &mut self.registers[self.register];
        let mut data = [0];
        *register = match self.input.read_exact(&mut data) {
            Ok(()) => data[0],
            Err(io_err) => match self.cfg_input_error {
                InputErrorMode::Value(value) => value,
                InputErrorMode::Unchanged => *register,
                InputErrorMode::Error => return Err(BrainfuckError::IOError(io_err)),
            },
        };
        Ok(self.ip + 1)
    }
    pub fn write(&mut self) -> Result<usize, BrainfuckError> {
        let register = self.registers[self.register];
        if let Err(io_err) = self.output.write_all(&[register]) {
            return Err(BrainfuckError::IOError(io_err));
        }
        Ok(self.ip + 1)
    }
    pub fn loop_begin(&mut self) -> Result<usize, BrainfuckError> {
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
        Ok(self.ip + 1)
    }
    pub fn loop_end(&mut self) -> Result<usize, BrainfuckError> {
        if let Some(ip) = self.dos.pop() {
            Ok(ip)
        } else {
            Err(BrainfuckError::MissingOpeningBraceError)
        }
    }
    pub fn next(&mut self) -> Result<bool, BrainfuckError> {
        if self.is_finished() {
            return Ok(true);
        }
        self.ip = match self.instructions[self.ip] {
            Instruction::Dump => self.dump_state(),
            Instruction::Right => self.right()?,
            Instruction::Left => self.left()?,
            Instruction::Add => self.add()?,
            Instruction::Sub => self.sub()?,
            Instruction::In => self.read()?,
            Instruction::Out => self.write()?,
            Instruction::Do => self.loop_begin()?,
            Instruction::While => self.loop_end()?,
        };
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
