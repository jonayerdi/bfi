use std::collections::{HashMap, VecDeque};
use std::io::{Read, Write};

/**
 * TODOS:
 *  - Refactor: interpret on the go (instructions out of State, run(instructions: &str), state.next(instruction))
 *  - ip, byte, line and column on error; newline sequence
 *  - Tests
 */

type DebugInfo = HashMap<usize, (usize, usize)>;

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

#[allow(dead_code)]
pub enum OutputErrorMode {
    Ignore,
    Error,
}

impl Default for OutputErrorMode {
    fn default() -> Self {
        OutputErrorMode::Ignore
    }
}

#[derive(Clone, Copy, PartialEq)]
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

#[allow(dead_code)]
pub fn decode(byte: u8, dump_state: bool) -> Option<Instruction> {
    if byte.is_ascii() {
        match byte as char {
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
    } else {
        None
    }
}

#[allow(dead_code)]
pub fn instructions<I>(iter: I) -> Vec<Instruction>
where
    I: Iterator<Item = u8>,
{
    iter.filter_map(|byte| decode(byte, false)).collect()
}

#[allow(dead_code)]
pub fn instructions_with_debug<I>(iter: I) -> (Vec<Instruction>, DebugInfo)
where
    I: Iterator<Item = u8>,
{
    let mut ip = 0;
    let mut line = 0;
    let mut column = 0;
    let mut debug_info = HashMap::new();
    let instructions = iter
        .filter_map(|byte| {
            let decoded = decode(byte, true);
            if let Some(instruction) = decoded {
                if instruction == Instruction::Dump {
                    debug_info.insert(ip, (line, column));
                }
                ip += 1;
            }
            if byte == '\n' as u8 {
                column = 0;
                line += 1;
            } else {
                column += 1;
            }
            decoded
        })
        .collect();
    (instructions, debug_info)
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
    cfg_output_error: OutputErrorMode,
    debug_info: DebugInfo,
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
            cfg_negative_register: Default::default(),
            cfg_registers: Default::default(),
            cfg_overflow: Default::default(),
            cfg_input_error: Default::default(),
            cfg_output_error: Default::default(),
            debug_info: Default::default(),
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
    pub fn with_output_error_mode(self, cfg_output_error: OutputErrorMode) -> Self {
        let mut this = self;
        this.cfg_output_error = cfg_output_error;
        this
    }
    pub fn with_debug_info(self, debug_info: DebugInfo) -> Self {
        let mut this = self;
        this.debug_info = debug_info;
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
        print!("IP={} ", self.ip);
        if let Some((line, column)) = self.debug_info.get(&self.ip) {
            println!("(line {}, column {})", line, column);
        } else {
            println!("(line ?, column ?)");
        }
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
            match self.cfg_output_error {
                OutputErrorMode::Ignore => {}
                OutputErrorMode::Error => return Err(BrainfuckError::IOError(io_err)),
            }
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
