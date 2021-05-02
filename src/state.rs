use super::config::{self, Config};
use super::decode::{DebugInfo, Instruction};
use std::collections::VecDeque;
use std::io::{Read, Write};

/**
 * TODOS:
 *  - Tests
 */

#[derive(Debug)]
pub enum ErrorType {
    Input(std::io::Error),
    Output(std::io::Error),
    RegisterUnderflow,
    RegisterOverallocation,
    ArithmeticOverflow,
    ArithmeticUnderflow,
    MissingClosingBrace,
    MissingOpeningBrace,
}

impl std::fmt::Display for ErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ErrorType::Input(io_err) => write!(f, "Input error: {}", io_err),
            ErrorType::Output(io_err) => write!(f, "Output error: {}", io_err),
            ErrorType::RegisterUnderflow => write!(f, "Register underflow on left shift"),
            ErrorType::RegisterOverallocation => write!(f, "Too many registers allocated"),
            ErrorType::ArithmeticOverflow => write!(f, "Addition overflow"),
            ErrorType::ArithmeticUnderflow => write!(f, "Substraction underflow"),
            ErrorType::MissingClosingBrace => write!(f, "Found [ with no closing ]"),
            ErrorType::MissingOpeningBrace => write!(f, "Found ] with no opening ["),
        }
    }
}

#[derive(Debug)]
pub struct Error {
    error_type: ErrorType,
    ip: usize,
    debug_info: Option<(usize, usize)>,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if let Some((line, column)) = self.debug_info {
            write!(
                f,
                "{} at IP={} (line {}, column {})",
                self.error_type, self.ip, line, column
            )
        } else {
            write!(f, "{} at IP={}", self.error_type, self.ip)
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match &self.error_type {
            ErrorType::Input(io_err) => Some(io_err),
            ErrorType::Output(io_err) => Some(io_err),
            _ => None,
        }
    }
}

pub struct State<'a, R, W>
where
    R: Read,
    W: Write,
{
    cfg: Config,
    debug_info: DebugInfo,
    instructions: &'a [Instruction],
    ip: usize,
    register: usize,
    loop_beginnings: Vec<usize>,
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
    fn error(&self, error_type: ErrorType) -> Error {
        Error {
            error_type,
            ip: self.ip,
            debug_info: match self.debug_info.get(self.ip) {
                Some(value) => Some(*value),
                None => None,
            },
        }
    }
    pub fn new(instructions: &'a [Instruction], input: R, output: W) -> Self {
        State {
            cfg: Default::default(),
            debug_info: Default::default(),
            instructions,
            ip: 0,
            register: 0,
            loop_beginnings: Vec::with_capacity(4),
            registers: VecDeque::from(vec![0]),
            input,
            output,
        }
    }
    pub fn with_negative_register_mode(
        mut self,
        negative_register: config::NegativeRegisterMode,
    ) -> Self {
        self.cfg.negative_register = negative_register;
        self
    }
    pub fn with_registers_mode(mut self, registers: config::RegistersMode) -> Self {
        self.cfg.registers = registers;
        self
    }

    pub fn with_overflow_mode(mut self, overflow: config::OverflowMode) -> Self {
        self.cfg.overflow = overflow;
        self
    }
    pub fn with_input_error_mode(mut self, input_error: config::InputErrorMode) -> Self {
        self.cfg.input_error = input_error;
        self
    }
    pub fn with_output_error_mode(mut self, output_error: config::OutputErrorMode) -> Self {
        self.cfg.output_error = output_error;
        self
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
        match self.cfg.registers {
            config::RegistersMode::Infinite => true,
            config::RegistersMode::Finite(max_register) => self.registers.len() < max_register,
        }
    }
    pub fn dump_state(&mut self) {
        println!();
        print!("IP={}", self.ip);
        if let Some((line, column)) = self.debug_info.get(self.ip) {
            println!(" (line {}, column {})", line, column);
        }
        print!("Registers: |");
        for (register, value) in self.registers.iter().enumerate() {
            if self.register == register {
                print!("*");
            }
            print!("{:02X}|", value);
        }
        println!();
        self.ip += 1
    }
    pub fn left(&mut self) -> Result<(), Error> {
        if self.register == 0 {
            match self.cfg.negative_register {
                config::NegativeRegisterMode::Allow => {
                    if self.can_allocate_register() {
                        self.registers.push_front(0);
                    } else {
                        return Err(self.error(ErrorType::RegisterOverallocation));
                    }
                }
                config::NegativeRegisterMode::Error => {
                    return Err(self.error(ErrorType::RegisterUnderflow))
                }
            }
        } else {
            self.register -= 1;
        }
        self.ip += 1;
        Ok(())
    }
    pub fn right(&mut self) -> Result<(), Error> {
        self.register += 1;
        if self.register == self.registers.len() {
            if self.can_allocate_register() {
                self.registers.push_back(0);
            } else {
                self.register -= 1;
                return Err(self.error(ErrorType::RegisterOverallocation));
            }
        }
        self.ip += 1;
        Ok(())
    }
    pub fn add(&mut self) -> Result<(), Error> {
        let register = &mut self.registers[self.register];
        *register = if let Some(result) = register.checked_add(1) {
            result
        } else {
            match self.cfg.overflow {
                config::OverflowMode::Wrap => u8::min_value(),
                config::OverflowMode::Error => {
                    return Err(self.error(ErrorType::ArithmeticOverflow))
                }
            }
        };
        self.ip += 1;
        Ok(())
    }
    pub fn sub(&mut self) -> Result<(), Error> {
        let register = &mut self.registers[self.register];
        *register = if let Some(result) = register.checked_sub(1) {
            result
        } else {
            match self.cfg.overflow {
                config::OverflowMode::Wrap => u8::max_value(),
                config::OverflowMode::Error => {
                    return Err(self.error(ErrorType::ArithmeticUnderflow))
                }
            }
        };
        self.ip += 1;
        Ok(())
    }
    pub fn read(&mut self) -> Result<(), Error> {
        let register = &mut self.registers[self.register];
        let mut data = [0];
        *register = match self.input.read_exact(&mut data) {
            Ok(()) => data[0],
            Err(io_err) => match self.cfg.input_error {
                config::InputErrorMode::Value(value) => value,
                config::InputErrorMode::Unchanged => *register,
                config::InputErrorMode::Error => return Err(self.error(ErrorType::Input(io_err))),
            },
        };
        self.ip += 1;
        Ok(())
    }
    pub fn write(&mut self) -> Result<(), Error> {
        let register = self.registers[self.register];
        if let Err(io_err) = self.output.write_all(&[register]) {
            match self.cfg.output_error {
                config::OutputErrorMode::Ignore => {}
                config::OutputErrorMode::Error => return Err(self.error(ErrorType::Output(io_err))),
            }
        }
        self.ip += 1;
        Ok(())
    }
    pub fn loop_begin(&mut self) -> Result<(), Error> {
        if self.registers[self.register] != 0 {
            // Enter loop
            self.loop_beginnings.push(self.ip);
            self.ip += 1;
            Ok(())
        } else {
            // Skip to loop end
            let mut loop_begins = 1;
            for ip in self.ip + 1..self.instructions.len() {
                match self.instructions[ip] {
                    Instruction::LoopBegin => loop_begins += 1,
                    Instruction::LoopEnd => {
                        loop_begins -= 1;
                        if loop_begins == 0 {
                            self.ip = ip + 1;
                            return Ok(());
                        }
                    }
                    _ => {}
                };
            }
            Err(self.error(ErrorType::MissingClosingBrace))
        }
    }
    pub fn loop_end(&mut self) -> Result<(), Error> {
        if let Some(loop_begin_ip) = self.loop_beginnings.pop() {
            self.ip = loop_begin_ip;
            Ok(())
        } else {
            Err(self.error(ErrorType::MissingOpeningBrace))
        }
    }
    pub fn next(&mut self) -> Result<bool, Error> {
        if self.is_finished() {
            return Ok(true);
        }
        match self.instructions[self.ip] {
            Instruction::Dump => self.dump_state(),
            Instruction::Right => self.right()?,
            Instruction::Left => self.left()?,
            Instruction::Add => self.add()?,
            Instruction::Sub => self.sub()?,
            Instruction::In => self.read()?,
            Instruction::Out => self.write()?,
            Instruction::LoopBegin => self.loop_begin()?,
            Instruction::LoopEnd => self.loop_end()?,
        };
        if self.is_finished() {
            if self.loop_beginnings.is_empty() {
                Ok(true)
            } else {
                Err(self.error(ErrorType::MissingClosingBrace))
            }
        } else {
            Ok(false)
        }
    }
    pub fn run(mut self) -> Result<(), Error> {
        while !self.next()? {}
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::super::decode;
    use super::*;

    fn run_program(
        code: &str,
        input: &str,
        dump_state: bool,
        debug_info: bool,
    ) -> Result<Vec<u8>, Error> {
        let mut output_data = Vec::new();
        let output = std::io::Cursor::new(&mut output_data);
        let (instructions, debug_info) = if debug_info {
            decode::instructions_with_debug(code.chars(), dump_state)
        } else {
            (
                decode::instructions(code.bytes(), dump_state),
                Default::default(),
            )
        };
        let state = State::new(&instructions, input.as_bytes(), output).with_debug_info(debug_info);
        match state.run() {
            Ok(()) => Ok(output_data),
            Err(error) => Err(error),
        }
    }

    #[test]
    fn helloworld_single_register() {
        let text = "hello world!";
        let mut code = String::new();
        let mut register = 0;
        for &byte in text.as_bytes() {
            let diff = byte as i16 - register as i16;
            if diff >= 0 {
                (0..diff).for_each(|_| code.push('+'));
            } else {
                (0..-diff).for_each(|_| code.push('-'));
            }
            code.push('.');
            register = byte;
        }
        match run_program(&code, "", false, false) {
            Ok(output) => assert_eq!(text.as_bytes(), output.as_slice()),
            Err(error) => panic!("{}", error),
        }
    }

    #[test]
    fn helloworld_multiregister() {
        let text = "hello world!";
        let mut code = String::new();
        for &byte in text.as_bytes() {
            (0..byte).for_each(|_| code.push('+'));
            code.push_str(".>");
        }
        match run_program(&code, "", false, false) {
            Ok(output) => assert_eq!(text.as_bytes(), output.as_slice()),
            Err(error) => panic!("{}", error),
        }
    }

    #[test]
    fn echo() {
        let code = ",[.,]";
        let inputs = [
            "hello",
            "-> hello world! <-",
            "",
            "123\n456",
            "123\n\r456",
            "سلام دنیا",
            "안녕 세상",
            "\r\n\thél\\lo",
        ];
        for input in inputs.iter() {
            match run_program(code, *input, false, false) {
                Ok(output) => assert_eq!(input.as_bytes(), output.as_slice()),
                Err(error) => panic!("{}", error),
            }
        }
    }

    #[test]
    fn reverse() {
        let code = ">,[>,]<[.<]";
        let inputs = [
            "hello",
            "-> hello world! <-",
            "",
            "123\n456",
            "123\n\r456",
            "سلام دنیا",
            "안녕 세상",
            "\r\n\thél\\lo",
        ];
        for input in inputs.iter() {
            match run_program(code, *input, false, false) {
                Ok(output) => assert_eq!(
                    input.as_bytes().iter().collect::<Vec<_>>(),
                    output.iter().rev().collect::<Vec<_>>()
                ),
                Err(error) => panic!("{}", error),
            }
        }
    }

    #[test]
    fn qsort() {
        // TODO
    }

    #[test]
    fn dump_state() {
        // TODO
    }

    #[test]
    fn no_dump_state() {
        // TODO
    }

    #[test]
    fn no_debug_info() {
        // TODO
    }

    #[test]
    fn debug_info_lf() {
        // TODO
    }

    #[test]
    fn debug_info_crlf() {
        // TODO
    }

    #[test]
    fn input_error() {
        // TODO
    }

    #[test]
    fn output_error() {
        // TODO
    }

    #[test]
    fn register_underflow_error() {
        // TODO
    }

    #[test]
    fn register_overallocation_error() {
        // TODO
    }

    #[test]
    fn arithmetic_overflow_error() {
        // TODO
    }

    #[test]
    fn arithmetic_underflow_error() {
        // TODO
    }

    #[test]
    fn missing_closing_brace_error() {
        // TODO
    }

    #[test]
    fn missing_opening_brace_error() {
        // TODO
    }
}
