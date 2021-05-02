#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Instruction {
    Dump, // Special instruction to dump state
    Right,
    Left,
    Add,
    Sub,
    In,
    Out,
    LoopBegin,
    LoopEnd,
}

pub type DebugInfo = Vec<(usize, usize)>;

pub fn decode_char(c: char, dump_state: bool) -> Option<Instruction> {
    match c {
        '>' => Some(Instruction::Right),
        '<' => Some(Instruction::Left),
        '+' => Some(Instruction::Add),
        '-' => Some(Instruction::Sub),
        ',' => Some(Instruction::In),
        '.' => Some(Instruction::Out),
        '[' => Some(Instruction::LoopBegin),
        ']' => Some(Instruction::LoopEnd),
        '#' if dump_state => Some(Instruction::Dump),
        _ => None,
    }
}

pub fn decode(byte: u8, dump_state: bool) -> Option<Instruction> {
    if byte.is_ascii() {
        decode_char(byte as char, dump_state)
    } else {
        None
    }
}

pub fn instructions<I>(iter: I, dump_state: bool) -> Vec<Instruction>
where
    I: Iterator<Item = u8>,
{
    iter.filter_map(|byte| decode(byte, dump_state)).collect()
}

pub fn instructions_with_debug<I>(iter: I, dump_state: bool) -> (Vec<Instruction>, DebugInfo)
where
    I: Iterator<Item = char>,
{
    let mut line = 1;
    let mut column = 1;
    let mut debug_info = Vec::new();
    let instructions = iter
        .filter_map(|c| {
            let decoded = decode_char(c, dump_state);
            if decoded.is_some() {
                debug_info.push((line, column));
            } else if c == '\n' {
                column = 1;
                line += 1;
            } else if c != '\r' {
                column += 1;
            }
            decoded
        })
        .collect();
    (instructions, debug_info)
}
