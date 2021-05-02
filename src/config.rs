pub struct Config {
    pub negative_register: NegativeRegisterMode,
    pub registers: RegistersMode,
    pub overflow: OverflowMode,
    pub input_error: InputErrorMode,
    pub output_error: OutputErrorMode,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            negative_register: Default::default(),
            registers: Default::default(),
            overflow: Default::default(),
            input_error: Default::default(),
            output_error: Default::default(),
        }
    }
}

pub enum NegativeRegisterMode {
    Allow,
    Error,
}

impl Default for NegativeRegisterMode {
    fn default() -> Self {
        NegativeRegisterMode::Allow
    }
}

pub enum RegistersMode {
    Infinite,
    Finite(usize),
}

impl Default for RegistersMode {
    fn default() -> Self {
        RegistersMode::Infinite
    }
}

pub enum OverflowMode {
    Wrap,
    Error,
}

impl Default for OverflowMode {
    fn default() -> Self {
        OverflowMode::Wrap
    }
}

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

pub enum OutputErrorMode {
    Ignore,
    Error,
}

impl Default for OutputErrorMode {
    fn default() -> Self {
        OutputErrorMode::Ignore
    }
}
