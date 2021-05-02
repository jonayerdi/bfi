mod brainfuck;

use std::io::Read;

macro_rules! error {
    ($($arg:tt)*) => ({
        eprintln!($($arg)*);
        std::process::exit(1);
    })
}

/**
 * TODOS:
 *  - Tests
 *  - Commandline
 *  - Select input/output files, stdio
 *  - DebugInfo
        - Compact (line lengths)
        - Remote Debugger Protocol?
 *  - Config
 *      - NegativeRegisterMode
 *      - RegistersMode
 *      - OverflowMode
 *      - InputErrorMode
 */

fn main() {
    let args = std::env::args().collect::<Vec<_>>();
    if args.len() != 2 {
        error!("Usage: bfi <FILE>");
    }
    let mut file = match std::fs::File::open(&args[1]) {
        Ok(file) => file,
        Err(e) => {
            error!("Error opening {}: {}", &args[1], e);
        }
    };
    let mut code = String::new();
    if let Err(io_err) = file.read_to_string(&mut code) {
        error!("IO error reading {}: {}", &args[1], io_err);
    }
    let (instructions, debug_info) = brainfuck::instructions_with_debug(code.chars(), true);
    let input = std::io::stdin();
    let output = std::io::stdout();
    let state = brainfuck::State::new(&instructions, input, output).with_debug_info(debug_info);
    if let Err(error) = state.run() {
        println!();
        error!("Runtime error: {}", error);
    }
    println!();
}
