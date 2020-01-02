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
 *  - Select input/output files
 *  - DebugInfo
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
    let file = match std::fs::File::open(&args[1]) {
        Ok(file) => file,
        Err(e) => {
            error!("Error opening {}: {}", &args[1], e);
        }
    };
    let (instructions, debug_info) =
        brainfuck::instructions_with_debug(file.bytes().map(|b| match b {
            Ok(byte) => byte,
            Err(e) => error!("IO error reading {}: {}", &args[1], e),
        }));
    let input = std::io::stdin();
    let output = std::io::stdout();
    let state = brainfuck::State::new(&instructions, input, output).with_debug_info(debug_info);
    if let Err(error) = state.run() {
        println!();
        error!("Runtime error: {}", error);
    }
    println!();
}
