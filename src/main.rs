mod compiler;
mod files;
mod math;

use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut settings = (false, false);
    for arg in args {
        if arg == "c" {
            settings.0 = true;
            continue;
        }

        if arg == "r" {
            settings.1 = true;
        }
    }

    if settings.0 {
        println!("Compiling...\r\n");
        let _ = compiler::compile(&"examples/test.ms".to_string());
    }

    if settings.1 {
        println!("Running...\r\n");

        let _ = files::run_program(&"examples/test.mmm".to_string());
    }
}
