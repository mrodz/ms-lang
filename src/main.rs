mod compiler;
mod files;
mod math;
mod bools;

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
        println!("Compiling...");
        let _ = compiler::compile(&"examples/test.ms".to_string());
    }

    if settings.1 {
        println!("Running...");

        let _ = files::run_program(&"examples/test.mmm".to_string());
    }

    if !settings.0 && !settings.1 {
        let _ = files::run_program(&"examples/compiled/program.mmm".to_string());
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn split_string() {
        let message = "DIM $x, \"aa\", \"bb\", \"cc\", \"dd\"";
        let res = crate::files::split_string(&message.to_string()).unwrap();

        assert_eq!(res, vec!["DIM", "$x", "aa", "bb", "cc", "dd"]);
    }
}
