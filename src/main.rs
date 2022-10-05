mod files;
mod compiler;

fn main() {
    let _ = compiler::compile(&"examples/test.ms".to_string());
    let _ = files::run_program(&"examples/test.mmm".to_string());
}