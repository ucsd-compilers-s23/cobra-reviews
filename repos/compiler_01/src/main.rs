use std::env;
use std::fs::File;
use std::io::prelude::*;

use cobra::parser;
use cobra::compiler;

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let expr = parser::parse_str(&in_contents);
    let asm_program = compiler::compile(&expr);

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}
