mod parser;
mod compiler;

use parser::parse_string;
use compiler::compile;
use std::env;
use std::fs::File;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    // Parse arguments and read input file
    let args: Vec<String> = env::args().collect();
    let in_name = &args[1];
    let out_name = &args[2];
    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    // Compile and format assembly instructions
    let result = compile(&parse_string(&in_contents));
    let asm_program = format!(
        "
section .text
extern snek_error
global our_code_starts_here
throw_overflow_error:
  mov rdi, 2
  push rsp
  call snek_error
  ret
throw_type_error:
  mov rdi, 1
  push rsp
  call snek_error
  ret
our_code_starts_here:
  {}
  ret
",
        result
    );

    // Write assembly instructions to output file
    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}
