mod enums;
mod parser;
mod compiler;
mod utils;

use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::*;

use crate::parser::*;
use crate::compiler::*;

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    // read input file
    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    // parse input file to expr
    let s_expr = parse(&in_contents).expect("parse error: Invalid sexp");
    let expr = parse_expr(&s_expr);

    // Instructions
    let instrs = compile_expr(&expr);
    
    // Assembly code
    let result = convert_enums_to_assembly(&instrs);

    let asm_program = format!(
        "
section .text
extern snek_error
global our_code_starts_here
overflow_error:
  mov rdi, 1
  push rsp
  call snek_error
invalid_type_error:
  mov rdi, 2
  push rsp
  call snek_error
our_code_starts_here:
  {}
  ret
",
        result
    );

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}
