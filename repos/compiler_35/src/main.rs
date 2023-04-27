use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::parse;

use crate::compile::compile_expr;
use crate::instr::instr_to_string;
use crate::parser::parse_s_expr;

mod expr;
mod instr;
mod parser;
mod compile;

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();


    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;
    
    let s_exp = parse(&in_contents).expect("Invalid s-exp");
    let mut label_counter = 1;
    let expr = compile_expr(&parse_s_expr(&s_exp), 2, &im::HashMap::<String, i32>::new(), &mut label_counter, None);

    println!("Done compiling");

    // You will make result hold the result of actually compiling
    let result: Vec<String> = expr.iter().map(|instr| instr_to_string(instr)).collect();
    let result_str = result.join("");

    let asm_program = format!(
        "
section .text
extern snek_error
global our_code_starts_here

throw_type_error:
      mov rdi, 1
      push rsp
      call snek_error 

throw_overflow_error:
      mov rdi, 2
      push rsp
      call snek_error 

our_code_starts_here:
  {}
  ret
",
        result_str
    );

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}
