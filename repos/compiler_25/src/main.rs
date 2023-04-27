use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::*;

// Modules
mod abstract_syntax;
mod compiler;
mod instructions;
mod parser;

use compiler::compile_to_instrs;
use compiler::get_error_instr_code;
use instructions::instr_to_str;
use parser::parse_sexpr;

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let parsed_sexpr = parse(&in_contents);
    let sexpr = match parsed_sexpr {
        Ok(sexpr) => sexpr,
        Err(_) => {
            panic!("Invalid S-expression format");
        }
    };

    let expr = parse_sexpr(&sexpr);
    // println!("Parsed expression:\n{expr:?}");

    let error_code = get_error_instr_code();

    let instrs = compile_to_instrs(&expr);
    let str_instrs: Vec<String> = instrs.iter().map(instr_to_str).collect();
    let compiled_code = str_instrs.join("\n\t\t");

    let asm_program = format!(
        "
    section .text
    global our_code_starts_here
    extern snek_error
    our_code_starts_here:
    {compiled_code}
    {error_code}
    "
    );

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}
