use sexp::*;
use std::env;
use std::fs::File;
use std::io::prelude::*;

mod structs;
use structs::{Context, ContextMut};
mod parser;
use parser::*;
mod compiler;
use compiler::*;
mod repl;
use repl::*;

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    // Interactive REPL session
    if args[1] == "-i" {
        return Ok(repl(None));
    }

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    // Invalid error message for parsing errors
    let expr = parse_expr(&parse(&in_contents).expect("Invalid"));
    let result = compile_expr(&expr, &Context::new(), &mut ContextMut::new());

    let asm_program = format!(
        "section .text
extern snek_error
global our_code_starts_here
our_code_starts_here:
{}
ret
",
        instrs_to_string(&result)
    );

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;
    // repl(Some(&result));

    Ok(())
}
