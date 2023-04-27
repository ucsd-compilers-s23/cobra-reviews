use dynasmrt::{dynasm, DynasmApi};
use im::hashmap;
use sexp::*;
use std::io::Write;
use std::mem;
use std::panic;

use crate::compiler::*;
use crate::parser::*;
use crate::structs::*;

fn eval(instrs: &Vec<Instr>) -> i32 {
    let mut ops = dynasmrt::x64::Assembler::new().unwrap();
    let start = ops.offset();
    instrs_to_asm(&instrs, &mut ops);
    // println!("Instrs: {:?}", instrs);
    dynasm!(ops; .arch x64; ret);
    let buf = ops.finalize().unwrap();
    let jitted_fn: extern "C" fn() -> i32 = unsafe { mem::transmute(buf.ptr(start)) };
    jitted_fn()
}

pub fn repl(res: Option<&Vec<Instr>>) {
    if let Some(instrs) = res {
        println!("{}", eval(instrs));
    } else {
        // REPL
        let mut line = String::new();
        // let mut env = hashmap! {};
        let mut content = hashmap! {};
        // let mut si = 2;
        let mut ce = Context::new();
        let cem = ContextMut::new();
        loop {
            line.clear();
            print!("> ");
            _ = std::io::stdout().flush();
            _ = std::io::stdin().read_line(&mut line);

            // Check empty input (Ctrl+D)
            if line == "" {
                break;
            }

            line = line.trim().to_string();

            // Check empty input
            if line == "" {
                continue;
            }

            // Add top level list
            if line.starts_with("let")
                || line.starts_with("define")
                || line.starts_with("add1")
                || line.starts_with("sub1")
                || line.starts_with("+")
                || line.starts_with("*")
                || line.starts_with("-")
            {
                line = format!("({line})");
            }

            // Parse and Compile, check for panic
            let res = panic::catch_unwind(|| {
                let expr = parse_expr(match &parse(&line) {
                    Ok(r) => r,
                    Err(e) => {
                        panic!("Error parsing input: {e}")
                    }
                });
                if let Expr::Define(x, e) = expr {
                    (Some(x), compile_expr(&e, &ce, &mut cem.clone()))
                } else {
                    (None, compile_expr(&expr, &ce, &mut cem.clone()))
                }
            });

            // Eval with dynasm
            match res {
                Ok((var, mut instrs)) => {
                    for (x, xsi) in &ce.env {
                        let xval = *content.get(x).unwrap();
                        instrs.insert(
                            0,
                            Instr::Mov(Args::ToMem(
                                MemRef {
                                    reg: Reg::Rsp,
                                    offset: xsi.offset,
                                },
                                Arg::Imm(xval),
                            )),
                        );
                    }

                    // println!("Instrs: {:?}", instrs);

                    let result = eval(&instrs);
                    if let Some(x) = &var {
                        if !ce.env.contains_key(x) {
                            ce.env.insert(x.to_string(), VarEnv::new(ce.si));
                            ce.si += 1;
                        }
                        content.insert(x.to_string(), result);
                    }
                    println!("{}", result)
                }
                _ => {}
            };
        }
    }
}
