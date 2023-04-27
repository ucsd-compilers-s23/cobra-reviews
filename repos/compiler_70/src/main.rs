use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::Atom::*;
use sexp::*;
use std::collections::HashMap;

// Additional dependencies
use regex::Regex;
use std::panic;

#[derive(Debug)]
enum Op1 {
    Add1,
    Sub1,
    IsNum,
    IsBool
}

#[derive(Debug)]
enum Op2 {
    Plus,
    Minus,
    Times,
    Equal, 
    Greater, 
    GreaterEqual, 
    Less,
    LessEqual
}

#[derive(Debug)]
enum Expr {
    // Prev types
    Number(i64), 
    Id(String), 
    UnOp(Op1, Box<Expr>), 
    BinOp(Op2, Box<Expr>, Box<Expr>),
    Let(Vec<(String, Expr)>, Box<Expr>), 
    // New types
    Boolean(bool), 
    If(Box<Expr>, Box<Expr>, Box<Expr>) ,
    Loop(Box<Expr>), 
    Break(Box<Expr>), 
    Set(String, Box<Expr>),
    Block(Vec<Expr>),
}

fn new_label(lbl: &mut i32, name: &str) -> String{
    let current = *lbl;
    *lbl += 1;
    format!("{name}_{current}")
}

fn parse_vec(vec: &Sexp) -> (String, Expr) {
    match vec {
        Sexp::List(v) => {
            match &v[..] {
                [Sexp::Atom(S(op)), e] => {
                    let re = Regex::new(r"[a-zA-z][a-zA-Z0-9]*").unwrap();
                    if !vec!["add1", "sub1", "isnum", "isbool", "+", "-", "*", "=", ">", ">=", "<", "<=", "let", "if", "block", "-", "break", "set!", "loop", "true", "false", "input"].iter().any(|elem| op.contains(elem)) && re.is_match(&op){
                        (op.to_string(), parse_expr(e))
                    } else{
                        panic!("Invalid Identifier {op} - Identifier cannot be a keyword and must match pattern [a-zA-z][a-zA-Z0-9]*");
                    }
                },
                _ => panic!("Invalid Code Input - Error in Parsing Code"),
            }
        },
        _ => panic!("Invalid Code Input - Error in Parsing Code"),
    }
}

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        // Number
        Sexp::Atom(I(n)) => {
            match i64::try_from(*n) {
                Ok(num) => {
                    if num >= -4611686018427387904 && num <= 4611686018427387903 {
                        Expr::Number(num)
                    } else {
                        panic!("Invalid")
                    }
                }
                Err(..) => {
                    panic!("Invalid")
                }
            }
        },
        // Id and Boolean
        Sexp::Atom(S(n)) => {
          // Check if Id is not any of the keywords and follows the regex pattern
          let id = String::try_from(n).unwrap();
          let re = Regex::new(r"[a-zA-z][a-zA-Z0-9]*").unwrap();
          if !vec!["add1", "sub1", "isnum", "isbool", "+", "-", "*", "=", ">", ">=", "<", "<=", "let", "if", "block", "-", "break", "set!", "loop"].iter().any(|elem| id.contains(elem)) && re.is_match(&id){
            if id == "true" || id == "false"{
              Expr::Boolean(id.parse().unwrap())
            } else {
              Expr::Id(id)
            }
          } else{
              panic!("Invalid Identifier {id} - Identifier cannot be a keyword and must match pattern [a-zA-z][a-zA-Z0-9]*");
          }
        },
        // UnOp, BinOp, Let, If
        Sexp::List(vec) => {
            match &vec[..] {
                // UnOp
                [Sexp::Atom(S(op)), e] if op == "add1" => Expr::UnOp(Op1::Add1, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "sub1" => Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "isnum" => Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "isbool" => Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e))),
                //BinOp
                [Sexp::Atom(S(op)), e1, e2] if op == "+" => Expr::BinOp(Op2::Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "-" => Expr::BinOp(Op2::Minus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "*" => Expr::BinOp(Op2::Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "=" => Expr::BinOp(Op2::Equal, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == ">" => Expr::BinOp(Op2::Greater, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == ">=" => Expr::BinOp(Op2::GreaterEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "<" => Expr::BinOp(Op2::Less, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "<=" => Expr::BinOp(Op2::LessEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                //Let
                [Sexp::Atom(S(op)), bind, body] if op == "let" => {
                    // Iterate through vector and parse bindings
                    let mut vbind = Vec::new();
                    match &bind {
                        Sexp::List(vec) => {
                            for binding in vec {
                                vbind.push(parse_vec(binding));
                            }
                            // If no bindings found, panic with error. Else continue with parsing
                            if vbind.len() > 0{
                                Expr::Let(vbind, Box::new(parse_expr(body)))
                            } else{
                                panic!("Invalid Code Input - Error in Parsing Code (let must have bindings)");
                            }
                        },
                        _ => panic!("Invalid Code Input - Error in Parsing Code (Invalid binding format)"),
                    }
                },
                // If
                [Sexp::Atom(S(op)), cond, then, els] if op == "if" => Expr::If(Box::new(parse_expr(cond)), Box::new(parse_expr(then)), Box::new(parse_expr(els))),
                // Loop
                [Sexp::Atom(S(op)), body] if op == "loop" => Expr::Loop(Box::new(parse_expr(body))),
                // Break
                [Sexp::Atom(S(op)), lbl] if op == "break" => Expr::Break(Box::new(parse_expr(lbl))),
                // Set!
                [Sexp::Atom(S(op)), var, exp] if op == "set!" => Expr::Set(var.to_string(), Box::new(parse_expr(exp))),
                // Block
                [Sexp::Atom(S(op)), ..] if op == "block" => {
                    // Iterate through vector and parse expressions
                    let mut vexpr = Vec::new();
                    let mut i = 1;
                    while i < vec.len() {
                        vexpr.push(parse_expr(&vec[i]));
                        i += 1
                    }
                    // If no expression found, panic with error. Else continue with parsing
                    if vexpr.len() > 0{
                        Expr::Block(vexpr)
                    } else{
                        panic!("Invalid Code Input - Error in Parsing Code (Block has no expressions)");
                    }
                }
                _ => panic!("Invalid Code Input - Error in Parsing Code (No match found)"),
            }
        },
        _ => panic!("Invalid Code Input - Error in Parsing Code (Invalid code)"),
    }
}

fn compile_expr(e: &Expr, si: i32, venv: &mut HashMap::<String, i32>, lbl: &mut i32, break_lbl: &str) -> String {
    match e {
        // Number (left shifted by 1 as per snek definition)
        Expr::Number(n) => format!("mov rax, {}", *n << 1),
        // Add1 (adding 2 if subexpr is a number, since everything is left shifted by 1. 1 << 1 = 2)
        Expr::UnOp(Op1::Add1, subexpr) => {
            let expr = compile_expr(subexpr, si, venv, lbl, break_lbl);
            format!("
                {expr}
                test rax, 1
                jnz op_err
                add rax, 2
                mov rbx, -4611686018427387904
                cmp rax, rbx
                jle overflow_err
                mov rbx, 4611686018427387903
                cmp rax, rbx
                jge overflow_err
            ")
        },
        // Sub1 (subtracting 2 if subexpr is a number, since everything is left shifted by 1. 1 << 1 = 2)
        Expr::UnOp(Op1::Sub1, subexpr) => {
            let expr = compile_expr(subexpr, si, venv, lbl, break_lbl);
            format!("
                {expr}
                test rax, 1
                jnz op_err
                sub rax, 2
                mov rbx, -4611686018427387904
                cmp rax, rbx
                jle overflow_err
                mov rbx, 4611686018427387903
                cmp rax, rbx
                jge overflow_err
            ")
        },
        // IsNum (check if expr is a number)
        Expr::UnOp(Op1::IsNum, subexpr) => {
            let expr = compile_expr(subexpr, si, venv, lbl, break_lbl);
            format!("
                {expr}
                mov rbx, rax
                and rbx, 1
                cmp rbx, 1
                mov rbx, 1
                mov rax, 3
                cmove rax, rbx
            ")
        },
        // IsBool (check if expr is a boolean)
        Expr::UnOp(Op1::IsBool, subexpr) => {
            let expr = compile_expr(subexpr, si, venv, lbl, break_lbl);
            format!("
                {expr}
                mov rbx, rax
                and rbx, 1
                cmp rbx, 1
                mov rbx, 3
                mov rax, 1
                cmove rax, rbx
            ")
        },
        // Plus
        Expr::BinOp(Op2::Plus, e1, e2) => {
            let e1_instrs = compile_expr(e1, si, venv, lbl, break_lbl);
            let e2_instrs = compile_expr(e2, si + 1, venv, lbl, break_lbl);
            let offset = si * 8;
            format!("              
                {e1_instrs}
                mov [rsp - {offset}], rax
                {e2_instrs}
                mov rbx, rax
                or rbx, [rsp - {offset}]
                not rbx
                test rbx, 1
                je op_err
                add rax, [rsp - {offset}]
                mov rbx, -4611686018427387904
                cmp rax, rbx
                jle overflow_err
                mov rbx, 4611686018427387903
                cmp rax, rbx
                jge overflow_err
            ")
        },
        // Minus
        Expr::BinOp(Op2::Minus, e1, e2) => {
            let e1_instrs = compile_expr(e1, si, venv, lbl, break_lbl);
            let e2_instrs = compile_expr(e2, si + 1, venv, lbl, break_lbl);
            let offset = si * 8;
            format!("
                {e1_instrs}
                mov [rsp - {offset}], rax
                {e2_instrs}
                mov rbx, rax
                or rbx, [rsp - {offset}]
                not rbx
                test rbx, 1
                je op_err
                sub [rsp - {offset}], rax
                mov rax, [rsp - {offset}]
                mov rbx, -4611686018427387904
                cmp rax, rbx
                jle overflow_err
                mov rbx, 4611686018427387903
                cmp rax, rbx
                jge overflow_err
            ")
        },
        // Times
        Expr::BinOp(Op2::Times, e1, e2) => {
            let e1_instrs = compile_expr(e1, si, venv, lbl, break_lbl);
            let e2_instrs = compile_expr(e2, si + 1, venv, lbl, break_lbl);
            let offset = si * 8;
            format!("
                {e1_instrs}
                mov [rsp - {offset}], rax
                {e2_instrs}
                mov rbx, rax
                or rbx, [rsp - {offset}]
                not rbx
                test rbx, 1
                je op_err
                imul rax, [rsp - {offset}]
                cqo
                mov rbx, 2
                idiv rbx
                mov rbx, -4611686018427387904
                cmp rax, rbx
                jle overflow_err
                mov rbx, 4611686018427387903
                cmp rax, rbx
                jge overflow_err
            ")
        },
        // Equal
        Expr::BinOp(Op2::Equal, e1, e2) => {
            let e1_instrs = compile_expr(e1, si, venv, lbl, break_lbl);
            let e2_instrs = compile_expr(e2, si + 1, venv, lbl, break_lbl);
            let offset = si * 8;
            format!("
                {e1_instrs}
                mov [rsp - {offset}], rax
                {e2_instrs}
                mov rbx, rax
                xor rbx, [rsp - {offset}]
                test rbx, 1
                jne op_err
                cmp rax, [rsp - {offset}]
                mov rbx, 3
                mov rax, 1
                cmove rax, rbx
            ")
        },
        // Greater
        Expr::BinOp(Op2::Greater, e1, e2) => {
            let e1_instrs = compile_expr(e1, si, venv, lbl, break_lbl);
            let e2_instrs = compile_expr(e2, si + 1, venv, lbl, break_lbl);
            let offset = si * 8;
            format!("
                {e1_instrs}
                mov [rsp - {offset}], rax
                {e2_instrs}
                mov rbx, rax
                xor rbx, [rsp - {offset}]
                test rbx, 1
                jne op_err
                cmp [rsp - {offset}], rax
                mov rbx, 3
                mov rax, 1
                cmovg rax, rbx
            ")
        },
        // GreaterEqual
        Expr::BinOp(Op2::GreaterEqual, e1, e2) => {
            let e1_instrs = compile_expr(e1, si, venv, lbl, break_lbl);
            let e2_instrs = compile_expr(e2, si + 1, venv, lbl, break_lbl);
            let offset = si * 8;
            format!("
                {e1_instrs}
                mov [rsp - {offset}], rax
                {e2_instrs}
                mov rbx, rax
                xor rbx, [rsp - {offset}]
                test rbx, 1
                jne op_err
                cmp [rsp - {offset}], rax
                mov rbx, 3
                mov rax, 1
                cmovge rax, rbx
            ")
        },
        // Less
        Expr::BinOp(Op2::Less, e1, e2) => {
            let e1_instrs = compile_expr(e1, si, venv, lbl, break_lbl);
            let e2_instrs = compile_expr(e2, si + 1, venv, lbl, break_lbl);
            let offset = si * 8;
            format!("
                {e1_instrs}
                mov [rsp - {offset}], rax
                {e2_instrs}
                mov rbx, rax
                xor rbx, [rsp - {offset}]
                test rbx, 1
                jne op_err
                cmp [rsp - {offset}], rax
                mov rbx, 3
                mov rax, 1
                cmovl rax, rbx
            ")
        },
        // LessEqual
        Expr::BinOp(Op2::LessEqual, e1, e2) => {
            let e1_instrs = compile_expr(e1, si, venv, lbl, break_lbl);
            let e2_instrs = compile_expr(e2, si + 1, venv, lbl, break_lbl);
            let offset = si * 8;
            format!("
                {e1_instrs}
                mov [rsp - {offset}], rax
                {e2_instrs}
                mov rbx, rax
                xor rbx, [rsp - {offset}]
                test rbx, 1
                jne op_err
                cmp [rsp - {offset}], rax
                mov rbx, 3
                mov rax, 1
                cmovle rax, rbx
            ")
        },
        // Id
        Expr::Id(s) => {
            // Check if venv contains a mapping of variable s. If not, variable is unbounded
            if s == "input"{
                format!("mov rax, rdi")    
            } else{
                if venv.contains_key(s){
                    format!("mov rax, [rsp - {}]", venv.get(s).unwrap())
                } else{
                    panic!("Unbound variable identifier {s}")
                }
            }
        },
        // Let
        Expr::Let(binding, body) => {
            // Binding instructions
            let mut bind_instrs = String::new();
            // Create new local environment (every let has its own scope...hence own local environment)
            let mut lvenv = HashMap::<String, i32>::new();
            // Create nenv (merging outer and local scopes overwriting outer variables aka shadowing)
            let mut nvenv = HashMap::<String, i32>::new();
            nvenv.clone_from(venv);

            // Iterate through vector and store bindings onto stack
            let mut i = 0;
            while i < binding.len(){
                // Identifier
                let vid = &binding[i].0;

                // Check if lvenv already has var. If yes, panic with "Duplicate binding error". Else, store value on stack and update map
                if lvenv.contains_key(vid){
                    panic!("Duplicate binding for {vid}")
                } else{
                    // Create bind instruction 
                    let offset = (si+(i as i32))*8;
                    // Update nenv
                    nvenv.insert(vid.to_string(), offset);
                    // Insert entry into lenv
                    lvenv.insert(vid.to_string(), offset);

                    // Compile Expr part of binding
                    bind_instrs.push_str(&compile_expr(&binding[i].1, si+(i as i32), &mut nvenv, lbl, break_lbl));
                    bind_instrs.push_str("\n");

                    // Add binding value to stack
                    bind_instrs.push_str(&format!("mov [rsp - {offset}], rax"));
                    bind_instrs.push_str("\n");
                }
                // Update i
                i += 1
            }
            
            // Body
            let body_instrs = compile_expr(body, si+(binding.len() as i32), &mut nvenv, lbl, break_lbl);

            // Bindings + Body
            format!("
                {bind_instrs}
                {body_instrs}
            ")
        },
        // Boolean (left shifted by 1 as per snek definition)
        Expr::Boolean(n) => {
            if *n == true { // 3 is true in snek
                format!("mov rax, 3")
            } else { // 1 is false in snek
                format!("mov rax, 1")
            }
        },
        // If
        Expr::If(cond, then, els) => {
            let end_label = new_label(lbl, "endlbl");
            let else_label = new_label(lbl, "elselbl");
            let cond_instrs = compile_expr(cond, si, venv, lbl, break_lbl);
            let then_instrs = compile_expr(then, si, venv, lbl, break_lbl);
            let els_instrs = compile_expr(els, si, venv, lbl, break_lbl);
            format!("              
                {cond_instrs}
                cmp rax, 0      ; compare to check if cond > 0. If no, jump to else. Else continue checking.
                jle {else_label}
                cmp rax, 1      ; compare to check if cond == 1 (false in snek). If yes, jump to else. Else, continue with then part.
                je {else_label}
                {then_instrs}
                jmp {end_label}
                {else_label}:
                    {els_instrs}
                {end_label}:
            ")
        },
        // Loop
        Expr::Loop(body) => {
            let loop_label = new_label(lbl, "looplbl");
            let loop_end_label = new_label(lbl, "loopendlbl");
            let body_instrs = compile_expr(body, si, venv, lbl, &loop_end_label); //loop_end_label is the break label for any inner break statement
            format!("              
                {loop_label}:
                    {body_instrs}
                    jmp {loop_label}
                {loop_end_label}:
            ")
        },
        // Break
        Expr::Break(label) => {
            if break_lbl == "" {
                panic!("Error - 'break' statement not in loop ")
            } else{
                let label_instrs = compile_expr(label, si, venv, lbl, "");
                format!("
                    {label_instrs}
                    jmp {break_lbl}
                ")
            }
        },
        // Set!
        Expr::Set(var, expr) => {
            if venv.contains_key(var) {
                let var_instrs = compile_expr(expr, si, venv, lbl, break_lbl);
                let offset = venv[var];
                format!("
                    {var_instrs}
                    mov [rsp-{offset}], rax
                ")
            } else{
                panic!("Unbound variable identifier {var}")
            }
        },
        // Block
        Expr::Block(vexpr) => {
            // Vector of expression instructions
            let mut vexp_instrs = String::new();

            // Iterate through vector and execute instructions
            let mut i = 0;
            while i < vexpr.len(){
                // Compile Expr
                vexp_instrs.push_str(&compile_expr(&vexpr[i], si+(i as i32), venv, lbl, break_lbl));
                vexp_instrs.push_str("\n");

                // Update i
                i += 1
            }
            // Block instructions
            format!("
                {vexp_instrs}
            ")
        }
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    // Open input file (.snek)
    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    // Parse the input file
    let parse_panic = panic::catch_unwind(|| {
        parse(&in_contents).unwrap()
    });
    if parse_panic.is_err(){
        panic!("Invalid Code Input - Error in Parsing Code")
    }
    let expr = parse_expr(&parse(&in_contents).unwrap());

    // Create variable environment
    let mut venv = HashMap::<String, i32>::new();

    // Label index
    let mut lbl = 0;

    // Break Label
    let break_lbl = "";

    // Store the result
    let result = compile_expr(&expr, 2, &mut venv, &mut lbl, &break_lbl);

    let asm_program = format!("
section .text
global our_code_starts_here
extern snek_error
op_err:
    mov rdi, 300
    push rsp
    call snek_error
overflow_err:
    mov rdi, 301
    push rsp
    call snek_error
our_code_starts_here:
{}
ret
",
    result
    );

    // Write to .s file
    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}
