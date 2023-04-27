use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::Atom::*;
use sexp::*;

use im::{HashMap, hashmap};
enum Op1 { Add1, Sub1, IsNum, IsBool, }

enum Op2 { Plus, Minus, Times, Equal, Greater, GreaterEqual, Less, LessEqual, }

enum Expr {
    Number(i64),
    Boolean(bool),
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
    Set(String, Box<Expr>),
    Block(Vec<Expr>),
}

#[derive(Debug)]
enum Val {
    Reg(Reg),
    Imm(i64),
    RegOffset(Reg, i32),
}

#[derive(Debug, Copy, Clone)]
enum Reg {
    RAX,
    RSP,
    RDI
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    AND(Val, Val),
    CMP(Val, Val),
    CMOVE(Reg, Reg),
    CMOVG(Reg, Reg),
    CMOVGE(Reg, Reg),
    CMOVL(Reg, Reg),
    CMOVLE(Reg, Reg),
    JMP(String),
    JNE(String),
    JE(String),
    JGE(String),
    JG(String),
    JLE(String),
    JL(String),
    JO(String),
    Push(Val),
    Pop(Val),
    ShiftLeft(Reg, i8),
    ShiftRight(Reg, i8),
    ShiftArithmeticRight(Reg, i8),
    LABEL(String),
    XOR(Val, Val),
}



fn parse_expr(s: &Sexp) -> Expr {
    let reserved_words = vec!["let", "add1", "sub1", "*", "-", "+", "loop", "break", "if", "set!", "block", ">=", "<=", "=", ">", "<", "true", "false", "isnum", "isbool"];
    match s {
        Sexp::Atom(I(n)) => {
            if *n > 4611686018427387903 || *n < -4611686018427387904 {
                panic!("Invalid")
            } else {
            Expr::Number(*n)
            }
        },
        Sexp::Atom(S(bool_string)) if bool_string == "true" => Expr::Boolean(true),
        Sexp::Atom(S(bool_string)) if bool_string == "false" => Expr::Boolean(false),
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(op)), e] if op == "add1" => Expr::UnOp(Op1::Add1, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "sub1" => Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "isnum" => Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "isbool" => Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "loop" => Expr::Loop(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "break" => Expr::Break(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), Sexp::Atom(S(name)), e] if op == "set!" => Expr::Set(name.clone(), Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e1, e2] if op == "+" => Expr::BinOp(Op2::Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "-" => Expr::BinOp(Op2::Minus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "*" => Expr::BinOp(Op2::Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == ">" => Expr::BinOp(Op2::Greater, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == ">=" => Expr::BinOp(Op2::GreaterEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "=" => Expr::BinOp(Op2::Equal, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "<" => Expr::BinOp(Op2::Less, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "<=" => Expr::BinOp(Op2::LessEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2, e3] if op == "if" => {
                    Expr::If(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)), Box::new(parse_expr(e3)))
                },
                [Sexp::Atom(S(op)), Sexp::List(bindings) , e] if op == "let" => {
                    if bindings.is_empty() {
                        panic!("Invalid : Empty body for let bindings");
                    }
                    let mut binding_vector: Vec<(String, Expr)> = vec![];
                    for binding in bindings.iter() {
                        binding_vector.push(parse_bind(binding));
                    }
                    Expr::Let(binding_vector, Box::new(parse_expr(e)))
                },
                [Sexp::Atom(S(op)), exprs @ .. ] if op == "block" => {
                    if (exprs.is_empty()) {
                        panic!("Invalid : Empty block")
                    }
                    Expr::Block(exprs.into_iter().map(parse_expr).collect())
                }
                _ => panic!("Invalid Sexp"),
            }
        },
        Sexp::Atom(S(identifier)) => {
            if !reserved_words.contains(&identifier.as_str()) {
                Expr::Id(identifier.clone())
            } else {
                panic!("keyword {} encountered", identifier)
            }
        }
        _ => panic!("Invalid Sexp"),
    }
}


fn parse_bind(s: &Sexp) -> (String, Expr) {
    let reserved_words = vec!["let", "add1", "sub1", "*", "-", "+", "loop", "break", "if", "set!", "block", ">=", "<=", "=", ">", "<", "input", "true", "false", "isnum", "isbool"];
    match s {
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(var)), e] => {
                    if !reserved_words.contains(&var.as_str()) {
                        (var.clone(), parse_expr(e))
                    } else {
                        panic!("keyword {} encountered", var)
                    }
                }
                _ => panic!("Invalid binding syntax")        
            }
        }
        _ => panic!("Invalid binding syntax")
    }
}

fn new_label(l: &mut i32, s: &str) -> String {
  let current = *l;
  *l += 1;
  format!("{s}_{current}")
}


fn compile_to_instrs(e: &Expr, stack_index: i32, env: &HashMap<String, i32>, brake: &String, l: &mut i32) -> Vec<Instr> {
    let mut v: Vec<Instr> = vec![];
    match e {
        Expr::Number(n) => v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n * 2))),
        Expr::Boolean(val) => {
            if *val == true {
                v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)))
            } else {
                v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)))
            }
        },
        Expr::UnOp(op, rest) => {
            match op {
                Op1::Add1 => {
                    v.append(& mut compile_to_instrs(rest, stack_index, env, brake, l));
                    v.push(Instr::Push(Val::Reg(Reg::RAX)));
                    v.push(Instr::Push(Val::Reg(Reg::RDI)));
                    v.push(Instr::AND(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::CMP(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(1)));
                    v.push(Instr::JE(String::from("throw_error")) );
                    v.push(Instr::Pop(Val::Reg(Reg::RDI) ));
                    v.push(Instr::Pop(Val::Reg(Reg::RAX)));
                    v.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(2)));

                    v.push(Instr::Push(Val::Reg(Reg::RDI)));
                    v.push(Instr::Push(Val::Reg(Reg::RAX))); // We don't really need it, but push and pop anyway to preserve 16-byte alignment
                    v.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(2))); // Pass 2 to snek_error
                    v.push(Instr::JO(String::from("throw_error")) );
                    v.push(Instr::Pop(Val::Reg(Reg::RAX)));
                    v.push(Instr::Pop(Val::Reg(Reg::RDI)));
                },
                Op1::Sub1 => {
                    v.append(& mut compile_to_instrs(rest, stack_index, env, brake, l));
                    v.push(Instr::Push(Val::Reg(Reg::RAX)));
                    v.push(Instr::Push(Val::Reg(Reg::RDI)));
                    v.push(Instr::AND(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::CMP(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(1)));
                    v.push(Instr::JE(String::from("throw_error")) );
                    v.push(Instr::Pop(Val::Reg(Reg::RDI) ));
                    v.push(Instr::Pop(Val::Reg(Reg::RAX)));
                    v.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(2)));

                    v.push(Instr::Push(Val::Reg(Reg::RDI)));
                    v.push(Instr::Push(Val::Reg(Reg::RAX))); // We don't really need it, but push and pop anyway to preserve 16-byte alignment
                    v.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(2))); // Pass 2 to snek_error
                    v.push(Instr::JO(String::from("throw_error")) );
                    v.push(Instr::Pop(Val::Reg(Reg::RAX)));
                    v.push(Instr::Pop(Val::Reg(Reg::RDI)));
                },
                Op1::IsBool => {
                    v.append(& mut compile_to_instrs(rest, stack_index, env, brake, l));                    
                    v.push(Instr::AND(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::CMP(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::Push(Val::Reg(Reg::RDI)));
                    v.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(3)));
                    v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::CMOVE(Reg::RAX, Reg::RDI));
                    v.push(Instr::Pop(Val::Reg(Reg::RDI) ));
                },
                Op1::IsNum => {
                    v.append(& mut compile_to_instrs(rest, stack_index, env, brake, l));                    
                    v.push(Instr::AND(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::CMP(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::Push(Val::Reg(Reg::RDI)));
                    v.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(1)));
                    v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
                    v.push(Instr::CMOVE(Reg::RAX, Reg::RDI));
                    v.push(Instr::Pop(Val::Reg(Reg::RDI) ));
                }
            }
        }
        Expr::BinOp(op, e1, e2) => {
            v.append(& mut compile_to_instrs(e2, stack_index, env, brake, l));
            let stack_offset = stack_index * 8;
            v.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
            v.append(& mut compile_to_instrs(e1, stack_index + 1, env, brake, l));
            match op {
                Op2::Plus => {
                    v.push(Instr::Push(Val::Reg(Reg::RDI)));
                    v.push(Instr::Push(Val::Reg(Reg::RAX))); // operand 1 saved
                    v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset - 16))); // account for pushes, which have already decremented rsp
                    v.push(Instr::AND(Val::Reg(Reg::RAX), Val::Imm(1))); // rax is now 1 (boolean) or 0 (integer)
                    v.push(Instr::CMP(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(1))); 
                    v.push(Instr::JE(String::from("throw_error")) );
                    // operand 2 checked at this point

                    v.push(Instr::Pop(Val::Reg(Reg::RAX))); // operand 1 loaded into rax
                    v.push(Instr::Push(Val::Reg(Reg::RAX))); // rax saved, still has operand 1
                    v.push(Instr::AND(Val::Reg(Reg::RAX), Val::Imm(1))); // rax is now 1 (boolean) or 0 (integer)
                    v.push(Instr::CMP(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(1))); // Pass 1 to snek_error
                    v.push(Instr::JE(String::from("throw_error")) );

                    // both are integers
                    
                    v.push(Instr::Pop(Val::Reg(Reg::RAX)));
                    v.push(Instr::Pop(Val::Reg(Reg::RDI)));

                    v.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset))); // pop restored rsp, so no -16 here

                    v.push(Instr::Push(Val::Reg(Reg::RDI)));
                    v.push(Instr::Push(Val::Reg(Reg::RAX))); // We don't really need it, but push and pop anyway to preserve 16-byte alignment
                    v.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(2))); // Pass 2 to snek_error
                    v.push(Instr::JO(String::from("throw_error")) );
                    v.push(Instr::Pop(Val::Reg(Reg::RAX)));
                    v.push(Instr::Pop(Val::Reg(Reg::RDI)));
                },
                Op2::Minus => {
                    v.push(Instr::Push(Val::Reg(Reg::RDI)));
                    v.push(Instr::Push(Val::Reg(Reg::RAX))); // operand 1 saved
                    v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset - 16))); // account for pushes, which have already decremented rsp
                    v.push(Instr::AND(Val::Reg(Reg::RAX), Val::Imm(1))); // rax is now 1 (boolean) or 0 (integer)
                    v.push(Instr::CMP(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(1))); 
                    v.push(Instr::JE(String::from("throw_error")) );
                    // operand 2 checked at this point

                    v.push(Instr::Pop(Val::Reg(Reg::RAX))); // operand 1 loaded into rax
                    v.push(Instr::Push(Val::Reg(Reg::RAX))); // rax saved, still has operand 1
                    v.push(Instr::AND(Val::Reg(Reg::RAX), Val::Imm(1))); // rax is now 1 (boolean) or 0 (integer)
                    v.push(Instr::CMP(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(1))); // Pass 1 to snek_error
                    v.push(Instr::JE(String::from("throw_error")) );

                    // both are integers
                    
                    v.push(Instr::Pop(Val::Reg(Reg::RAX)));
                    v.push(Instr::Pop(Val::Reg(Reg::RDI)));


                    v.push(Instr::ISub(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));

                    v.push(Instr::Push(Val::Reg(Reg::RDI)));
                    v.push(Instr::Push(Val::Reg(Reg::RAX))); // We don't really need it, but push and pop anyway to preserve 16-byte alignment
                    v.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(2))); // Pass 2 to snek_error
                    v.push(Instr::JO(String::from("throw_error")) );
                    v.push(Instr::Pop(Val::Reg(Reg::RAX)));
                    v.push(Instr::Pop(Val::Reg(Reg::RDI)));
                },
                Op2::Times => {
                    v.push(Instr::Push(Val::Reg(Reg::RDI)));
                    v.push(Instr::Push(Val::Reg(Reg::RAX))); // operand 1 saved
                    v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset - 16))); // account for pushes, which have already decremented rsp
                    v.push(Instr::AND(Val::Reg(Reg::RAX), Val::Imm(1))); // rax is now 1 (boolean) or 0 (integer)
                    v.push(Instr::CMP(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(1))); 
                    v.push(Instr::JE(String::from("throw_error")) );
                    // operand 2 checked at this point

                    v.push(Instr::Pop(Val::Reg(Reg::RAX))); // operand 1 loaded into rax
                    v.push(Instr::Push(Val::Reg(Reg::RAX))); // rax saved, still has operand 1
                    v.push(Instr::AND(Val::Reg(Reg::RAX), Val::Imm(1))); // rax is now 1 (boolean) or 0 (integer)
                    v.push(Instr::CMP(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(1))); // Pass 1 to snek_error
                    v.push(Instr::JE(String::from("throw_error")) );

                    // both are integers
                    
                    v.push(Instr::Pop(Val::Reg(Reg::RAX)));
                    v.push(Instr::Pop(Val::Reg(Reg::RDI)));

                    v.push(Instr::ShiftArithmeticRight(Reg::RAX, 1));
                    v.push(Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));

                    v.push(Instr::Push(Val::Reg(Reg::RDI)));
                    v.push(Instr::Push(Val::Reg(Reg::RAX))); // We don't really need it, but push and pop anyway to preserve 16-byte alignment
                    v.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(2))); // Pass 2 to snek_error
                    v.push(Instr::JO(String::from("throw_error")) );
                    v.push(Instr::Pop(Val::Reg(Reg::RAX)));
                    v.push(Instr::Pop(Val::Reg(Reg::RDI)));
                },
                Op2::Greater => {
                    v.push(Instr::Push(Val::Reg(Reg::RDI)));
                    v.push(Instr::Push(Val::Reg(Reg::RAX))); // operand 1 saved
                    v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset - 16))); // account for pushes, which have already decremented rsp
                    v.push(Instr::AND(Val::Reg(Reg::RAX), Val::Imm(1))); // rax is now 1 (boolean) or 0 (integer)
                    v.push(Instr::CMP(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(1))); 
                    v.push(Instr::JE(String::from("throw_error")) );
                    // operand 2 checked at this point

                    v.push(Instr::Pop(Val::Reg(Reg::RAX))); // operand 1 loaded into rax
                    v.push(Instr::Push(Val::Reg(Reg::RAX))); // rax saved, still has operand 1
                    v.push(Instr::AND(Val::Reg(Reg::RAX), Val::Imm(1))); // rax is now 1 (boolean) or 0 (integer)
                    v.push(Instr::CMP(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(1))); // Pass 1 to snek_error
                    v.push(Instr::JE(String::from("throw_error")) );

                    // both are integers

                    v.push(Instr::Pop(Val::Reg(Reg::RAX)));

                    v.push(Instr::CMP(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset - 8))); // One register still pushed
                    v.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(3)));
                    v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::CMOVG(Reg::RAX, Reg::RDI));
                    v.push(Instr::Pop(Val::Reg(Reg::RDI)));

                },
                Op2::GreaterEqual => {
                    v.push(Instr::Push(Val::Reg(Reg::RDI)));
                    v.push(Instr::Push(Val::Reg(Reg::RAX))); // operand 1 saved
                    v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset - 16))); // account for pushes, which have already decremented rsp
                    v.push(Instr::AND(Val::Reg(Reg::RAX), Val::Imm(1))); // rax is now 1 (boolean) or 0 (integer)
                    v.push(Instr::CMP(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(1))); 
                    v.push(Instr::JE(String::from("throw_error")) );
                    // operand 2 checked at this point

                    v.push(Instr::Pop(Val::Reg(Reg::RAX))); // operand 1 loaded into rax
                    v.push(Instr::Push(Val::Reg(Reg::RAX))); // rax saved, still has operand 1
                    v.push(Instr::AND(Val::Reg(Reg::RAX), Val::Imm(1))); // rax is now 1 (boolean) or 0 (integer)
                    v.push(Instr::CMP(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(1))); // Pass 1 to snek_error
                    v.push(Instr::JE(String::from("throw_error")) );

                    // both are integers

                    v.push(Instr::Pop(Val::Reg(Reg::RAX)));

                    v.push(Instr::CMP(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset - 8)));
                    v.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(3)));
                    v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::CMOVGE(Reg::RAX, Reg::RDI));
                    v.push(Instr::Pop(Val::Reg(Reg::RDI)));
                },
                Op2::Less => {
                    v.push(Instr::Push(Val::Reg(Reg::RDI)));
                    v.push(Instr::Push(Val::Reg(Reg::RAX))); // operand 1 saved
                    v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset - 16))); // account for pushes, which have already decremented rsp
                    v.push(Instr::AND(Val::Reg(Reg::RAX), Val::Imm(1))); // rax is now 1 (boolean) or 0 (integer)
                    v.push(Instr::CMP(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(1))); 
                    v.push(Instr::JE(String::from("throw_error")) );
                    // operand 2 checked at this point

                    v.push(Instr::Pop(Val::Reg(Reg::RAX))); // operand 1 loaded into rax
                    v.push(Instr::Push(Val::Reg(Reg::RAX))); // rax saved, still has operand 1
                    v.push(Instr::AND(Val::Reg(Reg::RAX), Val::Imm(1))); // rax is now 1 (boolean) or 0 (integer)
                    v.push(Instr::CMP(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(1))); // Pass 1 to snek_error
                    v.push(Instr::JE(String::from("throw_error")) );

                    // both are integers

                    v.push(Instr::Pop(Val::Reg(Reg::RAX)));

                    v.push(Instr::CMP(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset - 8)));
                    v.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(3)));
                    v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::CMOVL(Reg::RAX, Reg::RDI));
                    v.push(Instr::Pop(Val::Reg(Reg::RDI)));
                },
                Op2::LessEqual => {
                    v.push(Instr::Push(Val::Reg(Reg::RDI)));
                    v.push(Instr::Push(Val::Reg(Reg::RAX))); // operand 1 saved
                    v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset - 16))); // account for pushes, which have already decremented rsp
                    v.push(Instr::AND(Val::Reg(Reg::RAX), Val::Imm(1))); // rax is now 1 (boolean) or 0 (integer)
                    v.push(Instr::CMP(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(1))); 
                    v.push(Instr::JE(String::from("throw_error")) );
                    // operand 2 checked at this point

                    v.push(Instr::Pop(Val::Reg(Reg::RAX))); // operand 1 loaded into rax
                    v.push(Instr::Push(Val::Reg(Reg::RAX))); // rax saved, still has operand 1
                    v.push(Instr::AND(Val::Reg(Reg::RAX), Val::Imm(1))); // rax is now 1 (boolean) or 0 (integer)
                    v.push(Instr::CMP(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(1))); // Pass 1 to snek_error
                    v.push(Instr::JE(String::from("throw_error")) );

                    // both are integers
                    v.push(Instr::Pop(Val::Reg(Reg::RAX)));
                    
                    v.push(Instr::CMP(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset - 8)));
                    v.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(3)));
                    v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::CMOVLE(Reg::RAX, Reg::RDI));
                    v.push(Instr::Pop(Val::Reg(Reg::RDI)));
                },
                Op2::Equal => {
                    v.push(Instr::Push(Val::Reg(Reg::RDI)));
                    v.push(Instr::Push(Val::Reg(Reg::RAX)));

                    v.push(Instr::XOR(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset - 16)));
                    v.push(Instr::AND(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::CMP(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(1))); 
                    v.push(Instr::JE(String::from("throw_error")) );

                    v.push(Instr::Pop(Val::Reg(Reg::RAX)));

                    v.push(Instr::CMP(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset - 8)));
                    v.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(3)));
                    v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::CMOVE(Reg::RAX, Reg::RDI));


                    v.push(Instr::Pop(Val::Reg(Reg::RDI)));
                }
            }
            //v.push(Instr::)
        }
        Expr::Let(bindings, e) => {
            let mut current_stack_index = stack_index;
            let mut nenv = env.clone();
            let mut duplicate_check_map: HashMap<String, ()> = hashmap! {};
            for binding in bindings.iter() {
                if duplicate_check_map.contains_key(&(binding.0)) {
                    panic!("Duplicate binding");
                }
                duplicate_check_map = duplicate_check_map.update(binding.0.clone(), ());
                let mut expr_instrs = compile_to_instrs(&(binding.1), current_stack_index, &nenv, brake, l);
                v.append(& mut expr_instrs);
                let stack_offset = current_stack_index * 8;
                nenv = nenv.update(binding.0.clone(), stack_offset);
                //print!("{:?}", nenv);
                v.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                current_stack_index += 1;
            }
            v.append(& mut compile_to_instrs(e, current_stack_index, &nenv, brake, l));
        }
        Expr::Id(variable) => {
            if variable == "input" {
                v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI)))
            } else {
                match env.get(variable) {
                    Some(value) => v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, *value))),
                    None => panic!("Unbound variable identifier {}", variable)
                }
            }
        }
        Expr::If(e1, e2, e3) => {
            v.append(& mut compile_to_instrs(e1, stack_index, env, brake, l));
            let elselabel = new_label(l, "else");
            let endlabel = new_label(l, "else");
            v.push(Instr::CMP(Val::Reg(Reg::RAX), Val::Imm(1)));
            v.push(Instr::JE(elselabel.clone()));
            v.append(& mut compile_to_instrs(e2, stack_index, env, brake, l));
            v.push(Instr::JMP(endlabel.clone()));
            v.push(Instr::LABEL(elselabel));
            v.append(& mut compile_to_instrs(e3, stack_index, env, brake, l));
            v.push(Instr::LABEL(endlabel));
        },
        Expr::Block(expressions) => {
            for expression in expressions.iter() {
                v.append(& mut compile_to_instrs(expression, stack_index, env, brake, l));
            }
        },
        Expr::Set(var, expr) => {
            match env.get(var) {
                Some(value) => {
                    v.append(& mut compile_to_instrs(expr, stack_index, env, brake, l));
                    v.push(Instr::IMov(Val::RegOffset(Reg::RSP, *value), Val::Reg(Reg::RAX)));
                },
                None => panic!("Unbound variable identifier {}", var)
            }
        }
        Expr::Loop(expr) => {
            let loop_start_label = new_label(l, "loop_start");
            let loop_end_label = new_label(l, "loop_end");
            v.push(Instr::LABEL(loop_start_label.clone()));
            v.append(& mut compile_to_instrs(expr, stack_index, env, &loop_end_label, l));
            v.push(Instr::JMP(loop_start_label));
            v.push(Instr::LABEL(loop_end_label));
        }
        Expr::Break(expr) => {
            if (brake == "") {
                panic!("break outside loop")
            }
            v.append(& mut compile_to_instrs(expr, stack_index, env, brake, l));
            v.push(Instr::JMP(brake.to_string()));
        }

    }
    return v;
}

fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IAdd(v1, v2) => format!("add {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::IMov(v1, v2) => format!("mov {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ISub(v1, v2) => format!("sub {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::IMul(v1, v2) => format!("imul {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::AND(v1, v2) => format!("and {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::CMP(v1, v2) => format!("cmp {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::CMOVE(r1, r2) => format!("cmove {}, {}", val_to_str(&Val::Reg(*r1)), val_to_str(&Val::Reg(*r2))),
        Instr::CMOVG(r1, r2) => format!("cmovg {}, {}", val_to_str(&Val::Reg(*r1)), val_to_str(&Val::Reg(*r2))),
        Instr::CMOVGE(r1, r2) => format!("cmovge {}, {}", val_to_str(&Val::Reg(*r1)), val_to_str(&Val::Reg(*r2))),
        Instr::CMOVL(r1, r2) => format!("cmovl {}, {}", val_to_str(&Val::Reg(*r1)), val_to_str(&Val::Reg(*r2))),
        Instr::CMOVLE(r1, r2) => format!("cmovle {}, {}", val_to_str(&Val::Reg(*r1)), val_to_str(&Val::Reg(*r2))),
        Instr::JMP(label) => format!("jmp {}", label),
        Instr::JE(label) => format!("je {}", label),
        Instr::JNE(label) => format!("jne {}", label),
        Instr::JGE(label) => format!("jge {}", label),
        Instr::JG(label) => format!("jg {}", label),
        Instr::JLE(label) => format!("jle {}", label),
        Instr::JL(label) => format!("jl {}", label),
        Instr::JO(label) => format!("jo {}", label),
        Instr::Push(v) => format!("push {}", val_to_str(v)),
        Instr::Pop(v) => format!("pop {}", val_to_str(v)),
        Instr::ShiftLeft(r, num_bits) => format!("shl {}, {}", val_to_str(&Val::Reg(*r)), num_bits),
        Instr::ShiftRight(r, num_bits) => format!("shr {}, {}", val_to_str(&Val::Reg(*r)), num_bits),
        Instr::ShiftArithmeticRight(r,num_bits ) => format!("sar {}, {}", val_to_str(&Val::Reg(*r)), num_bits),
        Instr::LABEL(label) => format!("{}:", label),
        Instr::XOR(v1, v2) => format!("xor {}, {}", val_to_str(v1), val_to_str(v2)),
    }
}

fn val_to_str(v: &Val) -> String {
    match v {
        Val::Reg(reg) => {
            match reg {
                Reg::RAX => String::from("rax"),
                Reg::RSP => String::from("rsp"),
                Reg::RDI => String::from("rdi"),
            }
        }
        Val::Imm(n) => n.to_string(),
        Val::RegOffset(reg, n) => {
            match reg {
                Reg::RSP => format!("[rsp - {}]", *n),
                Reg::RAX => panic!("Unexpected use of RAX with offset"),
                Reg::RDI => panic!("Unexpected use of RDI with offset"),
            }
        }
    }
}

fn compile(e: &Expr) -> String {
    let env: HashMap<String, i32> = hashmap! {};
    let mut labels = 0;
    let instructions = compile_to_instrs(e, 3, &env, &String::from(""), &mut labels);
    // Stack index starts at 3 instead of 2, because I push upto two registers onto the stack before popping them
    let mut result  = String::new();
    for instr in instructions.iter() {
        result.push_str(instr_to_str(instr).as_str());
        result.push_str("\n");
    }
    return result;
}



fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let parse_result = parse(&in_contents);

    let sexp: Sexp;
    match parse_result {
        Ok(valid_sexp) => {
            sexp = valid_sexp;
        }
        Err(e) => panic!("Invalid Sexp: Error {}", e.message),
    };

    let expr = parse_expr(&sexp);
    let result = compile(&expr);
    let asm_program = format!(
        "
section .text
global our_code_starts_here
extern snek_error
extern snek_print
throw_error:
  push rsp
  call snek_error
  ret
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
