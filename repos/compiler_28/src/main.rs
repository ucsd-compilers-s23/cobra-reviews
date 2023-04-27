use std::env;
use std::fs::File;
use std::io::prelude::*;
use sexp::*;
use sexp::Atom::*;
use im::HashMap;
use im::hashmap;
use std::collections::HashSet;
use std::sync::atomic::{AtomicUsize, Ordering};

static UNIQUE_IDENTIFIER: AtomicUsize = AtomicUsize::new(0);
const ERROR_BOOLEAN: i64 = 1;

const STACK_INDEX: i64 = 2;

#[derive(Debug)]
enum Val {
    Reg(Reg),
    Imm(i64),              // Imm = Immediate value (int 32 bit)
    RegOffset(Reg, i64),   // Register offset tuple (Register, offset int 32 bit)
}

#[derive(Copy, Clone, Debug)] // Remove Copy & Clone implementation
enum Reg {
    RAX,
    RBX,
    RCX,
    RDX,
    RSP,
    RDI,
    RSI,
    AL,
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val,Val),
    IMul(Val, Val),
    ICmp(Val, Val),
    ISete(Val), // sete "set if equal" instruction only operates on 8-bit registers
    ISetg(Val), // setg "set if greater" instruction only operates on 8-bit registers like AL
    IOr(Val, Val),
    IAnd(Val, Val),
    IShl(Val, Val),
    IJe(String),
    IJmp(String),
    ILabel(String),
    IJne(String),
    ICall(String),
    IMovZ(Val, Val),
    IShr(Val, Val),
}

#[derive(Debug)]
enum Op1 {
    Add1,
    Sub1,
    IsNum,
    IsBool,
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
    LessEqual,
}

#[derive(Debug)]
enum Expr {
    Num(i64),
    Boolean(bool),
    Let(Vec<(String, Expr)>, Box<Expr>),
    Id(String),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
    Set(String, Box<Expr>),
    Block(Vec<Expr>),
}

fn unique_id() -> usize {
  UNIQUE_IDENTIFIER.fetch_add(1, Ordering::SeqCst)
}

fn val_to_str(v: &Val) -> String {
    match v {
        Val::Reg(reg) => match reg {
            Reg::RAX => "rax".to_string(),
            Reg::RSP => "rsp".to_string(),
            Reg::RBX => "rbx".to_string(),
            Reg::RCX => "rcx".to_string(),
            Reg::RDX => "rdx".to_string(),
            Reg::RDI => "rdi".to_string(),
            Reg::RSI => "rsi".to_string(),
            Reg::AL => "al".to_string(),
        },
        Val::Imm(n) => n.to_string(),
        Val::RegOffset(ref reg, offset) => format!("[{} - {}]", val_to_str(&Val::Reg(*reg)), offset),
    }
}

fn instr_to_str(i: Instr) -> String {
    match i {
        Instr::IMov(src, dst) => format!("mov {}, {}", val_to_str(&src), val_to_str(&dst)), // Remove ampersands & if not working
        Instr::IAdd(val1, val2) => format!("add {}, {}", val_to_str(&val1), val_to_str(&val2)),
        Instr::ISub(val1, val2) => format!("sub {}, {}", val_to_str(&val1), val_to_str(&val2)),
        Instr::IMul(val1, val2) => format!("imul {}, {}", val_to_str(&val1), val_to_str(&val2)),
        Instr::ICmp(val1, val2) => format!("cmp {}, {}", val_to_str(&val1), val_to_str(&val2)),
        Instr::ISete(reg) => format!("sete {}", val_to_str(&reg)),
        Instr::ISetg(reg) => format!("setg {}", val_to_str(&reg)),
        Instr::IOr(reg, val) => format!("or {}, {}", val_to_str(&reg), val_to_str(&val)),
        Instr::IAnd(reg, val) => format!("and {}, {}", val_to_str(&reg), val_to_str(&val)),
        Instr::IShl(reg, val) => format!("shl {}, {}", val_to_str(&reg), val_to_str(&val)),
        Instr::IShr(reg, val) => format!("sar {}, {}", val_to_str(&reg), val_to_str(&val)),
        Instr::IJmp(label) => format!("jmp {}", label),
        Instr::IJe(label) => format!("je {}", label),
        Instr::ILabel(label) => format!("{}:", label),
        Instr::IJne(label) => format!("jne {}", label),
        Instr::ICall(func_name) => format!("call {}", func_name),
        Instr::IMovZ(src, dst) => format!("movzx {}, {}", val_to_str(&src), val_to_str(&dst)),
    }
}

fn check_and_handle_invalid_boolean_operation(si: i64) -> Vec<Instr> {
  vec![
      Instr::IAnd(Val::Reg(Reg::RCX), Val::Imm(1)),
      Instr::ICmp(Val::Reg(Reg::RCX), Val::Imm(1)),
      Instr::IJne(format!(".second_error_check_{}", si)),
      Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(ERROR_BOOLEAN)),
      Instr::ICall(String::from("snek_error")),
      Instr::ILabel(format!(".second_error_check_{}", si)),
      Instr::IAnd(Val::Reg(Reg::RSI), Val::Imm(1)),
      Instr::ICmp(Val::Reg(Reg::RSI), Val::Imm(1)),
      Instr::IJne(format!(".finished_error_check_{}", si)),
      Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(ERROR_BOOLEAN)),
      Instr::ICall(String::from("snek_error")),
      Instr::ILabel(format!(".finished_error_check_{}", si)),
  ]
}

fn is_bool(id_str: &str) -> bool {
  if id_str.to_string() == "true" || id_str.to_string() == "false" {
    return true
  } else {
    return false
  }
}

fn is_valid_id(id_str: &str) -> bool {
  // Modify this to exclude: "false" "true" "input" "block" "break" "set!" "loop" "if"
    let mut chars = id_str.chars();
    match chars.next() {
        Some(c) if c.is_alphabetic() => {}
        _ => return false,
    }

    chars.all(|c| c.is_alphanumeric() || c == '-' || c == '_')
}

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => Expr::Num(i64::try_from(*n).unwrap()),
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(op)), e] if op == "isbool" => Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "isnum" => Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "add1" => Expr::UnOp(Op1::Add1, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "sub1" => Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e1, e2] if op == "+" => Expr::BinOp(Op2::Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "-" => Expr::BinOp(Op2::Minus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "*" => Expr::BinOp(Op2::Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "=" => Expr::BinOp(Op2::Equal, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "<" => Expr::BinOp(Op2::Less, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == ">" => Expr::BinOp(Op2::Greater, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == ">=" => Expr::BinOp(Op2::GreaterEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "<=" => Expr::BinOp(Op2::LessEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), Sexp::List(bindings), body] if op == "let" => {
                    if bindings.is_empty() {
                        panic!("Invalid - no let binding");
                    }
                    let seen_vars: HashSet<String> = HashSet::new();
                    let bindings = bindings.iter().map(|binding| {
                        if let Sexp::List(vec) = binding {
                            if let [Sexp::Atom(S(var)), e] = vec.as_slice() {
                                if seen_vars.contains(var) {
                                    panic!("Duplicate binding")
                                }
                                (var.clone(), parse_expr(e))
                            } else {
                                panic!("Invalid let binding format, must be a var, expr pair");
                            }
                        } else {
                            panic!("Invalid let binding format: must be a list of bindings");
                        }
                    }).collect::<Vec<(String, Expr)>>();
                    Expr::Let(bindings, Box::new(parse_expr(body)))
                },
                [Sexp::Atom(S(op)), cond, then_branch, else_branch] if op == "if" => {
                  Expr::If(
                    Box::new(parse_expr(cond)),
                    Box::new(parse_expr(then_branch)),
                    Box::new(parse_expr(else_branch)),
                  )
                },
                [Sexp::Atom(S(op)), elems @ ..] if op == "block" => {
                  let blocks = elems.iter().map(|elem| parse_expr(elem)).collect();
                  Expr::Block(blocks)
                },
                [Sexp::Atom(S(op)), Sexp::Atom(S(var)), value] if op == "set" => {
                  Expr::Set(var.clone(), Box::new(parse_expr(value)))
                },
                [Sexp::Atom(S(op)), subexpr] if op == "break" => {
                  Expr::Break(Box::new(parse_expr(subexpr)))
                }
                [Sexp::Atom(S(op)), subexpr] if op == "loop" => {
                  Expr::Loop(Box::new(parse_expr(subexpr)))
                },
                _ => panic!("Invalid amount of parameters"),
            }
        },
        Sexp::Atom(S(id_str)) => {
            if is_bool(id_str) {
              if id_str.to_string() == "true" {
                Expr::Boolean(true)
              } else {
                Expr::Boolean(false)
              }
            // TODO add keyword checking for ids (loop, break, if, etc.)
            } else if is_valid_id(id_str) {
                Expr::Id(id_str.to_string())
            } else {
                panic!("Invalid identifier format, must start with a letter and subsequent chars be alphanumeric, - , and _ chars")
            } 
        },
        _ => panic!("parse error on Invalid s-expression"),
    }
}

fn compile_to_instrs(e: &Expr, mut si: i64, envx: &HashMap<String, i64>) -> Vec<Instr> {
    match e {
      // TODO BOOLEAN(bool)
        Expr::Boolean(b) => {
          if b.to_string() == "true" {
            vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3))] // Put int representation of 'true' into RAX
          } else if b.to_string() == "false" {
            vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))] // Put int representation of 'false' into RAX
          } else if b.to_string() == "input" {
            vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI))] // Put int representation of 'input' into RAX from RDI
          } else {
            panic!("Invalid");
          }
        },
        Expr::Num(n) => {
          if *n < -(1 << 62) || *n >= (1 << 62) {
            panic!("Invalid");
          }
          vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm((*n)*2))]
        },
        Expr::UnOp(Op1::Add1, subexpr) => {
            let mut instrs = compile_to_instrs(subexpr, si, envx); // Change si to STACK_INDEX if not working
            instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(2)));
            instrs
        },
        Expr::UnOp(Op1::Sub1, subexpr) => {
            let mut instrs = compile_to_instrs(subexpr, si, envx); // Change si to STACK_INDEX if not working
            instrs.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(2)));
            instrs
        },
        // TODO ISNUM
        Expr::UnOp(Op1::IsNum, subexpr) => {
          let mut instrs = compile_to_instrs(subexpr, si, envx); // Change si to STACK_INDEX if not working
          
          instrs
        },
        // TODO ISBOOL
        Expr::UnOp(Op1::IsBool, subexpr) => {
          let mut instrs = compile_to_instrs(subexpr, si, envx); // Change si to STACK_INDEX if not working
          
          instrs
        },
        // TODO EQUAL
        Expr::BinOp(Op2::Equal, e1, e2) => {
          let mut instrs = compile_to_instrs(e1, si, envx);
          let stack_offset = si * 8;
          instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX))); 
          instrs.extend(compile_to_instrs(e2, si + 1, envx));
          instrs.push(Instr::IMov(Val::Reg(Reg::RDX), Val::Reg(Reg::RAX))); // e2 goes to RDX
          instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset))); // put e1 result to RAX
          instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Reg(Reg::RDX))); // Compare RAX and RDX
          instrs.push(Instr::ISete(Val::Reg(Reg::AL))); // Set AL to 1 if equal, 0 otherwise
          instrs.push(Instr::IShl(Val::Reg(Reg::RAX), Val::Imm(1))); // Shift left by 1 to make room for the least significant bit
          instrs.push(Instr::IOr(Val::Reg(Reg::RAX), Val::Imm(1))); // least significant bit becomes to 1 to signify boolean result
          instrs
        },
        // TODO Less
        Expr::BinOp(Op2::Less, e1, e2) => {
          // "invalid argument" for ANY Booleans used
          let mut instrs = compile_to_instrs(e1, si, envx);
          let stack_offset = si * 8;
          instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
          instrs.extend(compile_to_instrs(e2, si + 1, envx));
          instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
          instrs
        },
        // TODO LessEqual
        Expr::BinOp(Op2::LessEqual, e1, e2) => {
          // "invalid argument" for ANY Booleans used
          let mut instrs = compile_to_instrs(e1, si, envx);
          let stack_offset = si * 8;
          instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
          instrs.extend(compile_to_instrs(e2, si + 1, envx));
          instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
          instrs
        },
        // TODO Greater
        Expr::BinOp(Op2::Greater, e1, e2) => {
          // "invalid argument" for ANY Booleans used
          let mut instrs = compile_to_instrs(e1, si, envx);
          let stack_offset = si * 8;
          instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
          instrs.extend(compile_to_instrs(e2, si + 1, envx));
          instrs.push(Instr::IMov(Val::Reg(Reg::RDX), Val::Reg(Reg::RAX))); // e2 gets sent to RDX
          instrs.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Reg(Reg::RAX))); // e2 also gets copied to RCX
          instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset))); // put e1 result into RAX
          instrs.push(Instr::IMov(Val::Reg(Reg::RSI), Val::RegOffset(Reg::RSP, stack_offset))); // copy e1 result also into RSI
          instrs.extend(check_and_handle_invalid_boolean_operation(si));
          instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Reg(Reg::RDX))); // Compare RAX and RDX
          instrs.push(Instr::ISetg(Val::Reg(Reg::AL))); // Set AL to 1 if RAX > RDX, 0 otherwise
          instrs.push(Instr::IShl(Val::Reg(Reg::AL), Val::Imm(1))); // Shift left by 1 to make room for the least significant bit
          instrs.push(Instr::IOr(Val::Reg(Reg::AL), Val::Imm(1))); // least significant bit becomes to 1 to signify boolean result
          instrs.push(Instr::IMovZ(Val::Reg(Reg::RAX), Val::Reg(Reg::AL)));
          instrs
        },
        // TODO GreaterEqual
        Expr::BinOp(Op2::GreaterEqual, e1, e2) => {
          // "invalid argument" for ANY Booleans used
          let mut instrs = compile_to_instrs(e1, si, envx);
          let stack_offset = si * 8;
          instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
          instrs.extend(compile_to_instrs(e2, si + 1, envx));
          instrs.push(Instr::IMov(Val::Reg(Reg::RDX), Val::Reg(Reg::RAX))); // e2 gets sent to RDX
          instrs.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Reg(Reg::RAX))); // e2 also gets copied to RCX
          instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset))); // put e1 result into RAX
          instrs.push(Instr::IMov(Val::Reg(Reg::RSI), Val::RegOffset(Reg::RSP, stack_offset))); // copy e1 result also into RSI
          instrs.extend(check_and_handle_invalid_boolean_operation(si));
          instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Reg(Reg::RDX))); // Compare RAX and RDX
          instrs.push(Instr::ISetg(Val::Reg(Reg::AL))); // Set AL to 1 if RAX > RDX, 0 otherwise
          instrs.push(Instr::IMovZ(Val::Reg(Reg::RCX), Val::Reg(Reg::AL)));
          instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Reg(Reg::RDX))); // Compare RAX and RDX
          instrs.push(Instr::ISete(Val::Reg(Reg::AL)));
          instrs.push(Instr::IMovZ(Val::Reg(Reg::RAX), Val::Reg(Reg::AL)));
          instrs.push(Instr::IOr(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)));
          instrs.push(Instr::IShl(Val::Reg(Reg::RAX), Val::Imm(1))); // Shift left by 1 to make room for the least significant bit
          instrs.push(Instr::IOr(Val::Reg(Reg::RAX), Val::Imm(1))); // least significant bit becomes to 1 to signify boolean result
          instrs
        },
        // TODO IF
        Expr::If(e1, e2, e3) => {
          let mut instrs = compile_to_instrs(e1, si, envx);
          let true_label = format!("true_{}_{}", unique_id(), si);
          let end_label = format!("end_{}_{}", unique_id(), si);
      
          instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(3))); // Compare the result of e1 with the integer representation of true (3)
          instrs.push(Instr::IJe(true_label.clone())); // Jump to true_label if the condition is true
      
          // False branch (e3)
          instrs.extend(compile_to_instrs(e3, si + 1, envx));
          instrs.push(Instr::IJmp(end_label.clone())); // Jump to end_label after executing the false branch
      
          // True branch (e2)
          instrs.push(Instr::ILabel(true_label));
          instrs.extend(compile_to_instrs(e2, si + 2, envx));
      
          // End label
          instrs.push(Instr::ILabel(end_label));
      
          instrs
        },
        // TODO BREAK
        Expr::Break(e1) => {
          let mut instrs = compile_to_instrs(e1, si, envx);
          let stack_offset = si * 8;
          instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
          // instrs.extend(compile_to_instrs(e2, si + 1, envx));
          instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
          instrs
        },
        // TODO LOOP
        Expr::Loop(e1) => {
          let mut instrs = compile_to_instrs(e1, si, envx);
          let stack_offset = si * 8;
          instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
          // instrs.extend(compile_to_instrs(e2, si + 1, envx));
          instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
          instrs
        },
        // TODO SET!
        Expr::Set(name, e1) => {
          let mut instrs = compile_to_instrs(e1, si, envx);
          let stack_offset = si * 8;
          instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
          // instrs.extend(compile_to_instrs(e2, si + 1, envx));
          instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
          instrs
        },
        // TODO BLOCK
        Expr::Block(bindings) => {
            let mut instrs = vec![];
            let mut new_env = envx.clone();
            for e in bindings {
                let stack_offset = si * 8;
                let e_instrs = compile_to_instrs(e, si, &new_env);

                instrs.extend(e_instrs);
                instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));

                // new_env.insert(x.clone(), stack_offset);
                si += 1;
            }
            instrs
        },
        Expr::BinOp(Op2::Plus, e1, e2) => {
            let mut instrs = compile_to_instrs(e1, si, envx);
            let stack_offset = si * 8;
            instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
            instrs.extend(compile_to_instrs(e2, si + 1, envx));
            instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
            instrs
        },
        Expr::BinOp(Op2::Minus, e1, e2) => {
            let mut instrs = compile_to_instrs(e2, si, envx);
            let stack_offset = si * 8;
            instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
            instrs.extend(compile_to_instrs(e1, si + 1, envx));
            instrs.push(Instr::ISub(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
            instrs
        },
        Expr::BinOp(Op2::Times, e1, e2) => {
            let mut instrs = compile_to_instrs(e1, si, envx);
            instrs.push(Instr::IShr(Val::Reg(Reg::RAX), Val::Imm(1)));
            let stack_offset = si * 8;
            instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
            instrs.extend(compile_to_instrs(e2, si + 1, envx));
            instrs.push(Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
            instrs
        },
        // Updated Let match:
        Expr::Let(bindings, body) => {
            let mut instrs = vec![];
            let mut new_env = envx.clone();
            let mut seen_vars: HashSet<String> = HashSet::new();
            for (x, e) in bindings {
                if seen_vars.contains(x) {
                    panic!("Duplicate binding")
                }
                seen_vars.insert(x.to_string());
                let stack_offset = si * 8;
                let e_instrs = compile_to_instrs(e, si, &new_env);

                instrs.extend(e_instrs);
                instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));

                new_env.insert(x.clone(), stack_offset);
                si += 1;
            }

            let b_instrs = compile_to_instrs(body, si, &new_env);
            instrs.extend(b_instrs);

            instrs
        },
        Expr::Id(s) => {
            if let Some(stack_offset) = envx.get(s) {
                vec![Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, *stack_offset))]
            } else if s.to_string() == "input" {
                vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI))]
            } else {
                panic!("Unbound variable identifier {}", s.to_string())
            }
        },
    }
}

// Potentially updated compile function:
fn compile(e: &Expr, si: i64, envx: &HashMap<String, i64>) -> String {
    let instrs = compile_to_instrs(e, si, envx);
    instrs.into_iter().map(instr_to_str).collect::<Vec<String>>().join("\n")
}


fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let expr = parse(&in_contents);
    match expr {
        Ok(expr) => {
            let pexpr = parse_expr(&expr);
            let envx: HashMap<String, i64> = hashmap!{};
            let result = compile(&pexpr, STACK_INDEX, &envx);                    // STARTING INDEX IS 2, MAYBE REPLACE WITH CONSTANT VAR?
            let asm_program = format!(
"section .text
extern snek_error
global our_code_starts_here
our_code_starts_here:
    {}
    ret
        ", result);
        
            let mut out_file = File::create(out_name)?;
            out_file.write_all(asm_program.as_bytes())?;
        },
        Err(_) => panic!("Invalid - Must have surrounding parentheses"),
    }
    Ok(())
}
