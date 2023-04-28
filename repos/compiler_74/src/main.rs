use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::collections::HashSet;

use sexp::Atom::*;
use sexp::*;
use regex::Regex;

use im::HashMap;

#[derive(Debug)]
enum Val {
    Reg(Reg),
    Imm(i64),
    RegOffset(Reg, i64),
}

#[derive(Debug)]
enum Reg {
    RAX,
    RBX,
    RSP,
    EAX,
    RDI,
}

#[derive(Debug)]
enum Cond{
  Z,
  S,
  O,
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    INeg(Val),
    Test(Val, Val),
    Cmp(Val, Val),
    Jmp(String),
    Je(String),
    Jne(String),
    Jg(String),
    Jge(String),
    JO(String),
    Setz(Val),
    IShl(Val, Val),
    ISar(Val, Val),
    IShr(Val, Val),
    IAnd(Val, Val),
    IXor(Val, Val),
    CMov(Val, Val),
    CMovG(Val, Val),
    CMovGE(Val, Val),
    CMovL(Val, Val),
    CMovLE(Val, Val),
    Label(String),
}

#[derive(Debug)]
enum Op1 { Add1, Sub1, IsNum, IsBool, }

#[derive(Debug)]
enum Op2 { Plus, Minus, Times, Equal, Greater, GreaterEqual, Less, LessEqual, }

#[derive(Debug)]
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

fn parse_expr(s: &Sexp) -> Expr {
    let keywords: HashSet<String> = 
      ["true", "false", "input", "let", "if", "block", "loop", "break", "set!", "block", "add1", "sub1", "isnum", "isbool", "+", "-", "*", "=", ">", ">=", "<", "<="]
      .iter().map(|s| s.to_string()).collect();

    let re = Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*$").unwrap();
    match s {
        Sexp::Atom(I(n)) => {
          let max_cobra: i64 =  (1 << 62) - 1;
          let min_cobra: i64 = !(1 << 62) + 1;
          println!("n: {}, upbound: {}, lowerbound: {}", *n, max_cobra, min_cobra);
          if *n > max_cobra || (*n < min_cobra) {
            panic!("Invalid number {}", *n);
          }
          Expr::Number(i64::try_from(*n).unwrap())
        },
        Sexp::Atom(S(literal)) => {
            if literal == "true" {
                Expr::Boolean(true)
            } else if literal == "false" {
                Expr::Boolean(false)
            } else if literal == "input" {
                Expr::Id(literal.clone())
            } else if re.is_match(literal) && !(keywords.contains(literal)) {
                Expr::Id(literal.clone())
            } else {
                panic!("Invalid id using keyword");
            }
        },
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(op)), e] if op == "add1" => Expr::UnOp(Op1::Add1, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "sub1" => Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "isnum" => Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "isbool" => Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e1, e2] if op == "+" => Expr::BinOp(Op2::Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "-" => Expr::BinOp(Op2::Minus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "*" => Expr::BinOp(Op2::Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "=" => Expr::BinOp(Op2::Equal, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == ">" => Expr::BinOp(Op2::Greater, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == ">=" => Expr::BinOp(Op2::GreaterEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "<" => Expr::BinOp(Op2::Less, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "<=" => Expr::BinOp(Op2::LessEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2, e3] if op == "if" => Expr::If(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)), Box::new(parse_expr(e3))),
                [Sexp::Atom(S(op)), e] if op == "loop" => Expr::Loop(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "break" => Expr::Break(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), Sexp::Atom(S(id)), e] if op == "set!" => {
                    if re.is_match(id) && !(keywords.contains(id)) {
                        Expr::Set(id.clone(), Box::new(parse_expr(e)))
                    } else {
                        panic!("Invalid id");
                    }
                },
                [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => {
                    if exprs.len() == 0 {
                        panic!("Invalid: block is empty");
                    }
                    Expr::Block(exprs.iter().map(|e| parse_expr(e)).collect())
                },
                [Sexp::Atom(S(op)), Sexp::List(bindings), e] if op == "let" => {
                    if bindings.len() == 0 {
                        panic!("Invalid: bindings is empty");
                    }
                    let mut seen: HashSet<String> = HashSet::new();
                    let bindings = bindings.iter().map(|binding| {
                        match binding {
                            Sexp::List(vec) => {
                                match &vec[..] {
                                    [Sexp::Atom(S(id)), e] => {
                                        if seen.contains(id) {
                                            panic!("Duplicate binding");
                                        }
                                        seen.insert(id.clone());
                                        (id.clone(), parse_expr(e))
                                    },
                                    _ => panic!("Invalid"),
                                }
                            },
                            _ => panic!("Invalid binding member"),
                        }
                    }).collect::<Vec<_>>();
                    Expr::Let(bindings, Box::new(parse_expr(e)))
                },
                _ => panic!("Invalid expr"),
            }
        },
        _ => panic!("Invalid expr"),
    }
}

fn compile_to_instrs(e: &Expr, si: i64, env : HashMap<String, i64>, l: &mut i32) -> Vec<Instr> {
    let mut instrs: Vec<Instr> = Vec::new();
    match e {
        Expr::BinOp(Op2::Equal, e1, e2) => {
            let mut instrs1 = compile_to_instrs(e1, si, env.clone(), l);
            let mut instrs2 = compile_to_instrs(e2, si + 1, env.clone(), l);
            let stack_offset: i64 = si * 8;
            instrs.append(&mut instrs1);
            instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
            instrs.append(&mut instrs2);
            instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
            instrs.push(Instr::IXor(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, stack_offset)));
            instrs.push(Instr::Test(Val::Reg(Reg::RBX), Val::Imm(1)));
            instrs.push(Instr::Jne("type_error".to_string()));
            instrs.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
            instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
            instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
            instrs.push(Instr::CMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
        },
        Expr::BinOp(op, e1, e2) => {
            let mut instrs1 = compile_to_instrs(e1, si, env.clone(), l);
            let mut instrs2 = compile_to_instrs(e2, si + 1, env.clone(), l);
            let stack_offset: i64 = si * 8;
            instrs.append(&mut instrs1);
            instrs.push(Instr::Test(Val::Reg(Reg::RAX),Val::Imm(1)));
            instrs.push(Instr::Jne("type_error".to_string()));
            instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
            instrs.append(&mut instrs2);
            instrs.push(Instr::Test(Val::Reg(Reg::RAX),Val::Imm(1)));
            instrs.push(Instr::Jne("type_error".to_string()));
            match op {
                Op2::Plus => {
                  instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
                  instrs.push(Instr::JO("overflow_error".to_string()));
                },
                Op2::Minus => {
                  instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                  instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
                  instrs.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                  instrs.push(Instr::JO("overflow_error".to_string())); // TODO: what if the sub result is max_int? 0-128 = -128, but 128 - 0 = 128 overflow happened. 
                },
                Op2::Times => {
                  instrs.push(Instr::ISar(Val::Reg(Reg::RAX), Val::Imm(1)));
                  instrs.push(Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
                  instrs.push(Instr::JO("overflow_error".to_string()));
                },
                Op2::Greater => {
                    instrs.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::CMovL(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                },
                Op2::Less => {
                    instrs.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::CMovG(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                },
                Op2::GreaterEqual => {
                    instrs.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::CMovLE(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                },
                Op2::LessEqual => {
                    instrs.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::CMovGE(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                },
                Op2::Equal => {
                    panic!("not possible");
                },
            }
        },
        Expr::UnOp(op, e) => {
            let mut instrs1 = compile_to_instrs(e, si, env, l);
            instrs.append(&mut instrs1);
            match op {
                Op1::Add1 => {
                  instrs.push(Instr::Test(Val::Reg(Reg::RAX),Val::Imm(1)));
                  instrs.push(Instr::Jne("type_error".to_string()));
                  instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(2)));
                  instrs.push(Instr::JO("overflow_error".to_string()));
                },
                Op1::Sub1 => {
                  instrs.push(Instr::Test(Val::Reg(Reg::RAX),Val::Imm(1)));
                  instrs.push(Instr::Jne("type_error".to_string()));
                  instrs.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(2)));
                  instrs.push(Instr::JO("overflow_error".to_string()));
                },
                Op1::IsBool => {
                    instrs.push(Instr::IAnd(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::IShl(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1)));
                },
                Op1::IsNum => {
                    instrs.push(Instr::IAnd(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::IXor(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::IShl(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1)));
                },
            }
        },
        Expr::Number(n) => {
            instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n * 2)));
        },
        Expr::Boolean(b) => {
            instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(if *b { 3 } else { 1 })));
        },
        Expr::Id(id) => {
            if id == "input" {
              instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI)));
            } else {
              let v = env.get(id).unwrap_or_else(|| panic!("Unbound variable identifier {}", id));
              instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, *v)));
            }
        },
        Expr::Let(bindings, e) => {
            let mut nenv = env.clone();
            let mut cursi = si;
            for (id, e) in bindings {
                if id == "input" {
                  panic!("Invalid: input is a reserved keyword");
                }
                let mut instrs1 = compile_to_instrs(e, cursi, nenv.clone(), l);
                nenv = nenv.update(id.clone(), cursi * 8);                
                instrs.append(&mut instrs1);
                instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, cursi * 8), Val::Reg(Reg::RAX)));
                cursi += 1;
            }
            let mut instrs1 = compile_to_instrs(e, cursi, nenv, l);
            instrs.append(&mut instrs1);
        },
        Expr::Block(exprs) => {
            for e in exprs {
                let mut instrs1 = compile_to_instrs(e, si, env.clone(), l);
                instrs.append(&mut instrs1);
            }
        },
        Expr::If(cond, e1, e2) => {
            let else_label = format!("else_{}", *l);
            let end_label = format!("endif_{}", *l);
            *l += 1;

            let mut instrs1 = compile_to_instrs(cond, si, env.clone(), l);
            let mut instrs2 = compile_to_instrs(e1, si, env.clone(), l);
            let mut instrs3 = compile_to_instrs(e2, si, env.clone(), l);
            instrs.append(&mut instrs1);
            instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(1)));
            instrs.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
            instrs.push(Instr::Je(else_label.clone()));
            instrs.append(&mut instrs2);
            instrs.push(Instr::Jmp(end_label.clone()));
            instrs.push(Instr::Label(else_label.clone()));
            instrs.append(&mut instrs3);
            instrs.push(Instr::Label(end_label.clone()));
        },
        Expr::Loop(expr) => {
            let loopi = env.get("loop").unwrap();
            let mut nenv = env.clone();
            nenv.insert("loop".to_string(), (*l).into());
            let loop_label = format!("loop_{}", *l);
            let end_label = format!("endloop_{}", *l);
            *l += 1;

            let mut instrs1 = compile_to_instrs(expr, si, nenv.clone(), l);
            instrs.push(Instr::Label(loop_label.clone()));
            instrs.append(&mut instrs1);
            instrs.push(Instr::Jmp(loop_label.clone()));
            instrs.push(Instr::Label(end_label.clone()));
        },
        Expr::Break(expr) => {
            let loopi = env.get("loop").unwrap();
            if (*loopi) == -1 {
                panic!("break outside of loop");
            }
            let mut instrs1 = compile_to_instrs(expr, si, env.clone(), l);
            let loop_label = format!("endloop_{}", loopi);
            instrs.append(&mut instrs1);
            instrs.push(Instr::Jmp(loop_label.clone()));
        },
        Expr::Set(id, expr) => {

            let v = env.get(id).unwrap_or_else(|| panic!("Unbound variable identifier {}", id));
            let mut instrs1 = compile_to_instrs(expr, si, env.clone(), l);
            instrs.append(&mut instrs1);
            instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, *v), Val::Reg(Reg::RAX)));
        },
      }
    return instrs;
}

fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(dst, src) => format!("mov {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IAdd(dst, src) => format!("add {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::ISub(dst, src) => format!("sub {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IMul(dst, src) => format!("imul {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::INeg(dst) => format!("neg {}", val_to_str(dst)),
        Instr::IAnd(dst, src) => format!("and {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IXor(dst, src) => format!("xor {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IShl(dst, src) => format!("shl {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::ISar(dst, src) => format!("sar {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IShr(dst, src) => format!("shr {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::Cmp(dst, src) => format!("cmp {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::Jmp(label) => format!("jmp {}", label),
        Instr::CMovL(dst, src) => format!("cmovl {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::CMovLE(dst, src) => format!("cmovle {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::CMovG(dst, src) => format!("cmovg {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::CMovGE(dst, src) => format!("cmovge {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::CMov(dst, src) => format!("cmove {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::Je(label) => format!("je {}", label),
        Instr::Jne(label) => format!("jne {}", label),
        Instr::Jg(label) => format!("jl {}", label),
        Instr::Jge(label) => format!("jle {}", label),
        Instr::JO(label) => format!("jo {}", label),
        Instr::Setz(dst) => format!("setz {}", val_to_str(dst)),
        Instr::Test(dst, src) => format!("test {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::Label(label) => format!("{}:", label),
    }
}

fn val_to_str(v: &Val) -> String {
    match v {
        Val::Reg(r) => {
            match r {
                Reg::RAX => "rax".to_string(),
                Reg::RSP => "rsp".to_string(),
                Reg::RBX => "rbx".to_string(),
                Reg::EAX => "eax".to_string(),
                Reg::RDI => "rdi".to_string(),
            }
        },
        Val::Imm(n) => n.to_string(),
        Val::RegOffset(Reg::RSP, n) => format!("[rsp - {}]", n),
        _ => panic!("val to str error"),
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    // You will make result hold the result of actually compiling

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let s: Sexp = parse(&in_contents).unwrap_or_else(|e| panic!("Invalid. Parse error: {}", e));
    let expr = parse_expr(&s);
    println!("{:?}", expr);

    let env: HashMap<String, i64> = {
      let mut m = HashMap::new();
      m.insert("loop".to_string(), -1);
      m.insert("break".to_string(), 0);
      m.insert("if".to_string(), 0);
      m
    };
    let mut l = 0;
    let instrs = compile_to_instrs(&expr, 2, env, &mut l);
    println!("{:?}", instrs);

    let result = instrs.iter().map(|i| instr_to_str(i)).collect::<Vec<_>>().join("\n  ");

    let asm_program = format!(
        "
section .text
extern snek_error
global our_code_starts_here
type_error:
  mov rdi, 2
  push rsp
  call snek_error

overflow_error:
  mov rdi, 1
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
