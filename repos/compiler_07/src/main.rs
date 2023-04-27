use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::Atom::*;
use sexp::*;

use im::{HashMap, HashSet};

#[derive(Debug)]
enum Val {
    Reg(Reg),
    Imm(i64),
    Num(i32), // immediate, but w/o shift
    Bool(bool),
    RegOffset(Reg, i32),
    Label(String),
}

#[derive(Debug)]
enum Reg {
    RAX,
    RSP,
    RBX,
    RDI,
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    ISar(Val, Val),
    ITest(Val, Val),
    ICMovE(Val, Val),
    ICMovG(Val, Val),
    ICMovGE(Val, Val),
    ICMovL(Val, Val),
    ICMovLE(Val, Val),
    IXor(Val, Val),
    IJne(Val),
    IJe(Val),
    IJmp(Val),
    IJo(Val),
    ICmp(Val, Val),
    IOr(Val, Val),
    Label(Val),
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
    Number(i64),
    Boolean(bool),
    Input,
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

enum Errcode {
    InvalidArg = 1,
    Overflow = 2,
}

const TAG_LEN: i32 = 1;
const I63_MAX: i64 = 4611686018427387903;
const I63_MIN: i64 = -4611686018427387904;
const ERROR_LABEL: &str = "throw_error";
const KEYWORDS: &'static [&'static str] = &[
    "let", "add1", "sub1", "isnum", "isbool", "true", "false", "input", "set!", "if", "block",
    "loop", "break",
];

fn parse_bind(s: &Sexp) -> (String, Expr) {
    match s {
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(op)), e] => {
                if KEYWORDS.contains(&op.as_str()) {
                    panic!("{op} is a keyword and cannot be used as variable name")
                }
                (op.to_string(), parse_expr(e))
            }
            _ => panic!("Invalid let syntax 1"),
        },
        _ => panic!("Invalid let syntax 2"),
    }
}

fn new_label(l: &mut i32, s: &str) -> String {
    let current = *l;
    *l += 1;
    format!("{s}_{current}")
}

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => {
            let num = i64::try_from(*n).unwrap();
            if num < I63_MIN || num > I63_MAX {
                panic!("Invalid argument {num}")
            }
            Expr::Number(num)
        }
        Sexp::Atom(S(str)) => match str.as_str() {
            "true" => Expr::Boolean(true),
            "false" => Expr::Boolean(false),
            "input" => Expr::Input,
            _ => Expr::Id(str.to_string()),
        },
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(op)), e] if op == "add1" => {
                Expr::UnOp(Op1::Add1, Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), e] if op == "sub1" => {
                Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), e] if op == "isnum" => {
                Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), e] if op == "isbool" => {
                Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "+" => Expr::BinOp(
                Op2::Plus,
                Box::new(parse_expr(e1)),
                Box::new(parse_expr(e2)),
            ),
            [Sexp::Atom(S(op)), e1, e2] if op == "-" => Expr::BinOp(
                Op2::Minus,
                Box::new(parse_expr(e1)),
                Box::new(parse_expr(e2)),
            ),
            [Sexp::Atom(S(op)), e1, e2] if op == "*" => Expr::BinOp(
                Op2::Times,
                Box::new(parse_expr(e1)),
                Box::new(parse_expr(e2)),
            ),
            [Sexp::Atom(S(op)), e1, e2] if op == "=" => Expr::BinOp(
                Op2::Equal,
                Box::new(parse_expr(e1)),
                Box::new(parse_expr(e2)),
            ),
            [Sexp::Atom(S(op)), e1, e2] if op == ">" => Expr::BinOp(
                Op2::Greater,
                Box::new(parse_expr(e1)),
                Box::new(parse_expr(e2)),
            ),
            [Sexp::Atom(S(op)), e1, e2] if op == ">=" => Expr::BinOp(
                Op2::GreaterEqual,
                Box::new(parse_expr(e1)),
                Box::new(parse_expr(e2)),
            ),
            [Sexp::Atom(S(op)), e1, e2] if op == "<" => Expr::BinOp(
                Op2::Less,
                Box::new(parse_expr(e1)),
                Box::new(parse_expr(e2)),
            ),
            [Sexp::Atom(S(op)), e1, e2] if op == "<=" => Expr::BinOp(
                Op2::LessEqual,
                Box::new(parse_expr(e1)),
                Box::new(parse_expr(e2)),
            ),
            [Sexp::Atom(S(op)), Sexp::List(binds), body] if op == "let" => match binds[..] {
                [] => panic!("parse error: Invalid let expression - no binding"),
                _ => {
                    let defs = binds.iter().map(|b| parse_bind(b)).collect();
                    Expr::Let(defs, Box::new(parse_expr(body)))
                }
            },
            [Sexp::Atom(S(op)), e1, e2, e3] if op == "if" => Expr::If(
                Box::new(parse_expr(e1)),
                Box::new(parse_expr(e2)),
                Box::new(parse_expr(e3)),
            ),
            [Sexp::Atom(S(op)), Sexp::Atom(S(name)), e1] if op == "set!" => {
                Expr::Set(name.to_string(), Box::new(parse_expr(e1)))
            }
            [Sexp::Atom(S(op)), es @ ..] if op == "block" && es.len() >= 1 => {
                Expr::Block(es.iter().map(|expr| parse_expr(expr)).collect())
            }
            [Sexp::Atom(S(op)), e] if op == "loop" => Expr::Loop(Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "break" => Expr::Break(Box::new(parse_expr(e))),
            _ => panic!("parse error: Invalid operator {}", s),
        },
        _ => panic!("parse error: Invalid Sexp {}", s),
    }
}

fn check_duplicate_binding(binds: &Vec<(String, Expr)>) {
    let mut set = HashSet::new();
    let unique = binds.iter().all(|(id, _)| match set.insert(id) {
        None => true,
        Some(_) => false,
    });
    if !unique {
        panic!("Duplicate binding")
    }
}

fn overflow_instrs() -> Vec<Instr> {
    vec![
        Instr::IMov(Val::Reg(Reg::RBX), Val::Num(Errcode::Overflow as i32)),
        Instr::IJo(Val::Label(ERROR_LABEL.to_string())),
    ]
}

fn compile_to_instrs(
    e: &Expr,
    si: i32,
    env: &HashMap<String, i32>,
    l: &mut i32,
    b_target: &str,
) -> Vec<Instr> {
    match e {
        Expr::Number(n) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n))],
        Expr::Boolean(b) => match b {
            true => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Bool(true))],
            false => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Bool(false))],
        },
        Expr::Input => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI))],
        Expr::UnOp(op, e) => {
            let mut is = compile_to_instrs(e, si, env, l, b_target);
            match op {
                Op1::Add1 | Op1::Sub1 => {
                    //type check
                    is.append(&mut vec![
                        Instr::ITest(Val::Reg(Reg::RAX), Val::Num(1)),
                        Instr::IMov(Val::Reg(Reg::RBX), Val::Num(Errcode::InvalidArg as i32)),
                        Instr::IJne(Val::Label(ERROR_LABEL.to_string())),
                    ]);
                    match op {
                        Op1::Add1 => {
                            is.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1)));
                        }
                        Op1::Sub1 => {
                            is.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(1)));
                        }
                        _ => panic!(" (つД｀)･ﾟ･"),
                    }
                    is.append(&mut overflow_instrs());
                }
                Op1::IsNum => {
                    is.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Num(1)));
                    is.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Bool(false)));
                    is.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Bool(true)));
                    is.push(Instr::ICMovE(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                }
                Op1::IsBool => {
                    is.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Num(1)));
                    is.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Bool(true)));
                    is.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Bool(false)));
                    is.push(Instr::ICMovE(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                }
            }
            is
        }
        Expr::BinOp(op, e1, e2) => {
            let mut is1 = compile_to_instrs(e1, si, env, l, b_target);
            let mut is2 = compile_to_instrs(e2, si + 1, env, l, b_target);
            is1.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, si),
                Val::Reg(Reg::RAX),
            ));
            is1.append(&mut is2);

            // type check
            let cmp_instr = match op {
                Op2::Equal => Instr::IXor(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, si)), // same type
                _ => Instr::IOr(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, si)), // numbers
            };
            is1.append(&mut vec![
                Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)),
                cmp_instr,
                Instr::ITest(Val::Reg(Reg::RBX), Val::Num(1)),
                Instr::IMov(Val::Reg(Reg::RBX), Val::Num(Errcode::InvalidArg as i32)),
                Instr::IJne(Val::Label(ERROR_LABEL.to_string())),
            ]);

            match op {
                Op2::Plus | Op2::Minus | Op2::Times => {
                    match op {
                        Op2::Plus => {
                            is1.push(Instr::IAdd(
                                Val::Reg(Reg::RAX),
                                Val::RegOffset(Reg::RSP, si),
                            ));
                        }
                        Op2::Minus => {
                            is1.push(Instr::IMov(
                                Val::RegOffset(Reg::RSP, si + 1),
                                Val::Reg(Reg::RAX),
                            ));
                            is1.push(Instr::IMov(
                                Val::Reg(Reg::RAX),
                                Val::RegOffset(Reg::RSP, si),
                            ));
                            is1.push(Instr::ISub(
                                Val::Reg(Reg::RAX),
                                Val::RegOffset(Reg::RSP, si + 1),
                            ));
                        }
                        Op2::Times => {
                            is1.push(Instr::ISar(Val::Reg(Reg::RAX), Val::Num(TAG_LEN)));
                            is1.push(Instr::IMul(
                                Val::Reg(Reg::RAX),
                                Val::RegOffset(Reg::RSP, si),
                            ));
                        }
                        _ => panic!("|ू･ω･` )"),
                    }
                    is1.append(&mut overflow_instrs());
                }
                Op2::Equal | Op2::Greater | Op2::GreaterEqual | Op2::Less | Op2::LessEqual => {
                    let cmov_instr = match op {
                        Op2::Equal => Instr::ICMovE(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)),
                        Op2::Greater => Instr::ICMovG(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)),
                        Op2::GreaterEqual => Instr::ICMovGE(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)),
                        Op2::Less => Instr::ICMovL(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)),
                        Op2::LessEqual => Instr::ICMovLE(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)),
                        _ => panic!("┓( ´∀` )┏"),
                    };
                    is1.append(&mut vec![
                        Instr::ICmp(Val::RegOffset(Reg::RSP, si), Val::Reg(Reg::RAX)),
                        Instr::IMov(Val::Reg(Reg::RBX), Val::Bool(true)),
                        Instr::IMov(Val::Reg(Reg::RAX), Val::Bool(false)),
                        cmov_instr,
                    ]);
                }
            }
            is1
        }
        Expr::Id(id) => {
            let val = env.get(id);
            match val {
                Some(v) => vec![Instr::IMov(
                    Val::Reg(Reg::RAX),
                    Val::RegOffset(Reg::RSP, *v),
                )],
                None => panic!("Unbound variable identifier {id}"),
            }
        }
        Expr::Let(binds, body) => {
            check_duplicate_binding(binds);
            let mut bind_instr = Vec::new();
            let mut si = si;
            let mut env = env.clone();
            for (id, e) in binds {
                bind_instr.append(&mut compile_to_instrs(e, si, &env, l, b_target));
                bind_instr.push(Instr::IMov(
                    Val::RegOffset(Reg::RSP, si),
                    Val::Reg(Reg::RAX),
                ));
                env = env.update(id.to_string(), si);
                si += 1;
            }
            bind_instr.append(&mut compile_to_instrs(body, si, &env, l, b_target));
            bind_instr
        }
        Expr::If(cond, then, els) => {
            let end_label = new_label(l, "ifend");
            let else_label = new_label(l, "ifelse");
            let mut cond_instrs = compile_to_instrs(cond, si, env, l, b_target);
            let mut then_instrs = compile_to_instrs(then, si, env, l, b_target);
            let mut else_instrs = compile_to_instrs(els, si, env, l, b_target);
            let mut is: Vec<Instr> = vec![];
            is.append(&mut cond_instrs);
            is.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Bool(false)));
            is.push(Instr::IJe(Val::Label(else_label.clone())));
            is.append(&mut then_instrs);
            is.push(Instr::IJmp(Val::Label(end_label.clone())));
            is.push(Instr::Label(Val::Label(else_label)));
            is.append(&mut else_instrs);
            is.push(Instr::Label(Val::Label(end_label)));
            is
        }
        Expr::Loop(expr) => {
            let loop_label = new_label(l, "loop");
            let end_label = new_label(l, "loop_end");
            let mut is = vec![Instr::Label(Val::Label(loop_label.clone()))];
            is.append(&mut compile_to_instrs(expr, si, env, l, end_label.as_str()));
            is.push(Instr::IJmp(Val::Label(loop_label)));
            is.push(Instr::Label(Val::Label(end_label)));
            is
        }
        Expr::Break(expr) => {
            if b_target.is_empty() {
                panic!("Cannot break from outside of loop")
            }
            let mut is = compile_to_instrs(expr, si, env, l, b_target);
            is.push(Instr::IJmp(Val::Label(b_target.to_string())));
            is
        }
        Expr::Set(id, expr) => {
            let addr = match env.get(id) {
                None => panic!("Unbound variable identifier {id}"),
                Some(addr) => addr,
            };
            let mut is = compile_to_instrs(expr, si, env, l, b_target);
            is.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, *addr),
                Val::Reg(Reg::RAX),
            ));
            is
        }
        Expr::Block(es) => es
            .iter()
            .map(|expr| compile_to_instrs(expr, si, env, l, b_target))
            .flatten()
            .collect(),
    }
}

fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(v1, v2) => format!("\n  mov {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::IAdd(v1, v2) => format!("\n  add {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ISub(v1, v2) => format!("\n  sub {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::IMul(v1, v2) => format!("\n  imul {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ISar(v1, v2) => format!("\n  sar {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ITest(v1, v2) => format!("\n  test {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ICMovE(v1, v2) => format!("\n  cmove {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ICMovG(v1, v2) => format!("\n  cmovg {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ICMovGE(v1, v2) => format!("\n  cmovge {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ICMovL(v1, v2) => format!("\n  cmovl {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ICMovLE(v1, v2) => format!("\n  cmovle {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::IXor(v1, v2) => format!("\n  xor {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::IJne(v) => format!("\n  jne {}", val_to_str(v)),
        Instr::IJe(v) => format!("\n  je {}", val_to_str(v)),
        Instr::IJmp(v) => format!("\n  jmp {}", val_to_str(v)),
        Instr::IJo(v) => format!("\n  jo {}", val_to_str(v)),
        Instr::ICmp(v1, v2) => format!("\n  cmp {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::IOr(v1, v2) => format!("\n  or {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::Label(v) => format!("\n{}:", val_to_str(v)),
    }
}

fn val_to_str(v: &Val) -> String {
    match v {
        Val::Imm(n) => (n << TAG_LEN).to_string(),
        Val::Num(n) => n.to_string(),
        Val::Bool(b) => {
            if *b {
                "3".to_string()
            } else {
                "1".to_string()
            }
        }
        Val::Reg(Reg::RAX) => format!("rax"),
        Val::Reg(Reg::RBX) => format!("rbx"),
        Val::Reg(Reg::RDI) => format!("rdi"),
        Val::RegOffset(Reg::RSP, n) => format!("[rsp - {}]", n * 8),
        Val::Label(s) => format!("{s}"),
        _ => panic!("Invalid Val: {:?}", v),
    }
}

fn compile_expr(e: &Expr) -> String {
    let is = compile_to_instrs(e, 2, &HashMap::new(), &mut 0, "");
    let s: Vec<String> = is.iter().map(|i| instr_to_str(i)).collect();
    s.concat()
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    // You will make result hold the result of actually compiling
    let expr = parse_expr(&parse(&in_contents).expect("Invalid syntax"));
    let result = compile_expr(&expr);

    let asm_program = format!(
        "
section .text
extern snek_error
global our_code_starts_here
throw_error:
    mov rdi, rbx
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
