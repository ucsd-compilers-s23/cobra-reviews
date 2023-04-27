use std::env;
use std::fmt::Debug;
use std::fs::File;
use std::io::prelude::*;
use std::collections::HashSet;

use sexp::Atom::*;
use sexp::*;

use im::HashMap;

const I63_MAX: i64 = 0x3fff_ffff_ffff_ffff;
const I63_MIN: i64 = -0x4000_0000_0000_0000;
const ENTRY_FUNC: &str = "our_code_starts_here";
const ERR_FUNC: &str = "snek_error";
const WORD_SIZE: i32 = 8;
const VAR_MAX: i32 = i32::MAX / WORD_SIZE;

#[derive(Debug)]
enum Val {
    Reg(Reg),
    Imm(i64),
    Byte(i8),
    RegOffset(Reg, i32),
}

#[derive(Debug)]
enum Reg {
    RAX,
    R11,
    RDI,
    RSP,
    RBP,
    AL,
    R11B,
}

#[derive(Debug)]
enum Instr {
    Mov(Val, Val),
    Add(Val, Val),
    Sub(Val, Val),
    Imul(Val, Val),
    Xor(Val, Val),
    Sar(Val, Val),
    Cmp(Val, Val),
    Test(Val, Val),
    Cmove(Val, Val),
    Cmovg(Val, Val),
    Cmovge(Val, Val),
    Cmovl(Val, Val),
    Cmovle(Val, Val),
    Push(Val),
    Label(String),
    Call(String),
    Jmp(String),
    Je(String),
    Jne(String),
    Jno(String),
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

fn parse_sexp(str: &String) -> Sexp {
    let res = parse(str);
    match res {
        Ok(s) =>
            s,
        Err(_) => panic!("Invalid syntax"),
    }
}

fn parse_bind(s: &Sexp) -> (String, Expr) {
    match s {
        Sexp::List(vec) =>
            match vec.as_slice() {
                [Sexp::Atom(S(x)), e] =>
                    match x.as_str() {
                        "add1" | "sub1" | "isnum" | "isbool" |
                        "true" | "false" | "input" |
                        "let" | "if" | "block" | "loop" | "break" =>
                            panic!("Invalid name: keyword"),
                        _ =>
                            (x.to_string(), parse_expr(e)),
                    },
                _ => panic!("Invalid binding"),
            },
        _ => panic!("Invalid binding"),
    }
}

fn parse_expr(s : &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => {
            let num = i64::try_from(*n);
            match num {
                Ok(i) =>
                    if i > I63_MAX || i < I63_MIN {
                        panic!("Invalid number")
                    } else {
                        Expr::Number(i)
                    },
                Err(_) => panic!("Invalid number"),
            }
        }
        Sexp::Atom(S(x)) =>
            match x.as_str() {
                "add1" | "sub1" | "isnum" | "isbool" |
                "let" | "if" | "block" | "loop" | "break" =>
                    panic!("Invalid name: keyword"),
                "true" =>
                    Expr::Boolean(true),
                "false" =>
                    Expr::Boolean(false),
                "input" =>
                    Expr::Input,
                _ =>
                    Expr::Id(x.to_string()),
            },
        Sexp::List(vec) =>
            match vec.as_slice() {
                [Sexp::Atom(S(op)), es @ ..] if op == "block" =>
                    if es.len() > 0 {
                        Expr::Block(
                            es.iter().map(|e| parse_expr(e)).collect(),
                        )
                    } else {
                        panic!("Invalid syntax")
                    },
                [Sexp::Atom(S(op)), e] =>
                    match op.as_str() {
                        "add1" =>
                            Expr::UnOp(
                                Op1::Add1,
                                Box::new(parse_expr(e)),
                            ),
                        "sub1" =>
                            Expr::UnOp(
                                Op1::Sub1,
                                Box::new(parse_expr(e)),
                            ),
                        "isnum" =>
                            Expr::UnOp(
                                Op1::IsNum,
                                Box::new(parse_expr(e)),
                            ),
                        "isbool" =>
                            Expr::UnOp(
                                Op1::IsBool,
                                Box::new(parse_expr(e)),
                            ),
                        "loop" =>
                            Expr::Loop(
                                Box::new(parse_expr(e)),
                            ),
                        "break" =>
                            Expr::Break(
                                Box::new(parse_expr(e)),
                            ),
                        _ => panic!("Invalid op"),
                    },
                [Sexp::Atom(S(op)), e1, e2] =>
                    match op.as_str() {
                        "+" =>
                            Expr::BinOp(
                                Op2::Plus,
                                Box::new(parse_expr(e1)),
                                Box::new(parse_expr(e2)),
                            ),
                        "-" =>
                            Expr::BinOp(
                                Op2::Minus,
                                Box::new(parse_expr(e1)),
                                Box::new(parse_expr(e2)),
                            ),
                        "*" =>
                            Expr::BinOp(
                                Op2::Times,
                                Box::new(parse_expr(e1)),
                                Box::new(parse_expr(e2)),
                            ),
                        "=" =>
                            Expr::BinOp(
                                Op2::Equal,
                                Box::new(parse_expr(e1)),
                                Box::new(parse_expr(e2)),
                            ),
                        ">" =>
                            Expr::BinOp(
                                Op2::Greater,
                                Box::new(parse_expr(e1)),
                                Box::new(parse_expr(e2)),
                            ),
                        ">=" =>
                            Expr::BinOp(
                                Op2::GreaterEqual,
                                Box::new(parse_expr(e1)),
                                Box::new(parse_expr(e2)),
                            ),
                        "<" =>
                            Expr::BinOp(
                                Op2::Less,
                                Box::new(parse_expr(e1)),
                                Box::new(parse_expr(e2)),
                            ),
                        "<=" =>
                            Expr::BinOp(
                                Op2::LessEqual,
                                Box::new(parse_expr(e1)),
                                Box::new(parse_expr(e2)),
                            ),
                        "let" =>
                            match e1 {
                                Sexp::List(v) if v.len() > 0 =>
                                    Expr::Let(
                                        v.iter().map(|b| parse_bind(b)).collect(),
                                        Box::new(parse_expr(e2)),
                                    ),
                                _ => panic!("Invalid binding"),
                            },
                        "set!" =>
                            match e1 {
                                Sexp::Atom(S(x)) =>
                                    match x.as_str() {
                                        "add1" | "sub1" | "isnum" | "isbool" |
                                        "true" | "false" | "input" |
                                        "let" | "if" | "block" | "loop" | "break" =>
                                            panic!("Invalid name: keyword"),
                                        _ =>
                                            Expr::Set(
                                                x.to_string(),
                                                Box::new(parse_expr(e2)),
                                            ),
                                    },
                                _ => panic!("Invalid id"),
                            },
                        _ => panic!("Invalid op"),
                    },
                [Sexp::Atom(S(op)), e1, e2, e3] =>
                    match op.as_str() {
                        "if" =>
                            Expr::If(
                                Box::new(parse_expr(e1)),
                                Box::new(parse_expr(e2)),
                                Box::new(parse_expr(e3)),
                            ),
                        _ => panic!("Invalid op"),
                    },
                _ => panic!("Invalid syntax"),
            },
        _ => panic!("Invalid syntax"),
    }
}

fn val_to_str(v: &Val) -> String {
    match v {
        Val::Reg(Reg::RAX) =>
            String::from("rax"),
        Val::Reg(Reg::R11) =>
            String::from("r11"),
        Val::Reg(Reg::RDI) =>
            String::from("rdi"),
        Val::Reg(Reg::RBP) =>
            String::from("rbp"),
        Val::Reg(Reg::AL) =>
            String::from("al"),
        Val::Reg(Reg::R11B) =>
            String::from("r11b"),
        Val::Imm(n) =>
            format!("qword {n}"),
        Val::Byte(n) =>
            format!("byte {n}"),
        Val::RegOffset(Reg::RSP, n) => {
            if *n > VAR_MAX {
                panic!("Internal error: too many variables")
            }
            let offset = n * WORD_SIZE;
            format!("qword [rsp - {offset}]")
        }
        _ => panic!("Internal error: unknown value conversion"),
    }
}

fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::Mov(v1, v2) =>
            format!("mov {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::Add(v1, v2) =>
            format!("add {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::Sub(v1, v2) =>
            format!("sub {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::Imul(v1, v2) =>
            format!("imul {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::Xor(v1, v2) =>
            format!("xor {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::Sar(v1, v2) =>
            format!("sar {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::Cmp(v1, v2) =>
            format!("cmp {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::Test(v1, v2) =>
            format!("test {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::Cmove(v1, v2) =>
            format!("cmove {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::Cmovg(v1, v2) =>
            format!("cmovg {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::Cmovge(v1, v2) =>
            format!("cmovge {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::Cmovl(v1, v2) =>
            format!("cmovl {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::Cmovle(v1, v2) =>
            format!("cmovle {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::Push(v) =>
            format!("push {}", val_to_str(v)),
        Instr::Label(s) =>
            format!("{s}:"),
        Instr::Call(s) =>
            format!("call {s}"),
        Instr::Jmp(s) =>
            format!("jmp {s}"),
        Instr::Je(s) =>
            format!("je {s}"),
        Instr::Jne(s) =>
            format!("jne {s}"),
        Instr::Jno(s) =>
            format!("jno {s}"),
    }
}

fn new_label(l: &mut i32, s: &str) -> String {
    let current = *l;
    if current == i32::MAX {
        panic!("Internal error: too many labels")
    }
    *l += 1;
    format!("{s}_{current}")
}

fn compile_expr_instrs(e: &Expr, si: i32, env: &HashMap<String, i32>, l: &mut i32, bt: &str,
                       cmds: &mut Vec<Instr>) {
    match e {
        Expr::Number(n) =>
            cmds.push(Instr::Mov(
                Val::Reg(Reg::RAX),
                Val::Imm(*n << 1),
            )),
        Expr::Boolean(b) =>
            match b {
                true =>
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::RAX),
                        Val::Imm(3),
                    )),
                false =>
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::RAX),
                        Val::Imm(1),
                    )),
            },
        Expr::Input =>
            cmds.push(Instr::Mov(
                Val::Reg(Reg::RAX),
                Val::Reg(Reg::RDI),
            )),
        Expr::Id(x) => {
            let opt = env.get(x.as_str());
            match opt {
                Some(i) =>
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, *i),
                    )),
                None => panic!("Unbound variable identifier {x}"),
            }
        }
        Expr::Let(binds, body) => {
            let mut new_env = env.clone();
            let mut new_si = si;
            let mut names: HashSet<String> = HashSet::new();
            for (x, e) in binds {
                if names.contains(x.as_str()) {
                    panic!("Duplicate binding")
                }
                compile_expr_instrs(e, new_si, &new_env, l, bt, cmds);
                cmds.push(Instr::Mov(
                    Val::RegOffset(Reg::RSP, new_si),
                    Val::Reg(Reg::RAX),
                ));
                new_env = new_env.update(x.to_string(), new_si);
                new_si += 1;
                names.insert(x.to_string());
            }
            compile_expr_instrs(body, new_si, &new_env, l, bt, cmds)
        }
        Expr::UnOp(op1, e) => {
            compile_expr_instrs(e, si, env, l, bt, cmds);
            match op1 {
                Op1::Add1 => {
                    cmds.push(Instr::Test(
                        Val::Reg(Reg::AL),
                        Val::Byte(1),
                    ));
                    let error_1 = new_label(l, "error");
                    cmds.push(Instr::Jne(
                        error_1.clone(),
                    ));
                    cmds.push(Instr::Add(
                        Val::Reg(Reg::RAX),
                        Val::Imm(2),
                    ));
                    let label = new_label(l, "label");
                    cmds.push(Instr::Jno(
                        label.clone(),
                    ));
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::RDI),
                        Val::Imm(4),
                    ));
                    let error_2 = new_label(l, "error");
                    cmds.push(Instr::Jmp(
                        error_2.clone(),
                    ));
                    cmds.push(Instr::Label(
                        error_1,
                    ));
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::RDI),
                        Val::Imm(14),
                    ));
                    cmds.push(Instr::Label(
                        error_2,
                    ));
                    cmds.push(Instr::Push(
                        Val::Reg(Reg::RBP),
                    ));
                    cmds.push(Instr::Call(
                        ERR_FUNC.to_string(),
                    ));
                    cmds.push(Instr::Label(
                        label,
                    ))
                }
                Op1::Sub1 => {
                    cmds.push(Instr::Test(
                        Val::Reg(Reg::AL),
                        Val::Byte(1),
                    ));
                    let error_1 = new_label(l, "error");
                    cmds.push(Instr::Jne(
                        error_1.clone(),
                    ));
                    cmds.push(Instr::Sub(
                        Val::Reg(Reg::RAX),
                        Val::Imm(2),
                    ));
                    let label = new_label(l, "label");
                    cmds.push(Instr::Jno(
                        label.clone(),
                    ));
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::RDI),
                        Val::Imm(5),
                    ));
                    let error_2 = new_label(l, "error");
                    cmds.push(Instr::Jmp(
                        error_2.clone(),
                    ));
                    cmds.push(Instr::Label(
                        error_1,
                    ));
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::RDI),
                        Val::Imm(15),
                    ));
                    cmds.push(Instr::Label(
                        error_2,
                    ));
                    cmds.push(Instr::Push(
                        Val::Reg(Reg::RBP),
                    ));
                    cmds.push(Instr::Call(
                        ERR_FUNC.to_string(),
                    ));
                    cmds.push(Instr::Label(
                        label,
                    ))
                }
                Op1::IsBool => {
                    cmds.push(Instr::Test(
                        Val::Reg(Reg::AL),
                        Val::Byte(1),
                    ));
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::R11),
                        Val::Imm(1),
                    ));
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::RAX),
                        Val::Imm(3),
                    ));
                    cmds.push(Instr::Cmove(
                        Val::Reg(Reg::RAX),
                        Val::Reg(Reg::R11),
                    ))
                }
                Op1::IsNum => {
                    cmds.push(Instr::Test(
                        Val::Reg(Reg::AL),
                        Val::Byte(1),
                    ));
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::R11),
                        Val::Imm(3),
                    ));
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::RAX),
                        Val::Imm(1),
                    ));
                    cmds.push(Instr::Cmove(
                        Val::Reg(Reg::RAX),
                        Val::Reg(Reg::R11),
                    ))
                }
            }
        }
        Expr::BinOp(op2, e1, e2) => {
            compile_expr_instrs(e2, si, env, l, bt, cmds);
            cmds.push(Instr::Mov(
                Val::RegOffset(Reg::RSP, si),
                Val::Reg(Reg::RAX),
            ));
            compile_expr_instrs(e1, si + 1, env, l, bt, cmds);
            match op2 {
                Op2::Plus => {
                    cmds.push(Instr::Test(
                        Val::Reg(Reg::AL),
                        Val::Byte(1),
                    ));
                    let error_1 = new_label(l, "error");
                    cmds.push(Instr::Jne(
                        error_1.clone(),
                    ));
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::R11),
                        Val::RegOffset(Reg::RSP, si),
                    ));
                    cmds.push(Instr::Test(
                        Val::Reg(Reg::R11B),
                        Val::Byte(1),
                    ));
                    cmds.push(Instr::Jne(
                        error_1.clone(),
                    ));
                    cmds.push(Instr::Add(
                        Val::Reg(Reg::RAX),
                        Val::Reg(Reg::R11),
                    ));
                    let label = new_label(l, "label");
                    cmds.push(Instr::Jno(
                        label.clone(),
                    ));
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::RDI),
                        Val::Imm(6),
                    ));
                    let error_2 = new_label(l, "error");
                    cmds.push(Instr::Jmp(
                        error_2.clone(),
                    ));
                    cmds.push(Instr::Label(
                        error_1,
                    ));
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::RDI),
                        Val::Imm(16),
                    ));
                    cmds.push(Instr::Label(
                        error_2,
                    ));
                    cmds.push(Instr::Push(
                        Val::Reg(Reg::RBP),
                    ));
                    cmds.push(Instr::Call(
                        ERR_FUNC.to_string(),
                    ));
                    cmds.push(Instr::Label(
                        label,
                    ))
                }
                Op2::Minus => {
                    cmds.push(Instr::Test(
                        Val::Reg(Reg::AL),
                        Val::Byte(1),
                    ));
                    let error_1 = new_label(l, "error");
                    cmds.push(Instr::Jne(
                        error_1.clone(),
                    ));
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::R11),
                        Val::RegOffset(Reg::RSP, si),
                    ));
                    cmds.push(Instr::Test(
                        Val::Reg(Reg::R11B),
                        Val::Byte(1),
                    ));
                    cmds.push(Instr::Jne(
                        error_1.clone(),
                    ));
                    cmds.push(Instr::Sub(
                        Val::Reg(Reg::RAX),
                        Val::Reg(Reg::R11),
                    ));
                    let label = new_label(l, "label");
                    cmds.push(Instr::Jno(
                        label.clone(),
                    ));
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::RDI),
                        Val::Imm(7),
                    ));
                    let error_2 = new_label(l, "error");
                    cmds.push(Instr::Jmp(
                        error_2.clone(),
                    ));
                    cmds.push(Instr::Label(
                        error_1,
                    ));
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::RDI),
                        Val::Imm(17),
                    ));
                    cmds.push(Instr::Label(
                        error_2,
                    ));
                    cmds.push(Instr::Push(
                        Val::Reg(Reg::RBP),
                    ));
                    cmds.push(Instr::Call(
                        ERR_FUNC.to_string(),
                    ));
                    cmds.push(Instr::Label(
                        label,
                    ))
                }
                Op2::Times => {
                    cmds.push(Instr::Test(
                        Val::Reg(Reg::AL),
                        Val::Byte(1),
                    ));
                    let error_1 = new_label(l, "error");
                    cmds.push(Instr::Jne(
                        error_1.clone(),
                    ));
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::R11),
                        Val::RegOffset(Reg::RSP, si),
                    ));
                    cmds.push(Instr::Test(
                        Val::Reg(Reg::R11B),
                        Val::Byte(1),
                    ));
                    cmds.push(Instr::Jne(
                        error_1.clone(),
                    ));
                    cmds.push(Instr::Sar(
                        Val::Reg(Reg::RAX),
                        Val::Byte(1),
                    ));
                    cmds.push(Instr::Imul(
                        Val::Reg(Reg::RAX),
                        Val::Reg(Reg::R11),
                    ));
                    let label = new_label(l, "label");
                    cmds.push(Instr::Jno(
                        label.clone(),
                    ));
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::RDI),
                        Val::Imm(8),
                    ));
                    let error_2 = new_label(l, "error");
                    cmds.push(Instr::Jmp(
                        error_2.clone(),
                    ));
                    cmds.push(Instr::Label(
                        error_1,
                    ));
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::RDI),
                        Val::Imm(18),
                    ));
                    cmds.push(Instr::Label(
                        error_2,
                    ));
                    cmds.push(Instr::Push(
                        Val::Reg(Reg::RBP),
                    ));
                    cmds.push(Instr::Call(
                        ERR_FUNC.to_string(),
                    ));
                    cmds.push(Instr::Label(
                        label,
                    ))
                }
                Op2::Equal => {
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::R11),
                        Val::RegOffset(Reg::RSP, si),
                    ));
                    cmds.push(Instr::Xor(
                        Val::Reg(Reg::R11B),
                        Val::Reg(Reg::AL),
                    ));
                    cmds.push(Instr::Test(
                        Val::Reg(Reg::R11B),
                        Val::Byte(1),
                    ));
                    let label = new_label(l, "label");
                    cmds.push(Instr::Je(
                        label.clone(),
                    ));
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::RDI),
                        Val::Imm(9),
                    ));
                    cmds.push(Instr::Push(
                        Val::Reg(Reg::RBP),
                    ));
                    cmds.push(Instr::Call(
                        ERR_FUNC.to_string(),
                    ));
                    cmds.push(Instr::Label(
                        label,
                    ));
                    cmds.push(Instr::Cmp(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, si),
                    ));
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::R11),
                        Val::Imm(3),
                    ));
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::RAX),
                        Val::Imm(1),
                    ));
                    cmds.push(Instr::Cmove(
                        Val::Reg(Reg::RAX),
                        Val::Reg(Reg::R11),
                    ))
                }
                Op2::Greater => {
                    cmds.push(Instr::Test(
                        Val::Reg(Reg::AL),
                        Val::Byte(1),
                    ));
                    let error = new_label(l, "error");
                    cmds.push(Instr::Jne(
                        error.clone(),
                    ));
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::R11),
                        Val::RegOffset(Reg::RSP, si),
                    ));
                    cmds.push(Instr::Test(
                        Val::Reg(Reg::R11B),
                        Val::Byte(1),
                    ));
                    cmds.push(Instr::Jne(
                        error.clone(),
                    ));
                    cmds.push(Instr::Cmp(
                        Val::Reg(Reg::RAX),
                        Val::Reg(Reg::R11),
                    ));
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::R11),
                        Val::Imm(3),
                    ));
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::RAX),
                        Val::Imm(1),
                    ));
                    cmds.push(Instr::Cmovg(
                        Val::Reg(Reg::RAX),
                        Val::Reg(Reg::R11),
                    ));
                    let label = new_label(l, "label");
                    cmds.push(Instr::Jmp(
                        label.clone(),
                    ));
                    cmds.push(Instr::Label(
                        error,
                    ));
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::RDI),
                        Val::Imm(10),
                    ));
                    cmds.push(Instr::Push(
                        Val::Reg(Reg::RBP),
                    ));
                    cmds.push(Instr::Call(
                        ERR_FUNC.to_string(),
                    ));
                    cmds.push(Instr::Label(
                        label,
                    ))
                }
                Op2::GreaterEqual => {
                    cmds.push(Instr::Test(
                        Val::Reg(Reg::AL),
                        Val::Byte(1),
                    ));
                    let error = new_label(l, "error");
                    cmds.push(Instr::Jne(
                        error.clone(),
                    ));
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::R11),
                        Val::RegOffset(Reg::RSP, si),
                    ));
                    cmds.push(Instr::Test(
                        Val::Reg(Reg::R11B),
                        Val::Byte(1),
                    ));
                    cmds.push(Instr::Jne(
                        error.clone(),
                    ));
                    cmds.push(Instr::Cmp(
                        Val::Reg(Reg::RAX),
                        Val::Reg(Reg::R11),
                    ));
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::R11),
                        Val::Imm(3),
                    ));
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::RAX),
                        Val::Imm(1),
                    ));
                    cmds.push(Instr::Cmovge(
                        Val::Reg(Reg::RAX),
                        Val::Reg(Reg::R11),
                    ));
                    let label = new_label(l, "label");
                    cmds.push(Instr::Jmp(
                        label.clone(),
                    ));
                    cmds.push(Instr::Label(
                        error,
                    ));
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::RDI),
                        Val::Imm(11),
                    ));
                    cmds.push(Instr::Push(
                        Val::Reg(Reg::RBP),
                    ));
                    cmds.push(Instr::Call(
                        ERR_FUNC.to_string(),
                    ));
                    cmds.push(Instr::Label(
                        label,
                    ))
                }
                Op2::Less => {
                    cmds.push(Instr::Test(
                        Val::Reg(Reg::AL),
                        Val::Byte(1),
                    ));
                    let error = new_label(l, "error");
                    cmds.push(Instr::Jne(
                        error.clone(),
                    ));
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::R11),
                        Val::RegOffset(Reg::RSP, si),
                    ));
                    cmds.push(Instr::Test(
                        Val::Reg(Reg::R11B),
                        Val::Byte(1),
                    ));
                    cmds.push(Instr::Jne(
                        error.clone(),
                    ));
                    cmds.push(Instr::Cmp(
                        Val::Reg(Reg::RAX),
                        Val::Reg(Reg::R11),
                    ));
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::R11),
                        Val::Imm(3),
                    ));
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::RAX),
                        Val::Imm(1),
                    ));
                    cmds.push(Instr::Cmovl(
                        Val::Reg(Reg::RAX),
                        Val::Reg(Reg::R11),
                    ));
                    let label = new_label(l, "label");
                    cmds.push(Instr::Jmp(
                        label.clone(),
                    ));
                    cmds.push(Instr::Label(
                        error,
                    ));
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::RDI),
                        Val::Imm(12),
                    ));
                    cmds.push(Instr::Push(
                        Val::Reg(Reg::RBP),
                    ));
                    cmds.push(Instr::Call(
                        ERR_FUNC.to_string(),
                    ));
                    cmds.push(Instr::Label(
                        label,
                    ))
                }
                Op2::LessEqual => {
                    cmds.push(Instr::Test(
                        Val::Reg(Reg::AL),
                        Val::Byte(1),
                    ));
                    let error = new_label(l, "error");
                    cmds.push(Instr::Jne(
                        error.clone(),
                    ));
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::R11),
                        Val::RegOffset(Reg::RSP, si),
                    ));
                    cmds.push(Instr::Test(
                        Val::Reg(Reg::R11B),
                        Val::Byte(1),
                    ));
                    cmds.push(Instr::Jne(
                        error.clone(),
                    ));
                    cmds.push(Instr::Cmp(
                        Val::Reg(Reg::RAX),
                        Val::Reg(Reg::R11),
                    ));
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::R11),
                        Val::Imm(3),
                    ));
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::RAX),
                        Val::Imm(1),
                    ));
                    cmds.push(Instr::Cmovle(
                        Val::Reg(Reg::RAX),
                        Val::Reg(Reg::R11),
                    ));
                    let label = new_label(l, "label");
                    cmds.push(Instr::Jmp(
                        label.clone(),
                    ));
                    cmds.push(Instr::Label(
                        error,
                    ));
                    cmds.push(Instr::Mov(
                        Val::Reg(Reg::RDI),
                        Val::Imm(13),
                    ));
                    cmds.push(Instr::Push(
                        Val::Reg(Reg::RBP),
                    ));
                    cmds.push(Instr::Call(
                        ERR_FUNC.to_string(),
                    ));
                    cmds.push(Instr::Label(
                        label,
                    ))
                }
            }
        }
        Expr::If(cond, then, other) => {
            compile_expr_instrs(cond, si, env, l, bt, cmds);
            cmds.push(Instr::Cmp(
                Val::Reg(Reg::RAX),
                Val::Imm(1),
            ));
            let if_else = new_label(l, "if_else");
            cmds.push(Instr::Je(
                if_else.clone(),
            ));
            compile_expr_instrs(then, si, env, l, bt, cmds);
            let if_end = new_label(l, "if_end");
            cmds.push(Instr::Jmp(
                if_end.clone(),
            ));
            cmds.push(Instr::Label(
                if_else,
            ));
            compile_expr_instrs(other, si, env, l, bt, cmds);
            cmds.push(Instr::Label(
                if_end,
            ))
        }
        Expr::Loop(e) => {
            let loop_begin = new_label(l, "loop");
            cmds.push(Instr::Label(
                loop_begin.clone(),
            ));
            let loop_end = new_label(l, "loop_end");
            compile_expr_instrs(e, si, env, l, loop_end.as_str(), cmds);
            cmds.push(Instr::Jmp(
                loop_begin,
            ));
            cmds.push(Instr::Label(
                loop_end,
            ))
        }
        Expr::Break(e) => {
            if bt.is_empty() {
                panic!("Invalid break")
            }
            compile_expr_instrs(e, si, env, l, bt, cmds);  // break directly inside break is ok
            cmds.push(Instr::Jmp(
                bt.to_string(),
            ))
        }
        Expr::Set(x, e) => {
            let opt = env.get(x.as_str());
            match opt {
                Some(i) => {
                    compile_expr_instrs(e, si, env, l, bt, cmds);
                    cmds.push(Instr::Mov(
                        Val::RegOffset(Reg::RSP, *i),
                        Val::Reg(Reg::RAX),
                    ))
                }
                None => panic!("Unbound variable identifier {x}"),
            }
        }
        Expr::Block(es) => {
            for e in es {
                compile_expr_instrs(e, si, env, l, bt, cmds);
            }
        }
    }
}

fn compile_to_instrs(e: &Expr) -> Vec<Instr> {
    let mut v: Vec<Instr> = Vec::new();
    let env: HashMap<String, i32> = HashMap::new();
    let mut l: i32 = 0;
    let bt: &str = "";
    compile_expr_instrs(e, 2, &env, &mut l, bt, &mut v);
    v
}

fn instrs_to_str(cmds: &Vec<Instr>) -> String {
    cmds.iter().map(|c| instr_to_str(c)).collect::<Vec<_>>().join("\n  ")
}

fn compile(e: &Expr) -> String {
    let cmds = compile_to_instrs(e);
    instrs_to_str(&cmds)
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let sexp = parse_sexp(&in_contents);
    let expr = parse_expr(&sexp);
    let result = compile(&expr);

    let asm_program = format!(
        "section .text
global {ENTRY_FUNC}
extern {ERR_FUNC}
{ENTRY_FUNC}:
  {result}
  ret
",
    );

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}
