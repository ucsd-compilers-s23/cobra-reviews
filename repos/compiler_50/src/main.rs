use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::Atom::*;
use sexp::*;

use im::HashMap;

#[derive(Debug)]
enum Val {
    Reg(Reg),
    Imm(i64),
    RegOffset(Reg, i64),
    Label(String),
}

#[derive(Debug)]
enum Reg {
    RAX,
    RBX,
    RSP,
    RDI,
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    Cmp(Val, Val),
    CMov(Val, Val),
    Test(Val, Val),
    Je(Val),
    Jo(Val),
    Jne(Val),
    Jle(Val),
    Jge(Val),
    Jump(Val),
    Shl(Val, Val),
    Sar(Val, Val),
    Shr(Val, Val),
    And(Val, Val),
    Xor(Val, Val),
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

fn new_label(l: &mut i64, s: &str) -> String {
    let current = *l;
    *l += 1;
    return format!("{s}_{current}");
}

fn parse_expr(s: &Sexp) -> Expr {
    let disallowed = vec!["let", "add1", "sub1", "+", "-", "*", "=", "<", ">", "<=", ">=", "true", "false", "isnum", "isbool", "block", "set!", "loop", "if", "break", "input"];
    match s {
        Sexp::Atom(I(n)) => {
            let i: i64 = i64::try_from(*n).expect("Invalid");
            if i < (1 << 62) && i >= -1 * (1 << 62) {
                return Expr::Number(i);
            } else {
                panic!("Invalid");
            }
        },
        Sexp::Atom(S(b)) if b == "false" => Expr::Boolean(false),
        Sexp::Atom(S(b)) if b == "true" => Expr::Boolean(true),
        Sexp::Atom(S(i)) => { 
            if !i.eq("input") && disallowed.iter().any(|e| i.eq(e)) {
                panic!("Invalid: identifier matches keyword");
            } else {
                return Expr::Id(i.to_string());
            }
        },
        Sexp::List(vec) if vec.starts_with(&[Sexp::Atom(S("block".to_string()))]) => {
            let mut vec2: Vec<Expr> = Vec::new();
            let mut i = true;
            for e in vec {
                if i {
                  i = false;
                } else {
                  vec2.extend(vec![parse_expr(e)]);
                }
            }
            if vec2.is_empty() {
                panic!("Invalid");
            }
            return Expr::Block(vec2);
        },
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(op)), e] if op == "add1" => Expr::UnOp(Op1::Add1, Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "sub1" => Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "isnum" => Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "isbool" => Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "loop" => Expr::Loop(Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "break" => Expr::Break(Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e1, e2] if op == "+" => Expr::BinOp(Op2::Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
            [Sexp::Atom(S(op)), e1, e2] if op == "-" => Expr::BinOp(Op2::Minus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
            [Sexp::Atom(S(op)), e1, e2] if op == "*" => Expr::BinOp(Op2::Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
            [Sexp::Atom(S(op)), e1, e2] if op == "=" => Expr::BinOp(Op2::Equal, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
            [Sexp::Atom(S(op)), e1, e2] if op == "<" => Expr::BinOp(Op2::Less, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
            [Sexp::Atom(S(op)), e1, e2] if op == ">" => Expr::BinOp(Op2::Greater, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
            [Sexp::Atom(S(op)), e1, e2] if op == "<=" => Expr::BinOp(Op2::LessEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
            [Sexp::Atom(S(op)), e1, e2] if op == ">=" => Expr::BinOp(Op2::GreaterEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
            [Sexp::Atom(S(op)), e1, e2, e3] if op == "if" => Expr::If(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)), Box::new(parse_expr(e3))),
            [Sexp::Atom(S(op)), Sexp::Atom(S(s)), e] => {
                if op == "set!" && disallowed.iter().any(|a| s.eq(a)) {
                    panic!("Invalid: identifier matches keyword");
                } else {
                    return Expr::Set(s.to_string(), Box::new(parse_expr(e)));
                }
            },
            [Sexp::Atom(S(op)), Sexp::List(sexp_vec), e2] if op == "let" && !sexp_vec.is_empty() => {
                let mut vec: Vec<(String, Expr)> = Vec::new();
                for e in sexp_vec {
                    vec.extend(vec![parse_bind(e)]);
                }
                return Expr::Let(vec, Box::new(parse_expr(e2)));
            },
            _ => panic!("Invalid"),
        },
        _ => panic!("Invalid"),
    }
}

fn parse_bind(s: &Sexp) -> (String, Expr) {
  let disallowed = vec!["let", "add1", "sub1", "+", "-", "*", "=", "<", ">", "<=", ">=", "true", "false", "isnum", "isbool", "block", "set!", "loop", "if", "break", "input"];
    match s {
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(i)), e] => {
                if disallowed.iter().any(|a| i.eq(a)) {
                    panic!("Invalid: identifier matches keyword");
                } else {
                    return (i.to_string(), parse_expr(e));
                }
            },
            _ => panic!("Invalid"),
        },
        _ => panic!("Invalid"),
    }
}

fn compile_to_instrs(e: &Expr, si: i64, env: &HashMap<String, i64>, l: &mut i64, b_target: String) -> Vec<Instr> {
    match e {
        Expr::Number(n) => vec![(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n << 1)))],
        Expr::Boolean(b) if *b == false => vec![(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)))],
        Expr::Boolean(b) if *b == true => vec![(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)))],
        Expr::Id(i) => {
            if i == "input" {
                return vec![(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI)))];
            } else if env.contains_key(i) {
                return vec![(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, *env.get(i).unwrap())))];
            } else {
                panic!("Unbound variable identifier {}", i);
            }
        },
        Expr::UnOp(Op1::Add1, subexpr) => {
            let mut vec = compile_to_instrs(subexpr, si, env, l, b_target);
            vec.extend(vec![(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)))]);
            vec.extend(vec![(Instr::Jne(Val::Label("throw_error".to_string())))]);
            vec.extend(vec![(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(2)))]);
            vec.extend(vec![(Instr::Jo(Val::Label("throw_overflow_error".to_string())))]);
            return vec;
        },
        Expr::UnOp(Op1::Sub1, subexpr) => {
            let mut vec = compile_to_instrs(subexpr, si, env, l, b_target);
            vec.extend(vec![(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)))]);
            vec.extend(vec![(Instr::Jne(Val::Label("throw_error".to_string())))]);
            vec.extend(vec![(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(2)))]);
            vec.extend(vec![(Instr::Jo(Val::Label("throw_overflow_error".to_string())))]);
            return vec;
        },
        Expr::UnOp(Op1::IsNum, subexpr) => {
            let mut vec = compile_to_instrs(subexpr, si, env, l, b_target);
            vec.extend(vec![(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)))]);
            vec.extend(vec![(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)))]);
            vec.extend(vec![(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)))]);
            vec.extend(vec![(Instr::CMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)))]);
            return vec;
        },
        Expr::UnOp(Op1::IsBool, subexpr) => {
            let test_label = new_label(l, "test");
            let mut vec = compile_to_instrs(subexpr, si, env, l, b_target);
            vec.extend(vec![(Instr::Cmp(Val::Reg(Reg::RAX), Val::Imm(1)))]);
            vec.extend(vec![(Instr::Je(Val::Label(test_label.clone())))]);
            vec.extend(vec![(Instr::Cmp(Val::Reg(Reg::RAX), Val::Imm(3)))]);
            vec.extend(vec![(Instr::Label(Val::Label(test_label.clone())))]);
            vec.extend(vec![(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)))]);
            vec.extend(vec![(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)))]);
            vec.extend(vec![(Instr::CMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)))]);
            return vec;
        },
        Expr::BinOp(Op2::Plus, subexpr1, subexpr2) => {
            let mut vec = compile_to_instrs(subexpr1, si, env, l, b_target.clone());
            let vec2 = compile_to_instrs(subexpr2, si + 1, env, l, b_target.clone());
            let stack_offset = si * 8;
            vec.extend(vec![(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)))]);
            vec.extend(vec![(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)))]);
            vec.extend(vec![(Instr::Jne(Val::Label("throw_error".to_string())))]);
            vec.extend(vec![(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)))]);
            vec.extend(vec2);
            vec.extend(vec![(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)))]);
            vec.extend(vec![(Instr::Jne(Val::Label("throw_error".to_string())))]);
            vec.extend(vec![(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)))]);
            vec.extend(vec![(Instr::Jo(Val::Label("throw_overflow_error".to_string())))]);
            return vec;
        },
        Expr::BinOp(Op2::Minus, subexpr1, subexpr2) => {
            let mut vec = compile_to_instrs(subexpr2, si, env, l, b_target.clone());
            let vec2 = compile_to_instrs(subexpr1, si + 1, env, l, b_target.clone());
            let stack_offset = si * 8;
            vec.extend(vec![(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)))]);
            vec.extend(vec![(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)))]);
            vec.extend(vec![(Instr::Jne(Val::Label("throw_error".to_string())))]);
            vec.extend(vec![(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)))]);
            vec.extend(vec2);
            vec.extend(vec![(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)))]);
            vec.extend(vec![(Instr::Jne(Val::Label("throw_error".to_string())))]);
            vec.extend(vec![(Instr::ISub(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)))]);
            vec.extend(vec![(Instr::Jo(Val::Label("throw_overflow_error".to_string())))]);
            return vec;
        },
        Expr::BinOp(Op2::Times, subexpr1, subexpr2) => {
            let mut vec = compile_to_instrs(subexpr1, si, env, l, b_target.clone());
            let vec2 = compile_to_instrs(subexpr2, si + 1, env, l, b_target.clone());
            let stack_offset = si * 8;
            vec.extend(vec![(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)))]);
            vec.extend(vec![(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)))]);
            vec.extend(vec![(Instr::Jne(Val::Label("throw_error".to_string())))]);
            vec.extend(vec![(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)))]);
            vec.extend(vec2);
            vec.extend(vec![(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)))]);
            vec.extend(vec![(Instr::Jne(Val::Label("throw_error".to_string())))]);
            vec.extend(vec![(Instr::Sar(Val::Reg(Reg::RAX), Val::Imm(1)))]);
            vec.extend(vec![(Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)))]);
            vec.extend(vec![(Instr::Jo(Val::Label("throw_overflow_error".to_string())))]);
            return vec;
        },
        Expr::BinOp(Op2::Equal, subexpr1, subexpr2) => {
            let mut vec = compile_to_instrs(subexpr1, si, env, l, b_target.clone());
            let vec2 = compile_to_instrs(subexpr2, si + 1, env, l, b_target.clone());
            let offset = si * 8;
            vec.extend(vec![(Instr::IMov(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX)))]);
            vec.extend(vec2);
            vec.extend(vec![(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)))]);
            vec.extend(vec![(Instr::Xor(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, offset)))]);
            vec.extend(vec![(Instr::Test(Val::Reg(Reg::RBX), Val::Imm(1)))]);
            vec.extend(vec![(Instr::Jne(Val::Label("throw_error".to_string())))]);
            vec.extend(vec![(Instr::Cmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset)))]);
            vec.extend(vec![(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)))]);
            vec.extend(vec![(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)))]);
            vec.extend(vec![(Instr::CMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)))]);
            return vec;
        },
        Expr::BinOp(Op2::Greater, subexpr1, subexpr2) => {
            let false_label = new_label(l, "false_label");
            let end_label = new_label(l, "end");
            let mut vec = compile_to_instrs(subexpr1, si, env, l, b_target.clone());
            let vec2 = compile_to_instrs(subexpr2, si + 1, env, l, b_target.clone());
            let offset = si * 8;
            vec.extend(vec![(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)))]);
            vec.extend(vec![(Instr::Jne(Val::Label("throw_error".to_string())))]);
            vec.extend(vec![(Instr::IMov(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX)))]);
            vec.extend(vec2);
            vec.extend(vec![(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)))]);
            vec.extend(vec![(Instr::Jne(Val::Label("throw_error".to_string())))]);
            vec.extend(vec![(Instr::Cmp(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX)))]);
            vec.extend(vec![(Instr::Jle(Val::Label(false_label.clone())))]);
            vec.extend(vec![(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)))]);
            vec.extend(vec![(Instr::Jump(Val::Label(end_label.clone())))]);
            vec.extend(vec![(Instr::Label(Val::Label(false_label.clone())))]);
            vec.extend(vec![(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)))]);
            vec.extend(vec![(Instr::Label(Val::Label(end_label.clone())))]);
            return vec;
        },
        Expr::BinOp(Op2::Less, subexpr1, subexpr2) => {
            let false_label = new_label(l, "false_label");
            let end_label = new_label(l, "end");
            let mut vec = compile_to_instrs(subexpr1, si, env, l, b_target.clone());
            let vec2 = compile_to_instrs(subexpr2, si + 1, env, l, b_target.clone());
            let offset = si * 8;
            vec.extend(vec![(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)))]);
            vec.extend(vec![(Instr::Jne(Val::Label("throw_error".to_string())))]);
            vec.extend(vec![(Instr::IMov(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX)))]);
            vec.extend(vec2);
            vec.extend(vec![(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)))]);
            vec.extend(vec![(Instr::Jne(Val::Label("throw_error".to_string())))]);
            vec.extend(vec![(Instr::Cmp(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX)))]);
            vec.extend(vec![(Instr::Jge(Val::Label(false_label.clone())))]);
            vec.extend(vec![(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)))]);
            vec.extend(vec![(Instr::Jump(Val::Label(end_label.clone())))]);
            vec.extend(vec![(Instr::Label(Val::Label(false_label.clone())))]);
            vec.extend(vec![(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)))]);
            vec.extend(vec![(Instr::Label(Val::Label(end_label.clone())))]);
            return vec;
        },
        Expr::BinOp(Op2::GreaterEqual, subexpr1, subexpr2) => {
            let true_label = new_label(l, "false_label");
            let end_label = new_label(l, "end");
            let mut vec = compile_to_instrs(subexpr1, si, env, l, b_target.clone());
            let vec2 = compile_to_instrs(subexpr2, si + 1, env, l, b_target.clone());
            let offset = si * 8;
            vec.extend(vec![(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)))]);
            vec.extend(vec![(Instr::Jne(Val::Label("throw_error".to_string())))]);
            vec.extend(vec![(Instr::IMov(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX)))]);
            vec.extend(vec2);
            vec.extend(vec![(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)))]);
            vec.extend(vec![(Instr::Jne(Val::Label("throw_error".to_string())))]);
            vec.extend(vec![(Instr::Cmp(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX)))]);
            vec.extend(vec![(Instr::Jge(Val::Label(true_label.clone())))]);
            vec.extend(vec![(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)))]);
            vec.extend(vec![(Instr::Jump(Val::Label(end_label.clone())))]);
            vec.extend(vec![(Instr::Label(Val::Label(true_label.clone())))]);
            vec.extend(vec![(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)))]);
            vec.extend(vec![(Instr::Label(Val::Label(end_label.clone())))]);
            return vec;
        },
        Expr::BinOp(Op2::LessEqual, subexpr1, subexpr2) => {
            let true_label = new_label(l, "false_label");
            let end_label = new_label(l, "end");
            let mut vec = compile_to_instrs(subexpr1, si, env, l, b_target.clone());
            let vec2 = compile_to_instrs(subexpr2, si + 1, env, l, b_target.clone());
            let offset = si * 8;
            vec.extend(vec![(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)))]);
            vec.extend(vec![(Instr::Jne(Val::Label("throw_error".to_string())))]);
            vec.extend(vec![(Instr::IMov(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX)))]);
            vec.extend(vec2);
            vec.extend(vec![(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)))]);
            vec.extend(vec![(Instr::Jne(Val::Label("throw_error".to_string())))]);
            vec.extend(vec![(Instr::Cmp(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX)))]);
            vec.extend(vec![(Instr::Jle(Val::Label(true_label.clone())))]);
            vec.extend(vec![(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)))]);
            vec.extend(vec![(Instr::Jump(Val::Label(end_label.clone())))]);
            vec.extend(vec![(Instr::Label(Val::Label(true_label.clone())))]);
            vec.extend(vec![(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)))]);
            vec.extend(vec![(Instr::Label(Val::Label(end_label.clone())))]);
            return vec;
        },
        Expr::Let(vec_let, subexpr) => {
            let mut vec: Vec<Instr> = Vec::new();
            let mut nenv: HashMap<String, i64> = HashMap::new();
            let mut nenv2: HashMap<String, i64> = HashMap::new();
            let mut si2 = si;
            for (s, e) in vec_let.iter() {
                let vec2;
                if si2 == si {
                    vec2 = compile_to_instrs(e, si2, env, l, b_target.clone());
                } else {
                    vec2 = compile_to_instrs(e, si2, &nenv, l, b_target.clone());
                }
                vec.extend(vec2);
                vec.extend(vec![(Instr::IMov(Val::RegOffset(Reg::RSP, si2 * 8), Val::Reg(Reg::RAX)))]);
                if nenv2.contains_key(s) {
                    panic!("Duplicate binding");
                } else {
                    if si2 == si {
                        nenv = env.update(s.to_string(), si2 * 8);
                    } else {
                        nenv = nenv.update(s.to_string(), si2 * 8);
                    }
                    nenv2 = nenv2.update(s.to_string(), si2 * 8);
                }
                si2 += 1;
            }
            let b_instrs = compile_to_instrs(subexpr, si2, &nenv, l, b_target.clone());
            vec.extend(b_instrs);
            return vec;
        },
        Expr::If(cond, thn, els) => {
            let end_label = new_label(l, "ifend");
            let else_label = new_label(l, "ifelse");
            let mut vec = compile_to_instrs(cond, si, env, l, b_target.clone());
            let thn_instrs = compile_to_instrs(thn, si, env, l, b_target.clone());
            let els_instrs = compile_to_instrs(els, si, env, l, b_target.clone());
            vec.extend(vec![(Instr::Cmp(Val::Reg(Reg::RAX), Val::Imm(1)))]);
            vec.extend(vec![(Instr::Je(Val::Label(else_label.clone())))]);
            vec.extend(thn_instrs);
            vec.extend(vec![(Instr::Jump(Val::Label(end_label.clone())))]);
            vec.extend(vec![(Instr::Label(Val::Label(else_label)))]);
            vec.extend(els_instrs);
            vec.extend(vec![(Instr::Label(Val::Label(end_label)))]);
            return vec;
        },
        Expr::Loop(subexpr) => {
            let loop_label = new_label(l, "loop");
            let end_label = new_label(l, "endloop");
            let mut vec = vec![(Instr::Label(Val::Label(loop_label.clone())))];
            let vec2 = compile_to_instrs(subexpr, si, env, l, end_label.clone());
            vec.extend(vec2);
            vec.extend(vec![(Instr::Jump(Val::Label(loop_label.clone())))]);
            vec.extend(vec![(Instr::Label(Val::Label(end_label.clone())))]);
            return vec;
        },
        Expr::Break(subexpr) => {
            if b_target == "" {
                panic!("Invalid break");
            }
            let mut vec = compile_to_instrs(subexpr, si, env, l, b_target.clone());
            vec.extend(vec![(Instr::Jump(Val::Label(b_target.clone())))]);
            return vec;
        },
        Expr::Block(expr_vec) => {
            let mut vec: Vec<Instr> = Vec::new();
            for e in expr_vec {
                vec.extend(compile_to_instrs(e, si, env, l, b_target.clone()));
            }
            return vec;
        },
        Expr::Set(s, expr) => {
            let mut vec = compile_to_instrs(expr, si, env, l, b_target.clone());
            if env.contains_key(s) {
                vec.extend(vec![(Instr::IMov(Val::RegOffset(Reg::RSP, *env.get(s).unwrap()), Val::Reg(Reg::RAX)))]);
                return vec;
            } else {
                panic!("Unbound variable identifier {}", s);
            }
        },
        _ => todo!(),
    }
}

fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(val1, val2) => format!("mov {}, {}", val_to_str(val1), val_to_str(val2)),
        Instr::IAdd(val1, val2) => format!("add {}, {}", val_to_str(val1), val_to_str(val2)),
        Instr::ISub(val1, val2) => format!("sub {}, {}", val_to_str(val1), val_to_str(val2)),
        Instr::IMul(val1, val2) => format!("imul {}, {}", val_to_str(val1), val_to_str(val2)),
        Instr::Cmp(val1, val2) => format!("cmp {}, {}", val_to_str(val1), val_to_str(val2)),
        Instr::CMov(val1, val2) => format!("cmove {}, {}", val_to_str(val1), val_to_str(val2)),
        Instr::Test(val1, val2) => format!("test {}, {}", val_to_str(val1), val_to_str(val2)),
        Instr::Jump(label) => format!("jmp {}", val_to_str(label)),
        Instr::Jo(label) => format!("jo  {}", val_to_str(label)),
        Instr::Je(label) => format!("je  {}", val_to_str(label)),
        Instr::Jne(label) => format!("jne {}", val_to_str(label)),
        Instr::Jge(label) => format!("jge {}", val_to_str(label)),
        Instr::Jle(label) => format!("jle {}", val_to_str(label)),
        Instr::Shl(val1, val2) => format!("shl {}, {}", val_to_str(val1), val_to_str(val2)),
        Instr::Sar(val1, val2) => format!("sar {}, {}", val_to_str(val1), val_to_str(val2)),
        Instr::Shr(val1, val2) => format!("shr {}. {}", val_to_str(val1), val_to_str(val2)),
        Instr::And(val1, val2) => format!("and {}, {}", val_to_str(val1), val_to_str(val2)),
        Instr::Xor(val1, val2) => format!("xor {}, {}", val_to_str(val1), val_to_str(val2)),
        Instr::Label(label) => format!("{}:", val_to_str(label)),
    }
}

fn val_to_str(v: &Val) -> String {
    match v {
        Val::Reg(Reg::RAX) => "RAX".to_string(),
        Val::Reg(Reg::RSP) => "RSP".to_string(),
        Val::Reg(Reg::RBX) => "RBX".to_string(),
        Val::Reg(Reg::RDI) => "RDI".to_string(),
        Val::Imm(n) => format!("{}", *n),
        Val::RegOffset(Reg::RAX, n) => format!("[RAX - {}]", *n),
        Val::RegOffset(Reg::RSP, n) => format!("[RSP - {}]", *n),
        Val::Label(label) => label.to_string(),
        _ => panic!("Invalid instruction"),
    }
}

fn compile(e: &Expr) -> String {
    let env: HashMap<String, i64> = HashMap::new();
    let mut l = 0;
    let vec = compile_to_instrs(e, 2, &env, &mut l, "".to_string());
    let mut instructions = String::from("");
    for instr in vec {
        instructions += &("  ".to_owned() + &instr_to_str(&instr) + "\n");
    }

    return instructions;
}

fn main() -> std::io::Result<()> {
  let args: Vec<String> = env::args().collect();

  let in_name = &args[1];
  let out_name = &args[2];

  let mut in_file = File::open(in_name)?;
  let mut in_contents = String::new();
  in_file.read_to_string(&mut in_contents)?;

  let expr = &parse(&in_contents);
  let expr2;
  match expr {
      Ok(e) => expr2 = e,
      Err(_) => panic!("Invalid"),
  }
  let expr2 = parse_expr(expr2);
  let result = compile(&expr2);
  let asm_program = format!(
      "
section .text
extern snek_error
throw_error:
  mov rdi, 7
  push rsp
  call snek_error
throw_overflow_error:
  mov rdi, 10
  push rsp
  call snek_error
global our_code_starts_here
our_code_starts_here:
{}  ret
", result);

  let mut out_file = File::create(out_name)?;
  out_file.write_all(asm_program.as_bytes())?;

  Ok(())
}
