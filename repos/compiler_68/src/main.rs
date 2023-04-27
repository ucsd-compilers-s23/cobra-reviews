use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::Atom::*;
use sexp::*;

use im::HashMap;
use im::HashSet;

#[derive(Debug)]
enum Val {
    Reg(Reg),
    Imm(i64),
    RegOffset(Reg, i64),
    Label(String)
}

#[derive(Debug)]
enum Reg {
    RAX,
    RBX,
    RSP,
    RDI
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    ICMove(Val, Val),
    ICMovne(Val, Val),
    ICMovl(Val, Val),
    ICMovle(Val, Val),
    ICMovg(Val, Val),
    ICMovge(Val, Val),
    ICMovo(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    ISar(Val, Val),
    ICmp(Val, Val),
    ITest(Val, Val),
    IJmp(Val),
    IJe(Val),
    IJnz(Val),
    IJo(Val),
    IAnd(Val, Val),
    IOr(Val, Val),
    IXor(Val, Val),
    ILabel(Val)
}

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
    Less,
    LessEqual,
    Greater,
    GreaterEqual
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
    Block(Vec<Expr>)
}

const ERRCODE_INVALID_ARG: i64 = 7;
const ERRCODE_OVERFLOW: i64 = 8;

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => Expr::Number(i64::try_from(*n).unwrap()),
        Sexp::Atom(S(id)) => {
          match id.as_str() {
              "true" => Expr::Boolean(true),
              "false" => Expr::Boolean(false),
              _ => Expr::Id(id.to_string())
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
                [Sexp::Atom(S(op)), e1, e2] if op == "<" => Expr::BinOp(Op2::Less, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "<=" => Expr::BinOp(Op2::LessEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == ">" => Expr::BinOp(Op2::Greater, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == ">=" => Expr::BinOp(Op2::GreaterEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), Sexp::List(bs), e] if op == "let" && bs.len() > 0 => Expr::Let(bs.iter().map(parse_bind).collect(), Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e1, e2, e3] if op == "if" => Expr::If(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)), Box::new(parse_expr(e3))),
                [Sexp::Atom(S(op)), e] if op == "loop" => Expr::Loop(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "break" => Expr::Break(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), Sexp::Atom(S(id)), e] if op == "set!" => Expr::Set(id.to_string(), Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), exprs @ ..] if op == "block" && exprs.len() > 0 => Expr::Block(exprs.into_iter().map(parse_expr).collect()),
                _ => panic!("Invalid"),
            }
        },
        _ => panic!("Invalid"),
    }
}

fn parse_bind(s: &Sexp) -> (String, Expr) {
    match s {
        Sexp::List(vec) => {
            let forbidden = ["add1", "sub1", "isnum", "isbool", "+", "-", "*", "<", ">", "<=", ">=", "=", "let", "if", "set!", "block", "loop", 
                "break", "input", "true", "false"];
            match &vec[..] {
                [Sexp::Atom(S(id)), e] if !forbidden.iter().any(|&t| t == id.to_string()) => (id.to_string(), parse_expr(e)),
                _ => panic!("Invalid identifier contains keyword")
            }
        },
        _ => panic!("Invalid"),
    }
}

fn compile(e: &Expr) -> String {
    let env = HashMap::new();
    let instrs = compile_to_instrs(e, 2, &env, &String::from(""), &mut 0);
    let str_list = instrs.iter().map(instr_to_str).collect::<Vec<String>>();
    let concat = str_list.concat();
    return concat;
}

fn compile_to_instrs(e: &Expr, si: i64, env: &HashMap<String, i64>, brake: &String, l: &mut i32) -> Vec<Instr> {
    match e {
        Expr::Number(n) => {
            if *n < 2i64.pow(62) && *n >= -2i64.pow(62) {
                vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n << 1))]
            } else {
                panic!("Invalid")
            }
        },
        Expr::Boolean(true) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3))],
        Expr::Boolean(false) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))],
        Expr::Id(s) => {
            if s == "input" {
                vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI))]
            } else if env.contains_key(s) {
                vec![Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, *env.get(s).unwrap()))]
            } else {
                panic!("Unbound variable identifier {}", s)
            }
        },
        Expr::UnOp(op, subexpr) => {
            match op {
                Op1::Add1 => {
                    let mut instrs = compile_to_instrs(subexpr, si, env, brake, l);
                    instrs.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(ERRCODE_INVALID_ARG)));
                    instrs.push(Instr::ICMovne(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX)));
                    instrs.push(Instr::IJnz(Val::Label("throw_error".to_string())));
                    instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(2)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(ERRCODE_OVERFLOW)));
                    instrs.push(Instr::ICMovo(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX)));
                    instrs.push(Instr::IJo(Val::Label("throw_error".to_string())));
                    instrs
                },
                Op1::Sub1 => {
                    let mut instrs = compile_to_instrs(subexpr, si, env, brake, l);
                    instrs.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(ERRCODE_INVALID_ARG)));
                    instrs.push(Instr::ICMovne(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX)));
                    instrs.push(Instr::IJnz(Val::Label("throw_error".to_string())));
                    instrs.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(2)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(ERRCODE_OVERFLOW)));
                    instrs.push(Instr::ICMovo(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX)));
                    instrs.push(Instr::IJo(Val::Label("throw_error".to_string())));
                    instrs
                },
                Op1::IsNum => {
                    let mut instrs = compile_to_instrs(subexpr, si, env, brake, l);
                    instrs.push(Instr::IAnd(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(0)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::ICMove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                    instrs
                },
                Op1::IsBool => {
                    let mut instrs = compile_to_instrs(subexpr, si, env, brake, l);
                    instrs.push(Instr::IAnd(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::ICMove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                    instrs
                }
            }
        },
        Expr::BinOp(op, subexpr1, subexpr2) => {
            match op {
                Op2::Plus => {
                    let stack_offset = si * 8;
                    let mut instrs = compile_to_instrs(subexpr1, si, env, brake, l);
                    instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                    instrs.extend(compile_to_instrs(subexpr2, si + 1, env, brake, l));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IOr(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, stack_offset)));
                    instrs.push(Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(ERRCODE_INVALID_ARG)));
                    instrs.push(Instr::ICMovne(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX)));
                    instrs.push(Instr::IJnz(Val::Label("throw_error".to_string())));
                    instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(ERRCODE_OVERFLOW)));
                    instrs.push(Instr::ICMovo(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX)));
                    instrs.push(Instr::IJo(Val::Label("throw_error".to_string())));
                    instrs
                },
                Op2::Minus => {
                    let stack_offset = si * 8;
                    let mut instrs = compile_to_instrs(subexpr2, si, env, brake, l);
                    instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                    instrs.extend(compile_to_instrs(subexpr1, si + 1, env, brake, l));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IOr(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, stack_offset)));
                    instrs.push(Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(ERRCODE_INVALID_ARG)));
                    instrs.push(Instr::ICMovne(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX)));
                    instrs.push(Instr::IJnz(Val::Label("throw_error".to_string())));
                    instrs.push(Instr::ISub(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(ERRCODE_OVERFLOW)));
                    instrs.push(Instr::ICMovo(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX)));
                    instrs.push(Instr::IJo(Val::Label("throw_error".to_string())));
                    instrs
                },
                Op2::Times => {
                    let stack_offset = si * 8;
                    let mut instrs = compile_to_instrs(subexpr1, si, env, brake, l);
                    instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                    instrs.extend(compile_to_instrs(subexpr2, si + 1, env, brake, l));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IOr(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, stack_offset)));
                    instrs.push(Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(ERRCODE_INVALID_ARG)));
                    instrs.push(Instr::ICMovne(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX)));
                    instrs.push(Instr::IJnz(Val::Label("throw_error".to_string())));
                    instrs.push(Instr::ISar(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(ERRCODE_OVERFLOW)));
                    instrs.push(Instr::ICMovo(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX)));
                    instrs.push(Instr::IJo(Val::Label("throw_error".to_string())));
                    instrs
                },
                Op2::Equal => {
                    let stack_offset = si * 8;
                    let mut instrs = compile_to_instrs(subexpr1, si, env, brake, l);
                    instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                    instrs.extend(compile_to_instrs(subexpr2, si + 1, env, brake, l));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IXor(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, stack_offset)));
                    instrs.push(Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(ERRCODE_INVALID_ARG)));
                    instrs.push(Instr::ICMovne(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX)));
                    instrs.push(Instr::IJnz(Val::Label("throw_error".to_string())));
                    instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::ICMove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                    instrs
                },
                Op2::Less => {
                    let stack_offset = si * 8;
                    let mut instrs = compile_to_instrs(subexpr1, si, env, brake, l);
                    instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                    instrs.extend(compile_to_instrs(subexpr2, si + 1, env, brake, l));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IOr(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, stack_offset)));
                    instrs.push(Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(ERRCODE_INVALID_ARG)));
                    instrs.push(Instr::ICMovne(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX)));
                    instrs.push(Instr::IJnz(Val::Label("throw_error".to_string())));
                    instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::ICMovg(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                    instrs
                },
                Op2::LessEqual => {
                    let stack_offset = si * 8;
                    let mut instrs = compile_to_instrs(subexpr1, si, env, brake, l);
                    instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                    instrs.extend(compile_to_instrs(subexpr2, si + 1, env, brake, l));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IOr(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, stack_offset)));
                    instrs.push(Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(ERRCODE_INVALID_ARG)));
                    instrs.push(Instr::ICMovne(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX)));
                    instrs.push(Instr::IJnz(Val::Label("throw_error".to_string())));
                    instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::ICMovge(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                    instrs
                },
                Op2::Greater => {
                    let stack_offset = si * 8;
                    let mut instrs = compile_to_instrs(subexpr1, si, env, brake, l);
                    instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                    instrs.extend(compile_to_instrs(subexpr2, si + 1, env, brake, l));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IOr(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, stack_offset)));
                    instrs.push(Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(ERRCODE_INVALID_ARG)));
                    instrs.push(Instr::ICMovne(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX)));
                    instrs.push(Instr::IJnz(Val::Label("throw_error".to_string())));
                    instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::ICMovl(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                    instrs
                },
                Op2::GreaterEqual => {
                    let stack_offset = si * 8;
                    let mut instrs = compile_to_instrs(subexpr1, si, env, brake, l);
                    instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                    instrs.extend(compile_to_instrs(subexpr2, si + 1, env, brake, l));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IOr(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, stack_offset)));
                    instrs.push(Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(ERRCODE_INVALID_ARG)));
                    instrs.push(Instr::ICMovne(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX)));
                    instrs.push(Instr::IJnz(Val::Label("throw_error".to_string())));
                    instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::ICMovle(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                    instrs
                }
            }
        },
        Expr::Let(bindings, body) => {
            let mut instrs = Vec::new();
            let mut dup = HashSet::new();
            let mut nenv = env.clone();
            let mut idx = si;
            
            for (id, expr) in bindings.iter() {
                if !dup.contains(id) {
                    dup.insert(id);
                    instrs.extend(compile_to_instrs(expr, idx, &nenv, brake, l));
                    nenv.insert(id.to_string(), idx * 8);
                    instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, idx * 8), Val::Reg(Reg::RAX)));
                    idx += 1;
                } else {
                    panic!("Duplicate binding")
                }
            }

            let body_instrs = compile_to_instrs(body, idx, &nenv, brake, l);
            instrs.extend(body_instrs);
            instrs
        },
        Expr::If(cond_expr, then_expr, else_expr) => {
            let end_label = new_label(l, "ifend");
            let else_label = new_label(l, "ifelse");
            let mut instrs = compile_to_instrs(cond_expr, si, env, brake, l);
            instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(1)));
            instrs.push(Instr::IJe(Val::Label(else_label.clone())));
            instrs.extend(compile_to_instrs(then_expr, si, env, brake, l));
            instrs.push(Instr::IJmp(Val::Label(end_label.clone())));
            instrs.push(Instr::ILabel(Val::Label(else_label.clone())));
            instrs.extend(compile_to_instrs(else_expr, si, env, brake, l));
            instrs.push(Instr::ILabel(Val::Label(end_label.clone())));
            instrs
        },
        Expr::Loop(subexpr) => {
            let loop_label = new_label(l, "loop");
            let end_loop_label = new_label(l, "loop_end");
            let mut instrs = vec![Instr::ILabel(Val::Label(loop_label.clone()))];
            instrs.extend(compile_to_instrs(subexpr, si, env, &end_loop_label, l));
            instrs.push(Instr::IJmp(Val::Label(loop_label.clone())));
            instrs.push(Instr::ILabel(Val::Label(end_loop_label.clone())));
            instrs
        },
        Expr::Break(subexpr) => {
            if !brake.is_empty() {
                let mut instrs = compile_to_instrs(subexpr, si, env, brake, l);
                instrs.push(Instr::IJmp(Val::Label(brake.clone())));
                instrs
            } else {
                panic!("Invalid break");
            }
        },
        Expr::Set(id, subexpr) => {
            if env.contains_key(id) {
                let offset = *env.get(id).unwrap();
                let mut instrs = compile_to_instrs(subexpr, si, env, brake, l);
                instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX)));
                instrs
            } else {
                panic!("Unbound variable identifier {}", id)
            }
        },
        Expr::Block(subexprs) => subexprs.into_iter().flat_map(|e| compile_to_instrs(e, si, env, brake, l)).collect()
    }
}

fn new_label(l: &mut i32, s: &str) -> String {
    let current = *l;
    *l += 1;
    format!("{s}_{current}")
}

fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(v1, v2) => format!("\n  mov {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ICMove(v1, v2) => format!("\n  cmove {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ICMovne(v1, v2) => format!("\n  cmovne {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ICMovl(v1, v2) => format!("\n  cmovl {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ICMovle(v1, v2) => format!("\n  cmovle {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ICMovg(v1, v2) => format!("\n  cmovg {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ICMovge(v1, v2) => format!("\n  cmovge {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ICMovo(v1, v2) => format!("\n  cmovo {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::IAdd(v1, v2) => format!("\n  add {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ISub(v1, v2) => format!("\n  sub {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::IMul(v1, v2) => format!("\n  imul {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ISar(v1, v2) => format!("\n  sar {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ICmp(v1, v2) => format!("\n  cmp {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ITest(v1, v2) => format!("\n  test {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::IJmp(v) => format!("\n  jmp {}", val_to_str(v)),
        Instr::IJe(v) => format!("\n  je {}", val_to_str(v)),
        Instr::IJnz(v) => format!("\n  jnz {}", val_to_str(v)),
        Instr::IJo(v) => format!("\n  jo {}", val_to_str(v)),
        Instr::IAnd(v1, v2) => format!("\n  and {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::IOr(v1, v2) => format!("\n  or {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::IXor(v1, v2) => format!("\n  xor {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ILabel(v) => format!("\n  {}:", val_to_str(v))
    }
}

fn val_to_str(v: &Val) -> String {
    match v {
        Val::Reg(r) => {
            match r {
                Reg::RAX => "rax".to_string(),
                Reg::RBX => "rbx".to_string(),
                Reg::RSP => "rsp".to_string(),
                Reg::RDI => "rdi".to_string()
            }
        },
        Val::Imm(i) => i.to_string(),
        Val::RegOffset(r, i) => {
            match r {
                Reg::RAX => format!("[rax - {}]", i.to_string()),
                Reg::RBX => format!("[rbx - {}]", i.to_string()),
                Reg::RSP => format!("[rsp - {}]", i.to_string()),
                Reg::RDI => format!("[rdi - {}]", i.to_string())
            }
        },
        Val::Label(s) => s.to_string()
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let parsed = parse(&in_contents);
    if let Err(_e) = parsed {
        panic!("Invalid");
    }

    let expr = parse_expr(&parsed.unwrap());
    let result = compile(&expr);
    let asm_program = format!(
        "
section .text
global our_code_starts_here
extern snek_error
throw_error:
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
