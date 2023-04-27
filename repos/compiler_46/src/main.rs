use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::Atom::*;
use sexp::*;

use im::HashMap;

const RESERVED: [&str; 21] = [
    "let", "add1", "sub1", "true", "false", "block", "set!", "isnum", "isbool", "if", "loop",
    "break", "+", "-", "*", "<", ">", "<=", ">=", "=", "input",
];

const MIN : i64 = -i64::pow(2, 62);
const MAX : i64 = i64::pow(2, 62) - 1;
const OVERFLOW_LABEL : &str = "throw_overflow";

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
    RDI,
}

const TRUE: i64 = 0b11;
const FALSE: i64 = 0b01;

type Label = String;

fn new_label(l: &mut i32, s: &str) -> String {
    let current = *l;
    *l += 1;
    format!("{s}_{current}")
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    IAnd(Val, Val),
    IOr(Val, Val),
    IXor(Val, Val),
    ICmp(Val, Val),
    ITest(Val, Val),
    ILabel(Label),
    IJe(Label),
    IJl(Label),
    IJle(Label),
    IJg(Label),
    IJge(Label),
    IJne(Label),
    IJmp(Label),
    ICmove(Val, Val),
    ICmovl(Val, Val),
    ICmovle(Val, Val),
    ICmovg(Val, Val),
    ICmovge(Val, Val),
    IShr(Val, Val)
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

fn parse_bind(s: &Sexp) -> (String, Expr) {
    match s {
        Sexp::List(bind) => match &bind[..] {
            [Sexp::Atom(S(name)), e] => {
                if RESERVED.contains(&name.as_str()) {
                    panic!("keyword error: used reserved keyword {name} as variable name")
                }
                (name.to_owned(), parse_expr(e))
            }
            _ => panic!("Invalid binding"), // did not give a list of binds
        },
        _ => panic!("Invalid binding"), // not a list
    }
}

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => {
            if *n > MAX || *n < MIN {
                panic!("Invalid")
            }
            Expr::Number(*n)
        },
        Sexp::Atom(S(text)) => {
            if text == "true" {
                Expr::Boolean(true)
            } else if text == "false" {
                Expr::Boolean(false)
            } else {
                Expr::Id(text.to_owned())
            }
        }
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(op)), cond, yes, no] if op == "if" => Expr::If(
                Box::new(parse_expr(cond)),
                Box::new(parse_expr(yes)),
                Box::new(parse_expr(no)),
            ),
            [Sexp::Atom(S(op)), body] if op == "loop" => Expr::Loop(Box::new(parse_expr(body))),
            [Sexp::Atom(S(op)), e] if op == "break" => Expr::Break(Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => Expr::Block(exprs.iter().map(parse_expr).collect()),
            [Sexp::Atom(S(op)), Sexp::Atom(S(name)), e] if op == "set!" => {
                if RESERVED.contains(&name.as_str()) {
                    panic!("keyword error: can't set! to keyword")
                }
                Expr::Set(name.to_string(), Box::new(parse_expr(e)))
            },
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
            [Sexp::Atom(S(op)), binds, body] if op == "let" => match binds {
                Sexp::List(binds_vec) => match binds_vec.as_slice() {
                    [] => panic!("Invalid: empty list of let bindings"),
                    _ => Expr::Let(
                        binds_vec.iter().map(|x| parse_bind(x)).collect(),
                        Box::new(parse_expr(body)),
                    ),
                },
                _ => panic!("Invalid binding: let requires list of binds"),
            },
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
            [Sexp::Atom(S(op)), e1, e2] if op == "<" => Expr::BinOp(
                Op2::Less,
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
            [Sexp::Atom(S(op)), e1, e2] if op == "<=" => Expr::BinOp(
                Op2::LessEqual,
                Box::new(parse_expr(e1)),
                Box::new(parse_expr(e2)),
            ),
            [Sexp::Atom(S(op)), e1, e2] if op == "=" => Expr::BinOp(
                Op2::Equal,
                Box::new(parse_expr(e1)),
                Box::new(parse_expr(e2)),
            ),
            _ => panic!("Invalid syntax")
        },
        _ => panic!("Invalid syntax"),
    }
}

fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(dst, src) => format!("mov {}, {}\n", val_to_str(dst), val_to_str(src)),
        Instr::IAdd(dst, src) => format!("add {}, {}\n", val_to_str(dst), val_to_str(src)),
        Instr::ISub(dst, src) => format!("sub {}, {}\n", val_to_str(dst), val_to_str(src)),
        Instr::IMul(dst, src) => format!("imul {}, {}\n", val_to_str(dst), val_to_str(src)),
        Instr::IAnd(dst, src) => format!("and {}, {}\n", val_to_str(dst), val_to_str(src)),
        Instr::IOr(dst, src) => format!("or {}, {}\n", val_to_str(dst), val_to_str(src)),
        Instr::IXor(dst, src) => format!("xor {}, {}\n", val_to_str(dst), val_to_str(src)),
        Instr::ICmp(op1, op2) => format!("cmp {}, {}\n", val_to_str(op1), val_to_str(op2)),
        Instr::ITest(op1, op2) => format!("test {}, {}\n", val_to_str(op1), val_to_str(op2)),
        Instr::ILabel(name) => format!("{}:\n", name),
        Instr::IJmp(label) => format!("jmp {}\n", label),
        Instr::IJe(label) => format!("je {}\n", label),
        Instr::IJne(label) => format!("jne {}\n", label),
        Instr::IJg(label) => format!("jg {}\n", label),
        Instr::IJge(label) => format!("jge {}\n", label),
        Instr::IJl(label) => format!("jl {}\n", label),
        Instr::IJle(label) => format!("jle {}\n", label),
        Instr::ICmove(dst, src) => format!("cmove {}, {}\n", val_to_str(dst), val_to_str(src)),
        Instr::ICmovl(dst, src) => format!("cmovl {}, {}\n", val_to_str(dst), val_to_str(src)),
        Instr::ICmovle(dst, src) => format!("cmovle {}, {}\n", val_to_str(dst), val_to_str(src)),
        Instr::ICmovg(dst, src) => format!("cmovg {}, {}\n", val_to_str(dst), val_to_str(src)),
        Instr::ICmovge(dst, src) => format!("cmovge {}, {}\n", val_to_str(dst), val_to_str(src)),
        Instr::IShr(count, dst) => format!("shr {}, {}\n", val_to_str(count), val_to_str(dst)),
    }
}

fn val_to_str(v: &Val) -> String {
    match v {
        Val::Reg(reg) => match reg {
            Reg::RAX => "rax".to_owned(),
            Reg::RBX => "rbx".to_owned(),
            Reg::RSP => "rsp".to_owned(),
            Reg::RDI => "rdi".to_owned(),
        },
        Val::Imm(i) => format!("{}", i),
        Val::RegOffset(reg, offset) => match reg {
            Reg::RAX => format!("[rax - {}]", offset),
            Reg::RBX => format!("[rbx - {}]", offset),
            Reg::RSP => format!("[rsp - {}]", offset),
            Reg::RDI => format!("[rdi - {}]", offset),
        },
    }
}

fn compile_binds(
    binds: &[(String, Expr)],
    body: &Box<Expr>,
    si: i64,
    env: &HashMap<String, i64>,
    orig_env: &HashMap<String, i64>,
    l: &mut i32,
    brk: Option<&String>,
) -> Vec<Instr> {
    match binds {
        [] => Vec::new(),
        [(name, expr), rest @ ..] => {
            let expr_instr = compile_to_instrs(expr, si, env, l, brk);
            // mov expr from rax to stack
            let mov_var_instr = Instr::IMov(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX));
            // update env to point to expr
            //
            // see if bind was set before
            let nenv = match env.get(name) {
                // if bind was set before, check if it was set before this list of binds
                Some(_) => match orig_env.get(name) {
                    Some(_) => env.update(name.to_owned(), si * 8),
                    // was not bound before compiling this list of lets
                    None => panic!("Duplicate binding {name}"),
                },
                None => env.update(name.to_owned(), si * 8),
            };

            // eval the rest of the binds with the updated env
            let rest_instrs = compile_binds(rest, body, si + 1, &nenv, orig_env, l, brk);

            let mut instrs = Vec::new();
            instrs.extend(expr_instr);
            instrs.push(mov_var_instr);
            instrs.extend(rest_instrs);
            if rest.len() == 0 {
                instrs.extend(compile_to_instrs(body, si + 1, &nenv, l, brk));
            }
            instrs
        }
    }
}

fn compile_to_instrs(
    e: &Expr,
    si: i64,
    env: &HashMap<String, i64>,
    l: &mut i32,
    brk: Option<&String>,
) -> Vec<Instr> {
    match e {
        Expr::Number(n) => {
            // make this a dynamic error
            // if *n > (i64::pow(2, 62) - 1) || *n < i64::pow(2, 62).neg() {
            //     panic!("Invalid. Number overflow {}", n)
            // }
            vec![
                Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n)),
                Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(MIN)),
                Instr::ICmp(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)),
                Instr::IJle(OVERFLOW_LABEL.to_owned()),
                Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(MAX)),
                Instr::ICmp(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)),
                Instr::IJge(OVERFLOW_LABEL.to_owned()),
                Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n << 1)),
            ]
        }
        Expr::Boolean(b) => match b {
            true => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(TRUE))], // 0b0011
            false => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(FALSE))], // 0b0001
        },        
        Expr::Id(name) if name == "input" => vec![
            Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI)),
            Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(MIN)),
            Instr::ICmp(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)),
            Instr::IJle(OVERFLOW_LABEL.to_owned()),
            Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(MAX)),
            Instr::ICmp(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)),
            Instr::IJge(OVERFLOW_LABEL.to_owned()),
        ],
        Expr::Id(name) => match env.get(name) {
            Some(val) => vec![Instr::IMov(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RSP, *val),
            )],
            None => panic!("Unbound variable identifier {name}"),
        },
        Expr::Let(binds, body) => {
            // evaluate the bind expr
            // store that result in a variable
            // store that vars location in the env
            // do the rest of the binds
            // evaluate the body
            compile_binds(binds.as_slice(), body, si, env, env, l, brk)
        }
        Expr::UnOp(op, e) => match op {
            Op1::Add1 => {
                let mut instrs = compile_to_instrs(e, si, env, l, brk);
                instrs.extend(vec![
                    Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)), // save e1 in rbx
                    Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(1)),        // check if e1 is bool
                    Instr::ICmp(Val::Reg(Reg::RBX), Val::Imm(1)),
                    Instr::IJe("throw_invalid_arg".to_owned()), // error if e1 is bool
                ]);
                instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1 << 1)));
                instrs
            }
            Op1::Sub1 => {
                let mut instrs = compile_to_instrs(e, si, env, l, brk);
                instrs.extend(vec![
                    Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)), // save e1 in rbx
                    Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(1)),        // check if e1 is bool
                    Instr::ICmp(Val::Reg(Reg::RBX), Val::Imm(1)),
                    Instr::IJe("throw_invalid_arg".to_owned()), // error if e1 is bool
                ]);
                instrs.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(1 << 1)));
                instrs
            }
            Op1::IsNum => {
                let mut instrs = compile_to_instrs(e, si, env, l, brk);
                instrs.push(Instr::IAnd(Val::Reg(Reg::RAX), Val::Imm(1))); // and rax with 1
                instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(0)));
                let is_num_label = new_label(l, "is_num");
                let end_is_num_label = new_label(l, "end_is_num");
                instrs.push(Instr::IJe(is_num_label.to_owned()));
                // else block
                instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(FALSE)));
                instrs.push(Instr::IJmp(end_is_num_label.to_owned()));
                // then block
                instrs.push(Instr::ILabel(is_num_label));
                instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(TRUE)));
                // endif
                instrs.push(Instr::ILabel(end_is_num_label));
                instrs
            }
            Op1::IsBool => {
                let mut instrs = compile_to_instrs(e, si, env, l, brk);
                instrs.push(Instr::IAnd(Val::Reg(Reg::RAX), Val::Imm(1))); // and rax with 1
                instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(1)));
                let is_bool_label = new_label(l, "is_bool");
                let end_is_bool_label = new_label(l, "end_is_bool");
                instrs.push(Instr::IJe(is_bool_label.to_owned()));
                // else block
                instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(FALSE)));
                instrs.push(Instr::IJmp(end_is_bool_label.to_owned()));
                // then block
                instrs.push(Instr::ILabel(is_bool_label));
                instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(TRUE)));
                // endif
                instrs.push(Instr::ILabel(end_is_bool_label));
                instrs
            }
        },
        Expr::BinOp(op, e1, e2) => match op {
            Op2::Plus => {
                let mut instrs = Vec::new();
                let e1_instrs = compile_to_instrs(e1, si, env, l, brk);
                let e2_instrs = compile_to_instrs(e2, si + 1, env, l, brk);

                instrs.extend(e1_instrs);
                instrs.extend(vec![
                    Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)), // save e1 in rbx
                    Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(1)),        // check if e1 is bool
                    Instr::ICmp(Val::Reg(Reg::RBX), Val::Imm(1)),
                    Instr::IJe("throw_invalid_arg".to_owned()), // error if e1 is bool
                ]);
                instrs.push(Instr::IMov(
                    Val::RegOffset(Reg::RSP, si * 8),
                    Val::Reg(Reg::RAX),
                ));
                instrs.extend(e2_instrs);
                instrs.extend(vec![
                    Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)), // save e1 in rbx
                    Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(1)),        // check if e1 is bool
                    Instr::ICmp(Val::Reg(Reg::RBX), Val::Imm(1)),
                    Instr::IJe("throw_invalid_arg".to_owned()), // error if e1 is bool
                ]);
                instrs.push(Instr::IAdd(
                    Val::Reg(Reg::RAX),
                    Val::RegOffset(Reg::RSP, si * 8),
                ));

                instrs
            }
            Op2::Minus => {
                let mut instrs = Vec::new();
                let e2_instrs = compile_to_instrs(e2, si, env, l, brk);
                let e1_instrs = compile_to_instrs(e1, si + 1, env, l, brk);

                instrs.extend(e2_instrs);
                instrs.extend(vec![
                    Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)), // save e1 in rbx
                    Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(1)),        // check if e1 is bool
                    Instr::ICmp(Val::Reg(Reg::RBX), Val::Imm(1)),
                    Instr::IJe("throw_invalid_arg".to_owned()), // error if e1 is bool
                ]);
                instrs.push(Instr::IMov(
                    Val::RegOffset(Reg::RSP, si * 8),
                    Val::Reg(Reg::RAX),
                ));
                instrs.extend(e1_instrs);
                instrs.extend(vec![
                    Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)), // save e1 in rbx
                    Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(1)),        // check if e1 is bool
                    Instr::ICmp(Val::Reg(Reg::RBX), Val::Imm(1)),
                    Instr::IJe("throw_invalid_arg".to_owned()), // error if e1 is bool
                ]);
                instrs.push(Instr::ISub(
                    Val::Reg(Reg::RAX),
                    Val::RegOffset(Reg::RSP, si * 8),
                ));

                instrs
            }
            Op2::Times => {
                let mut instrs = Vec::new();
                let e1_instrs = compile_to_instrs(e1, si, env, l, brk);
                let e2_instrs = compile_to_instrs(e2, si + 1, env, l, brk);

                instrs.extend(e1_instrs);
                instrs.extend(vec![
                    Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)), // save e1 in rbx
                    Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(1)),        // check if e1 is bool
                    Instr::ICmp(Val::Reg(Reg::RBX), Val::Imm(1)),
                    Instr::IJe("throw_invalid_arg".to_owned()), // error if e1 is bool
                ]);
                instrs.push(Instr::IMov(
                    Val::RegOffset(Reg::RSP, si * 8),
                    Val::Reg(Reg::RAX),
                ));
                instrs.extend(e2_instrs);
                instrs.extend(vec![
                    Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)), // save e1 in rbx
                    Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(1)),        // check if e1 is bool
                    Instr::ICmp(Val::Reg(Reg::RBX), Val::Imm(1)),
                    Instr::IJe("throw_invalid_arg".to_owned()), // error if e1 is bool
                ]);
                instrs.push(Instr::IShr(Val::Reg(Reg::RAX), Val::Imm(1)));
                instrs.push(Instr::IMul(
                    Val::Reg(Reg::RAX),
                    Val::RegOffset(Reg::RSP, si * 8),
                ));

                instrs
            }
            Op2::Equal => {
                let mut instrs = Vec::new();
                let e1_instrs = compile_to_instrs(e1, si, env, l, brk);
                let e2_instrs = compile_to_instrs(e2, si + 1, env, l, brk);
                let offset = si * 8;

                instrs.extend(e1_instrs);
                instrs.push(Instr::IMov(
                    Val::RegOffset(Reg::RSP, offset),
                    Val::Reg(Reg::RAX),
                ));
                instrs.extend(e2_instrs);
                instrs.extend(vec![
                    Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)),
                    Instr::IXor(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, offset)),
                    Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1)),
                    Instr::IJne("throw_invalid_equality".to_owned()),
                    Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset)),
                    Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(TRUE)),
                    Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(FALSE)),
                    Instr::ICmove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)),
                ]);

                instrs
            }
            Op2::Greater => {
                let mut instrs = Vec::new();
                let e1_instrs = compile_to_instrs(e1, si, env, l, brk);
                let e2_instrs = compile_to_instrs(e2, si + 1, env, l, brk);
                let offset = si * 8;

                instrs.extend(e1_instrs);
                instrs.extend(vec![
                    Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)), // save e1 in rbx
                    Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(1)),        // check if e1 is bool
                    Instr::ICmp(Val::Reg(Reg::RBX), Val::Imm(1)),
                    Instr::IJe("throw_invalid_arg".to_owned()), // error if e1 is bool
                ]);
                instrs.push(Instr::IMov(
                    Val::RegOffset(Reg::RSP, offset),
                    Val::Reg(Reg::RAX),
                ));
                instrs.extend(e2_instrs);
                instrs.extend(vec![
                    Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)), // save e2 in rbx
                    Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(1)),        // check if e2 is bool
                    Instr::ICmp(Val::Reg(Reg::RBX), Val::Imm(1)),
                    Instr::IJe("throw_invalid_arg".to_owned()), // error if e2 is bool
                ]);
                instrs.extend(vec![
                    Instr::ICmp(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX)),
                    Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(TRUE)),
                    Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(FALSE)),
                    Instr::ICmovg(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)),
                ]);

                instrs
            }
            Op2::GreaterEqual => {
                let mut instrs = Vec::new();
                let e1_instrs = compile_to_instrs(e1, si, env, l, brk);
                let e2_instrs = compile_to_instrs(e2, si + 1, env, l, brk);
                let offset = si * 8;

                instrs.extend(e1_instrs);
                instrs.extend(vec![
                    Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)), // save e1 in rbx
                    Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(1)),        // check if e1 is bool
                    Instr::ICmp(Val::Reg(Reg::RBX), Val::Imm(1)),
                    Instr::IJe("throw_invalid_arg".to_owned()), // error if e1 is bool
                ]);
                instrs.push(Instr::IMov(
                    Val::RegOffset(Reg::RSP, offset),
                    Val::Reg(Reg::RAX),
                ));
                instrs.extend(e2_instrs);
                instrs.extend(vec![
                    Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)), // save e2 in rbx
                    Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(1)),        // check if e2 is bool
                    Instr::ICmp(Val::Reg(Reg::RBX), Val::Imm(1)),
                    Instr::IJe("throw_invalid_arg".to_owned()), // error if e2 is bool
                ]);
                instrs.extend(vec![
                    Instr::ICmp(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX)),
                    Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(TRUE)),
                    Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(FALSE)),
                    Instr::ICmovge(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)),
                ]);

                instrs
            }
            Op2::Less => {
                let mut instrs = Vec::new();
                let e1_instrs = compile_to_instrs(e1, si, env, l, brk);
                let e2_instrs = compile_to_instrs(e2, si + 1, env, l, brk);
                let offset = si * 8;

                instrs.extend(e1_instrs);
                instrs.extend(vec![
                    Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)), // save e1 in rbx
                    Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(1)),        // check if e1 is bool
                    Instr::ICmp(Val::Reg(Reg::RBX), Val::Imm(1)),
                    Instr::IJe("throw_invalid_arg".to_owned()), // error if e1 is bool
                ]);
                instrs.push(Instr::IMov(
                    Val::RegOffset(Reg::RSP, offset),
                    Val::Reg(Reg::RAX),
                ));
                instrs.extend(e2_instrs);
                instrs.extend(vec![
                    Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)), // save e2 in rbx
                    Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(1)),        // check if e2 is bool
                    Instr::ICmp(Val::Reg(Reg::RBX), Val::Imm(1)),
                    Instr::IJe("throw_invalid_arg".to_owned()), // error if e2 is bool
                ]);
                instrs.extend(vec![
                    Instr::ICmp(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX)),
                    Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(TRUE)),
                    Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(FALSE)),
                    Instr::ICmovl(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)),
                ]);

                instrs
            }
            Op2::LessEqual => {
                let mut instrs = Vec::new();
                let e1_instrs = compile_to_instrs(e1, si, env, l, brk);
                let e2_instrs = compile_to_instrs(e2, si + 1, env, l, brk);
                let offset = si * 8;

                instrs.extend(e1_instrs);
                instrs.extend(vec![
                    Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)), // save e1 in rbx
                    Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(1)),        // check if e1 is bool
                    Instr::ICmp(Val::Reg(Reg::RBX), Val::Imm(1)),
                    Instr::IJe("throw_invalid_arg".to_owned()), // error if e1 is bool
                ]);
                instrs.push(Instr::IMov(
                    Val::RegOffset(Reg::RSP, offset),
                    Val::Reg(Reg::RAX),
                ));
                instrs.extend(e2_instrs);
                instrs.extend(vec![
                    Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)), // save e2 in rbx
                    Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(1)),        // check if e2 is bool
                    Instr::ICmp(Val::Reg(Reg::RBX), Val::Imm(1)),
                    Instr::IJe("throw_invalid_arg".to_owned()), // error if e2 is bool
                ]);
                instrs.extend(vec![
                    Instr::ICmp(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX)),
                    Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(TRUE)),
                    Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(FALSE)),
                    Instr::ICmovle(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)),
                ]);

                instrs
            }
        },
        Expr::If(cond, yes, no) => {
            let mut instrs = Vec::new();
            let cond_instrs = compile_to_instrs(cond, si, env, l, brk); // goto else if false, then for
                                                                        // any other value
            let then_instrs = compile_to_instrs(yes, si + 1, env, l, brk);
            let else_instrs = compile_to_instrs(no, si + 1, env, l, brk);

            instrs.extend(cond_instrs);
            let else_label = new_label(l, "else");
            let endif_label = new_label(l, "endif");
            instrs.extend(vec![
                Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(1)),
                Instr::IJe(else_label.to_owned()), // jmp to else if cond is false
            ]);
            instrs.extend(then_instrs);
            instrs.push(Instr::IJmp(endif_label.to_owned()));
            instrs.push(Instr::ILabel(else_label));
            instrs.extend(else_instrs);
            instrs.push(Instr::ILabel(endif_label));
            instrs
        }
        Expr::Loop(body) => {
            let loop_label = new_label(l, "loop");
            let end_loop_label = new_label(l, "end_loop");
            let mut instrs = Vec::new();
            let body_instrs = compile_to_instrs(body, si, env, l, Some(&end_loop_label));
            instrs.push(Instr::ILabel(loop_label.to_owned()));
            instrs.extend(body_instrs);
            instrs.push(Instr::IJmp(loop_label));
            instrs.push(Instr::ILabel(end_loop_label));
            instrs
        }
        Expr::Break(expr) => {
            let mut instrs = Vec::new();
            let e_instrs = compile_to_instrs(expr, si, env, l, brk);
            instrs.extend(e_instrs);
            match brk {
                Some(b) => instrs.push(Instr::IJmp(b.to_owned())),
                None => panic!("break outside of loop"),
            }
            instrs
        }
        Expr::Block(exprs) => {
            if exprs.is_empty() {
                panic!("Invalid syntax: block requires at least one expression")
            }
            exprs.iter().fold(Vec::new(), |mut acc, expr| { acc.extend(compile_to_instrs(expr, si, env, l, brk)); acc })
        }
        Expr::Set(name, expr) => {
            let save_location = match env.get(name) {
                Some(val) => val,
                None => panic!("Unbound variable identifier {name}"),
            };
            let expr_instrs = compile_to_instrs(expr, si, env, l, brk);
            let mut instrs = Vec::new();
            instrs.extend(expr_instrs);
            // save to location of var in memory
            instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, *save_location), Val::Reg(Reg::RAX))); 
            instrs
        }
    }
}

fn compile(e: &Expr) -> String {
    let mut label_counter: i32 = 0;
    let instrs = compile_to_instrs(e, 2, &HashMap::new(), &mut label_counter, None);
    let mut assembly = String::new();
    for instr in instrs {
        assembly.push_str(instr_to_str(&instr).as_str());
    }
    return assembly;
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let expr = match parse(&in_contents) {
        Ok(valid) => parse_expr(&valid),
        Err(_) => panic!("Invalid syntax"),
    };
    let result = compile(&expr);

    let asm_program = format!(
        "
section .text
extern snek_error
global our_code_starts_here
extern snek_error
throw_invalid_equality:
    mov rdi, 2
    push rsp
    call snek_error
throw_invalid_arg:
    mov rdi, 3
    push rsp
    call snek_error
throw_overflow:
    mov rdi, 4 
    push rsp
    call snek_error
throw_error:
    mov rdi, 7
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
