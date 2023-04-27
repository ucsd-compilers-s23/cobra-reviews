use core::panic;
use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::Atom::*;
use sexp::*;

use im::HashMap;
use std::collections::HashSet;

#[derive(Debug)]
enum Val {
    Reg(Reg),
    Imm(u64),
    RegOffset(Reg, i64),
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
    ITest(Val, Val),
    IJNE(String),
    IJE(String),
    IJmp(String),
    IJO(String),
    ICMovZ(Val, Val),
    ICMovL(Val, Val),
    ICMovG(Val, Val),
    ICMovLE(Val, Val),
    ICMovGE(Val, Val),
    ICMovE(Val, Val),
    IXor(Val, Val),
    IOr(Val, Val),
    ISar(Val, Val),
    ICmp(Val, Val),
    ILabel(String),
    IPush(Reg),
    ICall(String),
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
    Less,
    Greater,
    LessEq,
    GreaterEq,
    Equals,
}

#[derive(Debug)]
enum Expr {
    Number(i64),
    Bool(bool),
    Input,
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    Set(String, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Block(Vec<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
}

const KEYWORDS: &'static [&'static str] = &[
    "true", "false", "input", "let", "set!", "if", "block", "loop", "break", "add1", "sub1",
    "isnum", "isbool", "+", "-", "*", "<", ">", "<=", ">=", "=",
];

fn check_identifier(id: &str) -> &str {
    if KEYWORDS.contains(&id) {
        panic!("Invalid identifier: {id} is a keyword")
    }
    id
}

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => Expr::Number(i64::try_from(*n).unwrap()),
        Sexp::Atom(S(token)) => match token.as_str() {
            "true" => Expr::Bool(true),
            "false" => Expr::Bool(false),
            "input" => Expr::Input,
            _ => Expr::Id(check_identifier(token).to_string()),
        },
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(op)), Sexp::List(binds), e] if op == "let" => {
                if binds.is_empty() {
                    panic!("Invalid let no binding");
                }
                let binds: Vec<_> = binds.iter().map(parse_bind).collect();
                // check for duplicates
                let unique_ids: HashSet<_> = binds.iter().map(|pair| &pair.0).collect();
                if binds.len() != unique_ids.len() {
                    panic!("Duplicate binding");
                }
                Expr::Let(binds, Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), Sexp::Atom(S(id)), e] if op == "set!" => {
                Expr::Set(check_identifier(id).to_string(), Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), cond, e_true, e_false] if op == "if" => Expr::If(
                Box::new(parse_expr(cond)),
                Box::new(parse_expr(e_true)),
                Box::new(parse_expr(e_false)),
            ),
            [Sexp::Atom(S(op)), e] if op == "loop" => Expr::Loop(Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "break" => Expr::Break(Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), ..] if op == "block" => {
                if vec.len() <= 1 {
                    panic!("Invalid empty block");
                }
                let exprs: Vec<_> = vec[1..].iter().map(parse_expr).collect();
                Expr::Block(exprs)
            }
            [Sexp::Atom(S(op)), e] => {
                let op = match op.as_str() {
                    "add1" => Op1::Add1,
                    "sub1" => Op1::Sub1,
                    "isnum" => Op1::IsNum,
                    "isbool" => Op1::IsBool,
                    _ => panic!("parse error: Invalid unary op {op}"),
                };
                Expr::UnOp(op, Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), e1, e2] => {
                let op = match op.as_str() {
                    "+" => Op2::Plus,
                    "-" => Op2::Minus,
                    "*" => Op2::Times,
                    "<" => Op2::Less,
                    ">" => Op2::Greater,
                    "<=" => Op2::LessEq,
                    ">=" => Op2::GreaterEq,
                    "=" => Op2::Equals,
                    _ => panic!("parse error: Invalid binary op {op}"),
                };
                Expr::BinOp(op, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            _ => panic!("parse error: Invalid syntax"),
        },
        _ => panic!("parse error: Invalid syntax"),
    }
}

fn parse_bind(s: &Sexp) -> (String, Expr) {
    if let Sexp::List(vec) = s {
        if let [Sexp::Atom(S(id)), e] = &vec[..] {
            return (check_identifier(id).to_string(), parse_expr(e));
        }
    }
    panic!("parse error: Invalid bind");
}

fn compile_to_instrs(e: &Expr) -> Vec<Instr> {
    let env: HashMap<String, i64> = HashMap::new();
    let mut label_index = 0;
    let mut instrs = Vec::new();
    compile_helper(e, 2, &env, &mut label_index, None, &mut instrs);
    instrs.push(Instr::IJmp("return".to_string()));
    instrs.push(Instr::ILabel("snek_invalid_argument".to_string()));
    instrs.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(2)));
    instrs.push(Instr::IPush(Reg::RSP));
    instrs.push(Instr::ICall("snek_error".to_string()));
    instrs.push(Instr::ILabel("snek_integer_overflow".to_string()));
    instrs.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(1)));
    instrs.push(Instr::IPush(Reg::RSP));
    instrs.push(Instr::ICall("snek_error".to_string()));
    instrs.push(Instr::ILabel("return".to_string()));
    instrs
}

fn to_internal_repr(val: i64) -> u64 {
    if val < -(1 << 62) || val > ((1 << 62) - 1) {
        panic!("Invalid integer literal {val}")
    }
    return (val << 1) as u64;
}

fn compile_helper(
    e: &Expr,
    mut stack_index: i64,
    env: &HashMap<String, i64>,
    label_index: &mut i64,
    loop_end_label: Option<&String>,
    instrs: &mut Vec<Instr>,
) {
    match e {
        Expr::Number(n) => instrs.push(Instr::IMov(
            Val::Reg(Reg::RAX),
            Val::Imm(to_internal_repr(*n)),
        )),
        Expr::Bool(true) => instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3))),
        Expr::Bool(false) => instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))),
        Expr::Input => instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI))),
        Expr::Id(id) => match env.get(id) {
            Option::Some(o) => instrs.push(Instr::IMov(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RSP, *o),
            )),
            _ => panic!("Unbound variable identifier {id}"),
        },
        Expr::Let(binds, body) => {
            let mut env = env.clone();
            for (id, e) in binds.iter() {
                compile_helper(e, stack_index, &env, label_index, loop_end_label, instrs);
                let offset = -8 * stack_index;
                instrs.push(Instr::IMov(
                    Val::RegOffset(Reg::RSP, offset),
                    Val::Reg(Reg::RAX),
                ));
                env = env.update(id.clone(), offset);
                stack_index += 1;
            }
            compile_helper(body, stack_index, &env, label_index, loop_end_label, instrs);
        }
        Expr::UnOp(op, e) => {
            compile_helper(e, stack_index, env, label_index, loop_end_label, instrs);

            // type check
            match op {
                Op1::Add1 | Op1::Sub1 => {
                    instrs.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::IJNE("snek_invalid_argument".to_string()));
                }
                _ => (),
            }

            match op {
                Op1::Add1 => instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(2))),
                Op1::Sub1 => instrs.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(2))),
                Op1::IsNum => {
                    instrs.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
                    instrs.push(Instr::ICMovZ(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                }
                Op1::IsBool => {
                    instrs.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(1)));
                    instrs.push(Instr::ICMovZ(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                }
            }

            // overflow check
            match op {
                Op1::Add1 | Op1::Sub1 => {
                    instrs.push(Instr::IJO("snek_integer_overflow".to_string()));
                }
                _ => (),
            }
        }
        Expr::BinOp(op, e1, e2) => {
            compile_helper(e1, stack_index, env, label_index, loop_end_label, instrs);
            let offset = -8 * stack_index;
            instrs.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, offset),
                Val::Reg(Reg::RAX),
            ));
            compile_helper(
                e2,
                stack_index + 1,
                env,
                label_index,
                loop_end_label,
                instrs,
            );

            // type check
            match op {
                Op2::Equals => {
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IXor(
                        Val::Reg(Reg::RBX),
                        Val::RegOffset(Reg::RSP, offset),
                    ));
                    instrs.push(Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1)));
                    instrs.push(Instr::IJNE("snek_invalid_argument".to_string()));
                }
                _ => {
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IOr(
                        Val::Reg(Reg::RBX),
                        Val::RegOffset(Reg::RSP, offset),
                    ));
                    instrs.push(Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1)));
                    instrs.push(Instr::IJNE("snek_invalid_argument".to_string()));
                }
            }

            match op {
                Op2::Plus => instrs.push(Instr::IAdd(
                    Val::Reg(Reg::RAX),
                    Val::RegOffset(Reg::RSP, offset),
                )),
                Op2::Minus => {
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IMov(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, offset),
                    ));
                    instrs.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                }
                Op2::Times => {
                    instrs.push(Instr::ISar(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::IMul(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, offset),
                    ));
                }
                _ => {
                    instrs.push(Instr::ICmp(
                        Val::RegOffset(Reg::RSP, offset),
                        Val::Reg(Reg::RAX),
                    ));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
                    match op {
                        Op2::Less => {
                            instrs.push(Instr::ICMovL(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)))
                        }
                        Op2::Greater => {
                            instrs.push(Instr::ICMovG(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)))
                        }
                        Op2::LessEq => {
                            instrs.push(Instr::ICMovLE(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)))
                        }
                        Op2::GreaterEq => {
                            instrs.push(Instr::ICMovGE(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)))
                        }
                        Op2::Equals => {
                            instrs.push(Instr::ICMovE(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)))
                        }
                        _ => (),
                    }
                }
            }

            // overflow check
            match op {
                Op2::Plus | Op2::Minus | Op2::Times => {
                    instrs.push(Instr::IJO("snek_integer_overflow".to_string()));
                }
                _ => (),
            }
        }
        Expr::Set(id, e) => {
            compile_helper(e, stack_index, env, label_index, loop_end_label, instrs);
            match env.get(id) {
                Option::Some(o) => instrs.push(Instr::IMov(
                    Val::RegOffset(Reg::RSP, *o),
                    Val::Reg(Reg::RAX),
                )),
                _ => panic!("Unbound variable identifier {id}"),
            }
        }
        Expr::If(cond, true_e, false_e) => {
            compile_helper(cond, stack_index, env, label_index, loop_end_label, instrs);
            instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(1)));
            let false_branch = format!("l{label_index}");
            *label_index += 1;
            let if_end = format!("l{label_index}");
            *label_index += 1;
            instrs.push(Instr::IJE(false_branch.clone()));
            // true branch
            compile_helper(
                true_e,
                stack_index,
                env,
                label_index,
                loop_end_label,
                instrs,
            );
            instrs.push(Instr::IJmp(if_end.clone()));
            // false branch
            instrs.push(Instr::ILabel(false_branch));
            compile_helper(
                false_e,
                stack_index,
                env,
                label_index,
                loop_end_label,
                instrs,
            );
            instrs.push(Instr::ILabel(if_end));
        }
        Expr::Block(exprs) => {
            for e in exprs.iter() {
                compile_helper(e, stack_index, env, label_index, loop_end_label, instrs);
            }
        }
        Expr::Loop(e) => {
            let loop_start = format!("l{label_index}");
            *label_index += 1;
            let loop_end = format!("l{label_index}");
            *label_index += 1;
            instrs.push(Instr::ILabel(loop_start.clone()));
            compile_helper(e, stack_index, env, label_index, Some(&loop_end), instrs);
            instrs.push(Instr::IJmp(loop_start));
            instrs.push(Instr::ILabel(loop_end));
        }
        Expr::Break(e) => match loop_end_label {
            None => panic!("Invalid break"),
            Some(label) => {
                compile_helper(e, stack_index, env, label_index, loop_end_label, instrs);
                instrs.push(Instr::IJmp(label.clone()));
            }
        },
    }
}

fn instr_to_str(i: &Instr) -> String {
    use Instr::*;

    match i {
        IMov(v1, v2) => format!("mov {}, {}", val_to_str(v1), val_to_str(v2)),
        IAdd(v1, v2) => format!("add {}, {}", val_to_str(v1), val_to_str(v2)),
        ISub(v1, v2) => format!("sub {}, {}", val_to_str(v1), val_to_str(v2)),
        IMul(v1, v2) => format!("imul {}, {}", val_to_str(v1), val_to_str(v2)),
        ITest(v1, v2) => format!("test {}, {}", val_to_str(v1), val_to_str(v2)),
        IJNE(label) => format!("jne {}", label),
        IJE(label) => format!("je {}", label),
        IJmp(label) => format!("jmp {}", label),
        IJO(label) => format!("jo {}", label),
        ICMovZ(v1, v2) => format!("cmovz {}, {}", val_to_str(v1), val_to_str(v2)),
        ICMovL(v1, v2) => format!("cmovl {}, {}", val_to_str(v1), val_to_str(v2)),
        ICMovG(v1, v2) => format!("cmovg {}, {}", val_to_str(v1), val_to_str(v2)),
        ICMovLE(v1, v2) => format!("cmovle {}, {}", val_to_str(v1), val_to_str(v2)),
        ICMovGE(v1, v2) => format!("cmovge {}, {}", val_to_str(v1), val_to_str(v2)),
        ICMovE(v1, v2) => format!("cmove {}, {}", val_to_str(v1), val_to_str(v2)),
        IXor(v1, v2) => format!("xor {}, {}", val_to_str(v1), val_to_str(v2)),
        IOr(v1, v2) => format!("or {}, {}", val_to_str(v1), val_to_str(v2)),
        ISar(v1, v2) => format!("sar {}, {}", val_to_str(v1), val_to_str(v2)),
        ICmp(v1, v2) => format!("cmp {}, {}", val_to_str(v1), val_to_str(v2)),
        ILabel(label) => format!("{}:", label),
        IPush(reg) => format!("push {}", reg_to_str(reg)),
        ICall(label) => format!("call {}", label),
    }
}

fn reg_to_str(r: &Reg) -> &str {
    match r {
        Reg::RAX => "rax",
        Reg::RBX => "rbx",
        Reg::RDI => "rdi",
        Reg::RSP => "rsp",
    }
}

fn val_to_str(v: &Val) -> String {
    match v {
        Val::Reg(r) => reg_to_str(r).to_string(),
        Val::Imm(n) => n.to_string(),
        Val::RegOffset(r, o) => format!("[{}{:+}]", reg_to_str(r), o),
    }
}

fn compile(e: &Expr) -> String {
    let instrs = compile_to_instrs(e);
    let instrs: Vec<_> = instrs.iter().map(instr_to_str).collect();
    instrs.join("\n")
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    match parse(&in_contents) {
        Err(err) => {
            panic!("Invalid sexp {}", err.message)
        }
        Ok(expr) => {
            let expr = parse_expr(&expr);

            // You will make result hold the result of actually compiling
            let result = compile(&expr);

            let asm_program = format!(
                "
section .text
extern snek_error
global our_code_starts_here
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
    }
}
