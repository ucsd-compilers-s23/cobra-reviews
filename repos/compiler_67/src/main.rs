use core::panic;
use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::Atom::*;
use sexp::*;

use im::HashMap;

static RESERVED_KEYWORDS: [&str; 19] = [
    "true", "false", "loop", "break", "set", "if", "let", "add1", "sub1", "isnum", "isbool", "+",
    "-", "*", "<", ">", "<=", ">=", "=",
];

#[derive(Debug)]
enum Val {
    Reg(Reg),
    Imm(i64),
    RegOffset(Reg, i32),
}

#[derive(Debug)]
enum Reg {
    RAX,
    RSP,
    RBX,
    RDI,
    RCX,
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    INeg(Val),
    ICmp(Val, Val),
    ILabel(String),
    IAnd(Val, Val),
    ITest(Val, Val),
    IJmp(String),
    IXor(Val, Val),
    IJNZ(String),
    IJL(String),
    IJLE(String),
    IJG(String),
    IJGE(String),
    IJE(String),
    IJNE(String),
    ICMov(Val, Val),
    ISar(Val),
    IJO(String),
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
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
}

#[derive(Debug)]
enum Expr {
    Number(i64),
    True,
    False,
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

fn new_label(l: &mut i32, s: &str) -> String {
    let current = *l;
    *l += 1;
    format!("{s}_{current}")
}

// Converts Val to Assembly Instruction String
fn val_to_str(v: &Val) -> String {
    match v {
        Val::Reg(Reg::RAX) => String::from("rax"),
        Val::Reg(Reg::RBX) => String::from("rbx"),
        Val::Reg(Reg::RDI) => String::from("rdi"),
        Val::Reg(Reg::RCX) => String::from("rcx"),
        Val::RegOffset(Reg::RSP, offset) => format!("[rsp - {}]", offset),
        Val::Imm(n) => n.to_string(),
        _ => {
            panic!("Invalid Value")
        }
    }
}

// Converts Instruction object to assembly string
fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(v1, v2) => return format!("mov {}, {}", val_to_str(&v1), val_to_str(&v2)),
        Instr::ISub(v1, v2) => {
            return format!("sub {}, {}", val_to_str(&v1), val_to_str(&v2));
        }
        Instr::IAdd(v1, v2) => {
            return format!("add {}, {}", val_to_str(&v1), val_to_str(&v2));
        }
        Instr::IMul(v1, v2) => {
            return format!("imul {}, {}", val_to_str(&v1), val_to_str(&v2));
        }
        Instr::INeg(v1) => {
            return format!("neg {}", val_to_str(&v1));
        }
        Instr::ICmp(v1, v2) => {
            return format!("cmp {}, {}", val_to_str(v1), val_to_str(v2));
        }
        Instr::IJE(label) => {
            return format!("je {}", label);
        }
        Instr::ILabel(label) => {
            return format!("{}:", label);
        }
        Instr::ICMov(v1, v2) => {
            return format!("cmove {}, {}", val_to_str(v1), val_to_str(v2));
        }
        Instr::IAnd(v1, v2) => {
            return format!("and {}, {}", val_to_str(v1), val_to_str(v2));
        }
        Instr::ITest(v1, v2) => {
            return format!("test {}, {}", val_to_str(v1), val_to_str(v2));
        }
        Instr::IJmp(label) => {
            return format!("jmp {}", label);
        }
        Instr::IXor(v1, v2) => {
            return format!("xor {}, {}", val_to_str(v1), val_to_str(v2));
        }
        Instr::IJNZ(label) => {
            return format!("jnz {}", label);
        }
        Instr::IJL(label) => {
            return format!("jl {}", label);
        }
        Instr::IJLE(label) => {
            return format!("jle {}", label);
        }
        Instr::IJG(label) => {
            return format!("jg {}", label);
        }
        Instr::IJGE(label) => {
            return format!("jge {}", label);
        }
        Instr::IJNE(label) => {
            return format!("jne {}", label);
        }
        Instr::ISar(v) => {
            return format!("sar {}, 1", val_to_str(v));
        }
        Instr::IJO(label) => {
            return format!("jo {}", label);
        }
    }
}

// Converts each instruction from vector of instructions into assembly instructions
// and contenate them by new line
fn instrs_to_str(instrs: &Vec<Instr>) -> String {
    instrs
        .iter()
        .map(|c| instr_to_str(c))
        .collect::<Vec<_>>()
        .join("\n")
}

// Parsing the expression
fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => {
            if *n > 4611686018427387903 || *n < -4611686018427387904 {
                panic!("Invalid")
            }
            Expr::Number(i64::try_from(*n).unwrap())
        }
        Sexp::Atom(S(name)) if name == "true" => Expr::True,
        Sexp::Atom(S(name)) if name == "false" => Expr::False,
        Sexp::Atom(S(id)) => match id.as_str() {
            // Reserved keywords
            id_str if RESERVED_KEYWORDS.contains(&id_str) => {
                panic!("keyword")
            }
            _ => Expr::Id(id.to_owned()),
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
            [Sexp::Atom(S(op)), e1, e2] if op == "=" => Expr::BinOp(
                Op2::Equal,
                Box::new(parse_expr(e1)),
                Box::new(parse_expr(e2)),
            ),
            [Sexp::Atom(S(op)), binding, body] if op == "let" => {
                let mut binding_vec: Vec<(String, Expr)> = Vec::new(); // vector of bindings
                let binding = &parse(binding.to_string().as_str()).unwrap();
                match binding {
                    Sexp::List(vec) => {
                        for assgn in vec {
                            match assgn {
                                Sexp::List(vec) => match &vec[..] {
                                    [Sexp::Atom(S(id)), e] => {
                                        // Reserved keywords
                                        if RESERVED_KEYWORDS.contains(&id.as_str()) {
                                            panic!("keyword");
                                        }
                                        if id == "input" {
                                            panic!("keyword");
                                        }
                                        let parsed_expr = parse_expr(e);
                                        binding_vec.push((id.to_owned(), parsed_expr));
                                    }
                                    _ => {
                                        panic!("Invalid")
                                    }
                                },
                                _ => {
                                    panic!("Invalid")
                                }
                            }
                        }
                    }
                    _ => {
                        panic!("Invalid")
                    }
                }
                Expr::Let(binding_vec, Box::new(parse_expr(body)))
            }
            [Sexp::Atom(S(op)), e] if op == "loop" => Expr::Loop(Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "break" => Expr::Break(Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), Sexp::Atom(S(id)), e] if op == "set!" => {
                Expr::Set(id.to_owned(), Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), ..] if op == "block" => {
                let mut exprs_vec: Vec<Expr> = Vec::new();
                for exp_ in &vec[1..] {
                    exprs_vec.push(parse_expr(exp_));
                }
                Expr::Block(exprs_vec)
            }
            [Sexp::Atom(S(op)), condition, then_body, else_body] if op == "if" => Expr::If(
                Box::new(parse_expr(condition)),
                Box::new(parse_expr(then_body)),
                Box::new(parse_expr(else_body)),
            ),
            _ => {
                panic!("Invalid")
            }
        },
        _ => {
            panic!("Invalid")
        }
    }
}

// Generating assembly instruction from parsed expression
fn compile_expr(
    e: &Expr,
    stack_index: i32,
    env: &HashMap<String, i32>,
    commands: &mut Vec<Instr>,
    l: &mut i32,
    break_label: &String,
) {
    match e {
        Expr::Id(id) if id == "input" => {
            commands.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI)));
        }
        Expr::Id(id) => {
            if !env.contains_key(id) {
                panic!("Unbound variable identifier {}", id);
            }
            commands.push(Instr::IMov(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RSP, *env.get(id).unwrap()),
            ))
        }
        Expr::Number(n) => {
            // let overflow_label = new_label(l, "overflow");
            // let overflow_end_label = new_label(l, "overflow_end");
            // commands.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n)));
            // commands.push(Instr::IJO(overflow_label.to_owned()));
            // commands.push(Instr::IJmp(overflow_end_label.to_owned()));
            // commands.push(Instr::ILabel(overflow_label.to_owned()));
            // commands.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(9)));
            // commands.push(Instr::IJmp(String::from("throw_error")));
            // commands.push(Instr::ILabel(overflow_end_label));
            commands.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n << 1)));
        }
        Expr::True => commands.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3))),
        Expr::False => commands.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))),
        Expr::UnOp(Op1::Add1, subexpr) => {
            let overflow_label = new_label(l, "overflow");
            let overflow_end_label = new_label(l, "overflow_end");
            compile_expr(subexpr, stack_index, env, commands, l, break_label);
            // Number check
            commands.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
            commands.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
            commands.push(Instr::IJNZ(String::from("throw_error")));
            // Overflow check
            commands.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(2)));
            commands.push(Instr::IJO(overflow_label.to_owned()));
            commands.push(Instr::IJmp(overflow_end_label.to_owned()));
            commands.push(Instr::ILabel(overflow_label.to_owned()));
            commands.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(9)));
            commands.push(Instr::IJmp(String::from("throw_error")));
            commands.push(Instr::ILabel(overflow_end_label));
        }
        Expr::UnOp(Op1::Sub1, subexpr) => {
            let overflow_label = new_label(l, "overflow");
            compile_expr(subexpr, stack_index, env, commands, l, break_label);
            let overflow_end_label = new_label(l, "overflow_end");
            // Number check
            commands.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
            commands.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
            commands.push(Instr::IJNZ(String::from("throw_error")));
            // Overflow check
            commands.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(2)));
            commands.push(Instr::IJO(overflow_label.to_owned()));
            commands.push(Instr::IJmp(overflow_end_label.to_owned()));
            commands.push(Instr::ILabel(overflow_label.to_owned()));
            commands.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(9)));
            commands.push(Instr::IJmp(String::from("throw_error")));
            commands.push(Instr::ILabel(overflow_end_label));
        }
        Expr::UnOp(Op1::IsNum, subexpr) => {
            compile_expr(subexpr, stack_index, env, commands, l, break_label);
            commands.push(Instr::IAnd(Val::Reg(Reg::RAX), Val::Imm(1)));
            commands.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(1)));
            commands.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(1)));
            commands.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
            commands.push(Instr::ICMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
        }
        Expr::UnOp(Op1::IsBool, subexpr) => {
            compile_expr(subexpr, stack_index, env, commands, l, break_label);
            commands.push(Instr::IAnd(Val::Reg(Reg::RAX), Val::Imm(1)));
            commands.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(1)));
            commands.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
            commands.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
            commands.push(Instr::ICMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
        }
        Expr::BinOp(Op2::Plus, subexpr1, subexpr2) => {
            let mut expr1_vec_cmds: Vec<Instr> = Vec::new();
            let mut expr2_vec_cmds: Vec<Instr> = Vec::new();
            let overflow_label = new_label(l, "overflow");
            let overflow_end_label = new_label(l, "overflow_end");
            compile_expr(
                subexpr1,
                stack_index,
                env,
                &mut expr1_vec_cmds,
                l,
                break_label,
            );
            compile_expr(
                subexpr2,
                stack_index + 1,
                env,
                &mut expr2_vec_cmds,
                l,
                break_label,
            );
            let stack_offset = stack_index * 8;
            commands.extend(expr1_vec_cmds);
            commands.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
            commands.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
            commands.push(Instr::IJNZ(String::from("throw_error")));
            commands.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, stack_offset),
                Val::Reg(Reg::RAX),
            ));
            commands.extend(expr2_vec_cmds);
            commands.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
            commands.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
            commands.push(Instr::IJNZ(String::from("throw_error")));
            commands.push(Instr::IAdd(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RSP, stack_offset),
            ));
            commands.push(Instr::IJO(overflow_label.to_owned()));
            commands.push(Instr::IJmp(overflow_end_label.to_owned()));
            commands.push(Instr::ILabel(overflow_label.to_owned()));
            commands.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(9)));
            commands.push(Instr::IJmp(String::from("throw_error")));
            commands.push(Instr::ILabel(overflow_end_label));
        }
        Expr::BinOp(Op2::Minus, subexpr1, subexpr2) => {
            let mut expr1_vec_cmds: Vec<Instr> = Vec::new();
            let mut expr2_vec_cmds: Vec<Instr> = Vec::new();
            let overflow_label = new_label(l, "overflow");
            let overflow_end_label = new_label(l, "overflow_end");
            compile_expr(
                subexpr1,
                stack_index,
                env,
                &mut expr1_vec_cmds,
                l,
                break_label,
            );
            compile_expr(
                subexpr2,
                stack_index + 1,
                env,
                &mut expr2_vec_cmds,
                l,
                break_label,
            );
            let stack_offset = stack_index * 8;
            commands.extend(expr1_vec_cmds);
            commands.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
            commands.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
            commands.push(Instr::IJNZ(String::from("throw_error")));
            commands.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, stack_offset),
                Val::Reg(Reg::RAX),
            ));
            commands.extend(expr2_vec_cmds);
            commands.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
            commands.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
            commands.push(Instr::IJNZ(String::from("throw_error")));
            // Negate the second operand
            commands.push(Instr::INeg(Val::Reg(Reg::RAX)));
            commands.push(Instr::IAdd(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RSP, stack_offset),
            ));
            commands.push(Instr::IJO(overflow_label.to_owned()));
            commands.push(Instr::IJmp(overflow_end_label.to_owned()));
            commands.push(Instr::ILabel(overflow_label.to_owned()));
            commands.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(9)));
            commands.push(Instr::IJmp(String::from("throw_error")));
            commands.push(Instr::ILabel(overflow_end_label));
        }
        Expr::BinOp(Op2::Times, subexpr1, subexpr2) => {
            let mut expr1_vec_cmds: Vec<Instr> = Vec::new();
            let mut expr2_vec_cmds: Vec<Instr> = Vec::new();
            let overflow_label = new_label(l, "overflow");
            let overflow_end_label = new_label(l, "overflow_end");
            compile_expr(
                subexpr1,
                stack_index,
                env,
                &mut expr1_vec_cmds,
                l,
                break_label,
            );
            compile_expr(
                subexpr2,
                stack_index + 1,
                env,
                &mut expr2_vec_cmds,
                l,
                break_label,
            );
            let stack_offset = stack_index * 8;
            commands.extend(expr1_vec_cmds);
            commands.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
            commands.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
            commands.push(Instr::IJNZ(String::from("throw_error")));
            commands.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, stack_offset),
                Val::Reg(Reg::RAX),
            ));
            commands.extend(expr2_vec_cmds);
            commands.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
            commands.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
            commands.push(Instr::IJNZ(String::from("throw_error")));
            commands.push(Instr::ISar(Val::Reg(Reg::RAX)));
            commands.push(Instr::IMul(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RSP, stack_offset),
            ));
            commands.push(Instr::IJO(overflow_label.to_owned()));
            commands.push(Instr::IJmp(overflow_end_label.to_owned()));
            commands.push(Instr::ILabel(overflow_label.to_owned()));
            commands.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(9)));
            commands.push(Instr::IJmp(String::from("throw_error")));
            commands.push(Instr::ILabel(overflow_end_label));
        }
        Expr::BinOp(Op2::Less, subexpr1, subexpr2) => {
            let mut expr1_vec_cmds: Vec<Instr> = Vec::new();
            let mut expr2_vec_cmds: Vec<Instr> = Vec::new();
            let less_label: String = new_label(l, "less_label");
            let end_label = new_label(l, "end_label");
            compile_expr(
                subexpr1,
                stack_index,
                env,
                &mut expr1_vec_cmds,
                l,
                break_label,
            );
            compile_expr(
                subexpr2,
                stack_index + 1,
                env,
                &mut expr2_vec_cmds,
                l,
                break_label,
            );
            let stack_offset = stack_index * 8;
            commands.extend(expr1_vec_cmds);
            commands.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
            commands.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
            commands.push(Instr::IJNZ(String::from("throw_error")));
            commands.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, stack_offset),
                Val::Reg(Reg::RAX),
            ));
            commands.extend(expr2_vec_cmds);
            commands.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
            commands.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
            commands.push(Instr::IJNZ(String::from("throw_error")));
            commands.push(Instr::ICmp(
                Val::RegOffset(Reg::RSP, stack_offset),
                Val::Reg(Reg::RAX),
            ));
            commands.push(Instr::IJL(less_label.to_owned()));
            commands.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
            commands.push(Instr::IJmp(end_label.to_owned()));
            commands.push(Instr::ILabel(less_label));
            commands.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
            commands.push(Instr::ILabel(end_label));
        }
        Expr::BinOp(Op2::LessEqual, subexpr1, subexpr2) => {
            let mut expr1_vec_cmds: Vec<Instr> = Vec::new();
            let mut expr2_vec_cmds: Vec<Instr> = Vec::new();
            let less_equal_label: String = new_label(l, "less_equal_label");
            let end_label = new_label(l, "end_label");
            compile_expr(
                subexpr1,
                stack_index,
                env,
                &mut expr1_vec_cmds,
                l,
                break_label,
            );
            compile_expr(
                subexpr2,
                stack_index + 1,
                env,
                &mut expr2_vec_cmds,
                l,
                break_label,
            );
            let stack_offset = stack_index * 8;
            commands.extend(expr1_vec_cmds);
            commands.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
            commands.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
            commands.push(Instr::IJNZ(String::from("throw_error")));
            commands.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, stack_offset),
                Val::Reg(Reg::RAX),
            ));
            commands.extend(expr2_vec_cmds);
            commands.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
            commands.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
            commands.push(Instr::IJNZ(String::from("throw_error")));
            commands.push(Instr::ICmp(
                Val::RegOffset(Reg::RSP, stack_offset),
                Val::Reg(Reg::RAX),
            ));
            commands.push(Instr::IJLE(less_equal_label.to_owned()));
            commands.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
            commands.push(Instr::IJmp(end_label.to_owned()));
            commands.push(Instr::ILabel(less_equal_label));
            commands.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
            commands.push(Instr::ILabel(end_label));
        }
        Expr::BinOp(Op2::Greater, subexpr1, subexpr2) => {
            let mut expr1_vec_cmds: Vec<Instr> = Vec::new();
            let mut expr2_vec_cmds: Vec<Instr> = Vec::new();
            let greater_label: String = new_label(l, "greater_label");
            let end_label = new_label(l, "end_label");
            compile_expr(
                subexpr1,
                stack_index,
                env,
                &mut expr1_vec_cmds,
                l,
                break_label,
            );
            compile_expr(
                subexpr2,
                stack_index + 1,
                env,
                &mut expr2_vec_cmds,
                l,
                break_label,
            );
            let stack_offset = stack_index * 8;
            commands.extend(expr1_vec_cmds);
            commands.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
            commands.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
            commands.push(Instr::IJNZ(String::from("throw_error")));
            commands.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, stack_offset),
                Val::Reg(Reg::RAX),
            ));
            commands.extend(expr2_vec_cmds);
            commands.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
            commands.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
            commands.push(Instr::IJNZ(String::from("throw_error")));
            commands.push(Instr::ICmp(
                Val::RegOffset(Reg::RSP, stack_offset),
                Val::Reg(Reg::RAX),
            ));
            commands.push(Instr::IJG(greater_label.to_owned()));
            commands.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
            commands.push(Instr::IJmp(end_label.to_owned()));
            commands.push(Instr::ILabel(greater_label));
            commands.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
            commands.push(Instr::ILabel(end_label));
        }
        Expr::BinOp(Op2::GreaterEqual, subexpr1, subexpr2) => {
            let mut expr1_vec_cmds: Vec<Instr> = Vec::new();
            let mut expr2_vec_cmds: Vec<Instr> = Vec::new();
            let greater_equal_label: String = new_label(l, "greater_equal_label");
            let end_label = new_label(l, "end_label");
            compile_expr(
                subexpr1,
                stack_index,
                env,
                &mut expr1_vec_cmds,
                l,
                break_label,
            );
            compile_expr(
                subexpr2,
                stack_index + 1,
                env,
                &mut expr2_vec_cmds,
                l,
                break_label,
            );
            let stack_offset = stack_index * 8;
            commands.extend(expr1_vec_cmds);
            commands.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
            commands.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
            commands.push(Instr::IJNZ(String::from("throw_error")));
            commands.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, stack_offset),
                Val::Reg(Reg::RAX),
            ));
            commands.extend(expr2_vec_cmds);
            commands.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
            commands.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
            commands.push(Instr::IJNZ(String::from("throw_error")));
            commands.push(Instr::ICmp(
                Val::RegOffset(Reg::RSP, stack_offset),
                Val::Reg(Reg::RAX),
            ));
            commands.push(Instr::IJGE(greater_equal_label.to_owned()));
            commands.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
            commands.push(Instr::IJmp(end_label.to_owned()));
            commands.push(Instr::ILabel(greater_equal_label));
            commands.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
            commands.push(Instr::ILabel(end_label));
        }
        Expr::BinOp(Op2::Equal, subexpr1, subexpr2) => {
            let mut expr1_vec_cmds: Vec<Instr> = Vec::new();
            let mut expr2_vec_cmds: Vec<Instr> = Vec::new();
            compile_expr(
                subexpr1,
                stack_index,
                env,
                &mut expr1_vec_cmds,
                l,
                break_label,
            );
            compile_expr(
                subexpr2,
                stack_index + 1,
                env,
                &mut expr2_vec_cmds,
                l,
                break_label,
            );
            let stack_offset = stack_index * 8;
            commands.extend(expr1_vec_cmds);
            commands.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, stack_offset),
                Val::Reg(Reg::RAX),
            ));
            commands.extend(expr2_vec_cmds);
            commands.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
            commands.push(Instr::IXor(
                Val::Reg(Reg::RBX),
                Val::RegOffset(Reg::RSP, stack_offset),
            ));
            commands.push(Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1)));
            commands.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(7)));
            commands.push(Instr::IJNE(String::from("throw_error")));
            commands.push(Instr::ICmp(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RSP, stack_offset),
            ));
            commands.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
            commands.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
            commands.push(Instr::ICMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
        }
        Expr::Let(bindings, body) => {
            let mut stack_index_copy = stack_index;
            let mut nenv = env.clone();
            // Parse error if no bindings
            if bindings.len() == 0 {
                panic!("Invalid");
            }
            let mut bindings_env: HashMap<String, bool> = HashMap::new();
            for binding in bindings {
                // If variable already exists in the bindings environment,
                // then its duplicate binding
                if bindings_env.contains_key(&binding.0) {
                    panic!("Duplicate binding")
                }
                bindings_env = bindings_env.update(binding.0.clone(), true);
                compile_expr(
                    &binding.1,
                    stack_index_copy,
                    &nenv,
                    commands,
                    l,
                    break_label,
                );
                nenv = nenv.update(binding.0.to_owned(), 8 * stack_index_copy);
                commands.push(Instr::IMov(
                    Val::RegOffset(Reg::RSP, stack_index_copy * 8),
                    Val::Reg(Reg::RAX),
                ));
                stack_index_copy += 1;
            }
            compile_expr(body, stack_index_copy, &nenv, commands, l, break_label);
        }
        Expr::Block(expr_vec) => {
            if expr_vec.len() == 0 {
                panic!("Invalid")
            }
            for expr in expr_vec {
                compile_expr(expr, stack_index, env, commands, l, break_label);
            }
        }
        Expr::If(condition, then_body, else_body) => {
            let end_label = new_label(l, "ifend");
            let else_label = new_label(l, "ifelse");
            compile_expr(&condition, stack_index, env, commands, l, break_label);
            let mut then_commands: Vec<Instr> = Vec::new();
            compile_expr(
                &then_body,
                stack_index,
                env,
                &mut then_commands,
                l,
                break_label,
            );
            let mut else_commands: Vec<Instr> = Vec::new();
            compile_expr(
                &else_body,
                stack_index,
                env,
                &mut else_commands,
                l,
                break_label,
            );
            commands.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(1)));
            commands.push(Instr::IJE(else_label.to_owned()));
            commands.extend(then_commands);
            commands.push(Instr::IJmp(end_label.to_owned()));
            commands.push(Instr::ILabel(else_label.to_owned()));
            commands.extend(else_commands);
            commands.push(Instr::ILabel(end_label.to_owned()));
        }
        Expr::Set(id, expr) => {
            if !env.contains_key(id) {
                panic!("Unbound variable identifier {}", id);
            }
            let offset = *env.get(id).unwrap();
            compile_expr(expr, stack_index, env, commands, l, break_label);
            commands.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, offset),
                Val::Reg(Reg::RAX),
            ));
        }
        Expr::Loop(expr) => {
            let loop_start = new_label(l, "loop");
            let loop_end = new_label(l, "loopend");
            let mut loop_body_commands: Vec<Instr> = Vec::new();
            compile_expr(
                expr,
                stack_index,
                env,
                &mut loop_body_commands,
                l,
                &loop_end,
            );
            commands.push(Instr::ILabel(loop_start.to_owned()));
            commands.extend(loop_body_commands);
            commands.push(Instr::IJmp(loop_start.to_owned()));
            commands.push(Instr::ILabel(loop_end));
        }
        Expr::Break(expr) => {
            if break_label.len() == 0 {
                panic!("break");
            }
            compile_expr(expr, stack_index, env, commands, l, break_label);
            commands.push(Instr::IJmp(break_label.to_owned()));
        }
    }
}

// Takes expression and result vector of instructions' objects
fn compile_to_instrs(e: &Expr) -> Vec<Instr> {
    let mut v: Vec<Instr> = Vec::new();
    let env: HashMap<String, i32> = HashMap::new();
    compile_expr(e, 2, &env, &mut v, &mut 1, &String::from(""));
    return v;
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;
    let sexp_parsed = parse(&in_contents);
    match sexp_parsed {
        Ok(parsed_sexp) => {
            let expr = parse_expr(&parsed_sexp);
            let instrs = compile_to_instrs(&expr);
            let result = instrs_to_str(&instrs);
            let asm_program = format!(
                "
            section .text
            global our_code_starts_here
            extern snek_error
            throw_error:
                mov rdi, rcx
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
        }
        Err(_) => {
            panic!("Invalid")
        }
    }

    // println!("Generated assembly:\n{}", asm_program);

    Ok(())
}
