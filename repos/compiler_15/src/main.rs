use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::Atom::*;
use sexp::*;

use im::HashMap;

const TRUE: i64 = 3;
const FALSE: i64 = 1;
const INT_MIN: i64 = -4611686018427387904;
const INT_MAX: i64 = 4611686018427387903;

const ERR_INVALID_ARGUMENT: i64 = 101;
const ERR_OVERFLOW: i64 = 102;

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
    RDI,
    RSP,
    R12,
}

#[derive(Debug)]
enum Jump {
    JMP,

    JE,
    JNE,
    JL,
    JLE,
    JG,
    JGE,

    JO,
}

#[derive(Debug)]
enum Target {
    Label(String),
}

#[derive(Debug)]
enum Instr {
    Mov(Val, Val),

    Add(Val, Val),
    Sub(Val, Val),
    Mul(Val, Val),

    And(Val, Val),
    // Or(Val, Val),
    Xor(Val, Val),

    Sar(Val, Val),

    Cmp(Reg, Val),
    CMove(Val, Val),
    Jump(Jump, Target),
    Label(String),

    // Push(Reg),
    // Call(String),
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

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let parse_result = parse(&in_contents);
    let expr = match parse_result {
        Ok(sexp) => parse_expr(&sexp),
        _ => panic!("Invalid"),
    };

    let result = compile(&expr);
    let asm_program = format!(
        "
section .text
extern snek_error
global our_code_starts_here

throw_error:
  mov rdi, r12
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

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(num)) => {
            if *num < INT_MIN || *num > INT_MAX {
                panic!("Invalid")
            } else {
                Expr::Number(*num)
            }
        }
        Sexp::Atom(S(boolean)) if boolean == "true" => Expr::Boolean(true),
        Sexp::Atom(S(boolean)) if boolean == "false" => Expr::Boolean(false),
        Sexp::Atom(S(name)) => {
            if name == "let"
                || name == "add1"
                || name == "sub1"
                || name == "isnum"
                || name == "isbool"
                || name == "set!"
                || name == "if"
                || name == "block"
                || name == "loop"
                || name == "break"
            {
                panic!(
                    "Reserved keyword {} cannot be used as an identifier name",
                    name
                )
            } else {
                Expr::Id(name.to_string())
            }
        }
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => {
                Expr::Block(exprs.into_iter().map(parse_expr).collect())
            }
            [Sexp::Atom(S(op)), e] => match op.as_str() {
                "add1"   => Expr::UnOp(Op1::Add1, Box::new(parse_expr(e))),
                "sub1"   => Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e))),
                "isnum"  => Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e))),
                "isbool" => Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e))),
                "loop"   => Expr::Loop(Box::new(parse_expr(e))),
                "break"   => Expr::Break(Box::new(parse_expr(e))),
                _        => panic!("Invalid"),
            }
            [Sexp::Atom(S(set)), Sexp::Atom(S(id)), e] if set == "set!" => {
                if id == "true"
                    || id == "false"
                    || id == "let"
                    || id == "add1"
                    || id == "sub1"
                    || id == "isnum"
                    || id == "isbool"
                    || id == "set!"
                    || id == "if"
                    || id == "block"
                    || id == "loop"
                    || id == "break"
                {
                    panic!(
                        "Reserved keyword {} cannot be used as an identifier name",
                        id
                    )
                } else {
                    Expr::Set(id.to_string(), Box::new(parse_expr(e)))
                }
            }
            [Sexp::Atom(S(op)), e1, e2, e3] if op == "if" => {
                Expr::If(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)), Box::new(parse_expr(e3)))
            }
            [Sexp::Atom(S(op)), Sexp::List(bindings), e] if op == "let" => {
                let parse_binding = |binding: &Sexp| -> (String, Expr) {
                    match binding {
                        Sexp::List(vec) => match &vec[..] {
                            [Sexp::Atom(S(id)), e] => (id.to_string(), parse_expr(e)),
                            _ => panic!("Invalid"),
                        },
                        _ => panic!("Invalid"),
                    }
                };
                Expr::Let(
                    bindings.iter().map(parse_binding).collect(),
                    Box::new(parse_expr(e)),
                )
            }
            [Sexp::Atom(S(op)), e1, e2] => match op.as_str() {
                "+"  => Expr::BinOp(Op2::Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                "-"  => Expr::BinOp(Op2::Minus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                "*"  => Expr::BinOp(Op2::Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                "<"  => Expr::BinOp(Op2::Less, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                "<=" => Expr::BinOp(Op2::LessEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                ">"  => Expr::BinOp(Op2::Greater, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                ">=" => Expr::BinOp(Op2::GreaterEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                "="  => Expr::BinOp(Op2::Equal, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                _    => panic!("Invalid"),
            }
            _ => panic!("Invalid"),
        },
        _ => panic!("Invalid"),
    }
}

fn compile(e: &Expr) -> String {
    let mut label = 0;
    let instr_list = compile_to_instrs(e, 2, &mut HashMap::new(), &mut label, &"".to_string());
    let mut result = "".to_string();

    for instruction in instr_list {
        result += "\n";
        result += &instr_to_str(&instruction);
    }
    result
}

fn compile_to_instrs(e: &Expr, stack_index: i64, env: &mut HashMap<String, i64>, label_index: &mut i32, break_target: &String) -> Vec<Instr> {
    match e {
        Expr::Number(num) => vec![Instr::Mov(Val::Reg(Reg::RAX), Val::Imm(*num << 1))],
        Expr::Boolean(boolean) => {
            if *boolean {
                vec![Instr::Mov(Val::Reg(Reg::RAX), Val::Imm(TRUE))]
            } else {
                vec![Instr::Mov(Val::Reg(Reg::RAX), Val::Imm(FALSE))]
            }
        }
        Expr::Id(id) if id == "input" => vec![Instr::Mov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI))],
        Expr::Id(id) => {
            let index = env.get(id);
            match index {
                Some(index) => vec![Instr::Mov(
                    Val::Reg(Reg::RAX),
                    Val::RegOffset(Reg::RSP, index * 8),
                )],
                None => panic!("{}", format!("Unbound variable identifier {}", id)),
            }
        }
        Expr::Let(bindings, e) => {
            if bindings.len() < 1 {
                panic!("Invalid");
            }

            let mut instrs = vec![];
            let mut increment = 0;
            let restore_env: &mut HashMap<String, i64> = &mut HashMap::new();
            for (id, expr) in bindings {
                if id == "true"
                    || id == "false"
                    || id == "input"
                    || id == "let"
                    || id == "add1"
                    || id == "sub1"
                    || id == "isnum"
                    || id == "isbool"
                    || id == "set!"
                    || id == "if"
                    || id == "block"
                    || id == "loop"
                    || id == "break"
                {
                    panic!(
                        "Reserved keyword {} cannot be used as an identifier name",
                        id
                    )
                }

                if restore_env.get(id) != None {
                    panic!("Duplicate binding")
                }

                if env.get(id) != None {
                    restore_env.insert(id.to_string(), *env.get(id).unwrap());
                } else {
                    restore_env.insert(id.to_string(), -1);
                }

                instrs.append(&mut compile_to_instrs(expr, stack_index + increment, env, label_index, break_target));
                instrs.append(&mut vec![Instr::Mov(
                    Val::RegOffset(Reg::RSP, (stack_index + increment) * 8),
                    Val::Reg(Reg::RAX),
                )]);
                env.insert(id.to_string(), stack_index + increment);
                increment += 1;
            }
            instrs.append(&mut compile_to_instrs(e, stack_index + increment, env, label_index, break_target));
            for (k, v) in restore_env.iter() {
                if *v == -1 {
                    env.remove(k);
                } else {
                    env.insert(k.to_string(), *v);
                }
            }
            instrs
        }
        Expr::UnOp(op, e) => match op {
            Op1::Add1 => {
                let mut instrs = compile_to_instrs(e, stack_index, env, label_index, break_target);
                instrs.append(&mut vec![Instr::Mov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)),
                                        Instr::And(Val::Reg(Reg::RBX), Val::Imm(1)),
                                        Instr::Cmp(Reg::RBX, Val::Imm(0)),
                                        Instr::Mov(Val::Reg(Reg::R12), Val::Imm(ERR_INVALID_ARGUMENT)),
                                        Instr::Jump(Jump::JNE, Target::Label("throw_error".to_string())),

                                        Instr::Add(Val::Reg(Reg::RAX), Val::Imm(2)),

                                        Instr::Mov(Val::Reg(Reg::R12), Val::Imm(ERR_OVERFLOW)),
                                        Instr::Jump(Jump::JO, Target::Label("throw_error".to_string()))]);
                instrs
            }
            Op1::Sub1 => {
                let mut instrs = compile_to_instrs(e, stack_index, env, label_index, break_target);
                instrs.append(&mut vec![Instr::Mov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)),
                                        Instr::And(Val::Reg(Reg::RBX), Val::Imm(1)),
                                        Instr::Cmp(Reg::RBX, Val::Imm(0)),
                                        Instr::Mov(Val::Reg(Reg::R12), Val::Imm(ERR_INVALID_ARGUMENT)),
                                        Instr::Jump(Jump::JNE, Target::Label("throw_error".to_string())),

                                        Instr::Sub(Val::Reg(Reg::RAX), Val::Imm(2)),
                                        
                                        Instr::Mov(Val::Reg(Reg::R12), Val::Imm(ERR_OVERFLOW)),
                                        Instr::Jump(Jump::JO, Target::Label("throw_error".to_string()))]);
                instrs
            }
            Op1::IsNum => {
                let mut instrs = compile_to_instrs(e, stack_index, env, label_index, break_target);
                instrs.append(&mut vec![Instr::And(Val::Reg(Reg::RAX), Val::Imm(1)),
                                        Instr::Cmp(Reg::RAX, Val::Imm(0)),
                                        Instr::Mov(Val::Reg(Reg::RBX), Val::Imm(3)),
                                        Instr::Mov(Val::Reg(Reg::RAX), Val::Imm(1)),
                                        Instr::CMove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))]);
                instrs
            }
            Op1::IsBool => {
                let mut instrs = compile_to_instrs(e, stack_index, env, label_index, break_target);
                let start_label = new_label(label_index, "isbool");
                let end_label = new_label(label_index, "isboolend");
                instrs.append(&mut vec![Instr::Cmp(Reg::RAX, Val::Imm(FALSE)),
                                        Instr::Jump(Jump::JE, Target::Label(start_label.clone())),
                                        Instr::Cmp(Reg::RAX, Val::Imm(TRUE)),
                                        Instr::Jump(Jump::JE, Target::Label(start_label.clone())),
                                        Instr::Mov(Val::Reg(Reg::RAX), Val::Imm(FALSE)),
                                        Instr::Jump(Jump::JMP, Target::Label(end_label.clone())),
                                        Instr::Label(start_label),
                                        Instr::Mov(Val::Reg(Reg::RAX), Val::Imm(TRUE)),
                                        Instr::Label(end_label)]);
                instrs
            }
        },
        Expr::BinOp(op, e1, e2) => match op {
            Op2::Plus => {
                let mut instrs = compile_to_instrs(e1, stack_index, env, label_index, break_target);
                instrs.append(&mut vec![Instr::Mov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)),
                                        Instr::And(Val::Reg(Reg::RBX), Val::Imm(1)),
                                        Instr::Cmp(Reg::RBX, Val::Imm(0)),
                                        Instr::Mov(Val::Reg(Reg::R12), Val::Imm(ERR_INVALID_ARGUMENT)),
                                        Instr::Jump(Jump::JNE, Target::Label("throw_error".to_string()))]);
                instrs.append(&mut vec![Instr::Mov(
                    Val::RegOffset(Reg::RSP, stack_index * 8),
                    Val::Reg(Reg::RAX),
                )]);
                instrs.append(&mut compile_to_instrs(e2, stack_index + 1, env, label_index, break_target));
                instrs.append(&mut vec![Instr::Mov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)),
                                        Instr::And(Val::Reg(Reg::RBX), Val::Imm(1)),
                                        Instr::Cmp(Reg::RBX, Val::Imm(0)),
                                        Instr::Mov(Val::Reg(Reg::R12), Val::Imm(ERR_INVALID_ARGUMENT)),
                                        Instr::Jump(Jump::JNE, Target::Label("throw_error".to_string()))]);
                instrs.append(&mut vec![
                    Instr::Add(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_index * 8)),
                    Instr::Mov(Val::Reg(Reg::R12), Val::Imm(ERR_OVERFLOW)),
                    Instr::Jump(Jump::JO, Target::Label("throw_error".to_string()))
                ]);
                instrs
            }
            Op2::Minus => {
                let mut instrs = compile_to_instrs(e2, stack_index, env, label_index, break_target);
                instrs.append(&mut vec![Instr::Mov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)),
                                        Instr::And(Val::Reg(Reg::RBX), Val::Imm(1)),
                                        Instr::Cmp(Reg::RBX, Val::Imm(0)),
                                        Instr::Mov(Val::Reg(Reg::R12), Val::Imm(ERR_INVALID_ARGUMENT)),
                                        Instr::Jump(Jump::JNE, Target::Label("throw_error".to_string()))]);
                instrs.append(&mut vec![Instr::Mov(
                    Val::RegOffset(Reg::RSP, stack_index * 8),
                    Val::Reg(Reg::RAX),
                )]);
                instrs.append(&mut compile_to_instrs(e1, stack_index + 1, env, label_index, break_target));
                instrs.append(&mut vec![Instr::Mov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)),
                                        Instr::And(Val::Reg(Reg::RBX), Val::Imm(1)),
                                        Instr::Cmp(Reg::RBX, Val::Imm(0)),
                                        Instr::Mov(Val::Reg(Reg::R12), Val::Imm(ERR_INVALID_ARGUMENT)),
                                        Instr::Jump(Jump::JNE, Target::Label("throw_error".to_string()))]);
                instrs.append(&mut vec![
                    Instr::Sub(Val::Reg(Reg::RAX),Val::RegOffset(Reg::RSP, stack_index * 8)),
                    Instr::Mov(Val::Reg(Reg::R12), Val::Imm(ERR_OVERFLOW)),
                    Instr::Jump(Jump::JO, Target::Label("throw_error".to_string()))
                ]);
                instrs
            }
            Op2::Times => {
                let mut instrs = compile_to_instrs(e1, stack_index, env, label_index, break_target);
                instrs.append(&mut vec![Instr::Mov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)),
                                        Instr::And(Val::Reg(Reg::RBX), Val::Imm(1)),
                                        Instr::Cmp(Reg::RBX, Val::Imm(0)),
                                        Instr::Mov(Val::Reg(Reg::R12), Val::Imm(ERR_INVALID_ARGUMENT)),
                                        Instr::Jump(Jump::JNE, Target::Label("throw_error".to_string()))]);
                instrs.append(&mut vec![Instr::Mov(
                    Val::RegOffset(Reg::RSP, stack_index * 8),
                    Val::Reg(Reg::RAX),
                )]);
                instrs.append(&mut compile_to_instrs(e2, stack_index + 1, env, label_index, break_target));
                instrs.append(&mut vec![Instr::Mov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)),
                                        Instr::And(Val::Reg(Reg::RBX), Val::Imm(1)),
                                        Instr::Cmp(Reg::RBX, Val::Imm(0)),
                                        Instr::Mov(Val::Reg(Reg::R12), Val::Imm(ERR_INVALID_ARGUMENT)),
                                        Instr::Jump(Jump::JNE, Target::Label("throw_error".to_string()))]);
                instrs.append(&mut vec![
                    Instr::Sar(Val::Reg(Reg::RAX), Val::Imm(1)),
                    Instr::Mul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_index * 8)),
                    Instr::Mov(Val::Reg(Reg::R12), Val::Imm(ERR_OVERFLOW)),
                    Instr::Jump(Jump::JO, Target::Label("throw_error".to_string()))
                ]);
                instrs
            }
            Op2::Equal => {
                let mut instrs = compile_to_instrs(e2, stack_index, env, label_index, break_target);
                instrs.append(&mut vec![Instr::Mov(
                    Val::RegOffset(Reg::RSP, stack_index * 8),
                    Val::Reg(Reg::RAX),
                )]);
                instrs.append(&mut compile_to_instrs(e1, stack_index + 1, env, label_index, break_target));
                instrs.append(&mut vec![
                    Instr::Mov(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, stack_index * 8)),
                    Instr::Xor(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)),

                    Instr::Mov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)),
                    Instr::And(Val::Reg(Reg::RBX), Val::Imm(1)),
                    Instr::Cmp(Reg::RBX, Val::Imm(1)),
                    Instr::Mov(Val::Reg(Reg::R12), Val::Imm(ERR_INVALID_ARGUMENT)),
                    Instr::Jump(Jump::JE, Target::Label("throw_error".to_string())),
                    
                    Instr::Cmp(Reg::RAX, Val::Imm(0)),
                    Instr::Mov(Val::Reg(Reg::RBX), Val::Imm(TRUE)),
                    Instr::Mov(Val::Reg(Reg::RAX), Val::Imm(FALSE)),
                    Instr::CMove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))
                ]);
                instrs
            }
            Op2::Greater => {
                let start_label = new_label(label_index, "gt");
                let end_label = new_label(label_index, "gtend");

                let mut instrs = compile_to_instrs(e2, stack_index, env, label_index, break_target);
                instrs.append(&mut vec![Instr::Mov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)),
                                        Instr::And(Val::Reg(Reg::RBX), Val::Imm(1)),
                                        Instr::Cmp(Reg::RBX, Val::Imm(0)),
                                        Instr::Mov(Val::Reg(Reg::R12), Val::Imm(ERR_INVALID_ARGUMENT)),
                                        Instr::Jump(Jump::JNE, Target::Label("throw_error".to_string()))]);
                instrs.append(&mut vec![Instr::Mov(
                    Val::RegOffset(Reg::RSP, stack_index * 8),
                    Val::Reg(Reg::RAX),
                )]);

                instrs.append(&mut compile_to_instrs(e1, stack_index + 1, env, label_index, break_target));
                instrs.append(&mut vec![Instr::Mov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)),
                                        Instr::And(Val::Reg(Reg::RBX), Val::Imm(1)),
                                        Instr::Cmp(Reg::RBX, Val::Imm(0)),
                                        Instr::Mov(Val::Reg(Reg::R12), Val::Imm(ERR_INVALID_ARGUMENT)),
                                        Instr::Jump(Jump::JNE, Target::Label("throw_error".to_string()))]);
                instrs.append(&mut vec![
                    Instr::Mov(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, stack_index * 8)),
                    Instr::Cmp(Reg::RAX, Val::Reg(Reg::RBX)),
                    Instr::Jump(Jump::JG, Target::Label(start_label.clone())),
                    Instr::Mov(Val::Reg(Reg::RAX), Val::Imm(FALSE)),
                    Instr::Jump(Jump::JMP, Target::Label(end_label.clone())),
                    Instr::Label(start_label),
                    Instr::Mov(Val::Reg(Reg::RAX), Val::Imm(TRUE)),
                    Instr::Label(end_label)
                ]);
                instrs
            }
            Op2::GreaterEqual => {
                let start_label = new_label(label_index, "ge");
                let end_label = new_label(label_index, "geend");

                let mut instrs = compile_to_instrs(e2, stack_index, env, label_index, break_target);
                instrs.append(&mut vec![Instr::Mov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)),
                                        Instr::And(Val::Reg(Reg::RBX), Val::Imm(1)),
                                        Instr::Cmp(Reg::RBX, Val::Imm(0)),
                                        Instr::Mov(Val::Reg(Reg::R12), Val::Imm(ERR_INVALID_ARGUMENT)),
                                        Instr::Jump(Jump::JNE, Target::Label("throw_error".to_string()))]);
                instrs.append(&mut vec![Instr::Mov(
                    Val::RegOffset(Reg::RSP, stack_index * 8),
                    Val::Reg(Reg::RAX),
                )]);
                
                instrs.append(&mut compile_to_instrs(e1, stack_index + 1, env, label_index, break_target));
                instrs.append(&mut vec![Instr::Mov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)),
                                        Instr::And(Val::Reg(Reg::RBX), Val::Imm(1)),
                                        Instr::Cmp(Reg::RBX, Val::Imm(0)),
                                        Instr::Mov(Val::Reg(Reg::R12), Val::Imm(ERR_INVALID_ARGUMENT)),
                                        Instr::Jump(Jump::JNE, Target::Label("throw_error".to_string()))]);
                instrs.append(&mut vec![
                    Instr::Mov(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, stack_index * 8)),
                    Instr::Cmp(Reg::RAX, Val::Reg(Reg::RBX)),
                    Instr::Jump(Jump::JGE, Target::Label(start_label.clone())),
                    Instr::Mov(Val::Reg(Reg::RAX), Val::Imm(FALSE)),
                    Instr::Jump(Jump::JMP, Target::Label(end_label.clone())),
                    Instr::Label(start_label),
                    Instr::Mov(Val::Reg(Reg::RAX), Val::Imm(TRUE)),
                    Instr::Label(end_label)
                ]);
                instrs
            }
            Op2::Less => {
                let start_label = new_label(label_index, "lt");
                let end_label = new_label(label_index, "ltend");

                let mut instrs = compile_to_instrs(e2, stack_index, env, label_index, break_target);
                instrs.append(&mut vec![Instr::Mov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)),
                                        Instr::And(Val::Reg(Reg::RBX), Val::Imm(1)),
                                        Instr::Cmp(Reg::RBX, Val::Imm(0)),
                                        Instr::Mov(Val::Reg(Reg::R12), Val::Imm(ERR_INVALID_ARGUMENT)),
                                        Instr::Jump(Jump::JNE, Target::Label("throw_error".to_string()))]);
                instrs.append(&mut vec![Instr::Mov(
                    Val::RegOffset(Reg::RSP, stack_index * 8),
                    Val::Reg(Reg::RAX),
                )]);
                
                instrs.append(&mut compile_to_instrs(e1, stack_index + 1, env, label_index, break_target));
                instrs.append(&mut vec![Instr::Mov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)),
                                        Instr::And(Val::Reg(Reg::RBX), Val::Imm(1)),
                                        Instr::Cmp(Reg::RBX, Val::Imm(0)),
                                        Instr::Mov(Val::Reg(Reg::R12), Val::Imm(ERR_INVALID_ARGUMENT)),
                                        Instr::Jump(Jump::JNE, Target::Label("throw_error".to_string()))]);
                instrs.append(&mut vec![
                    Instr::Mov(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, stack_index * 8)),
                    Instr::Cmp(Reg::RAX, Val::Reg(Reg::RBX)),
                    Instr::Jump(Jump::JL, Target::Label(start_label.clone())),
                    Instr::Mov(Val::Reg(Reg::RAX), Val::Imm(FALSE)),
                    Instr::Jump(Jump::JMP, Target::Label(end_label.clone())),
                    Instr::Label(start_label),
                    Instr::Mov(Val::Reg(Reg::RAX), Val::Imm(TRUE)),
                    Instr::Label(end_label)
                ]);
                instrs
            }
            Op2::LessEqual => {
                let start_label = new_label(label_index, "le");
                let end_label = new_label(label_index, "leend");

                let mut instrs = compile_to_instrs(e2, stack_index, env, label_index, break_target);
                instrs.append(&mut vec![Instr::Mov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)),
                                        Instr::And(Val::Reg(Reg::RBX), Val::Imm(1)),
                                        Instr::Cmp(Reg::RBX, Val::Imm(0)),
                                        Instr::Mov(Val::Reg(Reg::R12), Val::Imm(ERR_INVALID_ARGUMENT)),
                                        Instr::Jump(Jump::JNE, Target::Label("throw_error".to_string()))]);
                instrs.append(&mut vec![Instr::Mov(
                    Val::RegOffset(Reg::RSP, stack_index * 8),
                    Val::Reg(Reg::RAX),
                )]);
                
                instrs.append(&mut compile_to_instrs(e1, stack_index + 1, env, label_index, break_target));
                instrs.append(&mut vec![Instr::Mov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)),
                                        Instr::And(Val::Reg(Reg::RBX), Val::Imm(1)),
                                        Instr::Cmp(Reg::RBX, Val::Imm(0)),
                                        Instr::Mov(Val::Reg(Reg::R12), Val::Imm(ERR_INVALID_ARGUMENT)),
                                        Instr::Jump(Jump::JNE, Target::Label("throw_error".to_string()))]);
                instrs.append(&mut vec![
                    Instr::Mov(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, stack_index * 8)),
                    Instr::Cmp(Reg::RAX, Val::Reg(Reg::RBX)),
                    Instr::Jump(Jump::JLE, Target::Label(start_label.clone())),
                    Instr::Mov(Val::Reg(Reg::RAX), Val::Imm(FALSE)),
                    Instr::Jump(Jump::JMP, Target::Label(end_label.clone())),
                    Instr::Label(start_label),
                    Instr::Mov(Val::Reg(Reg::RAX), Val::Imm(TRUE)),
                    Instr::Label(end_label)
                ]);
                instrs
            }
        },
        Expr::If(cond, thn, els) => {
            let else_label = new_label(label_index, "ifelse");
            let end_label = new_label(label_index, "ifend");

            let mut instrs = compile_to_instrs(cond, stack_index, env, label_index, break_target);
            let mut then_instrs = compile_to_instrs(thn, stack_index, env, label_index, break_target);
            let mut else_instrs = compile_to_instrs(els, stack_index, env, label_index, break_target);
            instrs.append(&mut vec![
                Instr::Cmp(Reg::RAX, Val::Imm(FALSE)),
                Instr::Jump(Jump::JE, Target::Label(else_label.clone())),
            ]);
            instrs.append(&mut then_instrs);
            instrs.append(&mut vec![
                Instr::Jump(Jump::JMP, Target::Label(end_label.clone())),
                Instr::Label(else_label),
            ]);
            instrs.append(&mut else_instrs);
            instrs.append(&mut vec![
                Instr::Label(end_label),
            ]);
            instrs
        },
        Expr::Loop(body) => {
            let startloop = new_label(label_index, "loop");
            let endloop = new_label(label_index, "loopend");
            let mut body_instrs = compile_to_instrs(body, stack_index, env, label_index, &endloop);

            let mut instrs = vec![Instr::Label(startloop.clone())];
            instrs.append(&mut body_instrs);
            instrs.append(&mut vec![
                Instr::Jump(Jump::JMP, Target::Label(startloop.clone())),
                Instr::Label(endloop.clone()),
            ]);
            instrs
        },
        Expr::Break(e) => {
            if break_target == "" {
                panic!("break expression appears outside of any loop")
            }

            let mut instrs = compile_to_instrs(e, stack_index, env, label_index, break_target);
            instrs.append(&mut vec![
                Instr::Jump(Jump::JMP, Target::Label(break_target.clone())),
            ]);
            instrs
        },
        Expr::Set(id, e) => {
            if id == "true"
                    || id == "false"
                    || id == "input"
                    || id == "let"
                    || id == "add1"
                    || id == "sub1"
                    || id == "isnum"
                    || id == "isbool"
                    || id == "set!"
                    || id == "if"
                    || id == "block"
                    || id == "loop"
                    || id == "break"
            {
                panic!(
                    "Reserved keyword {} cannot be used as an identifier name",
                    id
                )
            }
            
            let mut instrs = compile_to_instrs(e, stack_index, env, label_index, break_target);
            let index = env.get(id);

            let mut after_instrs = match index {
                Some(index) => vec![
                    Instr::Mov(Val::RegOffset(Reg::RSP, index * 8), Val::Reg(Reg::RAX))
                ],
                None => panic!("{}", format!("Unbound variable identifier {}", id)),
            };

            instrs.append(&mut after_instrs);
            instrs
        },
        Expr::Block(statements) => {
            if statements.len() < 1 {
                panic!("Invalid");
            }

            let mut instrs = vec![];
            for statement in statements {
                let mut curr_instrs = compile_to_instrs(&statement, stack_index, env, label_index, break_target);
                instrs.append(&mut curr_instrs);
            }

            instrs
        },
    }
}

fn new_label(l: &mut i32, s: &str) -> String {
    let current = *l;
    *l += 1;
    format!("{s}_{current}")
}

fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::Mov(val1, val2) => format!("mov {}, {}", val_to_str(val1), val_to_str(val2)),

        Instr::Add(val1, val2) => format!("add {}, {}", val_to_str(val1), val_to_str(val2)),
        Instr::Sub(val1, val2) => format!("sub {}, {}", val_to_str(val1), val_to_str(val2)),
        Instr::Mul(val1, val2) => format!("imul {}, {}", val_to_str(val1), val_to_str(val2)),

        Instr::And(val1, val2) => format!("and {}, {}", val_to_str(val1), val_to_str(val2)),
        Instr::Xor(val1, val2) => format!("xor {}, {}", val_to_str(val1), val_to_str(val2)),

        Instr::Sar(val1, val2) => format!("sar {}, {}", val_to_str(val1), val_to_str(val2)),

        Instr::Cmp(reg, val) => format!("cmp {}, {}", reg_to_str(reg), val_to_str(val)),
        Instr::CMove(val1, val2) => format!("cmove {}, {}", val_to_str(val1), val_to_str(val2)),
        Instr::Jump(jump, target) => format!("{} {}", jump_to_str(jump), target_to_str(target)),
        Instr::Label(label) => format!("{}:", label),
    }
}

fn val_to_str(v: &Val) -> String {
    match v {
        Val::Reg(reg) => reg_to_str(reg),
        Val::Imm(val) => i64::to_string(val),
        Val::RegOffset(reg, val) => format!("[{} - {}]", reg_to_str(reg), i64::to_string(val)),
    }
}

fn reg_to_str(r: &Reg) -> String {
    match r {
        Reg::RAX => "rax".to_string(),
        Reg::RBX => "rbx".to_string(),
        Reg::RDI => "rdi".to_string(),
        Reg::RSP => "rsp".to_string(),
        Reg::R12 => "r12".to_string(),
    }
}

fn jump_to_str(j: &Jump) -> String {
    match j {
        Jump::JMP => "jmp".to_string(),

        Jump::JE  =>  "je".to_string(),
        Jump::JNE => "jne".to_string(),
        Jump::JL  =>  "jl".to_string(),
        Jump::JLE => "jle".to_string(),
        Jump::JG  =>  "jg".to_string(),
        Jump::JGE => "jge".to_string(),

        Jump::JO => "jo".to_string(),
    }
}

fn target_to_str(t: &Target) -> String {
    match t {
        Target::Label(string) => string.to_string(),
    }
}
