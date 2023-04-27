use std::collections::HashSet;
use std::env;
use std::fs::File;
use std::io::prelude::*;

use im::HashMap;
use sexp::Atom::*;
use sexp::*;

// ChatGPT prompt: how do I make a constant struct of strings in rust
struct StaticErrors {
    dup_bind: &'static str,
    key_word: &'static str,
    unbound_id: &'static str,
    break_err: &'static str,
}

const STATIC_ERRORS: StaticErrors = StaticErrors {
    dup_bind: "Duplicate binding",
    key_word: "Invalid Identifier: keyword used as identifier",
    unbound_id: "Unbound variable identifier",
    break_err: "break appears outside any loop",
};

// constant array of string slices to hold key words
const KEY_WORDS: [&'static str; 20] = [
    "let", "add1", "sub1", "block", "true", "false", "if", "break", "set!", "+", "-", "*", "<",
    ">", "<=", ">=", "=", "isnum", "isbool", "input",
];

struct Labels {
    end_label: &'static str,
    else_label: &'static str,
    error_label: &'static str,
    start_loop_label: &'static str,
    end_loop_label: &'static str,
}

const LABELS: Labels = Labels {
    end_label: "ifend",
    else_label: "ifelse",
    error_label: "throw_error",
    start_loop_label: "loop",
    end_loop_label: "loopend",
};

#[derive(Debug)]
enum TYPE {
    Numeric,
    Boolean,
}

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
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    Test(Val, Val),

    JMP(String),
    JNZ(String),
    JZ(String),
    JO(String),
    JNE(String),
    JE(String),
    Label(String),

    SAR(Val, Val),
    Xor(Val, Val),
    Cmp(Val, Val),
    Cmove(Val, Val),
    Cmovg(Val, Val),
    Cmovge(Val, Val),
    Cmovl(Val, Val),
    Cmovle(Val, Val),
    Cmovz(Val, Val),
    Cmovnz(Val, Val),
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
    Set(String, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Loop(Box<Expr>),
    Block(Vec<Expr>),
    Break(Box<Expr>),
}

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => {
            if *n >= -4611686018427387904 && *n <= 4611686018427387903 {
                Expr::Number(*n)
            } else {
                panic!("Invalid numeric literal, overflow")
            }
        }
        Sexp::Atom(S(s)) if s == "true" || s == "false" => Expr::Boolean(s == "true"),
        Sexp::Atom(S(s)) => Expr::Id(s.to_string()),
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
            [Sexp::Atom(S(op)), Sexp::List(parsed_binds), e] if op == "let" => {
                if parsed_binds.is_empty() {
                    panic!("Invalid empty let binding")
                }
                let mut bindings = Vec::new();
                for binding in parsed_binds {
                    bindings.push(parse_bind(binding));
                }
                Expr::Let(bindings, Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), Sexp::Atom(S(name)), e] if op == "set!" => {
                Expr::Set(name.to_string(), Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), e1, e2, e3] if op == "if" => Expr::If(
                Box::new(parse_expr(e1)),
                Box::new(parse_expr(e2)),
                Box::new(parse_expr(e3)),
            ),
            [Sexp::Atom(S(op)), e1] if op == "loop" => Expr::Loop(Box::new(parse_expr(e1))),
            [Sexp::Atom(S(op)), e1] if op == "break" => Expr::Break(Box::new(parse_expr(e1))),
            [Sexp::Atom(S(op)), statements @ ..] if op == "block" => {
                if statements.is_empty() {
                    panic!("Invalid empty block")
                }
                let stat_exprs = statements
                    .iter()
                    .map(|statement| parse_expr(statement))
                    .collect::<Vec<Expr>>();
                Expr::Block(stat_exprs)
            }
            _ => panic!("Invalid unable to parse"),
        },
        _ => panic!("Invalid unable to parse"),
    }
}

fn parse_bind(s: &Sexp) -> (String, Expr) {
    match s {
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(i)), e] => {
                // Convert i to a slice ref (&str), then ref b/c of KEY_WORDS type
                if KEY_WORDS.contains(&&i[..]) {
                    panic!("{}", STATIC_ERRORS.key_word);
                }
                (i.to_string(), parse_expr(e))
            }
            _ => panic!("Invalid unable to parse let binding"),
        },
        _ => panic!("Invalid unable to parse let binding"),
    }
}

fn compile_to_instrs(
    e: &Expr,
    si: i32,
    env: &HashMap<String, i32>,
    brake: &String,
    l: &mut i32,
) -> Vec<Instr> {
    match e {
        Expr::Number(n) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm((*n) << 1))],
        Expr::Boolean(b) => {
            if *b {
                vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3))]
            } else {
                vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))]
            }
        }
        Expr::Id(s) if s == "input" => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI))],
        Expr::Id(s) => {
            // im hashmap get returns an optional
            let val = env.get(s);
            match val {
                Some(index) => {
                    if KEY_WORDS.contains(&&s[..]) {
                        panic!("{}", STATIC_ERRORS.key_word)
                    }
                    return vec![Instr::IMov(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, *index),
                    )];
                }
                None => panic!("{} {}", STATIC_ERRORS.unbound_id, s),
            }
        }

        Expr::BinOp(Op2::Plus, subexpr1, subexpr2) => {
            let mut sub_instrs1 = compile_to_instrs(subexpr1, si, env, brake, l);
            let mut sub_instrs2 = compile_to_instrs(subexpr2, si + 1, env, brake, l);
            let stack_offset = si * 8;

            sub_instrs1.append(&mut check_numeric());
            sub_instrs1.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, stack_offset),
                Val::Reg(Reg::RAX),
            ));

            sub_instrs1.append(&mut sub_instrs2);
            sub_instrs1.append(&mut check_numeric());
            sub_instrs1.push(Instr::IAdd(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RSP, stack_offset),
            ));
            sub_instrs1.append(&mut check_overflow());
            sub_instrs1
        }
        Expr::BinOp(Op2::Minus, subexpr1, subexpr2) => {
            // We own sub_instrs
            let mut sub_instrs1 = compile_to_instrs(subexpr2, si, env, brake, l);
            let mut sub_instrs2 = compile_to_instrs(subexpr1, si + 1, env, brake, l);
            let stack_offset = si * 8;

            sub_instrs1.append(&mut check_numeric());
            sub_instrs1.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, stack_offset),
                Val::Reg(Reg::RAX),
            ));

            sub_instrs1.append(&mut sub_instrs2);
            sub_instrs1.append(&mut check_numeric());
            sub_instrs1.push(Instr::ISub(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RSP, stack_offset),
            ));
            sub_instrs1.append(&mut check_overflow());
            sub_instrs1
        }
        Expr::BinOp(Op2::Times, subexpr1, subexpr2) => {
            // We own sub_instrs
            let mut sub_instrs1 = compile_to_instrs(subexpr1, si, env, brake, l);
            let mut sub_instrs2 = compile_to_instrs(subexpr2, si + 1, env, brake, l);
            let stack_offset = si * 8;

            sub_instrs1.append(&mut check_numeric());
            // divide first arg by 2 for multiplication
            sub_instrs1.push(Instr::SAR(Val::Reg(Reg::RAX), Val::Imm(1)));
            sub_instrs1.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, stack_offset),
                Val::Reg(Reg::RAX),
            ));
            sub_instrs1.append(&mut sub_instrs2);
            sub_instrs1.append(&mut check_numeric());
            sub_instrs1.push(Instr::IMul(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RSP, stack_offset),
            ));
            sub_instrs1.append(&mut check_overflow());
            sub_instrs1
        }
        Expr::BinOp(Op2::Equal, subexpr1, subexpr2) => {
            let mut sub_instrs1 = compile_to_instrs(subexpr1, si, env, brake, l);
            let mut sub_instrs2 = compile_to_instrs(subexpr2, si + 1, env, brake, l);
            let stack_offset = si * 8;

            sub_instrs1.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, stack_offset),
                Val::Reg(Reg::RAX),
            ));
            sub_instrs1.append(&mut sub_instrs2);

            // check for same type

            sub_instrs1.append(&mut check_same_type(stack_offset));

            sub_instrs1.append(&mut compare_instrs(
                Instr::Cmove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)),
                stack_offset,
            ));

            sub_instrs1
        }
        Expr::BinOp(Op2::Greater, subexpr1, subexpr2) => {
            let mut sub_instrs1 = compile_to_instrs(subexpr2, si, env, brake, l);
            let mut sub_instrs2 = compile_to_instrs(subexpr1, si + 1, env, brake, l);
            let stack_offset = si * 8;

            sub_instrs1.append(&mut check_numeric());
            sub_instrs1.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, stack_offset),
                Val::Reg(Reg::RAX),
            ));

            sub_instrs1.append(&mut sub_instrs2);
            sub_instrs1.append(&mut check_numeric());

            sub_instrs1.append(&mut compare_instrs(
                Instr::Cmovg(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)),
                stack_offset,
            ));
            sub_instrs1
        }
        Expr::BinOp(Op2::GreaterEqual, subexpr1, subexpr2) => {
            let mut sub_instrs1 = compile_to_instrs(subexpr2, si, env, brake, l);
            let mut sub_instrs2 = compile_to_instrs(subexpr1, si + 1, env, brake, l);
            let stack_offset = si * 8;

            sub_instrs1.append(&mut check_numeric());
            sub_instrs1.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, stack_offset),
                Val::Reg(Reg::RAX),
            ));

            sub_instrs1.append(&mut sub_instrs2);
            sub_instrs1.append(&mut check_numeric());

            sub_instrs1.append(&mut compare_instrs(
                Instr::Cmovge(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)),
                stack_offset,
            ));
            sub_instrs1
        }
        Expr::BinOp(Op2::Less, subexpr1, subexpr2) => {
            let mut sub_instrs1 = compile_to_instrs(subexpr2, si, env, brake, l);
            let mut sub_instrs2 = compile_to_instrs(subexpr1, si + 1, env, brake, l);
            let stack_offset = si * 8;

            sub_instrs1.append(&mut check_numeric());
            sub_instrs1.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, stack_offset),
                Val::Reg(Reg::RAX),
            ));

            sub_instrs1.append(&mut sub_instrs2);
            sub_instrs1.append(&mut check_numeric());

            sub_instrs1.append(&mut compare_instrs(
                Instr::Cmovl(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)),
                stack_offset,
            ));
            sub_instrs1
        }
        Expr::BinOp(Op2::LessEqual, subexpr1, subexpr2) => {
            let mut sub_instrs1 = compile_to_instrs(subexpr2, si, env, brake, l);
            let mut sub_instrs2 = compile_to_instrs(subexpr1, si + 1, env, brake, l);
            let stack_offset = si * 8;

            sub_instrs1.append(&mut check_numeric());
            sub_instrs1.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, stack_offset),
                Val::Reg(Reg::RAX),
            ));

            sub_instrs1.append(&mut sub_instrs2);
            sub_instrs1.append(&mut check_numeric());

            sub_instrs1.append(&mut compare_instrs(
                Instr::Cmovle(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)),
                stack_offset,
            ));
            sub_instrs1
        }
        Expr::UnOp(Op1::Add1, subexpr) => {
            // We own sub_instrs
            let mut sub_instrs = compile_to_instrs(subexpr, si, env, brake, l);
            sub_instrs.append(&mut check_numeric());
            sub_instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(2)));
            sub_instrs.append(&mut check_overflow());
            sub_instrs
        }
        Expr::UnOp(Op1::Sub1, subexpr) => {
            let mut sub_instrs = compile_to_instrs(subexpr, si, env, brake, l);
            sub_instrs.append(&mut check_numeric());
            sub_instrs.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(2)));
            sub_instrs.append(&mut check_overflow());
            sub_instrs
        }
        Expr::UnOp(Op1::IsNum, subexpr) => {
            let mut sub_instrs = compile_to_instrs(subexpr, si, env, brake, l);
            sub_instrs.append(&mut check_type(TYPE::Numeric));
            sub_instrs
        }
        Expr::UnOp(Op1::IsBool, subexpr) => {
            let mut sub_instrs = compile_to_instrs(subexpr, si, env, brake, l);
            sub_instrs.append(&mut check_type(TYPE::Boolean));
            sub_instrs
        }
        Expr::Let(binds, subexpr) => {
            if has_dup_binding(binds) {
                panic!("{}", STATIC_ERRORS.dup_bind);
            }

            let mut stack_index = si;
            let mut b_instrs = Vec::new();
            let mut nenv = env.clone();

            for b in binds {
                // compile the body of the let
                let mut lb_instrs = compile_to_instrs(&b.1, stack_index, &nenv, brake, l);
                lb_instrs.push(Instr::IMov(
                    Val::RegOffset(Reg::RSP, stack_index * 8),
                    Val::Reg(Reg::RAX),
                ));
                b_instrs.append(&mut lb_instrs);
                nenv = nenv.update(b.0.to_string(), stack_index * 8);
                stack_index += 1;
            }
            let mut body_instrs = compile_to_instrs(subexpr, stack_index, &nenv, brake, l);
            b_instrs.append(&mut body_instrs);
            b_instrs
        }
        Expr::If(cond, then, els) => {
            let end_label = new_label(l, LABELS.end_label);
            let els_label = new_label(l, LABELS.else_label);

            let mut cond_instrs = compile_to_instrs(cond, si, env, brake, l);
            let mut then_instrs = compile_to_instrs(then, si, env, brake, l);
            let mut els_instrs = compile_to_instrs(els, si, env, brake, l);

            cond_instrs.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::Imm(1)));
            cond_instrs.push(Instr::JE(els_label.clone()));

            cond_instrs.append(&mut then_instrs);
            // unconditionally jump to the end label
            cond_instrs.push(Instr::JMP(end_label.clone()));

            cond_instrs.push(Instr::Label(els_label.clone()));
            cond_instrs.append(&mut els_instrs);
            cond_instrs.push(Instr::Label(end_label.clone()));
            cond_instrs
        }
        Expr::Block(b_exprs) => {
            let mut b_instrs = Vec::new();
            for expr in b_exprs {
                let mut res_instrs = compile_to_instrs(expr, si, env, brake, l);
                b_instrs.append(&mut res_instrs);
            }
            b_instrs
        }
        Expr::Set(name, subexpr) => {
            // ask the environment for the stack index where name is stored
            let val = env.get(name);
            let var_index = match val {
                Some(index) => *index,
                None => panic!("{} {}", STATIC_ERRORS.unbound_id, name),
            };

            // change binding to eval of subexpr
            let mut sub_instrs = compile_to_instrs(subexpr, si, env, brake, l);
            sub_instrs.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, var_index),
                Val::Reg(Reg::RAX),
            ));
            sub_instrs
        }
        Expr::Loop(subexpr) => {
            let start_loop = new_label(l, LABELS.start_loop_label);
            let end_loop = new_label(l, LABELS.end_loop_label);
            let mut loop_instrs = Vec::new();
            loop_instrs.push(Instr::Label(start_loop.clone()));

            let mut loop_body_instrs = compile_to_instrs(subexpr, si, env, &end_loop, l);
            loop_instrs.append(&mut loop_body_instrs);
            loop_instrs.push(Instr::JMP(start_loop.clone()));
            loop_instrs.push(Instr::Label(end_loop.clone()));

            loop_instrs
        }
        Expr::Break(subexpr) => {
            if brake == "" {
                panic!("{}", STATIC_ERRORS.break_err);
            }

            let mut sub_instrs = compile_to_instrs(subexpr, si, env, brake, l);
            sub_instrs.push(Instr::JMP(brake.clone()));
            sub_instrs
        }
    }
}

fn new_label(l: &mut i32, s: &str) -> String {
    let current = *l;
    *l += 1;
    format!("{s}_{current}")
}

// Functional structure idea from StackOverflow - https://stackoverflow.com/questions/58368801/how-do-i-check-if-a-thing-is-in-a-vector
fn has_dup_binding(binds: &Vec<(String, Expr)>) -> bool {
    let mut var_names: HashSet<&str> = HashSet::new();
    binds.iter().any(|b| !(var_names.insert(&b.0)))
}

fn compare_instrs(cmov_in: Instr, stack_offset: i32) -> Vec<Instr> {
    vec![
        Instr::Cmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)),
        Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)),
        Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)),
        cmov_in,
    ]
}

fn check_type(expr_type: TYPE) -> Vec<Instr> {
    let j_instr = match expr_type {
        TYPE::Boolean => Instr::Cmovnz(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)),
        TYPE::Numeric => Instr::Cmovz(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)),
    };
    vec![
        Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)),
        Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)),
        Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)),
        j_instr,
    ]
}

fn check_same_type(stack_offset: i32) -> Vec<Instr> {
    vec![
        Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)),
        Instr::Xor(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, stack_offset)),
        Instr::Test(Val::Reg(Reg::RBX), Val::Imm(1)),
        Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(18)),
        Instr::JNE(LABELS.error_label.to_string()),
    ]
}

// check that the value in RAX is numeric, jump to error if not
fn check_numeric() -> Vec<Instr> {
    vec![
        Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)),
        Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(18)),
        Instr::JNZ(LABELS.error_label.to_string()),
    ]
}

fn check_overflow() -> Vec<Instr> {
    vec![
        Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(17)),
        Instr::JO(LABELS.error_label.to_string()),
    ]
}

fn compile(e: &Expr) -> String {
    let env: HashMap<String, i32> = HashMap::new();
    let mut l = 1;
    let instrs = compile_to_instrs(e, 2, &env, &String::new(), &mut l);
    instrs
        .iter()
        .map(|instr| instr_to_str(instr))
        .collect::<String>()
}

fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(val1, val2) => format!("\nmov {}, {}", val_to_str(val1), val_to_str(val2)),
        Instr::IAdd(val1, val2) => format!("\nadd {}, {}", val_to_str(val1), val_to_str(val2)),
        Instr::ISub(val1, val2) => format!("\nsub {}, {}", val_to_str(val1), val_to_str(val2)),
        Instr::IMul(val1, val2) => format!("\nimul {}, {}", val_to_str(val1), val_to_str(val2)),
        Instr::Test(val1, val2) => format!("\ntest {}, {}", val_to_str(val1), val_to_str(val2)),
        Instr::SAR(val1, val2) => format!("\nsar {}, {}", val_to_str(val1), val_to_str(val2)),
        Instr::Xor(val1, val2) => format!("\nxor {}, {}", val_to_str(val1), val_to_str(val2)),
        Instr::Cmp(val1, val2) => format!("\ncmp {}, {}", val_to_str(val1), val_to_str(val2)),

        Instr::Cmove(val1, val2) => format!("\ncmove {}, {}", val_to_str(val1), val_to_str(val2)),
        Instr::Cmovl(val1, val2) => format!("\ncmovl {}, {}", val_to_str(val1), val_to_str(val2)),
        Instr::Cmovle(val1, val2) => format!("\ncmovle {}, {}", val_to_str(val1), val_to_str(val2)),
        Instr::Cmovg(val1, val2) => format!("\ncmovg {}, {}", val_to_str(val1), val_to_str(val2)),
        Instr::Cmovge(val1, val2) => format!("\ncmovge {}, {}", val_to_str(val1), val_to_str(val2)),
        Instr::Cmovz(val1, val2) => format!("\ncmovz {}, {}", val_to_str(val1), val_to_str(val2)),
        Instr::Cmovnz(val1, val2) => format!("\ncmovnz {}, {}", val_to_str(val1), val_to_str(val2)),

        Instr::JMP(target) => format!("\njmp {}", target),
        Instr::JNZ(target) => format!("\njnz {}", target),
        Instr::JZ(target) => format!("\njz {}", target),
        Instr::JO(target) => format!("\njo {}", target),
        Instr::JNE(target) => format!("\njne {}", target),
        Instr::JE(target) => format!("\nje {}", target),
        Instr::Label(name) => format!("\n{}:", name),
    }
}

fn val_to_str(v: &Val) -> String {
    match v {
        Val::Imm(n) => (*n).to_string(),
        Val::Reg(Reg::RAX) => String::from("rax"),
        Val::Reg(Reg::RSP) => String::from("rsp"),
        Val::Reg(Reg::RBX) => String::from("rbx"),
        Val::Reg(Reg::RDI) => String::from("rdi"),
        Val::RegOffset(Reg::RAX, n) => format!("[rax - {}]", *n),
        Val::RegOffset(Reg::RSP, n) => format!("[rsp - {}]", *n),
        Val::RegOffset(Reg::RBX, n) => format!("[rbx - {}]", *n),
        Val::RegOffset(Reg::RDI, n) => format!("[rdi - {}]", *n),
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let parsed_sexp: Result<Sexp, Box<Error>> = parse(&in_contents);
    let expr = match parsed_sexp {
        Ok(p) => parse_expr(&p),
        Err(_) => panic!("Invalid unable to parse"),
    };
    // You will make result hold the result of actually compiling
    let result = compile(&expr);

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
