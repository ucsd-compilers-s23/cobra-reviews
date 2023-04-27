use im::{hashmap, HashMap};
use sexp::Atom::*;
use sexp::*;
use std::env;
use std::fs::File;
use std::io::prelude::*;

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
    Xor(Val, Val),
    Cmp(Val, Val),
    Cmove(Val, Val),
    Cmovne(Val, Val),
    Cmovg(Val, Val),
    Cmovl(Val, Val),
    Cmovge(Val, Val),
    Cmovle(Val, Val),
    Label(String),
    JE(String),
    JO(String),
    JNE(String),
    Jmp(String),
    Test(Val, Val),
    Sar(Val, Val),
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
    Comp(Comp),
}
#[derive(Debug)]
enum Comp {
    Gt,
    Lt,
    Geq,
    Leq,
    Eq,
}

#[derive(Debug)]
enum Expr {
    Number(i64),
    Boolean(bool),
    Id(String),
    Input,
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
    match s {
        Sexp::Atom(I(n)) => {
            let num = i64::try_from(*n).unwrap();
            if num > i64::pow(2, 62) - 1 || num < -i64::pow(2, 62) {
                panic!("overflow error: Invalid number: {num}")
            }
            Expr::Number(num << 1)
        }
        Sexp::Atom(S(id)) => {
            if id == "add1"
                || id == "let"
                || id == "sub1"
                || id == "block"
                || id == "break"
                || id == "loop"
                || id == "set!"
                || id == "if"
                || id == "*"
                || id == "+"
                || id == "-"
                || id == "="
                || id == "<"
                || id == ">"
                || id == "<="
                || id == ">="
                || id == "isnum"
                || id == "isbool"
            {
                panic!("parse error: Invalid identifier name. Identifier matches a keyword")
            } else if id == "true" {
                Expr::Boolean(true)
            } else if id == "false" {
                Expr::Boolean(false)
            } else if id == "input" {
                Expr::Input
            } else {
                Expr::Id(id.to_string())
            }
        }
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
            [Sexp::Atom(S(op)), e] if op == "loop" => Expr::Loop(Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "break" => Expr::Break(Box::new(parse_expr(e))),
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
                Op2::Comp(Comp::Lt),
                Box::new(parse_expr(e1)),
                Box::new(parse_expr(e2)),
            ),
            [Sexp::Atom(S(op)), e1, e2] if op == ">" => Expr::BinOp(
                Op2::Comp(Comp::Gt),
                Box::new(parse_expr(e1)),
                Box::new(parse_expr(e2)),
            ),
            [Sexp::Atom(S(op)), e1, e2] if op == "<=" => Expr::BinOp(
                Op2::Comp(Comp::Leq),
                Box::new(parse_expr(e1)),
                Box::new(parse_expr(e2)),
            ),
            [Sexp::Atom(S(op)), e1, e2] if op == ">=" => Expr::BinOp(
                Op2::Comp(Comp::Geq),
                Box::new(parse_expr(e1)),
                Box::new(parse_expr(e2)),
            ),
            [Sexp::Atom(S(op)), e1, e2] if op == "=" => Expr::BinOp(
                Op2::Comp(Comp::Eq),
                Box::new(parse_expr(e1)),
                Box::new(parse_expr(e2)),
            ),
            [Sexp::Atom(S(op)), Sexp::Atom(S(var)), e] if op == "set!" => {
                Expr::Set(var.to_string(), Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), e1, e2, e3] if op == "if" => Expr::If(
                Box::new(parse_expr(e1)),
                Box::new(parse_expr(e2)),
                Box::new(parse_expr(e3)),
            ),
            [Sexp::Atom(S(keyword)), rest @ ..] if keyword == "block" => {
                let mut exps = Vec::new();
                if rest.len() == 0 {
                    panic!("parse error: Invalid block expressions")
                }
                for i in rest {
                    exps.push(parse_expr(i));
                }
                Expr::Block(exps)
            }
            [Sexp::Atom(S(keyword)), Sexp::List(vec), body] if keyword == "let" => {
                let mut bindings = Vec::new();
                if vec.len() == 0 {
                    panic!("parse error: Invalid bindings")
                }
                for i in vec {
                    let binding = parse_bind(i);
                    bindings.push(binding);
                }
                Expr::Let(bindings, Box::new(parse_expr(body)))
            }
            _ => panic!("parse error: Invalid expression"),
        },
        _ => panic!("parse error: Invalid expression"),
    }
}

fn parse_bind(s: &Sexp) -> (String, Expr) {
    let val = match s {
        Sexp::List(vec2) => match &vec2[..] {
            [Sexp::Atom(S(name)), val] => {
                if name == "add1"
                    || name == "sub1"
                    || name == "let"
                    || name == "block"
                    || name == "break"
                    || name == "loop"
                    || name == "set!"
                    || name == "if"
                    || name == "*"
                    || name == "+"
                    || name == "-"
                    || name == "="
                    || name == "<"
                    || name == ">"
                    || name == "<="
                    || name == ">="
                    || name == "isnum"
                    || name == "isbool"
                    || name == "true"
                    || name == "false"
                    || name == "input"
                {
                    panic!("parse error: Invalid identifier name. Identifier matches a keyword")
                } else {
                    (name.to_string(), parse_expr(val))
                }
            }
            _ => panic!("parse error: Invalid binding"),
        },
        _ => panic!("parse error: Invalid binding"),
    };
    val
}

fn compile_to_instrs(
    e: &Expr,
    si: i32,
    env: &HashMap<String, i32>,
    label_env: &HashMap<String, i32>,
) -> (HashMap<String, i32>, Vec<Instr>) {
    match e {
        Expr::Number(n) => (
            label_env.clone(),
            vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n))],
        ),
        Expr::Input => (
            label_env.clone(),
            vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI))],
        ),
        Expr::Boolean(true) => (
            label_env.clone(),
            vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3))],
        ),
        Expr::Boolean(false) => (
            label_env.clone(),
            vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))],
        ),
        Expr::Id(s) => {
            if !env.contains_key(s) {
                panic!("{}", format!("Unbound variable identifier {}", s))
            }
            let offset = env.get(s).unwrap() * 8;
            (
                label_env.clone(),
                vec![Instr::IMov(
                    Val::Reg(Reg::RAX),
                    Val::RegOffset(Reg::RSP, offset),
                )],
            )
        }
        Expr::UnOp(Op1::Add1, subexpr) => {
            let (new_label_env, mut istrs) = compile_to_instrs(subexpr, si, env, label_env);
            istrs.push(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)));
            istrs.push(Instr::JNE("throw_type_error".to_string()));
            istrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(2)));
            istrs.push(Instr::JO("throw_overflow_error".to_string()));
            (new_label_env, istrs)
        }
        Expr::UnOp(Op1::Sub1, subexpr) => {
            let (new_label_env, mut istrs) = compile_to_instrs(subexpr, si, env, label_env);
            istrs.push(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)));
            istrs.push(Instr::JNE("throw_type_error".to_string()));
            istrs.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(2)));
            istrs.push(Instr::JO("throw_overflow_error".to_string()));
            (new_label_env, istrs)
        }
        Expr::UnOp(Op1::IsNum, subexpr) => {
            let (new_label_env, mut istrs) = compile_to_instrs(subexpr, si, env, label_env);
            istrs.push(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)));
            istrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
            istrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
            istrs.push(Instr::Cmove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
            (new_label_env, istrs)
        }
        Expr::UnOp(Op1::IsBool, subexpr) => {
            let (new_label_env, mut istrs) = compile_to_instrs(subexpr, si, env, label_env);
            istrs.push(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)));
            istrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
            istrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
            istrs.push(Instr::Cmovne(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
            (new_label_env, istrs)
        }
        Expr::BinOp(Op2::Plus, e1, e2) => {
            let (new_label_env, mut istrs) = compile_to_instrs(e1, si, env, label_env);
            let (new_label_env, mut e2_instrs) = compile_to_instrs(e2, si + 1, env, &new_label_env);
            let stack_offset = si * 8;

            istrs.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, stack_offset),
                Val::Reg(Reg::RAX),
            ));
            istrs.append(&mut e2_instrs);

            //Type checking
            istrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
            istrs.push(Instr::Xor(
                Val::Reg(Reg::RBX),
                Val::RegOffset(Reg::RSP, stack_offset),
            ));
            istrs.push(Instr::Test(Val::Reg(Reg::RBX), Val::Imm(1)));
            istrs.push(Instr::JNE("throw_type_error".to_string()));

            istrs.push(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)));
            istrs.push(Instr::JNE("throw_type_error".to_string()));

            istrs.push(Instr::IAdd(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RSP, stack_offset),
            ));
            istrs.push(Instr::JO("throw_overflow_error".to_string()));
            (new_label_env, istrs)
        }
        Expr::BinOp(Op2::Minus, e1, e2) => {
            let (new_label_env, mut istrs) = compile_to_instrs(e2, si, env, label_env);
            let (new_label_env, mut e1_instrs) = compile_to_instrs(e1, si + 1, env, &new_label_env);
            let stack_offset = si * 8;

            istrs.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, stack_offset),
                Val::Reg(Reg::RAX),
            ));
            istrs.append(&mut e1_instrs);

            //Type checking
            istrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
            istrs.push(Instr::Xor(
                Val::Reg(Reg::RBX),
                Val::RegOffset(Reg::RSP, stack_offset),
            ));
            istrs.push(Instr::Test(Val::Reg(Reg::RBX), Val::Imm(1)));
            istrs.push(Instr::JNE("throw_type_error".to_string()));

            istrs.push(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)));
            istrs.push(Instr::JNE("throw_type_error".to_string()));

            istrs.push(Instr::ISub(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RSP, stack_offset),
            ));
            istrs.push(Instr::JO("throw_overflow_error".to_string()));
            (new_label_env, istrs)
        }
        Expr::BinOp(Op2::Times, e1, e2) => {
            let (new_label_env, mut istrs) = compile_to_instrs(e1, si, env, label_env);
            let (new_label_env, mut e2_instrs) = compile_to_instrs(e2, si + 1, env, &new_label_env);
            let stack_offset = si * 8;

            istrs.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, stack_offset),
                Val::Reg(Reg::RAX),
            ));
            istrs.append(&mut e2_instrs);

            //Type checking
            istrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
            istrs.push(Instr::Xor(
                Val::Reg(Reg::RBX),
                Val::RegOffset(Reg::RSP, stack_offset),
            ));
            istrs.push(Instr::Test(Val::Reg(Reg::RBX), Val::Imm(1)));
            istrs.push(Instr::JNE("throw_type_error".to_string()));

            istrs.push(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)));
            istrs.push(Instr::JNE("throw_type_error".to_string()));

            istrs.push(Instr::Sar(Val::Reg(Reg::RAX), Val::Imm(1)));

            istrs.push(Instr::IMul(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RSP, stack_offset),
            ));
            istrs.push(Instr::JO("throw_overflow_error".to_string()));
            (new_label_env, istrs)
        }
        Expr::BinOp(Op2::Comp(comp), e1, e2) => {
            let (new_label_env, mut istrs) = compile_to_instrs(e2, si, env, label_env);
            let (new_label_env, mut e1_instrs) = compile_to_instrs(e1, si + 1, env, &new_label_env);
            let stack_offset = si * 8;

            istrs.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, stack_offset),
                Val::Reg(Reg::RAX),
            ));
            istrs.append(&mut e1_instrs);

            //Type checking
            istrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
            istrs.push(Instr::Xor(
                Val::Reg(Reg::RBX),
                Val::RegOffset(Reg::RSP, stack_offset),
            ));
            istrs.push(Instr::Test(Val::Reg(Reg::RBX), Val::Imm(1)));
            istrs.push(Instr::JNE("throw_type_error".to_string()));

            match comp {
                Comp::Eq => (),
                _ => {
                    istrs.push(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)));
                    istrs.push(Instr::JNE("throw_type_error".to_string()));
                }
            }

            istrs.push(Instr::Cmp(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RSP, stack_offset),
            ));
            istrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
            istrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
            match comp {
                Comp::Eq => istrs.push(Instr::Cmove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))),
                Comp::Lt => istrs.push(Instr::Cmovl(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))),
                Comp::Gt => istrs.push(Instr::Cmovg(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))),
                Comp::Leq => istrs.push(Instr::Cmovle(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))),
                Comp::Geq => istrs.push(Instr::Cmovge(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))),
            }
            (new_label_env, istrs)
        }
        Expr::Let(bindings, body) => {
            let mut istrs = Vec::new();
            let mut i = si;
            let mut new_env = env.clone();
            let mut duplicates: HashMap<String, i32> = HashMap::new();
            let mut new_label_env = label_env.clone();
            for (name, val) in bindings {
                if duplicates.contains_key(name) {
                    panic!("Duplicate binding")
                }
                duplicates.insert(name.to_string(), 1);
                let (temp_env, mut val_is) = compile_to_instrs(val, i, &new_env, &new_label_env);
                new_label_env = temp_env;
                let stack_offset = i * 8;
                istrs.append(&mut val_is);
                istrs.push(Instr::IMov(
                    Val::RegOffset(Reg::RSP, stack_offset),
                    Val::Reg(Reg::RAX),
                ));
                new_env.insert(name.to_string(), i);
                i += 1
            }
            let (new_label_env, mut body_is) = compile_to_instrs(body, i, &new_env, &new_label_env);
            istrs.append(&mut body_is);
            (new_label_env, istrs)
        }

        Expr::Loop(e) => {
            let mut istrs = Vec::new();

            let loop_num = label_env.get("loop").unwrap();
            let new_label_env = label_env.update("loop".to_string(), loop_num + 1);
            let new_env = env.update("loop".to_string(), loop_num + 1);
            istrs.push(Instr::Label(format!("loop{loop_num}")));
            let (new_label_env, mut istr) = compile_to_instrs(e, si, &new_env, &new_label_env);
            istrs.append(&mut istr);
            istrs.push(Instr::Jmp(format!("loop{loop_num}")));
            istrs.push(Instr::Label(format!("endloop{loop_num}")));

            (new_label_env, istrs)
        }

        Expr::If(e1, e2, e3) => {
            let if_num = label_env.get("if").unwrap();
            let new_label_env = label_env.update("if".to_string(), if_num + 1);
            let (new_label_env, mut istrs) = compile_to_instrs(e1, si, env, &new_label_env);

            istrs.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::Imm(1)));
            istrs.push(Instr::JE(format!("else{if_num}")));

            let (new_label_env, mut istrs2) = compile_to_instrs(&e2, si, env, &new_label_env);
            istrs.append(&mut istrs2);
            istrs.push(Instr::Jmp(format!("endif{if_num}")));
            istrs.push(Instr::Label(format!("else{if_num}")));

            let (new_label_env, mut istrs3) = compile_to_instrs(&e3, si, env, &new_label_env);
            istrs.append(&mut istrs3);
            istrs.push(Instr::Label(format!("endif{if_num}")));
            (new_label_env, istrs)
        }

        Expr::Break(e) => {
            let (new_label_env, mut istrs) = compile_to_instrs(e, si, env, label_env);
            let loop_num = env.get("loop").unwrap() - 1;
            if loop_num <= 0 {
                panic!("Invalid break statement outside of loop")
            }
            istrs.push(Instr::Jmp(format!("endloop{loop_num}")));
            (new_label_env, istrs)
        }

        Expr::Block(exprs) => {
            let mut istrs = Vec::new();
            let mut new_label_env = label_env.clone();
            for e in exprs {
                let (temp_env, mut instr) = compile_to_instrs(&e, si, env, &new_label_env);
                new_label_env = temp_env;
                istrs.append(&mut instr);
            }
            (new_label_env, istrs)
        }

        Expr::Set(s, e) => {
            let mut istrs = Vec::new();
            let (new_label_env, mut istr) = compile_to_instrs(e, si, env, label_env);
            istrs.append(&mut istr);
            if !env.contains_key(s) {
                panic!("{}", format!("Unbound variable identifier {}", s))
            }
            let offset = env.get(s).unwrap() * 8;
            istrs.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, offset),
                Val::Reg(Reg::RAX),
            ));
            (new_label_env, istrs)
        }
    }
}

fn reg_to_str(r: &Reg) -> String {
    match r {
        Reg::RAX => "rax".to_string(),
        Reg::RSP => "rsp".to_string(),
        Reg::RDI => "rdi".to_string(),
        Reg::RBX => "rbx".to_string(),
    }
}
fn val_to_str(v: &Val) -> String {
    match v {
        Val::Imm(imm) => {
            format!("{}", *imm)
        }
        Val::Reg(r) => reg_to_str(r),
        Val::RegOffset(r, imm) => {
            format!("[{} - {}]", reg_to_str(r), *imm)
        }
    }
}

fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(v1, v2) => {
            let val1 = val_to_str(v1);
            let val2 = val_to_str(v2);

            format!("mov {val1}, {val2}")
        }
        Instr::IAdd(v1, v2) => {
            let val1 = val_to_str(v1);
            let val2 = val_to_str(v2);

            format!("add {val1}, {val2}")
        }
        Instr::ISub(v1, v2) => {
            let val1 = val_to_str(v1);
            let val2 = val_to_str(v2);

            format!("sub {val1}, {val2}")
        }
        Instr::IMul(v1, v2) => {
            let val1 = val_to_str(v1);
            let val2 = val_to_str(v2);

            format!("imul {val1}, {val2}")
        }
        Instr::Xor(v1, v2) => {
            let val1 = val_to_str(v1);
            let val2 = val_to_str(v2);

            format!("xor {val1}, {val2}")
        }
        Instr::Test(v1, v2) => {
            let val1 = val_to_str(v1);
            let val2 = val_to_str(v2);

            format!("test {val1}, {val2}")
        }
        Instr::Cmp(v1, v2) => {
            let val1 = val_to_str(v1);
            let val2 = val_to_str(v2);

            format!("cmp {val1}, {val2}")
        }
        Instr::Label(name) => {
            format!("{name}:")
        }
        Instr::JE(label) => {
            format!("je {label}")
        }
        Instr::JNE(label) => {
            format!("jne {label}")
        }
        Instr::JO(label) => {
            format!("jo {label}")
        }
        Instr::Jmp(label) => {
            format!("jmp {label}")
        }
        Instr::Cmove(v1, v2) => {
            let val1 = val_to_str(v1);
            let val2 = val_to_str(v2);
            format!("cmove {val1}, {val2}")
        }
        Instr::Cmovne(v1, v2) => {
            let val1 = val_to_str(v1);
            let val2 = val_to_str(v2);
            format!("cmovne {val1}, {val2}")
        }
        Instr::Cmovl(v1, v2) => {
            let val1 = val_to_str(v1);
            let val2 = val_to_str(v2);
            format!("cmovl {val1}, {val2}")
        }
        Instr::Cmovg(v1, v2) => {
            let val1 = val_to_str(v1);
            let val2 = val_to_str(v2);
            format!("cmovg {val1}, {val2}")
        }
        Instr::Cmovle(v1, v2) => {
            let val1 = val_to_str(v1);
            let val2 = val_to_str(v2);
            format!("cmovle {val1}, {val2}")
        }
        Instr::Cmovge(v1, v2) => {
            let val1 = val_to_str(v1);
            let val2 = val_to_str(v2);
            format!("cmovge {val1}, {val2}")
        }

        Instr::Sar(v1, v2) => {
            let val1 = val_to_str(v1);
            let val2 = val_to_str(v2);
            format!("sar {val1}, {val2}")
        }
    }
}

fn compile(e: &Expr) -> String {
    let instrs = compile_to_instrs(
        e,
        2,
        &hashmap! {"loop".to_string() => 1},
        &hashmap! {"loop".to_string() => 1, "if".to_string() => 1},
    )
    .1;
    let mut output = String::new();
    for i in instrs {
        let instr_string = instr_to_str(&i);
        output = output + &instr_string + "\n";
    }
    output
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    // You will make result hold the result of actually compiling
    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let parsed_contents = match parse(&in_contents) {
        Ok(contents) => contents,
        Err(_) => panic!("parse error: Invalid parentheses"),
    };

    let expr = parse_expr(&parsed_contents);
    let result = compile(&expr);

    let asm_program = format!(
        "
    section .text
    extern snek_error
    global our_code_starts_here
    throw_overflow_error:
        mov rdi, 7
        push rsp
        call snek_error
    throw_type_error:
        mov rdi, 5
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
