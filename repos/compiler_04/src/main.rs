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
    Cmp(Val, Val),
    Cmove(Val, Val),
    Cmovg(Val, Val),
    Cmovge(Val, Val),
    Cmovl(Val, Val),
    Cmovle(Val, Val),
    And(Val, Val),
    Xor(Val, Val),
    Test(Val, Val),
    Label(String),
    Jmp(String),
    Jne(String),
    Je(String),
    Sar(Val, Val),
    Jo(String),
    Or(Val, Val),
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
    let keywords = [
        "true".to_string(), 
        "false".to_string(), 
        "let".to_string(), 
        "add1".to_string(), 
        "sub1".to_string(), 
        "isnum".to_string(), 
        "isbool".to_string(), 
        "if".to_string(), 
        "loop".to_string(), 
        "break".to_string(), 
        "set".to_string(), 
        "block".to_string(), 
        "input".to_string()];
    match s {
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(id_str)), e] => {
                    if keywords.contains(id_str) {
                        panic!("Invalid keyword")
                    }
                    (id_str.to_string(), parse_expr(e))
                },
                _ => panic!("Invalid"),
            }
        },
        _ => panic!("Invalid"),
    }
}

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => Expr::Number(i64::try_from(*n).unwrap()),
        Sexp::Atom(S(id)) => {
            match id.as_str() {
                "true" => Expr::Boolean(true),
                "false" => Expr::Boolean(false),
                _ => Expr::Id(id.to_string()),
            }
        }
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
                [Sexp::Atom(S(op)), e1, e2] if op == ">" => Expr::BinOp(Op2::Greater, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == ">=" => Expr::BinOp(Op2::GreaterEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "<" => Expr::BinOp(Op2::Less, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "<=" => Expr::BinOp(Op2::LessEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), b, e] if op == "let" => {
                    match b {
                        Sexp::List(bvec) => {
                            if bvec.len() == 0 {
                                panic!("Invalid syntax");
                            }
                            let bindings = bvec.iter().map(|bsexp| parse_bind(bsexp)).collect::<Vec<(String, Expr)>>();
                            Expr::Let(bindings, Box::new(parse_expr(e)))
                        },
                        _ => panic!("Invalid syntax"),
                    }
                },
                [Sexp::Atom(S(op)), b, e1, e2] if op == "if" => {
                    Expr::If(Box::new(parse_expr(b)), Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
                },
                [Sexp::Atom(S(op)), e] if op == "loop" => {
                    Expr::Loop(Box::new(parse_expr(e)))
                },
                [Sexp::Atom(S(op)), e] if op == "break" => {
                    Expr::Break(Box::new(parse_expr(e)))
                },
                [Sexp::Atom(S(op)), Sexp::Atom(S(id)), e] if op == "set!" => {
                    Expr::Set(id.to_string(), Box::new(parse_expr(e)))
                },
                [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => {
                    if exprs.len() == 0 {
                        panic!("Invalid syntax");
                    }
                    Expr::Block(exprs.into_iter().map(parse_expr).collect())
                }
                _ => panic!("Invalid syntax"),
            }
        },
        _ => panic!("Invalid syntax"),
    }
}

fn new_label(l: &mut i32, s: &str) -> String {
    let current = *l;
    *l += 1;
    format!("{}_{}", s, current)
}

fn compile_to_instrs(e: &Expr, si:i32, env: &HashMap<String, i32>, l: &mut i32, b_target: &String) -> Vec<Instr> {
    match e {
        Expr::Number(n) => {
            if *n > (i64::MAX >> 1) || *n < (i64::MIN >> 1){
                panic!("Invalid");
            }
            // shift number by 1 bit to the left.
            vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n<<1))]
        },
        Expr::Boolean(b) => {
            if *b {
                vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3))]
            } else {
                vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))]
            }
        },
        Expr::UnOp(op1, subexpr) => {
            let mut expr = compile_to_instrs(subexpr, si, &env, l, b_target);
            match op1 {
                Op1::Add1 => {
                    // 2 = 00010 = 1 in cobra.
                    expr.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                    expr.push(Instr::Test(Val::Reg(Reg::RBX), Val::Imm(1)));
                    expr.push(Instr::Jne("throw_error".to_string()));
                    expr.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(2)));
                    expr.push(Instr::Jo("overflow_error".to_string()));
                    expr
                },
                Op1::Sub1 => {
                    expr.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                    expr.push(Instr::Test(Val::Reg(Reg::RBX), Val::Imm(1)));
                    expr.push(Instr::Jne("throw_error".to_string()));
                    expr.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(2)));
                    expr.push(Instr::Jo("overflow_error".to_string()));
                    expr
                },
                Op1::IsNum => {
                    expr.push(Instr::And(Val::Reg(Reg::RAX), Val::Imm(1))); // bitwise and with 00001
                    expr.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::Imm(0))); // if 0, it's a number
                    expr.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3))); // true
                    expr.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))); // false
                    expr.push(Instr::Cmove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                    expr.push(Instr::Jo("overflow_error".to_string()));
                    expr
                },
                Op1::IsBool => {
                    expr.push(Instr::And(Val::Reg(Reg::RAX), Val::Imm(1))); // bitwise and with 00001
                    expr.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::Imm(1))); // if 1, it's a boolean
                    expr.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3))); // true
                    expr.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))); // false
                    expr.push(Instr::Cmove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                    expr.push(Instr::Jo("overflow_error".to_string()));
                    expr
                }
            }
        },
        Expr::BinOp(op2, subexpr1, subexpr2) => {
            let offset = si * 8;
            match op2 {
                Op2::Plus => {
                    let expr1 = compile_to_instrs(subexpr1, si, &env, l, b_target);
                    let expr2 = compile_to_instrs(subexpr2, si+1, &env, l, b_target);
                    vec![
                        expr1,
                        vec![Instr::IMov(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX))],
                        expr2,
                        vec![Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX))],
                        vec![Instr::Or(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, offset))],
                        vec![Instr::Test(Val::Reg(Reg::RBX), Val::Imm(1))],
                        vec![Instr::Jne("throw_error".to_string())],
                        vec![Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset))],
                        vec![Instr::Jo("overflow_error".to_string())],
                    ].into_iter().flatten().collect()
                },
                Op2::Minus => {
                    let expr1 = compile_to_instrs(subexpr1, si+1, &env, l, b_target);
                    let expr2 = compile_to_instrs(subexpr2, si, &env, l, b_target);
                    vec![
                        expr2,
                        vec![Instr::IMov(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX))],
                        expr1,
                        vec![Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX))],
                        vec![Instr::Or(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, offset))],
                        vec![Instr::Test(Val::Reg(Reg::RBX), Val::Imm(1))],
                        vec![Instr::Jne("throw_error".to_string())],
                        vec![Instr::ISub(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset))],
                        vec![Instr::Jo("overflow_error".to_string())]
                    ].into_iter().flatten().collect()
                },
                Op2::Times => {
                    let expr1 = compile_to_instrs(subexpr1, si, &env, l, b_target);
                    let expr2 = compile_to_instrs(subexpr2, si+1, &env, l, b_target);
                    vec![
                        expr1,
                        vec![Instr::IMov(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX))],
                        expr2,
                        vec![Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX))],
                        vec![Instr::Or(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, offset))],
                        vec![Instr::Test(Val::Reg(Reg::RBX), Val::Imm(1))],
                        vec![Instr::Jne("throw_error".to_string())],
                        vec![Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset))],
                        vec![Instr::Jo("overflow_error".to_string())],
                        vec![Instr::Sar(Val::Reg(Reg::RAX), Val::Imm(1))]
                    ].into_iter().flatten().collect()
                },
                Op2::Equal => {
                    let expr1 = compile_to_instrs(subexpr1, si, &env, l, b_target);
                    let expr2 = compile_to_instrs(subexpr2, si+1, &env, l, b_target);
                    vec![
                        expr1,
                        vec![Instr::IMov(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX))],
                        expr2,
                        vec![Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX))],
                        vec![Instr::Xor(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, offset))],
                        vec![Instr::Test(Val::Reg(Reg::RBX), Val::Imm(1))],
                        vec![Instr::Jne("throw_error".to_string())],
                        vec![Instr::Cmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset))],
                        vec![Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3))],
                        vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))],
                        vec![Instr::Cmove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))]
                    ].into_iter().flatten().collect()
                },
                Op2::Greater => {
                    // invert the order as we want the first expr to be in RAX.
                    let expr1 = compile_to_instrs(subexpr1, si+1, &env, l, b_target);
                    let expr2 = compile_to_instrs(subexpr2, si, &env, l, b_target);
                    vec![
                        expr2,
                        vec![Instr::IMov(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX))],
                        expr1,
                        vec![Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX))],
                        vec![Instr::Xor(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, offset))],
                        vec![Instr::Test(Val::Reg(Reg::RBX), Val::Imm(1))],
                        vec![Instr::Jne("throw_error".to_string())],
                        vec![Instr::Cmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset))],
                        vec![Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3))],
                        vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))],
                        vec![Instr::Cmovg(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))]
                    ].into_iter().flatten().collect()
                },
                Op2::GreaterEqual => {
                    // invert the order as we want the first expr to be in RAX.
                    let expr1 = compile_to_instrs(subexpr1, si+1, &env, l, b_target);
                    let expr2 = compile_to_instrs(subexpr2, si, &env, l, b_target);
                    vec![
                        expr2,
                        vec![Instr::IMov(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX))],
                        expr1,
                        vec![Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX))],
                        vec![Instr::Xor(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, offset))],
                        vec![Instr::Test(Val::Reg(Reg::RBX), Val::Imm(1))],
                        vec![Instr::Jne("throw_error".to_string())],
                        vec![Instr::Cmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset))],
                        vec![Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3))],
                        vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))],
                        vec![Instr::Cmovge(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))]
                    ].into_iter().flatten().collect()
                },
                Op2::Less => {
                    // invert the order as we want the first expr to be in RAX.
                    let expr1 = compile_to_instrs(subexpr1, si+1, &env, l, b_target);
                    let expr2 = compile_to_instrs(subexpr2, si, &env, l, b_target);
                    vec![
                        expr2,
                        vec![Instr::IMov(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX))],
                        expr1,
                        vec![Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX))],
                        vec![Instr::Xor(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, offset))],
                        vec![Instr::Test(Val::Reg(Reg::RBX), Val::Imm(1))],
                        vec![Instr::Jne("throw_error".to_string())],
                        vec![Instr::Cmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset))],
                        vec![Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3))],
                        vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))],
                        vec![Instr::Cmovl(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))]
                    ].into_iter().flatten().collect()
                },
                Op2::LessEqual => {
                    // invert the order as we want the first expr to be in RAX.
                    let expr1 = compile_to_instrs(subexpr1, si+1, &env, l, b_target);
                    let expr2 = compile_to_instrs(subexpr2, si, &env, l, b_target);
                    vec![
                        expr2,
                        vec![Instr::IMov(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX))],
                        expr1,
                        vec![Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX))],
                        vec![Instr::Xor(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, offset))],
                        vec![Instr::Test(Val::Reg(Reg::RBX), Val::Imm(1))],
                        vec![Instr::Jne("throw_error".to_string())],
                        vec![Instr::Cmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset))],
                        vec![Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3))],
                        vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))],
                        vec![Instr::Cmovle(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))]
                    ].into_iter().flatten().collect()
                },
            }
        },
        Expr::Let(bindings, subexpr) => {

            let mut bin_instrs: Vec<Instr> = Vec::new();
            let mut env_new = env.clone();
            let mut si_new = si.clone();

            let mut env_dupcheck: HashMap<String, i32> = HashMap::new();
            for (id, _bexpr) in bindings {
                // variable already exists
                if env_dupcheck.contains_key(&id.to_string()) {
                    panic!("Duplicate binding");
                }
                env_dupcheck = env_dupcheck.update(id.to_string(), 0);
            }

            for (id, bexpr) in bindings {
                let mut binstr = compile_to_instrs(bexpr, si_new, &env_new, l, b_target);
                let offset = si_new * 8;
                si_new = si_new + 1;

                env_new = env_new.update(id.to_string(), offset);

                bin_instrs.append(&mut binstr);
                bin_instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX)));
            }

            let expr = compile_to_instrs(subexpr, si_new, &env_new, l, b_target);
            vec![
                bin_instrs,
                expr
            ].into_iter().flatten().collect()
        },
        Expr::Id(s) => {
            if s == "input" {
                vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI))]
            } else {
                let offset = env.get(s);
                if offset.is_none() {
                    panic!("Unbound variable identifier {}", s);
                }
                vec![Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, *offset.unwrap()))]
            }
        },
        Expr::If(cond, subexpr1, subexpr2) => {
            let end_label = new_label(l, "ifend");
            let else_label = new_label(l, "ifelse");
            let cond_instrs = compile_to_instrs(cond, si, &env, l, b_target);
            let thn_instrs = compile_to_instrs(subexpr1, si, &env, l, b_target);
            let els_instrs = compile_to_instrs(subexpr2, si, &env, l, b_target);
            vec![
                cond_instrs,
                vec![Instr::Cmp(Val::Reg(Reg::RAX), Val::Imm(1))],
                vec![Instr::Je(else_label.clone())],
                thn_instrs,
                vec![Instr::Jmp(end_label.clone())],
                vec![Instr::Label(else_label)],
                els_instrs,
                vec![Instr::Label(end_label)]
            ].into_iter().flatten().collect()
        },
        Expr::Loop(subexpr) => {
            let start_label = new_label(l, "loopstart");
            let end_label = new_label(l, "loopend");
            let body_instrs = compile_to_instrs(subexpr, si, &env, l, &end_label);
            vec![
                vec![Instr::Label(start_label.clone())],
                body_instrs,
                vec![Instr::Jmp(start_label.clone())],
                vec![Instr::Label(end_label)]
            ].into_iter().flatten().collect()
        },
        Expr::Break(expr) => {
            if b_target == "" {
                panic!("break outside of loop");
            }
            let instrs = compile_to_instrs(expr, si, &env, l, b_target);
            vec![
                instrs,
                vec![Instr::Jmp(b_target.clone())]
            ].into_iter().flatten().collect()
        },
        Expr::Set(s, subexpr) => {
            let offset = env.get(s);
            if offset.is_none() {
                panic!("Unbound variable identifier {}", s);
            }
            let instrs = compile_to_instrs(subexpr, si, &env, l, b_target);
            vec![
                instrs,
                vec![Instr::IMov(Val::RegOffset(Reg::RSP, *offset.unwrap()), Val::Reg(Reg::RAX))]
            ].into_iter().flatten().collect()
        },
        Expr::Block(exprs) => {
            let mut instrs: Vec<Instr> = Vec::new();
            for expr in exprs {
                let mut binstr = compile_to_instrs(expr, si, &env, l, b_target);
                instrs.append(&mut binstr);
            }
            instrs
        },
    }
}

fn reg_to_str(r: &Reg) -> String {
    match r {
        Reg::RAX => format!("rax"),
        Reg::RSP => format!("rsp"),
        Reg::RBX => format!("rbx"),
        Reg::RDI => format!("rdi"),
    }
}

fn val_to_str(v: &Val) -> String {
    match v {
        Val::Reg(reg) => reg_to_str(reg),
        Val::Imm(n) => format!("{}", *n),
        Val::RegOffset(reg, offset) => {
            let register = reg_to_str(reg);
            format!("[{register} - {offset}]")
        }
    }
}

fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(v1, v2) => {
            let str_v1 = val_to_str(v1);
            let str_v2 = val_to_str(v2);
            format!("mov {str_v1}, {str_v2}")
        },
        Instr::IAdd(v1, v2) => {
            let str_v1 = val_to_str(v1);
            let str_v2 = val_to_str(v2);
            format!("add {str_v1}, {str_v2}")
        },
        Instr::ISub(v1, v2) => {
            let str_v1 = val_to_str(v1);
            let str_v2 = val_to_str(v2);
            format!("sub {str_v1}, {str_v2}")
        },
        Instr::IMul(v1, v2) => {
            let str_v1 = val_to_str(v1);
            let str_v2 = val_to_str(v2);
            format!("imul {str_v1}, {str_v2}")
        },
        Instr::Cmp(v1, v2) => {
            let str_v1 = val_to_str(v1);
            let str_v2 = val_to_str(v2);
            format!("cmp {str_v1}, {str_v2}")
        },
        Instr::Cmove(v1, v2) => {
            let str_v1 = val_to_str(v1);
            let str_v2 = val_to_str(v2);
            format!("cmove {str_v1}, {str_v2}")
        },
        Instr::Cmovg(v1, v2) => {
            let str_v1 = val_to_str(v1);
            let str_v2 = val_to_str(v2);
            format!("cmovg {str_v1}, {str_v2}")
        },
        Instr::Cmovge(v1, v2) => {
            let str_v1 = val_to_str(v1);
            let str_v2 = val_to_str(v2);
            format!("cmovge {str_v1}, {str_v2}")
        },
        Instr::Cmovl(v1, v2) => {
            let str_v1 = val_to_str(v1);
            let str_v2 = val_to_str(v2);
            format!("cmovl {str_v1}, {str_v2}")
        },
        Instr::Cmovle(v1, v2) => {
            let str_v1 = val_to_str(v1);
            let str_v2 = val_to_str(v2);
            format!("cmovle {str_v1}, {str_v2}")
        },
        Instr::And(v1, v2) => {
            let str_v1 = val_to_str(v1);
            let str_v2 = val_to_str(v2);
            format!("and {str_v1}, {str_v2}")
        },
        Instr::Xor(v1, v2) => {
            let str_v1 = val_to_str(v1);
            let str_v2 = val_to_str(v2);
            format!("xor {str_v1}, {str_v2}")
        },
        Instr::Test(v1, v2) => {
            let str_v1 = val_to_str(v1);
            let str_v2 = val_to_str(v2);
            format!("test {str_v1}, {str_v2}")
        },
        Instr::Label(label) => {
            format!("{label}:")
        },
        Instr::Jmp(label) => {
            format!("jmp {label}")
        },
        Instr::Jne(label) => {
            format!("jne {label}")
        },
        Instr::Je(label) => {
            format!("je {label}")
        },
        Instr::Jo(label) => {
            format!("jo {label}")
        },
        Instr::Sar(v1, v2) => {
            let str_v1 = val_to_str(v1);
            let str_v2 = val_to_str(v2);
            format!("sar {str_v1}, {str_v2}")
        },
        Instr::Or(v1, v2) => {
            let str_v1 = val_to_str(v1);
            let str_v2 = val_to_str(v2);
            format!("or {str_v1}, {str_v2}")
        },
    }
}

fn compile(e: &Expr) -> String {
    let env: HashMap<String, i32> = HashMap::new();
    // b_target will not be used.
    let instrs = compile_to_instrs(e, 2, &env, &mut 0, &"".to_string());
    let instr_strs = instrs.iter().map(|instr| instr_to_str(instr)).collect::<Vec<String>>();
    instr_strs.join("\n")
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    // You will make result hold the result of actually compiling
    //let result = "mov rax, 131";
    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();

    in_file.read_to_string(&mut in_contents).unwrap();

    let parsed = match parse(&in_contents) {
        Ok(p) => {p},
        Err(_error) => panic!("Invalid syntax"),
    };

    let expr = parse_expr(&parsed);
    let result = compile(&expr);    

    let asm_program = format!(
        "
section .text
global our_code_starts_here
extern snek_error
throw_error:
  mov rdi, 7
  push rsp
  call snek_error
overflow_error:
  mov rdi, 6
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
