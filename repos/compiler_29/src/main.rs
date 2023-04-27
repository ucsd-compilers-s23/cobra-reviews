use std::collections::HashSet;
use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::Atom::*;
use sexp::*;

use im::HashMap;

#[derive(Debug, Clone)]
enum Val {
    Reg(Reg),
    Imm(i64),
    RegOffset(Reg, i32),
    Label(String),
}


#[derive(Debug, Clone)]
enum Reg {
    RAX,
    RSP,
    RDI,
    RBX,
    RCX,
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    ICMove(Val, Val),
    ICMovg(Val, Val),
    ICMovl(Val, Val),
    ICMovge(Val, Val),
    ICMovle(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    ICall(Val),
    IJump(Val),
    IJumpEq(Val),
    IJumpNe(Val),
    IJumpNz(Val),
    IXor(Val, Val),
    ICmp(Val, Val),
    ITest(Val, Val),
    ILabel(Val),
    ISar(Val, Val),
    IJumpOf(Val),
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

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => {
            if(*n < -4611686018427387904 || *n > 4611686018427387903) {
                panic!("Invalid");
            }
            Expr::Number(i64::try_from(*n).unwrap())
        }
        Sexp::Atom(S(s)) if s == "true" => Expr::Boolean(true),
        Sexp::Atom(S(s)) if s == "false" => Expr::Boolean(false),
        Sexp::Atom(S(s)) => Expr::Id(s.to_string()),
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(op)), e] if op == "add1" => Expr::UnOp(Op1::Add1, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "sub1" => Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "isnum" => Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "isbool" => Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e))),
                [Sexp::Atom(S(keyword)), id, e] if keyword == "set!" => Expr::Set(id.to_string(), Box::new(parse_expr(e))),
                [Sexp::Atom(S(keyword)), e] if keyword == "loop" => Expr::Loop(Box::new(parse_expr(e))),
                [Sexp::Atom(S(keyword)), e] if keyword == "break" => Expr::Break(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => {
                    Expr::Block(exprs.into_iter().map(parse_expr).collect())
                }
                [Sexp::Atom(S(keyword)), cond, thn, els] if keyword == "if" => Expr::If(
                    Box::new(parse_expr(cond)),
                    Box::new(parse_expr(thn)),
                    Box::new(parse_expr(els)),
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == "+" => Expr::BinOp(Op2::Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "-" => Expr::BinOp(Op2::Minus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "*" => Expr::BinOp(Op2::Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "=" => Expr::BinOp(Op2::Equal, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "<" => Expr::BinOp(Op2::Less, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == ">" => Expr::BinOp(Op2::Greater, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == ">=" => Expr::BinOp(Op2::GreaterEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "<=" => Expr::BinOp(Op2::LessEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), bindings, body] if op == "let" => {
                    let mut vector: Vec<(String, Expr)> = Vec::new();
                    match bindings {
                        Sexp::List(vec) => {
                            if(vec.len() == 0) {
                                panic!("Invalid")
                            }
                            for sexp in vec {
                                match sexp {
                                    Sexp::List(vect) => {
                                        let slice = &vect[..];
                                        match slice {
                                            [Sexp::Atom(S(id)), e] => {
                                                let mut set:HashSet<String> = HashSet::new();
                                                set.insert("add1".to_string()); set.insert("sub1".to_string()); set.insert("+".to_string());
                                                set.insert("-".to_string()); set.insert("*".to_string()); set.insert("let".to_string());
                                                set.insert("<".to_string()); set.insert("<=".to_string()); set.insert(">".to_string());
                                                set.insert("=".to_string()); set.insert(">=".to_string()); set.insert("isnum".to_string());
                                                set.insert("isbool".to_string()); set.insert("block".to_string()); set.insert("loop".to_string());
                                                set.insert("break".to_string()); set.insert("set!".to_string()); set.insert("input".to_string());
                                                if(set.contains(id)) {
                                                    panic!("keyword usage")
                                                }
                                                vector.push((id.to_string(), parse_expr(e)))
                                            }
                                            _ => panic!("Invalid")
                                        }
                                    }
                                    _ => panic!("Invalid")
                                }
                            }
                            Expr::Let(vector, Box::new(parse_expr(body)))
                        }
                        _ => panic!("Invalid")
                    }
                }
                _ => panic!("Invalid"),
            }
        },
        _ => panic!("Invalid"),
    }
}

fn compile_to_instrs(e: &Expr, si: i32, vec: &mut Vec<Instr>, env: &HashMap<String, i32>, brake: &String, l: &mut i32) {
    match e {
        Expr::Number(n) => vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm((*n) << 1))),
        Expr::Boolean(true) => vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3))),
        Expr::Boolean(false) => vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))),
        Expr::UnOp(op, e1) => {
            compile_to_instrs(e1, si, vec, env, brake, l);
             match op {
                Op1::Add1 => {
                    vec.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(2)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(9)));
                    vec.push(Instr::IJumpOf(Val::Label("throw_error".to_string())));
                }
                Op1::Sub1 => {
                    vec.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(2)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(9)));
                    vec.push(Instr::IJumpOf(Val::Label("throw_error".to_string())));
                }
                Op1::IsNum => {
                    let end_label = new_label(l, "ifend");
                    let else_label = new_label(l, "ifelse");
                    compile_to_instrs(e1, si, vec, env, brake, l);
                    vec.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::IJumpNz(Val::Label(else_label.clone())));
                    vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
                    vec.push(Instr::IJump(Val::Label(end_label.clone())));
                    vec.push(Instr::ILabel(Val::Label(else_label.clone())));
                    vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::ILabel(Val::Label(end_label.clone())));
                }
                Op1::IsBool => {
                    let end_label = new_label(l, "ifend");
                    let else_label = new_label(l, "ifelse");
                    compile_to_instrs(e1, si, vec, env, brake, l);
                    vec.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::IJumpNz(Val::Label(else_label.clone())));
                    vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::IJump(Val::Label(end_label.clone())));
                    vec.push(Instr::ILabel(Val::Label(else_label.clone())));
                    vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
                    vec.push(Instr::ILabel(Val::Label(end_label.clone())));
                }
             }
        }
        Expr::Id(id) => {
            let mut set:HashSet<String> = HashSet::new();
            set.insert("add1".to_string()); set.insert("sub1".to_string()); set.insert("+".to_string());
            set.insert("-".to_string()); set.insert("*".to_string()); set.insert("let".to_string());
            set.insert("<".to_string()); set.insert("<=".to_string()); set.insert(">".to_string());
            set.insert("=".to_string()); set.insert(">=".to_string()); set.insert("isnum".to_string());
            set.insert("isbool".to_string()); set.insert("block".to_string()); set.insert("loop".to_string());
            set.insert("break".to_string()); set.insert("set!".to_string()); 
            if(id == "input") {
                vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI)));
            }
            else if(set.contains(id)) {
                panic!("keyword usage");
            }
            else if(!env.contains_key(id)) {
                panic!("Unbound variable identifier {}", id);
            }
            else {
                vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, *env.get(id).expect("Unbound variable identifier"))))
            } 
        }
        Expr::Let(bindings, body) => {
            let mut num_vars = 0;
            let mut nenv: HashMap<String, i32> = env.clone();
            let mut set: HashSet<String> = HashSet::new();
            for (var, e) in bindings {
                if(set.contains(var)) {
                    panic!("Duplicate binding")
                }
                set.insert(var.to_string());
                compile_to_instrs(e, si+num_vars, vec, &nenv, brake, l);
                vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, ((si+num_vars))*8), Val::Reg(Reg::RAX)));
                nenv = nenv.update(var.to_string(), (si+num_vars)*8);
                num_vars += 1;
            }
            compile_to_instrs(body, si+num_vars, vec, &nenv, brake, l)
        }
        Expr::BinOp(op, e1, e2) => {
            match op {
                Op2::Plus => {
                    let stack_offset = si * 8;
                    compile_to_instrs(e1, si, vec, env, brake, l);
                    vec.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
                    vec.push(Instr::IJumpNz(Val::Label("throw_error".to_string())));
                    vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                    compile_to_instrs(e2, si + 1, vec, env, brake, l);
                    vec.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
                    vec.push(Instr::IJumpNz(Val::Label("throw_error".to_string())));
                    vec.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(9)));
                    vec.push(Instr::IJumpOf(Val::Label("throw_error".to_string())));
                }
                Op2::Minus => {
                    let stack_offset = si * 8;
                    compile_to_instrs(e2, si, vec, env, brake, l);
                    vec.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
                    vec.push(Instr::IJumpNz(Val::Label("throw_error".to_string())));
                    vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                    compile_to_instrs(e1, si + 1, vec, env, brake, l);
                    vec.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
                    vec.push(Instr::IJumpNz(Val::Label("throw_error".to_string())));
                    vec.push(Instr::ISub(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(9)));
                    vec.push(Instr::IJumpOf(Val::Label("throw_error".to_string())));
                }
                Op2::Times => {
                    let stack_offset = si * 8;
                    compile_to_instrs(e1, si, vec, env, brake, l);
                    vec.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
                    vec.push(Instr::IJumpNz(Val::Label("throw_error".to_string())));
                    vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                    vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                    compile_to_instrs(e2, si + 1, vec, env, brake, l);
                    vec.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
                    vec.push(Instr::IJumpNz(Val::Label("throw_error".to_string())));
                    vec.push(Instr::ISar(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(9)));
                    vec.push(Instr::IJumpOf(Val::Label("throw_error".to_string())));
                }
                Op2::Equal => {
                    let stack_offset = si * 8;
                    compile_to_instrs(e1, si, vec, env, brake, l);
                    vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                    compile_to_instrs(e2, si + 1, vec, env, brake, l);
                    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                    vec.push(Instr::IXor(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, stack_offset)));
                    vec.push(Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(7))); //7 here is error code
                    vec.push(Instr::IJumpNe(Val::Label("throw_error".to_string())));
                    vec.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::ICMove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                }
                Op2::Greater => {
                    let stack_offset = si * 8;
                    compile_to_instrs(e1, si, vec, env, brake, l);
                    vec.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
                    vec.push(Instr::IJumpNz(Val::Label("throw_error".to_string())));
                    vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                    compile_to_instrs(e2, si + 1, vec, env, brake, l);
                    vec.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
                    vec.push(Instr::IJumpNz(Val::Label("throw_error".to_string())));
                    vec.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::ICMovl(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                }
                Op2::GreaterEqual => {
                    let stack_offset = si * 8;
                    compile_to_instrs(e1, si, vec, env, brake, l);
                    vec.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
                    vec.push(Instr::IJumpNz(Val::Label("throw_error".to_string())));
                    vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                    compile_to_instrs(e2, si + 1, vec, env, brake, l);
                    vec.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
                    vec.push(Instr::IJumpNz(Val::Label("throw_error".to_string())));
                    vec.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::ICMovle(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                }
                Op2::Less => {
                    let stack_offset = si * 8;
                    compile_to_instrs(e1, si, vec, env, brake, l);
                    vec.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
                    vec.push(Instr::IJumpNz(Val::Label("throw_error".to_string())));
                    vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                    compile_to_instrs(e2, si + 1, vec, env, brake, l);
                    vec.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
                    vec.push(Instr::IJumpNz(Val::Label("throw_error".to_string())));
                    vec.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::ICMovg(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                }
                Op2::LessEqual => {
                    let stack_offset = si * 8;
                    compile_to_instrs(e1, si, vec, env, brake, l);
                    vec.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
                    vec.push(Instr::IJumpNz(Val::Label("throw_error".to_string())));
                    vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                    compile_to_instrs(e2, si + 1, vec, env, brake, l);
                    vec.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
                    vec.push(Instr::IJumpNz(Val::Label("throw_error".to_string())));
                    vec.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::ICMovge(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                }
            }
        }
        Expr::If(cond, thn, els) => {
            let end_label = new_label(l, "ifend");
            let else_label = new_label(l, "ifelse");
            compile_to_instrs(cond, si, vec, env, brake, l);
            vec.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(1)));
            vec.push(Instr::IJumpEq(Val::Label(else_label.clone())));
            compile_to_instrs(thn, si, vec, env, brake, l);
            vec.push(Instr::IJump(Val::Label(end_label.clone())));
            vec.push(Instr::ILabel(Val::Label(else_label)));
            compile_to_instrs(els, si, vec, env, brake, l);
            vec.push(Instr::ILabel(Val::Label(end_label)));
        }
        Expr::Loop(e1) => {
            let startloop = new_label(l, "loop");
            let endloop = new_label(l, "endloop");
            vec.push(Instr::ILabel(Val::Label(startloop.clone())));
            compile_to_instrs(e1, si, vec, env, &endloop, l);
            vec.push(Instr::IJump(Val::Label(startloop)));
            vec.push(Instr::ILabel(Val::Label(endloop)));
        }
        Expr::Break(e1) => {
            if(brake == "") {
                panic!("break not within loop");
            }
            compile_to_instrs(e1, si, vec, env, brake, l);
            vec.push(Instr::IJump(Val::Label(brake.to_string())))
        }
        Expr::Set(id, e1) => {
            if(!env.contains_key(id)) {
                panic!("Unbound variable identifier {}", id);
            }
            compile_to_instrs(e1, si, vec, &env, brake, l);
            vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, *env.get(id).unwrap()), Val::Reg(Reg::RAX)));
        }
        Expr::Block(es) => {
            if(es.len() == 0) {
                panic!("Invalid")
            }
            for expr in es {
                compile_to_instrs(expr, si, vec, env, brake, l)
            }
        }
    }
}

fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(dst, src) => {
            let dst_str = val_to_str(dst);
            let src_str = val_to_str(src);
            format!("\nmov {dst_str}, {src_str}")
        }
        Instr::IAdd(a, b) => {
            let a_str = val_to_str(a);
            let b_str = val_to_str(b);
            format!("\nadd {a_str}, {b_str}")
        }
        Instr::ISub(a, b) => {
            let a_str = val_to_str(a);
            let b_str = val_to_str(b);
            format!("\nsub {a_str}, {b_str}")
        }
        Instr::IMul(a, b) => {
            let a_str = val_to_str(a);
            let b_str = val_to_str(b);
            format!("\nimul {a_str}, {b_str}")
        }
        Instr::ICall(l) => {
            let label = val_to_str(l);
            format!("\ncall {label}")
        }
        Instr::IJump(l) => {
            let label = val_to_str(l);
            format!("\njmp {label}")
        }
        Instr::IJumpEq(l) => {
            let label = val_to_str(l);
            format!("\nje {label}")
        }
        Instr::ICMove(dst, src) => {
            let dst_str = val_to_str(dst);
            let src_str = val_to_str(src);
            format!("\ncmove {dst_str}, {src_str}")
        }
        Instr::IJumpNe(l) => {
            let label = val_to_str(l);
            format!("\njne {label}")
        }
        Instr::IJumpNz(l) => {
            let label = val_to_str(l);
            format!("\njnz {label}")
        }
        Instr::IXor(a, b) => {
            let a_str = val_to_str(a);
            let b_str = val_to_str(b);
            format!("\nxor {a_str}, {b_str}")
        }
        Instr::ICmp(a, b) => {
            let a_str = val_to_str(a);
            let b_str = val_to_str(b);
            format!("\ncmp {a_str}, {b_str}")
        }
        Instr::ITest(a, b) => {
            let a_str = val_to_str(a);
            let b_str = val_to_str(b);
            format!("\ntest {a_str}, {b_str}")
        }
        Instr::ILabel(l) => {
            match l {
                Val::Label(s) => format!("\n{}:", s),
                _ => panic!("Not a string (label)")
            }
            
        }
        Instr::ICMovg(dst, src) => {
            let dst_str = val_to_str(dst);
            let src_str = val_to_str(src);
            format!("\ncmovg {dst_str}, {src_str}")
        }
        Instr::ICMovge(dst, src) => {
            let dst_str = val_to_str(dst);
            let src_str = val_to_str(src);
            format!("\ncmovge {dst_str}, {src_str}")
        }
        Instr::ICMovl(dst, src) => {
            let dst_str = val_to_str(dst);
            let src_str = val_to_str(src);
            format!("\ncmovl {dst_str}, {src_str}")
        }
        Instr::ICMovle(dst, src) => {
            let dst_str = val_to_str(dst);
            let src_str = val_to_str(src);
            format!("\ncmovle {dst_str}, {src_str}")
        }
        Instr::ISar(dst, cnt) => {
            let dst_str = val_to_str(dst);
            let cnt_str = val_to_str(cnt);
            format!("\nsar {dst_str}, {cnt_str}")
        }
        Instr::IJumpOf(l) => {
            let label = val_to_str(l);
            format!("\njo {label}")
        }
    }
}

fn val_to_str(v: &Val) -> String {
    match v {
        Val::Imm(n) => format!("{}", *n),
        Val::Reg(reg) => {
            match reg {
                Reg::RAX => format!("rax"),
                Reg::RSP => format!("rsp"),
                Reg::RDI => format!("rdi"),
                Reg::RBX => format!("rbx"),
                Reg::RCX => format!("rcx"),
            }
        }
        Val::RegOffset(reg, offset) => {
            match reg {
                Reg::RAX => format!("[rax - {}]", *offset),
                Reg::RSP => format!("[rsp - {}]", *offset),
                Reg::RDI => format!("[rdi - {}]", *offset),
                Reg::RBX => format!("[rbx - {}]", *offset),
                Reg::RCX => format!("[rcx - {}]", *offset),
            }
        }
        Val::Label(s) => format!("{}", s),
    }
}

fn compile(e: &Expr, si: i32, vec: &mut Vec<Instr>, env: &HashMap<String, i32>, brake: &String, l: &mut i32) -> String {
    let mut assembly = String::new();
    compile_to_instrs(e, si, vec, env, brake, l);
    for instr in vec {
        assembly.push_str(&instr_to_str(instr));
    }
    return assembly;
}

fn new_label(l: &mut i32, s: &str) -> String {
    let current = *l;
    *l += 1;
    format!("{s}_{current}")
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let mut vec: Vec<Instr> = Vec::new();
    let map: HashMap<String, i32> = HashMap::new();
    let mut labels = 0;

    // You will make result hold the result of actually compiling
    let expr = parse_expr(&parse(&in_contents).expect("Invalid"));
    let result = compile(&expr, 2, &mut vec, &map, &String::from(""), &mut labels);

    let asm_program = format!(
        "
section .text
global our_code_starts_here
extern snek_error
extern snek_print
throw_error:
  push rsp
  mov rdi, rcx
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