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
    Label(String),
}

#[derive(Debug)]
enum Reg {
    RAX,
    RSP,
    RDI,
    RBX,
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),

    AND(Val, Val),
    CMP(Val, Val),
    JE(Val),
    Lab(Val),
    JMP(Val),
    JNE(Val),
    JNZ(Val),
    JO(Val),

    XOR(Val, Val),
    TEST(Val, Val),
    CMOVE(Val, Val),
    CMOVG(Val, Val),
    CMOVGE(Val, Val),
    CMOVL(Val, Val),
    CMOVLE(Val, Val),

    SAR (Val, Val),

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

fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(src, dst) => format!("mov {}, {}\n", val_to_str(src), val_to_str(dst)),
        Instr::IAdd(src, dst) => format!("add {}, {}\n", val_to_str(src), val_to_str(dst)),
        Instr::ISub(src, dst) => format!("sub {}, {}\n", val_to_str(src), val_to_str(dst)),
        Instr::IMul(src, dst) => format!("imul {}, {}\n", val_to_str(src), val_to_str(dst)),
        Instr::AND(src, dst ) => format!("and {}, {}\n",  val_to_str(src), val_to_str(dst)),
        Instr::CMP(src, dst ) => format!("cmp {}, {}\n",  val_to_str(src), val_to_str(dst)),
        Instr::JE(l) => format!("je {}\n", val_to_str(l)),
        Instr::JMP(l) => format!("jmp {}\n", val_to_str(l)),
        Instr::JNE(l) => format!("jne {}\n", val_to_str(l)),
        Instr::JNZ(l) => format!("jnz {}\n", val_to_str(l)),
        Instr::JO(l) => format!("jo {}\n", val_to_str(l)),
        Instr::Lab(l) => format!("{}:\n", val_to_str(l)),
        Instr::XOR(src, dst) => format!("xor {}, {}\n",  val_to_str(src), val_to_str(dst)),
        Instr::TEST(src, dst) => format!("test {}, {}\n",  val_to_str(src), val_to_str(dst)),
        Instr::CMOVE(src, dst) => format!("cmove {}, {}\n",  val_to_str(src), val_to_str(dst)),
        Instr::CMOVG(src, dst) => format!("cmovg {}, {}\n",  val_to_str(src), val_to_str(dst)),
        Instr::CMOVGE(src, dst) => format!("cmovge {}, {}\n",  val_to_str(src), val_to_str(dst)),
        Instr::CMOVL(src, dst) => format!("cmovl {}, {}\n",  val_to_str(src), val_to_str(dst)),
        Instr::CMOVLE(src, dst) => format!("cmovle {}, {}\n",  val_to_str(src), val_to_str(dst)),
        Instr::SAR (src, cnt) => format!("sar {}, {}\n",  val_to_str(src), val_to_str(cnt)),
    }
}

fn val_to_str(v: &Val) -> String {
    match v {
        Val::Reg(reg) => match reg {
            Reg::RAX => "rax".to_string(),
            Reg::RSP => "rsp".to_string(),
            Reg::RDI => "rdi".to_string(),
            Reg::RBX => "rbx".to_string(),
        },
        Val::Imm(val) => format!("{}", val),
        Val::Label(s) => format!("{}", s),
        Val::RegOffset(reg, offset) => {
            match reg {
                Reg::RAX =>  format!("[{} - {}]", "rax".to_string(), offset),
                Reg::RSP =>  format!("[{} - {}]", "rsp".to_string(), offset),
                Reg::RDI =>  format!("[{} - {}]", "rdi".to_string(), offset),
                Reg::RBX =>  format!("[{} - {}]", "rbx".to_string(), offset),
            }
        }
    }
}

fn new_label(l: &mut i32, s: &str) -> String {
    let current = *l;
    *l += 1;
    format!("{s}_{current}")
}

fn compile_to_instrs(e: &Expr, si: i32, env: &HashMap<String, i32>, brake: &String, l: &mut i32, instrs: &mut Vec<Instr>) {
    match e {
        Expr::Number(n) => {
            if *n > 4611686018427387903 || *n < -4611686018427387904{
                panic!("Invalid")
            }
            instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n << 1)))
        }
        Expr::Id(s) if s == "input" =>{
            instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI)))
        }
        Expr::Id(s) => {
            if ! env.contains_key(s){
                panic!("Unbound variable identifier {s}");
            }
            let offset = env.get(s).unwrap() * 8;
            instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset)))
        }

        Expr::Boolean(bool) =>{
            if *bool == true{
                instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)))
            }
            if *bool == false{
                instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)))
            }
        }

        Expr::UnOp(op, e1) => {
            compile_to_instrs(e1, si, env, brake, l, instrs);
            match op {
                Op1::Add1 => {
                    instrs.push(Instr::TEST(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RDI)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(3)));
                    instrs.push(Instr::JNZ(Val::Label("throw_error".to_string())));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX)));

                    instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1 << 1)));

                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RDI)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(6)));
                    instrs.push(Instr::JO(Val::Label("throw_error".to_string())));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX)));
                }
                Op1::Sub1 => {
                    instrs.push(Instr::TEST(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RDI)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(3)));
                    instrs.push(Instr::JNZ(Val::Label("throw_error".to_string())));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX)));

                    instrs.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(1 << 1)));

                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RDI)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(6)));
                    instrs.push(Instr::JO(Val::Label("throw_error".to_string())));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX)));
                }
                Op1::IsNum => {
                    let not_num = new_label(l, "notNum");
                    let end = new_label(l, "end");
                    instrs.push(Instr::AND(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::CMP(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::JE(Val::Label(not_num.clone())));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
                    instrs.push(Instr::JMP(Val::Label(end.clone())));
                    instrs.push(Instr::Lab(Val::Label(not_num.clone())));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::Lab(Val::Label(end.clone())));
                }
                Op1::IsBool => {
                    let not_bool = new_label(l, "notBool");
                    let end = new_label(l, "end");
                    instrs.push(Instr::AND(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::CMP(Val::Reg(Reg::RAX), Val::Imm(0)));
                    instrs.push(Instr::JE(Val::Label(not_bool.clone())));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
                    instrs.push(Instr::JMP(Val::Label(end.clone())));
                    instrs.push(Instr::Lab(Val::Label(not_bool.clone())));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::Lab(Val::Label(end.clone())));
                }
            }
        }
        Expr::BinOp(op, e1, e2) => {
            compile_to_instrs(e1, si, env, brake, l, instrs);
            match op{
                Op2::Plus | Op2::Minus | Op2::Times | Op2::Greater | Op2::GreaterEqual | Op2::Less | Op2::LessEqual=>{
                    instrs.push(Instr::TEST(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RDI)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(3)));
                    instrs.push(Instr::JNZ(Val::Label("throw_error".to_string())));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX)));
              }
              _ => {}
            }
            let stack_offset = si * 8;
            instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
            compile_to_instrs(e2, si + 1, env, brake, l, instrs);
            match op{
                Op2::Plus | Op2::Minus | Op2::Times | Op2::Greater | Op2::GreaterEqual | Op2::Less | Op2::LessEqual=>{
                    instrs.push(Instr::TEST(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RDI)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(3)));
                    instrs.push(Instr::JNZ(Val::Label("throw_error".to_string())));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX)));
                }
              _ => {}
            }

            match op {
                Op2::Plus => {
                    instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));

                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RDI)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(6)));
                    instrs.push(Instr::JO(Val::Label("throw_error".to_string())));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX)));
                }
                Op2::Minus => {
                    instrs.push(Instr::ISub(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));

                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RDI)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(6)));
                    instrs.push(Instr::JO(Val::Label("throw_error".to_string())));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX)));
                }
                Op2::Times =>{
                    instrs.push(Instr::SAR(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));

                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RDI)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(6)));
                    instrs.push(Instr::JO(Val::Label("throw_error".to_string())));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX)));
                }
                Op2::Equal =>{
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::XOR(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, stack_offset)));
                    instrs.push(Instr::TEST(Val::Reg(Reg::RBX), Val::Imm(1)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RDI)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(7)));
                    instrs.push(Instr::JNE(Val::Label("throw_error".to_string())));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX)));
                    
                    instrs.push(Instr::CMP(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::CMOVE(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                }
                Op2::Greater =>{
                    instrs.push(Instr::CMP(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::CMOVG(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                }
                Op2::GreaterEqual => {
                    instrs.push(Instr::CMP(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::CMOVGE(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                }
                Op2::Less => {
                    instrs.push(Instr::CMP(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::CMOVL(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                }
                Op2::LessEqual =>{
                    instrs.push(Instr::CMP(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::CMOVLE(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                }
            }
        }

        // the let maching is generated using Chatgpt
        Expr::Let(bindings, body) => {
            let keywords = vec!["input".to_string(), "let".to_string(), "true".to_string(), "false".to_string(), "block".to_string(), "loop".to_string(), "break".to_string(), "if".to_string(), "set!".to_string(), "add1".to_string(), "sub1".to_string(), "isnum".to_string(), "isbool".to_string(), "+".to_string(), "-".to_string(), "*".to_string(), "<".to_string(), ">".to_string(), ">=".to_string(), "<=".to_string(), "=".to_string()];
            let mut new_env = env.clone();
            let mut new_si = si;
            let mut dup = HashMap::new();

            for (name, expr) in bindings {
                if keywords.contains(name){
                    panic!("keyword");
                }
                if dup.contains_key(name) {
                    panic!("Duplicate binding");
                }
                compile_to_instrs(expr, new_si, &new_env, brake, l, instrs);
                instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, new_si * 8), Val::Reg(Reg::RAX)));
                new_env.insert(name.clone(), new_si);
                dup.insert(name.clone(), new_si);
                new_si += 1;
            }
            compile_to_instrs(body, new_si, &new_env, brake, l, instrs);
        }

        Expr::If(cond, thn, els) =>{
            let end_label = new_label(l, "ifend");
            let else_label = new_label(l, "ifelse");
            // cond_instrs
            compile_to_instrs(cond, si, env, brake, l, instrs);
            instrs.push(Instr::CMP(Val::Reg(Reg::RAX), Val::Imm(1)));
            instrs.push(Instr::JE(Val::Label(else_label.clone())));
            // thn_instrs 
            compile_to_instrs(thn, si, env, brake, l, instrs);
            instrs.push(Instr::JMP(Val::Label(end_label.clone())));
            instrs.push(Instr::Lab(Val::Label(else_label.clone())));
            // els_instrs
            compile_to_instrs(els, si, env, brake, l, instrs); 
            instrs.push(Instr::Lab(Val::Label(end_label.clone())));
        }

        Expr::Loop(e) =>{
            let startloop = new_label(l, "loop");
            let endloop = new_label(l, "loopend");
            instrs.push(Instr::Lab(Val::Label(startloop.clone())));
            compile_to_instrs(e, si, env, &endloop, l, instrs); 
            instrs.push(Instr::JMP(Val::Label(startloop.clone())));
            instrs.push(Instr::Lab(Val::Label(endloop.clone())));
        }

        Expr::Break(e) =>{
            if !brake.contains("loopend"){
                panic!("break")
            }
            compile_to_instrs(e, si, env, brake, l, instrs); 
            instrs.push(Instr::JMP(Val::Label(brake.clone())));
        }

        Expr::Set(name, val) =>{
            if !env.contains_key(name){
                panic!("Unbound variable identifier {name}")
            }
            let offset = env.get(name).unwrap() * 8;
            compile_to_instrs(val, si, env, brake, l, instrs); 
            instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX)));
        }

        Expr::Block(es) =>{
            if es.len() == 0{
                panic!("Invalid")
            }
            for e in es{
                compile_to_instrs(e, si, env, brake, l, instrs);
            }
        }
    }
}


fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => Expr::Number(i64::try_from(*n).unwrap()),
        Sexp::Atom(S(name)) if name == "true" => Expr::Boolean(true),
        Sexp::Atom(S(name)) if name == "false" => Expr::Boolean(false),
        Sexp::Atom(S(name)) => Expr::Id(name.to_string()),

        Sexp::List(vec) => match &vec[..] {
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

            [Sexp::Atom(S(op)), Sexp::List(bindings), body] if op == "let" => {
                if bindings.len() == 0{
                    panic!("Invalid");
                }

                let mut bindings_vec = Vec::new();
                for binding in bindings {
                    if let Sexp::List(vec) = binding {
                        if let [Sexp::Atom(S(name)), expr] = &vec[..] {
                            bindings_vec.push((name.to_string(), parse_expr(expr)));
                        } else {
                            panic!("Invalid");
                        }
                    } else {
                        panic!("Invalid");
                    }
                }
                Expr::Let(bindings_vec, Box::new(parse_expr(body)))
            }

            [Sexp::Atom(S(keyword)), cond, thn, els] if keyword == "if" => Expr::If(
                Box::new(parse_expr(cond)),
                Box::new(parse_expr(thn)),
                Box::new(parse_expr(els)),
            ),
            [Sexp::Atom(S(op)), e] if op == "loop" => {
                Expr::Loop(Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), e] if op == "break" => {
                Expr::Break(Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), Sexp::Atom(S(name)), e] if op == "set!" => {
                Expr::Set(name.to_string(), Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => {
                Expr::Block(exprs.into_iter().map(parse_expr).collect())
            }
            _ => panic!("Invalid"),
        },
        _ => panic!("Invalid"),
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    match parse(&in_contents){
      Err(err) => panic!("Invalid {:?}", err),
      _ => {}
    }
    
    let expr = parse_expr(&parse(&in_contents).unwrap());
    // print!("{:?}\n", expr);

    let mut instrs: Vec<Instr> = Vec::new();
    let mut labels = 0;
    compile_to_instrs(&expr, 2, &HashMap::new(), &String::from(""), &mut labels, &mut instrs);
    // print!("{:?}\n", instrs);

    let mut result = String::new();
    for instr in instrs{
        result.push_str(&instr_to_str(&instr));
    }
    // print!("{}\n", result);

    let asm_program = format!(
        "
section .text
global our_code_starts_here
extern snek_error
extern snek_print
throw_error:
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
