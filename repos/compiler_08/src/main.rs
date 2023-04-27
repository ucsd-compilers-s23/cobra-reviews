
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
    ISar(Val, Val),
    IAnd(Val, Val),
    ICmp(Val, Val),
    ICmov(Val, Val),
    ICmovne(Val, Val),
    ICmovo(Val,Val),
    ICmovg(Val, Val),
    ICmovge(Val, Val),
    ICmovl(Val, Val),
    ICmovle(Val, Val),
    IXor(Val, Val),
    ITest(Val, Val),
    
    ILabel(String),
    IJmp(String),
    IJe(String),
    IJne(String),
    IJo(String),
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

fn parse_bind(s: &Sexp) -> (String, Expr){
    match s {
        Sexp::List(vec) =>{
            let restricted = ["add1","sub1","let", "+", "-", "*", "=", "<", "<=", ">",">=", "true","false", "block", "loop", "break", "if", "set!","isnum","isbool","input"];
            match &vec[..] {
                [Sexp::Atom(S(n)),e] if !restricted.iter().any(|&w| w == n.to_string()) => (n.to_string(), parse_expr(e)),
                _ => panic!("Invalid_keyword"),
            } 
        },
        _ => panic!("Invalid"),
    }
}

fn parse_expr(s: &Sexp) -> Expr{
    match s {
        Sexp::Atom(I(n)) => Expr::Number(i64::try_from(*n).unwrap()),
        Sexp::Atom(S(n)) if n == "true" => Expr::Boolean(true),
        Sexp::Atom(S(n)) if n == "false" => Expr::Boolean(false),
        Sexp::Atom(S(n)) => Expr::Id(n.to_string()),
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(op)),e] if op == "add1" => Expr::UnOp(Op1::Add1, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)),e] if op == "sub1" => Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)),e] if op == "isnum" => Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)),e] if op == "isbool" => Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)),e] if op == "loop" => Expr::Loop(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)),e] if op == "break" => Expr::Break(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op))] if op == "block" => panic!("Invalid"),
                [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => Expr::Block(exprs.into_iter().map(parse_expr).collect()),
                [Sexp::Atom(S(op)),Sexp::List(e1),e2] if op == "let" && !e1.is_empty() => Expr::Let(e1.iter().map(|e1| parse_bind(e1)).collect::<Vec<(String,Expr)>>(), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)),e1,e2] if op == "+" => Expr::BinOp(Op2::Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)),e1,e2] if op == "-" => Expr::BinOp(Op2::Minus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)),e1,e2] if op == "*" => Expr::BinOp(Op2::Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)),e1,e2] if op == "=" => Expr::BinOp(Op2::Equal, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)),e1,e2] if op == ">" => Expr::BinOp(Op2::Greater, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)),e1,e2] if op == ">=" => Expr::BinOp(Op2::GreaterEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)),e1,e2] if op == "<" => Expr::BinOp(Op2::Less, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)),e1,e2] if op == "<=" => Expr::BinOp(Op2::LessEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)),Sexp::Atom(S(name)),e] if op == "set!" => Expr::Set(name.to_string(), Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)),e1,e2,e3] if op == "if" => Expr::If(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)), Box::new(parse_expr(e3))),
                _ => panic!("Invalid"),
            }
        },
        _ => panic!("Invalid"),
    }
}

fn compile_to_instrs(e: &Expr, si:i32, env:&HashMap<String,i32>, brake:&String ,l: &mut i32) -> Vec<Instr> {
    match e {
        Expr::Number(n) => {
            if n >= &-4611686018427387904 && n <= &4611686018427387903 {
                vec![Instr::IMov(Val::Reg(Reg::RAX),Val::Imm(*n<<1))]
            }
            else{
                panic!("Invalid out side of range");
            }
            
        },
        Expr::Boolean(b) => {
            if *b {
              vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3))]
            }
            else{
              vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))]
            }
        },
        Expr::Id(n) => {
            if n == "input" {
                vec![Instr::IMov(Val::Reg(Reg::RAX),Val::Reg(Reg::RDI))]
            }
            else if env.contains_key(n){
                let instrs = vec![Instr::IMov(Val::Reg(Reg::RAX),Val::RegOffset(Reg::RSP, *env.get(n).unwrap()))];
                instrs

            }
            else {
                panic!("Unbound variable identifier {}", n);
            }
        },
        Expr::UnOp(Op1::Add1, body) => { 
            let mut instrs = compile_to_instrs(body,si,env,brake,l);

            instrs.extend(vec![Instr::ITest(Val::Reg(Reg::RAX),Val::Imm(1))]); //test rax, 1
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RBX),Val::Imm(1))]);// mov rbx, 1    invalid_arg
            instrs.extend(vec![Instr::ICmovne(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX))]); //cmovne rdi, rax
            instrs.extend(vec![Instr::IJne("throw_error".to_string())]);

            instrs.extend(vec![Instr::IAdd(Val::Reg(Reg::RAX),Val::Imm(2))]);

            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RBX),Val::Imm(0))]); //mov rbx, 0   overflow
            instrs.extend(vec![Instr::ICmovo(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX))]); // cmovo rdi, rbx
            instrs.extend(vec![Instr::IJo("throw_error".to_string())]);
            instrs
        },
        Expr::UnOp(Op1::Sub1, body) => {
            let mut instrs = compile_to_instrs(body,si,env,brake,l);

            instrs.extend(vec![Instr::ITest(Val::Reg(Reg::RAX),Val::Imm(1))]); //test rax, 1
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RBX),Val::Imm(1))]);// mov rbx, 1    invalid_arg
            instrs.extend(vec![Instr::ICmovne(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX))]); //cmovne rdi, rax
            instrs.extend(vec![Instr::IJne("throw_error".to_string())]);

            instrs.extend(vec![Instr::ISub(Val::Reg(Reg::RAX),Val::Imm(2))]);

            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RBX),Val::Imm(0))]); //mov rbx, 0   overflow
            instrs.extend(vec![Instr::ICmovo(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX))]); // cmovo rdi, rbx
            instrs.extend(vec![Instr::IJo("throw_error".to_string())]);
            instrs
        },
        Expr::UnOp(Op1::IsNum, body) => {
            let mut instrs = compile_to_instrs(body,si, env,brake,l);
            instrs.extend(vec![Instr::IAnd(Val::Reg(Reg::RAX), Val::Imm(1))]);
            instrs.extend(vec![Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(0))]);
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RBX),Val::Imm(3))]);
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))]);
            instrs.extend(vec![Instr::ICmov(Val::Reg(Reg::RAX),Val::Reg(Reg::RBX))]);
            instrs
        },
        Expr::UnOp(Op1::IsBool, body) => {
            let mut instrs = compile_to_instrs(body,si, env,brake,l);
            instrs.extend(vec![Instr::IAnd(Val::Reg(Reg::RAX), Val::Imm(1))]);
            instrs.extend(vec![Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(1))]);
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RBX),Val::Imm(3))]);
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))]);
            instrs.extend(vec![Instr::ICmov(Val::Reg(Reg::RAX),Val::Reg(Reg::RBX))]);
            instrs
        },
        Expr::BinOp(Op2::Plus, e1,e2) =>{
            let stack_offset = si * 8;
            let mut instrs = compile_to_instrs(e1,si,env,brake,l);
            instrs.extend(vec![Instr::IMov(Val::RegOffset(Reg::RSP,stack_offset),Val::Reg(Reg::RAX))]);

            //error checking if e1 is boolean jmp error
            instrs.extend(vec![Instr::ITest(Val::Reg(Reg::RAX),Val::Imm(1))]); //test rax, 1
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RAX),Val::Imm(1))]);// mov rax, 1    invalid_arg
            instrs.extend(vec![Instr::ICmovne(Val::Reg(Reg::RDI), Val::Reg(Reg::RAX))]); //cmovne rdi, rax
            instrs.extend(vec![Instr::IJne("throw_error".to_string())]);

            instrs.extend(compile_to_instrs(e2,si+1,env,brake,l));
            
            //check if e1 and e2 are same type
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RBX),Val::Reg(Reg::RAX))]);// mov rbx, rax
            instrs.extend(vec![Instr::IXor(Val::Reg(Reg::RBX),Val::RegOffset(Reg::RSP, stack_offset))]);// xor rbx, rsp- stack_offset
            instrs.extend(vec![Instr::ITest(Val::Reg(Reg::RBX),Val::Imm(1))]);// test rbx, 1
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RBX),Val::Imm(1))]); //mov rbx, 1  invalid_arg
            instrs.extend(vec![Instr::ICmovne(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX))]); // cmovne, rdi, rbx
            instrs.extend(vec![Instr::IJne("throw_error".to_string())]);

            instrs.extend(vec![Instr::IAdd(Val::Reg(Reg::RAX),Val::RegOffset(Reg::RSP,stack_offset))]);
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RBX),Val::Imm(0))]); //mov rbx, 0   overflow
            instrs.extend(vec![Instr::ICmovo(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX))]); // cmovo rdi, rbx
            instrs.extend(vec![Instr::IJo("throw_error".to_string())]);
            instrs
        },
        
        Expr::BinOp(Op2::Minus, e1,e2) =>{
            let stack_offset = si * 8;
            let mut instrs = compile_to_instrs(e2,si,env,brake,l);
            instrs.extend(vec![Instr::IMov(Val::RegOffset(Reg::RSP,stack_offset),Val::Reg(Reg::RAX))]);

            //error checking if e1 is boolean jmp error
            instrs.extend(vec![Instr::ITest(Val::Reg(Reg::RAX),Val::Imm(1))]); //test rax, 1
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RAX),Val::Imm(1))]);// mov rax, 1    invalid_arg
            instrs.extend(vec![Instr::ICmovne(Val::Reg(Reg::RDI), Val::Reg(Reg::RAX))]); //cmovne rdi, rax
            instrs.extend(vec![Instr::IJne("throw_error".to_string())]);

            instrs.extend(compile_to_instrs(e1,si+1,env,brake,l));
            
            //check if e1 and e2 are same type
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RBX),Val::Reg(Reg::RAX))]);// mov rbx, rax
            instrs.extend(vec![Instr::IXor(Val::Reg(Reg::RBX),Val::RegOffset(Reg::RSP, stack_offset))]);// xor rbx, rsp- stack_offset
            instrs.extend(vec![Instr::ITest(Val::Reg(Reg::RBX),Val::Imm(1))]);// test rbx, 1
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RBX),Val::Imm(1))]); //mov rbx, 1   invalid_arg
            instrs.extend(vec![Instr::ICmovne(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX))]); // cmovne, rdi, rbx
            instrs.extend(vec![Instr::IJne("throw_error".to_string())]);

            instrs.extend(vec![Instr::ISub(Val::Reg(Reg::RAX),Val::RegOffset(Reg::RSP,stack_offset))]);
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RBX),Val::Imm(0))]); //mov rbx, 0
            instrs.extend(vec![Instr::ICmovo(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX))]); // cmovo rdi, rbx
            instrs.extend(vec![Instr::IJo("throw_error".to_string())]);
            instrs
        },

        Expr::BinOp(Op2::Times, e1,e2) =>{
            let stack_offset = si * 8;
            let mut instrs = compile_to_instrs(e1,si,env,brake, l);
            instrs.extend(vec![Instr::IMov(Val::RegOffset(Reg::RSP,stack_offset),Val::Reg(Reg::RAX))]);

            //error checking if e1 is boolean jmp error
            instrs.extend(vec![Instr::ITest(Val::Reg(Reg::RAX),Val::Imm(1))]); //test rax, 1
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RAX),Val::Imm(1))]);// mov rax, 1 invalid_arg
            instrs.extend(vec![Instr::ICmovne(Val::Reg(Reg::RDI), Val::Reg(Reg::RAX))]); //cmovne rdi, rax
            instrs.extend(vec![Instr::IJne("throw_error".to_string())]);

            instrs.extend(compile_to_instrs(e2,si+1,env,brake, l));

            //check if e1 and e2 are same type
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RBX),Val::Reg(Reg::RAX))]);// mov rbx, rax
            instrs.extend(vec![Instr::IXor(Val::Reg(Reg::RBX),Val::RegOffset(Reg::RSP, stack_offset))]);// xor rbx, rsp- stack_offset
            instrs.extend(vec![Instr::ITest(Val::Reg(Reg::RBX),Val::Imm(1))]);// test rbx, 1
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RBX),Val::Imm(1))]); //mov rbx, 1 invalid_arg
            instrs.extend(vec![Instr::ICmovne(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX))]); // cmovne, rdi, rbx
            instrs.extend(vec![Instr::IJne("throw_error".to_string())]);

            instrs.extend(vec![Instr::IMul(Val::Reg(Reg::RAX),Val::RegOffset(Reg::RSP,stack_offset))]);
            instrs.extend(vec![Instr::ISar(Val::Reg(Reg::RAX),Val::Imm(1))]);
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RBX),Val::Imm(0))]); //mov rbx, 0 overflow
            instrs.extend(vec![Instr::ICmovo(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX))]); // cmovo rdi, rbx
            instrs.extend(vec![Instr::IJo("throw_error".to_string())]);
            instrs
        },

        Expr::BinOp(Op2::Equal, e1,e2) => {
            let stack_offset = si * 8;
            let mut instrs = compile_to_instrs(e1,si,env,brake,l);
            instrs.extend(vec![Instr::IMov(Val::RegOffset(Reg::RSP,stack_offset),Val::Reg(Reg::RAX))]);
            instrs.extend(compile_to_instrs(e2,si+1,env,brake,l));

            //error checking part
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RBX),Val::Reg(Reg::RAX))]);// mov rbx, rax
            instrs.extend(vec![Instr::IXor(Val::Reg(Reg::RBX),Val::RegOffset(Reg::RSP, stack_offset))]);// xor rbx, rsp-stack_offset
            instrs.extend(vec![Instr::ITest(Val::Reg(Reg::RBX),Val::Imm(1))]); // test rbx, 1
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RBX),Val::Imm(1))]); //mov rbx, 1  invalid_arg
            instrs.extend(vec![Instr::ICmovne(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX))]); // cmovne, rdi, rbx
            instrs.extend(vec![Instr::IJne("throw_error".to_string())]);


            instrs.extend(vec![Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset))]);
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RBX),Val::Imm(3))]);
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RAX),Val::Imm(1))]);
            instrs.extend(vec![Instr::ICmov(Val::Reg(Reg::RAX),Val::Reg(Reg::RBX))]);
            instrs
        },

        Expr::BinOp(Op2::Greater, e1,e2) => {
            let stack_offset = si * 8;
            let mut instrs = compile_to_instrs(e1,si,env,brake,l);
            instrs.extend(vec![Instr::IMov(Val::RegOffset(Reg::RSP,stack_offset),Val::Reg(Reg::RAX))]);
            
            //error checking if e1 is boolean jmp error
            instrs.extend(vec![Instr::ITest(Val::Reg(Reg::RAX),Val::Imm(1))]); //test rax, 1
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RAX),Val::Imm(1))]);// mov rax, 1
            instrs.extend(vec![Instr::ICmovne(Val::Reg(Reg::RDI), Val::Reg(Reg::RAX))]); //cmovne rdi, rax
            instrs.extend(vec![Instr::IJne("throw_error".to_string())]);
            
            instrs.extend(compile_to_instrs(e2,si+1,env,brake,l));
            
            //check if e1 and e2 are same type
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RBX),Val::Reg(Reg::RAX))]);// mov rbx, rax
            instrs.extend(vec![Instr::IXor(Val::Reg(Reg::RBX),Val::RegOffset(Reg::RSP, stack_offset))]);// xor rbx, rsp- stack_offset
            instrs.extend(vec![Instr::ITest(Val::Reg(Reg::RBX),Val::Imm(1))]);// test rbx, 1
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RBX),Val::Imm(1))]); //mov rbx, 1
            instrs.extend(vec![Instr::ICmovne(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX))]); // cmovne, rdi, rbx
            instrs.extend(vec![Instr::IJne("throw_error".to_string())]);
            
            instrs.extend(vec![Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset))]);
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RBX),Val::Imm(3))]);
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RAX),Val::Imm(1))]);
            instrs.extend(vec![Instr::ICmovl(Val::Reg(Reg::RAX),Val::Reg(Reg::RBX))]);
            instrs
        },

        Expr::BinOp(Op2::GreaterEqual, e1,e2) =>{
            let stack_offset = si * 8;
            let mut instrs = compile_to_instrs(e1,si,env,brake,l);
            instrs.extend(vec![Instr::IMov(Val::RegOffset(Reg::RSP,stack_offset),Val::Reg(Reg::RAX))]);
            
            //error checking if e1 is boolean jmp error
            instrs.extend(vec![Instr::ITest(Val::Reg(Reg::RAX),Val::Imm(1))]); //test rax, 1
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RAX),Val::Imm(1))]);// mov rax, 1 invalid_arg
            instrs.extend(vec![Instr::ICmovne(Val::Reg(Reg::RDI), Val::Reg(Reg::RAX))]); //cmovne rdi, rax
            instrs.extend(vec![Instr::IJne("throw_error".to_string())]);
            
            
            instrs.extend(compile_to_instrs(e2,si+1,env,brake,l));

            //check if e1 and e2 are same type
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RBX),Val::Reg(Reg::RAX))]);// mov rbx, rax
            instrs.extend(vec![Instr::IXor(Val::Reg(Reg::RBX),Val::RegOffset(Reg::RSP, stack_offset))]);// xor rbx, rsp- stack_offset
            instrs.extend(vec![Instr::ITest(Val::Reg(Reg::RBX),Val::Imm(1))]);// test rbx, 1
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RBX),Val::Imm(1))]); //mov rbx, 1 invalid_arg
            instrs.extend(vec![Instr::ICmovne(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX))]); // cmovne, rdi, rbx
            instrs.extend(vec![Instr::IJne("throw_error".to_string())]);

            instrs.extend(vec![Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset))]);
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RBX),Val::Imm(3))]);
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RAX),Val::Imm(1))]);
            instrs.extend(vec![Instr::ICmovle(Val::Reg(Reg::RAX),Val::Reg(Reg::RBX))]);
            instrs
        },

        Expr::BinOp(Op2::Less, e1,e2) => {
            let stack_offset = si * 8;
            let mut instrs = compile_to_instrs(e1,si,env,brake,l);
            instrs.extend(vec![Instr::IMov(Val::RegOffset(Reg::RSP,stack_offset),Val::Reg(Reg::RAX))]);
            
            //error checking if e1 is boolean jmp error
            instrs.extend(vec![Instr::ITest(Val::Reg(Reg::RAX),Val::Imm(1))]); //test rax, 1
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RAX),Val::Imm(1))]);// mov rax, 1 invalid_arg
            instrs.extend(vec![Instr::ICmovne(Val::Reg(Reg::RDI), Val::Reg(Reg::RAX))]); //cmovne rdi, rax
            instrs.extend(vec![Instr::IJne("throw_error".to_string())]);

            instrs.extend(compile_to_instrs(e2,si+1,env,brake,l));
            
            //check if e1 and e2 are same type
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RBX),Val::Reg(Reg::RAX))]);// mov rbx, rax
            instrs.extend(vec![Instr::IXor(Val::Reg(Reg::RBX),Val::RegOffset(Reg::RSP, stack_offset))]);// xor rbx, rsp- stack_offset
            instrs.extend(vec![Instr::ITest(Val::Reg(Reg::RBX),Val::Imm(1))]);// test rbx, 1
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RBX),Val::Imm(1))]); //mov rbx, 1 invalid_arg
            instrs.extend(vec![Instr::ICmovne(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX))]); // cmovne, rdi, rbx
            instrs.extend(vec![Instr::IJne("throw_error".to_string())]);

            instrs.extend(vec![Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset))]);
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RBX),Val::Imm(3))]);
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RAX),Val::Imm(1))]);
            instrs.extend(vec![Instr::ICmovg(Val::Reg(Reg::RAX),Val::Reg(Reg::RBX))]);
            instrs
        },

        Expr::BinOp(Op2::LessEqual, e1,e2) => {
            let stack_offset = si * 8;
            let mut instrs = compile_to_instrs(e1,si,env,brake,l);
            instrs.extend(vec![Instr::IMov(Val::RegOffset(Reg::RSP,stack_offset),Val::Reg(Reg::RAX))]);
            
            //error checking if e1 is boolean jmp error
            instrs.extend(vec![Instr::ITest(Val::Reg(Reg::RAX),Val::Imm(1))]); //test rax, 1
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RAX),Val::Imm(1))]);// mov rax, 1 invalid_arg
            instrs.extend(vec![Instr::ICmovne(Val::Reg(Reg::RDI), Val::Reg(Reg::RAX))]); //cmovne rdi, rax
            instrs.extend(vec![Instr::IJne("throw_error".to_string())]);
            
            instrs.extend(compile_to_instrs(e2,si+1,env,brake,l));
            
            //check if e1 and e2 are same type
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RBX),Val::Reg(Reg::RAX))]);// mov rbx, rax
            instrs.extend(vec![Instr::IXor(Val::Reg(Reg::RBX),Val::RegOffset(Reg::RSP, stack_offset))]);// xor rbx, rsp- stack_offset
            instrs.extend(vec![Instr::ITest(Val::Reg(Reg::RBX),Val::Imm(1))]);// test rbx, 1
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RBX),Val::Imm(1))]); //mov rbx, 1 invalid_arg
            instrs.extend(vec![Instr::ICmovne(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX))]); // cmovne, rdi, rbx
            instrs.extend(vec![Instr::IJne("throw_error".to_string())]);

            instrs.extend(vec![Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset))]);
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RBX),Val::Imm(3))]);
            instrs.extend(vec![Instr::IMov(Val::Reg(Reg::RAX),Val::Imm(1))]);
            instrs.extend(vec![Instr::ICmovge(Val::Reg(Reg::RAX),Val::Reg(Reg::RBX))]);
            instrs
        },

        Expr::If(e1,e2,e3) =>{
            let end_label = new_label(l,"ifend");
            let else_label = new_label(l, "ifelse");
            let mut instrs = compile_to_instrs(e1,si,env,brake,l); //condition
            instrs.extend(vec![Instr::ICmp(Val::Reg(Reg::RAX),Val::Imm(1))]); //cmp rax,1 
            instrs.extend(vec![Instr::IJe(else_label.clone())]); //je else
            instrs.extend(compile_to_instrs(e2,si,env,brake,l));// then instrs
            instrs.extend(vec![Instr::IJmp(end_label.clone())]); // jmp end
            instrs.extend(vec![Instr::ILabel(else_label.clone())]);// else label
            instrs.extend(compile_to_instrs(e3,si,env,brake,l));// else instrs
            instrs.extend(vec![Instr::ILabel(end_label.clone())]);// end label
            instrs
        },

        Expr::Block(es) =>{
            es.into_iter().map(|e| { compile_to_instrs(e, si, env, brake, l) }).flatten().collect::<Vec<Instr>>()
        },

        Expr::Set(name, e) => {
            if env.contains_key(name){
                let stack_offset = env.get(name).unwrap();
                let mut instrs = compile_to_instrs(e,si,env,brake,l);
                instrs.extend(vec![Instr::IMov(Val::RegOffset(Reg::RSP,*stack_offset),Val::Reg(Reg::RAX))]);
                instrs
            }
            else{
                panic!("Unbound variable identifier {}", name);
            }
            
        },

        Expr::Loop(e) => {
            let start_loop = new_label(l, "loop");
            let end_loop = new_label(l, "loop_end");
            let mut instrs = vec![Instr::ILabel(start_loop.clone())];
            instrs.extend(compile_to_instrs(e,si,env,&end_loop,l));
            instrs.extend(vec![Instr::IJmp(start_loop.clone())]);
            instrs.extend(vec![Instr::ILabel(end_loop.clone())]);
            instrs
        },

        Expr::Break(e) => {
            if brake == "no_loop" {
                panic!("Invalid break");
            }
            let mut instrs = compile_to_instrs(e,si,env,brake,l);
            instrs.extend(vec![Instr::IJmp(brake.to_string())]);
            instrs
        },

        Expr::Let(bindings,body) =>{
            let mut count = si;
            let mut instrs = Vec::new();
            let mut nenv = env.clone();
            let mut ids = Vec::new();
            
            for (id,e) in bindings.iter(){
                
                if !ids.contains(id) {
                    ids.push(id.to_string());
                    let e_instrs = compile_to_instrs(e,count,&nenv,brake,l);
                    instrs.extend(e_instrs);
                    instrs.extend(vec![Instr::IMov(Val::RegOffset(Reg::RSP,count*8),Val::Reg(Reg::RAX))]);
                    nenv.insert(id.to_string(),count*8);
                    count +=1;
                }
                else {
                    panic!("Duplicate binding");
                }
            }
            
            instrs.extend(compile_to_instrs(body,count,&nenv,brake,l));
            instrs
        },


    }
}

fn new_label(l: &mut i32, s: &str) -> String{
    let current = *l;
    *l +=1;
    format!("{}_{}", s,current)
}

fn val_to_str(v: &Val) -> String{
    match v {
        Val::Reg(Reg::RAX) => "rax".to_string(),
        Val::Reg(Reg::RSP) => "rsp".to_string(),
        Val::Reg(Reg::RBX) => "rbx".to_string(),
        Val::Reg(Reg::RDI) => "rdi".to_string(),
        Val::Imm(n) => n.to_string(),
        Val::RegOffset(Reg::RAX, n) => format!("[RAX - {}]",*n),
        Val::RegOffset(Reg::RSP, n) => format!("[RSP - {}]", *n),
        Val::RegOffset(Reg::RBX, n) => format!("[RBX - {}]", *n),
        Val::RegOffset(Reg::RDI, n) => format!("[RDI - {}]", *n),
    }
}

fn instr_to_str(i: &Instr) -> String{
    match i {
        Instr::IMov(v1,v2) => format!("\nmov {}, {}",val_to_str(v1),val_to_str(v2)),
        Instr::IAdd(v1,v2) => format!("\nadd {}, {}",val_to_str(v1),val_to_str(v2)),
        Instr::ISub(v1,v2) => format!("\nsub {}, {}",val_to_str(v1),val_to_str(v2)),
        Instr::IMul(v1,v2) => format!("\nimul {}, {}",val_to_str(v1),val_to_str(v2)),
        Instr::ISar(v1,v2) => format!("\nsar {}, {}", val_to_str(v1),val_to_str(v2)),
        Instr::IAnd(v1,v2) => format!("\nand {}, {}",val_to_str(v1),val_to_str(v2)),
        Instr::ICmp(v1,v2) => format!("\ncmp {}, {}",val_to_str(v1),val_to_str(v2)),
        Instr::ICmov(v1,v2) => format!("\ncmove {}, {}",val_to_str(v1),val_to_str(v2)),
        Instr::ICmovne(v1,v2) => format!("\ncmovne {}, {}",val_to_str(v1),val_to_str(v2)),
        Instr::ICmovo(v1,v2) => format!("\ncmovo {}, {}", val_to_str(v1),val_to_str(v2)),
        Instr::ICmovg(v1,v2) => format!("\ncmovg {}, {}",val_to_str(v1),val_to_str(v2)),
        Instr::ICmovge(v1,v2) => format!("\ncmovge {}, {}",val_to_str(v1),val_to_str(v2)),
        Instr::ICmovl(v1,v2) => format!("\ncmovl {}, {}",val_to_str(v1),val_to_str(v2)),
        Instr::ICmovle(v1,v2) => format!("\ncmovle {}, {}",val_to_str(v1),val_to_str(v2)),
        Instr::ITest(v1,v2) => format!("\ntest {}, {}",val_to_str(v1),val_to_str(v2)),
        Instr::IXor(v1,v2) => format!("\nxor {}, {}",val_to_str(v1),val_to_str(v2)),
        Instr::ILabel(name) => format!("\n{}:", name),
        Instr::IJmp(name) => format!("\njmp {}", name),
        Instr::IJe(name) => format!("\nje {}", name),
        Instr::IJne(name) => format!("\njne {}", name),
        Instr::IJo(name) => format!("\njo {}", name),
    }
}

fn compile(e: &Expr,si: i32, env:&HashMap<String,i32>,brake: &String ,l: &mut i32) -> String{
    let instrs = compile_to_instrs(e,si,env,brake,l);
    let mut output = "".to_string();
    for instructions in instrs.iter(){
        let temp = instr_to_str(instructions);
        output.push_str(&temp);
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
    
    let mut labels = 0;
    let n_loop = "no_loop".to_string();
    let environment = HashMap::new();
    let sexp_text = parse(&in_contents);
    
    if let Err(_e) = sexp_text {
        panic!("Invalid");
    }

    let expr = parse_expr(&parse(&in_contents).unwrap());
    let result = compile(&expr,2,&environment, &n_loop, &mut labels);

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