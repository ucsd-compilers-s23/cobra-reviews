use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::*;

use im::HashMap;

#[derive(Debug, Clone)]
pub enum Val {
    Reg(Reg),
    Imm(i64),
    RegOffset(Reg, i64),
    Label(String),
}

#[derive(Debug, Clone, Copy)]
pub enum Reg {
    RAX,
    RSP,
    RBX,
    RDI,
}

#[derive(Debug)]
pub enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    IXor(Val, Val),
    ISar(Val, Val),
    ICmove(Val, Val),
    ICmovl(Val, Val),
    ICmovg(Val, Val),
    ICmovle(Val, Val),
    ICmovge(Val, Val),
    ITest(Val, Val),
    ICmp(Val, Val),
    IJmp(Val),
    IJnz(Val),
    IJne(Val),
    IJe(Val),
    IJa(Val),
    IJl(Val),
    ICall(Val),
    IPush(Val), 
    IPop(Val),
    INeg(Val),
    ILabel(Val),
}

#[derive(Debug)]
pub enum Bool {
  True, 
  False,
}

#[derive(Debug)]
pub enum Op1 {
    Add1,
    Sub1,
    Neg,
}

#[derive(Debug)]
pub enum Op2 {
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
pub enum Expr {
    Bool(Bool),
    Number(i64),
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Loop(Box<Expr>),
    Block(Vec<Expr>),
    Break(Box<Expr>),
    Print(Box<Expr>),
    Set(String, Box<Expr>),
    IsNum(Box<Expr>), 
    IsBool(Box<Expr>),
}
pub struct Context<'a> {
  si: i64,
  env: &'a HashMap<String, i64>,
  brake: &'a str,
  // target: Loc
}


mod parser;  
mod compiler; 

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let expr = match parse(&in_contents) {
      Ok(exprs) => parser::parse_expr(&exprs),
      _ => panic!("Invalid file syntax")

    }; 
    let mut labels: i64 = 0;
    let context = Context { si: 2, env: &HashMap::new(), brake: ""};
    let result = compiler::compile_expr(&expr, &context, &mut labels);

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


