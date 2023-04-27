use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::Atom::*;
use sexp::*;

use im::HashMap;

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
    Id(String),
    True,
    False, 
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

fn new_label (l : &mut i32, s : &str) -> String {
  let current = *l; 
  *l += 1; 
  format!("{s}_{current}")
}

fn compile (e: &Expr, si : i32, mut offset_map : HashMap<String, i32>, brake: &String, l: &mut i32) -> String {
    match e {
  Expr::Number(n) => {
    if *n > 4611686018427387903 || *n < -4611686018427387904 {
      panic!("overflow")
    } 
  format!("\nmov rax, {}", *n<<1)}, 
  Expr::True => format!("\nmov rax, 3"), 
  Expr::False => format!("\nmov rax, 1"),
  Expr::Input => format!("\nmov rax, rdi"),
  Expr::UnOp(op, subexpr) => {
  match op {
  Op1::Add1 => compile(subexpr, si, offset_map.clone(), brake, l) + "\nadd rax, 2",
  Op1::Sub1 => compile(subexpr, si, offset_map.clone(), brake, l) + "\nsub rax, 2",
  Op1::IsNum => {
  let e_instrs = compile(subexpr, si, offset_map.clone(), brake, l);
  format! ("{e_instrs}
  mov rbx, rax
  test rbx, 1
  mov rbx, 3
  mov rax, 1
  cmove rax, rbx
  ")
  },
  Op1::IsBool => {
  let e_instrs = compile(subexpr, si, offset_map.clone(), brake, l);
  format! ("{e_instrs}
  mov rbx, rax
  test rbx, 1
  mov rbx, 1
  mov rax, 3
  cmove rax, rbx
  ")
  },
  }
  }
  Expr::BinOp(op, e1, e2) => {

  let stack_offset = si*8; 
  let mut e1_instrs = compile(e1, si, offset_map.clone(), brake, l);
  let mut e2_instrs = compile(e2, si+1, offset_map.clone(), brake, l);
  match op {
  Op2::Plus => {
  format!("
  {e1_instrs}
  mov rbx, rax
  test rbx, 1

  jne throw_error
  mov [rsp - {stack_offset}], rax
  {e2_instrs}
  mov rbx, rax
  test rbx, 1

  jne throw_error
  add rax, [rsp - {stack_offset}]
  ")},
  Op2::Minus => {
     e1_instrs = compile(e1, si+1, offset_map.clone(), brake, l);
     e2_instrs = compile(e2, si, offset_map.clone(), brake, l);
  format!("
  {e2_instrs}
  mov rbx, rax
  test rbx, 1

  jne throw_error
  mov [rsp - {stack_offset}], rax
  {e1_instrs}
  mov rbx, rax
  test rbx, 1

  jne throw_error
  sub rax, [rsp - {stack_offset}]
  ")},
  Op2::Times => {
  format!("
  {e1_instrs}
  mov rbx, rax
  test rbx, 1

  jne throw_error
  mov [rsp - {stack_offset}], rax
  {e2_instrs}
  mov rbx, rax
  test rbx, 1
  jne throw_error
  sar rax, 1
  mov rbx, [rsp - {stack_offset}]
  sar rbx, 1
  imul rax, rbx
  imul rax, 2
  ")},
  // test to check that both are bools or numbers 
  Op2::Equal => {
  format!("{e1_instrs}
  mov [rsp - {stack_offset}], rax
  {e2_instrs}
  mov rbx, rax
  xor rbx, [rsp - {stack_offset}]
  test rbx, 1 

  jne throw_error
  cmp rax, [rsp - {stack_offset}]
  mov rbx, 3
  mov rax, 1
  cmove rax, rbx
  ")
  },
  Op2::Greater => {
  let end_label = new_label(l, "end");
  let true_label = new_label(l, "true");
  let false_label = new_label(l, "false");
  format!("
  {e2_instrs}
  mov rbx, rax
  test rbx, 1

  jne throw_error
  mov [rsp - {stack_offset}], rax
  {e1_instrs}
  mov rbx, rax
  test rbx, 1

  jne throw_error
  cmp rax, [rsp - {stack_offset}]
  jg {true_label} 
  {false_label} : 
  mov rax, 1
  jmp {end_label}
  {true_label} :
  mov rax, 3
  {end_label} :
  ")
  },              
  Op2::GreaterEqual => {
  let end_label = new_label(l, "end");
  let true_label = new_label(l, "true");
  let false_label = new_label(l, "false");
  format!("  
  {e2_instrs}
  mov rbx, rax
  test rbx, 1

  jne throw_error
  mov [rsp - {stack_offset}], rax
  {e1_instrs}
  mov rbx, rax
  test rbx, 1

  jne throw_error
  cmp rax, [rsp - {stack_offset}]
  jge {true_label} 
  {false_label} : 
  mov rax, 1
  jmp {end_label}
  {true_label} :
  mov rax, 3
  {end_label} :
  ")
  },          
  Op2::Less => {
  let end_label = new_label(l, "end");
  let true_label = new_label(l, "true");
  let false_label = new_label(l, "false");
  format!("  {e2_instrs}
  mov rbx, rax
  test rbx, 1

  jne throw_error
  mov [rsp - {stack_offset}], rax
  {e1_instrs}
  mov rbx, rax
  test rbx, 1

  jl throw_error
  cmp rax, [rsp - {stack_offset}]
  jl {true_label} 
  {false_label} : 
  mov rax, 1
  jmp {end_label}
  {true_label} :
  mov rax, 3
  {end_label} :")
  },
  Op2::LessEqual => {
  let end_label = new_label(l, "end");
  let true_label = new_label(l, "true");
  let false_label = new_label(l, "false");
  format!("  {e2_instrs}
  mov rbx, rax
  test rbx, 1

  jne throw_error
  mov [rsp - {stack_offset}], rax
  {e1_instrs}
  mov rbx, rax
  test rbx, 1

  jne throw_error
  cmp rax, [rsp - {stack_offset}]
  jle {true_label} 
  {false_label} : 
  mov rax, 1
  jmp {end_label}
  {true_label} :
  mov rax, 3
  {end_label} :")
  },
            }
        },
        Expr::Id(s) => {
        let check = offset_map.get(s); 
        match check {
            None => panic!("Unbound variable identifier {}", s),
            Some(_) => format!("\nmov rax, [rsp-{}]", *(offset_map.clone().get(s).unwrap())),
            }
        },
        Expr::Set(s, e) => {
          let check = offset_map.get(s); 
          let e_instrs = compile(e, si, offset_map.clone(), brake, l); 
          match check {
              None => panic!("Unbound variable identifier {}", s),
              Some(_) => format!(" {e_instrs}
              mov [rsp-{}], rax", *(offset_map.clone().get(s).unwrap())),
              }
          },
        Expr::Let(vec, body) => {
            let mut res = "".to_string();
            let mut my_map : HashMap<String, i32> = HashMap::new();
            let mut i = 0; 
            

            for binding in vec { 
                let e_instrs = compile(&(binding.1), si+i, offset_map.clone(),brake,l); 
                
                let check = my_map.get(&binding.0); 
                let stack_offset = (si+i)*8;

                if check.is_some() {
                    panic!("Duplicate binding")
                }

                my_map = my_map.update(binding.0.clone(), stack_offset);
                offset_map = offset_map.update(binding.0.clone(), stack_offset);
                res = format!("
                {res}
                {e_instrs}
                mov [rsp-{stack_offset}], rax
                ");
                i = i+1;
            }

            let b_instrs = compile(body, si+i, offset_map.clone(), brake, l); 

            res = format!("
            {res}
            {b_instrs}
            ");
            res 
        },   
        Expr::If(cond, thn, els) => {
          let end_label = new_label(l, "ifend");
          let else_label = new_label(l, "ifelse");
          let cond_instrs = compile (cond, si, offset_map.clone(), brake, l); 
          let thn_instrs = compile (thn, si, offset_map.clone(), brake, l); 
          let els_instrs = compile (els, si, offset_map.clone(), brake, l); 
          format!("
          {cond_instrs}
          cmp rax, 1 
          je {else_label}
          {thn_instrs}
          jmp {end_label}

          {else_label} : 
            {els_instrs}
          {end_label}:
          ")
        },
        Expr::Block(es) => {
          let mut res = "".to_string();
          let mut e_instrs : String;
          for e in es {
            e_instrs = compile (e, si, offset_map.clone(), brake, l);  
            res = format! ("{res}
            {e_instrs}
            ")
          }
          res
        },
        Expr::Break(e) => {   
          if brake == "" {
            panic!("break")
          }         
          let e_is = compile(e, si, offset_map.clone(), brake, l);
          format!("
            {e_is}
            jmp {brake}
          ")
        },
        Expr::Loop(e) => {
          let startloop = new_label(l, "loop");
          let endloop = new_label(l, "loopend");
          let e_is = compile(e, si, offset_map.clone(), &endloop, l);
          format!("
            {startloop}:
            {e_is}
            jmp {startloop}
            {endloop}:
          ")
        },
    }
}

fn parse_bind(s:&Sexp) -> (String, Expr) {
    match s {
        Sexp::List(vec) => {
            match &vec[..] {
            [Sexp::Atom(S(id)), e] =>  (id.to_string(), parse_expr(e)),
            _ => panic!("Invalid"),
        }
    }
        _ => panic!("Invalid"),
    }
}

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => Expr::Number(i64::try_from(*n).unwrap()),
        Sexp::Atom(S(b)) if b == "true"=> Expr::True,
        Sexp::Atom(S(b)) if b == "false"=> Expr::False,
        Sexp::Atom(S(b)) if b == "input"=> Expr::Input,
        Sexp::Atom(S(id)) if (id != "true" && id != "false" && id != "set!" && id != "block" ) => Expr::Id(id.to_string()),
        Sexp::List(vec) => {
            match &vec[..] {
              [Sexp::Atom(S(op)), Sexp::Atom(S(name)), e] if op == "set!" => Expr::Set(name.to_string(), Box::new(parse_expr(e))),
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
              [Sexp::Atom(S(op)), Sexp::List(bindings), e2] if op == "let" => {
                    if bindings.is_empty() {
                        panic!("Invalid")
                    }

                    let mut res :Vec<(String, Expr)> = vec![]; 
                    for binding in bindings { 
                    res.push(parse_bind(binding)) }
                    Expr::Let(res, Box::new(parse_expr(e2)))
                },
              [Sexp::Atom(S(op)), e] if op == "loop" => Expr::Loop(Box::new(parse_expr(e))),
              [Sexp::Atom(S(op)), e] if op == "break" => Expr::Break(Box::new(parse_expr(e))),
              [Sexp::Atom(S(op)), e1, e2, e3] if op == "if" => Expr::If(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)), Box::new(parse_expr(e3))),
              [Sexp::Atom(S(op)), Sexp::List(es)] if op == "block" => {

                if es.is_empty() {
                  panic!("Invalid")
              }

              let mut res :Vec<Expr> = vec![]; 
              for e in es { 
                res.push(parse_expr(e)) }
              Expr::Block(res)
              },
                _ => panic!("Invalid"),
            }
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

  let pass = &parse(&in_contents);

  if pass.is_err() {
      panic!("Invalid")
    }

  let mut l:i32 = 0;
  let expr = parse_expr(&parse(&in_contents).unwrap());
  let result = compile(&expr, 2, HashMap::new(), &String::from(""), &mut l);
  let asm_program = format!(
    "
section .text
extern snek_error
global our_code_starts_here
throw_error : 
  mov rdi, 7
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


