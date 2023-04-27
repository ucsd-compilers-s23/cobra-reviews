use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::Atom::*;
use sexp::*;

use im::HashMap;
use im::HashSet;



#[derive(Debug)]
enum Op1 { Add1, Sub1, IsNum, IsBool, }

#[derive(Debug)]
enum ConditionType {Equal, Greater, GreaterEqual, Less, LessEqual}

#[derive(Debug)]
enum ArithType { Plus, Minus, Times }

#[derive(Debug)]
enum Op2 { Arith(ArithType), Cond(ConditionType) }

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
    Print(Box<Expr>),
}

fn parse_bind(s: &Sexp) -> (String, Expr) {
    let keywords = vec!["let", "add1", "sub1", "if", "loop", "break", "block", "set!", "true", "false","isnum", "isbool", "print", "input"];
    match s {
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(name)), _e] if keywords.contains(&&name[..]) => panic!("keyword"),
            [Sexp::Atom(S(name)), e]  => (name.to_string(), parse_expr(e)),
            _ => panic!("Invalid"),
        },
        _ => panic!("Invalid"),
    }
}


fn parse_expr(s: &Sexp) -> Expr {
    let keywords = vec!["let", "add1", "sub1", "if", "loop", "break", "block", "set!", "true", "false","isnum", "isbool","print"];
    match s {
        Sexp::Atom(I(n)) => Expr::Number(i64::try_from(*n).unwrap()),
        Sexp::Atom(S(name)) if name == "true" => Expr::True,
        Sexp::Atom(S(name)) if name == "false" => Expr::False,
        Sexp::Atom(S(name)) if keywords.contains(&&name[..]) => panic!("keyword"),
        Sexp::Atom(S(name)) => Expr::Id(String::try_from(name).unwrap()),
        Sexp::List(vec) if vec.len() == 0 => panic!("Invalid"),
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(op)), e] if op == "add1" => Expr::UnOp(Op1::Add1,Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "sub1" => Expr::UnOp(Op1::Sub1,Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "isnum" => Expr::UnOp(Op1::IsNum,Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "isbool" => Expr::UnOp(Op1::IsBool,Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e1, e2] if op == "+" => {
                Expr::BinOp(Op2::Arith(ArithType::Plus), Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "-" => {
                Expr::BinOp(Op2::Arith(ArithType::Minus), Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "*" => {
                Expr::BinOp(Op2::Arith(ArithType::Times), Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(keyword)), bind, body] if keyword == "let" => {
                match bind {
                    Sexp::List(bindings) => {
                        if bindings.len() == 0 {
                            panic!("Invalid");
                        }
                        let mut binds = Vec::new();
                        for binding in bindings {
                            binds.push(parse_bind(binding));
                        }
                        Expr::Let(binds, Box::new(parse_expr(body)))
                    }
                    _ => panic!("Invalid"),
                }
                    
            },
            [Sexp::Atom(S(op)), Sexp::Atom(S(name)), e] if op == "set!" => {
                if keywords.contains(&&name[..]) || name == "input" {
                    panic!("keyword");
                }
                Expr::Set(name.to_string(), Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => {
                if exprs.len() == 0 {
                    panic!("Invalid");
                }
                Expr::Block(exprs.into_iter().map(parse_expr).collect())
            }
            [Sexp::Atom(S(op)), e] if op == "loop" => {
                Expr::Loop(Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), e] if op == "break" => {
                Expr::Break(Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), e] if op == "print" => {
                Expr::Print(Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "=" => {
                Expr::BinOp(Op2::Cond(ConditionType::Equal), Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            },
            [Sexp::Atom(S(op)), e1, e2] if op == ">=" => {
                Expr::BinOp(Op2::Cond(ConditionType::GreaterEqual), Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            },
            [Sexp::Atom(S(op)), e1, e2] if op == "<=" => {
                Expr::BinOp(Op2::Cond(ConditionType::LessEqual), Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            },
            [Sexp::Atom(S(op)), e1, e2] if op == ">" => {
                Expr::BinOp(Op2::Cond(ConditionType::Greater), Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            },
            [Sexp::Atom(S(op)), e1, e2] if op == "<" => {
                Expr::BinOp(Op2::Cond(ConditionType::Less), Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            },
            [Sexp::Atom(S(keyword)), cond, thn, els] if keyword == "if" => Expr::If(
              Box::new(parse_expr(cond)),
              Box::new(parse_expr(thn)),
              Box::new(parse_expr(els)),
            ),
            _ => panic!("Invalid"),
        },
        _ => panic!("Invalid"),
    }
}

fn new_label(l: &mut i32, s: &str) -> String {
    let current = *l;
    *l += 1;
    format!("{s}_{current}")
}



fn compile_to_instrs(e: &Expr, si: i32, env: &HashMap<String, i32>, brake: &String, l: &mut i32) -> String  {
    match e {   
        Expr::Number(n) => {
            let base:i64 = 2;
            if n >= &base.pow(62) || n < &(-base.pow(62)){
                panic!("Invalid")
            }
            else {
                format!("  mov rax, {}", *n * 2)
            }
        }
        Expr::Id(s) if s == "input" => format!("  mov rax, rdi"),
        Expr::Id(s) => {
            if !env.contains_key(s) {
                panic!("Unbound variable identifier {s}");
            }

            let offset = env.get(s).unwrap() * 8;
            format!("  mov rax, [rsp - {offset}]")
        }
        Expr::UnOp(Op1::Add1,subexpr) => {
            let e1_instrs = compile_to_instrs(subexpr, si, env, brake, l);
            format!("
  {e1_instrs}
  test rax, 1
  mov rdx, 1
  jnz throw_error
  add rax, 2
  mov rdx, 2
  jo throw_error")
        }
        Expr::UnOp(Op1::Sub1,subexpr) => {
            let e1_instrs = compile_to_instrs(subexpr, si, env, brake, l);
            format!("
  {e1_instrs}
  test rax, 1
  mov rdx, 1
  jnz throw_error
  sub rax, 2
  mov rdx, 2
  jo throw_error")
        }
        Expr::UnOp(Op1::IsNum,subexpr) => {
            compile_to_instrs(subexpr, si, env, brake, l) + "\n  and rax, 1\n  cmp rax, 0\n  mov rbx, 3\n  mov rax, 1\n  cmove rax,rbx"
        }
        Expr::UnOp(Op1::IsBool,subexpr) => {
            compile_to_instrs(subexpr, si, env, brake, l) + "\n  or rax, 0\n  cmp rax, 1\n  mov rbx, 3\n  mov rax, 1\n  cmove rax,rbx"
        }
        Expr::BinOp(Op2::Arith(op), e1, e2) => {
            let e1_instrs = compile_to_instrs(e1, si+1, env, brake, l);
            let e2_instrs = compile_to_instrs(e2, si, env, brake, l);
            let stack_offset = si * 8;
            let arith_instr = match op {
                ArithType::Plus => format!("add rax, [rsp - {stack_offset}]\n  mov rdx, 2"),
                ArithType::Minus => format!("sub rax, [rsp - {stack_offset}]\n  mov rdx, 2"),
                ArithType::Times => format!("sar rax, 1\n  imul rax, [rsp - {stack_offset}]\n  mov rdx, 2" ),
            };
            
            format!("
  {e2_instrs}
  test rax, 1
  mov rdx, 1
  jnz throw_error
  mov [rsp - {stack_offset}], rax
  {e1_instrs}
  test rax, 1
  mov rdx, 1
  jnz throw_error
  {arith_instr}
  mov rdx, 2
  jo throw_error")
        }
        Expr::Let(vec , body) => {
            let mut instrs = String::new();
            let mut count = 0;
            let mut sub_env = env.clone();
            // create a set and check if there are duplicate bindings
            let mut set = HashSet::new();
            for binding in vec {
                let name = &binding.0;
                let val = &binding.1;
                if set.contains(name) {
                    panic!("Duplicate binding");
                }
                else {
                    set.insert(name);
                    let val_is = compile_to_instrs(val, si+count, &sub_env, brake, l);
                    instrs = instrs + "\n" + &val_is;
                    // check if hashmap contains name, if so, panic "Duplicate binding" error:
                    let stack_offset = (si + count) * 8;
                    instrs = instrs + &format!("\n  mov [rsp - {stack_offset}], rax");
                    sub_env.insert(name.to_string(), si + count);
                    count += 1;
                }
            }
            instrs + "\n" + &compile_to_instrs(body, si + count, &sub_env, brake, l)
        }
        Expr::False => format!("  mov rax, {}", 1),
        Expr::True => format!("  mov rax, {}", 3),
        Expr::Print(e) => {
            let e_is = compile_to_instrs(e, si, env, brake, l);
            let index = if si % 2 == 1 { si + 1 } else { si };
            let offset = index * 8;
            format!("
  {e_is}
  mov [rsp - {offset}], rdi
  sub rsp, {offset}
  mov rdi, rax
  call snek_print
  add rsp, {offset}
  mov rdi, [rsp - {offset}]")
        }
        Expr::Set(name, val) => {
            let offset = env.get(name).expect(&format!("Unbound variable identifier {name}")) * 8;

            let save = format!("mov [rsp - {offset}], rax");
            let val_is = compile_to_instrs(val, si, env, brake, l);
            format!("
  {val_is}
  {save}")
        }
        Expr::Break(e) => {
            if brake == "" {
                panic!("break");
            }
            let e_is = compile_to_instrs(e, si, env, brake, l);
            format!("
  {e_is}
  jmp {brake}")
        }
        Expr::Loop(e) => {
            let startloop = new_label(l, "loop");
            let endloop = new_label(l, "loopend");
            let e_is = compile_to_instrs(e, si, env, &endloop, l);
            format!("
  {startloop}:
    {e_is}
    jmp {startloop}
  {endloop}:")
        }
        Expr::Block(es) => {
            es.into_iter().map(|e| { compile_to_instrs(e, si, env, brake, l) }).collect::<Vec<String>>().join("\n")
        }
        Expr::BinOp(Op2::Cond(cond) , e1, e2) => {
            let skip_label = new_label(l, "skip");
            let cond_instrs = match cond {
                ConditionType::Equal => format!("jne {skip_label}"),
                ConditionType::Less => format!("jge {skip_label}"),
                ConditionType::Greater => format!("jle {skip_label}"),
                ConditionType::LessEqual => format!("jg {skip_label}"),
                ConditionType::GreaterEqual => format!("jl {skip_label}"),

            };
            let e1_instrs = compile_to_instrs(e1, si, env, brake, l);
            let e2_instrs = compile_to_instrs(e2, si + 1, env, brake, l);
            let offset = si * 8;
            format!("
  {e1_instrs}
  mov [rsp - {offset}], rax
  {e2_instrs}
  mov rbx, rax
  xor rbx, [rsp - {offset}]
  test rbx, 1
  mov rdx, 1
  jne throw_error
  cmp [rsp - {offset}], rax 
  mov rbx, 3
  mov rax, 1
  {cond_instrs}
  mov rax, rbx
  {skip_label}:")
        }
        Expr::If(cond, thn, els) => {
            let end_label = new_label(l, "ifend");
            let else_label = new_label(l, "ifelse");
            let cond_instrs = compile_to_instrs(cond, si, env, brake, l);
            let thn_instrs = compile_to_instrs(thn, si, env, brake, l);
            let els_instrs = compile_to_instrs(els, si, env, brake, l);
            format!("
  {cond_instrs}
  cmp rax, 1
  je {else_label}
  {thn_instrs}
  jmp {end_label}
  {else_label}:
    {els_instrs}
  {end_label}:")
        }
        
    }
    
}








fn main() -> std::io::Result<()> {
  let args: Vec<String> = env::args().collect();

  let in_name = &args[1];
  let out_name = &args[2];

  let mut in_file = File::open(in_name)?;
  let mut in_contents = String::new();
  in_file.read_to_string(&mut in_contents)?;

  let mut labels = 0;
  let expr = parse_expr(&parse(&in_contents).expect("Invalid"));
  let result = compile_to_instrs(&expr, 2, &HashMap::new(), &String::from(""), &mut labels);

  // You will make result hold the result of actually compiling

  //let result = "mov rax, 131";

  let asm_program = format!(
      "
section .text
global our_code_starts_here
extern snek_error
extern snek_print
throw_error:
  mov rdi, rdx
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