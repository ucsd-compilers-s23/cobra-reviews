use sexp::Atom::*;
use sexp::*;
use std::env;
use std::fs::File;
use std::io::prelude::*;

use im::HashMap;

use std::collections::HashSet;


#[derive(Debug)]
enum Op1 { Add1, Sub1, IsNum, IsBool, }

#[derive(Debug)]
enum Op2 { Plus, Minus, Times, Equal, Greater, GreaterEqual, Less, LessEqual, }

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
}



fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => {
          let x = i64::try_from(*n).unwrap_or_else(|_| panic!("Invalid"));
          if x > 4611686018427387903 {
            panic!("Invalid");
          }
          if x < -4611686018427387904 {
            panic!("Invalid");
          }
          Expr::Number(i64::try_from(*n).unwrap_or_else(|_| panic!("Invalid")))
        }
        Sexp::Atom(S(name)) if name == "true" => Expr::True,
        Sexp::Atom(S(name)) if name == "false" => Expr::False,
        Sexp::Atom(S(name)) => Expr::Id(name.to_string()),
        Sexp::List(vec) => match &vec[..] {
          [Sexp::Atom(S(op)), e] if op == "add1" => Expr::UnOp(Op1::Add1, Box::new(parse_expr(e))),
          [Sexp::Atom(S(op)), e] if op == "sub1" => Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e))),
          [Sexp::Atom(S(op)), e] if op == "isnum" => Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e))),
          [Sexp::Atom(S(op)), e] if op == "isbool" => Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e))),
          [Sexp::Atom(S(op)), e1, e2] if op == "+" => Expr::BinOp(Op2::Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
          [Sexp::Atom(S(op)), e1, e2] if op == "-" => Expr::BinOp(Op2::Minus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
          [Sexp::Atom(S(op)), e1, e2] if op == "*" => Expr::BinOp(Op2::Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
            [Sexp::Atom(S(op)), Sexp::Atom(S(name)), e] if op == "set!" => {
                Expr::Set(name.to_string(), Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => {
                if exprs.is_empty() {
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
            [Sexp::Atom(S(op)), e1, e2] if op == "=" => {
              Expr::BinOp(Op2::Equal, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "<" => {
              Expr::BinOp(Op2::Less, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == ">" => {
              Expr::BinOp(Op2::Greater, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == ">=" => {
              Expr::BinOp(Op2::GreaterEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "<=" => {
              Expr::BinOp(Op2::LessEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(keyword)), Sexp::List(vec), body] if keyword == "let" =>  {
              let mut new_vec = Vec::new();
              for binding in vec {
                  match binding {
                      Sexp::List(test) => {
                          match &test[..] {
                              [Sexp::Atom(S(name)), val] => {
                                new_vec.push((name.to_string(), parse_expr(val)))
                              }
                              _ => panic!("Invalid"),
                          }
                      }
                      _ => panic!("Invalid"),
                  }
              };
              return Expr::Let( new_vec, Box::new(parse_expr(body)))
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

fn compile_expr(e: &Expr, si: i32, env: HashMap<String, i32>, brake: &String, l: &mut i32) -> String {
    match e {
        Expr::Number(n) => format!("mov rax, {}", *n << 1),
        Expr::True => format!("mov rax, {}", 3),
        Expr::False => format!("mov rax, {}", 1),
        Expr::Id(s) if s == "input" => format!("mov rax, rdi"),
        // chat gpt created the function
        Expr::Id(s) => {
          if let Some(val) = env.get(s) {
              let offset = val * 8;
              format!("mov rax, [rsp - {offset}]")
          } else {
              panic!("Unbound variable identifier {}", s);
          }
        }
        Expr::Set(name, val) => {

            let offset = if let Some(value) = env.get(name) {
                value * 8
            } else {
                panic!("Unbound variable identifier {}", name);
            };

            let save = format!("mov [rsp - {offset}], rax");
            let val_is = compile_expr(val, si, env.clone(), brake, l);
            format!("
              {val_is}
              {save}
              ")
        }
        Expr::UnOp(Op1::Add1, subexpr) => compile_expr(subexpr, si, env.clone(), brake, l) + "\nadd rax, 2",
        Expr::UnOp(Op1::Sub1, subexpr) => compile_expr(subexpr, si, env.clone(), brake, l) + "\nsub rax, 2",
        Expr::UnOp(Op1::IsBool, subexpr) => {
          let e_is = compile_expr(subexpr, si, env.clone(), brake, l);
          format!("
          {e_is}
          and rax, 1
          cmp rax, 1
          mov rbx, 3
          mov rax, 1
          cmove rax, rbx
        ")
        },
        Expr::UnOp(Op1::IsNum, subexpr) => {
          let e_is = compile_expr(subexpr, si, env.clone(), brake, l);
          format!("
          {e_is}
          and rax, 1
          cmp rax, 0
          mov rbx, 3
          mov rax, 1
          cmove rax, rbx
        ")
        },
        Expr::Break(e) => {
            if brake == "" {
              panic!("break");
            }
            let e_is = compile_expr(e, si, env.clone(), brake, l);
            format!("
              {e_is}
              jmp {brake}
            ")
        }
        Expr::Loop(e) => {
            let startloop = new_label(l, "loop");
            let endloop = new_label(l, "loopend");
            let e_is = compile_expr(e, si, env.clone(), &endloop, l);
            format!("
              {startloop}:
              {e_is}
              jmp {startloop}
              {endloop}:
            ")
        }
        Expr::Block(es) => {
            es.into_iter().map(|e| { compile_expr(e, si, env.clone(), brake, l) }).collect::<Vec<String>>().join("\n")
        }
        Expr::BinOp(Op2::Equal, e1, e2) => {
            let e1_instrs = compile_expr(e1, si, env.clone(), brake, l);
            let e2_instrs = compile_expr(e2, si + 1, env.clone(), brake, l);
            let offset = si * 8;
            format!(
                "
                {e1_instrs}
                mov [rsp - {offset}], rax
                {e2_instrs}
                mov rbx, rax
                xor rbx, [rsp - {offset}]
                test rbx, 1
                jne throw_error
                cmp rax, [rsp - {offset}]
                mov rbx, 3
                mov rax, 1
                cmove rax, rbx
            "
            )
        }
        Expr::BinOp(Op2::Less, e1, e2) => {
          let e1_instrs = compile_expr(e1, si, env.clone(), brake, l);
          let e2_instrs = compile_expr(e2, si + 1, env.clone(), brake, l);
          let offset = si * 8;
          format!(
              "
              {e1_instrs}
              mov [rsp - {offset}], rax
              {e2_instrs}
              mov rbx, rax
              xor rbx, [rsp - {offset}]
              test rbx, 1
              jne throw_error
              cmp rax, [rsp - {offset}]
              mov rbx, 3
              mov rax, 1
              cmovg rax, rbx
          "
          )
        },
        Expr::BinOp(Op2::GreaterEqual, e1, e2) => {
          let e1_instrs = compile_expr(e1, si, env.clone(), brake, l);
          let e2_instrs = compile_expr(e2, si + 1, env.clone(), brake, l);
          let offset = si * 8;
          format!(
              "
              {e1_instrs}
              mov [rsp - {offset}], rax
              {e2_instrs}
              mov rbx, rax
              xor rbx, [rsp - {offset}]
              test rbx, 1
              jne throw_error
              cmp rax, [rsp - {offset}]
              mov rbx, 3
              mov rax, 1
              cmovle rax, rbx
          "
          )
        },
        Expr::BinOp(Op2::LessEqual, e1, e2) => {
          let e1_instrs = compile_expr(e1, si, env.clone(), brake, l);
          let e2_instrs = compile_expr(e2, si + 1, env.clone(), brake, l);
          let offset = si * 8;
          format!(
              "
              {e1_instrs}
              mov [rsp - {offset}], rax
              {e2_instrs}
              mov rbx, rax
              xor rbx, [rsp - {offset}]
              test rbx, 1
              jne throw_error
              cmp rax, [rsp - {offset}]
              mov rbx, 3
              mov rax, 1
              cmovge rax, rbx
          "
          )
        },
        Expr::BinOp(Op2::Greater, e1, e2) => {
          let e1_instrs = compile_expr(e1, si, env.clone(), brake, l);
          let e2_instrs = compile_expr(e2, si + 1, env.clone(), brake, l);
          let offset = si * 8;
          format!(
              "
              {e1_instrs}
              mov [rsp - {offset}], rax
              {e2_instrs}
              mov rbx, rax
              xor rbx, [rsp - {offset}]
              test rbx, 1
              jne throw_error
              cmp rax, [rsp - {offset}]
              mov rbx, 3
              mov rax, 1
              cmovl rax, rbx
          "
          )
        },
        Expr::If(cond, thn, els) => {
            let end_label = new_label(l, "ifend");
            let else_label = new_label(l, "ifelse");
            let cond_instrs = compile_expr(cond, si, env.clone(), brake, l);
            let thn_instrs = compile_expr(thn, si, env.clone(), brake, l);
            let els_instrs = compile_expr(els, si, env.clone(), brake, l);
            format!(
                "
              {cond_instrs}
              cmp rax, 1
              je {else_label}
                {thn_instrs}
                jmp {end_label}
              {else_label}:
                {els_instrs}
              {end_label}:
           "
            )
        }
        Expr::BinOp(Op2::Plus, e1, e2) => {
            let e1_instrs = compile_expr(e1, si, env.clone(), brake, l);
            let e2_instrs = compile_expr(e2, si + 1, env.clone(), brake, l);
            let stack_offset = si * 8;
            format!(
                "
              {e1_instrs}
              test rax, 1
              jnz error
              mov [rsp - {stack_offset}], rax
              {e2_instrs}
              test rax, 1
              jnz error
              add rax, [rsp - {stack_offset}]
              jo throw_over_flow_error
          "
            )
        },
        Expr::BinOp(Op2::Minus, e1, e2) => {
          let e2_instrs = compile_expr(e1, si + 1, env.clone(), brake, l);
          let e1_instrs = compile_expr(e2, si + 2, env.clone(), brake, l);
          let stack_offset = si * 8;
          format!(
              "
            {e1_instrs}
            test rax, 1
            jnz error
            mov [rsp - {stack_offset}], rax
            {e2_instrs}
            test rax, 1
            jnz error
            sub rax, [rsp - {stack_offset}]
            jo throw_over_flow_error
        "
          )
        },
        Expr::BinOp(Op2::Times, e1, e2) => {
          let e1_instrs = compile_expr(e1, si, env.clone(), brake, l);
          let e2_instrs = compile_expr(e2, si + 1, env.clone(), brake, l);
          let stack_offset = si * 8;
          format!(
              "
            {e1_instrs}
            test rax, 1
            jnz error
            mov [rsp - {stack_offset}], rax
            {e2_instrs}
            test rax, 1
            jnz error
            ror rax, 1
            imul rax, [rsp - {stack_offset}]
            jo throw_over_flow_error
        "
          )
        }
        Expr::Let(vec, body) => {
          let mut offset  = 0;
          let mut initial_string = String::from("");
          let mut m = env.clone();

          let mut bad_words = HashSet::new();
          bad_words.insert("true".to_string());
          bad_words.insert("false".to_string());
          bad_words.insert("set!".to_string());
          bad_words.insert("break".to_string());
          bad_words.insert("let".to_string());
          bad_words.insert("isNum".to_string());
          bad_words.insert("isBool".to_string());
          bad_words.insert("loop".to_string());
          bad_words.insert("input".to_string());
          bad_words.insert("if".to_string());

          let mut set = HashSet::new();

          for (name, _) in vec {
            if !set.insert(name) {
                panic!("Duplicate binding");
            }
          }

          for tup in vec{

              let val = &tup.1;
              let name = &tup.0;

              if bad_words.contains(name) {
                panic!("keyword Keyword {}", name);
              }

              m = m.update(name.to_string(), si + offset);

              let val_is = compile_expr(&val, si + offset, m.clone() , brake, l);

              let amount = (si + offset) * 8;

              let cur_string = format!(
                  "
                {val_is}
                mov [rsp - {amount}], rax
            "
              );

              initial_string = initial_string + &cur_string;

              offset = offset + 1;
          }

          let body_is = compile_expr(body, si + offset , m, brake, l);

          
          let body_string = format!(
              "
            {body_is}
        "
          );

          let to_return = format!("{}{}", &initial_string, &body_string);

          return to_return;


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

    let expr = parse_expr(&parse(&in_contents).unwrap_or_else(|_| panic!("Invalid")));
    let mut labels = 0;
    let result = compile_expr(&expr, 2, HashMap::new() , &String::from(""), &mut labels);

    let asm_program = format!(
        "
section .text
global our_code_starts_here
extern error
throw_error:
  mov rdi, 7
  push rsp
  call error
  ret
throw_over_flow_error:
  mov rdi, 88
  push rsp
  call error
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