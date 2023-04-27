use sexp::Atom::*;
use sexp::*;
use std::env;
use std::fs::File;
use std::io::prelude::*;

use im::HashMap;
enum Op1 { Add1, Sub1, IsNum, IsBool, }

enum Op2 { Plus, Minus, Times, Equal, Greater, GreaterEqual, Less, LessEqual, }

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
    Sexp::Atom(I(n)) => Expr::Number(i64::try_from(*n).unwrap()),
    Sexp::Atom(S(name)) if name == "true" => Expr::Boolean(true),
    Sexp::Atom(S(name)) if name == "false" => Expr::Boolean(false),
    Sexp::Atom(S(name)) => Expr::Id(name.to_string()),
    Sexp::List(vec) => match &vec[..] {
      [Sexp::Atom(S(op)), e] if op == "isnum" => Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e))),
      [Sexp::Atom(S(op)), e] if op == "isbool" => Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e))),
      [Sexp::Atom(S(op)), e] if op == "add1" => Expr::UnOp(Op1::Add1, Box::new(parse_expr(e))),
      [Sexp::Atom(S(op)), e] if op == "sub1" => Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e))),
      [Sexp::Atom(S(op)), e1, e2] if op == "+" => Expr::BinOp(Op2::Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
      [Sexp::Atom(S(op)), e1, e2] if op == "-" => Expr::BinOp(Op2::Minus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
      [Sexp::Atom(S(op)), e1, e2] if op == "*" => Expr::BinOp(Op2::Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))), 
      [Sexp::Atom(S(op)), Sexp::List(pairs), body] if op == "let" => {
        let mut environment = Vec::new();
        if pairs.is_empty(){
          panic!("Invalid expressions")
        }
        for value in pairs {
          println!("{}", value);
          println!("next line");
          let (id, expr) = parse_bind(value);
          environment.push((id, expr));
        }
        Expr::Let(environment, Box::new(parse_expr(body)))
      },
      [Sexp::Atom(S(op)), Sexp::Atom(S(name)), e] if op == "set!" => {
        Expr::Set(name.to_string(), Box::new(parse_expr(e)))
      }
      [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => {
        if exprs.is_empty(){
          panic!("Invalid expressions")
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
      [Sexp::Atom(S(op)), e1, e2] if op == "<=" => {
        Expr::BinOp(Op2::LessEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
      }
      [Sexp::Atom(S(op)), e1, e2] if op == ">" => {
        Expr::BinOp(Op2::Greater, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
      }
      [Sexp::Atom(S(op)), e1, e2] if op == ">=" => {
        Expr::BinOp(Op2::GreaterEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
      }
      [Sexp::Atom(S(keyword)), cond, thn, els] if keyword == "if" => Expr::If(
        Box::new(parse_expr(cond)),
        Box::new(parse_expr(thn)),
        Box::new(parse_expr(els)),
      ),

      _ => panic!("Invalid error: {}", s),
    },
    _ => panic!("Invalid error"),
  }
}

fn parse_bind(s: &Sexp) -> (String, Expr) {
  // println!("{}", s);
  match s {
    Sexp::List(vec) => {
      if vec.len() < 2 {
        panic!("Invalid");
      }
      if let Sexp::Atom(S(id)) = &vec[0] {
        if id == "add1" || id == "sub1" || id == "*" || id == "+"  || id == "-" || id == "let" || id == "input"{
          panic!("Invalid keyword");
        }          
        return (id.to_string(), parse_expr(&vec[1]));
      }
    }
    _ => (),
  }
  panic!("Invalid");
}

fn new_label(l: &mut i32, s: &str) -> String {
  let current = *l;
  *l += 1;
  format!("{s}_{current}")
}

#[derive(Clone,Copy)]
enum REG {
  RAX,
  RSP,
  RDI,
}

#[derive(Clone,Copy)]
enum Loc {
  LReg(REG),
  LStack(i32)
}


#[derive(Clone,Copy)]
enum Val {
  VReg(REG),
  VStack(i32),
  VImm(i64)
}

use REG::*;
use Loc::*;
use Val::*;

struct Context<'a> {
  si: i32,
  env: &'a HashMap<String, Loc>,
  brake: &'a str,
  target: Loc
}

fn reg_to_str(r : &REG) -> String {
  match r {
    RAX => String::from("rax"),
    RSP => String::from("rsp"),
    RDI => String::from("rdi")
  }
}

fn val_to_str(v : &Val) -> String {
  match v {
    VStack(n) => {
      let offset = n * 8;
      format!("qword [rsp - {offset}]")
    }
    VReg(r) => reg_to_str(r),
    VImm(n) => format!("{}", n)
  }
}

fn mov_target(dest : &Loc, source : &Val) -> String {
  match (dest, source) {
    (LStack(n), VStack(_m)) => {
      format!("
        mov rax, {}
        mov {}, rax
      ",
      val_to_str(source), val_to_str(&VStack(*n)))
    },
    (LReg(r1), _) => format!("mov {}, {}", reg_to_str(r1), val_to_str(source)),
    (LStack(n), _) => format!("mov {}, {}", val_to_str(&VStack(*n)), val_to_str(source))
  }
}

fn compile_expr(e: &Expr, c : &Context, l: &mut i32) -> String {
    match e {
        Expr::Number(n) => {
          if n.checked_mul(2) == None{
            panic!("Invalid error");
          }
          mov_target(&c.target, &VImm(*n << 1))
        },
        Expr::Boolean(true) => mov_target(&c.target, &VImm(3)),
        Expr::Boolean(false) => mov_target(&c.target, &VImm(1)),
        Expr::Id(s) if s == "input" => mov_target(&c.target, &VReg(RDI)),
        Expr::Id(s) => {
          match c.env.get(s) {
            Some(LReg(reg)) => format!("mov rax, {}", reg_to_str(reg)),
            Some(LStack(offset)) => {
              mov_target(&c.target, &VStack(*offset))
            },
            None => panic!("Unbound variable identifier {}", s),
          }
        }
        Expr::Set(name, val) => {
          let target = match c.env.get(name) {
            Some(_) => c.env.get(name).unwrap(),
            None => panic!("Unbound variable identifier {}", name)
          };
          let nctxt = Context { target: LReg(RAX), ..*c };
          let val_is = compile_expr(val, &nctxt, l);
          let save = mov_target(&target, &VReg(RAX));
          format!("
            {val_is}
            {save}
            ")
        }
        Expr::UnOp(op, expr) => {
          let expr1 = compile_expr(expr, c, l);
          match op {
            Op1::Add1 => {
              let fintar = mov_target(&c.target, &VReg(RAX));
              format!("
                {expr1}
                test rax, 1
                mov rbx, 5
                jnz throw_error
                add rax, 2
                mov rbx, 10
                jo throw_error
                {fintar}
              ")
            }
            Op1::Sub1 => {
              let fintar = mov_target(&c.target, &VReg(RAX));
              format!("
                {expr1}
                test rax, 1
                mov rbx, 5
                jnz throw_error
                sub rax, 2
                mov rbx, 10
                jo throw_error
                {fintar}
              ")
            },
            Op1::IsBool => format!("
              {expr1}
              test rax, 1
              mov rbx, 3
              mov rax, 1
              cmovnz rax, rbx
            "),
            Op1::IsNum => format!("
              {expr1}
              test rax, 1
              mov rbx, 3
              mov rax, 1
              cmovz rax, rbx
            "),
          }
        }
        Expr::Break(e) => {
          if c.brake == "" {
            panic!("break error")
          }
          else{
            let nctxt = Context { target: LReg(RAX), ..*c };
            let e_is = compile_expr(e, &nctxt, l);
            format!("
              {e_is}
              jmp {}
            ", c.brake)
          }
        }
        Expr::Loop(e) => {
            let startloop = new_label(l, "loop");
            let endloop = new_label(l, "loopend");
            let e_is = compile_expr(e, &Context { brake: &endloop, ..*c }, l);
            let save = mov_target(&c.target, &VReg(RAX));
            format!("
              {startloop}:
              {e_is}
              jmp {startloop}
              {endloop}:
              {save}
            ")
        }
        Expr::Block(es) => {
            // could consider writing all but last into RAX or something

            es.into_iter().map(|e| { compile_expr(e, c, l) }).collect::<Vec<String>>().join("\n")
        }
        Expr::BinOp(op, e1, e2) => {
          match op {
            Op2::Plus => {
              let save_e1_ctxt = Context { target: LReg(RAX), ..*c };
              let e1_instrs = compile_expr(e1, &save_e1_ctxt, l);
              let e2_ctxt = Context { si: c.si + 1, target: LReg(RAX), ..*c };
              let e2_instrs = compile_expr(e2, &e2_ctxt, l);
              let stack_offset = c.si * 8;
              let save = mov_target(&c.target, &VReg(RAX));
              format!(
              "
                {e1_instrs}
                test rax, 1
                mov rbx, 5
                jnz throw_error
                mov [rsp - {stack_offset}], rax
                {e2_instrs}
                test rax, 1
                mov rbx, 5
                jnz throw_error
                add rax, [rsp - {stack_offset}]
                mov rbx, 10
                jo throw_error
                {save}
              "
              )
            },
            Op2::Times => {
              let save_e1_ctxt = Context { target: LReg(RAX), ..*c };
              let e1_instrs = compile_expr(e1, &save_e1_ctxt, l);
              let e2_ctxt = Context { si: c.si + 1, target: LReg(RAX), ..*c };
              let e2_instrs = compile_expr(e2, &e2_ctxt, l);
              let stack_offset = c.si * 8;
              let save = mov_target(&c.target, &VReg(RAX));
              format!(
              "
                {e1_instrs}
                test rax, 1
                mov rbx, 5
                jnz throw_error
                mov [rsp - {stack_offset}], rax
                {e2_instrs}
                test rax, 1
                mov rbx, 5
                jnz throw_error
                sar rax, 1
                imul rax, [rsp - {stack_offset}]
                mov rbx, 10
                jo throw_error
                {save}
              "
              )
            },
            Op2::Minus => {
              let save_e1_ctxt = Context { si: c.si + 1, target: LReg(RAX), ..*c };
              let e1_instrs = compile_expr(e1, &save_e1_ctxt, l);
              let e2_ctxt = Context { si: c.si, target: LReg(RAX), ..*c };
              let e2_instrs = compile_expr(e2, &e2_ctxt, l);
              let stack_offset = c.si * 8;
              let save = mov_target(&c.target, &VReg(RAX));
              format!(
              "
                {e2_instrs}
                test rax, 1
                mov rbx, 5
                jnz throw_error
                mov [rsp - {stack_offset}], rax
                {e1_instrs}
                test rax, 1
                mov rbx, 5
                jnz throw_error
                sub rax, [rsp - {stack_offset}]
                mov rbx, 10
                jo throw_error
                {save}
              "
              )
            },
            Op2::Equal => {
              let save_e1_ctxt = Context { target: LStack(c.si), ..*c };
              let e1_instrs = compile_expr(e1, &save_e1_ctxt, l);
              let e2_ctxt = Context { si: c.si + 1, target: LReg(RAX), ..*c };
              let e2_instrs = compile_expr(e2, &e2_ctxt, l);
              let offset = c.si * 8;
              format!(
              "
                {e1_instrs}
                {e2_instrs}
                mov rbx, rax
                xor rbx, [rsp - {offset}]
                test rbx, 1
                mov rbx, 5
                jne throw_error
                cmp rax, [rsp - {offset}]
                mov rbx, 3
                mov rax, 1
                cmove rax, rbx
              "
              )
            },
            Op2::Less => {
              let save_e1_ctxt = Context { target: LStack(c.si), ..*c };
              let e1_instrs = compile_expr(e1, &save_e1_ctxt, l);
              let e2_ctxt = Context { si: c.si + 1, target: LReg(RAX), ..*c };
              let e2_instrs = compile_expr(e2, &e2_ctxt, l);
              let offset = c.si * 8;
              format!(
              "
                {e1_instrs}
                {e2_instrs}
                mov rbx, rax
                xor rbx, [rsp - {offset}]
                test rbx, 1
                mov rbx, 5
                jne throw_error
                cmp rax, [rsp - {offset}]
                mov rbx, 3
                mov rax, 1
                cmovg rax, rbx
              "
              )
            },
            Op2::LessEqual => {
              let save_e1_ctxt = Context { target: LStack(c.si), ..*c };
              let e1_instrs = compile_expr(e1, &save_e1_ctxt, l);
              let e2_ctxt = Context { si: c.si + 1, target: LReg(RAX), ..*c };
              let e2_instrs = compile_expr(e2, &e2_ctxt, l);
              let offset = c.si * 8;
              format!(
              "
                {e1_instrs}
                {e2_instrs}
                mov rbx, rax
                xor rbx, [rsp - {offset}]
                test rbx, 1
                mov rbx, 5
                jne throw_error
                cmp rax, [rsp - {offset}]
                mov rbx, 3
                mov rax, 1
                cmovge rax, rbx
              "
              )
            },
            Op2::Greater => {
              let save_e1_ctxt = Context { target: LStack(c.si), ..*c };
              let e1_instrs = compile_expr(e1, &save_e1_ctxt, l);
              let e2_ctxt = Context { si: c.si + 1, target: LReg(RAX), ..*c };
              let e2_instrs = compile_expr(e2, &e2_ctxt, l);
              let offset = c.si * 8;
              format!(
              "
                {e1_instrs}
                {e2_instrs}
                mov rbx, rax
                xor rbx, [rsp - {offset}]
                test rbx, 1
                mov rbx, 5
                jne throw_error
                cmp rax, [rsp - {offset}]
                mov rbx, 3
                mov rax, 1
                cmovl rax, rbx
              "
              )
            },
            Op2::GreaterEqual => {
              let save_e1_ctxt = Context { target: LStack(c.si), ..*c };
              let e1_instrs = compile_expr(e1, &save_e1_ctxt, l);
              let e2_ctxt = Context { si: c.si + 1, target: LReg(RAX), ..*c };
              let e2_instrs = compile_expr(e2, &e2_ctxt, l);
              let offset = c.si * 8;
              format!(
              "
                {e1_instrs}
                {e2_instrs}
                mov rbx, rax
                xor rbx, [rsp - {offset}]
                test rbx, 1
                mov rbx, 5
                jne throw_error
                cmp rax, [rsp - {offset}]
                mov rbx, 3
                mov rax, 1
                cmovle rax, rbx
              "
              )
            },
          }
        }

        Expr::If(cond, thn, els) => {
            let end_label = new_label(l, "ifend");
            let else_label = new_label(l, "ifelse");
            let cond_ctxt = Context { target: LReg(RAX), ..*c };
            let cond_instrs = compile_expr(cond, &cond_ctxt, l);
            let thn_instrs = compile_expr(thn, c, l); // note original context, so store to wherever caller wants
            let els_instrs = compile_expr(els, c, l);
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
        Expr::Let(bindings, body) => {
          let mut new_env = c.env.clone();
          let mut new_si = c.si;
          let mut fin: String = String::from("");
          let mut dup_checker:HashMap<String, Loc> = HashMap::new();
          for (id, expr) in bindings {
            if dup_checker.contains_key(id){
              panic!("Duplicate binding");
            }
            let val_ctxt = Context { target: LStack(new_si), env: &new_env, si: new_si, ..*c };
            let val_is = compile_expr(expr, &val_ctxt, l);
            new_env = new_env.update(id.to_string(), LStack(new_si));
            dup_checker = dup_checker.update(id.to_string(), LStack(new_si));
            new_si += 1;
            fin.push_str(&val_is);
            fin.push_str("\n");
          }
          let body_is = compile_expr(body, &Context { si: new_si, env: &new_env, ..*c}, l);
          format!(
          "
            {fin}
            {body_is}
          " 
          )
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
    // if(parse(&in_contents) )
    let expr = parse_expr(&parse(&in_contents).expect("Invalid"));
    let mut labels = 0;
    let context = Context { si: 2, env: &HashMap::new(), brake: "", target: LReg(RAX) };
    let result = compile_expr(&expr, &context, &mut labels);
    let asm_program = format!(
        "
section .text
global our_code_starts_here
extern error
throw_error:
  mov rdi, rbx
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