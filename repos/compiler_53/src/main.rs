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
  RegOffset(Reg, i64),
}

#[derive(Debug)]
enum Label {
  TYPEERROR,
  OVERFLOW,
  LName(String),
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
  Test(Val, Val),
  Cmp(Val, Val),
  Sar(Val, Val),
  Xor(Val, Val),
  Jmp(Label),
  Je(Label),
  Jne(Label),
  Jg(Label),
  Jl(Label),
  Jge(Label),
  Jle(Label),
  Jo(Label),
  Nothing(Label),
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
  Lt,
  Gt,
  Ge,
  Le,
  Eq,
}

#[derive(Debug)]
enum Expr {
  Number(i64),
  TRUE,
  FALSE,
  INPUT,
  Id(String),
  Let(Vec<(String, Expr)>, Box<Expr>),
  UnOp(Op1, Box<Expr>),
  BinOp(Op2, Box<Expr>, Box<Expr>),
  Set(String, Box<Expr>),
  If(Box<Expr>, Box<Expr>, Box<Expr>),
  Block(Vec<Expr>),
  Loop(Box<Expr>),
  Break(Box<Expr>),
}

fn parse_bind(s: &Sexp) -> (String, Expr) {
  match s {
    Sexp::List(vec) => {
      match &vec[..] {
        [Sexp::Atom(S(name)), e] => (name.to_string(), parse_expr(e)),
        _ => panic!("Invalid"),
      }
    }
    _ => panic!("Invalid"),
  }
}

fn parse_expr(s: &Sexp) -> Expr {
  match s {
    Sexp::Atom(I(n)) => Expr::Number(i64::try_from(*n).unwrap()),
    Sexp::Atom(S(keyword)) if keyword == "true" => Expr::TRUE,
    Sexp::Atom(S(keyword)) if keyword == "false" => Expr::FALSE,
    Sexp::Atom(S(keyword)) if keyword == "input" => Expr::INPUT,
    Sexp::Atom(S(id)) => Expr::Id(id.to_string()),
    Sexp::List(vec) => {
      match &vec[..] {
        [Sexp::Atom(S(op)), e] if op == "add1" => Expr::UnOp(Op1::Add1, Box::new(parse_expr(e))),
        [Sexp::Atom(S(op)), e] if op == "sub1" => Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e))),
        [Sexp::Atom(S(op)), e] if op == "isnum" => Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e))),
        [Sexp::Atom(S(op)), e] if op == "isbool" => Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e))),
        [Sexp::Atom(S(op)), e1, e2] if op == "+" => Expr::BinOp(Op2::Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
        [Sexp::Atom(S(op)), e1, e2] if op == "-" => Expr::BinOp(Op2::Minus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
        [Sexp::Atom(S(op)), e1, e2] if op == "*" => Expr::BinOp(Op2::Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
        [Sexp::Atom(S(op)), e1, e2] if op == "<" => Expr::BinOp(Op2::Lt, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
        [Sexp::Atom(S(op)), e1, e2] if op == ">" => Expr::BinOp(Op2::Gt, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
        [Sexp::Atom(S(op)), e1, e2] if op == ">=" => Expr::BinOp(Op2::Ge, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
        [Sexp::Atom(S(op)), e1, e2] if op == "<=" => Expr::BinOp(Op2::Le, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
        [Sexp::Atom(S(op)), e1, e2] if op == "=" => Expr::BinOp(Op2::Eq, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
        [Sexp::Atom(S(set)), Sexp::Atom(S(name)), e2] if set == "set!" => Expr::Set(name.to_string(), Box::new(parse_expr(e2))),
        [Sexp::Atom(S(if_)), e1, e2, e3] if if_ == "if" => Expr::If(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)), Box::new(parse_expr(e3))),
        [Sexp::Atom(S(block)), body @ ..] if block == "block" => {
          if body.is_empty() {
            panic!("Invalid");
          }
          let mut vec = Vec::new();
          for e in body {
            vec.push(parse_expr(e));
          }
          Expr::Block(vec)
        },
        [Sexp::Atom(S(loop_)), e] if loop_ == "loop" => Expr::Loop(Box::new(parse_expr(e))),
        [Sexp::Atom(S(break_)), e] if break_ == "break" => Expr::Break(Box::new(parse_expr(e))),
        [Sexp::Atom(S(let_)), Sexp::List(binds), e] if let_ == "let" => {
          if binds.is_empty() {
            panic!("Invalid");
          }
          let mut vec = Vec::new();
          for bind in binds {
            vec.push(parse_bind(bind));
          }
          Expr::Let(vec, Box::new(parse_expr(e)))
        },
        _ => panic!("Invalid"),
      }
    },
    _ => panic!("Invalid"),
  }
}

fn compile_to_instrs(e: &Expr, si: i64, env: &HashMap<String, i64>, l: &mut i64, bl: i64) -> Vec<Instr> {
  let mut v = Vec::<Instr>::new();
  match e {
    Expr::Number(n) => {
      if *n < -4611686018427387904 || *n > 4611686018427387903 {
        panic!("Invalid");
      }
      v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm((*n) * 2)));
    },
    Expr::TRUE => v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3))),
    Expr::FALSE => v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))),
    Expr::INPUT => v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI))),
    Expr::UnOp(op, subexpr) => {
      v.extend(compile_to_instrs(subexpr, si, env, l, bl));
      match op {
        Op1::Add1 => {
          v.push(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)));
          v.push(Instr::Jne(Label::TYPEERROR));
          v.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(2)));
          v.push(Instr::Jo(Label::OVERFLOW));
        },
        Op1::Sub1 => {
          v.push(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)));
          v.push(Instr::Jne(Label::TYPEERROR));
          v.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(2)));
          v.push(Instr::Jo(Label::OVERFLOW));
        },
        Op1::IsNum => {
          v.push(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)));
          v.push(Instr::Jne(Label::LName(format!("label{}", *l)))); // is not num, return false
          v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
          v.push(Instr::Jmp(Label::LName(format!("label{}", *l + 1)))); // is num, jmp out
          v.push(Instr::Nothing(Label::LName(format!("label{}", *l))));
          v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
          v.push(Instr::Nothing(Label::LName(format!("label{}", *l + 1))));
          *l += 2;
        },
        Op1::IsBool => {
          v.push(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)));
          v.push(Instr::Jne(Label::LName(format!("label{}", *l)))); // is num, return false
          v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
          v.push(Instr::Jmp(Label::LName(format!("label{}", *l + 1)))); // is not num, jmp out
          v.push(Instr::Nothing(Label::LName(format!("label{}", *l))));
          v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
          v.push(Instr::Nothing(Label::LName(format!("label{}", *l + 1))));
          *l += 2;
        },
      }
    },
    Expr::BinOp(op, subexpr1, subexpr2) => {
      v.extend(compile_to_instrs(subexpr2, si, env, l, bl));
      // check if rax is num (exp2)
      match op {
        Op2::Eq => {},
        _ => {
          v.push(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)));
          v.push(Instr::Jne(Label::TYPEERROR));
        },
      }
      v.push(Instr::IMov(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
      v.extend(compile_to_instrs(subexpr1, si + 1, env, l, bl));
      // check if rax is num (exp1)
      match op {
        Op2::Eq => {
          v.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(0)));
          v.push(Instr::Xor(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
          v.push(Instr::Xor(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, si * 8)));
          v.push(Instr::Test(Val::Reg(Reg::RBX), Val::Imm(1)));
          v.push(Instr::Jne(Label::TYPEERROR));
        },
        _ => {
          v.push(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)));
          v.push(Instr::Jne(Label::TYPEERROR));
        },
      }
      match op {
        Op2::Plus => {
          v.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si * 8)));
          v.push(Instr::Jo(Label::OVERFLOW));
        },
        Op2::Minus => {
          v.push(Instr::ISub(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si * 8)));
          v.push(Instr::Jo(Label::OVERFLOW));
        },
        Op2::Times => {
          v.push(Instr::Sar(Val::Reg(Reg::RAX), Val::Imm(1)));
          v.push(Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si * 8)));
          v.push(Instr::Jo(Label::OVERFLOW));
        },
        Op2::Lt => {
          v.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si * 8)));
          v.push(Instr::Jl(Label::LName(format!("label{}", *l)))); // greater, return true
          v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
          v.push(Instr::Jmp(Label::LName(format!("label{}", *l + 1)))); // is not greater, jmp out
          v.push(Instr::Nothing(Label::LName(format!("label{}", *l))));
          v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
          v.push(Instr::Nothing(Label::LName(format!("label{}", *l + 1))));
          *l += 2;
        },
        Op2::Gt => {
          v.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si * 8)));
          v.push(Instr::Jg(Label::LName(format!("label{}", *l)))); // smaller, return true
          v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
          v.push(Instr::Jmp(Label::LName(format!("label{}", *l + 1)))); // is not smaller, jmp out
          v.push(Instr::Nothing(Label::LName(format!("label{}", *l))));
          v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
          v.push(Instr::Nothing(Label::LName(format!("label{}", *l + 1))));
          *l += 2;
        },
        Op2::Ge => {
          v.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si * 8)));
          v.push(Instr::Jge(Label::LName(format!("label{}", *l)))); // smaller equal, return true
          v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
          v.push(Instr::Jmp(Label::LName(format!("label{}", *l + 1)))); // is not smaller equal, jmp out
          v.push(Instr::Nothing(Label::LName(format!("label{}", *l))));
          v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
          v.push(Instr::Nothing(Label::LName(format!("label{}", *l + 1))));
          *l += 2;
        },
        Op2::Le => {
          v.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si * 8)));
          v.push(Instr::Jle(Label::LName(format!("label{}", *l)))); // greater equal, return true
          v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
          v.push(Instr::Jmp(Label::LName(format!("label{}", *l + 1)))); // is not greater equal, jmp out
          v.push(Instr::Nothing(Label::LName(format!("label{}", *l))));
          v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
          v.push(Instr::Nothing(Label::LName(format!("label{}", *l + 1))));
          *l += 2;
        },
        Op2::Eq => {
          v.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si * 8)));
          v.push(Instr::Je(Label::LName(format!("label{}", *l)))); // equal, return true
          v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
          v.push(Instr::Jmp(Label::LName(format!("label{}", *l + 1)))); // is not equal, jmp out
          v.push(Instr::Nothing(Label::LName(format!("label{}", *l))));
          v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
          v.push(Instr::Nothing(Label::LName(format!("label{}", *l + 1))));
          *l += 2;
        },
      }
    },
    Expr::Let(vec, body) => {
      let mut nenv = env.clone();
      let mut nsi = si;
      for (x, e) in vec{
        if x == "let" || x == "add1" || x == "sub1" || x == "true" || x == "false" || x == "set!" || x == "loop" || x == "break" || x == "if" || x == "block" || x == "input" {
          panic!("keyword");
        }
        if nenv.contains_key(x) && !env.contains_key(x) {
          panic!("Duplicate binding");
        }
        v.extend(compile_to_instrs(e, nsi, &nenv, l, bl));
        nenv = nenv.update(x.to_string(), nsi * 8);
        v.push(Instr::IMov(Val::RegOffset(Reg::RSP, nsi * 8), Val::Reg(Reg::RAX)));
        nsi += 1;
      };
      v.extend(compile_to_instrs(body, nsi, &nenv, l, bl));
    },
    Expr::Id(s) => {
      if s == "let" || s == "add1" || s == "sub1" || s == "true" || s == "false" || s == "set!" || s == "loop" || s == "break" || s == "if" || s == "block" {
        panic!("keyword");
      }
      if !env.contains_key(s) {
        panic!("Unbound variable identifier {}", s);
      }
      v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, *env.get(s).unwrap())));
    },
    Expr::Set(s, e) => {
      if s == "let" || s == "add1" || s == "sub1" || s == "true" || s == "false" || s == "set!" || s == "loop" || s == "break" || s == "if" || s == "block" || s == "input" {
        panic!("keyword");
      }
      if !env.contains_key(s) {
        panic!("Unbound variable identifier {}", s);
      }
      v.extend(compile_to_instrs(e, si, env, l, bl));
      v.push(Instr::IMov(Val::RegOffset(Reg::RSP, *env.get(s).unwrap()), Val::Reg(Reg::RAX)));
    },
    Expr::If(e1, e2, e3) => {
      v.extend(compile_to_instrs(e1, si, env, l, bl));
      // v.push(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)));
      // v.push(Instr::Je(Label::TYPEERROR)); // if not bool, jump to err
      let v2 = compile_to_instrs(e2, si, env, l, bl);
      let v3 = compile_to_instrs(e3, si, env, l, bl);
      v.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::Imm(1)));
      v.push(Instr::Je(Label::LName(format!("label{}", *l)))); // if false, jmp to else
      v.extend(v2);
      v.push(Instr::Jmp(Label::LName(format!("label{}", *l + 1)))); // jmp to endif
      v.push(Instr::Nothing(Label::LName(format!("label{}", *l))));
      v.extend(v3);
      v.push(Instr::Nothing(Label::LName(format!("label{}", *l + 1))));
      *l += 2;
    },
    Expr::Block(blk) => {
      for b in blk {
          v.extend(compile_to_instrs(b, si, env, l, bl)); 
      }
    },
    Expr::Loop(body) => {
      let curr_l = *l;
      *l += 2;
      v.push(Instr::Nothing(Label::LName(format!("label{}", curr_l))));
      v.extend(compile_to_instrs(body, si, env, l, curr_l + 1));
      v.push(Instr::Jmp(Label::LName(format!("label{}", curr_l))));
      v.push(Instr::Nothing(Label::LName(format!("label{}", curr_l + 1))));
    },
    Expr::Break(body) => {
      if bl == -1 {
        panic!("break");
      }
      v.extend(compile_to_instrs(body, si, env, l, bl));
      v.push(Instr::Jmp(Label::LName(format!("label{}", bl))));
    },
  }
  v
}

fn instr_to_str(instr: &Instr) -> String {
  match instr {
      Instr::IMov(v1, v2) => format!("\nmov {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::IAdd(v1, v2) => format!("\nadd {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::ISub(v1, v2) => format!("\nsub {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::IMul(v1, v2) => format!("\nimul {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::Test(v1, v2) => format!("\ntest {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::Cmp(v1, v2) => format!("\ncmp {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::Sar(v1, v2) => format!("\nsar {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::Xor(v1, v2) => format!("\nxor {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::Jmp(l1) => format!("\njmp {}", label_to_str(l1)),
      Instr::Jne(l1) => format!("\njne {}", label_to_str(l1)),
      Instr::Je(l1) => format!("\nje {}", label_to_str(l1)),
      Instr::Jg(l1) => format!("\njg {}", label_to_str(l1)),
      Instr::Jl(l1) => format!("\njl {}", label_to_str(l1)),
      Instr::Jge(l1) => format!("\njge {}", label_to_str(l1)),
      Instr::Jle(l1) => format!("\njle {}", label_to_str(l1)),
      Instr::Jo(l1) => format!("\njo {}", label_to_str(l1)),
      Instr::Nothing(l1) => format!("\n{}:", label_to_str(l1)),
  }
}

fn val_to_str(val: &Val) -> String {
  match val {
      Val::Imm(num) => format!("{}", *num),
      Val::Reg(Reg::RAX) => format!("rax"),
      Val::Reg(Reg::RBX) => format!("rbx"),
      Val::Reg(Reg::RSP) => format!("rsp"),
      Val::Reg(Reg::RDI) => format!("rdi"),
      Val::RegOffset(Reg::RSP, offset) => format!("[rsp - {}]", offset),
      _ => panic!("cannot convert val to str"),
  }
}

fn label_to_str(label: &Label) -> String {
  match label {
    Label::TYPEERROR => format!("TYPEERROR"),
    Label::OVERFLOW => format!("OVERFLOW"),
    Label::LName(st) => st.to_string(),
  }
}

fn compile(e: &Expr) -> String {
  let mut s = String::new();
  let v = compile_to_instrs(e, 2, &HashMap::new(), &mut 0, -1);
  for i in v {
      s.push_str(&instr_to_str(&i));
  }
  s
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    // You will make result hold the result of actually compiling
    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let s_expr = match parse(&in_contents) {
        Ok(expr) => expr,
        Err(_) => panic!("Invalid"),
    };
    
    let expr = parse_expr(&s_expr);

    let result = compile(&expr);

    let asm_program = format!(
        "
section .text
extern snek_error
global our_code_starts_here
our_code_starts_here:
  {}
  ret
TYPEERROR:
  mov rdi, 1
  push rsp
  call snek_error
OVERFLOW:
  mov rdi, 2
  push rsp
  call snek_error
",
        result
    );

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}
