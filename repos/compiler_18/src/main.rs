use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::Atom::*;
use sexp::*;

use im::HashMap;
use im::hashset;
use std::collections::HashSet;

#[derive(Debug)]
enum Val {
    Reg(Reg),
    Imm(i64),
    RegOffset(Reg, i64),
}

#[derive(Debug)]
enum Reg {
    RAX,
    RSP,
    RBX,
    RDI,
    RCX,
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    Cmp(Val, Val),
    Je(String),
    Jmp(String),
    Jno(String),
    Label(String),
    CMove(Val, Val),
    CMovg(Val, Val),
    CMovl(Val, Val),
    CMovge(Val, Val),
    CMovle(Val, Val),
    And(Val, Val),
    Xor(Val, Val),
    Error(i64),
    ISar(Val, Val),
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
    Less,
    GreaterEqual,
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
  let invalid_names: im::HashSet<String> = hashset!["add1".to_string(), "sub1".to_string(), "let".to_string(),
  "block".to_string(), "set!".to_string(), "if".to_string(), "break".to_string(), "loop".to_string(),
  "true".to_string(), "false".to_string(), "isbool".to_string(), "isnum".to_string(), "input".to_string()];
  match s {
    Sexp::Atom(I(n)) => Expr::Number(i64::try_from(*n).unwrap()),
    Sexp::Atom(S(str)) => {
      if str.to_string() == "true" {
        Expr::Boolean(true)
      }
      else if str.to_string() == "false" {
        Expr::Boolean(false)
      }
      else if str.to_string() != "input" && invalid_names.contains(str) {
        panic!("invalid use of keyword {str}");
      }
      else {
        Expr::Id(str.to_string())
      }
    }
    Sexp::List(vec) =>
      match &vec[..] {
        [Sexp::Atom(S(op)), e] if op == "add1" => {
          Expr::UnOp(Op1::Add1, Box::new(parse_expr(e)))},
        [Sexp::Atom(S(op)), e] if op == "sub1" =>
          Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e))),
        [Sexp::Atom(S(op)), e] if op == "isnum" =>
          Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e))),
        [Sexp::Atom(S(op)), e] if op == "isbool" =>
          Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e))),
        [Sexp::Atom(S(op)), e1, e2] if op == "+" =>
          Expr::BinOp(Op2::Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
        [Sexp::Atom(S(op)), e1, e2] if op == "-" =>
          Expr::BinOp(Op2::Minus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
        [Sexp::Atom(S(op)), e1, e2] if op == "*" =>
          Expr::BinOp(Op2::Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
        [Sexp::Atom(S(op)), e1, e2] if op == "=" =>
          Expr::BinOp(Op2::Equal, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
        [Sexp::Atom(S(op)), e1, e2] if op == ">" =>
          Expr::BinOp(Op2::Greater, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
        [Sexp::Atom(S(op)), e1, e2] if op == "<" =>
          Expr::BinOp(Op2::Less, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
        [Sexp::Atom(S(op)), e1, e2] if op == ">=" =>
          Expr::BinOp(Op2::GreaterEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
        [Sexp::Atom(S(op)), e1, e2] if op == "<=" =>
          Expr::BinOp(Op2::LessEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
        [Sexp::Atom(S(op)), e1, e2, e3] if op == "if" =>
          Expr::If(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)), Box::new(parse_expr(e3))),
        [Sexp::Atom(S(op)), Sexp::Atom(S(var)), e3] if op == "set!" => {
          if invalid_names.contains(var) {
            panic!("keyword cannot be used as a variable name");
          }
          return Expr::Set(var.to_string(), Box::new(parse_expr(e3)));
        },
        [Sexp::Atom(S(op)), e1] if op == "loop" =>
          Expr::Loop(Box::new(parse_expr(e1))),
        [Sexp::Atom(S(op)), e1] if op == "break" =>
          Expr::Break(Box::new(parse_expr(e1))),
        // For code on how to match variable length array (e1 @ .. syntax) see:
        // https://doc.rust-lang.org/rust-by-example/flow_control/match/destructuring/destructure_slice.html
        [Sexp::Atom(S(op)), e1 @ ..] if op == "block" => {
          let mut v = Vec::new();
          for exp in e1 {
            v.push(parse_expr(exp));
          }
          if v.is_empty() {
            panic!("Invalid, empty block");
          }
          return Expr::Block(v);
        },
        [Sexp::Atom(S(op)), e1, e2] if op == "let" => {
          match e1 {
            Sexp::List(vec2) => {
              if vec2.is_empty() {
                panic!("Invalid. Empty let expression");
              }
              let mut v = Vec::new();
              for e3 in vec2 {
                  match e3 {
                    Sexp::List(vec3) => {
                      match &vec3[..] {
                        //e2 is body e4 is what to set var to
                        [Sexp::Atom(S(s)), e4] => {
                          if invalid_names.contains(s) {
                            panic!("keyword cannot be used as a variable name");
                          }
                          let t = (s.to_string(), parse_expr(e4));
                          v.push(t);
                        }
                        _ => panic!("Invalid let binding")
                      }
                    },
                    _ => panic!("Invalid let bindings")
                  }
                }
                return Expr::Let(v, Box::new(parse_expr(e2)));
            }
            _ => panic!("Invalid let bindings")
          }
        }
        _ => panic!("Invalid Sexp Vec")
      }
      _ => panic!("Invalid Sexp")
  }
}

fn compile_to_instrs(e: &Expr, si: i64, env: &HashMap<String, i64>, l: &mut i32, break_label: &str) -> Vec<Instr> {
  match e {
    Expr::Number(n) => {
      if *n > 4611686018427387903 || *n < -4611686018427387904 {
        panic!("Invalid value. overflow");
      }
      return vec!(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n<<1)));
    },
    Expr::Boolean(boolean) => {
      if *boolean {
        return vec!(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
      }
      else {
        return vec!(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)))
      }
    }
    Expr::Id(s) => {
      if s == "input" {
        return vec!(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI)));
      }
      else if !env.contains_key(s) {
        panic!("Unbound variable identifier {}", s);
      }
      return vec!(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, *env.get(s).unwrap())))
    },
    Expr::UnOp(Op1::Add1, subexpr) => {
      let mut sub_instrs = compile_to_instrs(subexpr, si, env, l, break_label);
      sub_instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(2)));
      let no_overflow_label = new_label(l, "no_overflow_error");
      sub_instrs.push(Instr::Jno(no_overflow_label.clone()));
      sub_instrs.push(Instr::Error(10));
      sub_instrs.push(Instr::Label(no_overflow_label.clone()));
      return sub_instrs;
    },
    Expr::UnOp(Op1::Sub1, subexpr) => {
      let mut sub_instrs = compile_to_instrs(subexpr, si, env, l, break_label);
      sub_instrs.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(2)));
      let no_overflow_label = new_label(l, "no_overflow_error");
      sub_instrs.push(Instr::Jno(no_overflow_label.clone()));
      sub_instrs.push(Instr::Error(10));
      sub_instrs.push(Instr::Label(no_overflow_label.clone()));
      return sub_instrs;
    },
    Expr::UnOp(Op1::IsNum, subexpr) => {
      let mut sub_instrs = compile_to_instrs(subexpr, si, env, l, break_label);
      sub_instrs.push(Instr::And(Val::Reg(Reg::RAX), Val::Imm(1)));
      sub_instrs.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::Imm(0)));
      sub_instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
      sub_instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
      sub_instrs.push(Instr::CMove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
      return sub_instrs;
    },
    Expr::UnOp(Op1::IsBool, subexpr) => {
      let mut sub_instrs = compile_to_instrs(subexpr, si, env, l, break_label);
      sub_instrs.push(Instr::And(Val::Reg(Reg::RAX), Val::Imm(1)));
      sub_instrs.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::Imm(1)));
      sub_instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
      sub_instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
      sub_instrs.push(Instr::CMove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
      return sub_instrs;
    },
    Expr::BinOp(Op2::Plus, e1, e2) => {
      return compile_arithmetic(e1, e2, si, env, l, "plus", break_label);
    },
    Expr::BinOp(Op2::Minus, e1, e2) => {
      return compile_arithmetic(e1, e2, si, env, l, "minus", break_label);
    },
    // Idea to use IMul obtained from the following post: https://edstem.org/us/courses/38748/discussion/2950595
    Expr::BinOp(Op2::Times, e1, e2) => {
      return compile_arithmetic(e1, e2, si, env, l, "times", break_label);
    },
    Expr::BinOp(Op2::Equal, e1, e2) => {
      return compile_equal(e1, e2, si, env, l, break_label);
    },
    Expr::BinOp(Op2::Greater, e1, e2) => {
      return compile_comparisons(e1, e2, si, env, l, "greater", break_label);
    },
    Expr::BinOp(Op2::Less, e1, e2) => {
      return compile_comparisons(e1, e2, si, env, l, "less", break_label);
    },
    Expr::BinOp(Op2::LessEqual, e1, e2) => {
      return compile_comparisons(e1, e2, si, env, l, "less_equal", break_label);
    },
    Expr::BinOp(Op2::GreaterEqual, e1, e2) => {
      return compile_comparisons(e1, e2, si, env, l, "greater_equal", break_label);
    },
    Expr::Let(v, body) => {
      let mut sub_instrs = Vec::new();
      let mut new_si = si;
      let mut new_env = env.clone();
      let mut duplicate_binding: HashSet<String> = HashSet::new();
      for (var, binding) in v {
        if !duplicate_binding.insert(var.to_string()) {
          panic!("Duplicate binding");
        }
        sub_instrs.append(&mut compile_to_instrs(binding, new_si, &new_env, l, break_label));
        new_env.insert(var.to_string(), new_si*8);
        sub_instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, new_si*8), Val::Reg(Reg::RAX)));
        new_si += 1;
      }
      let mut body_instrs = compile_to_instrs(body, new_si, &new_env, l, break_label);
      sub_instrs.append(&mut body_instrs);
      return sub_instrs;
    }
    Expr::If(cond, e1, e2) => {
      let mut instrs = Vec::new();
      let end_label = new_label(l, "ifend");
      let else_label = new_label(l, "ifelse");
      instrs.append(&mut compile_to_instrs(cond, si, env, l, break_label));
      instrs.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::Imm(1)));
      instrs.push(Instr::Je(else_label.clone()));
      instrs.append(&mut compile_to_instrs(e1, si, env, l, break_label));
      instrs.push(Instr::Jmp(end_label.clone()));
      instrs.push(Instr::Label(else_label.clone()));
      instrs.append(&mut compile_to_instrs(e2, si, env, l, break_label));
      instrs.push(Instr::Label(end_label.clone()));
      return instrs;
    },
    Expr::Set(var, e1) => {
      if !env.contains_key(var) {
        panic!("Unbound variable identifier {var}");
      }
      let mut sub_instrs = Vec::new();
      sub_instrs.append(&mut compile_to_instrs(e1, si, env, l, break_label));
      sub_instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, *env.get(var).unwrap()), Val::Reg(Reg::RAX)));
      return sub_instrs;
    },
    Expr::Loop(e1) => {
      let loop_label = new_label(l, "loop");
      let end_label = new_label(l, "loopend");
      let mut instrs = Vec::new();
      instrs.push(Instr::Label(loop_label.clone()));
      instrs.append(&mut compile_to_instrs(e1, si, env, l, &end_label));
      instrs.push(Instr::Jmp(loop_label.clone()));
      instrs.push(Instr::Label(end_label.clone()));
      return instrs;
    },
    Expr::Break(e1) => {
      if break_label == "" {
        panic!("break without loop");
      }
      let mut instrs = Vec::new();
      instrs.append(&mut compile_to_instrs(e1, si, env, l, break_label));
      instrs.push(Instr::Jmp(break_label.to_string()));
      return instrs;
    }
    Expr::Block(exprs) => {
      let mut instrs = Vec::new();
      for exp in exprs {
        instrs.append(&mut compile_to_instrs(exp, si, env, l, break_label));
      }
      return instrs;
    }
  }
}

fn compile_arithmetic(e1: &Expr, e2: &Expr, si: i64, env: &HashMap<String, i64>, l: &mut i32, op: &str, break_label: &str ) -> Vec<Instr> {
  let mut sub_instrs2 = compile_to_instrs(e2, si, env, l, break_label);
  sub_instrs2.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Reg(Reg::RAX)));
  sub_instrs2.push(Instr::And(Val::Reg(Reg::RCX), Val::Imm(1)));
  sub_instrs2.push(Instr::Cmp(Val::Reg(Reg::RCX), Val::Imm(1)));
  let invalid_operands_label = new_label(l, "invalid_operands_error");
  sub_instrs2.push(Instr::Je(invalid_operands_label.clone()));
  sub_instrs2.push(Instr::IMov(Val::RegOffset(Reg::RSP, si*8), Val::Reg(Reg::RAX)));
  let mut sub_instrs1 = compile_to_instrs(e1, si + 1, env, l, break_label);
  sub_instrs2.append(&mut sub_instrs1);
  sub_instrs2.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Reg(Reg::RAX)));
  sub_instrs2.push(Instr::And(Val::Reg(Reg::RCX), Val::Imm(1)));
  sub_instrs2.push(Instr::Cmp(Val::Reg(Reg::RCX), Val::Imm(1)));
  sub_instrs2.push(Instr::Je(invalid_operands_label.clone()));
  if op == "times" {
    sub_instrs2.push(Instr::ISar(Val::Reg(Reg::RAX), Val::Imm(1)));
    sub_instrs2.push(Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si*8)));
  }
  else if op == "plus" {
    sub_instrs2.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si*8)));
  }
  else {
    sub_instrs2.push(Instr::ISub(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si*8)));
  }
  let no_overflow_label = new_label(l, "no_overflow_error");
  sub_instrs2.push(Instr::Jno(no_overflow_label.clone()));
  sub_instrs2.push(Instr::Error(10));
  sub_instrs2.push(Instr::Label(invalid_operands_label.clone()));
  sub_instrs2.push(Instr::Error(2));
  sub_instrs2.push(Instr::Label(no_overflow_label.clone()));
  return sub_instrs2;
}

fn compile_comparisons(e1: &Expr, e2: &Expr, si: i64, env: &HashMap<String, i64>, l: &mut i32, op: &str, break_label: &str ) -> Vec<Instr> {
  let mut sub_instrs2 = compile_to_instrs(e2, si, env, l, break_label);
  sub_instrs2.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Reg(Reg::RAX)));
  sub_instrs2.push(Instr::And(Val::Reg(Reg::RCX), Val::Imm(1)));
  sub_instrs2.push(Instr::Cmp(Val::Reg(Reg::RCX), Val::Imm(1)));
  let invalid_operands_label = new_label(l, "invalid_operands_error");
  sub_instrs2.push(Instr::Je(invalid_operands_label.clone()));
  sub_instrs2.push(Instr::IMov(Val::RegOffset(Reg::RSP, si*8), Val::Reg(Reg::RAX)));
  let mut sub_instrs1 = compile_to_instrs(e1, si + 1, env, l, break_label);
  sub_instrs2.append(&mut sub_instrs1);

  sub_instrs2.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Reg(Reg::RAX)));
  sub_instrs2.push(Instr::And(Val::Reg(Reg::RCX), Val::Imm(1)));
  sub_instrs2.push(Instr::Cmp(Val::Reg(Reg::RCX), Val::Imm(1)));
  sub_instrs2.push(Instr::Je(invalid_operands_label.clone()));

  sub_instrs2.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si*8)));
  sub_instrs2.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
  sub_instrs2.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
  if op == "greater" {
    sub_instrs2.push(Instr::CMovg(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
  }
  else if op == "less" {
    sub_instrs2.push(Instr::CMovl(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
  }
  else if op == "less_equal" {
    sub_instrs2.push(Instr::CMovle(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
  }
  else {
    sub_instrs2.push(Instr::CMovge(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
  }
  let no_comparison_error_label = new_label(l, "no_comparison_error_label");
  sub_instrs2.push(Instr::Jmp(no_comparison_error_label.clone()));
  sub_instrs2.push(Instr::Label(invalid_operands_label.clone()));
  sub_instrs2.push(Instr::Error(2));
  sub_instrs2.push(Instr::Label(no_comparison_error_label.clone()));
  return sub_instrs2;
}

fn compile_equal(e1: &Expr, e2: &Expr, si: i64, env: &HashMap<String, i64>, l: &mut i32, break_label: &str ) -> Vec<Instr> {
  let mut sub_instrs2 = compile_to_instrs(e2, si, env, l, break_label);
  sub_instrs2.push(Instr::IMov(Val::RegOffset(Reg::RSP, si*8), Val::Reg(Reg::RAX)));
  sub_instrs2.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
  sub_instrs2.push(Instr::And(Val::Reg(Reg::RBX), Val::Imm(1)));
  let mut sub_instrs1 = compile_to_instrs(e1, si + 1, env, l, break_label);
  let valid_operands_label = new_label(l, "valid_operands");
  sub_instrs2.append(&mut sub_instrs1);

  // Checks for type match
  sub_instrs2.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Reg(Reg::RAX)));
  sub_instrs2.push(Instr::And(Val::Reg(Reg::RCX), Val::Imm(1)));
  sub_instrs2.push(Instr::Xor(Val::Reg(Reg::RBX), Val::Reg(Reg::RCX)));
  sub_instrs2.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(0)));
  sub_instrs2.push(Instr::Je(valid_operands_label.clone()));
  sub_instrs2.push(Instr::Error(2));
  sub_instrs2.push(Instr::Label(valid_operands_label.clone()));

  sub_instrs2.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si*8)));
  sub_instrs2.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
  sub_instrs2.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
  sub_instrs2.push(Instr::CMove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
  return sub_instrs2;
}


fn new_label(l: &mut i32, s: &str) -> String {
  let current = *l;
  *l += 1;
  format!("{s}_{current}")
}

fn instr_to_str(i: &Instr) -> String {
  match i {
    Instr::IMov(v1, v2) => "\nmov ".to_string() + &val_to_str(v1) + ", " +  &val_to_str(v2),
    Instr::IAdd(v1, v2) => "\nadd ".to_string() + &val_to_str(v1) + ", " + &val_to_str(v2),
    Instr::ISub(v1, v2) => "\nsub ".to_string() + &val_to_str(v1) + ", " + &val_to_str(v2),
    Instr::IMul(v1, v2) => "\nimul ".to_string() + &val_to_str(v1) + ", " + &val_to_str(v2),
    Instr::ISar(v1, v2) => "\nsar ".to_string() + &val_to_str(v1) + ", " + &val_to_str(v2),
    Instr::Cmp(v1, v2) => "\ncmp ".to_string() + &val_to_str(v1) + ", " + &val_to_str(v2),
    Instr::And(v1, v2) => "\nand ".to_string() + &val_to_str(v1) + ", " + &val_to_str(v2),
    Instr::Xor(v1, v2) => "\nxor ".to_string() + &val_to_str(v1) + ", " + &val_to_str(v2),
    Instr::Je(v1) => "\nje ".to_string() + v1,
    Instr::Jmp(v1) => "\njmp ".to_string() + v1,
    Instr::Jno(v1) => "\njno ".to_string() + v1,
    Instr::Label(s) => "\n ".to_string() + s + ":",
    Instr::Error(s) => "\nmov rdi, ".to_string() + &s.to_string() +  &"\npush rsp ".to_string() + &"\ncall ".to_string() + &"snek_error".to_string(),
    Instr::CMove(v1, v2) => "\ncmove ".to_string() + &val_to_str(v1) + ", " + &val_to_str(v2),
    Instr::CMovg(v1, v2) => "\ncmovg ".to_string() + &val_to_str(v1) + ", " + &val_to_str(v2),
    Instr::CMovl(v1, v2) => "\ncmovl ".to_string() + &val_to_str(v1) + ", " + &val_to_str(v2),
    Instr::CMovge(v1, v2) => "\ncmovge ".to_string() + &val_to_str(v1) + ", " + &val_to_str(v2),
    Instr::CMovle(v1, v2) => "\ncmovle ".to_string() + &val_to_str(v1) + ", " + &val_to_str(v2),
  }
}

fn val_to_str(v: &Val) -> String {
  match v {
    Val::Reg(Reg::RAX) => "rax".to_string(),
    Val::Reg(Reg::RSP) => "rsp".to_string(),
    Val::Reg(Reg::RBX) => "rbx".to_string(),
    Val::Reg(Reg::RCX) => "rcx".to_string(),
    Val::Reg(Reg::RDI) => "rdi".to_string(),
    Val::Imm(n) => n.to_string(),
    Val::RegOffset(Reg::RAX, i) => "[".to_string() + &val_to_str(&Val::Reg(Reg::RAX)) + "-" + &val_to_str(&Val::Imm(*i)) + "]",
    Val::RegOffset(Reg::RSP, i) => "[".to_string() + &val_to_str(&Val::Reg(Reg::RSP)) + "-" + &val_to_str(&Val::Imm(*i)) + "]",
    Val::RegOffset(Reg::RBX, i) => "[".to_string() + &val_to_str(&Val::Reg(Reg::RBX)) + "-" + &val_to_str(&Val::Imm(*i)) + "]",
    Val::RegOffset(Reg::RCX, i) => "[".to_string() + &val_to_str(&Val::Reg(Reg::RCX)) + "-" + &val_to_str(&Val::Imm(*i)) + "]",
    Val::RegOffset(Reg::RDI, i) => "[".to_string() + &val_to_str(&Val::Reg(Reg::RDI)) + "-" + &val_to_str(&Val::Imm(*i)) + "]",

  }
}

fn compile(e: &Expr) -> String {
  let env = HashMap::new();
  let break_label = "";
  let instrs = compile_to_instrs(e, 2, &env, &mut 0, &break_label);
  let mut output = String::new();
  for instr in instrs {
    output.push_str(&instr_to_str(&instr));
  }
  return output;
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    // You will make result hold the result of actually compiling
    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;
    // Idea to check sexp for error and how to do so taken from: https://edstem.org/us/courses/38748/discussion/2963326
    let sexp_result = parse(&in_contents);
    if sexp_result.is_err() {
      panic!("Invalid Sexp");
    }
    let expr = parse_expr(&sexp_result.unwrap());
    let result = compile(&expr);

    let asm_program = format!(
        "
section .text
extern snek_error
global our_code_starts_here
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
