use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::Atom::*;
use sexp::*;

#[derive(Debug)]
enum Val {
    Reg(Reg),
    Imm(i64),
    RegOffset(Reg, i64),
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
    Sar(Val, Val),
    Cmp(Val, Val),
    CMove(Val, Val),
    CMovl(Val, Val),
    CMovg(Val, Val),
    CMovle(Val, Val),
    CMovge(Val, Val),
    JE(i32),
    JEErr(i32),
    JNeErr(i32),
    Jmp(i32),
    Label(i32),
    JO(i32),
    And(Val, Val),
}

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
      Sexp::Atom(I(n)) => {
        let num = i64::try_from(*n).expect("Invalid");
        if num > (i64::pow(2,62) - 1) || num < (0 - i64::pow(2,62)) {
          panic!("Invalid")
        } else {
          Expr::Number((num) << 1)
        }
      },
      Sexp::Atom(S(str)) if str == "true" => Expr::Boolean(true),
      Sexp::Atom(S(str)) if str == "false" => Expr::Boolean(false),
      Sexp::Atom(S(str)) => Expr::Id(String::try_from(str).expect("Invalid")),
      Sexp::List(vec) => {
          match &vec[..] {
              [Sexp::Atom(S(op)), e] if op == "add1" => Expr::UnOp(Op1::Add1, Box::new(parse_expr(e))),
              [Sexp::Atom(S(op)), e] if op == "sub1" => Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e))),
              [Sexp::Atom(S(op)), e] if op == "isnum" => Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e))),
              [Sexp::Atom(S(op)), e] if op == "isbool" => Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e))),
              [Sexp::Atom(S(op)), Sexp::List(binds), e] if op == "let" && !binds.is_empty() => Expr::Let(binds.iter().map(parse_bind).collect(), Box::new(parse_expr(e))),
              [Sexp::Atom(S(op)), e1, e2, e3] if op == "if" => Expr::If(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)), Box::new(parse_expr(e3))),
              [Sexp::Atom(S(op)), e1, e2] if op == "+" => Expr::BinOp(Op2::Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
              [Sexp::Atom(S(op)), e1, e2] if op == "-" => Expr::BinOp(Op2::Minus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
              [Sexp::Atom(S(op)), e1, e2] if op == "*" => Expr::BinOp(Op2::Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
              [Sexp::Atom(S(op)), e1, e2] if op == "<" => Expr::BinOp(Op2::Less, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
              [Sexp::Atom(S(op)), e1, e2] if op == "<=" => Expr::BinOp(Op2::LessEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
              [Sexp::Atom(S(op)), e1, e2] if op == ">" => Expr::BinOp(Op2::Greater, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
              [Sexp::Atom(S(op)), e1, e2] if op == ">=" => Expr::BinOp(Op2::GreaterEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
              [Sexp::Atom(S(op)), e1, e2] if op == "=" => Expr::BinOp(Op2::Equal, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
              [Sexp::Atom(S(op)), e] if op == "loop" => Expr::Loop(Box::new(parse_expr(e))),
              [Sexp::Atom(S(op)), e] if op == "break" => Expr::Break(Box::new(parse_expr(e))),
              [Sexp::Atom(S(op)), Sexp::Atom(S(id)), e] if op == "set!" => Expr::Set(id.to_string(), Box::new(parse_expr(e))),
              _ => {
                if vec.len() > 1 {
                    match &vec[0] {
                        Sexp::Atom(S(op)) if op == "block" => Expr::Block(vec[1..].iter().map(parse_expr).collect()),
                        _ => panic!("Invalid"),
                    }
                } else {
                    panic!("Invalid")
                }
                
              }
          }
      },
      _ => panic!("Invalid"),
  }
}

fn parse_bind(s: &Sexp) -> (String, Expr) {
  match s {
      Sexp::List(vec) => {
          match &vec[..] {
              [Sexp::Atom(S(str)), _] if str == "true" => panic!("Cannot use keyword as variable name"),
              [Sexp::Atom(S(str)), _] if str == "false" => panic!("Cannot use keyword as variable name"),
              [Sexp::Atom(S(str)), _] if str == "input" => panic!("Cannot use keyword as variable name"),
              [Sexp::Atom(S(str)), _] if str == "let" => panic!("Cannot use keyword as variable name"),
              [Sexp::Atom(S(str)), _] if str == "add1" => panic!("Cannot use keyword as variable name"),
              [Sexp::Atom(S(str)), _] if str == "sub1" => panic!("Cannot use keyword as variable name"),
              [Sexp::Atom(S(str)), _] if str == "isnum" => panic!("Cannot use keyword as variable name"),
              [Sexp::Atom(S(str)), _] if str == "isbool" => panic!("Cannot use keyword as variable name"),
              [Sexp::Atom(S(str)), _] if str == "if" => panic!("Cannot use keyword as variable name"),
              [Sexp::Atom(S(str)), _] if str == "block" => panic!("Cannot use keyword as variable name"),
              [Sexp::Atom(S(str)), _] if str == "loop" => panic!("Cannot use keyword as variable name"),
              [Sexp::Atom(S(str)), _] if str == "break" => panic!("Cannot use keyword as variable name"),
              [Sexp::Atom(S(str)), e] => (String::try_from(str).expect("Invalid"), parse_expr(e)),
              _ => panic!("Invalid"),
          }
      },
      _ => panic!("Invalid"),
  }
}

fn compile_to_instrs(e: &Expr, si: i64, env: &im::HashMap<String, i64>, b: i32, l: &mut i32) -> Vec<Instr> {
  match e {
      Expr::Number(n) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n))],
      Expr::Id(s) if s == "input" => {
        vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI))]
      }
      Expr::Boolean(true) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3))],
      Expr::Boolean(false) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))],
      Expr::Id(str) => {
          if !(env.contains_key(str)) {
              panic!("Unbound variable identifier {}", str)
          }
          vec![Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, *env.get(str).unwrap()))]
      },
      Expr::Let(binds, subexpr) => {
          let mut new_env = im::HashMap::new();
          let mut output = vec![];
          let mut new_si = si;
          for (id, val) in binds {
              if new_env.contains_key(id) {
                  panic!("Duplicate binding");
              }
              // let new_env_union = new_env.clone().union(im::HashMap::clone(env));
              //.union() doesn't work as expected here, workaround using difference and intersection
              let new_env_diff = new_env.clone().difference(im::HashMap::clone(env));
              let new_env_int = new_env.clone().intersection(im::HashMap::clone(env));
              let new_env_union = new_env_diff.clone().union(im::HashMap::clone(&new_env_int));
              let mut instrs = compile_to_instrs(val, new_si, &new_env_union, b, l);
              output.append(&mut instrs);
              output.push(Instr::IMov(Val::RegOffset(Reg::RSP, new_si * 8), Val::Reg(Reg::RAX)));
              new_env = new_env.update(id.to_string(), new_si * 8);
              new_si += 1;
          }
          // let new_env_union = new_env.clone().union(im::HashMap::clone(env));
          let new_env_diff = new_env.clone().difference(im::HashMap::clone(env));
          let new_env_int = new_env.clone().intersection(im::HashMap::clone(env));
          let new_env_union = new_env_diff.clone().union(im::HashMap::clone(&new_env_int));
          let mut instrs = compile_to_instrs(subexpr, new_si, &new_env_union, b, l);
          output.append(&mut instrs);
          output
      },
      Expr::If(e1,e2,e3) => {
        let end_label = new_label(l);
        let else_label = new_label(l);
        let mut cond_instrs = compile_to_instrs(e1, si, env, b, l);
        let mut thn_instrs = compile_to_instrs(e2, si, env, b, l);
        let mut els_instrs = compile_to_instrs(e3, si, env, b, l);
        cond_instrs.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::Imm(1)));
        cond_instrs.push(Instr::JE(else_label));
        cond_instrs.append(&mut thn_instrs);
        cond_instrs.push(Instr::Jmp(end_label));
        cond_instrs.push(Instr::Label(else_label));
        cond_instrs.append(&mut els_instrs);
        cond_instrs.push(Instr::Label(end_label));

        cond_instrs
      },
      Expr::UnOp(Op1::Add1, subexpr) => {
          let mut output = compile_to_instrs(subexpr, si, env, b, l);

          output.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
          output.push(Instr::And(Val::Reg(Reg::RBX), Val::Imm(1)));
          output.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(1)));
          output.push(Instr::JEErr(2));

          output.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(2)));
          output.push(Instr::JO(1));
          output
      },
      Expr::UnOp(Op1::Sub1, subexpr) => {
          let mut output = compile_to_instrs(subexpr, si, env, b, l);

          output.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
          output.push(Instr::And(Val::Reg(Reg::RBX), Val::Imm(1)));
          output.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(1)));
          output.push(Instr::JEErr(2));

          output.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(2)));
          output.push(Instr::JO(1));
          output
      },
      Expr::UnOp(Op1::IsBool, subexpr) => {
        let bool_label = new_label(l);
        let num_label = new_label(l);
        let mut output = compile_to_instrs(subexpr, si, env, b, l);
        output.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::Imm(1)));
        output.push(Instr::JE(bool_label));
        output.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::Imm(3)));
        output.push(Instr::JE(bool_label));
        output.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
        output.push(Instr::Jmp(num_label));
        output.push(Instr::Label(bool_label));
        output.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
        output.push(Instr::Label(num_label));
        output
      },
      Expr::UnOp(Op1::IsNum, subexpr) => {
        let bool_label = new_label(l);
        let num_label = new_label(l);
        let mut output = compile_to_instrs(subexpr, si, env, b, l);
        output.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::Imm(1)));
        output.push(Instr::JE(bool_label));
        output.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::Imm(3)));
        output.push(Instr::JE(bool_label));
        output.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
        output.push(Instr::Jmp(num_label));
        output.push(Instr::Label(bool_label));
        output.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
        output.push(Instr::Label(num_label));
        output
      },
      Expr::BinOp(Op2::Plus, e1, e2) => {
          let mut e1_instrs = compile_to_instrs(e1, si, env, b, l);
          let mut e2_instrs = compile_to_instrs(e2, si + 1, env, b, l);
          let stack_offset = si * 8;

          e1_instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
          e1_instrs.push(Instr::And(Val::Reg(Reg::RBX), Val::Imm(1)));
          e1_instrs.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(1)));
          e1_instrs.push(Instr::JEErr(2));

          e1_instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
          e1_instrs.append(&mut e2_instrs);

          e1_instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
          e1_instrs.push(Instr::And(Val::Reg(Reg::RBX), Val::Imm(1)));
          e1_instrs.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(1)));
          e1_instrs.push(Instr::JEErr(2));

          e1_instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
          e1_instrs.push(Instr::JO(1));
          e1_instrs
      },
      Expr::BinOp(Op2::Minus, e1, e2) => {
          let mut e1_instrs = compile_to_instrs(e1, si + 1, env, b, l);
          let mut e2_instrs = compile_to_instrs(e2, si, env, b, l);
          let stack_offset = si * 8;

          e2_instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
          e2_instrs.push(Instr::And(Val::Reg(Reg::RBX), Val::Imm(1)));
          e2_instrs.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(1)));
          e2_instrs.push(Instr::JEErr(2));

          e2_instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
          e2_instrs.append(&mut e1_instrs);

          e2_instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
          e2_instrs.push(Instr::And(Val::Reg(Reg::RBX), Val::Imm(1)));
          e2_instrs.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(1)));
          e2_instrs.push(Instr::JEErr(2));

          e2_instrs.push(Instr::ISub(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
          e2_instrs.push(Instr::JO(1));
          e2_instrs
      },
      Expr::BinOp(Op2::Times, e1, e2) => {
          let mut e1_instrs = compile_to_instrs(e1, si, env, b, l);
          let mut e2_instrs = compile_to_instrs(e2, si + 1, env, b, l);
          let stack_offset = si * 8;

          e1_instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
          e1_instrs.push(Instr::And(Val::Reg(Reg::RBX), Val::Imm(1)));
          e1_instrs.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(1)));
          e1_instrs.push(Instr::JEErr(2));

          e1_instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
          e1_instrs.append(&mut e2_instrs);

          e1_instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
          e1_instrs.push(Instr::And(Val::Reg(Reg::RBX), Val::Imm(1)));
          e1_instrs.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(1)));
          e1_instrs.push(Instr::JEErr(2));

          e1_instrs.push(Instr::Sar(Val::Reg(Reg::RAX), Val::Imm(1)));
          e1_instrs.push(Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
          e1_instrs.push(Instr::JO(1));
          e1_instrs
      },
      Expr::BinOp(Op2::Equal, e1, e2) => {
        let mut e1_instrs = compile_to_instrs(e1, si, env, b, l);
        let mut e2_instrs = compile_to_instrs(e2, si + 2, env, b, l);
        let stack_offset = si * 8;

        e1_instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
        e1_instrs.push(Instr::And(Val::Reg(Reg::RBX), Val::Imm(1)));
        e1_instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset + 8), Val::Reg(Reg::RBX)));

        e1_instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
        e1_instrs.append(&mut e2_instrs);

        e1_instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
        e1_instrs.push(Instr::And(Val::Reg(Reg::RBX), Val::Imm(1)));
        e1_instrs.push(Instr::Cmp(Val::RegOffset(Reg::RSP, stack_offset + 8), Val::Reg(Reg::RBX)));
        e1_instrs.push(Instr::JNeErr(2));

        e1_instrs.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
        e1_instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
        e1_instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
        e1_instrs.push(Instr::CMove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
        e1_instrs
      },
      Expr::BinOp(Op2::Less, e1, e2) => {
        let mut e1_instrs = compile_to_instrs(e1, si, env, b, l);
        let mut e2_instrs = compile_to_instrs(e2, si + 1, env, b, l);
        let stack_offset = si * 8;

        e1_instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
        e1_instrs.push(Instr::And(Val::Reg(Reg::RBX), Val::Imm(1)));
        e1_instrs.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(1)));
        e1_instrs.push(Instr::JEErr(2));

        e1_instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
        e1_instrs.append(&mut e2_instrs);

        e1_instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
        e1_instrs.push(Instr::And(Val::Reg(Reg::RBX), Val::Imm(1)));
        e1_instrs.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(1)));
        e1_instrs.push(Instr::JEErr(2));

        e1_instrs.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
        e1_instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
        e1_instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
        e1_instrs.push(Instr::CMovg(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
        e1_instrs
      },
      Expr::BinOp(Op2::Greater, e1, e2) => {
        let mut e1_instrs = compile_to_instrs(e1, si, env, b, l);
        let mut e2_instrs = compile_to_instrs(e2, si + 1, env, b, l);
        let stack_offset = si * 8;

        e1_instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
        e1_instrs.push(Instr::And(Val::Reg(Reg::RBX), Val::Imm(1)));
        e1_instrs.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(1)));
        e1_instrs.push(Instr::JEErr(2));

        e1_instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
        e1_instrs.append(&mut e2_instrs);

        e1_instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
        e1_instrs.push(Instr::And(Val::Reg(Reg::RBX), Val::Imm(1)));
        e1_instrs.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(1)));
        e1_instrs.push(Instr::JEErr(2));

        e1_instrs.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
        e1_instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
        e1_instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
        e1_instrs.push(Instr::CMovl(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
        e1_instrs
      },
      Expr::BinOp(Op2::LessEqual, e1, e2) => {
        let mut e1_instrs = compile_to_instrs(e1, si, env, b, l);
        let mut e2_instrs = compile_to_instrs(e2, si + 1, env, b, l);
        let stack_offset = si * 8;

        e1_instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
        e1_instrs.push(Instr::And(Val::Reg(Reg::RBX), Val::Imm(1)));
        e1_instrs.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(1)));
        e1_instrs.push(Instr::JEErr(2));

        e1_instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
        e1_instrs.append(&mut e2_instrs);

        e1_instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
        e1_instrs.push(Instr::And(Val::Reg(Reg::RBX), Val::Imm(1)));
        e1_instrs.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(1)));
        e1_instrs.push(Instr::JEErr(2));

        e1_instrs.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
        e1_instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
        e1_instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
        e1_instrs.push(Instr::CMovge(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
        e1_instrs
      },
      Expr::BinOp(Op2::GreaterEqual, e1, e2) => {
        let mut e1_instrs = compile_to_instrs(e1, si, env, b, l);
        let mut e2_instrs = compile_to_instrs(e2, si + 1, env, b, l);
        let stack_offset = si * 8;

        e1_instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
        e1_instrs.push(Instr::And(Val::Reg(Reg::RBX), Val::Imm(1)));
        e1_instrs.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(1)));
        e1_instrs.push(Instr::JEErr(2));

        e1_instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
        e1_instrs.append(&mut e2_instrs);

        e1_instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
        e1_instrs.push(Instr::And(Val::Reg(Reg::RBX), Val::Imm(1)));
        e1_instrs.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(1)));
        e1_instrs.push(Instr::JEErr(2));

        e1_instrs.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
        e1_instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
        e1_instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
        e1_instrs.push(Instr::CMovle(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
        e1_instrs
      },
      Expr::Loop(subexpr) => {
        let start_label = new_label(l);
        let end_label = new_label(l);
        let mut instrs = compile_to_instrs(subexpr, si, env, end_label, l);
        let mut output = vec![];
        output.push(Instr::Label(start_label));
        output.append(&mut instrs);
        output.push(Instr::Jmp(start_label));
        output.push(Instr::Label(end_label));
        output
      },
      Expr::Break(subexpr) => {
        if b == -1 {
            panic!("Tried to break while outside of loop")
        }
        let mut output = compile_to_instrs(subexpr, si, env, b, l);
        output.push(Instr::Jmp(b));
        output
      },
      Expr::Set(id, subexpr) => {
        let offset:i64;
        match env.get(id) {
            Some(val) => offset = *val,
            None => panic!("Unbound variable identifier {}", id)
        }
        let mut output = compile_to_instrs(subexpr, si, env, b, l);
        output.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX)));
        output
      },
      Expr::Block(exprs) => {
        let mut output = vec![];
        for expr in exprs {
            let mut instrs = compile_to_instrs(expr, si, env, b, l);
            output.append(&mut instrs);
        }
        output
      }
  }
}

fn instr_to_str(i: &Instr) -> String {
  match i {
      Instr::IMov(v1, v2) => format!("mov {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::IAdd(v1, v2) => format!("add {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::ISub(v1, v2) => format!("sub {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::IMul(v1, v2) => format!("imul {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::Sar(v1, v2) => format!("sar {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::Cmp(v1, v2) => format!("cmp {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::CMove(v1, v2) => format!("cmove {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::CMovl(v1, v2) => format!("cmovl {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::CMovg(v1, v2) => format!("cmovg {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::CMovle(v1, v2) => format!("cmovle {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::CMovge(v1, v2) => format!("cmovge {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::JE(l) => format!("je label_{}",l),
      Instr::JEErr(l) => format!("je error_label_{}",l),
      Instr::JNeErr(l) => format!("jne error_label_{}",l),
      Instr::JO(l) => format!("jo error_label_{}",l),
      Instr::Jmp(l) => format!("jmp label_{}",l),
      Instr::Label(l) => format!("label_{}:",l),
      Instr::And(v1, v2) => format!("and {}, {}", val_to_str(v1), val_to_str(v2)),
  }
}

fn val_to_str(v: &Val) -> String {
  match v {
      Val::Imm(n) => format!("{}", n),
      Val::Reg(Reg::RAX) => format!("rax"),
      Val::Reg(Reg::RBX) => format!("rbx"),
      Val::Reg(Reg::RSP) => format!("rsp"),
      Val::Reg(Reg::RDI) => format!("rdi"),
      Val::RegOffset(Reg::RAX, n) => format!("[rax-{}]", n),
      Val::RegOffset(Reg::RBX, n) => format!("[rbx-{}]", n),
      Val::RegOffset(Reg::RSP, n) => format!("[rsp-{}]", n),
      Val::RegOffset(Reg::RDI, n) => format!("[rdi-{}]", n),
  }
}

fn new_label(l: &mut i32) -> i32 {
    let current = *l;
    *l += 1;
    current
}

fn compile(e: &Expr) -> String {
  let mut output = String::from("");
  let env = im::HashMap::new();
  let mut l = 0;
  let b = -1;
  let instrs = compile_to_instrs(e, 2, &env, b, &mut l);
  for instr in instrs {
      output = output + &instr_to_str(&instr) + "\n";
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

    let s = &parse(&in_contents).expect("Invalid");    
    let expr = parse_expr(s);
    let result = compile(&expr);

    let asm_program = format!(
        "
section .text
extern snek_error
global our_code_starts_here
error_label_1:
mov rdi, 1
push rsp
call snek_error
error_label_2:
mov rdi, 2
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
