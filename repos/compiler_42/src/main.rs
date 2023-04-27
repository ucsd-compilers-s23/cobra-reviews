use std::env;
use std::fmt::format;
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
    IAnd(Val, Val),
    IOr(Val, Val),
    ICmp(Val, Val),
    ICall(String),
    IJe(String),
    IJmp(String),
    ILabel(String),
    IXor(Val, Val),
    ITest(Val, Val),
    IJne(String),
    ICMove(Val, Val),
    ICMovL(Val, Val),
    ICmovLe(Val, Val),
    ICmovGe(Val, Val),
    ICmovG(Val, Val),
    IShl(Val, Val),
    IShr(Val, Val),
    IRet,
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
    Less,
    Greater,
    GreaterEq,
    LessEq,
    Eq,
}

#[derive(Debug)]
enum Expr {
    Number(i64),
    True(bool),
    False(bool),
    input(Box<Expr>),
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    Set(String, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Block(Vec<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
    Print(Box<Expr>),
}

fn parse_expr(s: &Sexp) -> Expr {
  match s {
    Sexp::Atom(I(n)) => Expr::Number(i64::try_from(*n).unwrap()),
    Sexp::Atom(S(s)) => match s.as_str() {
      "true" => Expr::True(true),
      "false" => Expr::False(false),
      "input" => Expr::input(Box::new(Expr::Number(0))),
      _ => Expr::Id(s.to_string()),
    },
    Sexp::List(vec) => {
      let first = &vec[0];
      match first {
        Sexp::Atom(S(s)) => match s.as_str() {
          "let" => {
            let bindings = &vec[1];
            let body = &vec[2];
            match bindings {
              Sexp::List(bindings) => {
                let bindings = bindings
                  .iter()
                  .map(|b| parse_bind(b))
                  .collect::<Vec<(String, Expr)>>();
                Expr::Let(bindings, Box::new(parse_expr(body)))
              }
              _ => panic!("parse error"),
            }
          }
          "add1" => {
            let e = &vec[1];
            Expr::UnOp(Op1::Add1, Box::new(parse_expr(e)))
          }
          "sub1" => {
            let e = &vec[1];
            Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e)))
          }
          "isNum" => {
            let e = &vec[1];
            Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e)))
          }
          "isBool" => {
            let e = &vec[1];
            Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e)))
          }
          "+" => {
            let e1 = &vec[1];
            let e2 = &vec[2];
            Expr::BinOp(Op2::Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
          }
          "-" => {
            let e1 = &vec[1];
            let e2 = &vec[2];
            Expr::BinOp(Op2::Minus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
          }
          "*" => {
            let e1 = &vec[1];
            let e2 = &vec[2];
            Expr::BinOp(Op2::Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
          }
          "<" => {
            let e1 = &vec[1];
            let e2 = &vec[2];
            Expr::BinOp(Op2::Less, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
          }
          ">" => {
            let e1 = &vec[1];
            let e2 = &vec[2];
            Expr::BinOp(Op2::Greater, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
          }
          ">=" => {
            let e1 = &vec[1];
            let e2 = &vec[2];
            Expr::BinOp(Op2::GreaterEq, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
          }
          "<=" => {
            let e1 = &vec[1];
            let e2 = &vec[2];
            Expr::BinOp(Op2::LessEq, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
          }
          "=" => {
            let e1 = &vec[1];
            let e2 = &vec[2];
            Expr::BinOp(Op2::Eq, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
          }
          "if" => {
            let e1 = &vec[1];
            let e2 = &vec[2];
            let e3 = &vec[3];
            Expr::If(
                Box::new(parse_expr(e1)),
                Box::new(parse_expr(e2)),
                Box::new(parse_expr(e3)),
            )
          }
          "block" => {
            let e = &vec[1];
            Expr::Block(vec.iter().skip(1).map(|e| parse_expr(e)).collect())
          }
          "set!" => {
            let id = &vec[1];
            let e = &vec[2];
            Expr::Set(id.to_string(), Box::new(parse_expr(e)))
          }
          "loop" => {
            let e = &vec[1];
            Expr::Loop(Box::new(parse_expr(e)))
          }
          "break" => {
            let e = &vec[1];
            Expr::Break(Box::new(parse_expr(e)))
          }
          "print" => {
            let e = &vec[1];
            Expr::Print(Box::new(parse_expr(e)))
          }
          _ => panic!("parse error"),
        },
        _ => panic!("Invalid"),
      }
    }
    _ => panic!("Invalid"),
  }
}

fn parse_bind(s: &Sexp) -> (String, Expr) {
  match s {
    Sexp::List(vec) => {
      match &vec[..] {
        [Sexp::Atom(S(s)), e] => (s.to_string(), parse_expr(e)),
        _ => panic!("parse error"),
      }
    }
    _ => panic!("parse error"),
  }
}

fn new_label(l: &mut i32, s: &str) -> String {
  let current = *l;
  *l += 1;
  format!("{s}_{current}", s = s, current = current)
}

fn compile_to_instrs(e: &Expr, si: i32, env: &HashMap<String, i32>, l: &mut i32) -> Vec<Instr> {
  match e {
    Expr::Number(n) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n << 1))],
    Expr::True(_) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3))],
    Expr::False(_) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))],
    Expr::Id(s) => {
      let offset = env.get(s);
      if offset.is_none() {
        panic!("Unbound variable {}", s);
      } else {
        vec![Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset.unwrap().clone()))]
      }
    },
    // get input from rdi and multiply it by 2
    Expr::input(_) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI)), Instr::IShl(Val::Reg(Reg::RAX), Val::Imm(1))],
    Expr::Let(bindings, body) => {
      let mut e_instrs = vec![];
      let mut si_cpy = si;
      let mut new_env = env.clone();
      for i in 0..bindings.len() {
        let x = bindings[i].0.to_string();
        let e = &bindings[i].1;
        // check for duplicate bindings
        if new_env.contains_key(&x) {
          eprintln!("Duplicate binding {}", x);
          std::process::exit(1);
        }

        // check for unbound variables
        for (_, e) in bindings.iter() {
          match e {
            Expr::Id(s) => {
              if !new_env.contains_key(s) {
                eprintln!("Unbound variable identifier {}", s);
                std::process::exit(1);
              }
            }
            _ => {}
          }
        }
        e_instrs.extend(compile_to_instrs(e, si_cpy, env, l));
        new_env = new_env.update(x, si_cpy);
        e_instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, si_cpy), Val::Reg(Reg::RAX)));
        si_cpy += 8;
      }
      let mut body_instrs = compile_to_instrs(body, si_cpy, &new_env, l);
      e_instrs.extend(body_instrs);
      e_instrs
    }
    Expr::UnOp(op, e) => {
      let mut instrs = compile_to_instrs(e, si + 8, env, l);
      match op {
        Op1::Add1 => {
          instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(2)));
        }
        Op1::Sub1 => {
          instrs.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(2)));
        }
        Op1::IsNum => {
          instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(1)));
          instrs.push(Instr::IJe("is_num_false".to_string()));
          instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(3)));
          instrs.push(Instr::IJe("is_num_false".to_string()));

          instrs.push(Instr::IJmp("is_num_end".to_string()));

          instrs.push(Instr::ILabel("is_num_false".to_string()));
          instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
          instrs.push(Instr::IRet);

          instrs.push(Instr::ILabel("is_num_end".to_string()));
          instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
        }
        Op1::IsBool => {
          instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(1)));
          instrs.push(Instr::IJe("is_bool_true".to_string()));
          instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(3)));
          instrs.push(Instr::IJe("is_bool_true".to_string()));

          instrs.push(Instr::IJmp("is_bool_end".to_string()));

          instrs.push(Instr::ILabel("is_bool_true".to_string()));
          instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
          instrs.push(Instr::IRet);

          instrs.push(Instr::ILabel("is_bool_end".to_string()));
          instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
        }
        _ => panic!("Invalid"),
      }
      instrs
    }
    Expr::BinOp(op, e1, e2) => {
      let mut instrs = compile_to_instrs(e1, si + 8, env, l);
      match op {
        Op2::Plus => {
          let mut e2_instrs = compile_to_instrs(e2, si + 8, env, l);
          instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, si), Val::Reg(Reg::RAX)));
          instrs.extend(e2_instrs);
          instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si)));
        }
        Op2::Minus => {
          let mut e2_instrs = compile_to_instrs(e2, si + 8, env, l);
          let mut e1_instrs = compile_to_instrs(e1, si + 8, env, l);

          instrs.pop();
          instrs.extend(e2_instrs);
          instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, si), Val::Reg(Reg::RAX)));
          instrs.extend(e1_instrs);
          instrs.push(Instr::ISub(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si)));
        }                                           
        Op2::Times => {
          let mut e2_instrs = compile_to_instrs(e2, si + 8, env, l);
          instrs.push(Instr::IShr(Val::Reg(Reg::RAX), Val::Imm(1)));
          instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, si), Val::Reg(Reg::RAX)));
          instrs.extend(e2_instrs);
          instrs.push(Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si)));
        }
        Op2::Less => {
          let mut e2_instrs = compile_to_instrs(e2, si + 8, env, l);
          instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, si), Val::Reg(Reg::RAX))); // mov [rsp - {offset}], rax
          instrs.extend(e2_instrs); // {e2_instrs}
          instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX))); // mov rbx, rax
          instrs.push(Instr::IXor(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, si))); // xor rbx, [rsp - {offset}]
          instrs.push(Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1))); // test rbx, 1
          instrs.push(Instr::IJne("snek_error".to_string())); // jne snek_error
          instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si))); // cmp rax, [rsp - {offset}]
          instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(1))); // mov rbx, 3
          instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3))); // mov rax, 1
          instrs.push(Instr::ICmovLe(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))); // cmovle rax, rbx
        }
        Op2::Greater => {
          let mut e2_instrs = compile_to_instrs(e2, si + 8, env, l);
          instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, si), Val::Reg(Reg::RAX))); // mov [rsp - {offset}], rax
          instrs.extend(e2_instrs); // {e2_instrs}
          instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX))); // mov rbx, rax
          instrs.push(Instr::IXor(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, si))); // xor rbx, [rsp - {offset}]
          instrs.push(Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1))); // test rbx, 1
          instrs.push(Instr::IJne("snek_error".to_string())); // jne snek_error
          instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si))); // cmp rax, [rsp - {offset}]
          instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(1))); // mov rbx, 3
          instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3))); // mov rax, 1
          instrs.push(Instr::ICmovGe(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))); // cmovg rax, rbx
        }
        Op2::Eq => {
          let mut e2_instrs = compile_to_instrs(e2, si + 8, env, l);
          instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, si), Val::Reg(Reg::RAX))); // mov [rsp - {offset}], rax
          instrs.extend(e2_instrs); // {e2_instrs}
          instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX))); // mov rbx, rax
          instrs.push(Instr::IXor(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, si))); // xor rbx, [rsp - {offset}]
          instrs.push(Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1))); // test rbx, 1
          instrs.push(Instr::IJne("snek_error".to_string())); // jne snek_error
          instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si))); // cmp rax, [rsp - {offset}]
          instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3))); // mov rbx, 3
          instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))); // mov rax, 1
          instrs.push(Instr::ICMove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))); // cmove rax, rbx
        }
        Op2::LessEq => {
          let mut e2_instrs = compile_to_instrs(e2, si + 8, env, l);
          instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, si), Val::Reg(Reg::RAX))); // mov [rsp - {offset}], rax
          instrs.extend(e2_instrs); // {e2_instrs}
          instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX))); // mov rbx, rax
          instrs.push(Instr::IXor(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, si))); // xor rbx, [rsp - {offset}]
          instrs.push(Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1))); // test rbx, 1
          instrs.push(Instr::IJne("snek_error".to_string())); // jne snek_error
          instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si))); // cmp rax, [rsp - {offset}]
          instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(1))); // mov rbx, 3
          instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3))); // mov rax, 1
          instrs.push(Instr::ICMovL(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))); // cmovl rax, rbx
        }
        Op2::GreaterEq => {
          let mut e2_instrs = compile_to_instrs(e2, si + 8, env, l);
          instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, si), Val::Reg(Reg::RAX))); // mov [rsp - {offset}], rax
          instrs.extend(e2_instrs); // {e2_instrs}
          instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX))); // mov rbx, rax
          instrs.push(Instr::IXor(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, si))); // xor rbx, [rsp - {offset}]
          instrs.push(Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1))); // test rbx, 1
          instrs.push(Instr::IJne("snek_error".to_string())); // jne snek_error
          instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si))); // cmp rax, [rsp - {offset}]
          instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(1))); // mov rbx, 3
          instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3))); // mov rax, 1
          instrs.push(Instr::ICmovG(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))); // cmovge rax, rbx
        }
        _ => panic!("Invalid"),
      }
      instrs
    }
    Expr::Print(e) => {
      let mut instrs = compile_to_instrs(e, si + 8, env, l);
      instrs.push(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm(i64::from(si))));
      instrs.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Reg(Reg::RAX)));
      instrs.push(Instr::ICall("snek_print".to_string()));
      instrs.push(Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(i64::from(si))));
      instrs
    }
    Expr::If(cond, thn, els) => {
      let mut instrs = Vec::new();
      *l += 1;
      let end_label = new_label(l, "end_if");
      let else_label = new_label(l, "else_if");
      let mut cond_instrs = compile_to_instrs(cond, si, env, l);
      let mut thn_instrs = compile_to_instrs(thn, si, env, l);
      let mut els_instrs = compile_to_instrs(els, si, env, l);
      instrs.append(&mut cond_instrs); // {cond_instrs}
      instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(1))); // cmp rax, 1
      instrs.push(Instr::IJe(else_label.clone())); // je {else_label}
      instrs.append(&mut thn_instrs); // {thn_instrs}
      instrs.push(Instr::IJmp(end_label.clone())); // jmp {end_label}
      instrs.push(Instr::ILabel(else_label)); // {else_label}:
      instrs.append(&mut els_instrs); // {els_instrs}
      instrs.push(Instr::ILabel(end_label)); // {end_label}:
      instrs
    }
    Expr::Loop(e) => {
      let mut instrs = Vec::new();
      let start_label = format!("loopstart_{}", l);
      let end_label = format!("loopend_{}", l);
      instrs.push(Instr::ILabel(start_label.clone())); // {start_label}:
      let mut e_instrs = compile_to_instrs(e, si, env, l);
      instrs.append(&mut e_instrs); // {e_instrs}
      instrs.push(Instr::IJmp(start_label)); // jmp {start_label}
      instrs.push(Instr::ILabel(end_label.clone())); // {end_label}:
      instrs
    }
    Expr::Break(e) => {
      let mut instrs = Vec::new();
      let end_label = format!("loopend_{}", l);
      let mut e_instrs = compile_to_instrs(e, si, env, l);
      instrs.append(&mut e_instrs);
      instrs.push(Instr::IJmp(end_label));
      instrs
    }
    Expr::Set(id, e) => {
      let offset = env.get(id).unwrap();
      let mut instrs = compile_to_instrs(e, si, env, l);
      instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, *offset), Val::Reg(Reg::RAX)));
      instrs
    }
    Expr::Block(e) => {
      let mut instrs = Vec::new();
      for expr in e {
        let mut e_instrs = compile_to_instrs(expr, si, env, l);
        instrs.append(&mut e_instrs);
      }
      instrs
    }
    _ => panic!("Invalid"),
  }
}

fn instr_to_str(i: &Instr) -> String {
  match i {
    Instr::IMov(v1, v2) => format!("mov {}, {}", val_to_str(v1), val_to_str(v2)),
    Instr::IAdd(v1, v2) => format!("add {}, {}", val_to_str(v1), val_to_str(v2)),
    Instr::ISub(v1, v2) => format!("sub {}, {}", val_to_str(v1), val_to_str(v2)),
    Instr::IMul(v1, v2) => format!("imul {}, {}", val_to_str(v1), val_to_str(v2)),
    Instr::IAnd(v1, v2) => format!("and {}, {}", val_to_str(v1), val_to_str(v2)),
    Instr::IOr(v1, v2) => format!("or {}, {}", val_to_str(v1), val_to_str(v2)),
    Instr::ICmp(v1, v2) => format!("cmp {}, {}", val_to_str(v1), val_to_str(v2)),
    Instr::ICall(s) => format!("call {}", s),
    Instr::IJe(s) => format!("je {}", s),
    Instr::IJmp(s) => format!("jmp {}", s),
    Instr::ILabel(s) => format!("{}:", s),
    Instr::IXor(v1, v2) => format!("xor {}, {}", val_to_str(v1), val_to_str(v2)),
    Instr::ITest(v1, v2) => format!("test {}, {}", val_to_str(v1), val_to_str(v2)),
    Instr::IJne(s) => format!("jne {}", s),
    Instr::ICMove(v1, v2) => format!("cmove {}, {}", val_to_str(v1), val_to_str(v2)),
    Instr::ICMovL(v1, v2) => format!("cmovl {}, {}", val_to_str(v1), val_to_str(v2)),
    Instr::ICmovLe(v1, v2) => format!("cmovle {}, {}", val_to_str(v1), val_to_str(v2)),
    Instr::ICmovGe(v1, v2) => format!("cmovge {}, {}", val_to_str(v1), val_to_str(v2)),
    Instr::ICmovG(v1, v2) => format!("cmovg {}, {}", val_to_str(v1), val_to_str(v2)),
    Instr::IShl(v1, v2) => format!("shl {}, {}", val_to_str(v1), val_to_str(v2)),
    Instr::IShr(v1, v2) => format!("shr {}, {}", val_to_str(v1), val_to_str(v2)),
    Instr::IRet => format!("ret"),
  }
}

fn val_to_str(v: &Val) -> String {
  match v {
    Val::Imm(n) => format!("{}", n),
    Val::Reg(r) => format!("{:?}", r),
    Val::RegOffset(r, n) => format!("[{:?} - {}]", r, n),
  }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    // You will make result hold the result of actually compiling
    let mut in_file = File::open(in_name)?;
    let mut contents = String::new();
    in_file.read_to_string(&mut contents)?;

    let expr = parse_expr(&parse(&contents).unwrap());
    let result = compile_to_instrs(&expr, 16, &HashMap::new(), &mut 0)
        .iter()
        .map(|i| instr_to_str(i))
        .collect::<Vec<String>>()
        .join("
");

    let asm_program = format!(
        "
section .text
extern snek_error
extern snek_print
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