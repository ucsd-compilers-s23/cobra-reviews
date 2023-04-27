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
    Label(String),
}

#[derive(Debug)]
enum Reg {
    RAX,
    RCX,
    RDX,
    RBX,
    RSP,
    RBP,
    RSI,
    RDI,
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val),
    ICmp(Val, Val),
    IJe(Val),
    ILabel(Val),
    IJmp(Val),
    IShl(Val, Val),
    ISar(Val, Val),
    IShr(Val, Val),
    IAnd(Val, Val),
    IXor(Val, Val),
    ITest(Val, Val),
    IJne(Val),
    ICmove(Val, Val),
    IJg(Val),
    IJl(Val),
    IJge(Val),
    IJle(Val),
    IOr(Val, Val),
    IJo(Val),
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
    Id(String),
    Boolean(bool),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
    Block(Vec<Expr>),
    Set(String, Box<Expr>),
}

fn parse_bind(s: &Sexp, loops: i64) -> Vec<(String, Expr)> {
  let mut result = Vec::new();
  let mut duplicate: HashMap<String, i64> = HashMap::new();
     if let Sexp::List(vec) = s {
          if vec.is_empty() {
              panic!("Invalid parsing");
          }
          for bind in vec.iter() {
              if let Sexp::List(vec1) = bind {
                  match &vec1[..] {
                      [Sexp::Atom(Atom::S(label)), value] => {
                        let identifier = label.as_str();
                        match identifier {
                            "true" => {
                                panic!("invalid identifier: true");
                            },
                            "false" => {
                                panic!("invalid identifier: false");
                            },
                            "input" => {
                                panic!("invalid identifier: input");
                            },
                            "let " => {
                                panic!("invalid identifier: let ");
                            },
                            "set!" => {
                                panic!("invalid identifier: set!");
                            },
                            "if" => {
                                panic!("invalid identifier: if");
                            },
                            "block " => {
                                panic!("invalid identifier: block");
                            },
                            "loop " => {
                                panic!("invalid identifier: loop");
                            },
                            "break " => {
                                panic!("invalid identifier: break");
                            },
                            "add1" => {
                                panic!("invalid identifier: add1");
                            },
                            "sub1" => {
                                panic!("invalid identifier: sub1");
                            },
                            "isnum" => {
                                panic!("invalid identifier: isnum");
                            },
                            "+" => {
                                panic!("invalid identifier: +");
                            },
                            "-" => {
                                panic!("invalid identifier: -");
                            },
                            "*" => {
                                panic!("invalid identifier: *");
                            },
                            "<" => {
                                panic!("invalid identifier: <");
                            },
                            ">" => {
                                panic!("invalid identifier: >");
                            },
                            ">=" => {
                                panic!("invalid identifier: >=");
                            },
                            "<=" => {
                                panic!("invalid identifier: <=");
                            },
                            "=" => {
                                panic!("invalid identifier: =");
                            },
                            "isbool" => {
                                panic!("invalid identifier: isbool");
                            },
                            _ => {
                                if duplicate.contains_key(label) {
                                    panic!("Duplicate binding");
                                } else {
                                    duplicate.insert(label.to_string(), 0);
                                    result.push((label.to_string(), parse_expr(value, loops)));
                                }
                            },
                        }
                      },
                      _ => panic!("Invalid binding"),
                  }
              } else {
                  panic!("Invalid binding");
              }
          }
     } else {
          panic!("Invalid binding");
     }
  result
}

fn parse_expr(s: &Sexp, loops: i64) -> Expr {
    match s {
      
      Sexp::Atom(Atom::I(n)) => {
        if *n > 4611686018427387903 {
            panic!("Invalid parsing");
        }
        if *n < -4611686018427387904 {
            panic!("Invalid parsing");
        }
        println!("parse num");
        Expr::Number(i64::try_from(*n).unwrap())
      }
      Sexp::Atom(Atom::F(f)) => {
        if *f > 4611686018427387903.0 {
            panic!("Invalid parsing");
        }
        if *f < -4611686018427387904.0 {
            panic!("Invalid parsing");
        }
        println!("parse num");
        Expr::Number(i64::try_from(f.round() as i64).unwrap())
      }
      Sexp::Atom(Atom::S(s)) if s == "true" => {
        println!("parse true");
        Expr::Boolean(true)
        
      }
      Sexp::Atom(Atom::S(s)) if s == "false" => {
        println!("parse false");
        Expr::Boolean(false)
        
      }
      Sexp::Atom(Atom::S(s)) if s == "input" => {
        println!("parse input");
        Expr::Id(s.to_string())
      }
      Sexp::Atom(Atom::S(s)) => {
        println!("parse string");
        Expr::Id(s.to_string())
      }
      Sexp::List(vec) => {
          match &vec[..] {
              [Sexp::Atom(Atom::S(op)), e] if op == "add1" => {
                println!("parse add1");
                Expr::UnOp(Op1::Add1, Box::new(parse_expr(e, loops)))
              }
              [Sexp::Atom(Atom::S(op)), e] if op == "sub1" => {
                println!("parse sub1");
                Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e, loops)))
              }
              [Sexp::Atom(Atom::S(op)), e] if op == "isnum" => {
                println!("parse isnum");
                Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e, loops)))
              }
              [Sexp::Atom(Atom::S(op)), e] if op == "isbool" => {
                println!("parse isbool");
                Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e, loops)))
              }
              [Sexp::Atom(Atom::S(op)), e] if op == "loop" => {
                println!("parse loop");
                Expr::Loop(Box::new(parse_expr(e, loops + 1)))
              }
              [Sexp::Atom(Atom::S(op)), e1, e2] if op == "let" => {
                  let bind = parse_bind(e1, loops);
                  let body = parse_expr(e2, loops);
                  println!("parse let");
                  Expr::Let(bind, Box::new(body))
              }
              [Sexp::Atom(Atom::S(op)), exprs @ ..] if op == "block" => {
                let mut parse_vec = Vec::new();
                for expr in exprs.into_iter() {
                    parse_vec.push(parse_expr(expr, loops));
                }
                println!("parse block");
                Expr::Block(parse_vec)
              }
              [Sexp::Atom(S(op)), Sexp::Atom(S(name)), e] if op == "set!" => {
                println!("parse set");
                Expr::Set(name.to_string(), Box::new(parse_expr(e, loops)))
              }
              [Sexp::Atom(Atom::S(op)), e1, e2] if op == "+" => {
                println!("parse +");
                Expr::BinOp(Op2::Plus, Box::new(parse_expr(e1, loops)), Box::new(parse_expr(e2, loops)))
              }
              [Sexp::Atom(Atom::S(op)), e1, e2] if op == "-" => {
                println!("parse -");
                Expr::BinOp(Op2::Minus, Box::new(parse_expr(e1, loops)), Box::new(parse_expr(e2, loops)))
              }
              [Sexp::Atom(Atom::S(op)), e1, e2] if op == "*" => {
                println!("parse *");
                Expr::BinOp(Op2::Times, Box::new(parse_expr(e1, loops)), Box::new(parse_expr(e2, loops)))
              }
              [Sexp::Atom(Atom::S(op)), e1, e2] if op == "<" => {
                println!("parse <");
                Expr::BinOp(Op2::Less, Box::new(parse_expr(e1, loops)), Box::new(parse_expr(e2, loops)))
              }
              [Sexp::Atom(Atom::S(op)), e1, e2] if op == ">" => {
                println!("parse >");
                Expr::BinOp(Op2::Greater, Box::new(parse_expr(e1, loops)), Box::new(parse_expr(e2, loops)))
              }
              [Sexp::Atom(Atom::S(op)), e1, e2] if op == ">=" => {
                println!("parse >=");
                Expr::BinOp(Op2::GreaterEqual, Box::new(parse_expr(e1, loops)), Box::new(parse_expr(e2, loops)))
              }
              [Sexp::Atom(Atom::S(op)), e1, e2] if op == "<=" => {
                println!("parse <=");
                Expr::BinOp(Op2::LessEqual, Box::new(parse_expr(e1, loops)), Box::new(parse_expr(e2, loops)))
              }
              [Sexp::Atom(Atom::S(op)), e1, e2] if op == "=" => {
                println!("parse =");
                Expr::BinOp(Op2::Equal, Box::new(parse_expr(e1, loops)), Box::new(parse_expr(e2, loops)))
              }
              [Sexp::Atom(Atom::S(op)), e1, e2, e3] if op == "if" => {
                println!("parse if");
                Expr::If(Box::new(parse_expr(e1, loops)), Box::new(parse_expr(e2, loops)), Box::new(parse_expr(e3, loops)))
              }
              [Sexp::Atom(Atom::S(op)), e] if op == "break" => {
                if loops == 0 {
                    panic!("invalid parse: break");
                }
                Expr::Break(Box::new(parse_expr(e, loops - 1)))
              }
              _ => panic!("Invalid parse"),
          }
      },
  }
}

fn new_label(l : &mut i64, s : &str) -> String {
  let current = *l;
  *l += 1;
  let label = format!("{s}_{current}");
  label
}

fn compile_to_instr(e: &Expr, si: i64, env: &HashMap<String, i64>, l: &mut i64, brake: &String) -> Vec<Instr> {
  let mut result = Vec::new();
  match e {
      Expr::Number(n) => {
        result.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n)));
        result.push(Instr::IShl(Val::Reg(Reg::RAX), Val::Imm(1)));
    }
      Expr::Id(l) => {
          if l == "input" {
              result.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI)));
          }
          else if let Some(value) = env.get(l) {
              result.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, *value)));
          } else {
              panic!("Unbound variable identifier {}", l);
          }
      }
      Expr::Boolean(b) => {
        if *b {
          result.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
        } else {
          result.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
        }
      }
      Expr::UnOp(Op1::Add1, v) => {
        let throw_error = "throw_error";
        let overflow = new_label(l, "overflow");
        let overflow_end = new_label(l, "overflow_end");
        let invalid_argument = new_label(l, "invalid_argument");
        let invalid_argument_end = new_label(l, "invalid_argument_end");

        //argument error
        result.push(Instr::IJmp(Val::Label(invalid_argument_end.clone())));
        result.push(Instr::ILabel(Val::Label(invalid_argument.clone())));
        result.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(0)));
        result.push(Instr::IJmp(Val::Label(throw_error.to_string())));
        result.push(Instr::ILabel(Val::Label(invalid_argument_end.clone())));

        //overflow
        result.push(Instr::IJmp(Val::Label(overflow_end.clone())));
        result.push(Instr::ILabel(Val::Label(overflow.clone())));
        result.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
        result.push(Instr::IJmp(Val::Label(throw_error.to_string())));
        result.push(Instr::ILabel(Val::Label(overflow_end.clone())));

        result.append(&mut compile_to_instr(v, si, env, l, brake));

        //check argument
        result.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
        result.push(Instr::IJne(Val::Label(invalid_argument.clone())));

        //check overflow
        result.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(-2)));
        result.push(Instr::IJo(Val::Label(overflow.clone())));

        result.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(2)));
      },
      Expr::UnOp(Op1::Sub1, v) => {
        let throw_error = "throw_error";
        let overflow = new_label(l, "overflow");
        let overflow_end = new_label(l, "overflow_end");
        let invalid_argument = new_label(l, "invalid_argument");
        let invalid_argument_end = new_label(l, "invalid_argument_end");

        //argument error
        result.push(Instr::IJmp(Val::Label(invalid_argument_end.clone())));
        result.push(Instr::ILabel(Val::Label(invalid_argument.clone())));
        result.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(0)));
        result.push(Instr::IJmp(Val::Label(throw_error.to_string())));
        result.push(Instr::ILabel(Val::Label(invalid_argument_end.clone())));

        //overflow
        result.push(Instr::IJmp(Val::Label(overflow_end.clone())));
        result.push(Instr::ILabel(Val::Label(overflow.clone())));
        result.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
        result.push(Instr::IJmp(Val::Label(throw_error.to_string())));
        result.push(Instr::ILabel(Val::Label(overflow_end.clone())));

        result.append(&mut compile_to_instr(v, si, env, l, brake));

        //check argument
        result.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
        result.push(Instr::IJne(Val::Label(invalid_argument.clone())));

        result.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(2)));
        result.push(Instr::IJo(Val::Label(overflow.clone())));

        result.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(2)));        
      },
      Expr::UnOp(Op1::IsNum, v) => {
        let end_label = new_label(l, "isnumend");
        let is_label = new_label(l, "notnum");
        result.append(&mut compile_to_instr(v, si, env, l, brake));
        result.push(Instr::IAnd(Val::Reg(Reg::RAX), Val::Imm(1)));
        result.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(1)));
        result.push(Instr::IJe(Val::Label(is_label.clone())));
        result.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
        result.push(Instr::IJmp(Val::Label(end_label.clone())));
        result.push(Instr::ILabel(Val::Label(is_label.clone())));
        result.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
        result.push(Instr::ILabel(Val::Label(end_label.clone())));
      },
      Expr::UnOp(Op1::IsBool, v) => {
        let end_label = new_label(l, "isboolend");
        let is_label = new_label(l, "notbool");
        result.append(&mut compile_to_instr(v, si, env, l, brake));
        result.push(Instr::IAnd(Val::Reg(Reg::RAX), Val::Imm(1)));
        result.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(0)));
        result.push(Instr::IJe(Val::Label(is_label.clone())));
        result.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
        result.push(Instr::IJmp(Val::Label(end_label.clone())));
        result.push(Instr::ILabel(Val::Label(is_label.clone())));
        result.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
        result.push(Instr::ILabel(Val::Label(end_label.clone())));
    },
      Expr::BinOp(Op2::Plus, v1, v2) => {
        let throw_error = "throw_error";
        let overflow = new_label(l, "overflow");
        let overflow_end = new_label(l, "overflow_end");
        let invalid_argument = new_label(l, "invalid_argument");
        let invalid_argument_end = new_label(l, "invalid_argument_end");

        //argument error
        result.push(Instr::IJmp(Val::Label(invalid_argument_end.clone())));
        result.push(Instr::ILabel(Val::Label(invalid_argument.clone())));
        result.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(0)));
        result.push(Instr::IJmp(Val::Label(throw_error.to_string())));
        result.push(Instr::ILabel(Val::Label(invalid_argument_end.clone())));

        //overflow
        result.push(Instr::IJmp(Val::Label(overflow_end.clone())));
        result.push(Instr::ILabel(Val::Label(overflow.clone())));
        result.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
        result.push(Instr::IJmp(Val::Label(throw_error.to_string())));
        result.push(Instr::ILabel(Val::Label(overflow_end.clone())));

        result.append(&mut compile_to_instr(v1, si, env, l, brake));

        //check argument
        result.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
        result.push(Instr::IJne(Val::Label(invalid_argument.clone())));

        result.push(Instr::IMov(Val::RegOffset(Reg::RSP, si*8), Val::Reg(Reg::RAX)));
        
        result.append(&mut compile_to_instr(v2, si + 1, env, l, brake));
        
        //check argument
        result.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
        result.push(Instr::IJne(Val::Label(invalid_argument.clone())));

        result.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si*8)));
        result.push(Instr::IJo(Val::Label(overflow.clone())));
      },
      Expr::BinOp(Op2::Minus, v1, v2) => {
        let throw_error = "throw_error";
        let overflow = new_label(l, "overflow");
        let overflow_end = new_label(l, "overflow_end");
        let invalid_argument = new_label(l, "invalid_argument");
        let invalid_argument_end = new_label(l, "invalid_argument_end");

        //argument error
        result.push(Instr::IJmp(Val::Label(invalid_argument_end.clone())));
        result.push(Instr::ILabel(Val::Label(invalid_argument.clone())));
        result.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(0)));
        result.push(Instr::IJmp(Val::Label(throw_error.to_string())));
        result.push(Instr::ILabel(Val::Label(invalid_argument_end.clone())));

        //overflow
        result.push(Instr::IJmp(Val::Label(overflow_end.clone())));
        result.push(Instr::ILabel(Val::Label(overflow.clone())));
        result.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
        result.push(Instr::IJmp(Val::Label(throw_error.to_string())));
        result.push(Instr::ILabel(Val::Label(overflow_end.clone())));

        result.append(&mut compile_to_instr(v2, si, env, l, brake));
        
        //check argument
        result.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
        result.push(Instr::IJne(Val::Label(invalid_argument.clone())));

        result.push(Instr::IMov(Val::RegOffset(Reg::RSP, si*8), Val::Reg(Reg::RAX)));
        
        result.append(&mut compile_to_instr(v1, si + 1, env, l, brake));
        
        //check argument
        result.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
        result.push(Instr::IJne(Val::Label(invalid_argument.clone())));

        result.push(Instr::ISub(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si*8)));
        result.push(Instr::IJo(Val::Label(overflow.clone())));
      },
      Expr::BinOp(Op2::Times, v1, v2) => {
        let throw_error = "throw_error";
        let overflow = new_label(l, "overflow");
        let overflow_end = new_label(l, "overflow_end");
        let invalid_argument = new_label(l, "invalid_argument");
        let invalid_argument_end = new_label(l, "invalid_argument_end");

        //argument error
        result.push(Instr::IJmp(Val::Label(invalid_argument_end.clone())));
        result.push(Instr::ILabel(Val::Label(invalid_argument.clone())));
        result.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(0)));
        result.push(Instr::IJmp(Val::Label(throw_error.to_string())));
        result.push(Instr::ILabel(Val::Label(invalid_argument_end.clone())));

        //overflow
        result.push(Instr::IJmp(Val::Label(overflow_end.clone())));
        result.push(Instr::ILabel(Val::Label(overflow.clone())));
        result.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
        result.push(Instr::IJmp(Val::Label(throw_error.to_string())));
        result.push(Instr::ILabel(Val::Label(overflow_end.clone())));

        result.append(&mut compile_to_instr(v1, si, env, l, brake));
        
        //check argument
        result.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
        result.push(Instr::IJne(Val::Label(invalid_argument.clone())));

        result.push(Instr::IMov(Val::RegOffset(Reg::RSP, si*8), Val::Reg(Reg::RAX)));
        
        result.append(&mut compile_to_instr(v2, si + 1, env, l, brake));
        
        //check argument
        result.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
        result.push(Instr::IJne(Val::Label(invalid_argument.clone())));

        result.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Reg(Reg::RAX)));
        result.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si*8)));
        result.push(Instr::IMul(Val::Reg(Reg::RCX)));
        
        //check overflow
        result.push(Instr::IJo(Val::Label(overflow.clone())));
        
        //divide by 2
        result.push(Instr::ISar(Val::Reg(Reg::RAX), Val::Imm(1)));
      },
      Expr::BinOp(Op2::Equal, v1, v2) => {
          let mut v1_instrs = compile_to_instr(v1, si, env, l, brake);
          let mut v2_instrs = compile_to_instr(v2, si + 1, env, l, brake);
          let offset = si * 8;
          let error_start = new_label(l, "error");
          let error_end = new_label(l, "error_end");
          let throw_error = "throw_error";
          result.push(Instr::IJmp(Val::Label(error_end.clone())));
          result.push(Instr::ILabel(Val::Label(error_start.clone())));
          result.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(2)));
          result.push(Instr::IJne(Val::Label(throw_error.to_string())));
          result.push(Instr::ILabel(Val::Label(error_end.clone())));
          result.append(&mut v1_instrs);
          result.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX)));
          result.append(&mut v2_instrs);
          result.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
          result.push(Instr::IXor(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, offset)));
          result.push(Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1)));
          result.push(Instr::IJne(Val::Label(error_start.clone())));
          result.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset)));
          result.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
          result.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
          result.push(Instr::ICmove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
      },

      Expr::BinOp(Op2::Greater, v1, v2) => {
          let mut v1_instrs = compile_to_instr(v1, si, env, l, brake);
          let mut v2_instrs = compile_to_instr(v2, si + 1, env, l, brake);
          let end_cond = new_label(l, "end_cond");
          let greater = new_label(l, "greater");
          let offset = si * 8;
          let error_start = new_label(l, "error");
          let error_end = new_label(l, "error_end");
          let throw_error = "throw_error";
          result.push(Instr::IJmp(Val::Label(error_end.clone())));
          result.push(Instr::ILabel(Val::Label(error_start.clone())));
          result.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(2)));
          result.push(Instr::IJne(Val::Label(throw_error.to_string())));
          result.push(Instr::ILabel(Val::Label(error_end.clone())));
          result.append(&mut v1_instrs);
          result.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX)));
          result.append(&mut v2_instrs);
          result.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
          result.push(Instr::IOr(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, offset)));
          result.push(Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1)));
          result.push(Instr::IJne(Val::Label(error_start.clone())));
          result.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset)));
          result.push(Instr::IJl(Val::Label(greater.clone())));
          result.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
          result.push(Instr::IJmp(Val::Label(end_cond.clone())));
          result.push(Instr::ILabel(Val::Label(greater.clone())));
          result.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
          result.push(Instr::ILabel(Val::Label(end_cond.clone())));
      }

      Expr::BinOp(Op2::GreaterEqual, v1, v2) => {
        let mut v1_instrs = compile_to_instr(v1, si, env, l, brake);
        let mut v2_instrs = compile_to_instr(v2, si + 1, env, l, brake);
        let end_cond = new_label(l, "end_cond");
        let greater = new_label(l, "greater");
        let offset = si * 8;
        let error_start = new_label(l, "error");
        let error_end = new_label(l, "error_end");
        let throw_error = "throw_error";
        result.push(Instr::IJmp(Val::Label(error_end.clone())));
        result.push(Instr::ILabel(Val::Label(error_start.clone())));
        result.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(2)));
        result.push(Instr::IJne(Val::Label(throw_error.to_string())));
        result.push(Instr::ILabel(Val::Label(error_end.clone())));
        result.append(&mut v1_instrs);
        result.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX)));
        result.append(&mut v2_instrs);
        result.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
        result.push(Instr::IOr(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, offset)));
        result.push(Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1)));
        result.push(Instr::IJne(Val::Label(error_start.clone())));
        result.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset)));
        result.push(Instr::IJle(Val::Label(greater.clone())));
        result.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
        result.push(Instr::IJmp(Val::Label(end_cond.clone())));
        result.push(Instr::ILabel(Val::Label(greater.clone())));
        result.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
        result.push(Instr::ILabel(Val::Label(end_cond.clone())));
      }

      Expr::BinOp(Op2::Less, v1, v2) => {
        let mut v1_instrs = compile_to_instr(v1, si, env, l, brake);
        let mut v2_instrs = compile_to_instr(v2, si + 1, env, l, brake);
        let end_cond = new_label(l, "end_cond");
        let greater = new_label(l, "greater");
        let offset = si * 8;
        let error_start = new_label(l, "error");
        let error_end = new_label(l, "error_end");
        let throw_error = "throw_error";
        result.push(Instr::IJmp(Val::Label(error_end.clone())));
        result.push(Instr::ILabel(Val::Label(error_start.clone())));
        result.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(2)));
        result.push(Instr::IJne(Val::Label(throw_error.to_string())));
        result.push(Instr::ILabel(Val::Label(error_end.clone())));
        result.append(&mut v1_instrs);
        result.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX)));
        result.append(&mut v2_instrs);
        result.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
        result.push(Instr::IOr(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, offset)));
        result.push(Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1)));
        result.push(Instr::IJne(Val::Label(error_start.clone())));
        result.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset)));
        result.push(Instr::IJg(Val::Label(greater.clone())));
        result.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
        result.push(Instr::IJmp(Val::Label(end_cond.clone())));
        result.push(Instr::ILabel(Val::Label(greater.clone())));
        result.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
        result.push(Instr::ILabel(Val::Label(end_cond.clone())));
      }

      Expr::BinOp(Op2::LessEqual, v1, v2) => {
        let mut v1_instrs = compile_to_instr(v1, si, env, l, brake);
        let mut v2_instrs = compile_to_instr(v2, si + 1, env, l, brake);
        let end_cond = new_label(l, "end_cond");
        let greater = new_label(l, "greater");
        let offset = si * 8;
        let error_start = new_label(l, "error");
        let error_end = new_label(l, "error_end");
        let throw_error = "throw_error";
        result.push(Instr::IJmp(Val::Label(error_end.clone())));
        result.push(Instr::ILabel(Val::Label(error_start.clone())));
        result.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(2)));
        result.push(Instr::IJne(Val::Label(throw_error.to_string())));
        result.push(Instr::ILabel(Val::Label(error_end.clone())));
        result.append(&mut v1_instrs);
        result.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX)));
        result.append(&mut v2_instrs);
        result.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
        result.push(Instr::IOr(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, offset)));
        result.push(Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1)));
        result.push(Instr::IJne(Val::Label(error_start.clone())));
        result.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset)));
        result.push(Instr::IJge(Val::Label(greater.clone())));
        result.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
        result.push(Instr::IJmp(Val::Label(end_cond.clone())));
        result.push(Instr::ILabel(Val::Label(greater.clone())));
        result.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
        result.push(Instr::ILabel(Val::Label(end_cond.clone())));
      }

      Expr::Let(binds, body) => {
          let mut nenv = env.clone();
          let mut address = si;       
          for (var, expr) in binds {
              result.append(&mut compile_to_instr(expr, address, &nenv, l, brake));
              result.push(Instr::IMov(Val::RegOffset(Reg::RSP, address*8), Val::Reg(Reg::RAX)));
              nenv.insert(var.clone(), address*8);
              address += 1;
          }
          result.append(&mut compile_to_instr(body, address, &nenv, l, brake));
      },
      Expr::If(cond, thn, els) => {
        let end_label = new_label(l, "ifend");
        let else_label = new_label(l, "ifelse");
        let mut cond_instrs = compile_to_instr(cond, si, env, l, brake);
        let mut thn_instrs = compile_to_instr(thn, si, env, l, brake);
        let mut els_instrs = compile_to_instr(els, si, env, l, brake);
        result.append(&mut cond_instrs);
        result.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(1)));
        result.push(Instr::IJe(Val::Label(else_label.clone())));
        result.append(&mut thn_instrs);
        result.push(Instr::IJmp(Val::Label(end_label.clone())));
        result.push(Instr::ILabel(Val::Label(else_label.clone())));
        result.append(&mut els_instrs);
        result.push(Instr::ILabel(Val::Label(end_label.clone())));
      },

      Expr::Loop(body) => {
        let new_loop = new_label(l, "loop");
        let end_loop = new_label(l, "loop_end");
        let mut body_instrs = compile_to_instr(body, si, env, l, &end_loop);

        result.push(Instr::ILabel(Val::Label(new_loop.clone())));
        result.append(&mut body_instrs);
        result.push(Instr::IJmp(Val::Label(new_loop.clone())));
        result.push(Instr::ILabel(Val::Label(end_loop.clone())));
      },

      Expr::Break(e) => {
        result.append(&mut compile_to_instr(e, si, env, l, brake));
        result.push(Instr::IJmp(Val::Label(brake.to_string())));
      },
      
      Expr::Block(es) => {
        result.append(&mut es.into_iter().flat_map(|e| {
            compile_to_instr(e, si, env, l, brake)
        }).collect::<Vec<Instr>>());
      },

      Expr::Set(name, val) => {
        let offset = env.get(name).unwrap();
        let mut val_is = compile_to_instr(val, si, env, l, brake);

        result.append(&mut val_is);
        result.push(Instr::IMov(Val::RegOffset(Reg::RSP, *offset), Val::Reg(Reg::RAX)));
      },
  }
  result
}

fn instr_to_str(i: &Instr) -> String {
  match i {
      Instr::IMov(val1, val2) => format!("mov {}, {}", val_to_str(val1), val_to_str(val2)),
      Instr::IAdd(val1, val2) => format!("add {}, {}", val_to_str(val1), val_to_str(val2)),
      Instr::ISub(val1, val2) => format!("sub {}, {}", val_to_str(val1), val_to_str(val2)),
      Instr::IMul(val) => format!("imul {}", val_to_str(val)),
      Instr::ICmp(val1, val2) => format!("cmp {}, {}", val_to_str(val1), val_to_str(val2)),
      Instr::IJe(val) => format!("je {}", val_to_str(val)),
      Instr::ILabel(val) => format!("{}:", val_to_str(val)),
      Instr::IJmp(val) => format!("jmp {}", val_to_str(val)),
      Instr::IShl(val1, val2) => format!("shl {}, {}", val_to_str(val1), val_to_str(val2)),
      Instr::ISar(val1, val2) => format!("sar {}, {}", val_to_str(val1), val_to_str(val2)),
      Instr::IShr(val1, val2) => format!("shr {}, {}", val_to_str(val1), val_to_str(val2)),
      Instr::IAnd(val1, val2) => format!("and {}, {}", val_to_str(val1), val_to_str(val2)),
      Instr::IXor(val1, val2) => format!("xor {}, {}", val_to_str(val1), val_to_str(val2)),
      Instr::ITest(val1, val2) => format!("test {}, {}", val_to_str(val1), val_to_str(val2)),
      Instr::IJne(val) => format!("jne {}", val_to_str(val)),
      Instr::ICmove(val1, val2) => format!("cmove {}, {}", val_to_str(val1), val_to_str(val2)),
      Instr::IJg(val) => format!("jg {}", val_to_str(val)),
      Instr::IJl(val) => format!("jl {}", val_to_str(val)),
      Instr::IJle(val) => format!("jle {}", val_to_str(val)),
      Instr::IJge(val) => format!("jge {}", val_to_str(val)),
      Instr::IJo(val) => format!("jo {}", val_to_str(val)),
      Instr::IOr(val1, val2) => format!("or {}, {}", val_to_str(val1), val_to_str(val2)),
  }
}

fn val_to_str(v: &Val) -> String {
  match v {
      Val::Reg(Reg::RAX) => format!("rax"),
      Val::Reg(Reg::RSP) => format!("rsp"),
      Val::Reg(Reg::RCX) => format!("rcx"),
      Val::Reg(Reg::RDX) => format!("rdx"),
      Val::Reg(Reg::RBX) => format!("rbx"),
      Val::Reg(Reg::RBP) => format!("rbp"),
      Val::Reg(Reg::RSI) => format!("rsi"),
      Val::Reg(Reg::RDI) => format!("rdi"),
      Val::Imm(n) => format!("{}", *n),
      Val::RegOffset(_v1, v2) => format!("[rsp - {}]", v2),
      Val::Label(s) => format!("{}", s),
  }
}


fn compile(e: &Expr, si: i64, env: &HashMap<String,i64>, l: &mut i64, brake: &String ) -> String {
  let parse = compile_to_instr(e, si, env, l, brake);
  let mut output = String::new();
  let len = parse.len();

  for (i, instr) in parse.iter().enumerate() {
      output.push_str(&instr_to_str(instr));
      if i < len - 1 {
          output.push_str("\n  ");
      }
  }
  output
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    // You will make result hold the result of actually compiling
    let map: HashMap<String, i64> = HashMap::new();
    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    let mut curr_brake = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let s_expr = parse(&in_contents);
    if let Err(n) = s_expr {
        panic!("Invalid {}", n);
    }
    let si = 2;
    let mut label = 1;
    let s_expr1 = s_expr.unwrap();
    let expr = parse_expr(&s_expr1, 0);
    let result = compile(&expr, si, &map, &mut label, &mut curr_brake);

    let asm_program = format!(
        "
section .text
extern snek_error
extern snek_print
global our_code_starts_here
throw_error:
mov rdi, rax
push rsp
call snek_error
our_code_starts_here:
  {}
  mov rdi, rax
  push rdi
  call snek_print
  pop rdi
  ret
",
        result
    );

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}


