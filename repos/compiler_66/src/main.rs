
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
    RegOffset(Reg, i32),
    Label(String),
}

#[derive(Debug)]
enum Reg {
    RAX,
    RSP,
    RBX,
    RDI,
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    XOR(Val, Val),
    TEST(Val, Val),
    JNE(Val),
    JE(Val),
    JL(Val),
    JG(Val),
    JLE(Val),
    JGE(Val),
    JMP(Val),
    JO(Val),
    CMP(Val, Val),
    CMOVE(Val, Val),
    SAR(Val),
    LABEL(Val),
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
    //   Sexp::Atom(I(n)) => {
    //     match i64::try_from(*n).unwrap(){
    //         Ok(num) => Expr::Number(num),
    //         Err(error) => panic!("invalid {}", error)
    //     }
    //   }
    Sexp::Atom(I(n)) => {
        if *n < -4611686018427387904 || *n > 4611686018427387903 {
            panic!("Invalid number")
        }
        Expr::Number(i64::try_from(*n).unwrap())
    }
      Sexp::Atom(S(name)) if name == "true" => Expr::Boolean(true),
      Sexp::Atom(S(name)) if name == "false" => Expr::Boolean(false),
      Sexp::Atom(S(name)) => Expr::Id(name.to_string()),
      Sexp::List(vec) => {
          match &vec[..] {
            // add1
            [Sexp::Atom(S(op)), e] if op == "add1" =>
                Expr::UnOp(Op1::Add1, Box::new(parse_expr(e))),
            // sub1
            [Sexp::Atom(S(op)), e] if op == "sub1" =>
                Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e))),
            // isNum
            [Sexp::Atom(S(op)), e] if op == "isnum" =>
                Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e))),
            // isBool
            [Sexp::Atom(S(op)), e] if op == "isbool" =>
                Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e))),
            // plus
            [Sexp::Atom(S(op)), e1, e2] if op == "+" =>
                Expr::BinOp(Op2::Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
            // minus
            [Sexp::Atom(S(op)), e1, e2] if op == "-" =>
                Expr::BinOp(Op2::Minus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
            // times
            [Sexp::Atom(S(op)), e1, e2] if op == "*" =>
                Expr::BinOp(Op2::Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
            // equal
            [Sexp::Atom(S(op)), e1, e2] if op == "=" =>
                Expr::BinOp(Op2::Equal, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
            // less equal
            [Sexp::Atom(S(op)), e1, e2] if op == "<=" =>
                Expr::BinOp(Op2::LessEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
            // greater equal
            [Sexp::Atom(S(op)), e1, e2] if op == ">=" =>
                Expr::BinOp(Op2::GreaterEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
            // less
            [Sexp::Atom(S(op)), e1, e2] if op == "<" =>
                Expr::BinOp(Op2::Less, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
            // greater
            [Sexp::Atom(S(op)), e1, e2] if op == ">" =>
                Expr::BinOp(Op2::Greater, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
            // let
            [Sexp::Atom(S(op)), Sexp::List(binds), body] if op == "let" => {
                if binds.is_empty() {
                    panic!("Invalid: empty binding")
                }
                let mut v: Vec<(String, Expr)> = Vec::new();
                for bind in binds {
                    v.push(parse_bind(bind));
                }
                Expr::Let(v, Box::new(parse_expr(body)))
            }
            // if
            [Sexp::Atom(S(op)), condition, then ,els] if op == "if" =>
                Expr::If(
                    Box::new(parse_expr(condition)),
                    Box::new(parse_expr(then)), 
                    Box::new(parse_expr(els))
                ),
            // loop
            [Sexp::Atom(S(op)), e] if op == "loop" => Expr::Loop(Box::new(parse_expr(e))),
            // break
            [Sexp::Atom(S(op)), e] if op == "break" => Expr::Break(Box::new(parse_expr(e))),
            // set
            [Sexp::Atom(S(op)), Sexp::Atom(S(name)), e] if op == "set!" => 
                Expr::Set(name.to_string(), Box::new(parse_expr(e))),
            // block
            [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => {
                if exprs.is_empty() {
                    panic!("Invalid: empty block")
                }
                let mut v: Vec<Expr> = Vec::new();
                for expr in exprs {
                    v.push(parse_expr(expr));
                }
                Expr::Block(v)
                // Expr::Block(exprs.into_iter().map(parse_expr).collect())
            }
            _ => panic!("Invalid: {}", s),

          }
      }
      _ => panic!("Invalid"),
  }
}

fn parse_bind(s: &Sexp) -> (String, Expr) {
  match s {
      Sexp::List(vec) => {
          match &vec[..] {
                [Sexp::Atom(S(name)), val] => {
                    if name == "input" {
                        panic!("invalid keyword: input cannot be used in binding")
                    } 
                    (name.to_string(), parse_expr(val))
                }
            
              _ => panic!("Invalid"),
          }            
      }
      _ => panic!("Invalid"),
  }
}

fn compile_to_instrs(e: &Expr, si: i32, env: &HashMap<String, i32>, brake: &String, l: &mut i32) -> Vec<Instr> {
  let mut v: Vec<Instr> = Vec::new();
  match e {
    // number
    Expr::Number(n) =>
        v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n << 1))),
    // true
    Expr::Boolean(true) =>
        v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3))),
    // false
    Expr::Boolean(false) =>
        v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))),
    // add1
    Expr::UnOp(Op1::Add1, sub_expr) => {
        v.extend(compile_to_instrs(sub_expr, si, env, brake, l));
        v.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(2)));
        v.extend(check_overflow(si));
    },
    //sub1
    Expr::UnOp(Op1::Sub1, sub_expr) => {
        v.extend(compile_to_instrs(sub_expr, si, env, brake, l));
        v.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(2)));
        v.extend(check_overflow(si));
    },
    // isNum
    Expr::UnOp(Op1::IsNum, sub_expr) => {
        v.extend(compile_to_instrs(sub_expr, si, env, brake, l));
        v.push(Instr::TEST(Val::Reg(Reg::RAX), Val::Imm(1)));
        let num_label = new_label(l, "is_num");
        let end_label = new_label(l, "is_num_end");
        v.push(Instr::JE(Val::Label(num_label.clone())));
        v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
        v.push(Instr::JMP(Val::Label(end_label.clone())));
        v.push(Instr::LABEL(Val::Label(num_label)));
        v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
        v.push(Instr::LABEL(Val::Label(end_label)));
    }
    // isBool
    Expr::UnOp(Op1::IsBool, sub_expr) => {
        // v.extend(compile_to_instrs(sub_expr, si, env, brake, l));
        // v.push(Instr::TEST(Val::Reg(Reg::RAX), Val::Imm(1)));
        // v.push(Instr::IMov(Val::RegOffset(Reg::RSP, (si + 1) * 8), Val::Reg(Reg::RDI)));
        // v.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(11)));
        // v.push(Instr::JE(Val::Label("throw_error".to_string())));
        // v.push(Instr::IMov(Val::Reg(Reg::RDI), Val::RegOffset(Reg::RSP, (si + 1) * 8)));
        v.extend(compile_to_instrs(sub_expr, si, env, brake, l));
        v.push(Instr::TEST(Val::Reg(Reg::RAX), Val::Imm(1)));
        let bool_label = new_label(l, "is_bool");
        let end_label = new_label(l, "is_bool_end");
        v.push(Instr::JNE(Val::Label(bool_label.clone())));
        v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
        v.push(Instr::JMP(Val::Label(end_label.clone())));
        v.push(Instr::LABEL(Val::Label(bool_label)));
        v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
        v.push(Instr::LABEL(Val::Label(end_label)));
    }
    // plus
    Expr::BinOp(Op2::Plus, sub_expr1, sub_expr2) => {
    v.extend(compile_to_instrs_for_bin_op(sub_expr1, sub_expr2, si, env, brake, l));
    v.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si * 8)));
    v.extend(check_overflow(si));
    },
    // minus
    Expr::BinOp(Op2::Minus, sub_expr1, sub_expr2) => {
    v.extend(compile_to_instrs_for_bin_op(sub_expr1, sub_expr2, si, env, brake, l));
    v.push(Instr::ISub(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
    v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si * 8)));
    v.extend(check_overflow(si));
    },
    // times
    Expr::BinOp(Op2::Times, sub_expr1, sub_expr2) => {
    v.extend(compile_to_instrs_for_bin_op(sub_expr1, sub_expr2, si, env, brake, l));
    v.push(Instr::SAR(Val::Reg(Reg::RAX)));
    v.push(Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si * 8)));
    v.extend(check_overflow(si));
    },
    // let
    Expr::Let(binds, body) => {
        let mut new_env = env.clone();
        let mut new_si = si;
        for (var_name, expr) in binds{
            if new_env.contains_key(var_name) && !env.contains_key(var_name) {
                panic!("Duplicate binding");
            }
            v.extend(compile_to_instrs(expr, new_si, &new_env, brake, l));
            new_env = new_env.update(var_name.to_string(), new_si);
            v.push(Instr::IMov(Val::RegOffset(Reg::RSP, new_si * 8), Val::Reg(Reg::RAX)));
            new_si += 1;
        };
        v.extend(compile_to_instrs(body, new_si, &new_env, brake, l))
    }
    // Id
    Expr::Id(s) => {
        if s == "let" || s == "add1" || s == "sub1" || s == "true" || s == "false" || s == "block" || s == "set" || s == "break" {
            panic!("invalid keyword!");
        }else if s == "input"{
            v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI)));
        }else if !env.contains_key(s) {
            panic!("Unbound variable identifier {}", s);
        }else {
            let offset = env.get(s).unwrap() * 8;
            v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset)));
        }
        
    }
    // Equal
    Expr::BinOp(Op2::Equal, sub_expr1, sub_expr2) => {
        // unique in Equal: can be boolean in both sides
        v.extend(compile_to_instrs(sub_expr1, si, env, brake, l));
        v.push(Instr::IMov(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
        v.extend(compile_to_instrs(sub_expr2, si + 1, env, brake, l));
        v.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
        
        v.push(Instr::XOR(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, si * 8)));
        //   v.push(Instr::TEST(Val::Reg(Reg::RBX), Val::Imm(1)));
        //   v.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(7)));
        //   v.push(Instr::JNE(Val::Label("throw_error".to_string())));
        v.extend(check_error(Reg::RBX, 7, si));
        v.push(Instr::CMP(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
        v.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
        v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
        v.push(Instr::CMOVE(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
    }
    // less
    Expr::BinOp(Op2::Less, sub_expr1, sub_expr2) => {
        v.extend(compile_to_instrs_for_bin_op(sub_expr1, sub_expr2, si, env, brake, l));
        v.push(Instr::CMP(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
        let less_label = new_label(l, "less");
        let end_label = new_label(l, "less_end");
        v.push(Instr::JL(Val::Label(less_label.clone())));
        v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
        v.push(Instr::JMP(Val::Label(end_label.clone())));
        v.push(Instr::LABEL(Val::Label(less_label)));
        v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
        v.push(Instr::LABEL(Val::Label(end_label)));
    }
    // greater
    Expr::BinOp(Op2::Greater, sub_expr1, sub_expr2) => {
        v.extend(compile_to_instrs_for_bin_op(sub_expr1, sub_expr2, si, env, brake, l));
        v.push(Instr::CMP(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
        let gt_label = new_label(l, "greater");
        let end_label = new_label(l, "greater_end");
        v.push(Instr::JG(Val::Label(gt_label.clone())));
        v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
        v.push(Instr::JMP(Val::Label(end_label.clone())));
        v.push(Instr::LABEL(Val::Label(gt_label)));
        v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
        v.push(Instr::LABEL(Val::Label(end_label)));
    }
    // less equal
    Expr::BinOp(Op2::LessEqual, sub_expr1, sub_expr2) => {
        v.extend(compile_to_instrs_for_bin_op(sub_expr1, sub_expr2, si, env, brake, l));
        v.push(Instr::CMP(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
        let less_eq_label = new_label(l, "less_eq");
        let end_label = new_label(l, "less_eq_end");
        v.push(Instr::JLE(Val::Label(less_eq_label.clone())));
        v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
        v.push(Instr::JMP(Val::Label(end_label.clone())));
        v.push(Instr::LABEL(Val::Label(less_eq_label)));
        v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
        v.push(Instr::LABEL(Val::Label(end_label)));
    }
    // greater equal
    Expr::BinOp(Op2::GreaterEqual, sub_expr1, sub_expr2) => {
        v.extend(compile_to_instrs_for_bin_op(sub_expr1, sub_expr2, si, env, brake, l));
        v.push(Instr::CMP(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
        let gt_eq_label = new_label(l, "greater_equal");
        let end_label = new_label(l, "greater_eq_end");
        v.push(Instr::JGE(Val::Label(gt_eq_label.clone())));
        v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
        v.push(Instr::JMP(Val::Label(end_label.clone())));
        v.push(Instr::LABEL(Val::Label(gt_eq_label)));
        v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
        v.push(Instr::LABEL(Val::Label(end_label)));
    }
    // if
    Expr::If(cond, thn , els) => {
        let end_label = new_label(l, "ifend");
        let else_label = new_label(l, "ifelse");
        v.extend(compile_to_instrs(cond, si, env, brake, l));

        // v.push(Instr::TEST(Val::Reg(Reg::RAX), Val::Imm(1)));
        // v.push(Instr::IMov(Val::RegOffset(Reg::RSP, (si + 1) * 8), Val::Reg(Reg::RDI)));
        // v.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(11)));
        // v.push(Instr::JE(Val::Label("throw_error".to_string())));
        // v.push(Instr::IMov(Val::Reg(Reg::RDI), Val::RegOffset(Reg::RSP, (si + 1) * 8)));

        v.push(Instr::CMP(Val::Reg(Reg::RAX), Val::Imm(1)));
        v.push(Instr::JE(Val::Label(else_label.clone())));
        v.extend(compile_to_instrs(thn, si, env, brake, l));

        v.push(Instr::JMP(Val::Label(end_label.clone())));
        v.push(Instr::LABEL(Val::Label(else_label)));
        v.extend(compile_to_instrs(els, si, env, brake, l));
        v.push(Instr::LABEL(Val::Label(end_label)));
    }
    // loop
    Expr::Loop(sub_expr) => {
        let st_label = new_label(l, "loop");
        let end_label = new_label(l, "loopend");
        v.push(Instr::LABEL(Val::Label(st_label.clone())));
        v.extend(compile_to_instrs(sub_expr, si, env, &end_label, l));
        v.push(Instr::JMP(Val::Label(st_label)));
        v.push(Instr::LABEL(Val::Label(end_label)));
    }
    // break
    Expr::Break(e) => {
        if brake == "" {
            panic!("break appears outside a loop");
        }
        v.extend(compile_to_instrs(e, si, env, brake, l));
        v.push(Instr::JMP(Val::Label(brake.clone())));
    }
    // set
    Expr::Set(name, val) => {
        if name == "let" || name == "add1" || name == "sub1" || name == "true" || name == "false" || name == "block" || name == "set"{
            panic!("invalid keyword!");
        }else if !env.contains_key(name) {
            panic!("Unbound variable identifier {}", name);
        }else {
            let offset = env.get(name).unwrap() * 8;
            v.extend(compile_to_instrs(val, si, env, brake, l));
            v.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX)));
        } 
    }
    // block
    Expr::Block(exprs) => {
        for expr in exprs {
            v.extend(compile_to_instrs(expr, si, env, brake, l));
        }
    }
    // _ => panic!("complie to instrs error!")
  }
  
  v
}
fn check_error(reg: Reg, err_code: i64, si: i32) -> Vec<Instr> {
    let mut v: Vec<Instr> = Vec::new();
    v.push(Instr::TEST(Val::Reg(reg), Val::Imm(1)));
    v.push(Instr::IMov(Val::RegOffset(Reg::RSP, (si + 1) * 8), Val::Reg(Reg::RDI)));
    v.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(err_code)));
    v.push(Instr::JNE(Val::Label("throw_error".to_string())));
    v.push(Instr::IMov(Val::Reg(Reg::RDI), Val::RegOffset(Reg::RSP, (si + 1) * 8)));
    return v
}
fn check_overflow(si: i32) -> Vec<Instr> {
    let mut v: Vec<Instr> = Vec::new();
    v.push(Instr::IMov(Val::RegOffset(Reg::RSP, (si + 1) * 8), Val::Reg(Reg::RDI)));
    v.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(9)));
    v.push(Instr::JO(Val::Label("throw_error".to_string())));
    v.push(Instr::IMov(Val::Reg(Reg::RDI), Val::RegOffset(Reg::RSP, (si + 1) * 8)));
    return v
}

fn compile_to_instrs_for_bin_op(sub_expr1: &Box<Expr>, sub_expr2: &Box<Expr>, si: i32, env: &HashMap<String, i32>, brake: &String, l: &mut i32) -> Vec<Instr> {
    let mut v: Vec<Instr> = Vec::new();
    v.extend(compile_to_instrs(sub_expr1, si, env, brake, l));
    v.extend(check_error(Reg::RAX, 3, si));
    v.push(Instr::IMov(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
    v.extend(compile_to_instrs(sub_expr2, si + 1, env, brake, l));
    v.extend(check_error(Reg::RAX, 3, si));
    return v
}

fn new_label(l: &mut i32, s: &str) -> String {
    let current = *l;
    *l += 1;
    format!("{s}_{current}")
}

fn instr_to_str(i: &Instr) -> String {
  match i {
      Instr::IMov(v1, v2) => format!("\nmov {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::IAdd(v1, v2) => format!("\nadd {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::ISub(v1, v2) => format!("\nsub {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::IMul(v1, v2) => format!("\nimul {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::CMOVE(v1, v2) => format!("\ncmove {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::CMP(v1, v2) => format!("\ncmp {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::XOR(v1, v2) => format!("\nxor {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::TEST(v1, v2) => format!("\ntest {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::JNE(v1) => format!("\njne {}", val_to_str(v1)),
      Instr::JE(v1) => format!("\nje {}", val_to_str(v1)),
      Instr::JL(v1) => format!("\njl {}", val_to_str(v1)),
      Instr::JG(v1) => format!("\njg {}", val_to_str(v1)),
      Instr::JLE(v1) => format!("\njle {}", val_to_str(v1)),
      Instr::JGE(v1) => format!("\njge {}", val_to_str(v1)),
      Instr::JMP(v1) => format!("\njmp {}", val_to_str(v1)),
      Instr::JO(v1) => format!("\njo {}", val_to_str(v1)),
      Instr::LABEL(l) => format!("\n{}:", val_to_str(l)),
      Instr::SAR(v1) => format!("\nsar {}, 1", val_to_str(v1)),
      // _ => panic!("instr to str error!"),
  }
}

fn val_to_str(v: &Val) -> String {
  match v {
      Val::Imm(num) => format!("{}", *num),
      Val::Reg(Reg::RAX) => format!("rax"),
      Val::Reg(Reg::RSP) => format!("rsp"),
      Val::Reg(Reg::RBX) => format!("rbx"),
      Val::Reg(Reg::RDI) => format!("rdi"),
      Val::RegOffset(Reg::RSP, offset) => format!("[rsp - {}]", offset),
      Val::Label(label) => format!("{label}"),
      _ => panic!("val to str error!"),
  }
}

fn compile(e: &Expr) -> String {
  let mut s = String::new();
  let mut labels = 0;
  let mut instrs = compile_to_instrs(e, 2, &HashMap::new(), &"".to_string(), &mut labels);
  for instr in &mut instrs{
      s.push_str(&instr_to_str(&instr));
  }
  s
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;


    let s_expr = match parse(&in_contents) {
      Ok(sexp) => sexp,
      Err(error) => panic!("Invalid: {}", error),
  };
  println!("{s_expr}");
  let expr = parse_expr(&s_expr);
  let result = compile(&expr);

    // You will make result hold the result of actually compiling
    // let result = "mov rax, 131";

    let asm_program = format!(
        "
section .text
extern snek_error
extern snek_print
throw_error:
  push rsp
  call snek_error
  ret
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
