use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::Atom::*;
use sexp::*;

use im::HashMap;
use std::collections::HashSet;

#[derive(Debug)]
enum Val {
    Reg(Reg),
    Imm(i64),
    RegOffset(Reg, i64)
}

#[derive(Debug)]
enum Reg {
    RAX,
    RSP,
    RDI,
    RBX
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    Test(Val, Val),
    Jmp(String),
    Je(String),
    Jne(String),
    CMove(Val, Val),
    Cmp(Val, Val),
    Sar(Val, Val),
    Jg(String),
    Jl(String),
    Jge(String),
    Jle(String),
    Jo(String),
    Label(String),
    Xor(Val, Val)
}

#[derive(Debug)]
enum Op1 {
    Add1,
    Sub1,
    IsNum,
    IsBool
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
    LessEqual
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
    Block(Vec<Expr>),
    Set(String, Box<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>)
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];
    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;
    let content = parse(&in_contents).expect("Invalid parentheses");
    let expr = parse_expr(&content);
    let mut labels = 0;
    let result = compile(&expr, 2, &HashMap::new(), &String::from(""), &mut labels);

    let asm_program = format!(
        "
        section .text
        global our_code_starts_here
        extern snek_error
        extern snek_print
        invalid_argument:
          mov rdi, 99
          jmp throw_error
        overflow:
          mov rdi, 101
          jmp throw_error
        throw_error:
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

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => Expr::Number(i64::try_from(*n).unwrap()),
        Sexp::Atom(S(name)) if name == "true" => Expr::Boolean(true),
        Sexp::Atom(S(name)) if name == "false" => Expr::Boolean(false), 
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
                [Sexp::Atom(S(op)), e1, e2] if op == "=" => Expr::BinOp(Op2::Equal, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == ">" => Expr::BinOp(Op2::Greater, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "<" => Expr::BinOp(Op2::Less, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == ">=" => Expr::BinOp(Op2::GreaterEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "<=" => Expr::BinOp(Op2::LessEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2, e3] if op == "if" => Expr::If(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)), Box::new(parse_expr(e3))),
                [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => {Expr::Block(exprs.into_iter().map(parse_expr).collect())},
                [Sexp::Atom(S(op)), name, e] if op == "set!" => {Expr::Set(name.to_string(), Box::new(parse_expr(e)))},
                [Sexp::Atom(S(op)), e] if op == "loop" => {Expr::Loop(Box::new(parse_expr(e)))},
                [Sexp::Atom(S(op)), e] if op == "break" => {Expr::Break(Box::new(parse_expr(e)))},
                [Sexp::Atom(S(op)), Sexp::List(bind_expr), e] if op == "let" => {
                    let mut vars: Vec<(String, Expr)> = Vec::new();
                    for bind in bind_expr {
                        vars.push(parse_bind(bind))
                    }
                    if vars.len() == 0{
                        panic!("Invalid no binding")
                    }
                    Expr::Let(vars, Box::new(parse_expr(e)))
                },
                _ => panic!("Invalid parse error"),
            }
        },
        _ => panic!("Invalid parse error"),
    }
}

fn new_label(l: &mut i32, s: &str) -> String {
  let current = *l;
  *l += 1;
  format!("{s}_{current}")
}

fn parse_bind(s: &Sexp) -> (String, Expr) {
    match s {
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(n)), e] => {
                  match n.as_str() {
                    "input" | "let" | "if" | "block" | "loop" | "break" => {panic!("illegal name, {} is a keyword", s)},
                    _ => (n.to_string(), parse_expr(e))
                  }
                },
                _ => panic!("Invalid"),
            }
        }
        _ => panic!("Invalid"),
    }
}

fn compile_to_instrs(e: &Expr, si: i64, env: &HashMap<String, i64>, brake: &String, l: &mut i32) -> Vec<Instr> {
    let mut instrs: Vec<Instr> = Vec::new();
    match e {
        Expr::Number(n) => {
          let num = *n;
          if num < i64::min_value() >> 1  || num > i64::max_value() >> 1{
            panic!("Invalid");
          }
          instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n<<1)));
        },
        Expr::Boolean(true) => {instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));},
        Expr::Boolean(false) => {instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));},
        Expr::Id(s) => {
            match s.as_str() {
              "input" => {instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI)));},
              "let" | "if" | "block" | "loop" | "break" => {panic!("illegal name, {} is a keyword", s)},
              _ => {
                let bool_key = env.contains_key(s);
                if bool_key == true {
                    let offset = env.get(s).unwrap() * 8;
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP,offset)));
                } else {
                    panic!("Unbound variable identifier {}", s);
                }
              }
            }
        },
        Expr::UnOp(op, expr) => {
            match op {
                Op1:: Add1 => {
                    let mut new_instrs = compile_to_instrs(expr, si, env, brake, l);
                    instrs.append(&mut new_instrs);
                    instrs.push(Instr::Test(Val::Reg(Reg::RAX),  Val::Imm(1)));
                    instrs.push(Instr::Jne("invalid_argument".to_string()));
                    instrs.push(Instr::IAdd(Val::Reg(Reg::RAX),  Val::Imm(2)));
                    instrs.push(Instr::Jo("overflow".to_string()));
                }
                Op1:: Sub1 => {
                    let mut new_instrs = compile_to_instrs(expr, si, env, brake, l);
                    instrs.append(&mut new_instrs);
                    instrs.push(Instr::Test(Val::Reg(Reg::RAX),  Val::Imm(1)));
                    instrs.push(Instr::Jne("invalid_argument".to_string()));
                    instrs.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(2)));
                    instrs.push(Instr::Jo("overflow".to_string()));
                }
                Op1:: IsNum => {
                    let mut new_instrs = compile_to_instrs(expr, si, env, brake, l);
                    instrs.append(&mut new_instrs);
                    instrs.push(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
                    instrs.push(Instr::CMove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                }
                Op1:: IsBool => {
                  let mut new_instrs = compile_to_instrs(expr, si, env, brake, l);
                  instrs.append(&mut new_instrs);
                  instrs.push(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)));
                  instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
                  instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(1)));
                  instrs.push(Instr::CMove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                }
            }
        },
        Expr::BinOp(op, expr1, expr2) => {
            match op {
                Op2::Plus | Op2:: Minus | Op2:: Times => {
                    let mut new_instrs2 = compile_to_instrs(expr2, si, env, brake, l);
                    let stack_offset = si * 8;
                    instrs.append(&mut new_instrs2);
                    instrs.push(Instr::Test(Val::Reg(Reg::RAX),  Val::Imm(1)));
                    instrs.push(Instr::Jne("invalid_argument".to_string()));
                    instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                    let mut new_instrs1 = compile_to_instrs(expr1, si + 1, env, brake, l);
                    instrs.append(&mut new_instrs1);
                    instrs.push(Instr::Test(Val::Reg(Reg::RAX),  Val::Imm(1)));
                    instrs.push(Instr::Jne("invalid_argument".to_string()));
                    match op {
                      Op2::Plus => {
                          instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
                      }
                      Op2::Minus => {
                          instrs.push(Instr::ISub(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
                      }
                      Op2::Times => {
                          instrs.push(Instr::Sar(Val::Reg(Reg::RAX), Val::Imm(1)));
                          instrs.push(Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
                      }
                      _ => {}
                    }
                    instrs.push(Instr::Jo("overflow".to_string()));
                },
                Op2::Equal => {
                    let mut new_instrs1 = compile_to_instrs(expr1, si, env, brake, l);
                    let stack_offset = si * 8;
                    instrs.append(&mut new_instrs1);
                    instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                    let mut new_instrs2 = compile_to_instrs(expr2, si + 1, env, brake, l);
                    instrs.append(&mut new_instrs2);
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::Xor(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, stack_offset)));
                    instrs.push(Instr::Test(Val::Reg(Reg::RBX), Val::Imm(1)));
                    instrs.push(Instr::Jne("invalid_argument".to_string()));
                    instrs.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP,stack_offset)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
                    instrs.push(Instr::CMove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)))
                },
                Op2::Greater | Op2::Less | Op2:: GreaterEqual | Op2:: LessEqual=> {
                    let mut label = String::new();
                    let mut end_label = String::new();
                    let mut com_instrs = Vec::new();
                    let stack_offset = si * 8;
                    match op {
                      Op2::Greater => {
                        label = new_label(l, "greater");
                        end_label = new_label(l, "greaterend");
                        com_instrs = compare_instrs(">", stack_offset, label, end_label);
                      }
                      Op2::Less => {
                        label = new_label(l, "less");
                        end_label = new_label(l, "lessend");
                        com_instrs = compare_instrs("<", stack_offset, label, end_label);
                      }
                      Op2::GreaterEqual => {
                        label = new_label(l, "greaterequal");
                        end_label = new_label(l, "greaterequalend");
                        com_instrs = compare_instrs(">=", stack_offset, label, end_label);
                      }
                      Op2::LessEqual => {
                        label = new_label(l, "lessequal");
                        end_label = new_label(l, "lessequalend");
                        com_instrs = compare_instrs("<=", stack_offset, label, end_label);
                      }
                      _ => {

                      }
                    }
                    let mut new_instrs2 = compile_to_instrs(expr2, si, env, brake, l);
                    instrs.append(&mut new_instrs2);
                    instrs.push(Instr::Test(Val::Reg(Reg::RAX),  Val::Imm(1)));
                    instrs.push(Instr::Jne("invalid_argument".to_string()));
                    instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                    let mut new_instrs1 = compile_to_instrs(expr1, si + 1, env, brake, l);
                    instrs.append(&mut new_instrs1);
                    instrs.push(Instr::Test(Val::Reg(Reg::RAX),  Val::Imm(1)));
                    instrs.push(Instr::Jne("invalid_argument".to_string()));
                    instrs.append(&mut com_instrs);
                },
            }
        },
        Expr::If(condition, thn, els) => {
            let label = new_label(l, "ifelse");
            let end_label = new_label(l, "ifend");
            let mut cond_instrs = compile_to_instrs(condition, si, env, brake, l);
            let stack_offset = si * 8;
            instrs.append(&mut cond_instrs);
            instrs.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::Imm(1)));
            instrs.push(Instr::Je(label.clone()));
            let mut if_instrs = compile_to_instrs(thn, si, env, brake, l);
            instrs.append(&mut if_instrs);
            instrs.push(Instr::Jmp(end_label.clone()));
            instrs.push(Instr::Label(label.clone()));
            let mut else_instrs = compile_to_instrs(els, si, env, brake, l);
            instrs.append(&mut else_instrs);
            instrs.push(Instr::Label(end_label.clone()));
        }
        Expr::Block(es) => {
            if es.len() == 0 {
              panic!("Invalid");
            }
            for block in es {
              instrs.append(&mut compile_to_instrs(block, si, env, brake, l));
            }
        }
        Expr::Set(name, expr) => {
            let bool_key = env.contains_key(name);
            if bool_key == true {
              let offset = env.get(name).unwrap() * 8;
              let mut new_instrs = compile_to_instrs(expr, si, env, brake, l);
              instrs.append(&mut new_instrs);
              instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX)));
            } else {
              panic!("Unbound variable identifier {}", name);
            }
        }
        Expr::Loop(expr) => {
            let startloop = new_label(l, "loop");
            let endloop = new_label(l, "loopend");
            let mut loop_instrs = compile_to_instrs(expr, si, env, &endloop, l);
            instrs.push(Instr::Label(startloop.clone()));
            instrs.append(&mut loop_instrs);
            instrs.push(Instr::Jmp(startloop));
            instrs.push(Instr::Label(endloop.clone()));
        }
        Expr::Break(expr) => {
            if brake.len() == 0 {
              panic!("unpaired break");
            }
            let mut new_instrs = compile_to_instrs(expr, si, env, brake, l);
            instrs.append(&mut new_instrs);
            instrs.push(Instr::Jmp(brake.to_string()));
        }
        Expr::Let(vars, expr) => {
            let mut set: HashSet<String> = HashSet::new();
            let mut dist = env.clone();
            // dist.extend(env.into_iter());
            let mut index = 0;
            for var in vars {
                if set.contains(&var.0) {
                    panic!("Duplicate binding")
                } else {
                    set.insert(var.0.to_string());
                    let mut val_is = compile_to_instrs(&var.1, si + index, &dist, brake, l);
                    dist = dist.update(var.0.to_string(), si + index);
                    instrs.append(&mut val_is);
                    let stack_offset = (si + index) * 8;
                    instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                }
                index += 1;
            }
            let mut body_is = compile_to_instrs(expr, si + index, &dist, brake, l);
            instrs.append(&mut body_is);
        },
    }
    instrs
}

fn compare_instrs(operation: &str, stack_offset: i64,label: String, end_label: String) -> Vec<Instr> {
  let mut instrs: Vec<Instr> = Vec::new();
  instrs.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP,stack_offset)));
  match operation {
    ">" => {
        instrs.push(Instr::Jg(label.clone()));
    }
    "<" => {
      instrs.push(Instr::Jl(label.clone()));
    }
    ">=" => {
      instrs.push(Instr::Jge(label.clone()));
    }
    "<=" => {
      instrs.push(Instr::Jle(label.clone()));
    }
    _ => {
        panic!("wrong operation");
    }
  }
  instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
  instrs.push(Instr::Jmp(end_label.clone()));
  instrs.push(Instr::Label(label.clone()));
  instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
  instrs.push(Instr::Label(end_label.clone()));
  instrs
}

fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(val1, val2) => {
            let s_val1 = val_to_str(val1);
            let s_val2 = val_to_str(val2);
            let str = format!("mov {}, {}\n", s_val1, s_val2);
            return str
        },
        Instr::IAdd(val1, val2) => {
            let s_val1 = val_to_str(val1);
            let s_val2 = val_to_str(val2);
            let str = format!("add {}, {}\n", s_val1, s_val2);
            return str
        },
        Instr::ISub(val1, val2) => {
            let s_val1 = val_to_str(val1);
            let s_val2 = val_to_str(val2);
            let str = format!("sub {}, {}\n", s_val1, s_val2);
            return str
        },
        Instr::IMul(val1, val2) => {
            let s_val1 = val_to_str(val1);
            let s_val2 = val_to_str(val2);
            let str = format!("imul {}, {}\n", s_val1, s_val2);
            return str
        },
        Instr::Test(val1, val2) => {
          let s_val1 = val_to_str(val1);
          let s_val2 = val_to_str(val2);
          let str = format!("test {}, {}\n", s_val1, s_val2);
          return str
        },
        Instr::Jmp(label) => {
            let str = format!("jmp {}\n", label);
            return str
        },
        Instr::Jne(label) => {
          let str = format!("jne {}\n", label);
          return str
        },
        Instr::Je(label) => {
          let str = format!("je {}\n", label);
          return str
        },
        Instr::CMove(val1, val2) => {
          let s_val1 = val_to_str(val1);
          let s_val2 = val_to_str(val2);
          let str = format!("cmove {}, {}\n", s_val1, s_val2);
          return str
        },
        Instr::Cmp(val1, val2) => {
          let s_val1 = val_to_str(val1);
          let s_val2 = val_to_str(val2);
          let str = format!("cmp {}, {}\n", s_val1, s_val2);
          return str
        },
        Instr::Sar(val1, val2) => {
          let s_val1 = val_to_str(val1);
          let s_val2 = val_to_str(val2);
          let str = format!("sar {}, {}\n", s_val1, s_val2);
          return str
        },
        Instr::Jg(label) => {
          let str = format!("jg {}\n", label);
          return str
        },
        Instr::Jl(label) => {
          let str = format!("jl {}\n", label);
          return str
        },
        Instr::Jge(label) => {
          let str = format!("jge {}\n", label);
          return str
        },
        Instr::Jle(label) => {
          let str = format!("jle {}\n", label);
          return str
        },
        Instr::Jo(label) => {
          let str = format!("jo {}\n", label);
          return str
        },
        Instr::Label(label) => {
          let str = format!("{}:\n", label);
          return str
        },
        Instr::Xor(val1, val2) => {
          let s_val1 = val_to_str(val1);
          let s_val2 = val_to_str(val2);
          let str = format!("xor {}, {}\n", s_val1, s_val2);
          return str
        }
    }
}

fn val_to_str(v: &Val) -> String {
    match v {
        Val::Reg(reg) => {
            match reg {
                Reg::RAX => {
                    return format!("rax")
                }
                Reg::RBX => {
                  return format!("rbx")
                }
                Reg::RSP => {
                    return format!("rsp")
                }
                Reg::RDI => {
                    return format!("rdi") 
                }

            }
        },
        Val::Imm(n) => {
            return format!("{}", n)
        },
        Val::RegOffset(reg, offset) => {
            match reg {
                Reg::RAX => {
                  return format!("[rax - {}]", offset);  
                },
                Reg::RBX => {
                  return format!("[rbx - {}]", offset);  
                },
                Reg::RSP => {
                  return format!("[rsp - {}]", offset); 
                },
                Reg::RDI => {
                  return format!("[rdi = {}]", offset);
                }
            }
        },
    }
}

fn compile(e: &Expr, si: i64, env: &HashMap<String, i64>, brake: &String, l: &mut i32) -> String {
    let instrs = compile_to_instrs(e, si, env, brake, l);
    let mut strs = String::new();
    for i in &instrs {
        strs.push_str(&instr_to_str(i))
    }
    return (*strs.trim()).to_string();
}