use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::Atom::*;
use sexp::*;

// use im::HashMap;
use std::collections::HashMap;
use std::collections::HashSet;

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
    RSP,
    RDI,
    RBX,
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAnd(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    ISar(Val, Val),
    IJo(Val),
    IJmp(Val),
    IJnz(Val),
    IJe(Val),
    IJne(Val),
    ITest(Val, Val),
    ICmp(Val, Val),
    ICmovg(Val, Val),
    ICmovge(Val, Val),
    ICmovl(Val, Val),
    ICmovle(Val, Val),
    ICmove(Val, Val),
    IXor(Val, Val),
    ILabel(Val),
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

fn is_keyword(name: &String) -> bool {
    return ["add1", "sub1", "isnum", "isbool", "let", "if", "loop", "block", "break", "set!", "input", "+", "-", "*", "<", "<=", ">", ">=", "="].contains(&name.as_str());
}

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) =>  {
            let num = i64::try_from(*n).unwrap();
            if num > 4611686018427387903 || num < -4611686018427387904 {
                panic!("Invalid: Number Bounds exceeded")
            }
            Expr::Number(num)
        }
        Sexp::Atom(S(name)) if name == "true" => Expr::Boolean(true),
        Sexp::Atom(S(name)) if name == "false" => Expr::Boolean(false),
        Sexp::Atom(S(name)) => Expr::Id(name.to_string()),
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(op)), e] if op == "loop" => {
                    Expr::Loop(Box::new(parse_expr(e)))
                }
                [Sexp::Atom(S(op)), e] if op == "break" => {
                    Expr::Break(Box::new(parse_expr(e)))
                }
                [Sexp::Atom(S(op)), Sexp::Atom(S(name)), e] if op == "set!" => {
                    Expr::Set(name.to_string(), Box::new(parse_expr(e)))
                }
                [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => {
                    Expr::Block(exprs.into_iter().map(parse_expr).collect())
                }
                [Sexp::Atom(S(op)), e] if op == "add1" => Expr::UnOp(Op1::Add1, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "sub1" => Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "isnum" => Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "isbool" => Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e1, e2] if op == "+" => Expr::BinOp(Op2::Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                // NOTE: to make sure we get the correct codegen, we swap the expr order here
                [Sexp::Atom(S(op)), e1, e2] if op == "-" => Expr::BinOp(Op2::Minus, Box::new(parse_expr(e2)), Box::new(parse_expr(e1))),
                [Sexp::Atom(S(op)), e1, e2] if op == "*" => Expr::BinOp(Op2::Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "=" => Expr::BinOp(Op2::Equal, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == ">" => Expr::BinOp(Op2::Greater, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == ">=" => Expr::BinOp(Op2::GreaterEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "<" => Expr::BinOp(Op2::Less, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "<=" => Expr::BinOp(Op2::LessEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(keyword)), cond, thn, els] if keyword == "if" => Expr::If(
                    Box::new(parse_expr(cond)),
                    Box::new(parse_expr(thn)),
                    Box::new(parse_expr(els)),
                ),
                [Sexp::Atom(S(op)), Sexp::List(bindings), body] if op == "let" => {
                    let mut binding_vec = Vec::new();
                    let mut name_set = HashSet::new();
                    if bindings.is_empty() {
                        panic!("Invalid");
                    }
                    for binding in bindings.into_iter() {
                        match &binding {
                            Sexp::List(b) => match &b[..] {
                                [Sexp::Atom(S(name)), val] => {
                                    if is_keyword(name) {
                                        panic!("keyword used as Identifier");
                                    }
                                    if name_set.contains(name) {
                                        panic!("Duplicate binding");
                                    }
                                    name_set.insert(name);
                                    let binding_expr = (name.to_string(), parse_expr(val));
                                    binding_vec.push(binding_expr);
                                }
                                _ => panic!("Invalid"),
                            }
                            _ => panic!("Invalid"),
                        }
                    }
                    Expr::Let(binding_vec, Box::new(parse_expr(body)))
                }
                _ => panic!("Invalid"),
            }
        },
        _ => panic!("Invalid"),
    }
}


fn generate_overflow_check(instrs: &mut Vec<Instr>) {
    // Jump on overflow
    let jo_instr = Instr::IJo(Val::Label("throw_overflow_error".to_string()));
    instrs.push(jo_instr);
}

fn generate_check_not_boolean(instrs: &mut Vec<Instr>) {
    let test_instr = Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1));
    instrs.push(test_instr);
    let mov_instr = Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(8));
    instrs.push(mov_instr);
    let jnz_instr = Instr::IJnz(Val::Label("throw_error".to_string()));
    instrs.push(jnz_instr);
}

fn new_label(l: &mut i32, s: &str) -> String {
    let current = *l;
    *l += 1;
    format!("{s}_{current}")
}

fn compile_expr(e: &Expr, si: i64, env: &mut HashMap<String, i64>, instrs: &mut Vec<Instr>, label: &mut i32, brake: &String) {
    match e {
        Expr::Number(n) => {
            let instr = Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n << 1));
            instrs.push(instr)
        }
        Expr::Boolean(b) => {
            match b {
                true => {
                    let instr = Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3));
                    instrs.push(instr)
                }
                false => {
                    let instr = Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1));
                    instrs.push(instr)
                }
            }
        }
        Expr::Id(name) => {
            if name == "input" {
                let instr = Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI));
                instrs.push(instr)
            } else if env.contains_key(name) {
                let stack_offset = env.get(name).unwrap() * 8;
                let instr = Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset));
                instrs.push(instr)
            } else {
                panic!("Unbound variable identifier {}", name)
            }
        }
        Expr::If(cond, thn, els) => {
            let end_label = new_label(label, "ifend");
            let else_label = new_label(label, "ifelse");
            compile_expr(cond, si, env, instrs, label, brake);
            let cmp_instr = Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(1));
            instrs.push(cmp_instr);
            let je_instr = Instr::IJe(Val::Label(else_label.clone()));
            instrs.push(je_instr);
            compile_expr(thn, si, env, instrs, label, brake);
            let jmp_instr = Instr::IJmp(Val::Label(end_label.clone()));
            instrs.push(jmp_instr);
            let else_label_instr = Instr::ILabel(Val::Label(else_label.clone()));
            instrs.push(else_label_instr);
            compile_expr(els, si, env, instrs, label, brake);
            let end_label_instr = Instr::ILabel(Val::Label(end_label.clone()));
            instrs.push(end_label_instr);
        }
        Expr::Let(bindings, body) => {
            let size: i64 = bindings.len() as i64;
            let mut new_env = env.clone();
            for (i, binding) in bindings.iter().enumerate() {
                let (name, expr) = binding;
                compile_expr(expr, si + (i as i64), &mut new_env, instrs, label, brake);
                new_env.insert(name.to_string(), si + (i as i64));
                let stack_offset = (si + (i as i64)) * 8;
                let mov_instr = Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX));
                instrs.push(mov_instr);
            }
            compile_expr(body, si + size, &mut new_env, instrs, label, brake);
        }
        Expr::UnOp(op, expr) => {
            compile_expr(expr, si, env, instrs, label, brake);
            match op {
                Op1::Add1 => {
                    generate_check_not_boolean(instrs);
                    let plus_instr =  Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(2));
                    instrs.push(plus_instr);
                    generate_overflow_check(instrs);
                },
                Op1::Sub1 => {
                    generate_check_not_boolean(instrs);
                    let sub_instr =  Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(2));
                    instrs.push(sub_instr);
                    generate_overflow_check(instrs);
                },
                Op1::IsNum => {
                    let and_instr = Instr::IAnd(Val::Reg(Reg::RAX), Val::Imm(1));
                    instrs.push(and_instr);
                    let cmp_instr = Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(0));
                    instrs.push(cmp_instr);
                    let mov_rbx_instr = Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3));
                    instrs.push(mov_rbx_instr);
                    let mov_rax_instr = Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1));
                    instrs.push(mov_rax_instr);
                    let cmove_instr = Instr::ICmove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX));
                    instrs.push(cmove_instr);
                },
                Op1::IsBool => {
                    let and_instr = Instr::IAnd(Val::Reg(Reg::RAX), Val::Imm(1));
                    instrs.push(and_instr);
                    let cmp_instr = Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(1));
                    instrs.push(cmp_instr);
                    let mov_rbx_instr = Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3));
                    instrs.push(mov_rbx_instr);
                    let mov_rax_instr = Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1));
                    instrs.push(mov_rax_instr);
                    let cmove_instr = Instr::ICmove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX));
                    instrs.push(cmove_instr);
                }
            }
        }
        Expr::BinOp(op, expr1, expr2) => {
            match op {
                Op2::Plus => {
                    compile_expr(expr1, si, env, instrs, label, brake);
                    generate_check_not_boolean(instrs);
                    let stack_offset = si * 8;
                    let mov_instr = Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX));
                    instrs.push(mov_instr);
                    compile_expr(expr2, si + 1, env, instrs, label, brake);
                    generate_check_not_boolean(instrs);
                    let plus_instr = Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset));
                    instrs.push(plus_instr);
                    generate_overflow_check(instrs);
                },
                Op2::Minus => {
                    compile_expr(expr1, si, env, instrs, label, brake);
                    generate_check_not_boolean(instrs);
                    let stack_offset = si * 8;
                    let mov_instr = Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX));
                    instrs.push(mov_instr);
                    compile_expr(expr2, si + 1, env, instrs, label, brake);
                    generate_check_not_boolean(instrs);
                    let sub_instr = Instr::ISub(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset));
                    instrs.push(sub_instr);
                    generate_overflow_check(instrs);
                },
                Op2::Times => {
                    compile_expr(expr1, si, env, instrs, label, brake);
                    generate_check_not_boolean(instrs);
                    let stack_offset = si * 8;
                    let mov_instr = Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX));
                    instrs.push(mov_instr);
                    compile_expr(expr2, si + 1, env, instrs, label, brake);
                    generate_check_not_boolean(instrs);
                    let sar_instr = Instr::ISar(Val::Reg(Reg::RAX), Val::Imm(1));
                    instrs.push(sar_instr);
                    let mul_instr = Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset));
                    instrs.push(mul_instr);
                    generate_overflow_check(instrs);
                },
                Op2::Equal => {
                    compile_expr(expr1, si, env, instrs, label, brake);
                    let stack_offset = si * 8;
                    let mov_instr = Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX));
                    instrs.push(mov_instr);
                    compile_expr(expr2, si + 1, env, instrs, label, brake);

                    // check if type matches
                    let mov1_instr = Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX));
                    instrs.push(mov1_instr);
                    let xor_instr = Instr::IXor(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, stack_offset));
                    instrs.push(xor_instr);
                    let test_instr = Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1));
                    instrs.push(test_instr);
                    let mov2_instr = Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(7));
                    instrs.push(mov2_instr);
                    let jne_instr = Instr::IJne(Val::Label("throw_error".to_string()));
                    instrs.push(jne_instr);

                    let cmp_instr = Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset));
                    instrs.push(cmp_instr);
                    let mov_rbx_instr = Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3));
                    instrs.push(mov_rbx_instr);
                    let mov_rax_instr = Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1));
                    instrs.push(mov_rax_instr);
                    let cmove_instr = Instr::ICmove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX));
                    instrs.push(cmove_instr);
                }
                Op2::Greater => {
                    compile_expr(expr1, si, env, instrs, label, brake);
                    generate_check_not_boolean(instrs);
                    let stack_offset = si * 8;
                    let mov_instr = Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX));
                    instrs.push(mov_instr);
                    compile_expr(expr2, si + 1, env, instrs, label, brake);
                    generate_check_not_boolean(instrs);
                    let cmp_instr = Instr::ICmp(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX));
                    instrs.push(cmp_instr);
                    let mov_rbx_instr = Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3));
                    instrs.push(mov_rbx_instr);
                    let mov_rax_instr = Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1));
                    instrs.push(mov_rax_instr);
                    let cmovg_instr = Instr::ICmovg(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX));
                    instrs.push(cmovg_instr);
                },
                Op2::GreaterEqual => {
                    compile_expr(expr1, si, env, instrs, label, brake);
                    generate_check_not_boolean(instrs);
                    let stack_offset = si * 8;
                    let mov_instr = Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX));
                    instrs.push(mov_instr);
                    compile_expr(expr2, si + 1, env, instrs, label, brake);
                    generate_check_not_boolean(instrs);
                    let cmp_instr = Instr::ICmp(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX));
                    instrs.push(cmp_instr);
                    let mov_rbx_instr = Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3));
                    instrs.push(mov_rbx_instr);
                    let mov_rax_instr = Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1));
                    instrs.push(mov_rax_instr);
                    let cmovge_instr = Instr::ICmovge(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX));
                    instrs.push(cmovge_instr);
                },
                Op2::Less => {
                    compile_expr(expr1, si, env, instrs, label, brake);
                    generate_check_not_boolean(instrs);
                    let stack_offset = si * 8;
                    let mov_instr = Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX));
                    instrs.push(mov_instr);
                    compile_expr(expr2, si + 1, env, instrs, label, brake);
                    generate_check_not_boolean(instrs);
                    let cmp_instr = Instr::ICmp(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX));
                    instrs.push(cmp_instr);
                    let mov_rbx_instr = Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3));
                    instrs.push(mov_rbx_instr);
                    let mov_rax_instr = Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1));
                    instrs.push(mov_rax_instr);
                    let cmovl_instr = Instr::ICmovl(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX));
                    instrs.push(cmovl_instr);
                },
                Op2::LessEqual => {
                    compile_expr(expr1, si, env, instrs, label, brake);
                    generate_check_not_boolean(instrs);
                    let stack_offset = si * 8;
                    let mov_instr = Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX));
                    instrs.push(mov_instr);
                    compile_expr(expr2, si + 1, env, instrs, label, brake);
                    generate_check_not_boolean(instrs);
                    let cmp_instr = Instr::ICmp(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX));
                    instrs.push(cmp_instr);
                    let mov_rbx_instr = Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3));
                    instrs.push(mov_rbx_instr);
                    let mov_rax_instr = Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1));
                    instrs.push(mov_rax_instr);
                    let cmovle_instr = Instr::ICmovle(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX));
                    instrs.push(cmovle_instr);
                },
            }
        },
        Expr::Loop(e) => {
            let startloop = new_label(label, "loop");
            let startloop_label_instr = Instr::ILabel(Val::Label(startloop.clone()));
            instrs.push(startloop_label_instr);
            let endloop = new_label(label, "loopend");
            compile_expr(e, si, env, instrs, label, &endloop);
            let jmp_instr = Instr::IJmp(Val::Label(startloop.clone()));
            instrs.push(jmp_instr);
            let endloop_label_instr = Instr::ILabel(Val::Label(endloop.clone()));
            instrs.push(endloop_label_instr);
        },
        Expr::Break(e) => {
            if brake == "" {
                panic!("break outside of loop")
            }
            compile_expr(e, si, env, instrs, label, brake);
            let jmp_instr = Instr::IJmp(Val::Label(brake.clone()));
            instrs.push(jmp_instr);
        },
        Expr::Set(name, val) => {
            if env.contains_key(name) {
                compile_expr(val, si, env, instrs, label, brake);
                let stack_offset = env.get(name).unwrap() * 8;
                let instr = Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX));
                instrs.push(instr);
            } else {
                panic!("Unbound variable identifier {}", name)
            }
        },
        Expr::Block(es) => {
            if es.is_empty() {
                panic!("Invalid: empty block")
            }
            for e in es.into_iter() {
                compile_expr(e, si, env, instrs, label, brake);
            }
        }
    }
}


fn compile_to_instrs(e: &Expr) -> Vec<Instr> {
    // the stack offset starts with 2
    let si = 2;
    let mut env = HashMap::new();
    let mut instrs = Vec::new();
    let mut labels = 0;
    compile_expr(&e, si, &mut env, &mut instrs, &mut labels, &String::from(""));
    instrs
}


fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(v1, v2) => {
            format!("  mov {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::IAnd(v1, v2) => {
            format!("  and {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::IAdd(v1, v2) => {
            format!("  add {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::IMul(v1, v2) => {
            format!("  imul {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::ISub(v1, v2) => {
            format!("  sub {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::ISar(v1, v2) => {
            format!("  sar {}, {}", val_to_str(v1), val_to_str(v2))
        }
        Instr::IJo(v) => {
            format!("  jo {}", val_to_str(v))
        },
        Instr::IJmp(v) => {
            format!("  jmp {}", val_to_str(v))
        },
        Instr::IJnz(v) => {
            format!("  jnz {}", val_to_str(v))
        },
        Instr::IJe(v) => {
            format!("  je {}", val_to_str(v))
        },
        Instr::IJne(v) => {
            format!("  jne {}", val_to_str(v))
        },
        Instr::ITest(v1, v2) => {
            format!("  test {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::ICmp(v1, v2) => {
            format!("  cmp {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::ICmovg(v1, v2) => {
            format!("  cmovg {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::ICmovge(v1, v2) => {
            format!("  cmovge {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::ICmovl(v1, v2) => {
            format!("  cmovl {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::ICmovle(v1, v2) => {
            format!("  cmovle {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::ICmove(v1, v2) => {
            format!("  cmove {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::IXor(v1, v2) => {
            format!("  xor {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::ILabel(v) => {
            format!("{}: ", val_to_str(v))
        }
    }
}

fn val_to_str(v: &Val) -> String {
    match v {
        Val::Imm(n) => {
            n.to_string()
        },
        Val::Reg(reg) => {
            match reg {
                Reg::RAX => {
                    "rax".to_string()
                },
                Reg::RSP => {
                    "rsp".to_string()
                },
                Reg::RDI => {
                    "rdi".to_string()
                },
                Reg::RBX => {
                    "rbx".to_string()
                }
            }
        },
        Val::RegOffset(reg, offset) => {
            match reg {
                Reg::RAX => {
                    format!("[rax - {}]", offset)
                },
                Reg::RSP => {
                    format!("[rsp - {}]", offset)
                },
                Reg::RDI => {
                    format!("[rdi - {}]", offset)
                },
                Reg::RBX => {
                    format!("[rbx - {}]", offset)
                }
            }
        },
        Val::Label(s) => {
            s.to_string()
        }
    }
}


fn compile(e: &Expr) -> String {
    let instructions = compile_to_instrs(e);
    let mut result = String::new();
    for instr in &instructions {
        result += instr_to_str(instr).as_str();
        result += "\n";
    }
    result
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;
    let sexpr = match parse(&in_contents) {
        Ok(sexpr)  => sexpr,
        Err(_) => panic!("Invalid"),
    };
    let expr = parse_expr(&sexpr);

    // You will make result hold the result of actually compiling
    let result = compile(&expr);

    let asm_program = format!(
        "
section .text
extern snek_error
throw_overflow_error:
    mov rdi, 9
    push rsp
    call snek_error
    ret
throw_error:
    mov rdi, rbx
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
