use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::Atom::*;
use sexp::*;

use im::HashMap;
use im::HashSet;
#[derive(Debug)]
enum Val {
    Reg(Reg),
    Imm(i64),
    RegOffset(Reg, i32),
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
    ITest(Val, Val),
    IJmp(String),
    // IJz(String),
    IJnz(String),
    Lable(String),
    IXor(Val, Val),
    IOr(Val, Val),
    // IJne(String),
    IJe(String),
    IJo(String),
    ICmove(Val, Val),
    ICmovl(Val, Val),
    ICmovle(Val, Val),
    ICmovg(Val, Val),
    ICmovge(Val, Val),
    ICmovz(Val, Val),
    ICmovnz(Val, Val),
    ICmp(Val, Val),
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
    let keywords = std::collections::HashSet::from(["true".to_string(), "false".to_string(), "input".to_string(), "let".to_string(), "set!".to_string(), 
                                                                    "if".to_string(), "block".to_string(), "loop".to_string(), "break".to_string(), 
                                                                    "add1".to_string(), "sub1".to_string(), "isnum".to_string(), "isbool".to_string()]);
    match s {
        Sexp::Atom(I(n)) => {
            if ((n >> 62) & 1) ^ ((n >> 63) & 1) == 1 {
                panic!("Invalid")
            } else {
                Expr::Number(*n)
            }
        }
        Sexp::Atom(S(name)) if name == "true" => Expr::Boolean(true),
        Sexp::Atom(S(name)) if name == "false" => Expr::Boolean(false),
        Sexp::Atom(S(s)) => Expr::Id(s.to_string()),
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(op)), e] if op == "add1" => Expr::UnOp(Op1::Add1, Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "sub1" => Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "isnum" => Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "isbool" => Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e1, e2] if op == "+" => Expr::BinOp(Op2::Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
            [Sexp::Atom(S(op)), e1, e2] if op == "-" => Expr::BinOp(Op2::Minus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
            [Sexp::Atom(S(op)), e1, e2] if op == "*" => Expr::BinOp(Op2::Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
            [Sexp::Atom(S(op)), e1, e2] if op == "=" => Expr::BinOp(Op2::Equal, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
            [Sexp::Atom(S(op)), e1, e2] if op == "<" => Expr::BinOp(Op2::Less, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
            [Sexp::Atom(S(op)), e1, e2] if op == ">" => Expr::BinOp(Op2::Greater, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
            [Sexp::Atom(S(op)), e1, e2] if op == "<=" => Expr::BinOp(Op2::LessEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
            [Sexp::Atom(S(op)), e1, e2] if op == ">=" => Expr::BinOp(Op2::GreaterEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
            [Sexp::Atom(S(op)), Sexp::List(bs), e] if op == "let" => {
                let mut vec_binds : Vec<(String, Expr)> = Vec::new();
                if bs.is_empty() {
                    panic!("Invalid")
                }
                for b in bs {
                    match parse_bind(b) {
                        (s, _) if keywords.contains(&s) => panic!("Can't take keywords as identifier"),
                        r_ => vec_binds.push(r_),
                    };
                }
                Expr::Let(vec_binds, Box::new(parse_expr(e)))
            },
            [Sexp::Atom(S(op)), Sexp::Atom(S(name)), e] if op == "set!" => {
                if keywords.contains(name) {
                    panic!("Can't take keywords as identifier")
                }
                Expr::Set(name.to_string(), Box::new(parse_expr(e)))
            },
            [Sexp::Atom(S(op)), e1, e2, e3] if op == "if" => Expr::If(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)), Box::new(parse_expr(e3))),
            [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => {
                if exprs.is_empty() {
                    panic!("Invalid")
                }
                Expr::Block(exprs.into_iter().map(parse_expr).collect())
            },
            [Sexp::Atom(S(op)), e] if op == "loop" => Expr::Loop(Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "break" => Expr::Break(Box::new(parse_expr(e))),
            _ => panic!("Invalid"),
        },
        _ => panic!("Invalid"),
    }
}

fn parse_bind(s: &Sexp) -> (String, Expr) {
    match s { 
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(id)), e] => (id.to_string(), parse_expr(e)),
                _ => panic!("Invalid"),
            } 
        },
        _ => panic!("Invalid"),
    }
}
fn new_label(l: &mut i32, s: &str) -> String {
    let current = *l;
    *l += 1;
    format!("{s}_{current}")
}
fn compile_to_instrs(e: &Expr) -> Vec<Instr> {
    _compile_to_instrs(e, 2, &mut HashMap::new(), &"".to_string(), &mut 0)
}

fn _compile_to_instrs(e: &Expr, si : i32, env : &HashMap<String, i32>, brake: &String, l : &mut i32) -> Vec<Instr> {
    let mut vec : Vec<Instr> = Vec::new();
    let offset = si * 8;
    match e {
        Expr::Number(n) => vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n << 1))),
        Expr::Boolean(b) => vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(if *b {3} else {1}))),
        Expr::Id(s) if s == "input" => vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI))),
        Expr::Id(s) => {
            match env.get(s) {
                Some(offset_) => vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, *offset_))),
                None => panic!("Unbound variable identifier {}", s),
            }
        },
        Expr::UnOp(op, e1) => {
            vec.append(&mut _compile_to_instrs(e1, si, env, brake, l));
            vec.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
            match op {
                Op1::Add1 => {
                    vec.push(Instr::IJnz("throw_error_2".to_string()));
                    vec.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(2)));
                    vec.push(Instr::IJo("throw_error_1".to_string()));
                },
                Op1::Sub1 => {
                    vec.push(Instr::IJnz("throw_error_2".to_string()));
                    vec.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(2)));
                    vec.push(Instr::IJo("throw_error_1".to_string()));
                },
                _ => {
                    vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
                    vec.push(
                        match op {
                            Op1::IsNum => Instr::ICmovz(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)),
                            Op1::IsBool => Instr::ICmovnz(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)),
                            _ => panic!("Shouldn't appear this error"),
                        }
                    );
                }
            };
        },
        Expr::BinOp(op, e1, e2) => {
            vec.append(&mut _compile_to_instrs(e2, si, env, brake, l));
            vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX)));
            vec.append(&mut _compile_to_instrs(e1, si + 1, env, brake, l));
            vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
            match op {
                Op2::Equal => vec.push(Instr::IXor(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, offset))),
                _ => vec.push(Instr::IOr(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, offset))),
            };
            vec.push(Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1)));
            vec.push(Instr::IJnz("throw_error_2".to_string()));
            match op {
                Op2::Plus => {
                    vec.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset)));
                    vec.push(Instr::IJo("throw_error_1".to_string()));
                },
                Op2::Minus => {
                    vec.push(Instr::ISub(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset)));
                    vec.push(Instr::IJo("throw_error_1".to_string()))
                },
                Op2::Times => {
                    vec.push(Instr::ISar(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset)));
                    vec.push(Instr::IJo("throw_error_1".to_string()))
                },
                _ => {
                    vec.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push( 
                        match op {
                            Op2::Equal => Instr::ICmove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)),
                            Op2::Less => Instr::ICmovl(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)),
                            Op2::LessEqual => Instr::ICmovle(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)),
                            Op2::Greater => Instr::ICmovg(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)),
                            Op2::GreaterEqual => Instr::ICmovge(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)),
                            _ => panic!("Shouldn't appear this error"),
                        }
                    );
                }
            };
        },
        Expr::Let(bindings, e1) => {
            let mut name_set : HashSet<String> = HashSet::new();
            let mut sj = si;
            let mut new_env = env.clone();
            for (id, e2) in bindings {
                if name_set.contains(id) {
                    panic!("Duplicate binding");
                }
                vec.append(&mut _compile_to_instrs(e2, sj, &new_env, brake, l));
                vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, sj * 8), Val::Reg(Reg::RAX)));
                name_set = name_set.update(id.to_string());
                new_env = new_env.update(id.to_string(), sj * 8);
                sj += 1;
            }

            vec.append(&mut _compile_to_instrs(e1, sj, &new_env, brake, l));
        },
        Expr::If(cond, thn, els) => {
            let end_label = new_label(l, "ifend");
            let else_label = new_label(l, "ifelse");
            vec.append(&mut _compile_to_instrs(cond, si, env, brake, l));
            vec.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(1)));
            vec.push(Instr::IJe(else_label.clone()));
            vec.append(&mut _compile_to_instrs(thn, si, env, brake, l));
            vec.push(Instr::IJmp(end_label.clone()));
            vec.push(Instr::Lable(else_label.clone()));
            vec.append(&mut _compile_to_instrs(els, si, env, brake, l));
            vec.push(Instr::Lable(end_label.clone()));
        },
        Expr::Loop(e) => {
            let startloop = new_label(l, "loop");
            let endloop = new_label(l, "loopend");
            vec.push(Instr::Lable(startloop.clone()));
            vec.append(&mut _compile_to_instrs(e, si, env, &endloop, l));
            vec.push(Instr::IJmp(startloop.clone()));
            vec.push(Instr::Lable(endloop.clone()));
        },
        Expr::Break(e) => {
            if brake == "" {
                panic!("Outside loop break");
            }
            vec.append(&mut _compile_to_instrs(e, si, env, brake, l));
            vec.push(Instr::IJmp(brake.clone()));
        }
        Expr::Set(name, val) => {
            match env.get(name) {
                Some(offset_) => {
                    vec.append(&mut _compile_to_instrs(val, si, env, brake, l));
                    vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, *offset_), Val::Reg(Reg::RAX)));
                },
                None => panic!("Unbound variable identifier {}", name),
            };
        },
        Expr::Block(es) => {
            for e_ in es {
                vec.append(&mut _compile_to_instrs(e_, si, env, brake, l));
            }
        },
    };
    vec
}

fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IAdd(v1, v2) => format!("add {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ISub(v1, v2) => format!("sub {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::IMul(v1, v2) => format!("imul {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::IMov(v1, v2) => format!("mov {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ICmove(v1, v2) => format!("cmove {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ICmovg(v1, v2) => format!("cmovg {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ICmovge(v1, v2) => format!("cmovge {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ICmovl(v1, v2) => format!("cmovl {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ICmovle(v1, v2) => format!("cmovle {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ICmovnz(v1, v2) => format!("cmovnz {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ICmovz(v1, v2) => format!("cmovz {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ICmp(v1, v2) => format!("cmp {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ITest(v1, v2) => format!("test {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::IXor(v1, v2) => format!("xor {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::IOr(v1, v2) => format!("or {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::IJmp(label) => format!("jmp {}", label),
        Instr::IJe(label) => format!("je {}", label),
        // Instr::IJne(label) => format!("jne {}", label),
        Instr::IJnz(label) => format!("jnz {}", label),
        Instr::IJo(label) => format!("jo {}", label),
        Instr::Lable(label) => format!("{}:", label),
        Instr::ISar(v1, v2) => format!("sar {}, {}", val_to_str(v1), val_to_str(v2)),
    }
}

fn val_to_str(v: &Val) -> String {
    match v {
        Val::Reg(r) => reg_to_str(r),
        Val::Imm(n) => n.to_string(),
        Val::RegOffset(r, n) => format!(
            "[{} - {}]", reg_to_str(r), n.to_string()
        ),
    }
}
fn reg_to_str(r: &Reg) -> String {
    match r {
        Reg::RAX => "rax".to_string(),
        Reg::RSP => "rsp".to_string(),
        Reg::RBX => "rbx".to_string(),
        Reg::RDI => "rdi".to_string(),
    }
}
fn compile(e: &Expr) -> String {
    compile_to_instrs(e).into_iter().map(|e_|instr_to_str(&e_)).collect::<Vec<String>>().join("\n")
    // let mut ret_string = String::new();
    // for instr in compile_to_instrs(e) {
    //     ret_string.push('\n');
    //     ret_string.push_str(&instr_to_str(&instr));
    // }
    // ret_string
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;
    let parse_result = parse(&in_contents);
    match parse_result {
        Ok(se) => {    
            let expr = parse_expr(&se);
            let result = compile(&expr);
            let asm_program = format!(
"
section .text
global our_code_starts_here
extern snek_error
throw_error_1:
    mov rdi, 1
    push rsp
    call snek_error
    ret
throw_error_2:
    mov rdi, 2
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
        },
        Err(_) => {panic!("Invalid")},
    }
}
