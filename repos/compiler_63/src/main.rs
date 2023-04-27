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
    RegOffset(Reg, i64),
    Label(String),
}

#[derive(Debug)]
enum Reg {
    RAX,
    RSP,
    RDI,
    RBX,
    RCX, // store rdi
    AL,
}

#[derive(Debug)]
enum Instr {
    Label(String),
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    ITest(Val, Val),
    IXor(Val, Val),
    ICmp(Val, Val),
    // ICmove(Val, Val),
    IAnd(Val, Val),
    ISar(Val, Val),
    INeg(Val),
    IJe(Val),
    // IJg(Val),
    // IJge(Val),
    // IJl(Val),
    // IJle(Val),
    ISete(Val),
    ISetg(Val),
    ISetge(Val),
    ISetl(Val),
    ISetle(Val),
    // IJne(Val),
    IJo(Val),
    IJnz(Val),
    IJmp(Val),
}

#[derive(Debug)]
enum Op1 { Add1, Sub1, IsNum, IsBool, }

#[derive(Debug)]
enum Op2 { Plus, Minus, Times, Equal, Greater, GreaterEqual, Less, LessEqual, }

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
        Sexp::Atom(I(n)) => {
            match n.checked_mul(2) {
                Some(_) => Expr::Number(*n),
                None => panic!("Invalid"),
            }
        },
        Sexp::Atom(S(s)) => {
            match s.as_str() {
                "true" => Expr::Boolean(true),
                "false" => Expr::Boolean(false),
                "input" => Expr::Id("input".to_string()),
                _ => {
                    if check_keyword(s) {
                        panic!("keyword");
                    }
                    Expr::Id(s.to_string())
                }
            }
        }
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(keyword)), Sexp::List(bindings), e] if keyword == "let" => {
                    if bindings.len() == 0 {
                        panic!("Invalid let binding");
                    }
                    Expr::Let(bindings.iter().map(|binding| parse_bind(binding)).collect(), Box::new(parse_expr(e)))
                },
                [Sexp::Atom(S(keyword)), Sexp::Atom(S(name)), e] if keyword == "set!" => {
                    Expr::Set(name.to_string(), Box::new(parse_expr(e)))
                },
                [Sexp::Atom(S(keyword)), condition, thn, els] if keyword == "if" => {
                    Expr::If(Box::new(parse_expr(condition)), Box::new(parse_expr(thn)), Box::new(parse_expr(els)))
                },
                [Sexp::Atom(S(keyword)), rest @ ..] if keyword == "block" => {
                    if rest.len() == 0 {
                        panic!("Invalid block");
                    }
                    Expr::Block(rest.iter().map(|e| parse_expr(e)).collect())
                },
                [Sexp::Atom(S(keyword)), e] if keyword == "loop" => Expr::Loop(Box::new(parse_expr(e))),
                [Sexp::Atom(S(keyword)), e] if keyword == "break" => Expr::Break(Box::new(parse_expr(e))),
                [Sexp::Atom(S(keyword)), e] if keyword == "add1" => Expr::UnOp(Op1::Add1, Box::new(parse_expr(e))),
                [Sexp::Atom(S(keyword)), e] if keyword == "sub1" => Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e))),
                [Sexp::Atom(S(keyword)), e] if keyword == "isnum" => Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e))),
                [Sexp::Atom(S(keyword)), e] if keyword == "isbool" => Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e))),
                [Sexp::Atom(S(keyword)), e1, e2] if keyword == "+" => Expr::BinOp(Op2::Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(keyword)), e1, e2] if keyword == "-" => Expr::BinOp(Op2::Minus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(keyword)), e1, e2] if keyword == "*" => Expr::BinOp(Op2::Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(keyword)), e1, e2] if keyword == "=" => Expr::BinOp(Op2::Equal, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(keyword)), e1, e2] if keyword == ">" => Expr::BinOp(Op2::Greater, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(keyword)), e1, e2] if keyword == "<" => Expr::BinOp(Op2::Less, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(keyword)), e1, e2] if keyword == ">=" => Expr::BinOp(Op2::GreaterEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(keyword)), e1, e2] if keyword == "<=" => Expr::BinOp(Op2::LessEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                _ => panic!("Invalid"),
            }
        },
        _ => panic!("Invalid"),
    }
}

fn parse_bind(s: &Sexp) -> (String, Expr) {
    match s {
        Sexp::Atom(_) => panic!("Invalid"),
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(id)), e] => {
                    if check_keyword(id) {
                        panic!("keyword")
                    }
                    (id.to_string(), parse_expr(e))
                }
                _ => panic!("Invalid")
            }
        }
    }
}

fn check_keyword(s: &String) -> bool { // can we modify "input"?
    if vec!["add1", "sub1", "let", "isnum", "isbool", "true", "input",
     "false", "block", "loop", "break", "if", "set!"].iter().any(|w| w == s) {
        return true;
    }
    false
}

fn new_label(l: &mut i32, s: &str) -> String {
    let current = *l;
    *l += 1;
    format!("{s}_{current}")
}

fn check_type(ins: &mut Vec<Instr>) {
    ins.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
    ins.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
    ins.push(Instr::IJnz(Val::Label("throw_error".to_string())));
}

fn check_overflow(ins: &mut Vec<Instr>) {
    ins.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(7)));
    ins.push(Instr::IJo(Val::Label("throw_error".to_string())));
}

fn compile_to_instrs(e: &Expr) -> Vec<Instr> {
    let mut instructions: Vec<Instr> = Vec::new();
    let mut label = 0;

    compile_expr(&mut instructions, e, 2, &HashMap::new(), &String::from(""), &mut label);

    instructions
}

fn compile_expr(ins: &mut Vec<Instr>, e: &Expr, si:i64, env: &HashMap<String, i64>, brake: &String, l: &mut i32) {
    match e {
        Expr::Number(n) => ins.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n<<1))),
        Expr::Boolean(b) => {
            if *b {
                ins.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
            } else {
                ins.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
            }
        },
        Expr::Id(s) => {
            if s == "input" {
                ins.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI)));
            } else {
                match env.get(s) {
                    Some(i) => ins.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, *i))),
                    None => panic!("Unbound variable identifier {s}"),
                }
            }
        },
        Expr::Let(bindings, expr) => {
            // first check if there are duplicate bindings
            let mut names: HashSet<&String> = HashSet::new();
            for binding in bindings {
                if !names.insert(&binding.0) {
                    panic!("Duplicate binding");
                }
            }
            // bind one by one
            let mut new_env = HashMap::clone(env);
            let mut new_si = si;
            for (id, subexpr) in bindings {
                compile_expr(ins, subexpr, new_si, &new_env, brake, l);
                new_env.insert(id.to_string(), new_si*8);
                ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, new_si*8), Val::Reg(Reg::RAX)));
                new_si += 1;
            }
            compile_expr(ins, expr, new_si, &new_env, brake, l);
        },
        Expr::UnOp(op1, subexpr) => {
            compile_expr(ins, subexpr, si, env, brake, l);
            match op1 {
                Op1::Add1 | Op1:: Sub1 => {
                    // first check type
                    check_type(ins);
                    if matches!(op1, Op1::Add1) {
                        ins.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(2)));
                    } else {
                        ins.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(2)));
                    }
                    // finally check if overflow
                    check_overflow(ins);
                }
                Op1::IsBool => {
                    ins.push(Instr::IAnd(Val::Reg(Reg::RAX), Val::Imm(1)));
                    ins.push(Instr::IMul(Val::Reg(Reg::RAX), Val::Imm(2)));
                    ins.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1)));
                },
                Op1::IsNum => {
                    ins.push(Instr::IAnd(Val::Reg(Reg::RAX), Val::Imm(1)));
                    ins.push(Instr::IXor(Val::Reg(Reg::RAX), Val::Imm(1)));
                    ins.push(Instr::IMul(Val::Reg(Reg::RAX), Val::Imm(2)));
                    ins.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1)));
                }
            }
        },
        Expr::BinOp(op2, subexpr1, subexpr2) => {
            if matches!(op2, Op2::Equal) {
                compile_expr(ins, subexpr1, si, env, brake, l);
                ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, si*8), Val::Reg(Reg::RAX)));
                compile_expr(ins, subexpr2, si+1, env, brake, l);
                // check type error
                ins.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                ins.push(Instr::IXor(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, si*8)));
                ins.push(Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1)));
                ins.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
                ins.push(Instr::IJnz(Val::Label("throw_error".to_string())));
                // compare
                ins.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si*8)));
                ins.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(0)));       
                ins.push(Instr::ISete(Val::Reg(Reg::AL)));
                ins.push(Instr::IMul(Val::Reg(Reg::RAX), Val::Imm(2)));
                ins.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1)));       
            } else {
                compile_expr(ins, subexpr1, si, env, brake, l);
                check_type(ins);
                ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, si*8), Val::Reg(Reg::RAX)));
                compile_expr(ins, subexpr2, si+1, env, brake, l);
                check_type(ins);
                match op2 {
                    Op2::Plus => {
                        ins.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si*8)));
                        check_overflow(ins);
                    },
                    Op2::Minus => {
                        ins.push(Instr::ISub(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si*8)));
                        check_overflow(ins);
                        ins.push(Instr::INeg(Val::Reg(Reg::RAX)));
                    },
                    Op2::Times => {
                        ins.push(Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si*8)));
                        check_overflow(ins);
                        ins.push(Instr::ISar(Val::Reg(Reg::RAX), Val::Imm(1)));
                    },
                    _ => {
                        ins.push(Instr::ICmp(Val::RegOffset(Reg::RSP, si*8), Val::Reg(Reg::RAX)));
                        ins.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(0)));
                        if matches!(op2, Op2::Greater) {
                            ins.push(Instr::ISetg(Val::Reg(Reg::AL)));
                        } else if matches!(op2, Op2::GreaterEqual) {
                            ins.push(Instr::ISetge(Val::Reg(Reg::AL)));
                        } else if matches!(op2, Op2::Less) {
                            ins.push(Instr::ISetl(Val::Reg(Reg::AL)));
                        } else if matches!(op2, Op2::LessEqual) {
                            ins.push(Instr::ISetle(Val::Reg(Reg::AL)));
                        }
                        ins.push(Instr::IMul(Val::Reg(Reg::RAX), Val::Imm(2)));
                        ins.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1)));
                    }
                }
            }
        },
        Expr::Break(e) => {
            if brake == "" {
                panic!("break");
            }
            compile_expr(ins, e, si, env, brake, l);
            ins.push(Instr::IJmp(Val::Label(brake.to_string())));
        },
        Expr::Loop(e) => {
            let start_loop = new_label(l, "loop");
            let end_loop = new_label(l, "endloop");
            ins.push(Instr::Label(String::from(&start_loop)));
            compile_expr(ins, e, si, env, &end_loop, l);
            ins.push(Instr::IJmp(Val::Label(start_loop)));
            ins.push(Instr::Label(end_loop));
        },
        Expr::Block(list_expr) => {
            for expr in list_expr {
                compile_expr(ins, expr, si, env, brake, l);
            }
        },
        Expr::If(iff, thnn, elze) => {
            let end_label = new_label(l, "ifend");
            let else_label = new_label(l, "ifelse");
            compile_expr(ins, iff, si, env, brake, l);
            ins.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(1)));
            ins.push(Instr::IJe(Val::Label(String::from(&else_label))));
            compile_expr(ins, thnn, si, env, brake, l);
            ins.push(Instr::IJmp(Val::Label(String::from(&end_label))));
            ins.push(Instr::Label(else_label));
            compile_expr(ins, elze, si, env, brake, l);
            ins.push(Instr::Label(end_label));
        },
        Expr::Set(name, e) => {
            match env.get(name) {
                Some(i) => {
                    compile_expr(ins, e, si, env, brake, l);
                    ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, *i), Val::Reg(Reg::RAX)))
                },
                None => panic!("Unbound variable identifier {name}"),
            }
        },
    }
}

fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(v1, v2) => {
            format!("\nmov {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::IAdd(v1, v2) => {
            format!("\nadd {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::ISub(v1, v2) => {
            format!("\nsub {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::IMul(v1, v2) => {
            format!("\nimul {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::ITest(v1, v2) => {
            format!("\ntest {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::IXor(v1, v2) => {
            format!("\nxor {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::ICmp(v1, v2) => {
            format!("\ncmp {}, {}", val_to_str(v1), val_to_str(v2))
        },
        // Instr::ICmove(v1, v2) => {
        //     format!("\ncmove {}, {}", val_to_str(v1), val_to_str(v2))
        // },
        Instr::IAnd(v1, v2) => {
            format!("\nand {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::ISar(v1, v2) => {
            format!("\nsar {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::INeg(v) => {
            format!("\nneg {}", val_to_str(v))
        },
        Instr::IJe(v) => {
            format!("\nje {}", val_to_str(v))
        },
        // Instr::IJne(v) => {
        //     format!("\njne {}", val_to_str(v))
        // },
        Instr::IJo(v) => {
            format!("\njo {}", val_to_str(v))
        },
        Instr::IJnz(v) => {
            format!("\njnz {}", val_to_str(v))
        },
        Instr::ISete(v) => {
            format!("\nsete {}", val_to_str(v))
        },
        Instr::ISetg(v) => {
            format!("\nsetg {}", val_to_str(v))
        },
        Instr::ISetge(v) => {
            format!("\nsetge {}", val_to_str(v))
        },
        Instr::ISetl(v) => {
            format!("\nsetl {}", val_to_str(v))
        },
        Instr::ISetle(v) => {
            format!("\nsetle {}", val_to_str(v))
        },
        Instr::IJmp(v) => {
            format!("\njmp {}", val_to_str(v))
        },
        Instr::Label(s) => {
            format!("\n{s}:")
        },
    }
}

fn val_to_str(v: &Val) -> String {
    match v {
        Val::Reg(Reg::RAX) => "rax".to_string(),
        Val::Reg(Reg::RSP) => "rsp".to_string(),
        Val::Reg(Reg::RDI) => "rdi".to_string(),
        Val::Reg(Reg::RBX) => "rbx".to_string(),
        Val::Reg(Reg::RCX) => "rcx".to_string(),
        Val::Reg(Reg::AL) => "al".to_string(),
        Val::Imm(n) => n.to_string(),
        Val::RegOffset(Reg::RSP, offset) => format!("[rsp-{}]", offset),
        Val::Label(s) => s.to_string(),
        _ => panic!("not a valid Val"),
    }
}

fn compile(e: &Expr) -> String {
    let instrs = compile_to_instrs(e);
    let mut asm_string = String::new();
    for instruction in instrs {
        asm_string = asm_string + &instr_to_str(&instruction);
    }
    
    asm_string
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    // You will make result hold the result of actually compiling
    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let expr = parse_expr(&parse(&in_contents).unwrap_or_else(|_| panic!("Invalid")));
    let result = compile(&expr);

    let asm_program = format!(
        "
section .text
extern snek_error
global our_code_starts_here
throw_error:
  mov rdi, rcx
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
