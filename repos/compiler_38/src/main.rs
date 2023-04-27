use std::cell::RefCell;
use std::env;
use std::fs::File;
use std::io::prelude::*;

use std::collections::HashMap;

use std::fmt::Write;

use sexp::Atom::*;
use sexp::*;
use std::fmt::Display;

#[derive(Debug, Clone)]
enum Val {
    Reg(Reg),
    Imm(i64),
    RegOffset(Reg, i32),
}

#[derive(Debug, Clone)]
enum Reg {
    RAX,
    RSP,
    RBX,
    RDI,
}

#[derive(Debug, Clone)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    IXor(Val, Val),
    IAnd(Val, Val),
    ISar(Val, Val),
    ITest(Val, Val),
    LdFlag(&'static str, Reg),
    CMovCC(&'static str, Reg, Reg),
    Jcc(&'static str, String),
    Label(String),
}

impl Display for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Imm(n) => write!(f, "{}", n),
            Self::Reg(r) => write!(f, "{}", r),
            Self::RegOffset(r, o) => write!(f, "QWORD [{}-{}]", r, o),
        }
    }
}

impl Reg {
  fn lo8(&self) -> &'static str {
    match self {
      Self::RAX => "al",
      Self::RBX => "bl",
      _ => panic!("Internal compiler error: lo8"),
    }
  }
}

impl Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::RAX => write!(f, "rax"),
            Self::RSP => write!(f, "rsp"),
            Self::RDI => write!(f, "rdi"),
            Self::RBX => write!(f, "rbx"),
        }
    }
}

impl Display for Instr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instr::IMov(dst, src) => write!(f, "mov {}, {}", dst, src),
            Instr::IAdd(dst, src) => write!(f, "add {}, {}", dst, src),
            Instr::ISub(dst, src) => write!(f, "sub {}, {}", dst, src),
            Instr::IMul(dst, src) => write!(f, "imul {}, {}", dst, src),
            Self::IXor(dst, src) => write!(f, "xor {}, {}", dst, src),
            Self::IAnd(dst, src) => write!(f, "and {}, {}", dst, src),
            Self::ISar(dst, src) => write!(f, "sar {}, {}", dst, src),
            Self::ITest(dst, src) => write!(f, "test {}, {}", dst, src),
            Self::Label(label) => write!(f, "{}:", label),
            Self::LdFlag(cc, dst) => write!(f, "set{0} {2}
              movzx {1}, {2}
              shl {1}, 1
              or {1}, 1", cc, dst, dst.lo8()),
            Self::CMovCC(cc, dst, src) => write!(f, "cmov{cc} {dst}, {src}"),
            Self::Jcc(cc, label) => write!(f, "j{cc} {label}"),
        }
    }
}

fn format_insns(v: &Vec<Instr>) -> String {
    let mut ret = String::new();
    for i in v {
        match write!(ret, "{}\n", i) {
            Err(e) => panic!("{}", e),
            _ => ()
        }
    }
    ret
}

#[derive(Debug)]
enum Op1 {
    Add1,
    Sub1,
    IsNum,
    IsBool,
}

impl TryFrom<&str> for Op1 {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
          "add1" => Ok(Op1::Add1),
          "sub1" => Ok(Op1::Sub1),
          "isnum" => Ok(Op1::IsNum),
          "isbool" => Ok(Op1::IsBool),
          _ => Err(format!("Invalid unary operator {value}")),
        }
    }
}

fn parse_op1(s: &str) -> Op1 {
  match Op1::try_from(s) {
    Ok(op) => op,
    Err(e) => panic!("{}", e),
  }
}

#[derive(Debug)]
enum Op2 {
    Plus,
    Minus,
    Times,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    Equal
}

impl TryFrom<&str> for Op2 {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
          "+" => Ok(Self::Plus),
          "-" => Ok(Self::Minus),
          "*" => Ok(Self::Times),
          "<" => Ok(Self::Less),
          "<=" => Ok(Self::LessEq),
          ">" => Ok(Self::Greater),
          ">=" => Ok(Self::GreaterEq),
          "=" => Ok(Self::Equal),
          _ => Err(format!("Invalid binary operator {value}")),
        }
    }
}

impl Op2 {
  fn is_compare(&self) -> bool {
    match self {
      Self::Less => true,
      Self::LessEq => true,
      Op2::Greater => true,
      Self::GreaterEq => true,
      _ => false,
    }
  }
  fn getcc(&self) -> &'static str {
    match self {
      Self::Less => "l",
      Self::LessEq => "le",
      Self::Greater => "g",
      Self::GreaterEq => "ge",
      _ => panic!("Internal compiler error: Op2::getcc"),
    }
  }
}

fn parse_op2(s: &str) -> Op2 {
  match Op2::try_from(s) {
    Ok(op) => op,
    Err(e) => panic!("{}", e),
  }
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
    SetQ(String, Box<Expr>),
    Block(Vec<Expr>),
}

fn parse_bindings(s: &Sexp) -> Vec<(String, Expr)> {
    let mut ret:Vec<(String, Expr)> = vec![];
    match s {
        Sexp::List(vec) => {
            if vec.is_empty() {
                panic!("Invalid bindings: is empty");
            }
            for ss in vec {
                match ss {
                    Sexp::List(vec) => {
                        match &vec[..] {
                            [Sexp::Atom(S(id)), v] => {
                                ret.push((id.to_string(), parse_expr(v)));
                            },
                            _ => panic!("Invalid binding: {:?}", vec),
                        }
                    }
                    _ => panic!("Invalid binding: {:?}", ss),
                }
            }
            ret
        },
        _ => panic!("Invalid bindings: {:?}", s),
    }
}

fn parse_expr(s: &Sexp) -> Expr {
    let bpe = |e| Box::new(parse_expr(e));
    match s {
        Sexp::Atom(I(n)) => {
          if *n < -4611686018427387904 || *n > 4611686018427387903 {
            panic!("Invalid numeric constant {}", n);
          }
          Expr::Number(*n)
        },
        Sexp::Atom(S(x)) if x == "true" => Expr::Boolean(true),
        Sexp::Atom(S(x)) if x == "false" => Expr::Boolean(false),
        Sexp::Atom(S(id)) => Expr::Id(id.to_string()),
        Sexp::List(vec) => {
            match vec.as_slice() {
                [Sexp::Atom(S(op)), bindings, body] if op == "let" => Expr::Let(parse_bindings(bindings), bpe(body)),
                [Sexp::Atom(S(op)), Sexp::Atom(S(name)), value] if op == "set!" => Expr::SetQ(name.to_string(), bpe(value)),
                [Sexp::Atom(S(op)), cond, then0, else0] if op == "if" => Expr::If(bpe(cond), bpe(then0), bpe(else0)),
                [Sexp::Atom(S(op)), body] if op == "loop" => Expr::Loop(bpe(body)),
                [Sexp::Atom(S(op)), value] if op == "break" => Expr::Break(bpe(value)),
                [Sexp::Atom(S(op)), rest@..] if op == "block" => {
                  if rest.is_empty() {
                    panic!("Invalid expression: empty block");
                  }
                  let mut ret : Vec<Expr> = vec![];
                  for sex in rest {
                    ret.push(parse_expr(sex));
                  }
                  Expr::Block(ret)
                },
                [Sexp::Atom(S(op)), e] => Expr::UnOp(parse_op1(op), bpe(e)),
                [Sexp::Atom(S(op)), e1, e2] => Expr::BinOp(parse_op2(op), bpe(e1), bpe(e2)),
                _ => panic!("Invalid expression: {:?}", vec),
            }
        },
        _ => panic!("Invalid expression: {:?}", s),
    }
}

const WORD: i32 = 8;

thread_local!(static GLOBAL: RefCell<u64> = RefCell::new(0));
fn new_label(comment: &str) -> String {
  GLOBAL.with(|rc| {
    *rc.borrow_mut() += 1;
    format!("{comment}_{}", rc.borrow())
  })
}


const KEYWORDS: [&str;21] = ["true", "false", "input", "let", "set!", "if", "block", "loop", "break", "add1", "sub1", "isnum", "isbool", "=", "+", "-", "*", "<", ">", "<=", ">="];

fn compile_bindings(b: &Vec<(String, Expr)>, si: i32, env: &HashMap<String, i32>, bloc: &Option<String>, output: &mut Vec<Instr>) -> (i32, HashMap<String, i32>) {
    let mut msi = si;
    let mut ret: HashMap<String, i32> = env.clone();
    let mut test: HashMap<String, ()> = HashMap::default();
    for (id, e) in b {
      if KEYWORDS.iter().any(|k| k == id) {
        panic!("Cannot use keyword as variable name");
      }
        if test.contains_key(id) {
            panic!("Duplicate binding");
        }
        compile_expr(e, msi+1, &ret, bloc, output);
        output.push(Instr::IMov(Val::RegOffset(Reg::RSP, msi * WORD), Val::Reg(Reg::RAX)));
        ret.insert(id.to_string(), msi*WORD);
        test.insert(id.to_string(), ());
        msi += 1;
    }
    (msi, ret)
}

fn c_check_overflow(output: &mut Vec<Instr>) {
  output.push(Instr::Jcc("o", "err_overflow".to_string()));
}

fn c_check_number(target: Val, output: &mut Vec<Instr>) {
  output.push(Instr::ITest(target, Val::Imm(1)));
  output.push(Instr::Jcc("ne", "err_invalid_argument".to_string()));
}

fn compile_unop(op: &Op1, subexpr: &Expr, si: i32, env: &HashMap<String, i32>, bloc: &Option<String>, output: &mut Vec<Instr>) {
  compile_expr(subexpr, si, env, bloc, output);
  match op {
    Op1::Add1 => {
      c_check_number(Val::Reg(Reg::RAX), output);
      output.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(2)));
      c_check_overflow(output);
    },
    Op1::Sub1 => {
      c_check_number(Val::Reg(Reg::RAX), output);
      output.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(2)));
      c_check_overflow(output);
    },
    Op1::IsNum => {
      output.push(Instr::IAnd(Val::Reg(Reg::RAX), Val::Imm(1)));
      output.push(Instr::LdFlag("z", Reg::RAX));
    },
    Op1::IsBool => {
      output.push(Instr::IAnd(Val::Reg(Reg::RAX), Val::Imm(1)));
      output.push(Instr::LdFlag("nz", Reg::RAX));
    },
  }
}

fn compile_binop(op: &Op2, el: &Expr, er: &Expr, si: i32, env: &HashMap<String, i32>, bloc: &Option<String>, output: &mut Vec<Instr>) {
  let rsp = si*WORD;
  let rval = Val::RegOffset(Reg::RSP, rsp);
  compile_expr(er, si, env, bloc, output);
  output.push(Instr::IMov(rval.clone(), Val::Reg(Reg::RAX)));
  compile_expr(el, si+1, env, bloc, output);
  match op {
    Op2::Equal => {
      output.push(Instr::IXor(Val::Reg(Reg::RAX), rval));
      output.push(Instr::LdFlag("e", Reg::RBX));
      output.push(Instr::IAnd(Val::Reg(Reg::RAX), Val::Imm(1)));
      output.push(Instr::Jcc("ne", "err_invalid_argument".to_string()));
      output.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
    },
    _ => {
      c_check_number(Val::Reg(Reg::RAX), output);
      c_check_number(rval.clone(), output);
      match op {
        Op2::Times => {
          output.push(Instr::ISar(Val::Reg(Reg::RAX), Val::Imm(1)));
          output.push(Instr::IMul(Val::Reg(Reg::RAX), rval));
          c_check_overflow(output);
        },
        Op2::Plus => {
          output.push(Instr::IAdd(Val::Reg(Reg::RAX), rval));
          c_check_overflow(output);
        },
        Op2::Minus => {
          output.push(Instr::ISub(Val::Reg(Reg::RAX), rval));
          c_check_overflow(output);
        },
        _ if op.is_compare() => {
          output.push(Instr::ISub(Val::Reg(Reg::RAX), rval));
          output.push(Instr::LdFlag(op.getcc(), Reg::RAX));
        },
        _ => panic!("Internal Compiler error: compile_binop"),
      }
    },
  }
}

fn compile_expr(e: &Expr, si: i32, env: &HashMap<String, i32>, bloc: &Option<String>, output: &mut Vec<Instr>) -> () {
    match e {
        Expr::Number(n) => {
          let realn = i64::from(*n);
          output.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(realn << 1)))
        },
        Expr::Id(id) => match env.get(id) {
            Some(offset) => output.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, *offset))),
            None => {
              if id == "input" {
                output.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI)));
              } else {
                panic!("Unbound variable identifier {id}")
              }
            },
        },
        Expr::UnOp(op, se) => {
            compile_unop(op, se, si, env, bloc, output);
        },
        Expr::BinOp(op, e1, e2) => {
            compile_binop(op, e1, e2, si, env, bloc, output);
        },
        Expr::Let(bindings, body) => {
            let (nsi, nev) = compile_bindings(bindings, si, env, bloc, output);
            compile_expr(body, nsi, &nev, bloc, output);
        },
        Expr::Boolean(true) => output.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3))),
        Expr::Boolean(false) => output.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))),
        Expr::If(cond, trueb, falseb) => {
          let lelse = new_label("ifelse");
          let lend = new_label("ifend");
          compile_expr(cond, si, env, bloc, output);
          output.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(1)));
          output.push(Instr::Jcc("e", lelse.clone()));
          compile_expr(trueb, si, env, bloc, output);
          output.push(Instr::Jcc("mp", lend.clone()));
          output.push(Instr::Label(lelse));
          compile_expr(falseb, si, env, bloc, output);
          output.push(Instr::Label(lend));
        },
        Expr::Loop(se) => {
          let lbegin = new_label("loop");
          let lend = new_label("endloop");
          let newbloc = Some(lend.clone());
          output.push(Instr::Label(lbegin.clone()));
          compile_expr(se, si, env, &newbloc, output);
          output.push(Instr::Jcc("mp", lbegin));
          output.push(Instr::Label(lend));
        },
        Expr::Break(se) => {
          match bloc {
            Some(label) => {
              compile_expr(se, si, env, bloc, output);
              output.push(Instr::Jcc("mp", label.to_string()));
            },
            None => panic!("break outside loop"),
          }
        },
        Expr::SetQ(id, value) => match env.get(id) {
          Some(offset) => {
            compile_expr(value, si, env, bloc, output);
            output.push(Instr::IMov(Val::RegOffset(Reg::RSP, *offset), Val::Reg(Reg::RAX)));
          },
          None => panic!("Unbound variable identifier {id}")
      },
        Expr::Block(vec) => {
            for se in vec {
              compile_expr(se, si, env, bloc, output);
            }
        },
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents);

    let sex = match parse(&in_contents) {
      Ok(s) => s,
      Err(e) => panic!("Invalid expression: {}", e),
    };

    let expr = parse_expr(&sex);
    let empty_env: HashMap<String, i32> = HashMap::default();
    let mut insn: Vec<Instr> = vec![];
    let bloc: Option<String> = None;
    compile_expr(&expr, 2, &empty_env, &bloc, &mut insn);
    // You will make result hold the result of actually compiling
    let result = format_insns(&insn);

    let asm_program = format!(
        "
section .text
extern snek_error
global our_code_starts_here
err_invalid_argument:
      mov rdi, 1
      jmp errorp
err_overflow:
      mov rdi, 2
      jmp errorp
errorp:
      push rsp
      call snek_error
our_code_starts_here:
  {}
  ret
",
        result
    );

    let mut out_file = File::create(out_name)?;
    std::io::Write::write_all(&mut out_file, asm_program.as_bytes())?;
    
    Ok(())
}
