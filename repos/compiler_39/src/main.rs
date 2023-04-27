use std::{env, collections::HashSet};
use std::fs::File;
use std::io::prelude::*;

use sexp::Atom::*;
use sexp::*;


use im::{HashMap};

#[derive(Debug)]
enum Val {
  Reg(Reg),
  Imm(i64),
  RegOffset(Reg, i32),
}

#[derive(Debug)]
enum VarTypes {
  NUM,
  BOOL
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
    ITest(Val, Val),
    IXor(Val, Val),
    ISar(Val, Val),
    ICmp(Val, Val),
    IJe(String),
    IJne(String),
    IJo(String),
    IJmp(String),
    IJnz(String),
    ILabel(String),
    ICmove(Val, Val),
    ICmovz(Val, Val),
    ICmovl(Val, Val),
    ICmovle(Val, Val),
    ICmovg(Val, Val),
    ICmovge(Val, Val)
}

struct Context<'a> {
  si: i32,
  env: &'a HashMap<String, i32>,
  brake: &'a str,
}


#[derive(Debug)]
enum Op1 { Add1, Sub1, IsNum, IsBool, }

#[derive(Debug)]
enum Op2 { Plus, Minus, Times, Equal, Greater, GreaterEqual, Less, LessEqual, }

enum NumConditionalOp2 {Greater, GreaterEqual, Less, LessEqual}

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

const BOOLEAN_CONSTANTS: &'static [&str] = &["true", "false"];
const ALL_RESERVED_WORDS: &'static [&str] = &["true", "false","add1", "sub1", "isnum", "isbool","let", "block", "set", "if", "break", "set!", "+", "-", "*", "=", ">", ">=", "<", "<=", "input"];

fn parse_bind(sexp: &Sexp) -> (String, Expr) {
  match sexp {
      Sexp::List(vec) => match &vec[..] {
          [Sexp::Atom(S(s)), e] => {
              if ALL_RESERVED_WORDS.contains(&&s[..]) {
                  panic!("Invalid identifier name: {s} since it is a keyword.");
              } else {
                 (s.to_string(), parse_expr(e))
              }
          },
          _ => panic!("Invalid binding format"),
      }
      _ => panic!("Invalid let binding"),
  }
}

fn parse_expr(sexp: &Sexp) -> Expr {
  match sexp {
    Sexp::Atom(I(n)) => {
        match i64::try_from(*n) {
          Ok(i) => Expr::Number(i),
          Err(_) => panic!("Invalid i32: {n}")
      }
    },
    Sexp::Atom(S(s)) => {
      let parsed_str = &&s[..];
      if BOOLEAN_CONSTANTS.contains(parsed_str) {
        if s == "true" {
          Expr::Boolean(true)
        } else {
          Expr::Boolean(false)
        }
      } else if ALL_RESERVED_WORDS.contains(&&s[..]) && s!="input" {
        panic!("Invalid identifier name: {s}")
      } else {
        Expr::Id(s.to_string())
      }
    },
    Sexp::Atom(F(n)) => panic!("Invalid value due to unsupported type (Float): {n}"),
    Sexp::List(vec) => match &vec[..] {
      /* Unary Operation */
      [Sexp::Atom(S(op)), e] if op == "add1" => {
          Expr::UnOp(Op1::Add1, Box::new(parse_expr(e)))
      },
      [Sexp::Atom(S(op)), e] if op == "sub1" => {
          Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e)))
      },
      [Sexp::Atom(S(op)), e] if op == "isnum" => {
        Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e)))
      },
      [Sexp::Atom(S(op)), e] if op == "isbool" => {
        Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e)))
      },
      /* Binary Operation */
      [Sexp::Atom(S(op)), e1, e2] if op == "+" => {
        Expr::BinOp(
          Op2::Plus,
          Box::new(parse_expr(e1)),
          Box::new(parse_expr(e2)),
        )
      }
      [Sexp::Atom(S(op)), e1, e2] if op == "-" => {
        Expr::BinOp(
          Op2::Minus,
          Box::new(parse_expr(e1)),
          Box::new(parse_expr(e2)),
        )
      }
      [Sexp::Atom(S(op)), e1, e2] if op == "*" => {
        Expr::BinOp(
          Op2::Times,
          Box::new(parse_expr(e1)),
          Box::new(parse_expr(e2)),
        )
      }
      [Sexp::Atom(S(op)), e1, e2] if op == "<" => {
        Expr::BinOp(
          Op2::Less,
          Box::new(parse_expr(e1)),
          Box::new(parse_expr(e2)),
        )
      }
      [Sexp::Atom(S(op)), e1, e2] if op == ">" => {
        Expr::BinOp(
          Op2::Greater,
          Box::new(parse_expr(e1)),
          Box::new(parse_expr(e2)),
        )
      }
      [Sexp::Atom(S(op)), e1, e2] if op == ">=" => {
        Expr::BinOp(
          Op2::GreaterEqual,
          Box::new(parse_expr(e1)),
          Box::new(parse_expr(e2)),
       )
      }
      [Sexp::Atom(S(op)), e1, e2] if op == "<=" => {
        Expr::BinOp(
          Op2::LessEqual,
          Box::new(parse_expr(e1)),
          Box::new(parse_expr(e2)),
        )
      }
      [Sexp::Atom(S(op)), e1, e2] if op == "=" => {
        Expr::BinOp(
          Op2::Equal,
          Box::new(parse_expr(e1)),
          Box::new(parse_expr(e2)),
        )
      }
      /* Let Operation */
      [Sexp::Atom(S(op)),Sexp::List(bindings), body] if op == "let" => {
        if bindings.is_empty() {
            panic!("Invalid let without binding")
        }
        let mut vec = Vec::new();
        for binding in bindings {
            vec.push(parse_bind(binding));
        }

        Expr::Let(vec, Box::new(parse_expr(body)))
      }
      /* If Operation */
      [Sexp::Atom(S(op)), condition_expr, then_expr, else_expr] if op == "if" => {
        Expr::If(Box::new(parse_expr(condition_expr)), Box::new(parse_expr(then_expr)), Box::new(parse_expr(else_expr)))
      }
      /* block Operation */
      [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => {
        /*
         * Generated inline lambda with ChatGPT using prompt:
         * "Generate lambda in Rust to convert a vector of type A to a vector of type B given a method func that converts objects of type A to type B"
         */
        if exprs.is_empty() {
          panic!("Invalid empty block")
      }
        Expr::Block(exprs.iter().map(|expression| parse_expr(expression)).collect())
      }
      /* Break Operation */
      [Sexp::Atom(S(op)), e] if op == "break" => {
        Expr::Break(Box::new(parse_expr(e)))
      }
      /* Set Operation */
      [Sexp::Atom(S(op)), Sexp::Atom(S(s)), e] if op == "set!" => {
        if ALL_RESERVED_WORDS.contains(&&s[..]) {
          panic!("Invalid identifier in set operation: {s}, which is a keyword")
        } else {
          Expr::Set(s.to_string(), Box::new(parse_expr(e)))
        }
      }
      /* Loop Operation */
      [Sexp::Atom(S(op)), e] if op == "loop" => {
        Expr::Loop(Box::new(parse_expr(e)))
      }
      _ => panic!("Invalid Sexp format: {sexp}")
    }
  }
}

fn reg_to_str(r: &Reg) -> String {
  match r {
    Reg::RAX => "rax".to_string(),
    Reg::RSP => "rsp".to_string(),
    Reg::RDI => "rdi".to_string(),
    Reg::RBX => "rbx".to_string(),
}
}

// NOTE: we need to multiply si by 8 when creating the final x86 instructions
fn val_to_str(v: &Val) -> String {
  match v {
      Val::Imm(n) => (*n).to_string(),
      Val::Reg(r) => reg_to_str(r),
      Val::RegOffset(r, n) => format!("[{}-{}]", reg_to_str(r), n*8),
  }
}

fn instr_to_str(i: &Instr) -> String {
  match i {
      Instr::IMov(v1, v2) => format!("\nmov {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::IAdd(v1, v2) => format!("\nadd {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::IMul(v1, v2) => format!("\nimul {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::ISub(v1, v2) => format!("\nsub {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::ITest(v1, v2) => format!("\ntest {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::IXor(v1, v2) => format!("\nxor {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::ICmp(v1, v2) => format!("\ncmp {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::IJe(s) => format!("\nje {}", s.to_string()),
      Instr::IJne(s) => format!("\njne {}", s.to_string()),
      Instr::IJmp(s) => format!("\njmp {}", s.to_string()),
      Instr::ILabel(s) => format!("\n {}:", s.to_string()),
      Instr::IJo(s) => format!("\njo {}", s.to_string()),
      Instr::IJnz(s) => format!("\njnz {}", s.to_string()),
      Instr::ISar(v1, v2) => format!("\nsar {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::ICmove(v1, v2) => format!("\ncmove {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::ICmovz(v1, v2) => format!("\ncmovz {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::ICmovl(v1, v2) => format!("\ncmovl {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::ICmovle(v1, v2) => format!("\ncmovle {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::ICmovg(v1, v2) => format!("\ncmovg {}, {}", val_to_str(v1), val_to_str(v2)),
      Instr::ICmovge(v1, v2) => format!("\ncmovge {}, {}", val_to_str(v1), val_to_str(v2)),
  }
}

fn duplicate_bindings(bindings: &Vec<(String, Expr)>) -> bool{
  let mut unique_var_identifiers = HashSet::new(); 
  return bindings.iter().any(|binding| unique_var_identifiers.insert(binding.0.to_string()) == false);
}

fn convert_i64_to_val(n: &i64) -> Val {
  let deref = *n;
  if deref > 4611686018427387903 || deref < -4611686018427387904 {
    panic!("Invalid overflow: {n}")
  } else {
    Val::Imm(*n << 1)
  }
}

fn convert_bool_to_val(b: &bool) -> Val {
  match b {
    true => Val::Imm(3),
    false => Val::Imm(1),
  }
}

fn new_label(l: &mut i32, s: &str) -> String {
  let current = *l;
  *l += 1;
  format!("{s}_{current}")
}

fn check_if_int(instruction_vec: &mut Vec<Instr>) {
  instruction_vec.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
  instruction_vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(2)));
  instruction_vec.push(Instr::IJnz("throw_error".to_string()));
}

fn check_for_overflow(instruction_vec: &mut Vec<Instr>) {
  instruction_vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(1)));
  instruction_vec.push(Instr::IJo("throw_error".to_string()));
}

fn check_type(instruction_vec: &mut Vec<Instr>, type_to_check: VarTypes) {
  instruction_vec.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
  match type_to_check {
    VarTypes::NUM => {
      instruction_vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
      instruction_vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
    }
    VarTypes::BOOL => {
      instruction_vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(1)));
      instruction_vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
    }
  }
  instruction_vec.push(Instr::ICmovz(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)))
}

fn compile_num_conditional_op2(e1: &Box<Expr>, e2: &Box<Expr>, instruction_vec: &mut Vec<Instr>, context: &Context, l: &mut i32, op2: NumConditionalOp2) {
    compile_to_instrs(e2, instruction_vec, context, l);
    check_if_int(instruction_vec);
    instruction_vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, context.si), Val::Reg(Reg::RAX)));
    compile_to_instrs(e1, instruction_vec, &Context { si: context.si+1, env: context.env, brake: context.brake }, l);
    check_if_int(instruction_vec);
    instruction_vec.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, context.si)));
    instruction_vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
    instruction_vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
    match op2 {
      NumConditionalOp2::Greater => instruction_vec.push(Instr::ICmovg(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))),
      NumConditionalOp2::GreaterEqual => instruction_vec.push(Instr::ICmovge(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))),
      NumConditionalOp2::Less => instruction_vec.push(Instr::ICmovl(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))),
      NumConditionalOp2::LessEqual => instruction_vec.push(Instr::ICmovle(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))),
  }
}

fn compile_to_instrs(expr: &Expr, instruction_vec: &mut Vec<Instr>, context: &Context, l: &mut i32) {
  match expr {
    Expr::Number(n) => {
      instruction_vec.push(Instr::IMov(Val::Reg(Reg::RAX), convert_i64_to_val(n)));
    }
    Expr::Boolean(b) => {
      instruction_vec.push(Instr::IMov(Val::Reg(Reg::RAX), convert_bool_to_val(b)))
    }
    Expr::UnOp(Op1::Add1, e) => {
        compile_to_instrs(e, instruction_vec, context, l);
        check_if_int(instruction_vec);
        instruction_vec.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(2)));
        check_for_overflow(instruction_vec);
    }
    Expr::UnOp(Op1::Sub1, e) => {
        compile_to_instrs(e, instruction_vec, context, l);
        check_if_int(instruction_vec);
        instruction_vec.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(2)));
        check_for_overflow(instruction_vec);
    }
    Expr::UnOp(Op1::IsNum, e) => {
      compile_to_instrs(e, instruction_vec, context, l);
      check_type(instruction_vec, VarTypes::NUM);
    }
    Expr::UnOp(Op1::IsBool, e) => {
      compile_to_instrs(e, instruction_vec, context, l);
      check_type(instruction_vec, VarTypes::BOOL);
    }
    Expr::BinOp(Op2::Plus, e1, e2) => {
        compile_to_instrs(e1, instruction_vec, context, l);
        check_if_int(instruction_vec);
        instruction_vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, context.si), Val::Reg(Reg::RAX)));
        compile_to_instrs(e2, instruction_vec, &Context { si: context.si+1, env: context.env, brake: context.brake }, l);
        check_if_int(instruction_vec);
        instruction_vec.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, context.si)));
        check_for_overflow(instruction_vec);
    }
    Expr::BinOp(Op2::Minus, e1, e2) => {
        compile_to_instrs(e2, instruction_vec, context, l);
        check_if_int(instruction_vec);
        instruction_vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, context.si), Val::Reg(Reg::RAX)));
        compile_to_instrs(e1, instruction_vec, &Context { si: context.si+1, env: context.env, brake: context.brake }, l);
        check_if_int(instruction_vec);
        instruction_vec.push(Instr::ISub(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, context.si)));
        check_for_overflow(instruction_vec);
    }
    Expr::BinOp(Op2::Times, e1, e2) => {
        compile_to_instrs(e1, instruction_vec, context, l);
        check_if_int(instruction_vec);
        instruction_vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, context.si), Val::Reg(Reg::RAX)));
        compile_to_instrs(e2, instruction_vec, &Context { si: context.si+1, env: context.env, brake: context.brake }, l);
        check_if_int(instruction_vec);
        instruction_vec.push(Instr::ISar(Val::Reg(Reg::RAX), Val::Imm(1)));
        instruction_vec.push(Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, context.si)));
        check_for_overflow(instruction_vec);
    }
    Expr::BinOp(Op2::Equal, e1, e2) => {
      compile_to_instrs(&e1, instruction_vec, context, l);
      instruction_vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, context.si), Val::Reg(Reg::RAX)));
      compile_to_instrs(&e2, instruction_vec, &Context { si: context.si+1, env: context.env, brake: context.brake }, l);
      instruction_vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
      instruction_vec.push(Instr::IXor(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, context.si)));
      instruction_vec.push(Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1)));
      instruction_vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(2)));
      instruction_vec.push(Instr::IJne("throw_error".to_string()));
      instruction_vec.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, context.si)));
      instruction_vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
      instruction_vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
      instruction_vec.push(Instr::ICmove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
    }
    Expr::BinOp(Op2::Greater, e1, e2) => {
        compile_num_conditional_op2(e1, e2, instruction_vec, context, l, NumConditionalOp2::Greater);
    }
    Expr::BinOp(Op2::GreaterEqual, e1, e2) => {
        compile_num_conditional_op2(e1, e2, instruction_vec, context, l, NumConditionalOp2::GreaterEqual);
    }
    Expr::BinOp(Op2::Less, e1, e2) => {
      compile_num_conditional_op2(e1, e2, instruction_vec, context, l, NumConditionalOp2::Less);
    }
    Expr::BinOp(Op2::LessEqual, e1, e2) => {
      compile_num_conditional_op2(e1, e2, instruction_vec, context, l, NumConditionalOp2::LessEqual);
    }
    Expr::Id(s) if s == "input" => {
      instruction_vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI)))
    }
    Expr::Id(s) => {
        if !context.env.contains_key(s) {
            panic!("Unbound variable identifier {s}")
        } else {
            instruction_vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, *(context.env.get(s).unwrap()))))
        }
    }
    Expr::Let(bindings, body) => {
        if duplicate_bindings(bindings) {
            panic!("Duplicate binding")
        }
        let mut new_env = context.env.clone();
        let mut new_si = context.si;
        for binding in bindings {
            let identifier = binding.0.to_string();
            let binding_expr = &binding.1;
            compile_to_instrs(&binding_expr, instruction_vec, &Context { si:new_si, env: &new_env, brake: context.brake }, l);
            instruction_vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, new_si), Val::Reg(Reg::RAX)));
            new_env = new_env.update(identifier, new_si);
            new_si = new_si+1;
        }
        compile_to_instrs(body, instruction_vec, &Context { si:new_si, env: &new_env, brake: context.brake }, l);
    }
    Expr::If(cond, thn, els) => {
      let end_label = new_label(l, "ifend");
      let else_label = new_label(l, "ifelse");
      compile_to_instrs(cond, instruction_vec, context, l);
      instruction_vec.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(1)));
      instruction_vec.push(Instr::IJe(else_label.to_string()));
      compile_to_instrs(thn, instruction_vec, context, l);
      instruction_vec.push(Instr::IJmp(end_label.to_string()));
      instruction_vec.push(Instr::ILabel(else_label.to_string()));
      compile_to_instrs(els, instruction_vec, context, l);
      instruction_vec.push(Instr::ILabel(end_label.to_string()));
    }
    Expr::Loop(e) => {
      let startloop = new_label(l, "loop");
      let endloop = new_label(l, "loopend");
      instruction_vec.push(Instr::ILabel(startloop.to_string()));
      compile_to_instrs(e, instruction_vec, &Context { si: context.si, env: context.env, brake: &endloop }, l);
      instruction_vec.push(Instr::IJmp(startloop.to_string()));
      instruction_vec.push(Instr::ILabel(endloop.to_string()));
    }
    Expr::Break(e) => {
      if context.brake == "" {
        panic!("Error: break occurred outside a loop in sexp")
      }
      compile_to_instrs(e, instruction_vec, context, l);
      instruction_vec.push(Instr::IJmp(context.brake.to_string()));
    },
    Expr::Set(name, e) => {
      if !context.env.contains_key(name) {
        panic!("Unbound variable identifier {name}")
    } else {
      compile_to_instrs(e, instruction_vec, context, l);
      instruction_vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, *(context.env.get(name).unwrap())), Val::Reg(Reg::RAX)));
    }
    }
    Expr::Block(es) => {
      for e in es {
        compile_to_instrs(e, instruction_vec, context, l)
      }
    }
  }
}

fn compile(e: &Expr) -> String {
  let mut instruction_vec: Vec<Instr> = Vec::new();
  let si = 2;
  let env = HashMap::new(); 
  let mut labels = 0;
  compile_to_instrs(e, &mut instruction_vec, &Context { si, env: &env, brake: "" }, &mut labels);

  /* print!("{}", instruction_vec.iter().map(instr_to_str).collect::<String>()); */

  instruction_vec.iter().map(instr_to_str).collect::<String>()
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    // You will make result hold the result of actually compiling
    let parsed_sexpr = &parse(&in_contents);
    let expr = match parsed_sexpr {
        Ok(sexpr) => parse_expr(sexpr),
        Err(_) => panic!("Invalid unable to parse input file into valid SExpr AST."),
    };
    let result = compile(&expr);

    let asm_program = format!(
        "
section .text
global our_code_starts_here
extern snek_error
throw_error:
mov rdi, rbx
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
