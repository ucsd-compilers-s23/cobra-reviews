use sexp::Atom::*;
use sexp::*;
use std::env;
use std::fs::File;
use std::io::prelude::*;

use im::HashMap;
use im::Vector;

enum Op1 { Add1, Sub1, IsNum, IsBool, }

enum Op2 { Plus, Minus, Times, Equal, Greater, GreaterEqual, Less, LessEqual, }

enum Expr {
    Number(i32),
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
        // values
        Sexp::Atom(I(num)) => {
            // let i_128 = i128::try_from(*num).unwrap();
            // i_128.checked_shl(1).unwrap();
            let i = i32::try_from(*num);
            match i {
                Err(_err) => panic!("Error: Invalid"),
                _ => Expr::Number(i.unwrap()),
            }
        },
        Sexp::Atom(S(name)) if name == "true"  => Expr::Boolean(true),
        Sexp::Atom(S(name)) if name == "false" => Expr::Boolean(false),
        Sexp::Atom(S(name)) => Expr::Id(name.to_string()),
        // expressions
        Sexp::List(vec) => match &vec[..] {
            // unary operations
            [Sexp::Atom(S(op)), e] if op == "add1"   => Expr::UnOp(Op1::Add1,   Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "sub1"   => Expr::UnOp(Op1::Sub1,   Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "isnum"  => Expr::UnOp(Op1::IsNum,  Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "isbool" => Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e))),
            // binary operations
            [Sexp::Atom(S(op)), e1, e2] if op == "+" => {
                Expr::BinOp(Op2::Plus,         Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "-" => {
                Expr::BinOp(Op2::Minus,        Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "*" => {
                Expr::BinOp(Op2::Times,        Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "<" => {
                Expr::BinOp(Op2::Less,         Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == ">" => {
                Expr::BinOp(Op2::Greater,      Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "<=" => {
                Expr::BinOp(Op2::LessEqual,    Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == ">=" => {
                Expr::BinOp(Op2::GreaterEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "=" => {
                Expr::BinOp(Op2::Equal,        Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            // loops
            [Sexp::Atom(S(op)), e] if op == "loop" => {
                Expr::Loop(Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), e] if op == "break" => {
                Expr::Break(Box::new(parse_expr(e)))
            }
            // setting assigned variables
            [Sexp::Atom(S(op)), Sexp::Atom(S(name)), e] if op == "set!" => {
                Expr::Set(name.to_string(), Box::new(parse_expr(e)))
            }
            // conditionals
            [Sexp::Atom(S(keyword)), cond, thn, els] if keyword == "if" => Expr::If(
                Box::new(parse_expr(cond)),
                Box::new(parse_expr(thn)),
                Box::new(parse_expr(els)),
            ),
            // blocks
            [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => {
                Expr::Block(exprs.into_iter().map(parse_expr).collect())
            }
            // bindings
            [Sexp::Atom(S(keyword)), Sexp::List(vec), body] if keyword == "let" => Expr::Let(
                vec.into_iter().map(|e| {
                    match e {
                        Sexp::List(vec) => match &vec[..] {
                            [Sexp::Atom(S(id)), body] => (id.to_string(), parse_expr(&body)),
                            _ => panic!("Error: Invalid"),
                        }
                        _ => panic!("Error: Invalid"),
                    }
                }).collect::<Vec<(String, Expr)>>(),
                Box::new(parse_expr(body))
            ),
            // default statement
            _ => panic!("Invalid parse error: {}", s),
        },
        _ => panic!("Invalid parse error: {}", s),
    }
}

// Beginning of compile segment

fn new_label(l: &mut i32, s: &str) -> String {
    let current = *l;
    *l += 1;
    format!("{s}_{current}")
}

#[derive(Clone,Copy)]
enum REG {
  RAX,
  RSP,
  RDI,
}

#[derive(Clone,Copy)]
enum Loc {
    LReg(REG),
    LStack(i32)
}


#[derive(Clone,Copy)]
enum Val {
    VReg(REG),
    VStack(i32),
    VImm(i32)
}

use REG::*;
use Loc::*;
use Val::*;

struct Context<'a> {
    si: i32,
    env: &'a HashMap<String, Loc>,
    brake: &'a Vector<String>,
    target: Loc
}

fn reg_to_str(r : &REG) -> String {
    match r {
        RAX => String::from("rax"),
        RSP => String::from("rsp"),
        RDI => String::from("rdi")
    }
}

fn val_to_str(v : &Val) -> String {
    match v {
        VStack(n) => {
            let offset = n * 8;
            format!("qword [rsp - {offset}]")
        }
        VReg(r) => reg_to_str(r),
        VImm(n) => format!("{}", n)
    }
}

fn mov_target(dest : &Loc, source : &Val) -> String {
    match (dest, source) {
        (LStack(n), VStack(_m)) => {
            format!("
                mov rax, {}
                mov {}, rax
            ",
            val_to_str(source), val_to_str(&VStack(*n)))
        },
        (LReg(r1), _) => format!("mov {}, {}", reg_to_str(r1), val_to_str(source)),
        (LStack(n), _) => format!("mov {}, {}", val_to_str(&VStack(*n)), val_to_str(source))
    }
}

fn compile_expr(e: &Expr, c : &Context, l: &mut i32) -> String {
    match e {
        Expr::Number(n) => mov_target(&c.target, &VImm(*n << 1)),
        Expr::Boolean(true) => mov_target(&c.target, &VImm(3)),
        Expr::Boolean(false) => mov_target(&c.target, &VImm(1)),
        Expr::Id(s) if s == "input" => mov_target(&c.target, &VReg(RDI)),
        Expr::Id(s) => {
            if !(c.env.contains_key(s)) {
                panic!("Error: Unbound variable identifier {s}");
            }
            match c.env.get(s).unwrap() {
                LReg(reg) => format!("mov rax, {}", reg_to_str(reg)),
                LStack(offset) => {
                    mov_target(&c.target, &VStack(*offset))
                }
            }
        }
        Expr::Set(name, val) => {
            if !(c.env.contains_key(name)) {
                panic!("Unbound variable identifier {name}");
            }
            let target = c.env.get(name).unwrap();
            let nctxt = Context { target: LReg(RAX), ..*c };
            let val_is = compile_expr(val, &nctxt, l);
            let save = mov_target(&target, &VReg(RAX));
            format!("
                {val_is}
                {save}
            ")
        }
        Expr::UnOp(Op1::Add1, subexpr) => {
            // evaluate into new_ctxt, then operate, then move to target
            let new_ctxt = Context { target: LReg(RAX), ..*c };
            let subexpr_instrs = compile_expr(subexpr, &new_ctxt, l);
            let mov_out_instrs = mov_target(&c.target, &VReg(RAX));
            format!("
                {subexpr_instrs}
                add rax, 2
                {mov_out_instrs}
            ")
        }
        Expr::UnOp(Op1::Sub1, subexpr) => {
            let new_ctxt = Context { target: LReg(RAX), ..*c };
            let subexpr_instrs = compile_expr(subexpr, &new_ctxt, l);
            let mov_out_instrs = mov_target(&c.target, &VReg(RAX));
            format!("
                {subexpr_instrs}
                add rax, -2
                {mov_out_instrs}
            ")
        }
        Expr::UnOp(Op1::IsNum, e) => {
            let new_ctxt = Context { target: LReg(RAX), ..*c };
            let e_instrs = compile_expr(e, &new_ctxt, l);
            let mov_out_instrs = mov_target(&c.target, &VReg(RAX));
            format!("
                {e_instrs}
                test rax, 1
                mov rbx, 3
                mov rax, 1
                cmove rax, rbx
                {mov_out_instrs}
            ")
        }
        Expr::UnOp(Op1::IsBool, e) => {
            let new_ctxt = Context { target: LReg(RAX), ..*c };
            let e_instrs = compile_expr(e, &new_ctxt, l);
            let mov_out_instrs = mov_target(&c.target, &VReg(RAX));
            format!("
                {e_instrs}
                test rax, 1
                mov rbx, 3
                mov rax, 1
                cmovne rax, rbx
                {mov_out_instrs}
            ")
        }
        Expr::Break(e) => {
            // let nctxt = Context { target: LReg(RAX), ..*c };
            // let e_is = compile_expr(e, &nctxt, l);
            // compute e to the target of the break, then jump out of the loop
            let mut new_brake = c.brake.clone();
            let brake_target = new_brake.pop_back().unwrap();
            if brake_target.eq(&String::from("")) {
                panic!("Error: unbound break");
            }
            let new_ctxt = Context {brake: &new_brake, ..*c};
            let e_is = compile_expr(e, &new_ctxt, l);
            // let mov_instrs = mov_target(&c.target, VReg(RAX));
            format!("
                {e_is}
                jmp {}
            ", brake_target)
        }
        Expr::Loop(e) => {
            let startloop = new_label(l, "loop");
            let endloop = new_label(l, "endloop");
            let mut new_brake = c.brake.clone();
            new_brake.push_back(endloop.clone());
            let new_ctxt = Context { brake: &new_brake, ..*c };
            // let e_is = compile_expr(e, &Context { brake: &endloop, ..*c }, l);
            let e_is = compile_expr(e, &new_ctxt, l);
            format!("
                {startloop}:
                    {e_is}
                    jmp {startloop}
                {endloop}:
            ")
        }
        Expr::Block(es) => {
            // could consider writing all but last into RAX or something
            es.into_iter().map(|e| { compile_expr(e, c, l) }).collect::<Vec<String>>().join("\n")
        }
        Expr::BinOp(Op2::Greater, e1, e2) => {
            let save_e1_ctxt = Context { target: LStack(c.si), ..*c };
            let e1_instrs = compile_expr(e1, &save_e1_ctxt, l);
            let e2_ctxt = Context { si: c.si + 1, target: LReg(RAX), ..*c };
            let e2_instrs = compile_expr(e2, &e2_ctxt, l);
            let offset = c.si * 8;
            let mov_instrs = mov_target(&c.target, &VReg(RAX));
            format!(
                "
                {e1_instrs}
                {e2_instrs}
                mov rbx, rax
                or rbx, [rsp - {offset}]
                test rbx, 1
                mov rbx, 1
                jne throw_error
                cmp [rsp - {offset}], rax
                mov rbx, 3
                mov rax, 1
                cmovg rax, rbx
                {mov_instrs}
            "
            )
        }
        Expr::BinOp(Op2::Less, e1, e2) => {
            let save_e1_ctxt = Context { target: LStack(c.si), ..*c };
            let e1_instrs = compile_expr(e1, &save_e1_ctxt, l);
            let e2_ctxt = Context { si: c.si + 1, target: LReg(RAX), ..*c };
            let e2_instrs = compile_expr(e2, &e2_ctxt, l);
            let offset = c.si * 8;
            let mov_instrs = mov_target(&c.target, &VReg(RAX));
            format!(
                "
                {e1_instrs}
                {e2_instrs}
                mov rbx, rax
                or rbx, [rsp - {offset}]
                test rbx, 1
                mov rbx, 1
                jne throw_error
                cmp [rsp - {offset}], rax
                mov rbx, 3
                mov rax, 1
                cmovl rax, rbx
                {mov_instrs}
            "
            )
        }
        Expr::BinOp(Op2::GreaterEqual, e1, e2) => {
            let save_e1_ctxt = Context { target: LStack(c.si), ..*c };
            let e1_instrs = compile_expr(e1, &save_e1_ctxt, l);
            let e2_ctxt = Context { si: c.si + 1, target: LReg(RAX), ..*c };
            let e2_instrs = compile_expr(e2, &e2_ctxt, l);
            let offset = c.si * 8;
            let mov_instrs = mov_target(&c.target, &VReg(RAX));
            format!(
                "
                {e1_instrs}
                {e2_instrs}
                mov rbx, rax
                or rbx, [rsp - {offset}]
                test rbx, 1
                mov rbx, 1
                jne throw_error
                cmp [rsp - {offset}], rax
                mov rbx, 3
                mov rax, 1
                cmovge rax, rbx
                {mov_instrs}
            "
            )
        }
        Expr::BinOp(Op2::LessEqual, e1, e2) => {
            let save_e1_ctxt = Context { target: LStack(c.si), ..*c };
            let e1_instrs = compile_expr(e1, &save_e1_ctxt, l);
            let e2_ctxt = Context { si: c.si + 1, target: LReg(RAX), ..*c };
            let e2_instrs = compile_expr(e2, &e2_ctxt, l);
            let offset = c.si * 8;
            let mov_instrs = mov_target(&c.target, &VReg(RAX));
            format!(
                "
                {e1_instrs}
                {e2_instrs}
                mov rbx, rax
                or rbx, [rsp - {offset}]
                test rbx, 1
                mov rbx, 1
                jne throw_error
                cmp [rsp - {offset}], rax
                mov rbx, 3
                mov rax, 1
                cmovle rax, rbx
                {mov_instrs}
            "
            )
        }
        Expr::BinOp(Op2::Equal, e1, e2) => {
            let save_e1_ctxt = Context { target: LStack(c.si), ..*c };
            let e1_instrs = compile_expr(e1, &save_e1_ctxt, l);
            let e2_ctxt = Context { si: c.si + 1, target: LReg(RAX), ..*c };
            let e2_instrs = compile_expr(e2, &e2_ctxt, l);
            let offset = c.si * 8;
            let mov_instrs = mov_target(&c.target, &VReg(RAX));
            format!(
                "
                {e1_instrs}
                {e2_instrs}
                mov rbx, rax
                xor rbx, [rsp - {offset}]
                test rbx, 1
                mov rbx, 1
                jne throw_error
                cmp rax, [rsp - {offset}]
                mov rbx, 3
                mov rax, 1
                cmove rax, rbx
                {mov_instrs}
            "
            )
        }
        Expr::If(cond, thn, els) => {
            let end_label = new_label(l, "ifend");
            let else_label = new_label(l, "ifelse");
            let cond_ctxt = Context { target: LReg(RAX), ..*c };
            let cond_instrs = compile_expr(cond, &cond_ctxt, l);
            let thn_instrs = compile_expr(thn, c, l); // note original context, so store to wherever caller wants
            let els_instrs = compile_expr(els, c, l);
            format!(
                "
                {cond_instrs}
                cmp rax, 1
                je {else_label}
                    {thn_instrs}
                    jmp {end_label}
                {else_label}:
                    {els_instrs}
                {end_label}:
           "
            )
        }
        Expr::BinOp(Op2::Plus, e1, e2) => {
            let save_e1_ctxt = Context { target: LStack(c.si), ..*c };
            let e1_instrs = compile_expr(e1, &save_e1_ctxt, l);
            let e2_ctxt = Context { si: c.si + 1, target: LReg(RAX), ..*c };
            let e2_instrs = compile_expr(e2, &e2_ctxt, l);
            let stack_offset = c.si * 8;
            let mov_instrs = mov_target(&c.target, &VReg(RAX));
            format!(
                "
                {e1_instrs}
                test qword [rsp - {stack_offset}], 1
                mov rbx, 1
                jnz throw_error
                {e2_instrs}
                test rax, 1
                mov rbx, 1
                jnz throw_error
                add rax, [rsp - {stack_offset}]
                mov rbx, 2
                JO throw_error
                {mov_instrs}
          "
            )
        }
        Expr::BinOp(Op2::Minus, e1, e2) => {
            let save_e1_ctxt = Context { target: LStack(c.si), ..*c };
            let e1_instrs = compile_expr(e1, &save_e1_ctxt, l);
            let e2_ctxt = Context { si: c.si + 1, target: LReg(RAX), ..*c };
            let e2_instrs = compile_expr(e2, &e2_ctxt, l);
            let stack_offset = c.si * 8;
            let mov_instrs = mov_target(&c.target, &VReg(RAX));
            format!(
                "
            {e1_instrs}
            test qword [rsp - {stack_offset}], 1
            mov rbx, 1
            jnz throw_error
            {e2_instrs}
            test rax, 1
            mov rbx, 1
            jnz throw_error
            neg rax
            add rax, [rsp - {stack_offset}]
            mov rbx, 2
            JO throw_error
            {mov_instrs}
          "
            )
        }
        Expr::BinOp(Op2::Times, e1, e2) => {
            let save_e1_ctxt = Context { target: LStack(c.si), ..*c };
            let e1_instrs = compile_expr(e1, &save_e1_ctxt, l);
            let e2_ctxt = Context { si: c.si + 1, target: LReg(RAX), ..*c };
            let e2_instrs = compile_expr(e2, &e2_ctxt, l);
            let stack_offset = c.si * 8;
            let mov_instrs = mov_target(&c.target, &VReg(RAX));
            format!(
                "
            {e1_instrs}
            test qword [rsp - {stack_offset}], 1
            mov rbx, 1
            jnz throw_error
            {e2_instrs}
            test rax, 1
            mov rbx, 1
            jnz throw_error
            sar rax, 1
            imul rax, [rsp - {stack_offset}]
            mov rbx, 2
            JO throw_error
            {mov_instrs}
          "
            )
        }
        Expr::Let(binding, body) => {
            // will store rolling contexts for the bindings and body
            let mut new_si = c.si;
            let mut new_env = c.env.clone();
            // first we bind all the variables through this map construction I only barely understand
            let bind_instrs = binding.into_iter().map(|(id, e)| {
                if new_env.contains_key(id) {
                    panic!("Duplicate binding");
                }
                else if id.eq(&String::from("input")) {
                    panic!("Variable cannot be keyword");
                }
                // update new_env and new_si
                new_env = new_env.update(id.to_string(), LStack(new_si));
                new_si += 1;
                // then compile the expression
                compile_expr(e, &Context { si: new_si, env: &new_env, target: LStack(new_si - 1), ..*c }, l)
                
            }).collect::<Vec<String>>().join("\n");
            // finally, evaluate the body
            let body_instrs = compile_expr(body, &Context { si: new_si, env: &new_env, ..*c}, l);
            format!("
              {bind_instrs}
              {body_instrs}
            ")
        }
        _ => panic!("compile error"),
    }
}

fn main() -> std::io::Result<()> {
  let args: Vec<String> = env::args().collect();

  let in_name = &args[1];
  let out_name = &args[2];

  let mut in_file = File::open(in_name)?;
  let mut in_contents = String::new();
  in_file.read_to_string(&mut in_contents)?;

  env::set_var("RUST_BACKTRACE", "1");

  let expr = parse_expr(&parse(&in_contents).unwrap());
  let mut labels = 0;
  let mut top_level_brake = Vector::new();
  top_level_brake.push_back(String::from(""));
  let context = Context { si: 2, env: &HashMap::new(), brake: &top_level_brake, target: LReg(RAX) };
  let result = compile_expr(&expr, &context, &mut labels);
  let asm_program = format!(
      "
section .text
global our_code_starts_here
extern snek_error
throw_error:
    mov rdi, rbx
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