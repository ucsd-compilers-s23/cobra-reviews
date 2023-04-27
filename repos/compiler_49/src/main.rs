use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::Atom::*;
use sexp::*;

// to store variable - address mapping
use im::HashMap;
// to check for duplicate bindings
use std::collections::HashSet;

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

// parses the unary operator string to Op1
fn parse_un_op(op: &str) -> Op1 {
    match op {
        "add1" => Op1::Add1,
        "sub1" => Op1::Sub1,
        "isnum" => Op1::IsNum,
        "isbool" => Op1::IsBool,
        _ => panic!("Invalid unary operator"),
    }
}

// parses the binary operator string to Op2
fn parse_bi_op(op: &str)-> Op2 {
    match op {
        "+" => Op2::Plus,
        "-" => Op2::Minus,
        "*" => Op2::Times,
        "=" => Op2::Equal,
        ">" => Op2::Greater,
        ">=" => Op2::GreaterEqual,
        "<" => Op2::Less,
        "<=" => Op2::LessEqual,
        _ => panic!("Invalid binary operator"),
    }
}

// Given a (String, Expr) binding, returns the variable name and the parsed expression
fn parse_bind(s: &Sexp) -> (String, Expr) {
    match &s {
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(id)), e] =>
                    // check for reserved keywords
                    match id.as_str() {
                        "let" | "add1" | "sub1" | "loop" | "block" | "if" | "break" | "true" | "false" | "input" | "set!" | "isnum" | "isbool" => panic!("Invalid: reserved keyword {}", id),
                        _ => (id.to_string(), parse_expr(e)),
                    },
                _ => panic!("Invalid type"),
            }
        },
        _ => panic!("Invalid type"),
    }
}

// sexpr to expr
fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => Expr::Number(i64::try_from(*n).unwrap_or_else(  // check if the number is within i64 range
            |_| panic!("Invalid: failed to convert to i64")
        )),
        Sexp::Atom(S(name)) if name == "true" => Expr::Boolean(true),
        Sexp::Atom(S(name)) if name == "false" => Expr::Boolean(false),
        Sexp::Atom(S(var_name)) => {
            // check for reserved keywords
            match var_name.as_str() {"let" | "add1" | "sub1" | "loop" | "block" | "if" | "break" | "true" | "false" | "set!" | "isnum" | "isbool" => panic!("Invalid: reserved keyword {}", var_name),
                _ => Expr::Id(var_name.to_string())
            }
        },
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(op)), rest @ ..] if op == "block" => {
                    // check for empty block
                    if rest.len() == 0 {
                        panic!("Invalid: empty block");
                    }

                    // store the expressions in a vector
                    let mut expressions = Vec::new();
                    for expr in rest {
                        expressions.push(parse_expr(expr));
                    }
                    Expr::Block(expressions)
                },
                [Sexp::Atom(S(op)), Sexp::Atom(S(var_name)), e] if op == "set!" => Expr::Set(var_name.to_string(), Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), Sexp::List(bindings), body]  if op == "let" => {
                    let mut bindings_vec = Vec::new();

                    // check for no bindings
                    if bindings.len() == 0 {
                        panic!("Invalid: no bindings");
                    }

                    // check for duplicate bindings
                    let mut var_names: HashSet<String> = HashSet::new();

                    // store the bindings in a vector
                    for binding in bindings {
                        let parsed_binding = parse_bind(binding);
                        if var_names.contains(&parsed_binding.0) {
                            panic!("Duplicate binding for variable {}", parsed_binding.0);
                        }
                        var_names.insert(parsed_binding.0.clone());
                        bindings_vec.push(parsed_binding);
                    }

                    Expr::Let(bindings_vec, Box::new(parse_expr(body)))
                },
                [Sexp::Atom(S(op)), e] if op == "loop" => Expr::Loop(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "break" => Expr::Break(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] => Expr::UnOp(parse_un_op(op), Box::new(parse_expr(e))), // parses all unary operators
                [Sexp::Atom(S(op)), e1, e2] => Expr::BinOp(parse_bi_op(op), Box::new(parse_expr(e1)), Box::new(parse_expr(e2))), // parses all binary operators
                [Sexp::Atom(S(keyword)), cond, thn, els] if keyword == "if" => {
                    Expr::If(Box::new(parse_expr(cond)), Box::new(parse_expr(thn)), Box::new(parse_expr(els)))
                },
                _ => panic!("Invalid Expression"),
            }
        }
        _ => panic!("Invalid Grammar, {:?}", s),    // grammar does not match our defined syntax
    }
}

// checks if input is an integer by performing logical AND with 1
fn validate_input_int_unary() -> String {
    format!("; -- validate input types --
  mov rbx, rax
  mov rcx, 101
  and rbx, 1
  cmp rbx, 1
  cmove rdi, rcx
  je throw_error")
}

// checks if previous operation sets the overflow flag, throws error if it does
fn validate_overflow() -> String {
    format!("\n  ; -- validate input types --
  mov rcx, 102
  cmovo rdi, rcx
  jo throw_error")
}

// Compilers all unary operators
fn compile_unop(op: &Op1, subexpr: &Expr, si: i32, env: &HashMap<String, i32>, l: &mut i32, break_target: &str) -> String {
    match op {
        Op1::Add1 => compile_expr(subexpr, si, env, l, break_target)
            + validate_input_int_unary().as_str()       // type check
            + "\n  add rax, 2"  // 2 here is equal to 1 with a 0 in the rightmost bit
            + validate_overflow().as_str(),             // overflow check
        Op1::Sub1 => compile_expr(subexpr, si, env, l, break_target)
            + validate_input_int_unary().as_str()
            + "\n  sub rax, 2"
            + validate_overflow().as_str(),
        Op1::IsNum => {
            let e_instr = compile_expr(subexpr, si, env, l, break_target);
            // if the rightmost bit (moved to carry using shr) is 0 then it is a number (true/3) else we cmovb false (1)
            // cmovb checks the state of carry flag
            format!("{0}
  sar rax, 1
  mov rax, 3
  mov rbx, 1
  cmovb rax, rbx", e_instr)
        },
        Op1::IsBool => {
            let e_instr = compile_expr(subexpr, si, env, l, break_target);
            format!("{0}
  sar rax, 1
  mov rax, 1
  mov rbx, 3
  cmovbe rax, rbx", e_instr)
        },
    }
}

// Checks if the input in rax and dest are of the integer type using logical AND
fn validate_input_int(dest: i32) -> String {
    format!("; -- validate input types --
  mov rbx, rax
  mov rcx, 101
  or rbx, [rsp - {}]
  and rbx, 1
  cmp rbx, 1
  cmove rdi, rcx
  je throw_error", dest)
}

// Checks if the input in rax and dest are of the same type
fn validate_input_types_same(dest: i32) -> String {
    format!("; -- validate input types --
  mov rbx, rax
  mov rcx, 101
  xor rbx, [rsp - {}]
  test rbx, 1
  cmovne rdi, rcx
  jne throw_error", dest)
}

// A template for all comparison instructions except for equals.
fn get_compare_instructions(e1_instr: String, e2_instr: String, si: i32, cmov_op: &str) -> String {
    let validate_instr = validate_input_int(si*8);
    format!("  \n  ; -- compare begins --{0}
  mov [rsp-{1}], rax{2}
  {4}
  cmp [rsp-{1}], rax
  mov rax, 1
  mov rbx, 3
  {3} rax, rbx", e1_instr, si*8, e2_instr, cmov_op, validate_instr)
}

// Compiles all binary operators
fn compile_binop(op: &Op2, e1: &Expr, e2: &Expr, si: i32, env: &HashMap<String, i32>, l: &mut i32, break_target: &str) -> String {
    let e1_instr = compile_expr(e1, si, env, l, break_target);
    let e2_instr = compile_expr(e2, si+1, env, l, break_target);
    match op {
        Op2::Plus => {
            let validate_instr = validate_input_int(si*8);
            format!("  \n  ; -- + begins --{0}
  mov [rsp-{1}], rax{2}
  {3}
  add rax, [rsp-{1}]{4}", e1_instr, si*8, e2_instr, validate_instr, validate_overflow().as_str())
        },
        Op2::Minus => {
            let validate_instr = validate_input_int(si*8);
            format!("  \n  ; -- - begins --{0}
  mov [rsp-{1}], rax{2}
  {3}
  sub [rsp-{1}], rax
  mov rax, [rsp-{1}]{4}", e1_instr, si*8, e2_instr, validate_instr, validate_overflow().as_str())
        },
        Op2::Times => {
            let validate_instr = validate_input_int(si*8);
            format!("  \n  ; -- multiply begins --{0}
  mov [rsp-{1}], rax{2}
  {3}
  sar rax, 1
  sar qword[rsp-{1}], 1
  imul rax, [rsp-{1}]{4}
  sal rax, 1{4}", e1_instr, si*8, e2_instr, validate_instr, validate_overflow().as_str())
        },
        Op2::Equal => {
            // We check if both operands are of same type here
            let validate_instr = validate_input_types_same(si*8);
            // In the below snippet, we compare the two expr results and then move 1 into rax for false. If the two expr results are equal, we overwrite 3 into rax for true.
            format!("  \n  ; -- compare = begins --{0}
  mov [rsp-{1}], rax{2}
  {3}
  cmp [rsp-{1}], rax
  mov rax, 1
  mov rbx, 3
  cmove rax, rbx", e1_instr, si*8, e2_instr, validate_instr)  // 3 is true, 1 is false
        },
        Op2::Greater => get_compare_instructions(e1_instr, e2_instr, si, "cmovnle"),
        Op2::Less =>  get_compare_instructions(e1_instr, e2_instr, si, "cmovnge"),
        Op2::GreaterEqual =>  get_compare_instructions(e1_instr, e2_instr, si, "cmovnl"),
        Op2::LessEqual => get_compare_instructions(e1_instr, e2_instr, si, "cmovng"),
    }
}

// returns a new label
fn new_label(l : &mut i32, s : &str) -> String {
    let current = *l;
    *l += 1;
    format!("{s}_{current}")
}

// expr to assembly
fn compile_expr(e: &Expr, si: i32, env: &HashMap<String, i32>, l: &mut i32, break_target: &str) -> String {
    match e {
        Expr::Number(n) => {
            if n >= &4611686018427387904 || n < &-4611686018427387904 {
                panic!("Invalid: overflow for number")
            }
            format!("\n  mov rax, {}", *n<<1)
        },
        Expr::Boolean(b) => if *b {
            format!("\n  mov rax, {}", 3)
        } else {
            format!("\n  mov rax, {}", 1)
        },
        Expr::Id(s) => if s == "input" {
            format!("\n  mov rax, rdi")
        }
        else if env.contains_key(s) {
            format!("\n  mov rax, [rsp-{}]", env.get(s).unwrap())
        } else {
            panic!("Unbound variable identifier {}", s)
        },
        Expr::Let(bindings, body) => {
            let mut instr = String::from("");
            let mut si_: i32 = si;
            let mut new_env = env.clone();
            for binding in bindings {
                let binding_instr = compile_expr(&binding.1, si_, &new_env, l, break_target);
                new_env = new_env.update(binding.0.clone(), si_*8);
                instr.push_str(&format!("{}\n  mov [rsp-{}], rax", binding_instr, si_*8));
                si_ += 1;
            }
            let body = compile_expr(body, si_, &new_env, l, break_target);
            format!("{}{}", instr, body)

        },
        Expr::UnOp(op, subexpr) => compile_unop(op, subexpr, si, env, l, break_target),
        Expr::BinOp(op, e1, e2) => compile_binop(op, e1, e2, si, env, l, break_target),
        Expr::If(cond, thn, els) => {
            let end_label = new_label(l, "endif");
            let else_label = new_label(l, "else");
            let cond_instrs = compile_expr(cond, si, env, l, break_target);
            let then_instrs = compile_expr(thn, si, env, l, break_target);
            let else_instrs = compile_expr(els, si, env, l, break_target);
            format!("  \n  ; -- if-else begins --{cond_instrs}
  cmp rax, 1
  je {else_label}{then_instrs}
  jmp {end_label}
  {else_label}:{else_instrs}
  {end_label}:
  ; -- if-else ends --")
         },
         Expr::Loop(body) => {
            let loop_begin_label = new_label(l, "loop_begin");
            let loop_end_label = new_label(l, "loop_end");
            let body_instrs = compile_expr(body, si, env, l, &loop_end_label);
            format!("  \n  ; -- loop begins --
  {loop_begin_label}:{body_instrs}
  jmp {loop_begin_label}
  {loop_end_label}:
   ; -- loop ends --")
         },
         Expr::Break(body) => {
            let body_instr = compile_expr(body, si, env, l, break_target);
            // check if the break occurs outside of any parent loop
            if break_target == "" {
               panic!("break outside of loop")
            }
            format!("{body_instr}
  jmp {break_target}")
         },
         Expr::Set(id, e) => if env.contains_key(id) {
            format!("{}\n  mov [rsp - {}], rax", &compile_expr(e, si, env, l, break_target), env.get(id).unwrap())
         } else {
            panic!("Unbound variable identifier {}", id)
         },
         Expr::Block(exprs) => {
            let mut instructions = String::new();
            for e in exprs {
                instructions.push_str(&compile_expr(e, si, env, l, break_target));
            }
            instructions
         },
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    // read file contents
    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    // initialize the hashmap for the let bindings
    let env: HashMap<String, i32> = HashMap::new();

    // initialize stack index
    let si = 2;

    let expr = parse_expr(&parse(&in_contents).unwrap_or_else(
        |_| panic!("Invalid")
    ));

    let mut labels = 0;
    let result = compile_expr(&expr, si, &env, &mut labels, "");

    let asm_program = format!(
        "
section .text
global our_code_starts_here
extern snek_error
throw_error:
  push rsp
  call snek_error
our_code_starts_here:{}
  ret
",
        result
    );

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}
