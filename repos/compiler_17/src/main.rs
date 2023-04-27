use sexp::Atom::*;
use sexp::*;
use std::collections::HashSet;
use std::env;
use std::fs::File;
use std::io::prelude::*;

use im::HashMap;

#[derive(Clone, Debug)]
enum Op1 {
    Add1,
    Sub1,
    IsNum,
    IsBool,
}
#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

const ERROR_IF_OVERFLOW: &str = "
mov rdx, rdi
mov rdi, 9
jo throw_error
mov rdi, rdx
";

const IF_BOOL_ERROR: &str = "test rax, 1
mov rdx, rdi
mov rdi, 3
jnz throw_error
mov rdi, rdx
";

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let mut labels = 0;
    let sexp_parsed = parse(&in_contents).expect("Invalid input, SExp parse failed");
    let parsed = parse_expr(&sexp_parsed);
    let result = compile_expr(&parsed, 2, &HashMap::new(), &String::from(""), &mut labels);
    let asm_program = format!(
        "
section .text
global our_code_starts_here
extern snek_error
extern snek_print
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
        Sexp::Atom(I(n)) => Expr::Number(match i64::try_from(*n) {
            Ok(n_as_i64) => {
                // Yes this is a dumb way to do this
                if n_as_i64 < -4611686018427387904 || n_as_i64 > 4611686018427387903 {
                    panic!("Invalid number, larger than 63 bits: {}", n)
                }
                n_as_i64
            }
            Err(_) => panic!("Invalid number, larger than 64 bits: {}", n),
        }),
        Sexp::Atom(S(name)) => match &name[..] {
            "true" => Expr::Boolean(true),
            "false" => Expr::Boolean(false),
            "add1" | "sub1" | "isnum" | "isbool" | "+" | "-" | "*" | "=" | ">" | ">=" | "<"
            | "<=" | "let" | "if" | "loop" | "break" | "set!" | "block" => {
                panic!("Invalid identifier: cannot use a keyword")
            }
            _ => Expr::Id(name.to_string()),
        },
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(op)), Sexp::Atom(S(name)), e] if op == "set!" => {
                Expr::Set(name.to_string(), Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => {
                if exprs.is_empty() {
                    panic!("Invalid block, no expressions");
                } else {
                    Expr::Block(exprs.into_iter().map(parse_expr).collect())
                }
            }
            [Sexp::Atom(S(keyword)), Sexp::List(bindings), body] if keyword == "let" => Expr::Let(
                if bindings.is_empty() {
                    panic!("Invalid let, no bindings")
                } else {
                    bindings.iter().map(parse_bind).collect()
                },
                Box::new(parse_expr(body)),
            ),
            [Sexp::Atom(S(keyword)), cond, thn, els] if keyword == "if" => Expr::If(
                Box::new(parse_expr(cond)),
                Box::new(parse_expr(thn)),
                Box::new(parse_expr(els)),
            ),
            [Sexp::Atom(S(op)), e] if op == "loop" => Expr::Loop(Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "break" => Expr::Break(Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] => Expr::UnOp(
                match &op[..] {
                    "add1" => Op1::Add1,
                    "sub1" => Op1::Sub1,
                    "isnum" => Op1::IsNum,
                    "isbool" => Op1::IsBool,
                    _ => panic!("Invalid unary operator {}", op),
                },
                Box::new(parse_expr(e)),
            ),
            [Sexp::Atom(S(op)), e1, e2] => Expr::BinOp(
                match &op[..] {
                    "+" => Op2::Plus,
                    "-" => Op2::Minus,
                    "*" => Op2::Times,
                    "=" => Op2::Equal,
                    ">" => Op2::Greater,
                    ">=" => Op2::GreaterEqual,
                    "<" => Op2::Less,
                    "<=" => Op2::LessEqual,
                    _ => panic!("Invalid binary operator {}", op),
                },
                Box::new(parse_expr(e1)),
                Box::new(parse_expr(e2)),
            ),
            _ => panic!("Invalid parsing error: {}", s),
        },
        _ => panic!("Invalid parsing error: {}", s),
    }
}

fn parse_bind(s: &Sexp) -> (String, Expr) {
    match s {
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(id)), e] => match &id[..] {
                "add1" | "sub1" | "isnum" | "isbool" | "+" | "-" | "*" | "=" | ">" | ">=" | "<"
                | "<=" | "let" | "if" | "loop" | "break" | "set!" | "block" | "input" => {
                    panic!("Invalid identifier: cannot use a keyword")
                }
                _ => (id.to_string(), parse_expr(e)),
            },
            _ => panic!("Invalid binding"),
        },
        _ => panic!("Invalid binding"),
    }
}

fn new_label(l: &mut i32, s: &str) -> String {
    let current = *l;
    *l += 1;
    format!("{s}_{current}")
}

fn compile_expr(
    e: &Expr,
    si: i32,
    env: &HashMap<String, i32>,
    brake: &String,
    l: &mut i32,
) -> String {
    match e {
        Expr::Number(n) => format!("mov rax, {}", *n << 1),
        Expr::Boolean(true) => format!("mov rax, {}", 3),
        Expr::Boolean(false) => format!("mov rax, {}", 1),
        Expr::Id(s) if s == "input" => format!("mov rax, rdi"),
        Expr::Id(s) => match env.get(s) {
            Some(offset) => format!("mov rax, [rsp - {}]", offset * 8),
            None => panic!("Unbound variable identifier {}", s),
        },
        Expr::Set(name, val) => {
            let offset = match env.get(name) {
                Some(offset) => offset * 8,
                None => panic!("Unbound variable identifier {}", name),
            };
            let save = format!("mov [rsp - {offset}], rax");
            let val_is = compile_expr(val, si, env, brake, l);
            format!(
                "
{val_is}
{save}
"
            )
        }
        Expr::UnOp(op, e) => match op {
            Op1::Add1 => format!(
                "
{}
{IF_BOOL_ERROR}
add rax, 2
{ERROR_IF_OVERFLOW}
",
                compile_expr(e, si, env, brake, l)
            ),
            Op1::Sub1 => format!(
                "
{}
{IF_BOOL_ERROR}
sub rax, 2
{ERROR_IF_OVERFLOW}
",
                compile_expr(e, si, env, brake, l)
            ),
            Op1::IsNum => format!(
                "
{}
test rax, 1
mov rbx, 3
mov rax, 1
cmove rax, rbx
",
                compile_expr(e, si, env, brake, l)
            ),
            Op1::IsBool => format!(
                "
{}
test rax, 1
mov rbx, 1
mov rax, 3
cmove rax, rbx
",
                compile_expr(e, si, env, brake, l)
            ),
        },
        Expr::BinOp(op, e1, e2) => match op {
            Op2::Plus => {
                let e1_instrs = compile_expr(e1, si, env, brake, l);
                let e2_instrs = compile_expr(e2, si + 1, env, brake, l);
                let stack_offset = si * 8;
                format!(
                    "
{e1_instrs}
{IF_BOOL_ERROR}
mov [rsp - {stack_offset}], rax
{e2_instrs}
{IF_BOOL_ERROR}
add rax, [rsp - {stack_offset}]
{ERROR_IF_OVERFLOW}
"
                )
            }
            Op2::Minus => {
                let e1_instrs = compile_expr(e1, si + 1, env, brake, l);
                let e2_instrs = compile_expr(e2, si, env, brake, l);
                let stack_offset = si * 8;
                format!(
                    "
{e2_instrs}
{IF_BOOL_ERROR}
mov [rsp - {stack_offset}], rax
{e1_instrs}
{IF_BOOL_ERROR}
sub rax, [rsp - {stack_offset}]
{ERROR_IF_OVERFLOW}
"
                )
            }
            Op2::Times => {
                let e1_instrs = compile_expr(e1, si, env, brake, l);
                let e2_instrs = compile_expr(e2, si + 1, env, brake, l);
                let stack_offset = si * 8;
                format!(
                    "
{e1_instrs}
{IF_BOOL_ERROR}
mov [rsp - {stack_offset}], rax
{e2_instrs}
{IF_BOOL_ERROR}
sar rax, 1
imul rax, [rsp - {stack_offset}]
{ERROR_IF_OVERFLOW}
"
                )
            }
            Op2::Equal => {
                let e1_instrs = compile_expr(e1, si, env, brake, l);
                let e2_instrs = compile_expr(e2, si + 1, env, brake, l);
                let offset = si * 8;
                format!(
                    "
{e1_instrs}
mov [rsp - {offset}], rax
{e2_instrs}
mov rbx, rax
xor rbx, [rsp - {offset}]
test rbx, 1
mov rdi, 7
jne throw_error
cmp rax, [rsp - {offset}]
mov rbx, 3
mov rax, 1
cmove rax, rbx
"
                )
            }
            Op2::Greater | Op2::GreaterEqual | Op2::Less | Op2::LessEqual => {
                let not = new_label(l, "not");
                let e1_instrs = compile_expr(e1, si + 1, env, brake, l);
                let e2_instrs = compile_expr(e2, si, env, brake, l);
                let offset = si * 8;
                let cmp_instr = match op {
                    Op2::Greater => "jg",
                    Op2::GreaterEqual => "jge",
                    Op2::Less => "jl",
                    Op2::LessEqual => "jle",
                    _ => panic!("Impossible"),
                };
                format!(
                    "
{e2_instrs}
{IF_BOOL_ERROR}
mov [rsp - {offset}], rax
{e1_instrs}
{IF_BOOL_ERROR}
cmp rax, [rsp - {offset}]
mov rax, 3
{cmp_instr} {not}
mov rax, 1
{not}:
"
                )
            }
        },
        Expr::Break(e) => {
            if brake.is_empty() {
                panic!("break outside of loop");
            }
            let e_is = compile_expr(e, si, env, brake, l);
            format!(
                "
{e_is}
jmp {brake}
"
            )
        }
        Expr::Loop(e) => {
            let startloop = new_label(l, "loop");
            let endloop = new_label(l, "loopend");
            let e_is = compile_expr(e, si, env, &endloop, l);
            format!(
                "
{startloop}:
{e_is}
jmp {startloop}
{endloop}:
"
            )
        }
        Expr::Block(es) => es
            .into_iter()
            .map(|e| compile_expr(e, si, env, brake, l))
            .collect::<Vec<String>>()
            .join("\n"),
        Expr::If(cond, thn, els) => {
            let end_label = new_label(l, "ifend");
            let else_label = new_label(l, "ifelse");
            let cond_instrs = compile_expr(cond, si, env, brake, l);
            let thn_instrs = compile_expr(thn, si, env, brake, l);
            let els_instrs = compile_expr(els, si, env, brake, l);
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
        Expr::Let(bindings, body) => {
            if bindings_has_duplicates(bindings) {
                panic!("Duplicate binding in let expression")
            }

            let (val_is, new_env) = bind(bindings, si, env, brake, l);
            let body_is = compile_expr(
                body,
                si + 1 + ((bindings.len() * 8) as i32),
                &new_env,
                brake,
                l,
            );
            format!(
                "
{val_is}
{body_is}
"
            )
        }
    }
}

fn bind(
    bindings: &Vec<(String, Expr)>,
    si: i32,
    env: &HashMap<String, i32>,
    brake: &String,
    l: &mut i32,
) -> (String, HashMap<String, i32>) {
    if bindings.is_empty() {
        return ("".to_string(), env.clone());
    }

    let offset = si * 8;
    let name = &bindings[0].0;
    let val = &bindings[0].1;
    let val_is = compile_expr(val, si, env, brake, l);

    let (rest_is, new_env) = bind(
        &bindings[1..].to_vec(),
        si + 1,
        &env.update(name.to_string(), si),
        brake,
        l,
    );

    (
        format!(
            "
{val_is}
mov [rsp - {offset}], rax
{rest_is}
"
        ),
        new_env,
    )
}

fn bindings_has_duplicates(v: &Vec<(String, Expr)>) -> bool {
    let mut set = HashSet::new();
    for (s, _e) in v {
        if !set.insert(s) {
            return true;
        }
    }
    false
}
