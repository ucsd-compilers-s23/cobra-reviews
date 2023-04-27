use im::HashMap;
use sexp::Atom::*;
use sexp::*;
use std::env;
use std::fs::File;
use std::io::prelude::*;

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
        // Number
        Sexp::Atom(I(n)) => Expr::Number(i64::try_from(*n).unwrap()),

        // Boolean
        Sexp::Atom(S(name)) if name == "true" => Expr::Boolean(true),
        Sexp::Atom(S(name)) if name == "false" => Expr::Boolean(false),

        // Id
        Sexp::Atom(S(id)) => Expr::Id(id.clone()),

        Sexp::List(vec) => match &vec[..] {
            // add1
            [Sexp::Atom(S(op)), e] if op == "add1" => {
                Expr::UnOp(Op1::Add1, Box::new(parse_expr(e)))
            }

            // sub1
            [Sexp::Atom(S(op)), e] if op == "sub1" => {
                Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e)))
            }

            // isnum
            [Sexp::Atom(S(op)), e] if op == "isnum" => {
                Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e)))
            }

            // isbool
            [Sexp::Atom(S(op)), e] if op == "isbool" => {
                Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e)))
            }

            // set!
            [Sexp::Atom(S(op)), Sexp::Atom(S(name)), e] if op == "set!" => {
                // let mut seen_vars = std::collections::HashSet::new();
                // if seen_vars.contains(name) {
                //     panic!("Duplicate binding: {}", name);
                // }
                // seen_vars.insert(name.clone());
                if name.to_string() == "input" {
                    panic!("Invalid");
                }
                Expr::Set(name.to_string(), Box::new(parse_expr(e)))
            }

            // block
            [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => {
                if exprs.is_empty() {
                    panic!("Invalid");
                }
                Expr::Block(exprs.into_iter().map(parse_expr).collect())
            }

            // loop
            [Sexp::Atom(S(op)), e] if op == "loop" => Expr::Loop(Box::new(parse_expr(e))),

            // break
            [Sexp::Atom(S(op)), e] if op == "break" => Expr::Break(Box::new(parse_expr(e))),

            // let
            [Sexp::Atom(S(op)), Sexp::List(bindings), body] if op == "let" => {
                let mut bindings_vector = Vec::new();
                let mut seen_vars = std::collections::HashSet::new(); // to check for duplicate bindings
                if bindings.is_empty() {
                    panic!("Invalid");
                }
                for binding in bindings {
                    // check if binding is a list of expressions
                    if let Sexp::List(binding_vec) = binding {
                        // check if binding is of this form: [id, expr]
                        if let [Sexp::Atom(S(id)), expr] = &binding_vec[..] {
                            if id == "input" {
                                panic!("keyword used in binding");
                            }
                            if seen_vars.contains(id) {
                                panic!("Duplicate binding: {}", id);
                            }
                            seen_vars.insert(id.clone());
                            let sub_expr = parse_expr(expr);
                            bindings_vector.push((id.clone(), sub_expr));
                        } else {
                            panic!("Invalid binding form: {:?}", binding_vec);
                        }
                    } else {
                        panic!("Invalid binding list: {:?}", binding);
                    }
                }
                let parsed_body = Box::new(parse_expr(body));
                Expr::Let(bindings_vector, parsed_body)
            }

            // BinOp
            [Sexp::Atom(S(op1)), e1, e2] => {
                let op2 = match op1.as_str() {
                    "+" => Op2::Plus,
                    "-" => Op2::Minus,
                    "*" => Op2::Times,
                    ">" => Op2::Greater,
                    ">=" => Op2::GreaterEqual,
                    "<" => Op2::Less,
                    "<=" => Op2::LessEqual,
                    "=" => Op2::Equal,
                    _ => panic!("Invalid operator {} for BinOp", op1),
                };
                let left = Box::new(parse_expr(e1));
                let right = Box::new(parse_expr(e2));
                Expr::BinOp(op2, left, right)
            }
            [Sexp::Atom(S(keyword)), cond, thn, els] if keyword == "if" => Expr::If(
                Box::new(parse_expr(cond)),
                Box::new(parse_expr(thn)),
                Box::new(parse_expr(els)),
            ),
            _ => panic!("Invalid"),
        },
        _ => panic!("Invalid"),
    }
}

fn new_label(l: &mut i32, s: &str) -> String {
    let current = *l;
    *l += 1;
    format!("{s}_{current}")
}

fn compile_expr(
    e: &Expr,
    mut si: i32,
    env: &HashMap<String, i32>,
    brake: &String,
    l: &mut i32,
) -> String {
    println!("this is env: {:?}", env);
    match e {
        Expr::Number(n) => match n.checked_mul(2) {
            Some(_v) => {
                format!("mov rax, {}", *n << 1)
            }
            None => {
                panic!("Invalid");
            }
        },
        Expr::Id(s) if s == "input" => format!("mov rax, rdi"),
        Expr::Boolean(b) => {
            if b == &true {
                format!("mov rax, 3")
            } else {
                format!("mov rax, 1")
            }
        }
        Expr::Id(s) => {
            let offset = env
                .get(s)
                .unwrap_or_else(|| panic!("Unbound variable identifier {} || keyword used", s));
            format!("mov rax, [rsp-{}]", offset)
        }
        Expr::UnOp(op, subexpr) => match op {
            Op1::Add1 => {
                let add1_instrs = compile_expr(subexpr, si, env, brake, l);

                format!(
                    "
                    {add1_instrs}
                    test rax, 1
                    mov rbx, 3
                    jnz throw_error
                    add rax, 2
                    jo throw_error
                "
                )
            }
            Op1::Sub1 => {
                let sub1_instrs = compile_expr(subexpr, si, env, brake, l);

                format!(
                    "
                    {sub1_instrs}
                    test rax, 1
                    mov rbx, 3
                    jnz throw_error
                    sub rax, 2
                    jo throw_error
                "
                )
            }
            Op1::IsBool => {
                let bool_instrs = compile_expr(subexpr, si, env, brake, l);
                format!(
                    "
                    {bool_instrs}
                    and rax, 1
                    cmp rax, 1
                    mov rax, 1
                    mov rbx, 3
                    cmove rax, rbx
                "
                )
            }
            Op1::IsNum => {
                let num_instrs = compile_expr(subexpr, si, env, brake, l);
                format!(
                    "
                    {num_instrs}
                    and rax, 1
                    cmp rax, 1
                    mov rax, 3
                    mov rbx, 1
                    cmove rax, rbx
                "
                )
            }
        },
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
        Expr::Break(e) => {
            let e_is = compile_expr(e, si, env, brake, l);
            if brake == "" {
                panic!("Invalid break")
            }
            format!(
                "
              {e_is}
              jmp {brake}
            "
            )
        }
        Expr::Set(name, val) => {
            let offset = env
                .get(name)
                .unwrap_or_else(|| panic!("Unbound variable identifier {} || keyword used", name));
            // let offset = env.get(name).unwrap();

            let save = format!("mov [rsp - {offset}], rax");
            let val_is = compile_expr(val, si, env, brake, l);
            println!("Inside set, this is offset: {}", offset);
            format!(
                "
              {val_is}
              {save}
              "
            )
        }
        Expr::Block(es) => es
            .into_iter()
            .map(|e| compile_expr(e, si, env, brake, l))
            .collect::<Vec<String>>()
            .join("\n"),
        Expr::BinOp(op, left, right) => {
            let e1_instrs = compile_expr(left, si, env, brake, l);
            let e2_instrs = compile_expr(right, si + 1, env, brake, l);
            let stack_offset = si * 8;
            match op {
                Op2::Plus => format!(
                    "{e1_instrs}
                    test rax, 1
                    mov rbx, 3
                    jnz throw_error
                    mov [rsp - {stack_offset}], rax
                    {e2_instrs}
                    test rax, 1
                    mov rbx, 3
                    jnz throw_error
                    add rax, [rsp - {stack_offset}]
                    jo throw_error
                "
                ),
                Op2::Minus => format!(
                    "{e1_instrs}
                    test rax, 1
                    mov rbx, 3
                    jnz throw_error
                    mov [rsp - {stack_offset}], rax
                    {e2_instrs}
                    test rax, 1
                    mov rbx, 3
                    jnz throw_error
                    sub [rsp - {stack_offset}], rax
                    mov rax, [rsp - {stack_offset}]
                    jo throw_error
                "
                ),
                Op2::Times => format!(
                    "{e1_instrs}
                    sar rax, 1
                    mov [rsp - {stack_offset}], rax
                    {e2_instrs}
                    imul rax, [rsp - {stack_offset}]
                    jo throw_error
                "
                ),
                Op2::Equal => {
                    format!(
                        "
                        {e1_instrs}
                    mov [rsp - {stack_offset}], rax
                    {e2_instrs}
                    mov rbx, rax
                    xor rbx, [rsp - {stack_offset}]
                    test rbx, 1
                    jne throw_error
                    cmp rax, [rsp - {stack_offset}]
                    mov rbx, 3
                    mov rax, 1
                    cmove rax, rbx
                "
                    )
                }
                Op2::Less => format!(
                    "
                    {e1_instrs}
                    mov [rsp - {stack_offset}], rax
                    {e2_instrs}
                    mov rbx, rax
                    xor rbx, [rsp - {stack_offset}]
                    test rbx, 1
                    jne throw_error
                    cmp [rsp - {stack_offset}], rax
                    mov rbx, 3
                    mov rax, 1
                    cmovl rax, rbx
                "
                ),
                Op2::LessEqual => format!(
                    "{e1_instrs}
                    mov [rsp - {stack_offset}], rax
                    {e2_instrs}
                    mov rbx, rax
                    xor rbx, [rsp - {stack_offset}]
                    test rbx, 1
                    jne throw_error
                    cmp [rsp - {stack_offset}], rax
                    mov rbx, 3
                    mov rax, 1
                    cmovle rax, rbx
                "
                ),
                Op2::Greater => format!(
                    "{e1_instrs}
                    mov [rsp - {stack_offset}], rax
                    {e2_instrs}
                    mov rbx, rax
                    xor rbx, [rsp - {stack_offset}]
                    test rbx, 1
                    jne throw_error
                    cmp [rsp - {stack_offset}], rax
                    mov rbx, 3
                    mov rax, 1
                    cmovg rax, rbx
                "
                ),
                Op2::GreaterEqual => format!(
                    "{e1_instrs}
                    mov [rsp - {stack_offset}], rax
                    {e2_instrs}
                    mov rbx, rax
                    xor rbx, [rsp - {stack_offset}]
                    test rbx, 1
                    jne throw_error
                    cmp [rsp - {stack_offset}], rax
                    mov rbx, 3
                    mov rax, 1
                    cmovge rax, rbx
                "
                ),
            }
        }
        Expr::Let(bindings, body) => {
            let mut env = env.clone();
            let mut instrs = String::new();
            for (var, expr) in bindings {
                let stack_offset = si * 8;
                let e_instrs = compile_expr(expr, si, &env, brake, l);
                let nenv = env.update(var.clone(), stack_offset);
                // let b_instrs = compile_expr(body, si + 1, &nenv);
                instrs += &format!(
                    "{e_instrs}
                    mov [rsp - {stack_offset}], rax
                ",
                );
                si += 1;
                env = nenv;
            }
            instrs + &compile_expr(body, si, &env, brake, l)
        }
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    // You will make result hold the result of actually compiling
    let parsed_sexpr = parse(&in_contents);
    let mut labels = 0;
    let expr = match parsed_sexpr {
        Ok(sexpr) => parse_expr(&sexpr),
        Err(err) => panic!("Invalid expression: {}", err),
    };
    println!("This is expr: {:?}", expr);
    let env = HashMap::new();
    let result = compile_expr(&expr, 2, &env, &String::from(""), &mut labels);
    println!("This is result: {:?}", result);

    let asm_program = format!(
        "
section .text
extern snek_error
global our_code_starts_here
throw_error:
    mov rdi, 7
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
