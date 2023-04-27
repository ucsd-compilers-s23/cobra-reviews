use sexp::Atom::*;
use sexp::*;
use std::env;
use std::fs::File;
use std::io::prelude::*;

use im::hashmap;
use im::HashMap;
use regex::Regex;

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
        Sexp::Atom(I(n)) => {
            // if the number is greater than i63, should error invalid
            let result = i64::try_from(*n);
            match result {
                Ok(n) => {
                    if -4611686018427387904 <= n && n <= 4611686018427387903 {
                        Expr::Number(n)
                    } else {
                        panic!("Invalid") // exceed 63, still static fault 
                    }
                }
                Err(_) => panic!("Invalid"),// exceed i64
            }
        }
        // match boolean
        Sexp::Atom(S(s)) if s == "true" => Expr::Boolean(true),
        Sexp::Atom(S(s)) if s == "false" => Expr::Boolean(false),
        // match variable
        Sexp::Atom(S(s)) => Expr::Id(s.to_string()),

        Sexp::List(vec) => {
            // println!("{:?}", vec);
            match &vec[..] {
                [Sexp::Atom(S(op)), e] if op == "add1" => {
                    Expr::UnOp(Op1::Add1, Box::new(parse_expr(e)))
                }
                [Sexp::Atom(S(op)), e] if op == "sub1" => {
                    Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e)))
                }
                [Sexp::Atom(S(op)), e] if op == "isnum" => {
                    Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e)))
                }
                [Sexp::Atom(S(op)), e] if op == "isbool" => {
                    Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e)))
                }
                [Sexp::Atom(S(op)), e] if op == "loop" => Expr::Loop(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "break" => Expr::Break(Box::new(parse_expr(e))),

                [Sexp::Atom(S(op)), e1, e2] if op == "+" => Expr::BinOp(
                    Op2::Plus,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == "-" => Expr::BinOp(
                    Op2::Minus,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == "*" => Expr::BinOp(
                    Op2::Times,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == ">" => Expr::BinOp(
                    Op2::Greater,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == "<" => Expr::BinOp(
                    Op2::Less,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == ">=" => Expr::BinOp(
                    Op2::GreaterEqual,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == "<=" => Expr::BinOp(
                    Op2::LessEqual,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == "=" => Expr::BinOp(
                    Op2::Equal,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                [Sexp::Atom(S(op)), lst, e] if op == "let" => {
                    Expr::Let(parse_bind_all(lst), Box::new(parse_expr(e)))
                }
                [Sexp::Atom(S(op)), s, e] if op == "set!" => parse_set(s, e),
                [Sexp::Atom(S(op)), e1, e2, e3] if op == "if" => Expr::If(
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                    Box::new(parse_expr(e3)),
                ),
                [Sexp::Atom(S(op)), _lst, _e]
                    if op != "+"
                        && op != "-"
                        && op != "*"
                        && op != "let"
                        && op != "set!"
                        && op != "block" =>
                {
                    panic!("Invalid")
                } // invalid operator
                [Sexp::Atom(S(op)), _e]
                    if op != "sub1"
                        && op != "add1"
                        && op != "isnum"
                        && op != "isbool"
                        && op != "block" =>
                {
                    panic!("Invalid")
                } // invalid operator
                expr => {
                    // println!("{:?}", expr);
                    // TODO empty loop
                    if expr.len() == 0 {
                        panic!("Invalid");
                    }
                    match &expr[0] {
                        Sexp::Atom(S(op)) if op == "block" => Expr::Block(parse_block(&expr[1..])),
                        _ => panic!("Invalid"),
                    }
                }
            }
        }
        _ => panic!("Invalid"),
    }
}

fn parse_block(s: &[Sexp]) -> Vec<Expr> {
    let mut all_blocks = Vec::new();
    if s.len() == 0 {
        panic!("Invalid") // TODO empty block?
    }
    for (_i, item) in s.iter().enumerate() {
        let res = parse_expr(&item); // TODO should only allow parse 'set!' ? can there be expr other than set!?
        all_blocks.push(res);
    }
    all_blocks
}

fn parse_set(s: &Sexp, e: &Sexp) -> Expr {
    match s {
        Sexp::Atom(S(s)) => {
            let valid = check_var_name(s.to_string());
            if valid {
              let b = s != "let"
                  && s != "sub1"
                  && s != "add1"
                  && s != "loop"
                  && s != "break"
                  && s != "block"
                  && s != "set!"
                  && s != "if"
                  && s != "input"
                  && s != "true"
                  && s != "false"
                  && s != "isnum"
                  && s != "isbool"
                  && s != "isNum"
                  && s != "isBool";
              if b == false {
                  panic!("Invalid indentifier containing keyword") // invalid variable name
              }
                Expr::Set(s.to_string(), Box::new(parse_expr(e)))
            } else {
                panic!("Invalid") // invalid variable name
            }
        }
        _ => panic!("Invalid"),
    }
}

fn parse_bind_all(s: &Sexp) -> Vec<(String, Expr)> {
    let mut all_binds = Vec::new();
    match s {
        Sexp::List(vec) => {
            // println!("{:?}", vec);
            // no-binding error: let()
            if vec.len() == 0 {
                panic!("Invalid")
            }
            for (_i, item) in vec.iter().enumerate() {
                let res = parse_bind(&item);
                all_binds.push(res);
            }
        }
        _ => panic!("Invalid"),
    };
    all_binds
}

fn parse_bind(s: &Sexp) -> (String, Expr) {
    match s {
        // List([Atom("x"), Atom("5")])
        Sexp::List(inner) => {
            match &inner[..] {
                [Sexp::Atom(S(s)), e] => {
                    let valid = check_var_name(s.to_string());
                    if valid {
                        let b = s != "let"
                            && s != "sub1"
                            && s != "add1"
                            && s != "loop"
                            && s != "break"
                            && s != "block"
                            && s != "set!"
                            && s != "if"
                            && s != "input"
                            && s != "true"
                            && s != "false"
                            && s != "isnum"
                            && s != "isbool"
                            && s != "isNum"
                            && s != "isBool";
                        if b == false {
                            panic!("Invalid indentifier containing keyword") // invalid variable name
                        }
                        (s.to_string(), parse_expr(e))
                    } else {
                        panic!("Invalid") // invalid variable name
                    }
                }
                _ => panic!("Invalid"),
            }
        }
        _ => panic!("Invalid"),
    }
}

fn check_var_name(s: String) -> bool {
    // match the variable name
    let re = Regex::new(r"[a-zA-z][a-zA-Z0-9]*").unwrap();
    return re.is_match(&s);
}

fn compile(
    e: &Expr,
    si: i32,
    env: &HashMap<String, i32>,
    labels: &mut i32,
    endloop: &String,
) -> String {
    match e {
        Expr::Number(n) => format!("mov rax, {}", *n << 1),
        Expr::Boolean(true) => format!("mov rax, {}", 3),
        Expr::Boolean(false) => format!("mov rax, {}", 1),
        Expr::Id(s) if s == "input" => {
            format!("mov rax, rdi")
        }
        Expr::Id(s) => {
            let res = env.get(s);
            match res {
                Some(offset) => format!("mov rax, [rsp - {}]", offset),
                None => panic!("Unbound variable identifier {s}"),
            }
        }
        Expr::Let(bind, body) => {
            let mut res = Vec::new();
            let mut nsi = si;
            // ****************************
            // NOTICE
            // duplicated binding only happens in one `let` layer, do not interfere with other layers
            // ****************************
            let mut checkenv: HashMap<String, i32> = hashmap! {}; // check duplicated binding
            let mut nenv = env.clone(); // make env live outside the for scope
            for (_i, item) in bind.iter().enumerate() {
                let inst = compile(&item.1, nsi, &nenv, labels, endloop);
                let offset = 8 * nsi;
                // check duplicated binding, only for this level
                let result = checkenv.get(&item.0.to_string());
                match result {
                    None => {}
                    Some(_s) => panic!("Duplicate binding"),
                };
                nenv = nenv.update(item.0.to_string(), offset);
                checkenv = checkenv.update(item.0.to_string(), offset);
                res.push(inst);
                res.push(format!("mov [rsp-{offset}], rax"));
                nsi = nsi + 1;
            }
            // println!("{:?}", nenv);
            // println!("{:?}", res);
            let binst = compile(body, nsi, &nenv, labels, endloop);
            // let offset = (nsi -1 ) * 8;
            let res_str = res.join("\n");
            format!(
                "
                {res_str}
                {binst}
                ")
        }

        Expr::UnOp(Op1::Add1, sube) => {
            let res = compile(sube, si, env, labels, endloop);
            format!(
                "
                  {res}
                  mov rbx, rax
                  xor rbx, 2
                  test rbx, 1
                  jne throw_error
                  add rax, 2
                  jo throw_error2
                  "
            )
        }
        Expr::UnOp(Op1::Sub1, sube) => {
            let res = compile(sube, si, env, labels, endloop);
            format!(
                "
                  {res}
                  mov rbx, rax
                  xor rbx, 2
                  test rbx, 1
                  jne throw_error
                  sub rax, 2
                  jo throw_error2
                  "
            )
        }
        // test rax, 1 will set Z to true if and only if the LSB is 0
        Expr::UnOp(Op1::IsNum, sube) => {
            let res = compile(sube, si, env, labels, endloop);
            let end_label = new_label(labels, "ifend");
            let else_label = new_label(labels, "ifelse");
            format!(
                "
                  {res}
                  test rax, 1
                  jne {else_label}
                  mov rax, 3
                  jmp {end_label}
                  {else_label}:
                  mov rax, 1
                  {end_label}:
                  "
            )
        }

        Expr::UnOp(Op1::IsBool, sube) => {
            let res = compile(sube, si, env, labels, endloop);
            let end_label = new_label(labels, "ifend");
            let else_label = new_label(labels, "ifelse");
            format!(
                "
                  {res}
                  test rax, 1
                  jne {else_label}
                  mov rax, 1
                  jmp {end_label}
                  {else_label}:
                  mov rax, 3
                  {end_label}:
                  "
            )
        }
        Expr::BinOp(Op2::Plus, sube1, sube2) => {
            let e1 = compile(sube1, si, env, labels, endloop);
            let e2 = compile(sube2, si + 1, env, labels, endloop);
            let offset = si * 8;
            // TODO should check both operand are numbers
            format!(
                "
                  {e1}
                  mov rbx, rax
                  xor rbx, 2
                  test rbx, 1
                  jne throw_error
                  jo throw_error2
                  mov [rsp - {offset}], rax
                  {e2}
                  mov rbx, rax
                  xor rbx, 2
                  test rbx, 1
                  jne throw_error
                  jo throw_error2
                  mov rbx, rax
                  xor rbx, [rsp - {offset}]
                  test rbx, 1
                  jne throw_error
                  jo throw_error2
                  add rax, [rsp - {offset}]
                  jo throw_error2"
            )
        }
        Expr::BinOp(Op2::Times, sube1, sube2) => {
            let e1 = compile(sube1, si, env, labels, endloop);
            let e2 = compile(sube2, si + 1, env, labels, endloop);
            let offset = si * 8;
            format!(
                "
                {e1}
                mov rbx, rax
                xor rbx, 2
                test rbx, 1
                jne throw_error
                jo throw_error2
                mov [rsp - {offset}], rax
                {e2}
                mov rbx, rax
                xor rbx, 2
                test rbx, 1
                jne throw_error
                jo throw_error2
                mov rbx, rax
                xor rbx, [rsp - {offset}]
                test rbx, 1
                jne throw_error
                jo throw_error2
                sar rax, 1
                imul rax, [rsp - {offset}]
                jo throw_error2
              "
            )
        }
        Expr::BinOp(Op2::Minus, sube1, sube2) => {
            let e1 = compile(sube1, si + 1, env, labels, endloop);
            let e2 = compile(sube2, si, env, labels, endloop);
            let offset = si * 8;
            // ************* 
            // NOTICE
            // in the middle of arithmetic, should check the number (check every step)! 
            // ***************** 
            format!(
                "
                  {e2}
                  mov rbx, rax
                  xor rbx, 2
                  test rbx, 1
                  jne throw_error
                  jo throw_error2
                  mov [rsp - {offset}], rax
                  {e1}
                  mov rbx, rax
                  xor rbx, 2
                  test rbx, 1
                  jo throw_error2
                  jne throw_error
                  mov rbx, rax
                  xor rbx, [rsp - {offset}]
                  test rbx, 1
                  jne throw_error
                  jo throw_error2
                  sub rax, [rsp - {offset}]
                  jo throw_error2
                "
            )
        }
        Expr::BinOp(Op2::Equal, sube1, sube2) => {
            let e1_instrs = compile(sube1, si, env, labels, endloop);
            let e2_instrs = compile(sube2, si + 1, env, labels, endloop);
            let offset = si * 8;
            format!(
                "
                {e1_instrs}
                mov [rsp - {offset}], rax
                {e2_instrs}
                mov rbx, rax
                xor rbx, [rsp - {offset}]
                test rbx, 1
                jne throw_error
                cmp rax, [rsp - {offset}]
                mov rbx, 3
                mov rax, 1
                cmove rax, rbx
                "
            )
        }

        Expr::BinOp(Op2::Greater, sube1, sube2) => {
            let e1_instrs = compile(sube1, si + 1, env, labels, endloop);
            let e2_instrs = compile(sube2, si, env, labels, endloop);
            let offset = si * 8;
            let end_label = new_label(labels, "ifend");
            let else_label = new_label(labels, "ifelse");
            format!(
                "
                  {e2_instrs}
                  mov rbx, rax
                  xor rbx, 2
                  test rbx, 1
                  jne throw_error
                  mov [rsp - {offset}], rax
                  {e1_instrs}
                  mov rbx, rax
                  xor rbx, 2
                  test rbx, 1
                  jne throw_error
                  mov rbx, rax
                  xor rbx, [rsp - {offset}]
                  test rbx, 1
                  jne throw_error
                  cmp rax, [rsp - {offset}]
                  jg {else_label}
                  mov rax, 1
                  jmp {end_label}
                  {else_label}:
                  mov rax, 3
                  {end_label}:
                  "
            )
        }
        Expr::BinOp(Op2::GreaterEqual, sube1, sube2) => {
            let e1_instrs = compile(sube1, si + 1, env, labels, endloop);
            let e2_instrs = compile(sube2, si, env, labels, endloop);
            let offset = si * 8;
            let end_label = new_label(labels, "ifend");
            let else_label = new_label(labels, "ifelse");
            format!(
                "
                {e2_instrs}
                mov rbx, rax
                xor rbx, 2
                test rbx, 1
                jne throw_error
                mov [rsp - {offset}], rax
                {e1_instrs}
                mov rbx, rax
                xor rbx, 2
                test rbx, 1
                jne throw_error
                mov rbx, rax
                xor rbx, [rsp - {offset}]
                test rbx, 1
                jne throw_error
                cmp rax, [rsp - {offset}]
                jge {else_label}
                mov rax, 1
                jmp {end_label}
                {else_label}:
                mov rax, 3
                {end_label}:
                "
            )
        }
        Expr::BinOp(Op2::Less, sube1, sube2) => {
            let e1_instrs = compile(sube1, si + 1, env, labels, endloop);
            let e2_instrs = compile(sube2, si, env, labels, endloop);
            let offset = si * 8;
            let end_label = new_label(labels, "ifend");
            let else_label = new_label(labels, "ifelse");
            format!(
                "
                  {e2_instrs}
                  mov rbx, rax
                  xor rbx, 2
                  test rbx, 1
                  jne throw_error
                  mov [rsp - {offset}], rax
                  {e1_instrs}
                  mov rbx, rax
                  xor rbx, 2
                  test rbx, 1
                  jne throw_error
                  mov rbx, rax
                  xor rbx, [rsp - {offset}]
                  test rbx, 1
                  jne throw_error
                  cmp rax, [rsp - {offset}]
                  jl {else_label}
                  mov rax, 1
                  jmp {end_label}
                  {else_label}:
                  mov rax, 3
                  {end_label}:
                  "
            )
        }
        Expr::BinOp(Op2::LessEqual, sube1, sube2) => {
            let e1_instrs = compile(sube1, si + 1, env, labels, endloop);
            let e2_instrs = compile(sube2, si, env, labels, endloop);
            let offset = si * 8;
            let end_label = new_label(labels, "ifend");
            let else_label = new_label(labels, "ifelse");
            format!(
                "
              {e2_instrs}
              mov rbx, rax
              xor rbx, 2
              test rbx, 1
              jne throw_error
              mov [rsp - {offset}], rax
              {e1_instrs}
              mov rbx, rax
              xor rbx, 2
              test rbx, 1
              jne throw_error
              mov rbx, rax
              xor rbx, [rsp - {offset}]
              test rbx, 1
              jne throw_error
              cmp rax, [rsp - {offset}]
              jle {else_label}
              mov rax, 1
              jmp {end_label}
              {else_label}:
              mov rax, 3
              {end_label}:
              "
            )
        }

        Expr::If(cond, thn, els) => {
            let end_label = new_label(labels, "ifend");
            let else_label = new_label(labels, "ifelse");
            let cond_instrs = compile(cond, si, env, labels, endloop);
            let thn_instrs = compile(thn, si, env, labels, endloop);
            let els_instrs = compile(els, si, env, labels, endloop);
            // to the second expression if any other value (including numbers).
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

        // TODO check if the loop has block and break, if no block and no break, should error?
        // only block or break is fine?
        Expr::Loop(e) => {
            let start_label = new_label(labels, "loop");
            let end_label = new_label(labels, "endloop");
            let e = compile(e, si, env, labels, &end_label);
            format!(
                "
                jmp {start_label}
                {start_label}:
                {e}
                jmp {start_label}
                {end_label}:
                "
            )
        }
        Expr::Break(e) => {
            // endloop == "" means no loop outside
            if endloop == "" {
                panic!("Invalid break")
            }
            let e = compile(e, si, env, labels, endloop);
            format!(
                "
                  {e}
                  jmp {endloop}
                  "
            )
            // need to jmp {end_label}
        }
        Expr::Set(v, e) => {
            let e = compile(e, si, env, labels, endloop);
            let res = env.get(v);
            match res {
                Some(offset) => format!(
                    "
                    {e}
                    mov [rsp - {offset}], rax"
                ),
                None => panic!("Unbound variable identifier {v}"),
            }
        }
        Expr::Block(seq) => {
            let mut res = Vec::new();
            for e in seq.iter() {
                let i = compile(&e, si, env, labels, endloop);
                res.push(i);
            }
            let res_str = res.join("");
            // evaluate to the result of the last expression.
            res_str
        }
    }
}

fn new_label(l: &mut i32, s: &str) -> String {
    let current = *l;
    *l += 1;
    format!("{s}_{current}")
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let result = &parse(&in_contents);
    match result {
        Ok(_s) => {}
        Err(_) => panic!("Invalid"),
    }
    let expr = parse_expr(&parse(&in_contents).unwrap());

    // println!("{:?}", expr);
    let mut labels = 0;
    let endloop: String = "".to_string();
    let result = compile(&expr, 2, &HashMap::new(), &mut labels, &endloop);
    let asm_program = format!(
        "
        section .text
        global our_code_starts_here
        extern snek_error
        extern snek_error2
        throw_error:
          mov rdi, 7 
          push rsp
          call snek_error 
        throw_error2:
          push rsp
          call snek_error2 
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
