use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::Atom::*;
use sexp::*;

use dynasmrt::{dynasm, DynasmApi};

//use im::HashMap;

/*#[derive(Debug)]
enum Val {
    Reg(Reg),
    Imm(i32),
    RegOffset(Reg, i32),
}

#[derive(Debug)]
enum Reg {
    RAX,
    RSP,
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
}*/

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
    Eq,
    LessThan,
    GreaterThan,
    Geq,
    Leq,
}

#[derive(Debug)]
enum Expr {
    Number(i64),
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    Def(String, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Set(String, Box<Expr>),
    Block(Vec<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
    Input,
}

fn labelgen(c: &mut i64, label: String) -> String
{
    let out = label + &(*c).to_string();
    *c += 1;
    return out;
}

fn letvec_parse(v: Vec<Sexp>) -> Vec<(String, Expr)> {
    let mut outvec = Vec::new();
    if v.len() == 0 { panic!("parse error: Invalid expression"); }
    for i in 0..(v.len()) {
        match &v[i] {
            Sexp::List(vec) => {
                match &vec[..] {
                    [Sexp::Atom(S(id)), expr] => outvec.push((String::from(id), parse_expr(expr))),
                    _ => panic!("parse error: Invalid expression")
                }
            }
            _ => 
            {
                panic!("parse error: Invalid expression: {}", v[i].to_string());
            }
        }
    }
    return outvec;
}

fn parse_dyn(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => {
            let v = i64::try_from(*n);
            if v.is_ok() { Expr::Number(v.unwrap()) }
            else { panic!("parse error: Invalid expression") }
        }
        Sexp::Atom(S(id)) => {
            let r_keyw = vec!["add1", "sub1", "let", "define", "if", "set", "loop", "break"];
            if r_keyw.contains(&(id.as_str())) { panic!("parse error: Invalid expression") }
            if id == "true"
              { Expr::Number(1) }
            else if id == "false"
              { Expr::Number(3) }
            else if id == "input"
              { Expr::Input }
            else { Expr::Id(id.to_string()) }
        },
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(op)), e] if op == "add1" => Expr::UnOp(Op1::Add1, Box::new(parse_dyn(e))),
                [Sexp::Atom(S(op)), e] if op == "sub1" => Expr::UnOp(Op1::Sub1, Box::new(parse_dyn(e))),
                [Sexp::Atom(S(op)), e1, e2] if op == "+" => Expr::BinOp(Op2::Plus, Box::new(parse_dyn(e1)), Box::new(parse_dyn(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "-" => Expr::BinOp(Op2::Minus, Box::new(parse_dyn(e1)), Box::new(parse_dyn(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "*" => Expr::BinOp(Op2::Times, Box::new(parse_dyn(e1)), Box::new(parse_dyn(e2))),
                [Sexp::Atom(S(op)), Sexp::List(letvec), e] if op == "let" => Expr::Let(letvec_parse((letvec).clone()), Box::new(parse_dyn(e))),
                [Sexp::Atom(S(op)), Sexp::Atom(S(id)), e] if op == "define" => Expr::Def(id.to_string(), Box::new(parse_dyn(e))),
                _ => panic!("parse error: Invalid expression")
            }
        },
        _ => panic!("parse error: Invalid expression")
    }
}

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => {
            let v = i64::try_from(*n);
            if v.is_ok() && (v.unwrap() < ((1 << 62) - 1)) && (v.unwrap() > -(1 << 62)) 
              { Expr::Number(v.unwrap() << 1) }
            else { panic!("parse error: Invalid expression") }
        },
        Sexp::Atom(S(id)) => {
          let r_keyw = vec!["add1", "sub1", "let", "define", "if", "set!", "loop", "break", "block"];
          if r_keyw.contains(&(id.as_str())) { panic!("parse error: keyword shadowing") }
          if id == "true"
            { Expr::Number(1) }
          else if id == "false"
            { Expr::Number(3) }
          else if id == "input"
            { Expr::Input }
          else { Expr::Id(id.to_string()) }
        },
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(op)), e] if op == "add1" => Expr::UnOp(Op1::Add1, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "sub1" => Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "isnum" => Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "isbool" => Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => Expr::Block(exprs.into_iter().map(parse_expr).collect()),
                [Sexp::Atom(S(op)), e] if op == "loop" => Expr::Loop(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "break" => Expr::Break(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e1, e2] if op == "+" => Expr::BinOp(Op2::Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "-" => Expr::BinOp(Op2::Minus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "*" => Expr::BinOp(Op2::Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "=" => Expr::BinOp(Op2::Eq, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "<" => Expr::BinOp(Op2::LessThan, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == ">" => Expr::BinOp(Op2::GreaterThan, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "<=" => Expr::BinOp(Op2::Leq, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == ">=" => Expr::BinOp(Op2::Geq, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), Sexp::List(letvec), e] if op == "let" => Expr::Let(letvec_parse((letvec).clone()), Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), cond, then, els] if op == "if" => Expr::If(Box::new(parse_expr(cond)), Box::new(parse_expr(then)), Box::new(parse_expr(els))),
                [Sexp::Atom(S(op)), Sexp::Atom(S(id)), expr] if op == "set!" => Expr::Set(id.to_string(), Box::new(parse_expr(expr))),
                _ => panic!("parse error: Invalid expression1111")
            }
        },
        _ => panic!("parse error: Invalid expression")
    }
}

fn compile_dyn(e: &Expr, si: i64, env: im::HashMap<String, i64>, denv: &std::collections::HashMap<String, i64>,
    ops: &mut dynasmrt::x64::Assembler)
{
    match e {
        Expr::Number(n) => dynasm!(ops; .arch x64; mov rax, (*n).try_into().unwrap()),
        Expr::UnOp(Op1::Add1, subexpr) =>  { 
            compile_dyn(subexpr, si, env.clone(), denv, ops);
            dynasm!(ops; .arch x64; add rax, 1);
        },
        Expr::UnOp(Op1::Sub1, subexpr) =>  { 
            compile_dyn(subexpr, si, env.clone(), denv, ops);
            dynasm!(ops; .arch x64; sub rax, 1);
        },
        Expr::BinOp(Op2::Plus, se1, se2) => {
            let offset = si * 8;
            compile_dyn(se1, si, env.clone(), denv, ops);
            dynasm!(ops; .arch x64; mov [rsp - {offset.try_into().unwrap()}], rax);
            compile_dyn(se2, si + 1, env.clone(), denv, ops);
            dynasm!(ops; .arch x64; add rax, [rsp - {offset.try_into().unwrap()}]);
        },
        Expr::BinOp(Op2::Minus, se1, se2) => {
            let offset = si * 8;
            compile_dyn(se2, si, env.clone(), denv, ops);
            dynasm!(ops; .arch x64; mov [rsp - {offset.try_into().unwrap()}], rax);
            compile_dyn(se1, si + 1, env.clone(), denv, ops);
            dynasm!(ops; .arch x64; sub rax, [rsp - {offset.try_into().unwrap()}]);
        },
        Expr::BinOp(Op2::Times, se1, se2) => {
            let offset = si * 8;
            compile_dyn(se1, si, env.clone(), denv, ops);
            dynasm!(ops; .arch x64; mov [rsp - {offset.try_into().unwrap()}], rax);
            compile_dyn(se2, si + 1, env.clone(), denv, ops);
            dynasm!(ops; .arch x64; imul rax, [rsp - {offset.try_into().unwrap()}]);
        },
        Expr::Let(vars, body) => {
            //list of all bound and free variables
            //env contains all free vars
            let mut tenv = env.clone();
            //offset in which current variable's value is stored
            //apply and assign binding expr for each var assigned
            let mut si_offset = 0;
            for i in 0..(vars.len()) {
                //we have id x and binding expr e
                //x
                let id = &vars[i].0;
                //e
                let id_def = &vars[i].1;
                //cannot modify heap variable using let
                if denv.contains_key(id) { panic!("Use define to modify persistent variables!"); }
                //if x is already a bound variable (and not a free one), duplicate binding
                if tenv.contains_key(id) && !env.contains_key(id) { panic!("Duplicate binding"); }
                let offset;
                //store newly defined variable or update free var
                if env.contains_key(id)
                {
                    //updating existing var
                    match env.get(id)
                    {
                        Some(off) => offset = *off,
                        None => panic!("Variable not allocated properly")
                    }
                }
                else
                {
                    //defining new var
                    offset = (si + si_offset) * 8;
                    tenv = tenv.update(id.to_string(), offset);
                    si_offset += 1;
                }
                compile_dyn(&id_def, si + si_offset, tenv.clone(), denv, ops);
                dynasm!(ops; .arch x64; mov [rsp - {offset.try_into().unwrap()}], rax);
            }
            //evaluate body expr using new environment
            compile_dyn(body, si + si_offset, tenv.clone(), denv, ops);
        },
        Expr::Id(s) => {
            if !(env.contains_key(s as &str)) && !(denv.contains_key(s as &str)) { panic!("Unbound variable identifier {}", s); }
            if env.contains_key(s as &str)
            {
                let pos = &env[s as &str];
                dynasm!(ops; .arch x64; mov rax, [rsp - {(*pos).try_into().unwrap()}]);
            }
            else
            {
                let val = &denv[s as &str];
                dynasm!(ops; .arch x64; mov rax, (*val).try_into().unwrap());
            }
        }
        _ => ()
    }
}

fn compile_expr(e: &Expr, si: i64, env: im::HashMap<String, i64>, c: &mut i64, bp: &String) -> String {
    match e {
        Expr::Input => format!("mov rax, rdi"),
        Expr::Number(n) => format!("mov rax, {}\n", *n),
        Expr::UnOp(Op1::Add1, subexpr) => {
          let se_instrs = compile_expr(subexpr, si, env.clone(), c, bp);
          let contbranch = labelgen(c, "cont".to_string());
          format!("
            {se_instrs}
            mov rcx, rax
            and rcx, 1
            cmp rcx, 1
            jne {contbranch}
            mov rdi, 1
            jmp throw_error
            {contbranch}:
            add rax, 2
            jo overflow
          ")
        },
        Expr::UnOp(Op1::Sub1, subexpr) => {
          let se_instrs = compile_expr(subexpr, si, env.clone(), c, bp);
          let contbranch = labelgen(c, "cont".to_string());
          format!("
            {se_instrs}
            mov rcx, rax
            and rcx, 1
            cmp rcx, 1
            jne {contbranch}
            mov rdi, 1
            jmp throw_error
            {contbranch}:
            sub rax, 2
            jo overflow
          ")
        },
        Expr::UnOp(Op1::IsNum, subexpr) => {
          let se_instrs = compile_expr(subexpr, si, env.clone(), c, bp);
          let elsebranch = labelgen(c, "else".to_string());
          let contbranch = labelgen(c, "cont".to_string());
          format!("
            {se_instrs}
            mov rcx, rax
            and rcx, 1
            cmp rcx, 0
            jne {elsebranch}
            mov rax, 1
            jmp {contbranch}
            {elsebranch}:
              mov rax, 3
            {contbranch}:
          ")
        }
        Expr::UnOp(Op1::IsBool, subexpr) => {
          let se_instrs = compile_expr(subexpr, si, env.clone(), c, bp);
          let elsebranch = labelgen(c, "else".to_string());
          let contbranch = labelgen(c, "cont".to_string());
          format!("
            {se_instrs}
            mov rcx, rax
            and rcx, 1
            cmp rcx, 1
            jne {elsebranch}
            mov rax, 1
            jmp {contbranch}
            {elsebranch}:
              mov rax, 3
            {contbranch}:
          ")
        }
        Expr::BinOp(Op2::Plus, se1, se2) => {
            let se1_instrs = compile_expr(se1, si, env.clone(), c, bp);
            let se2_instrs = compile_expr(se2, si + 1, env.clone(), c, bp);
            let contbranch1 = labelgen(c, "cont".to_string());
            let contbranch2 = labelgen(c, "cont".to_string());
            let offset = si * 8;
            format!("
                {se1_instrs}
                mov rcx, rax
                and rcx, 1
                cmp rcx, 1
                jne {contbranch1}
                mov rdi, 1
                jmp throw_error
                {contbranch1}:
                mov [rsp - {offset}], rax
                {se2_instrs}
                mov rcx, rax
                and rcx, 1
                cmp rcx, 1
                jne {contbranch2}
                mov rdi, 1
                jmp throw_error
                {contbranch2}:
                add rax, [rsp - {offset}]  
                jo overflow         
            ")
        },
        Expr::BinOp(Op2::Minus, se1, se2) => {
            let se2_instrs = compile_expr(se2, si, env.clone(), c, bp);
            let se1_instrs = compile_expr(se1, si + 1, env.clone(), c, bp);
            let contbranch1 = labelgen(c, "cont".to_string());
            let contbranch2 = labelgen(c, "cont".to_string());
            let offset = si * 8;
            format!("
                {se2_instrs}
                mov rcx, rax
                and rcx, 1
                cmp rcx, 1
                jne {contbranch1}
                mov rdi, 1
                jmp throw_error
                {contbranch1}:
                mov [rsp - {offset}], rax
                {se1_instrs}
                mov rcx, rax
                and rcx, 1
                cmp rcx, 1
                jne {contbranch2}
                mov rdi, 1
                jmp throw_error
                {contbranch2}:
                sub rax, [rsp - {offset}]
                jo overflow         
            ")
        },
        Expr::BinOp(Op2::Times, se1, se2) => {
          let se1_instrs = compile_expr(se1, si, env.clone(), c, bp);
          let se2_instrs = compile_expr(se2, si + 1, env.clone(), c, bp);
          let contbranch1 = labelgen(c, "cont".to_string());
          let contbranch2 = labelgen(c, "cont".to_string());
          let offset = si * 8;
          format!("
              {se1_instrs}
              mov rcx, rax
              and rcx, 1
              cmp rcx, 1
              jne {contbranch1}
              mov rdi, 1
              jmp throw_error
              {contbranch1}:
              mov [rsp - {offset}], rax
              {se2_instrs}
              mov rcx, rax
              and rcx, 1
              cmp rcx, 1
              jne {contbranch2}
              mov rdi, 1
              jmp throw_error
              {contbranch2}:
              shr rax, 1
              imul rax, [rsp - {offset}]
              jo overflow           
          ")
        },
        Expr::BinOp(Op2::Eq, e1, e2) => {
            let e1_instrs = compile_expr(e1, si, env.clone(), c, bp);
            let e2_instrs = compile_expr(e2, si + 1, env.clone(), c, bp);
            let elname = labelgen(c, "else".to_string());
            let contname = labelgen(c, "cont".to_string());
            let errcont = labelgen(c, "errcont".to_string());
            let offset = si * 8;
            format!("
                {e1_instrs}
                mov [rsp - {offset}], rax
                {e2_instrs}
                mov rcx, rax
                xor rcx, [rsp - {offset}]
                and rcx, 1
                cmp rcx, 1
                jne {errcont}
                mov rdi, 1
                jmp throw_error
                {errcont}:
                xor rax, [rsp - {offset}]
                cmp rax, 0
                jne {elname}
                mov rax, 1
                jmp {contname}
                {elname}:
                    mov rax, 3
                {contname}:
            ")
        },
        Expr::BinOp(Op2::LessThan, e1, e2) => {
            let e1_instrs = compile_expr(e1, si, env.clone(), c, bp);
            let e2_instrs = compile_expr(e2, si + 1, env.clone(), c, bp);
            let contname = labelgen(c, "cont".to_string());
            let errcont = labelgen(c, "errcont".to_string());
            let throwbranch = labelgen(c, "error".to_string());
            let offset = si * 8;
            format!("
                {e1_instrs}
                mov rcx, rax
                and rcx, 1
                cmp rcx, 0
                jne {throwbranch}
                mov [rsp - {offset}], rax
                {e2_instrs}
                mov rcx, rax
                and rcx, 1
                cmp rcx, 0
                jne {throwbranch}
                cmp [rsp - {offset}], rax
                jge {errcont}
                mov rax, 1
                jmp {contname}
                {errcont}:
                  mov rax, 3
                  jmp {contname}
                {throwbranch}:
                  mov rdi, 1
                  jmp throw_error
                {contname}:
            ")
        },
        Expr::BinOp(Op2::Leq, e1, e2) => {
          let e1_instrs = compile_expr(e1, si, env.clone(), c, bp);
          let e2_instrs = compile_expr(e2, si + 1, env.clone(), c, bp);
          let contname = labelgen(c, "cont".to_string());
          let errcont = labelgen(c, "errcont".to_string());
          let throwbranch = labelgen(c, "error".to_string());
          let offset = si * 8;
          format!("
              {e1_instrs}
              mov rcx, rax
              and rcx, 1
              cmp rcx, 0
              jne {throwbranch}
              mov [rsp - {offset}], rax
              {e2_instrs}
              mov rcx, rax
              and rcx, 1
              cmp rcx, 0
              jne {throwbranch}
              cmp [rsp - {offset}], rax
              jg {errcont}
              mov rax, 1
              jmp {contname}
              {errcont}:
                mov rax, 3
                jmp {contname}
              {throwbranch}:
                mov rdi, 1
                jmp throw_error
              {contname}:
          ")
        },
        Expr::BinOp(Op2::Geq, e1, e2) => {
          let e1_instrs = compile_expr(e1, si, env.clone(), c, bp);
          let e2_instrs = compile_expr(e2, si + 1, env.clone(), c, bp);
          let contname = labelgen(c, "cont".to_string());
          let errcont = labelgen(c, "errcont".to_string());
          let throwbranch = labelgen(c, "error".to_string());
          let offset = si * 8;
          format!("
              {e1_instrs}
              mov rcx, rax
              and rcx, 1
              cmp rcx, 0
              jne {throwbranch}
              mov [rsp - {offset}], rax
              {e2_instrs}
              mov rcx, rax
              and rcx, 1
              cmp rcx, 0
              jne {throwbranch}
              cmp [rsp - {offset}], rax
              jl {errcont}
              mov rax, 1
              jmp {contname}
              {errcont}:
                mov rax, 3
                jmp {contname}
              {throwbranch}:
                mov rdi, 1
                jmp throw_error
              {contname}:
          ")
        },
        Expr::BinOp(Op2::GreaterThan, e1, e2) => {
          let e1_instrs = compile_expr(e1, si, env.clone(), c, bp);
          let e2_instrs = compile_expr(e2, si + 1, env.clone(), c, bp);
          let contname = labelgen(c, "cont".to_string());
          let errcont = labelgen(c, "errcont".to_string());
          let throwbranch = labelgen(c, "error".to_string());
          let offset = si * 8;
          format!("
              {e1_instrs}
              mov rcx, rax
              and rcx, 1
              cmp rcx, 0
              jne {throwbranch}
              mov [rsp - {offset}], rax
              {e2_instrs}
              mov rcx, rax
              and rcx, 1
              cmp rcx, 0
              jne {throwbranch}
              cmp [rsp - {offset}], rax
              jle {errcont}
              mov rax, 1
              jmp {contname}
              {errcont}:
                mov rax, 3
                jmp {contname}
              {throwbranch}:
                mov rdi, 1
                jmp throw_error
              {contname}:
          ")
        },
        Expr::Let(vars, body) => {
            //binding expr instructions for each variable bound in vars
            let mut e_instrs: String = "".to_string();
            //list of all bound and free variables
            //env contains all free vars
            let mut tenv = env.clone();
            //offset in which current variable's value is stored
            //apply and assign binding expr for each var assigned
            let mut si_offset = 0;
            for i in 0..(vars.len()) {
                //we have id x and binding expr e
                //x
                let id = &vars[i].0;
                //e
                let id_def = &vars[i].1;
                //if x is already a bound variable (and not a free one), duplicate binding
                if tenv.contains_key(id) && !env.contains_key(id) { panic!("Duplicate binding"); }
                let offset;
                //store newly defined variable or update free var
                if env.contains_key(id)
                {
                    //updating existing var
                    match env.get(id)
                    {
                        Some(off) => offset = *off,
                        None => panic!("Variable not allocated properly")
                    }
                }
                else
                {
                    //defining new var
                    offset = (si + si_offset) * 8;
                    tenv = tenv.update(id.to_string(), offset);
                    si_offset += 1;
                }
                e_instrs = e_instrs + &compile_expr(&id_def, si + si_offset, tenv.clone(), c, bp) 
                + &format!("
                mov [rsp - {offset}], rax
                ");
            }
            //evaluate body expr using new environment
            let b_instrs = compile_expr(body, si + si_offset, tenv.clone(), c, bp);
            format!("
                {e_instrs}
                {b_instrs}
            ")
        },
        Expr::Id(s) => {
            if !(env.contains_key(s as &str)) { panic!("Unbound variable identifier {}", s); }
            let pos = &env[s as &str];
            format!("
                mov rax, [rsp - {pos}]
            ")
        },
        Expr::If(cond, th, el) => {
            let c_instrs = compile_expr(cond, si, env.clone(), c, bp);
            let th_instrs = compile_expr(th, si, env.clone(), c, bp);
            let el_instrs = compile_expr(el, si, env.clone(), c, bp);
            let elname = labelgen(c, "else".to_string());
            let contname = labelgen(c, "cont".to_string());
            format!("
                {c_instrs}
                cmp rax, 3
                je {elname}
                {th_instrs}
                jmp {contname}
                {elname}:
                    {el_instrs}
                {contname}:
            ")
        },
        Expr::Loop(e) => {
            let loopname = labelgen(c, "loop".to_string());
            let breakname = labelgen(c, "break".to_string());
            let e_instrs = compile_expr(e, si, env.clone(), c, &breakname);
            format!("
                {loopname}:
                    {e_instrs}
                    jmp {loopname}
                {breakname}:
            ")
        }
        Expr::Break(e) => {
            if bp == "" { panic!("Error: break used outside of loop") }
            let e_instrs = compile_expr(e, si, env.clone(), c, bp);
            format!("
                {e_instrs}
                jmp {bp}
            ")
        }
        Expr::Set(id, e) => {
          if !(env.contains_key(id as &str)) { panic!("Unbound variable identifier {}", id); }
          let e_instrs = compile_expr(e, si, env.clone(), c, bp);
          let pos = &env[id as &str];
          format!("
            {e_instrs}
            mov [rsp - {pos}], rax
          ")
        }
        Expr::Block(exprs) => {
          if exprs.len() == 0 { panic!("parse error: Invalid expression"); }
          let mut outstr: String = "".to_string();
          for i in 0..exprs.len() {
            outstr = outstr + &compile_expr(&exprs[i], si, env.clone(), c, bp);
          }
          format!("
            {outstr}
          ")
        }
        _ => "mov rax, rax".to_string() //define doesn't work for compile_expr!
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() == 2 && args[1] == "-i"
    {
        //interactive mode
        //https://stackoverflow.com/questions/25632070/how-do-i-efficiently-read-in-a-single-line-of-user-input-from-the-console
        //https://stackoverflow.com/questions/54262976/how-do-i-print-stdout-and-get-stdin-on-the-same-line-in-rust
        let stdin = std::io::stdin();
        let mut buf = String::new();
        let mut denv = std::collections::HashMap::new();
        loop
        {
            buf.clear();
            //get user input
            print!("> ");
            let _ = std::io::stdout().flush();
            stdin.read_line(&mut buf)?;
            let res = match buf.trim_end() {
                "" => "Bad input".into(),
                s => s,
            };
            //break when directed to by the user
            if res == "exit"
            {
                break;
            }
            //get expression
            let mut expr = Expr::Number(0);
            let stree = parse(&res);
            //https://stackoverflow.com/questions/27384824/catching-panic-when-rust-called-from-c-ffi-without-spawning-threads
            if stree.is_err() {
                println!("parse error: Invald expression");
                continue;
            }
            let res2 = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                expr = parse_dyn(&(stree.unwrap()));
            }));
            if res2.is_err() {
                println!("Invalid expression! (parse failure)");
                continue;
            }
            //environment
            let env = im::hashmap!{};
            let mut ops = dynasmrt::x64::Assembler::new().unwrap();
            let start = ops.offset();
            match expr {
                //define command used?
                Expr::Def(x, e) => {
                    //get value of expr and write into the hashmap
                    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                        compile_dyn(&e, 2, env, &denv, &mut ops);
                    }));
                    if result.is_err() {
                        println!("Expression error! (compilation failure)");
                        continue;
                    }
                    dynasm!(ops
                        ; .arch x64
                        ; ret);
                    let buf = ops.finalize().unwrap();
                    let jitted_fn: extern "C" fn() -> i64 = unsafe { std::mem::transmute(buf.ptr(start)) };
                    let x_val = jitted_fn();
                    denv.insert(x, x_val);
                }
                //otherwise, just compile expr
                _ => {
                    //environment
                    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                        compile_dyn(&expr, 2, env, &denv, &mut ops);
                    }));
                    if result.is_err() {
                        println!("Expression error! (compilation failure)");
                        continue;
                    }
                    dynasm!(ops
                        ; .arch x64
                        ; ret);
                    let buf = ops.finalize().unwrap();
                    let jitted_fn: extern "C" fn() -> i64 = unsafe { std::mem::transmute(buf.ptr(start)) };
                    println!("{}", jitted_fn());
                }
            }
        }
    }
    else
    {
        //compile mode
        let in_name = &args[1];
        let out_name = &args[2];
        // You will make result hold the result of actually compiling
        let mut in_file = File::open(in_name)?;
        let mut in_contents = String::new();
        in_file.read_to_string(&mut in_contents)?;
        //parse the expression and check for errors
        let stree = parse(&in_contents);
        if stree.is_err() {
            panic!("parse error: Invalid expression");
        }
        let expr = parse_expr(&(stree.unwrap()));
        let env = im::hashmap!{};
        let mut c: i64 = 0;
        let bp: String = "".to_string();
        let result = compile_expr(&expr, 2, env, &mut c, &bp);

        let asm_program = format!(
            "
    section .text
    global our_code_starts_here
    extern snek_error
    overflow:
      mov rdi, 2
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
    }
    Ok(())
}
