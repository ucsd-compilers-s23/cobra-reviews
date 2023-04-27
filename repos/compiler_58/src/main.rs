use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::panic;

use sexp::Atom::*;
use sexp::*;

use im::HashMap;
use im::HashSet;

#[derive(Debug)]
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
}

#[derive(Debug)]
enum Op1 {
    Add1,
    Sub1,
    IsNum, 
    IsBool
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
    Block(Vec<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
    Set(String, Box<Expr>),
}

fn check_id_valid(s: &str) -> bool {
    match s {
        "add" => false,
        "add1" => false,
        "sub" => false,
        "sub1" => false,
        "*" => false,
        "+" => false,
        "-" => false,
        ">" => false,
        "<" => false,
        "=" => false,
        ">=" => false,
        "<=" => false,
        "let" => false,
        "false" => false,
        "true" => false,
        "set!" => false,
        "set" => false,
        "if" => false,
        "block" => false,
        "input" => false,
        "break" => false,
        "loop" => false,
        "isnum" => false,
        "isbool" => false,
        _ => true
    }
}

fn parse_bind(s: &Sexp) -> Vec<(String, Expr)> {
    //println!("parse_bind expr: {:?}", s);
    let mut ret = Vec::new();
    //"( () )"
    match s {
        Sexp::List(vec) => {
            let mut duplicationMap: im::HashSet<String> = HashSet::new();
            if vec.len() == 0 {
                panic!("Invalid parsing, expression should contain at least one binding.")
            }
            for bindVec in vec {
                //println!("{bindVec}");
                // ( "()" )
                match bindVec {
                    Sexp::List(vec2) => {
                        //"x 10"
                        match &vec2[..] {
                            [Sexp::Atom(S(id)), e] => {
                                if duplicationMap.contains(id) {
                                    panic!("Duplicate binding")
                                }
                                if check_id_valid(id) == false {
                                    panic!("Invalid id name which matches a keyword. Given: {id}")
                                }
                                duplicationMap.insert(id.to_string());
                                ret.push((id.to_owned(), (parse_expr(e))));
                            }
                            _ => panic!("Invalid parsing, unmatched <Id, Expr> pattern. Given: {bindVec}"),
                        }
                    }
                    _ => panic!("Invalid parsing, the elements inside binding should be a list. Given: {bindVec}"),
                }
            }
        },
        _ => panic!("Invalid parsing, the binding should be a list. Given: {s}"),
    }
    return ret;
}

fn parse_block(s: &Vec<Sexp>) -> Vec<Expr> {
  //println!("parse_block expr: {:?}", s);
  let mut ret = Vec::new();
  let mut block_idx = 0;
  if s.len() == 1 {
    panic!("Invalid parsing, block should contain at least one expression.")
  }
  for block in s {
    if block_idx != 0 {
      ret.push(parse_expr(block));
    }
    block_idx += 1;
  }
  return ret;
}


fn overflow_check(input: i64) -> bool {
  let typed_value = input << 1;
  let div_value = typed_value / 2;

  return div_value == input;
}

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(S(op)) if op == "true" => Expr::Boolean(true),
        Sexp::Atom(S(op)) if op == "false" => Expr::Boolean(false),
        Sexp::Atom(I(n)) => {
          let i64_val = i64::try_from(*n).unwrap();
          if overflow_check(i64_val) == false {
            eprintln!("Invalid");
            std::process::exit(1);
          }
          Expr::Number(i64::try_from(*n).unwrap() << 1)
        }
        Sexp::Atom(S(id)) => Expr::Id(id.to_owned()),
        Sexp::List(vec) => {
            if vec.len() >= 1 {
                let first_elem = &vec[0];
                match first_elem {
                  Sexp::Atom(S(op)) if op == "block" => {
                    //println!("parse_expr encounters block: {:?}", vec);
                    let block_expr = Expr::Block(parse_block(vec));
                    //println!("parse_expr encounters block result: {:?}", block_expr);
                    return Expr::Block(parse_block(vec));
                  }
                  _ => {}
                }
            }
            //println!("parse_expr normal: {:?}", vec);
            match &vec[..] {
                //[Sexp::Atom(S(op)), Sexp::List(body)] if op == "block" => ,
                [Sexp::Atom(S(op)), e] if op == "loop" => Expr::Loop(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "break" => Expr::Break(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), name, e] if op == "set!" => Expr::Set((*name).to_string(), Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), cond, t_e, f_e] if op == "if" => Expr::If(Box::new(parse_expr(cond)), Box::new(parse_expr(t_e)), Box::new(parse_expr(f_e))),
                [Sexp::Atom(S(op)), e] if op == "isnum" => Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "isbool" => Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "add1" => Expr::UnOp(Op1::Add1, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "sub1" => Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), l, r] if op == "+" => Expr::BinOp(Op2::Plus, Box::new(parse_expr(l)),Box::new(parse_expr(r))),
                [Sexp::Atom(S(op)), l, r] if op == "-" => Expr::BinOp(Op2::Minus, Box::new(parse_expr(l)),Box::new(parse_expr(r))),
                [Sexp::Atom(S(op)), l, r] if op == "*" => Expr::BinOp(Op2::Times, Box::new(parse_expr(l)), Box::new(parse_expr(r))),
                [Sexp::Atom(S(op)), l, r] if op == "=" => Expr::BinOp(Op2::Equal, Box::new(parse_expr(l)), Box::new(parse_expr(r))),
                [Sexp::Atom(S(op)), l, r] if op == ">" => Expr::BinOp(Op2::Greater, Box::new(parse_expr(l)), Box::new(parse_expr(r))),
                [Sexp::Atom(S(op)), l, r] if op == "<" => Expr::BinOp(Op2::Less, Box::new(parse_expr(l)), Box::new(parse_expr(r))),
                [Sexp::Atom(S(op)), l, r] if op == ">=" => Expr::BinOp(Op2::GreaterEqual, Box::new(parse_expr(l)), Box::new(parse_expr(r))),
                [Sexp::Atom(S(op)), l, r] if op == "<=" => Expr::BinOp(Op2::LessEqual, Box::new(parse_expr(l)), Box::new(parse_expr(r))),
                [Sexp::Atom(S(op)), binding, body] if op == "let" => Expr::Let(parse_bind(binding), Box::new(parse_expr(body))),
                _ => panic!("Invalid parsing, unsupported expression: {s}."),
            }
        },
        _ => panic!("Invalid parsing, invalid element type: {s}."),
    }
}

fn create_label(label_idx: &mut i32, label_name: &str) -> String {
  let current_idx = *label_idx;
  *label_idx += 1;
  format!("{label_name}_{current_idx}")
}

fn compile_expr(e: &Expr, si: i32, env: &HashMap<String, i32>, label_idx: &mut i32, loop_idx: i32) -> String {
    let stack_offset = si * 8;
    let ERR_INVALID_ARGU = 2;
    let ERR_OVERFLOW = 3;
    match e {
        Expr::Boolean(b) => {
          if *b == true {
            format!("mov rax, 3")
          } else {
            format!("mov rax, 1")
          }
        }
        Expr::Number(n) => format!("mov rax, {}", *n),
        Expr::UnOp(Op1::Add1, subexpr) => {
          let e_instrs = compile_expr(subexpr, si, env, label_idx, loop_idx);
          format!("
          {e_instrs}
          add rax, 2
          mov rdx, {ERR_OVERFLOW}
          jo throw_error
          mov rbx, rax
          and rbx, 1
          cmp rbx, 1
          mov rdx, {ERR_INVALID_ARGU}
          je throw_error
          ")
        }
        Expr::UnOp(Op1::Sub1, subexpr) => {
          let e_instrs = compile_expr(subexpr, si, env, label_idx, loop_idx);
          format!("
          {e_instrs}
          sub rax, 2
          mov rdx, {ERR_OVERFLOW}
          jo throw_error
          mov rbx, rax
          and rbx, 1
          cmp rbx, 1
          mov rdx, {ERR_INVALID_ARGU}
          je throw_error
          ")
        }
        Expr::UnOp(Op1::IsNum, expr) => {
          let e_instrs = compile_expr(expr, si, env, label_idx, loop_idx);
          format!("
            {e_instrs}
            and rax, 1
            mov rbx, 3
            mov rax, 1
            cmove rax, rbx
          ")
        }
        Expr::UnOp(Op1::IsBool, expr)  => {
          let e_instrs = compile_expr(expr, si, env, label_idx, loop_idx);
          format!("
            {e_instrs}
            and rax, 1
            mov rbx, 3
            mov rax, 1
            cmova rax, rbx
          ")
        }
        Expr::BinOp(Op2::Minus, subexpr1, subexpr2) => {
            let e1_instrs = compile_expr(subexpr1, si, env, label_idx, loop_idx);
            let e2_instrs = compile_expr(subexpr2, si + 1, env, label_idx, loop_idx);
            format!("
                {e1_instrs}
                mov [rsp - {stack_offset}], rax
                mov rbx, rax
                and rbx, 1
                cmp rbx, 1
                mov rdx, {ERR_INVALID_ARGU}
                je throw_error
                
                {e2_instrs}
                mov rbx, rax
                and rbx, 1
                cmp rbx, 1
                mov rdx, {ERR_INVALID_ARGU}
                je throw_error

                mov rbx, rax
                xor rbx, [rsp - {stack_offset}]
                test rbx, 1
                mov rdx, {ERR_INVALID_ARGU}
                jne throw_error
                sub rax, [rsp - {stack_offset}]
                mov rdx, {ERR_OVERFLOW}
                jo throw_error
                neg rax
                mov rbx, rax
                and rbx, 1
                cmp rbx, 1
                mov rdx, {ERR_INVALID_ARGU}
                je throw_error
            ")
        }
        Expr::BinOp(Op2::Plus, subexpr1, subexpr2) => {
            let e1_instrs = compile_expr(subexpr1, si, env, label_idx, loop_idx);
            let e2_instrs = compile_expr(subexpr2, si + 1, env, label_idx, loop_idx);
            format!("
                {e1_instrs}
                mov [rsp - {stack_offset}], rax
                mov rbx, rax
                and rbx, 1
                cmp rbx, 1
                mov rdx, {ERR_INVALID_ARGU}
                je throw_error

                {e2_instrs}
                mov rbx, rax
                and rbx, 1
                cmp rbx, 1
                mov rdx, {ERR_INVALID_ARGU}
                je throw_error

                mov rbx, rax
                xor rbx, [rsp - {stack_offset}]
                test rbx, 1
                mov rdx, {ERR_INVALID_ARGU}
                jne throw_error
                add rax, [rsp - {stack_offset}]
                mov rdx, {ERR_OVERFLOW}
                jo throw_error
                mov rbx, rax
                and rbx, 1
                cmp rbx, 1
                mov rdx, {ERR_INVALID_ARGU}
                je throw_error
            ")
        }
        Expr::BinOp(Op2::Times, subexpr1, subexpr2) => {
            let e1_instrs = compile_expr(subexpr1, si, env, label_idx, loop_idx);
            let e2_instrs = compile_expr(subexpr2, si + 1, env, label_idx, loop_idx);
            format!("
                {e1_instrs}
                mov [rsp - {stack_offset}], rax
                mov rbx, rax
                and rbx, 1
                cmp rbx, 1
                mov rdx, {ERR_INVALID_ARGU}
                je throw_error

                {e2_instrs}
                mov rbx, rax
                and rbx, 1
                cmp rbx, 1
                mov rdx, {ERR_INVALID_ARGU}
                je throw_error
                
                mov rbx, rax
                xor rbx, [rsp - {stack_offset}]
                test rbx, 1
                mov rdx, {ERR_INVALID_ARGU}
                jne throw_error
                imul rax, [rsp - {stack_offset}]
                mov rdx, {ERR_OVERFLOW}
                jo throw_error
                sar rax, 1
                mov rbx, rax
                and rbx, 1
                cmp rbx, 1
                mov rdx, {ERR_INVALID_ARGU}
                je throw_error
            ")
        }
        Expr::BinOp(Op2::Equal, subexpr1, subexpr2) => {
            let e1_instrs = compile_expr(subexpr1, si, env, label_idx, loop_idx);
            let e2_instrs = compile_expr(subexpr2, si + 1, env, label_idx, loop_idx);
            format!("
                {e1_instrs}
                mov [rsp - {stack_offset}], rax
                {e2_instrs}
                mov rbx, rax
                xor rbx, [rsp - {stack_offset}]
                test rbx, 1
                mov rdx, {ERR_INVALID_ARGU}
                jne throw_error
                cmp rax, [rsp - {stack_offset}]
                mov rbx, 3
                mov rax, 1
                cmove rax, rbx
            ")
        }
        Expr::BinOp(Op2::Greater, subexpr1, subexpr2) => {
            let e1_instrs = compile_expr(subexpr1, si, env, label_idx, loop_idx);
            let e2_instrs = compile_expr(subexpr2, si + 1, env, label_idx, loop_idx);
            format!("
                {e1_instrs}
                mov [rsp - {stack_offset}], rax
                mov rbx, rax
                and rbx, 1
                cmp rbx, 1
                mov rdx, {ERR_INVALID_ARGU}
                je throw_error

                {e2_instrs}
                mov rbx, rax
                and rbx, 1
                cmp rbx, 1
                mov rdx, {ERR_INVALID_ARGU}
                je throw_error

                mov rbx, rax
                xor rbx, [rsp - {stack_offset}]
                test rbx, 1
                mov rdx, {ERR_INVALID_ARGU}
                jne throw_error
                mov rbx, [rsp - {stack_offset}]
                cmp rbx, rax
                mov rbx, 3
                mov rax, 1
                cmova rax, rbx
            ")
        }        
        Expr::BinOp(Op2::Less, subexpr1, subexpr2) => {
          let e1_instrs = compile_expr(subexpr1, si, env, label_idx, loop_idx);
          let e2_instrs = compile_expr(subexpr2, si + 1, env, label_idx, loop_idx);
          format!("
              {e1_instrs}
              mov [rsp - {stack_offset}], rax
              mov rbx, rax
              and rbx, 1
              cmp rbx, 1
              mov rdx, {ERR_INVALID_ARGU}
              je throw_error

              {e2_instrs}
              mov rbx, rax
              and rbx, 1
              cmp rbx, 1
              mov rdx, {ERR_INVALID_ARGU}
              je throw_error

              mov rbx, rax
              xor rbx, [rsp - {stack_offset}]
              test rbx, 1
              mov rdx, {ERR_INVALID_ARGU}
              jne throw_error
              mov rbx, [rsp - {stack_offset}]
              cmp rbx, rax
              mov rbx, 3
              mov rax, 1
              cmovl rax, rbx
          ")
        }     
        Expr::BinOp(Op2::GreaterEqual, subexpr1, subexpr2) => {
          let e1_instrs = compile_expr(subexpr1, si, env, label_idx, loop_idx);
          let e2_instrs = compile_expr(subexpr2, si + 1, env, label_idx, loop_idx);
          format!("
              {e1_instrs}
              mov [rsp - {stack_offset}], rax
              mov rbx, rax
              and rbx, 1
              cmp rbx, 1
              mov rdx, {ERR_INVALID_ARGU}
              je throw_error

              {e2_instrs}
              mov rbx, rax
              and rbx, 1
              cmp rbx, 1
              mov rdx, {ERR_INVALID_ARGU}
              je throw_error

              mov rbx, rax
              xor rbx, [rsp - {stack_offset}]
              test rbx, 1
              mov rdx, {ERR_INVALID_ARGU}
              jne throw_error
              mov rbx, [rsp - {stack_offset}]
              cmp rbx, rax
              mov rbx, 3
              mov rax, 1
              cmovae rax, rbx
          ")
      }        
      Expr::BinOp(Op2::LessEqual, subexpr1, subexpr2) => {
        let e1_instrs = compile_expr(subexpr1, si, env, label_idx, loop_idx);
        let e2_instrs = compile_expr(subexpr2, si + 1, env, label_idx, loop_idx);
        format!("
            {e1_instrs}
            mov [rsp - {stack_offset}], rax
            mov rbx, rax
            and rbx, 1
            cmp rbx, 1
            mov rdx, {ERR_INVALID_ARGU}
            je throw_error

            {e2_instrs}
            mov rbx, rax
            and rbx, 1
            cmp rbx, 1
            mov rdx, {ERR_INVALID_ARGU}
            je throw_error

            mov rbx, rax
            xor rbx, [rsp - {stack_offset}]
            test rbx, 1
            mov rdx, {ERR_INVALID_ARGU}
            jne throw_error
            mov rbx, [rsp - {stack_offset}]
            cmp rbx, rax
            mov rbx, 3
            mov rax, 1
            cmovle rax, rbx
        ")
        }
        Expr::Id(s) if s == "input" => {
          format!("mov rax, rdi")
        }     
        Expr::Id(s) => {
            if env.contains_key(s) == false {
                panic!("Unbound variable identifier {s}")
            }
            format!("mov rax, [rsp-{}]", env.get(s).unwrap())
        }
        Expr::Let(vars, body) => {
            let mut binding_str: String = "".to_owned();

            let mut stack_offset_idx = 0;
            let mut nenv = env.clone();
            for var in vars {
                let (id, expr) = var;
                //println!("Compile Let SubElem: {id}");
                let current_stack_offset = si + stack_offset_idx;
                let e_instrs = compile_expr(expr, current_stack_offset, &nenv, label_idx, loop_idx);
                nenv = nenv.update(id.to_owned(), current_stack_offset * 8);
                binding_str.push_str(&format!("{e_instrs}\n").to_owned());
                binding_str.push_str(&format!("mov [rsp-{}], rax\n", current_stack_offset * 8).to_owned());
                stack_offset_idx += 1;
            }
            //println!("Compile Let Done map{:?}.", nenv);
            let b_instrs = compile_expr(body, si + 1 + stack_offset_idx, &nenv, label_idx, loop_idx);
            binding_str.push_str(&format!("{b_instrs}\n").to_owned());
            return binding_str;
        }
        Expr::If(cond, t_e, f_e) => {
          let end_label = create_label(label_idx, "ifend");
          let else_label = create_label(label_idx, "ifelse");
          let cond_instrs = compile_expr(cond, si, env, label_idx, loop_idx); 
          let thn_instrs = compile_expr(t_e, si, env, label_idx, loop_idx); 
          let els_instrs = compile_expr(f_e, si, env, label_idx, loop_idx); 
          format!(" 
              {cond_instrs}
              cmp rax, 1 
              je {else_label} 
              {thn_instrs}
              jmp {end_label}
              {else_label}: 
              {els_instrs}
              {end_label}:
          ")
        }
        Expr::Block(exprs) => {
          let mut binding_str: String = "".to_owned();
          let mut stack_offset_idx = 0;
          //let mut nenv = env.clone();
          for expr in exprs {
              //println!("Compile Let SubElem: {id}");
              let current_stack_offset = si + stack_offset_idx;
              let e_instrs = compile_expr(expr, current_stack_offset, env, label_idx, loop_idx);
              //nenv = nenv.update(id.to_owned(), current_stack_offset * 8);
              binding_str.push_str(&format!("{e_instrs}\n").to_owned());
              binding_str.push_str(&format!("mov [rsp-{}], rax\n", current_stack_offset * 8).to_owned());
              stack_offset_idx += 1;
          }
          return binding_str;
        }
        Expr::Loop(expr) => {

          let start_label = create_label(label_idx, "loopstart");
          let next_loop_idx = *label_idx;
          let end_label = create_label(label_idx, "loopend");
          let e_instrs = compile_expr(expr, si, env, label_idx, next_loop_idx);
          format!(" 
              {start_label}:
              {e_instrs}
              jmp {start_label}
              {end_label}:
          ")
        }                
        Expr::Break(expr) => {
          if loop_idx == 0 {
            panic!("Invalid break.")
          }
          let end_label = format!("loopend_{loop_idx}");
          let e_instrs = compile_expr(expr, si, env, label_idx, loop_idx);
          format!(" 
              {e_instrs}
              jmp {end_label}
          ")
        }       
        Expr::Set(name, expr) => {
          if env.contains_key(name) == false {
            panic!("Unbound variable identifier {name}")
          }

          let e_instrs = compile_expr(expr, si, env, label_idx, loop_idx);
          format!(" 
              mov rax, [rsp-{}]
              {e_instrs}
              mov [rsp-{}], rax
          ", env.get(name).unwrap(), env.get(name).unwrap())
        }
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    let mut label_index : i32 = 1;
    let mut loop_index = 0;
    in_file.read_to_string(&mut in_contents)?;
    
    match parse(&in_contents) {
        Ok(sexp) => {
            let expr = parse_expr(&sexp);
            //println!("expr: {:?}", expr);
            let env = HashMap::new();
            let result = compile_expr(&expr, 2, &env, &mut label_index, loop_index);
            let asm_program = format!("
        section .text
        global our_code_starts_here
        extern snek_error 
        throw_error:
        mov rdi, rdx
        push rsp
        call snek_error
        our_code_starts_here:
        {}
        mov rdx, 3
        jo throw_error
        ret
        ", result);

            let mut out_file = File::create(out_name)?;
            out_file.write_all(asm_program.as_bytes())?;

            Ok(())
        },
        Err(e) => {
            panic!("Invalid sexp.")
        }
    }
    /*let sexp = &parse(&in_contents);
    if sexp.is_err() == true {
        panic!("Invalid sexp.")
    }    */
    //let expr = parse_expr(sexp);
}
