#![allow(unreachable_patterns)]
#![allow(unused_variables)]
#![allow(dead_code)]



use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::Atom::*;
use sexp::*;

use im::HashMap;

//I used ChatGPT mainly for the assembly code in the comparison methods and for the isNum and isBool code 
//About half of my test cases weren't working since I started this assignment too late

enum Op1 { Add1, Sub1, IsNum, IsBool, }

enum Op2 { Plus, Minus, Times, Equal, Greater, GreaterEqual, Less, LessEqual, }

enum Expr {
    Num(i64), //done
    True, //done
    False, //done
    Add1(Box<Expr>), //done
    Sub1(Box<Expr>), //done
    IsNum(Box<Expr>),  
    IsBool(Box<Expr>),
    Plus(Box<Expr>, Box<Expr>), //done
    Minus(Box<Expr>, Box<Expr>), //done
    Times(Box<Expr>, Box<Expr>), //done
    Eq(Box<Expr>, Box<Expr>), //done (equal)
    Greater(Box<Expr>, Box<Expr>),
    GreaterEqual(Box<Expr>, Box<Expr>),
    Less(Box<Expr>, Box<Expr>),
    LessEqual(Box<Expr>, Box<Expr>),
    Let(Vec<(String, Expr)>, Box<Expr>), //done
    Id(String), //done
    If(Box<Expr>, Box<Expr>, Box<Expr>), //done
    Loop(Box<Expr>), //done
    Block(Vec<Expr>), //done
    Break(Box<Expr>), //done
    Print(Box<Expr>), //done
    Set(String, Box<Expr>) //done
}

impl Expr{
    fn is_bool(&self) -> bool{
        match self{
            Expr::True | Expr::False => true,
            _ => false,
        }
    }
    fn is_num(&self) -> bool{
        match self{
            Expr::Num(e) => true,
            _ => false,
        }
    }
}
fn in_range(i:i64) -> bool {
    if i >= -4611686018427387904 && i < 4611686018427387903{
        return true;
    }
    return false;
}

fn parse_expr(s: &Sexp) -> Expr {
    match s{
        //valid range is 63 bit signed ranged is 
        Sexp::Atom(I(n)) => { 
            //return Expr::Num(i32::try_from(*n).unwrap())  
            let num = i64::try_from(*n).unwrap();
            if in_range(num){
                return Expr::Num(num) 
            } else{ 
                panic!("invalid")
            }
        },
        Sexp::Atom(S(name)) if name == "true" => Expr::True,
        Sexp::Atom(S(name)) if name == "false" => Expr::False,
        Sexp::Atom(S(s1)) => {
            let s = s1.to_string(); 
            if s == "add1" || s == "sub1" || s == "print" || s == "let" || s == "loop" || s == "break" ||
                s == "mut" || s == "const" || s == "fn" || s == "if" || s == "else" || s == "match" || s == "while" || s == "for" ||
                s == "return" || s == "return" || s == "impl" || s == "enum" || s == "mod" || s == "pub" || s == "crate" || s == "super" ||
                s == "self" || s == "set!"{
                    panic!("keyword")

            }
            return Expr::Id(s);
        },
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(op)), e] if op == "add1" => {
                return Expr::Add1(Box::new(parse_expr(e))) 
                /* 
                let left = parse_expr(e);
                if let Expr::Num(n1) = left{
                    let result = n1.checked_add(1);
                    match result {
                        Some(x) => {
                            if in_range(n1 + 1){
                                return Expr::Add1(Box::new(parse_expr(e)))
                            } else{
                                panic!("OVERFLOW")
                            }
                        },
                        None => panic!("OVERFLOW")
                    }
                }
                panic!("OVERFLOW")
                */
            },
            [Sexp::Atom(S(op)), e] if op == "sub1" => { 
                return Expr::Sub1(Box::new(parse_expr(e)))
                /* 
                let left = parse_expr(e);
                if let Expr::Num(n1) = left{
                    let result = n1.checked_sub(1);
                    match result {
                        Some(x) => {
                            if in_range(n1 - 1){
                                return Expr::Sub1(Box::new(parse_expr(e)))
                            } else{
                                panic!("OVERFLOW")
                            }
                        },
                        None => panic!("OVERFLOW")
                    }
                }
                panic!("OVERFLOW")
                //return Expr::Sub1(Box::new(parse_expr(e)))
                */
            },
            [Sexp::Atom(S(op)), e1, e2] if op == "+" => {

                /* 
                let left = parse_expr(e1);
                let right = parse_expr(e2);
                if left.is_num() && right.is_num(){
                    return Expr::Plus(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
                }

                panic!("invalid argument")
                */

                return Expr::Plus(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)));
                /* 
                let left = parse_expr(e1);
                let right = parse_expr(e2);
                
                if let Expr::Num(n1) = left{
                    if let Expr::Num(n2) = right{
                        let result = n1.checked_add(n2);
                        match result {
                            Some(x) => {
                                //if in_range(n1+n2){
                                println!("Hello1");
                                return Expr::Plus(Box::new(left), Box::new(right))
                                //} else{
                                //    panic!("OVERFLOW")
                                //}
                            },
                            None => panic!("OVERFLOW1"),
                        }   
                    }
                    panic!("OVERFLOW2")
                }
                panic!("OVERFLOW3");
                */
            },  
            [Sexp::Atom(S(op)), e1, e2] if op == "-" => {
                /* 
                let left = parse_expr(e1);
                let right = parse_expr(e2);
                if left.is_num() && right.is_num(){
                    return Expr::Minus(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
                }
                */

                return Expr::Minus(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)));
                //panic!("invalid argument")
                /* 
                let left = parse_expr(e1);
                let right = parse_expr(e2);
                if let Expr::Num(n1) = left{
                    if let Expr::Num(n2) = right{
                        let result = n1.checked_sub(n2);
                        match result {
                            Some(x) => {
                                //if in_range(n1-n2){
                                println!("Hello2");
                                return Expr::Minus(Box::new(left), Box::new(right))
                                //} else{
                                //    panic!("OVERFLOW")
                                //}
                            },
                            None => panic!("OVERFLOW3"),
                        }   
                    }
                    panic!("OVERFLOW4")
                }
                panic!("OVERFLOW5");
                //return Expr::Minus(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
                */
            },
            [Sexp::Atom(S(op)), e1, e2] if op == "*" => {
                /* 
                let left = parse_expr(e1);
                let right = parse_expr(e2);
                if left.is_num() && right.is_num(){
                    return Expr::Times(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
                }

                panic!("invalid argument")
                */

                return Expr::Times(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)));

  
                /* 
                let left = parse_expr(e1);
                let right = parse_expr(e2);
                if let Expr::Num(n1) = left{
                    if let Expr::Num(n2) = right{
                        let result = n1.checked_mul(n2);
                        match result {
                            Some(x) => {
                                println!("Hello3");
                                if in_range(n1*n2){
                                    return Expr::Times(Box::new(left), Box::new(right))
                                }
                            },
                            None => panic!("OVERFLOW"),
                        }   
                    }
                    panic!("OVERFLOW")
                }
                panic!("OVERFLOW");
                */
                //return Expr::Times(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            },  
            [Sexp::Atom(S(op)), e] if op == "isnum" => {
                return Expr::IsNum(Box::new(parse_expr(e)));
            }, 
            [Sexp::Atom(S(op)), e] if op == "isbool" => {
                return Expr::IsBool(Box::new(parse_expr(e)));
            },
            [Sexp::Atom(S(op)), Sexp::Atom(S(name)), e] if op == "set!" => {
                return Expr::Set(name.to_string(), Box::new(parse_expr(e)))
            },
            [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => {
                return Expr::Block(exprs.into_iter().map(parse_expr).collect())
            },
            [Sexp::Atom(S(op)), e] if op == "loop" => {
                return Expr::Loop(Box::new(parse_expr(e)))
            },
            [Sexp::Atom(S(op)), e] if op == "break" => {
                return Expr::Break(Box::new(parse_expr(e)))
            },
            [Sexp::Atom(S(op)), e] if op == "print" => {
                return Expr::Print(Box::new(parse_expr(e)))
            },
            [Sexp::Atom(S(op)), e1, e2] if op == "=" => {
               //let left = parse_expr(e1);
               //let right = parse_expr(e2);
               /* 
               if left.is_num() && right.is_num(){
                  return Expr::Eq(Box::new(left), Box::new(right));
               }
               else if left.is_bool() && right.is_bool(){
                  return Expr::Eq(Box::new(left), Box::new(right));
               }
               */
               //panic!("invalid argument")
               return Expr::Eq(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            },
            [Sexp::Atom(S(op)), e1, e2] if op == ">" => {

                /* 
                let first = parse_expr(e1);
                let second = parse_expr(e2);
                if first.is_bool() {
                    panic!("invalid argument")
                }
                if second.is_bool(){
                    panic!("invalid argument")
                }
                */
                return Expr::Greater(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            },
            [Sexp::Atom(S(op)), e1, e2] if op == "<" => {

                /* 
                let first = parse_expr(e1);
                let second = parse_expr(e2);
                if first.is_bool() {
                    panic!("invalid argument")
                }
                if second.is_bool(){
                    panic!("invalid argument")
                }
                */
                return Expr::Less(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            },
            [Sexp::Atom(S(op)), e1, e2] if op == ">=" => {
                /* 
                let first = parse_expr(e1);
                let second = parse_expr(e2);
                if first.is_bool() {
                    panic!("invalid argument")
                }
                if second.is_bool(){
                    panic!("invalid argument")
                }
                */
                return Expr::GreaterEqual(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            },
            [Sexp::Atom(S(op)), e1, e2] if op == "<=" => {
                /* 
                let first = parse_expr(e1);
                let second = parse_expr(e2);
                if first.is_bool() {
                    panic!("invalid argument")
                }
                if second.is_bool(){
                    panic!("invalid argument")
                }
                */
                return Expr::LessEqual(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            },
            /* 
            [Sexp::Atom(S(keyword)), Sexp::List(vec), body] if keyword == "let" => match &vec[..] {
                [Sexp::Atom(S(name)), val] => 
                return Expr::Let(
                    name.to_string(),
                    Box::new(parse_expr(val)),
                    Box::new(parse_expr(body)),
                ),
                _ => panic!("parse error"),
            }, */
            [Sexp::Atom(S(keyword)), cond, thn, els] if keyword == "if" => {
                return Expr::If(Box::new(parse_expr(cond)), Box::new(parse_expr(thn)), Box::new(parse_expr(els)))
            },
            [Sexp::Atom(S(l)), Sexp::List(bindings), body] if l == "let" => {
                let mut let_bindings:Vec<(String, Expr)> = vec![];
        
                if bindings.len() < 1 { //should have at least one binding
                    panic!("Invalid");
                }
                //generated this block of code from GPT but modified the if conditionals
                for binding in bindings{
                    match binding {
                        Sexp::List(pair) if pair.len() == 2 => {
                            let_bindings.push((pair.get(0).unwrap().to_string(), parse_expr(pair.get(1).unwrap())));
                        },
                        Sexp::List(pair) if pair.len() == 1 => {
                            panic!("Unbound variable identifier {}", pair.get(0).unwrap().to_string());
                        },
                        _ => panic!("Invalid")
                    }
                }
                return Expr::Let(let_bindings, Box::new(parse_expr(body)));
            },
            _ => panic!("Invalid"),
        },
        _ => panic!("Invalid"),
    }
}


//every label needs a unique identifier
fn new_label(l: &mut i32, s: &str) -> String {
    let current = *l;
    *l += 1;
    format!("{s}_{current}")
}

fn compile_expr(e: &Expr, si: i32, env: &HashMap<String, i32>, brake: &String, l: &mut i32, inside_loop: &mut bool) -> String {
    match e {
        Expr::Num(n) => format!("mov rax, {}", *n << 1), //done
        Expr::True => format!("mov rax, {}", 3), //done
        Expr::False => format!("mov rax, {}", 1), //done
        Expr::Id(s) if s == "input" => {        
            format!("mov rax, rdi")
        }, //done
        Expr::Id(id) => {
            if env.contains_key(id) == false{
                panic!("Unbound variable identifier {id}")
            } else{
                format!("mov rax, [rsp-{}]", env.get(id).unwrap())
            }   
        },
        Expr::IsBool(e) => {
            if e.is_bool() == true{
                return format!("mov rax, {}", 3)
            } else{
                return format!("mov rax, {}", 1)
            }
        },
        Expr::IsNum(e) => {
            if e.is_num() == true{
                return format!("mov rax, {}", 3)
            } else{
                return format!("mov rax, {}", 1)
            }
        }
        Expr::Print(e) => {
            let e_is = compile_expr(e, si, env, brake, l, inside_loop);
            let index = if si % 2 == 1 { si + 1 } else { si };
            let offset = index * 8;
            format!("
              {e_is}
              mov [rsp - {offset}], rdi
              sub rsp, {offset}
              mov rdi, rax
              call snek_print
              add rsp, {offset}
              mov rdi, [rsp - {offset}]
            ")
        },
        Expr::Set(name, val) => {
            if env.contains_key(name) == false{
                panic!("Unbound variable identifier {name}")
            }
            let offset = env.get(name).unwrap(); //env.get(name).unwrap() * 8;

            let save = format!("mov [rsp - {offset}], rax");
            let val_is = compile_expr(val, si, env, brake, l, inside_loop);
            format!("
              {val_is}
              {save}
              ")
        },
        Expr::Add1(subexpr) => compile_expr(subexpr, si, env, brake, l, inside_loop) + "\nadd rax, 2", //done
        Expr::Sub1(subexpr) => compile_expr(subexpr, si, env, brake, l, inside_loop) + "\nsub rax, 2", //done
        Expr::Break(e) => { //done
            //if *inside_loop == true{
            let e_is = compile_expr(e, si, env, brake, l, inside_loop);
            format!("
            {e_is}
            jmp {brake}
            ")
            //} else{
            //    panic!("break")
            //}
        },
        Expr::Loop(e) => {
            let startloop = new_label(l, "loop");
            let endloop = new_label(l, "loopend");
            *inside_loop = true;
            let e_is = compile_expr(e, si, env, &endloop, l, inside_loop);
            *inside_loop = false;
            format!("
              {startloop}:
              {e_is}
              jmp {startloop}
              {endloop}:
            ")
        }, //(if (= 10 10) true false)

        Expr::Block(es) => {
            es.into_iter().map(|e| { compile_expr(e, si, env, brake, l, inside_loop) }).collect::<Vec<String>>().join("\n")
        },
        Expr::Eq(e1, e2) => {
            let e1_instrs = compile_expr(e1, si, env, brake, l, inside_loop);
            let e2_instrs = compile_expr(e2, si + 1, env, brake, l, inside_loop);
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
        },
        Expr::Less(e1, e2) => {
            let e1_instrs = compile_expr(e1, si, env, brake, l, inside_loop);
            let e2_instrs = compile_expr(e2, si + 1, env, brake, l, inside_loop);
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
                cmovg rax, rbx
                "
            )
        },
        Expr::Greater(e1, e2) => {
            let e1_instrs = compile_expr(e1, si, env, brake, l, inside_loop);
            let e2_instrs = compile_expr(e2, si + 1, env, brake, l, inside_loop);
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
                cmovl rax, rbx
                "
            )
        },
        Expr::LessEqual(e1, e2) => {
            let e1_instrs = compile_expr(e1, si, env, brake, l, inside_loop);
            let e2_instrs = compile_expr(e2, si + 1, env, brake, l, inside_loop);
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
                cmovge rax, rbx
                "
            )
        },
        Expr::GreaterEqual(e1, e2) => {
            let e1_instrs = compile_expr(e1, si, env, brake, l, inside_loop);
            let e2_instrs = compile_expr(e2, si + 1, env, brake, l, inside_loop);
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
                cmovle rax, rbx
                "
            )
        },
        Expr::If(cond, thn, els) => {
            let end_label = new_label(l, "ifend");
            let else_label = new_label(l, "ifelse");
            let cond_instrs = compile_expr(cond, si, env, brake, l, inside_loop);
            let thn_instrs = compile_expr(thn, si, env, brake, l, inside_loop);
            let els_instrs = compile_expr(els, si, env, brake, l, inside_loop);
            format!(
                "
                {cond_instrs}
                cmp rax, 3
                jne {else_label}
                {thn_instrs}
                jmp {end_label}
                {else_label}:
                {els_instrs}
                {end_label}:
                "
            )
        },
        Expr::Plus(e1, e2) => {
            let e1_instrs = compile_expr(e1, si, env, brake, l, inside_loop);
            let e2_instrs = compile_expr(e2, si + 1, env, brake, l, inside_loop);
            let stack_offset = si * 8;
            format!(
                "
              {e1_instrs}
              test rax, 1
              mov rdi, 3
              jnz throw_error 
              mov [rsp - {stack_offset}], rax
              {e2_instrs}
              test rax, 1
              mov rdi, 3
              jnz throw_error
              add rax, [rsp - {stack_offset}]
          "
            )
        },
        Expr::Minus(e1, e2) => {
            let e1_instrs = compile_expr(e2, si, env, brake, l, inside_loop);
            let e2_instrs = compile_expr(e1, si + 1, env, brake, l, inside_loop);
            let stack_offset = si * 8;
            format!(
                "
              {e1_instrs}
              test rax, 1
              mov rdi, 3
              jnz throw_error
              mov [rsp - {stack_offset}], rax
              {e2_instrs}
              test rax, 1
              mov rdi, 3
              jnz throw_error
              sub rax, [rsp - {stack_offset}]
          "
            )
        },
        Expr::Times(e1, e2) => {
            let e1_instrs = compile_expr(e1, si, env, brake, l, inside_loop);
            let e2_instrs = compile_expr(e2, si + 1, env, brake, l, inside_loop);
            let stack_offset = si * 8;
            format!(
                "
              {e1_instrs}
              test rax, 1
              mov rdi, 3
              jnz throw_error
              mov [rsp - {stack_offset}], rax
              {e2_instrs}
              test rax, 1
              mov rdi, 3
              jnz throw_error
              imul rax, [rsp - {stack_offset}]
              sar rax, 1

          "
            )
        },
        Expr::Let(bindings, body) => {
            //list of bindings of the identifier to the the actual binding expression
            //populate nenv with all new bindings -> if var already exists -> return an error
                //then with nenv do the body_instrs
        
            let mut nenv = env.clone();
            let mut binding_instrs = String::new();
            let mut si_counter = si;
        
            let mut dupl: HashMap<String, i32> = HashMap::new(); //only checks in the binding list for duplicate strings (so shadowed bindings (in the body are still ok))
            //println!("{:#?}", bindings);

            //duplicate binding check if bindings has any two pairs with the same string name
            for binding in bindings{

                if dupl.contains_key(&binding.0.to_string()) == false{
                    dupl = dupl.update(binding.0.to_string(), 1);
                } else{
                    panic!("Duplicate binding");
                }
                println!("{}", compile_expr(&binding.1, si_counter, &nenv, brake, l, inside_loop));

                binding_instrs.push_str(&compile_expr(&binding.1, si_counter, &nenv, brake, l, inside_loop));
                binding_instrs.push_str("\n");
                binding_instrs.push_str(&format!("mov [rsp-{}], rax\n", si_counter*8));
                nenv = nenv.update(binding.0.to_string(), si_counter*8);  //let nenv = env.update(x.to_string(), si*8);
                si_counter+=1;
            }

            println!("{:?}", nenv);
        
            println!("{}", si_counter+1);
            let body_instrs = compile_expr(body, si_counter+1, &nenv, brake, l, inside_loop);
            println!("{}", body_instrs);
            let stack_offset = si*8;
            return format!("{binding_instrs}{body_instrs}");
        },
        _ => panic!("Couldn't compile") 
    }
}


//notes 

//input is first command-line argument
//if none is given, then value is false
//when running the program the arguments should be provided as true, false, or base-10 (valid input)


//relative comparison operators like < and > evaluate their arguments and then evaluate to true or false based on the comparison result
//Boolean expressions (true and false) evaluate to themselves

//




//Report errors

//overflow error should be reported for any arithmetic
//There is a binding list containing two or more bindings with the same name. The error should contain the string "Duplicate binding"
//An identifier is unbound (there is no surrounding let binding for it) The error should contain the string "Unbound variable identifier {identifier}" (where the actual name of the variable is substituted for {identifier})
//A break appears outside of any surrounding loop. The error should contain "break"
//An invalid identifier is used (it matches one of the keywords). The error should contain "keyword"


fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    // You will make result hold the result of actually compiling
    let result = "mov rax, 131";

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let expr = parse_expr(&parse(&in_contents).unwrap());
    let mut labels = 0; //mutable global counter for amount of lables
    let mut inside_loop = false;
    let result = compile_expr(&expr, 2, &HashMap::new(), &String::from(""), &mut labels, &mut inside_loop);

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
