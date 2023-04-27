// Author: Ehson Pirouzian
// template provided by CSE 231 class UCSD. Taught by Joe Politz

use std::env;
use std::fs::File;
use std::io::prelude::*;
use sexp::Atom::*;
use sexp::*;

use im::HashMap;

#[derive(Debug)]
enum Op1 {
    Add1,
    Sub1,
    IsNum,
    IsBool,
    Negate,
}

#[derive(Debug)]
enum Op2 {
    Plus,
    Minus,
    Times,
    Equal,
    Less,
    Great,
    LessEqual,
    GreatEqual,
}


#[derive(Debug)]
enum Expr {
    Number(i64),
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    True, 
    False, 
    Set(String, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Block(Vec<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),

}


// Parser Essentially transforms Sexp (from input file) into Expr class
fn parse_expr(s: &Sexp) -> Expr {
    // Given Sexp s I match to the following
    match s {
        Sexp::Atom(I(n)) => {
            if (*n > 4611686018427387903) | (*n < -4611686018427387904) {
                panic!("Invalid overflow");
            }
            Expr::Number(*n)
        },
        Sexp::Atom(S(name)) if name == "true" => Expr::True,
        Sexp::Atom(S(name)) if name == "false" => Expr::False,
        Sexp::Atom(S(s)) => Expr::Id(s.clone()),

        // if its a vector it goes through every combination that could be there
        Sexp::List(vec) => {
            match &vec[..] {
                // Most of them just make a Expr class with the rest of the expression parsed inside of it, into expr's
                [Sexp::Atom(S(op)), e] if op == "add1" => Expr::UnOp(Op1::Add1, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "sub1" => Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "negate" => Expr::UnOp(Op1::Negate, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "isbool" => Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "isnum" => Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e))),

                [Sexp::Atom(S(op)), e1, e2] if op == "+" => Expr::BinOp(Op2::Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "-" => Expr::BinOp(Op2::Minus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "*" => Expr::BinOp(Op2::Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                
                [Sexp::Atom(S(op)), e1, e2] if op == "=" => Expr::BinOp(Op2::Equal, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "<" => Expr::BinOp(Op2::Less, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == ">" => Expr::BinOp(Op2::Great, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == ">=" => Expr::BinOp(Op2::GreatEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "<=" => Expr::BinOp(Op2::LessEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),

                [Sexp::Atom(S(op)), e1, e2] if op == "let" => Expr::Let(vector_bind(e1), Box::new(parse_expr(e2))),

                // made use of the starter code that was put on the discussion board
                [Sexp::Atom(S(op)), Sexp::Atom(S(name)), e] if op == "set!" => Expr::Set(name.to_string(), Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), es @ ..] if op == "block" => {
                    if es.len() == 0 {
                        panic!("Invalid block arguments")
                    }
                    Expr::Block(es.into_iter().map(parse_expr).collect())
                },
                [Sexp::Atom(S(op)), e] if op == "loop" => Expr::Loop(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "break" => Expr::Break(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), condition, then_e, else_e] if op == "if" => Expr::If(Box::new(parse_expr(condition)), Box::new(parse_expr(then_e)), Box::new(parse_expr(else_e)),),
                // otherwise there is a fail and an error ensues
                _ => panic!("Invalid"),
            }
        },
        // If it does not match with anything at all then another error happens
        _ => panic!("Invalid"),
    }
}

// this function returns a list of binds for Let statements
fn vector_bind(s: &Sexp) -> Vec<(String, Expr)> {
    let mut return_vector = vec!();
    let mut i = 0;
    let mut j = 0;
    match s {
        // grab the list
        Sexp::List(vec) => {
            // if its an empty list fail
            if vec.len() == 0 {
                panic!("Invalid");
                //panic!("Improper Arguments");
            }
            // iterate through the list of binds
            while i < vec.len() {
                // add the bind into the return list of tuples
                // but call parse_bind to get the corresponding tuple
                return_vector.insert(i, parse_bind(&vec[i]));

                // loop through all existing binds to check it does not exist already
                while j < return_vector.len()-1 {
                    if return_vector[j].0 == return_vector[i].0 {
                        panic!("Duplicate binding");
                    }
                    j = j + 1;
                }
                i = i + 1;
                j = 0;
            };
            // return the vector
            return_vector
        },
        // _ => panic!("Improper Arguments"),
        _ => panic!("Invalid"),

    }
}

// helper function for parsing the vector of binds
fn parse_bind(s: &Sexp) -> (String, Expr) {
    match s {
        // for the tuple
        Sexp::List(vec) => {
            match &vec[..] {
                // each value of the tuple will be a String, with a parsed Sexp which is a Expr
                [Sexp::Atom(S(op)), x] => {
                    let keywords = ["input", "set!", "let", "if", "block", "break", "loop"];
                    let string = &op[..];
                    if keywords.contains(&string) {
                        panic!("keyword issue");
                    }
                    (op.clone(), parse_expr(x))
                }
                // otherwise panic for bad syntax
                _ => panic!("Invalid"),
            }
        },
        _ => panic!("Invalid"),

    }
}


// made use of the starter code that was put on the discussion board
fn new_label(l: &mut i32, s: &str) -> String {
    let current = *l;
    *l += 1;
    format!("{s}_{current}")
}

// All compiliation happens here
fn compile(e: &Expr, si: i32, env: &HashMap<String, i32>, brake: &String, l: &mut i32) -> String {
    let mut i = 0;
    let mut e_instr = String::new(); // string to return for let statements
    let mut new_env = env.clone(); // clone the env for let statements

    // for a given Expr compute the following
    match e {

        // If its a number, then store number in rax.
        Expr::Number(n) => format!("mov rax, {}", (*n << 1)),
        Expr::True => format!("mov rax, {}", 3),
        Expr::False => format!("mov rax, {}", 1),
        Expr::Id(s) if s == "input" => format!("mov rax, rdi"),
        // if its a UnOp with one input then do the following functions, 
        // but continue compiling the subexpressions
        Expr::UnOp(operation, subexpr) => match operation {
            Op1::Add1 => {
                let val = compile(subexpr, si, env, brake, l);
                format!("
                    {val}
                    mov rbx, rax
                    and rbx, 1
                    cmp rbx, 1
                    mov rdx, 10
                    je throw_error
                    add rax, 2
                    mov rdx, 9
                    jo throw_error
                ", )
            },
            Op1::Sub1 => {
                let val = compile(subexpr, si, env, brake, l);
                format!("
                    {val}
                    mov rbx, rax
                    and rbx, 1
                    cmp rbx, 1
                    mov rdx, 10
                    je throw_error
                    sub rax, 2
                    mov rdx, 9
                    jo throw_error
                ", )
            },
            Op1::Negate => compile(subexpr, si, env, brake, l) + "\nneg rax",
            Op1::IsNum => {
                let val = compile(subexpr, si, env, brake, l);
                format!("
                    {val}
                    and rax, 1
                    cmp rax, 1
                    mov rbx, 1
                    mov rax, 3
                    cmove rax, rbx
                ", )
            },
            Op1::IsBool => {
                let val = compile(subexpr, si, env, brake, l);
                format!("
                    {val}
                    and rax, 1
                    cmp rax, 1
                    mov rbx, 3
                    mov rax, 1
                    cmove rax, rbx
                ", )
            }
        },
        // if its an id check it exists then move into rax the value if it does exist.
        Expr::Id(s) => {
            if env.get(s) == None {
                panic!("Unbound variable identifier {}", s)
            }
            format!("mov rax, [rsp - {}]", env.get(s).unwrap())
        },
        // Let statement matches a list e and body expr
        Expr::Let(e, body) => {
            // get total binds
            let num_vars = e.len() as i32;
            // for each bind
            while i < num_vars {
                // Just to make printing consistent 
                if i > 0 {
                    e_instr.push_str(&format!("\n"));
                }
                // push into the rax the compiled value 
                e_instr.push_str(&compile(&e[i as usize].1, si+(i*8), &new_env, brake, l));
                let stack_offset = (si*8) + (i*8);
                // then with the calculated stack offset put the calculated value into its
                // respective stack position
                e_instr.push_str(&format!("\nmov [rsp - {stack_offset}], rax"));
                // then insert this pair into the env and use it again for the body and again if
                // there is more than one bind
                new_env.insert((e[i as usize].0).clone(), (si*8) + (i*8));
                i = i + 1;
            }

            // let e_instr = compile(&e[0].1, si, env);
            // let new_env = env.update((e[0].0).clone(), si*8);
            let b_instr = compile(body, si+num_vars, &new_env, brake, l);
            
            // put it together
            format!("
                {e_instr}
                {b_instr}
            ", )
        },
        // Now is the binary operations which boil down to compiling one sides
        // then storing the result into a stack pointer, then doing the next one and 
        // then returning that.
        Expr::BinOp(operation, op1, op2) => match operation {
            Op2::Plus => {
                let e1 = compile(op1, si, env, brake, l);
                let e2 = compile(op2, si+1, env, brake, l);
                let stack_offset = si*8;
                format!("
                    {e1}
                    mov rbx, rax
                    and rbx, 1
                    cmp rbx, 1
                    mov rdx, 10
                    je throw_error
                    mov [rsp - {stack_offset}], rax
                    {e2}
                    mov rbx, rax
                    and rbx, 1
                    cmp rbx, 1
                    mov rdx, 10
                    je throw_error
                    add rax, [rsp - {stack_offset}]
                    mov rdx, 9
                    jo throw_error
                ")
            },
            Op2::Minus => {
                let e1 = compile(op1, si+1, env, brake, l);
                let e2 = compile(op2, si, env, brake, l);
                let stack_offset = si*8;
                format!("
                    {e2}
                    mov rbx, rax
                    and rbx, 1
                    cmp rbx, 1
                    mov rdx, 10
                    je throw_error
                    mov [rsp - {stack_offset}], rax
                    {e1}
                    mov rbx, rax
                    and rbx, 1
                    cmp rbx, 1
                    mov rdx, 10
                    je throw_error
                    sub rax, [rsp - {stack_offset}]
                    mov rdx, 9
                    jo throw_error
                ")
            },
            Op2::Times => {
                let e1 = compile(op1, si, env, brake, l);
                let e2 = compile(op2, si+1, env, brake, l);
                let stack_offset = si*8;
                format!("
                    {e1}
                    mov rbx, rax
                    and rbx, 1
                    cmp rbx, 1
                    mov rdx, 10
                    je throw_error
                    mov [rsp - {stack_offset}], rax
                    {e2}
                    mov rbx, rax
                    and rbx, 1
                    cmp rbx, 1
                    mov rdx, 10
                    je throw_error
                    imul rax, [rsp - {stack_offset}]
                    mov rdx, 9
                    jo throw_error
                    sar rax, 1
                ")
            },
            Op2::Equal => {
                let e1 = compile(op1, si, env, brake, l);
                let e2 = compile(op2, si+1, env, brake, l);
                let stack_offset = si*8;
                format!("
                    {e1}
                    mov [rsp - {stack_offset}], rax
                    {e2}

                    mov rbx, rax
                    xor rbx, [rsp - {stack_offset}]
                    test rbx, 1
                    mov rdx, 10
                    jne throw_error

                    cmp rax, [rsp - {stack_offset}]
                    mov rbx, 3
                    mov rax, 1
                    cmove rax, rbx
                ")
            },
            Op2::Less => {
                let e1 = compile(op1, si, env, brake, l);
                let e2 = compile(op2, si+1, env, brake, l);
                let stack_offset = si*8;
                format!("
                    {e1}
                    mov [rsp - {stack_offset}], rax
                    {e2}

                    mov rbx, rax
                    or rbx, [rsp - {stack_offset}]
                    and rbx, 1
                    cmp rbx, 1
                    mov rdx, 10
                    jz throw_error

                    cmp rax, [rsp - {stack_offset}]
                    mov rbx, 1
                    mov rax, 3
                    cmovle rax, rbx
                ")
            },
            Op2::Great => {
                let e1 = compile(op1, si, env, brake, l);
                let e2 = compile(op2, si+1, env, brake, l);
                let stack_offset = si*8;
                format!("
                    {e1}
                    mov [rsp - {stack_offset}], rax
                    {e2}
                                        
                    mov rbx, rax
                    or rbx, [rsp - {stack_offset}]
                    and rbx, 1
                    cmp rbx, 1
                    mov rdx, 10
                    jz throw_error

                    cmp rax, [rsp - {stack_offset}]
                    mov rbx, 1
                    mov rax, 3
                    cmovge rax, rbx
                ")
            },
            Op2::LessEqual => {
                let e1 = compile(op1, si, env, brake, l);
                let e2 = compile(op2, si+1, env, brake, l);
                let stack_offset = si*8;
                format!("
                    {e1}
                    mov [rsp - {stack_offset}], rax
                    {e2}

                    mov rbx, rax
                    or rbx, [rsp - {stack_offset}]
                    and rbx, 1
                    cmp rbx, 1
                    mov rdx, 10
                    jz throw_error

                    cmp rax, [rsp - {stack_offset}]
                    mov rbx, 1
                    mov rax, 3
                    cmovl rax, rbx
                ")
            },
            Op2::GreatEqual => {
                let e1 = compile(op1, si, env, brake, l);
                let e2 = compile(op2, si+1, env, brake, l);
                let stack_offset = si*8;
                format!("
                    {e1}
                    mov [rsp - {stack_offset}], rax
                    {e2}

                    mov rbx, rax
                    or rbx, [rsp - {stack_offset}]
                    and rbx, 1
                    cmp rbx, 1
                    mov rdx, 10
                    jz throw_error

                    cmp rax, [rsp - {stack_offset}]
                    mov rbx, 1
                    mov rax, 3
                    cmovg rax, rbx
                ")
            },
        },

        // made use of the starter code that was put on the discussion board
        Expr::Set(name, expr) => {
            if env.get(name) == None {
                panic!("Unbound variable identifier {}", name)
            }
            let offset = env.get(name).unwrap();

            let store = format!("mov [rsp - {offset}], rax");
            let val = compile(expr, si, env, brake, l);
            format!("
              {val}
              {store}
              ")
        },
        // made use of the starter code that was put on the discussion board
        Expr::Break(e) => {
            if brake == "" {
                panic!("break issue")
            }
            let val = compile(e, si, env, brake, l);
            format!("
              {val}
              jmp {brake}
            ")
        },
        // made use of the starter code that was put on the discussion board
        Expr::Loop(e) => {
            let startloop = new_label(l, "loop");
            let endloop = new_label(l, "loopend");
            let e_is = compile(e, si, env, &endloop, l);
            format!("
              {startloop}:
              {e_is}
              jmp {startloop}
              {endloop}:
            ")
        },
        // made use of the starter code that was put on the discussion board
        Expr::Block(es) => {
            es.into_iter().map(|e| { compile(e, si, env, brake, l) }).collect::<Vec<String>>().join("\n")
        },
        // made use of the starter code that was put on the discussion board
        Expr::If(condition, then_expr, else_expr) => {
            let end_label = new_label(l, "ifend");
            let else_label = new_label(l, "ifelse");
            let condition_instrs = compile(condition, si, env, brake, l);
            let then_instrs = compile(then_expr, si, env, brake, l);
            let else_instrs = compile(else_expr, si, env, brake, l);
            format!(
                "
              {condition_instrs}
              cmp rax, 1
              je {else_label}
                {then_instrs}
                jmp {end_label}
              {else_label}:
                {else_instrs}
              {end_label}:
           "
            )
        },
    }
}


fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    // simply create a hashmap that will be used in compile
    let hash_map: HashMap<String, i32> = HashMap::new();

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;


    let parsed = parse(&in_contents);
    // check if parsed failed
    match parsed {
        Ok(_) => print!(""),
        Err(_) => panic!("Invalid Syntax (parenthesis issue?)"),
    }
    // otherwise continue
    let expr = parse_expr(&parse(&in_contents).unwrap());
    let mut l = 0;
    let result = compile(&expr, 2, &hash_map, &"".to_string(), &mut l);

    // I was very confused for how to get error handling to work with rdi 
    // or i was not very confused but i was not wanting to write a lot of
    // code to store rdi and then use the reg for errors and if an error did not
    // happen to put it back in rdi. and this would make the code inefficient.
    // ta suggested on ed board to just use rdx instead and write into rdi 
    // if we go into error handler. Which i cant believe i did not realize.
    let asm_program = format!(
        "
section .text
global our_code_starts_here
extern snek_error
throw_error:
  mov rdi, rdx
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