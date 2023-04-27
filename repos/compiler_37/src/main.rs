use std::env;
use std::fs::File;
use std::io::prelude::*;

use std::fmt::format;
use std::hash::Hash;
use std::io::prelude::*;

use sexp::Atom::*;
use sexp::*;

use im::{HashMap, HashSet};

/* 
// REPRESENATIONS //
 false --> 1
 true  --> 3
 error code (overflow) --> 5
 error code (invalid argument) --> 7

*/

const TRUE_VAL:u64  = 3;
const FALSE_VAL:u64 = 1;
const OVERFLOW_ERROR_CODE:u64 = 5;
const INVALID_ARGUMENT_ERROR_CODE:u64 = 7;
const GREATEST_VAL:i64 = 4611686018427387903;
const LEAST_VAL:i64 = -4611686018427387904;


#[derive(Debug)]
enum Val {
    Reg(Reg),
    Imm(u64),
    RegOffset(Reg, i32),
    Label(String),
}

#[derive(Debug)]
enum Reg {
    RAX,
    RSP,
    RDI,
    RBX,
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    Shr(Val,Val),
    Shl(Val,Val),
    Jmp(Val),
    Cmp(Val,Val),
    JEqual(Val),
    JCarry(Val),
    JNotEqual(Val),
    JGreater(Val),
    JGreaterEqual(Val),
    JLess(Val),
    JLessEqual(Val),
    Test(Val,Val),
    Label(Val),
    Xor(Val,Val),
    Cmove(Val,Val),
}

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
    Number(u64),
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

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let si = 2;
    let mut l  = 0;

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let parse_res = parse(&in_contents);
    let expr_inp = match parse_res {
        Ok(val) => val,
        Err(_) => panic!("Invalid S-Expression.")
    };
    let expr = parse_expr(&expr_inp);
    let start_env = HashMap::new();
    let result = compile(&expr,si,&start_env, &mut l, &String::from(""));
    let asm_program = format!(
        "
section .text
extern snek_error
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

fn new_label(l: &mut i32, s: &str) -> String {
    let current = *l;
    *l += 1;
    format!("{s}_{current}")
}

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => { 
            if *n < LEAST_VAL || *n > GREATEST_VAL {
                panic!("Invalid - Number too large")
            } else {
            Expr::Number(*n as u64)}
        },
        Sexp::Atom(S(var)) => {
            if var == "true" {
                Expr::Boolean(true)
            } else if var == "false" {
                Expr::Boolean(false)
            } else {            
                Expr::Id(String::from(var))
            }
        },
        Sexp::List(vec) => {println!("{:?}",vec);
            match &vec[..] {
                [Sexp::Atom(S(op)), e] if op == "add1" => Expr::UnOp(Op1::Add1, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "sub1" => Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "isnum" => Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "isbool" => Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e1, e2] if op == "+" => 
                    Expr::BinOp(Op2::Plus, Box::new(parse_expr(e1)),Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "-" => 
                    Expr::BinOp(Op2::Minus, Box::new(parse_expr(e1)),Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "*" => 
                    Expr::BinOp(Op2::Times, Box::new(parse_expr(e1)),Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "=" => 
                    Expr::BinOp(Op2::Equal, Box::new(parse_expr(e1)),Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == ">=" => 
                    Expr::BinOp(Op2::GreaterEqual, Box::new(parse_expr(e1)),Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "<=" => {
                    println!("less!!!!!");
                    Expr::BinOp(Op2::LessEqual, Box::new(parse_expr(e1)),Box::new(parse_expr(e2)))},
                [Sexp::Atom(S(op)), e1, e2] if op == ">" => 
                    Expr::BinOp(Op2::Greater, Box::new(parse_expr(e1)),Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "<" => 
                    Expr::BinOp(Op2::Less, Box::new(parse_expr(e1)),Box::new(parse_expr(e2))),
                
                [Sexp::Atom(S(op)), e1, e2, e3] if op == "if" => 
                    Expr::If(Box::new(parse_expr(e1)),Box::new(parse_expr(e2)), Box::new(parse_expr(e3))),
                [Sexp::Atom(S(op)), e] if op == "loop" =>  
                    Expr::Loop(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), Sexp::List(list_vec), e] if op == "let" => {
                    let mut bind_vec = Vec::new();
                    for item in list_vec{
                        bind_vec.push(parse_bind(item))
                    }
                    if bind_vec.len() == 0 {
                        panic!("Invalid S-Expression, missing binding for let.")
                    }
                    Expr::Let(bind_vec, Box::new(parse_expr(e)))
                },
                [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => {
                    let coll:Vec<Expr> = exprs.into_iter().map(parse_expr).collect();
                    println!("{:?}",coll);
                    if coll.len() == 0 {
                        panic!("Invalid S-Expression")
                    }
                    Expr::Block(coll)
                },
                [Sexp::Atom(S(op)), Sexp::Atom(S(name)), e] if op == "set!" => {
                    Expr::Set(name.to_string(), Box::new(parse_expr(e)))
                },
                [Sexp::Atom(S(op)), e] if op == "break" => {
                    Expr::Break(Box::new(parse_expr(e)))
                },
                a => {println!("{:?}",a);
                    panic!("Invalid S-Expression.")}
            }},
        
        _ => panic!("Invalid S-Expression.")
        }
}

fn parse_bind(s: &Sexp) -> (String, Expr) {
    let mut reserved_words = HashSet::new();
    reserved_words.insert("let".to_string());
    reserved_words.insert("block".to_string());
    reserved_words.insert("set!".to_string());
    reserved_words.insert("loop".to_string());
    reserved_words.insert("break".to_string());
    reserved_words.insert("if".to_string());
    reserved_words.insert("input".to_string());
    reserved_words.insert("+".to_string());
    reserved_words.insert("-".to_string());
    reserved_words.insert("*".to_string());
    reserved_words.insert("=".to_string());
    reserved_words.insert("true".to_string());
    reserved_words.insert("false".to_string());
    reserved_words.insert(">".to_string());
    reserved_words.insert("<".to_string());
    reserved_words.insert(">=".to_string());
    reserved_words.insert("<=".to_string());

    match s {
        Sexp::List(vec) =>
            match &vec[..] {
                [Sexp::Atom(S(var)), e] => {   
                    if reserved_words.contains(var){
                        panic!("Error - keyword used.")
                    }
                    (String::from(var),parse_expr(e)) },
                _ => panic!("Invalid S-Expression.")
            },
        _ => panic!("Invalid S-Expression.")
    }
}

fn compile_to_instrs(e: &Expr, mut si: i32, env: &HashMap<String,i32>, l: &mut i32, brake: &String) -> Vec<Instr> {
    let mut instr = Vec::new();
    match e {
        Expr::Number(n) => {
            instr.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n)));
            instr.push(Instr::Shl(Val::Reg(Reg::RAX),Val::Imm(1)));
            //check_overflow(&mut instr, l, si);
        },
        Expr::Boolean(val) => {
            if *val { 
                instr.push(Instr::IMov(Val::Reg(Reg::RAX),Val::Imm(TRUE_VAL)));
            } else {
                instr.push(Instr::IMov(Val::Reg(Reg::RAX),Val::Imm(FALSE_VAL)));
            }
        }

        // Block //
        Expr::Block(es) => {
            for item in es {
                instr.extend(compile_to_instrs(item, si, env, l, brake));
            }
        },

        // Loop //
        Expr::Loop(e) => {
            // create labels
            let startloop = new_label(l, "loop");
            let endloop = new_label(l, "loopend");

            let e_is = compile_to_instrs(e, si, env, l, &endloop);
            instr.push(Instr::Label(Val::Label(startloop.clone())));
            instr.extend(e_is);
            instr.push(Instr::Jmp(Val::Label(startloop.clone())));
            instr.push(Instr::Label(Val::Label(endloop.clone())));
        },

        // Break // 
        Expr::Break(e) => {
            let e_is = compile_to_instrs(e, si, env, l, brake);
            instr.extend(e_is);
            
            if brake == ""{
                panic!("Error - break must be within a loop.")
            }
            instr.push(Instr::Jmp(Val::Label(brake.clone())));
        }
        
        // Set // 
        Expr::Set(name, val) => {
            println!("{:?}",env);
            let res = env.get(name);
            let offset = match res {
                Some(x) => x,
                None => panic!("Unbound variable identifier {name}"),
            };
            println!("OBTAINED VALUE FOR OFFSET: {offset}");
            instr.extend(compile_to_instrs(val, si, env, l, brake));
            instr.push(Instr::IMov(Val::RegOffset(Reg::RSP, *offset), Val::Reg(Reg::RAX)));
        }

        // If expression //
        Expr::If(cond, e2, e3) => {

            // evaluate expression of conditional and type check
            instr.extend(compile_to_instrs(cond, si, env, l, brake));

            // create labels
            let cond_label = new_label(l, "if");
            let end_label  = new_label(l, "endif");

            // if conditional false, jump to false branch, otherwise it must be true (number or true bool)
            instr.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::Imm(FALSE_VAL)));
            instr.push(Instr::JEqual(Val::Label(cond_label.clone())));

            // else branch
            instr.extend(compile_to_instrs(e2, si, env, l, brake));
            instr.push(Instr::Jmp(Val::Label(end_label.clone())));

            // true branch
            instr.push(Instr::Label(Val::Label(cond_label.clone())));
            instr.extend(compile_to_instrs(e3, si+1, env, l, brake));
            instr.push(Instr::Label(Val::Label(end_label.clone())));

        },

        // Uniary Operations //
        Expr::UnOp(Op1::Add1,subexpr) => {
            update_vec_unop(&mut instr, compile_to_instrs(subexpr,si,env, l, brake),
             Instr::IAdd(Val::Reg(Reg::RAX), 
             Val::Imm(1 << 1)));
             check_overflow(&mut instr, l, si);
        },
        Expr::UnOp(Op1::Sub1,subexpr) => {
            update_vec_unop(&mut instr, compile_to_instrs(subexpr,si,env, l, brake), 
            Instr::ISub(Val::Reg(Reg::RAX), 
            Val::Imm(1 << 1)));
            check_overflow(&mut instr, l, si);
        },
        Expr::UnOp(Op1::IsBool,subexpr) => {
            instr.extend(compile_to_instrs(subexpr, si, env, l, brake));
            check_bool_type_instr(&mut instr, l);
        }
        Expr::UnOp(Op1::IsNum, subexpr) => {
            instr.extend(compile_to_instrs(subexpr,si,env,l, brake));
            check_num_type_instr(&mut instr, l);
        }
        
        // Binary Operations //
        Expr::BinOp(Op2::Plus,subexpr1, subexpr2) => {
            update_vec_binop(
                &mut instr, compile_to_instrs(subexpr1,si,env, l, brake), 
                compile_to_instrs(subexpr2,si+1,env, l, brake), 
                Instr::IAdd(Val::Reg(Reg::RAX),Val::RegOffset(Reg::RSP, si*8)),
                si,
            );
            check_overflow(&mut instr, l, si);
        },
        Expr::BinOp(Op2::Minus,subexpr1, subexpr2) => {
            update_vec_binop(
                &mut instr, compile_to_instrs(subexpr2,si,env,l, brake), 
                compile_to_instrs(subexpr1,si+1,env,l, brake), 
                Instr::ISub(Val::Reg(Reg::RAX),Val::RegOffset(Reg::RSP, si*8)),
                si,
            );
            check_overflow(&mut instr, l, si);
        },
        Expr::BinOp(Op2::Times,subexpr1, subexpr2) => {
            let ops = compile_to_instrs(subexpr2,si+1,env,l, brake);
            
            update_vec_binop(
                &mut instr, compile_to_instrs(subexpr1,si,env,l, brake), 
                ops, 
                Instr::Shr(Val::Reg(Reg::RAX), Val::Imm(1)),
                si,
            );
            instr.push(Instr::IMul(Val::Reg(Reg::RAX),Val::RegOffset(Reg::RSP, si*8)));
            check_overflow(&mut instr, l, si);
        },
        Expr::BinOp(Op2::Equal, subexpr1, subexpr2) => {
            let op1 = compile_to_instrs(subexpr2,si,env,l, brake);
            let op2 = compile_to_instrs(subexpr1,si+1,env,l, brake);

            let offset = si*8;

            instr.extend(op1);
            instr.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX)));
            instr.extend(op2);

            // check that both expressions are of same type
            same_type_expr(&mut instr, l, offset);

            // compare values for equivalence
            instr.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset)));
            instr.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
            instr.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
            instr.push(Instr::Cmove(Val::Reg(Reg::RAX),Val::Reg(Reg::RBX)));
        },
        Expr::BinOp(Op2::Greater, subexpr1, subexpr2) => {
            compare_size(&mut instr, 
                compile_to_instrs(&subexpr1, si, env, l, brake), 
                compile_to_instrs(&subexpr2, si+1, env, l, brake), 
                si, l);

            // create labels
            let cond_label = new_label(l, "if");
            let end_label  = new_label(l, "endif");

            // jump to if condition if greater than
            instr.push(Instr::JGreater(Val::Label(cond_label.clone())));
            
            conditional_jmp_compare(&mut instr, end_label, cond_label);

        },
        Expr::BinOp(Op2::GreaterEqual, subexpr1, subexpr2) => {
            compare_size(&mut instr, 
                compile_to_instrs(&subexpr1, si, env, l, brake), 
                compile_to_instrs(&subexpr2, si+1, env, l, brake), 
                si, l);

            // create labels
            let cond_label = new_label(l, "if");
            let end_label  = new_label(l, "endif");

            // jump to if condition if greater than or equal
            instr.push(Instr::JGreaterEqual(Val::Label(cond_label.clone())));
            
            conditional_jmp_compare(&mut instr, end_label, cond_label);

        },
        Expr::BinOp(Op2::Less, subexpr1, subexpr2) => {
            compare_size(&mut instr, 
                compile_to_instrs(&subexpr1, si, env, l, brake), 
                compile_to_instrs(&subexpr2, si+1, env, l, brake), 
                si, l);

            // create labels
            let cond_label = new_label(l, "if");
            let end_label  = new_label(l, "endif");

            // jump to if condition if less than
            instr.push(Instr::JLess(Val::Label(cond_label.clone())));
            
            conditional_jmp_compare(&mut instr, end_label, cond_label);

        },
        Expr::BinOp(Op2::LessEqual, subexpr1, subexpr2) => {
            compare_size(&mut instr, 
                compile_to_instrs(&subexpr1, si, env, l, brake), 
                compile_to_instrs(&subexpr2, si+1, env, l, brake), 
                si, l);

            // create labels
            let cond_label = new_label(l, "if");
            let end_label  = new_label(l, "endif");

            // jump to if condition if less than or equal
            instr.push(Instr::JLessEqual(Val::Label(cond_label.clone())));
            
            conditional_jmp_compare(&mut instr, end_label, cond_label);

        },


        Expr::Let(vec,body) => {
            let mut nenv = env.clone();
            let mut scope_keys:std::collections::HashSet<String> = std::collections::HashSet::new();
            for item in vec {
                let key = item.0.clone();
                if scope_keys.contains(&key) {
                    panic!("Error - Duplicate binding.")
                } else {
                    scope_keys.insert(key.clone());
                }
                instr.extend(compile_to_instrs(&item.1, si, &nenv, l, brake));
                nenv = nenv.update(key, si*8); 
                instr.push(Instr::IMov(Val::RegOffset(Reg::RSP, si*8), Val::Reg(Reg::RAX)));
                si = si + 1;
            }
            instr.extend(compile_to_instrs(body, si+1, &nenv, l, brake));
        },
        Expr::Id(s) => {if s == "input" {        
            instr.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI)));
            check_overflow(&mut instr, l, si);
        }
        else {
            let output = env.get(s);
            match output {
                Option::Some(x) => instr.push(Instr::IMov(Val::Reg(Reg::RAX),Val::RegOffset(Reg::RSP, *x))),
                Option::None => {
                    let s_out = &s[..];
                    panic!("Error - Unbound variable identifier {}",s_out);
                }
           }
        }},
        _ => panic!("UNFINSHED!!")
    }
    instr
}


fn conditional_jmp_compare(instr: &mut Vec<Instr>, end_label: String, cond_label: String){
    // condition not met, set RAX to false
    instr.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(FALSE_VAL)));
    instr.push(Instr::Jmp(Val::Label(end_label.clone())));

    // condition met, set RAX to true
    instr.push(Instr::Label(Val::Label(cond_label.clone())));
    instr.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(TRUE_VAL)));        
    instr.push(Instr::Label(Val::Label(end_label.clone())));
}

fn compare_size(vec: &mut Vec<Instr>, e1: Vec<Instr>, e2: Vec<Instr>, si: i32, l: &mut i32){
    // compute expression1, save to memory
    vec.extend(e1);
    vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, si*8), Val::Reg(Reg::RAX)));

    // compute expression2 
    vec.extend(e2);

    // confirm that they are the same type 
    same_type_expr(vec, l, si *8);

    // confirm that of type number
    type_number_check(vec);

    // compare values
    vec.push(Instr::Cmp(Val::RegOffset(Reg::RSP, si*8), Val::Reg(Reg::RAX)));    

}

fn type_number_check(vec: &mut Vec<Instr>){
    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(INVALID_ARGUMENT_ERROR_CODE)));
    vec.push(Instr::Test(Val::Reg(Reg::RAX),Val::Imm(1)));
    vec.push(Instr::JNotEqual(Val::Label(String::from("throw_error"))));
}

fn type_bool_check(vec: &mut Vec<Instr>){
    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(INVALID_ARGUMENT_ERROR_CODE)));
    vec.push(Instr::Test(Val::Reg(Reg::RAX),Val::Imm(1)));
    vec.push(Instr::JEqual(Val::Label(String::from("throw_error"))));
}

fn update_vec_binop(vec: &mut Vec<Instr>, append1: Vec<Instr>, append2: Vec<Instr>, append3: Instr, si: i32) {
    vec.extend(append1);
    let stack_offset = si * 8;

    // confirm that value is a number 
    type_number_check(vec);

    vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset),Val::Reg(Reg::RAX)));
    vec.extend(append2);

    // confirm that value is a number 
    type_number_check(vec);
    vec.push(append3);
}

fn update_vec_unop(vec: &mut Vec<Instr>, append1: Vec<Instr>, append2: Instr) {
    vec.extend(append1);
    type_number_check(vec);
    vec.push(append2);
}

fn check_overflow(instr: &mut Vec<Instr>, l: &mut i32, si: i32){
    // create labels
    let cond_label = new_label(l, "if");
    let end_label  = new_label(l, "endif");

    // check if overflow flag set; jump to cond_label if set
    instr.push(Instr::JCarry(Val::Label(cond_label.clone())));

    // operation did not overlow -> reload RAX to original state from memory and jmp to endif
    instr.push(Instr::Jmp(Val::Label(end_label.clone())));

    // operation overflowed -> report error
    instr.push(Instr::Label(Val::Label(cond_label.clone())));
    instr.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(OVERFLOW_ERROR_CODE)));
    instr.push(Instr::Jmp(Val::Label(format!("throw_error"))));
    instr.push(Instr::Label(Val::Label(end_label.clone())));


}

fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(val_a, val_b) => format!("\nmov {}, {}", val_to_str(val_a), val_to_str(val_b)),
        Instr::IAdd(val_a, val_b) => format!("\nadd {}, {}", val_to_str(val_a), val_to_str(val_b)),
        Instr::ISub(val_a, val_b) => format!("\nsub {}, {}", val_to_str(val_a), val_to_str(val_b)),
        Instr::IMul(val_a, val_b) => format!("\nimul {}, {}", val_to_str(val_a), val_to_str(val_b)),
        Instr::Shr(val_a, val_b) => format!("\nshr {},{}", val_to_str(val_a), val_to_str(val_b)),
        Instr::Shl(val_a,val_b) => format!("\nshl {},{}",val_to_str(val_a),val_to_str(val_b)),
        Instr::Cmp(val_a, val_b) => format!("\ncmp {},{}", val_to_str(val_a), val_to_str(val_b)),
        Instr::JEqual(val_a) => format!("\nje {}", val_to_str(val_a)),
        Instr::Jmp(val_a) => format!("\njmp {}", val_to_str(val_a)),
        Instr::JNotEqual(val_a) => format!("\njne {}", val_to_str(val_a)),
        Instr::JGreater(val_a) => format!("\njg {}", val_to_str(val_a)),
        Instr::JGreaterEqual(val_a) => format!("\njge {}", val_to_str(val_a)),
        Instr::JLess(val_a) => format!("\njl {}", val_to_str(val_a)),
        Instr::JLessEqual(val_a) => format!("\njle {}", val_to_str(val_a)),
        Instr::Test(val_a, val_b) => format!("\ntest {},{}", val_to_str(val_a), val_to_str(val_b)),
        Instr::Label(val_a) => format!("\n{}:",val_to_str(val_a)),
        Instr::JCarry(val_a) => format!("\njo {}",val_to_str(val_a)),
        Instr::Xor(val_a,val_b) => format!("\nxor {},{}",val_to_str(val_a),val_to_str(val_b)),
        Instr::Cmove(val_a,val_b) => format!("\ncmove {},{}",val_to_str(val_a),val_to_str(val_b)),

    }
}

fn val_to_str(v: &Val) -> String {
    match v {
        Val::Reg(Reg::RAX) => String::from("rax"),
        Val::Reg(Reg::RBX) => String::from("rbx"),
        Val::Reg(Reg::RSP) => String::from("rsp"),
        Val::Reg(Reg::RDI) => String::from("rdi"),
        Val::Imm(n) => n.to_string(),
        Val::RegOffset(Reg::RSP,n) => format!("[rsp-{}]",n),
        Val::Label(str_val) => format!("{}",str_val),
        _ => panic!("TODO val_to_str")
    }
}

fn check_bool_type_instr(instr: &mut Vec<Instr>, l: &mut i32){
    // Create labels
    let cond_label = new_label(l, "if");
    let end_label  = new_label(l, "endif");

    // bitwise and with RAX register and 1 to determine if LSB is 0
    // LSB of 0 denotes that the value is a number -> jmp if not zero...i.e. true if bool
    instr.push(Instr::Test(Val::Reg(Reg::RAX),Val::Imm(1)));
    instr.push(Instr::JNotEqual(Val::Label(cond_label.clone())));

    // else branch --> value is not a bool
    instr.push(Instr::IMov(Val::Reg(Reg::RAX),Val::Imm(FALSE_VAL)));
    instr.push(Instr::Jmp(Val::Label(end_label.clone())));

    // if true branch --> value is a bool
    instr.push(Instr::Label(Val::Label(cond_label.clone())));
    instr.push(Instr::IMov(Val::Reg(Reg::RAX),Val::Imm(TRUE_VAL)));
    instr.push(Instr::Label(Val::Label(end_label.clone())))
}

fn check_num_type_instr(instr: &mut Vec<Instr>, l: &mut i32){
    // Create labels
    let cond_label = new_label(l, "if");
    let end_label  = new_label(l, "endif");

    // bitwise and with RAX register and 1 to determine if LSB is 0
    // LSB of 0 denotes that the value is a number -> jmp if not zero...i.e. true if bool
    instr.push(Instr::Test(Val::Reg(Reg::RAX),Val::Imm(1)));
    instr.push(Instr::JNotEqual(Val::Label(cond_label.clone())));
    
    // else branch --> value is a number 
    instr.push(Instr::IMov(Val::Reg(Reg::RAX),Val::Imm(TRUE_VAL)));
    instr.push(Instr::Jmp(Val::Label(end_label.clone())));

    // if true branch --> value is not a number
    instr.push(Instr::Label(Val::Label(cond_label.clone())));
    instr.push(Instr::IMov(Val::Reg(Reg::RAX),Val::Imm(FALSE_VAL)));
    instr.push(Instr::Label(Val::Label(end_label.clone())))
}

fn same_type_expr(instr: &mut Vec<Instr>, l: &mut i32, offset:i32){
    instr.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
    instr.push(Instr::Xor(Val::Reg(Reg::RBX),Val::RegOffset(Reg::RSP, offset)));

    instr.push(Instr::Test(Val::Reg(Reg::RBX), Val::Imm(1)));

    // incase we need to throw error, move proper error code into RBX register
    instr.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(INVALID_ARGUMENT_ERROR_CODE)));
    instr.push(Instr::JNotEqual(Val::Label(String::from("throw_error"))));
}

fn compile(e: &Expr, si: i32, env: &HashMap<String,i32>, l: &mut i32, brake: &String) -> String {
    let instr_vec = compile_to_instrs(e,si,env, l, brake);
    let mut output = String::new();

    for entry in &instr_vec {
        output = [output, instr_to_str(entry)].join("")
    }
    output
}


