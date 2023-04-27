use std::alloc::GlobalAlloc;
use std::fs::File;
use std::io::prelude::*;
use std::{cmp, env};

use sexp::Atom::*;
use sexp::*;

use std::collections::HashMap;

#[derive(Debug, Copy, Clone)]
enum Val {
    Reg(Reg),
    Imm(i32),
    RegOffset(Reg, i32),
}

#[derive(Debug, Copy, Clone)]
enum Reg {
    RAX,
    RSP,
    RDI,
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    ITestNumOrBool(Val, Val, Val),
    ICmpNum(Val, Val, Val),
    ICmp(Val, Val),
    IJmpEqual(String),
    IJmp(String),
    ILabel(String),
    ITypeError,
    ICheckOverflow,
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
        Sexp::Atom(I(n)) => {
            // need to check overflow
            Expr::Number(i32::try_from(*n).unwrap_or_else(|_| panic!("Invalid integer value")))
        }
        Sexp::Atom(S(n)) => {
            if n == "true" {
                Expr::Boolean(true)
            } else if n == "false" {
                Expr::Boolean(false)
            } else {
                Expr::Id(n.clone())
            }
        }
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(ex)), e] if ex == "loop" => Expr::Loop(Box::new(parse_expr(e))),
            [Sexp::Atom(S(ex)), e] if ex == "break" => Expr::Break(Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), Sexp::List(e1), e2] if op == "let" => {
                if e1.is_empty() {
                    panic!("Invalid: let no binding");
                }
                let mut vec = Vec::new();
                for x in e1 {
                    vec.push(parse_bind(x));
                }
                Expr::Let(vec, Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(ex)), e1, e2] if ex == "set!" => {
                Expr::Set(e1.to_string(), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(ex)), cond, e1, e2] if ex == "if" => Expr::If(
                Box::new(parse_expr(cond)),
                Box::new(parse_expr(e1)),
                Box::new(parse_expr(e2)),
            ),
            [Sexp::Atom(S(ex)), es @ ..] if ex == "block" => {
                let mut exprs = Vec::new();
                for e in es {
                    exprs.push(parse_expr(e));
                }
                Expr::Block(exprs)
            }

            [Sexp::Atom(S(op1)), e] if op1 == "add1" => {
                Expr::UnOp(Op1::Add1, Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op1)), e] if op1 == "sub1" => {
                Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op1)), e] if op1 == "isnum" => {
                Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op1)), e] if op1 == "isbool" => {
                Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e)))
            }

            [Sexp::Atom(S(op2)), e1, e2] if op2 == "+" => Expr::BinOp(
                Op2::Plus,
                Box::new(parse_expr(e1)),
                Box::new(parse_expr(e2)),
            ),
            [Sexp::Atom(S(op2)), e1, e2] if op2 == "-" => Expr::BinOp(
                Op2::Minus,
                Box::new(parse_expr(e1)),
                Box::new(parse_expr(e2)),
            ),
            [Sexp::Atom(S(op2)), e1, e2] if op2 == "*" => Expr::BinOp(
                Op2::Times,
                Box::new(parse_expr(e1)),
                Box::new(parse_expr(e2)),
            ),
            [Sexp::Atom(S(op2)), e1, e2] if op2 == "=" => Expr::BinOp(
                Op2::Equal,
                Box::new(parse_expr(e1)),
                Box::new(parse_expr(e2)),
            ),
            [Sexp::Atom(S(op2)), e1, e2] if op2 == ">" => Expr::BinOp(
                Op2::Greater,
                Box::new(parse_expr(e1)),
                Box::new(parse_expr(e2)),
            ),
            [Sexp::Atom(S(op2)), e1, e2] if op2 == ">=" => Expr::BinOp(
                Op2::GreaterEqual,
                Box::new(parse_expr(e1)),
                Box::new(parse_expr(e2)),
            ),
            [Sexp::Atom(S(op2)), e1, e2] if op2 == "<" => Expr::BinOp(
                Op2::Less,
                Box::new(parse_expr(e1)),
                Box::new(parse_expr(e2)),
            ),
            [Sexp::Atom(S(op2)), e1, e2] if op2 == "<=" => Expr::BinOp(
                Op2::LessEqual,
                Box::new(parse_expr(e1)),
                Box::new(parse_expr(e2)),
            ),
            _ => panic!("Invalid: does not match to any operation"),
        },
        _ => panic!("Invalid: does not match to any Sexp"),
    }
}

fn parse_bind(s: &Sexp) -> (String, Expr) {
    match s {
        Sexp::List(vec) if vec.len() == 2 => {
            let name = match &vec[0] {
                Sexp::Atom(S(n)) => {
                    if is_keywords(n.to_string()) {
                        panic!("Invalid: using keyword as id");
                    }
                    n.to_string()
                }
                _ => panic!("Invalid: expected identifier"),
            };
            let value = parse_expr(&vec[1]);
            (name, value)
        }
        _ => panic!("Invalid: expected let-binding"),
    }
}

fn compile_expr(
    e: &Expr,
    stack_index: &mut i32,
    env: &mut HashMap<String, Val>,
    target: String,
) -> Vec<Instr> {
    match e {
        Expr::Number(n) => match n.checked_shl(1) {
            Some(result) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(result))],
            None => panic!("Invalid: overflow number"),
        },
        Expr::Boolean(b) => {
            let val = if *b { 3 } else { 1 };
            vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(val))]
        }
        Expr::Id(id) => {
            if *id == "input" {
                vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI))]
            } else {
                match env.get(id) {
                    Some(val) => vec![Instr::IMov(Val::Reg(Reg::RAX), *val)],
                    None => panic!("Unbound variable identifier {}", id),
                }
            }
        }
        Expr::Let(bindings, body) => {
            let mut instrs = Vec::new();
            let mut new_env = env.clone();

            let mut list = Vec::new();
            let mut count = 0;

            for (id, e) in bindings {
                if list.contains(id) {
                    panic!("Duplicate binding for {}", id);
                }

                list.push(id.to_string());
                count += 1;

                // compute the binding expression
                let bind_instrs = compile_expr(e, stack_index, &mut new_env, target.clone());
                instrs.extend(bind_instrs);

                instrs.push(Instr::IMov(
                    Val::RegOffset(Reg::RSP, *stack_index),
                    Val::Reg(Reg::RAX),
                ));
                new_env.insert(id.clone(), Val::RegOffset(Reg::RSP, *stack_index));
                *stack_index += 8;
            }

            // evaluate body
            let body_instrs = compile_expr(body, stack_index, &mut new_env, target.clone());
            instrs.extend(body_instrs);
            *stack_index -= count * 8;

            instrs
        }

        Expr::UnOp(op, e) => {
            let mut instrs = compile_expr(e, stack_index, env, target.clone());
            match op {
                Op1::Add1 => {
                    instrs.push(Instr::IMov(
                        Val::RegOffset(Reg::RSP, *stack_index),
                        Val::Reg(Reg::RAX),
                    ));
                    *stack_index += 8;
                    let mut type_check_instrs =
                        check_is_num(Val::RegOffset(Reg::RSP, *stack_index - 8));
                    instrs.append(&mut type_check_instrs);
                    instrs.push(Instr::IMov(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, *stack_index - 8),
                    ));
                    *stack_index -= 8;

                    instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(2)));
                    instrs.push(Instr::ICheckOverflow)
                }
                Op1::Sub1 => {
                    instrs.push(Instr::IMov(
                        Val::RegOffset(Reg::RSP, *stack_index),
                        Val::Reg(Reg::RAX),
                    ));
                    *stack_index += 8;
                    let mut type_check_instrs =
                        check_is_num(Val::RegOffset(Reg::RSP, *stack_index - 8));
                    instrs.append(&mut type_check_instrs);
                    instrs.push(Instr::IMov(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, *stack_index - 8),
                    ));
                    *stack_index -= 8;
                    instrs.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(2)));
                    instrs.push(Instr::ICheckOverflow)
                }
                Op1::IsNum => instrs.push(Instr::ITestNumOrBool(
                    Val::Reg(Reg::RAX),
                    Val::Imm(3),
                    Val::Imm(1),
                )),
                Op1::IsBool => instrs.push(Instr::ITestNumOrBool(
                    Val::Reg(Reg::RAX),
                    Val::Imm(1),
                    Val::Imm(3),
                )),
            }
            instrs
        }
        Expr::BinOp(op, e1, e2) => {
            let mut instrs = Vec::new();

            // evaluate first expression, store it into rsp offset
            let mut instrs1 = compile_expr(e1, stack_index, env, target.clone());
            instrs.append(&mut instrs1);
            instrs.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, *stack_index),
                Val::Reg(Reg::RAX),
            ));
            *stack_index += 8;

            // evaluate second expression, store it into rsp offset
            let mut instrs2 = compile_expr(e2, stack_index, env, target.clone());
            instrs.append(&mut instrs2);
            instrs.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, *stack_index),
                Val::Reg(Reg::RAX),
            ));
            *stack_index += 8;

            // check if both expressions evaluate to the same type, throw error if they are not
            let mut type_check_instrs = check_both_type(
                Val::RegOffset(Reg::RSP, *stack_index - 16),
                Val::RegOffset(Reg::RSP, *stack_index - 8),
            );
            instrs.append(&mut type_check_instrs);

            // move the first expression into rax
            instrs.push(Instr::IMov(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RSP, *stack_index - 16),
            ));

            // if operation is equal, do the equal operation
            match op {
                Op2::Equal => {
                    instrs.push(Instr::ICmpNum(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, *stack_index - 8),
                        Val::Imm(1),
                    ));
                    *stack_index -= 16;
                    return instrs;
                }
                _ => (),
            }

            // if operation is not equal, check if the first expression is a number, throw an error if it is not
            let mut type_check_instrs = check_is_num(Val::RegOffset(Reg::RSP, *stack_index - 16));
            instrs.append(&mut type_check_instrs);

            // move the first expression into rax
            instrs.push(Instr::IMov(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RSP, *stack_index - 16),
            ));
            match op {
                Op2::Plus => {
                    instrs.push(Instr::IAdd(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, *stack_index - 8),
                    ));
                    instrs.push(Instr::ICheckOverflow)
                }
                Op2::Minus => {
                    instrs.push(Instr::ISub(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, *stack_index - 8),
                    ));
                    instrs.push(Instr::ICheckOverflow)
                }
                Op2::Times => {
                    instrs.push(Instr::IMul(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, *stack_index - 8),
                    ));
                    instrs.push(Instr::ICheckOverflow)
                }
                Op2::Greater => instrs.push(Instr::ICmpNum(
                    Val::Reg(Reg::RAX),
                    Val::RegOffset(Reg::RSP, *stack_index - 8),
                    Val::Imm(2),
                )),
                Op2::GreaterEqual => instrs.push(Instr::ICmpNum(
                    Val::Reg(Reg::RAX),
                    Val::RegOffset(Reg::RSP, *stack_index - 8),
                    Val::Imm(3),
                )),
                Op2::Less => instrs.push(Instr::ICmpNum(
                    Val::Reg(Reg::RAX),
                    Val::RegOffset(Reg::RSP, *stack_index - 8),
                    Val::Imm(4),
                )),
                Op2::LessEqual => instrs.push(Instr::ICmpNum(
                    Val::Reg(Reg::RAX),
                    Val::RegOffset(Reg::RSP, *stack_index - 8),
                    Val::Imm(5),
                )),
                Op2::Equal => panic!("Invalid: should not get to this second match equal"),
            }
            *stack_index -= 16;
            instrs
        }
        Expr::If(cond, e1, e2) => unsafe {
            let mut instrs = Vec::new();
            let flag_index = GLOBAL_FLAG_INDEX;
            GLOBAL_FLAG_INDEX += 1;

            // evaluate condition
            let mut condition = compile_expr(cond, stack_index, env, target.clone());
            instrs.append(&mut condition);

            // jump to if_false if the result is equal to false
            instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(1)));
            instrs.push(Instr::IJmpEqual(format!("if_false_{}", flag_index)));

            // evaluate the if_true instructions, then jump to end_if
            let mut true_instrs = compile_expr(e1, stack_index, env, target.clone());
            instrs.append(&mut true_instrs);
            instrs.push(Instr::IJmp(format!("end_if_{}", flag_index)));

            // evaluate the if_false instructions
            instrs.push(Instr::ILabel(format!("if_false_{}", flag_index)));
            let mut false_instrs = compile_expr(e2, stack_index, env, target.clone());
            instrs.append(&mut false_instrs);

            // end of if statement
            instrs.push(Instr::ILabel(format!("end_if_{}", flag_index)));

            instrs
        },
        Expr::Loop(e) => unsafe {
            let mut instrs = Vec::new();
            let flag_index = GLOBAL_FLAG_INDEX;
            GLOBAL_FLAG_INDEX += 1;

            let start_loop_label = format!("start_loop_{}", flag_index);
            let end_loop_label = format!("end_loop_{}", flag_index);

            // start loop
            instrs.push(Instr::ILabel(start_loop_label.clone()));

            // evaluate loop body
            let mut body = compile_expr(e, stack_index, env, end_loop_label.clone());
            instrs.append(&mut body);

            // jump to the start of loop
            instrs.push(Instr::IJmp(start_loop_label.clone()));

            // if there is a break operation inside loop, it will jump to the end of loop
            instrs.push(Instr::ILabel(end_loop_label.clone()));

            instrs
        },
        Expr::Break(e) => {
            // using break outside a loop
            if target == "" {
                panic!("Invalid: break outside of loop");
            }

            let mut instrs = Vec::new();

            // evaluate break expression
            let mut expression = compile_expr(e, stack_index, env, "".to_string());
            instrs.append(&mut expression);

            // jump to target, break the loop
            instrs.push(Instr::IJmp(target.clone()));

            instrs
        }
        Expr::Set(id, e) => {
            let mut instrs = Vec::new();

            // evaluate set expression
            let mut expression = compile_expr(e, stack_index, env, target.clone());
            instrs.append(&mut expression);

            match env.get(id) {
                Some(val) => instrs.push(Instr::IMov(*val, Val::Reg(Reg::RAX))),
                None => panic!("Unbound variable identifier {}", id),
            }

            instrs
        }

        Expr::Block(es) => {
            let mut instrs = Vec::new();

            // evaluate each expressions
            for e in es {
                let mut expression = compile_expr(e, stack_index, env, target.clone());
                instrs.append(&mut expression);
            }

            instrs
        }
    }
}

fn compile(e: &Expr) -> String {
    let mut stack_index = 16;
    let instrs = compile_expr(e, &mut stack_index, &mut HashMap::new(), "".to_string());
    instrs
        .iter()
        .map(|instr| instr_to_str(instr))
        .collect::<Vec<String>>()
        .join("\n")
}

fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(dst, src) => format!("mov {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IAdd(dst, src) => format!("add {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::ISub(dst, src) => format!("sub {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IMul(dst, src) => format!(
            "shr {}, 1
imul {}, {}",
            val_to_str(dst),
            val_to_str(dst),
            val_to_str(src)
        ),

        Instr::ITestNumOrBool(dst, num, bool) => unsafe {
            let flag_index = GLOBAL_FLAG_INDEX;
            GLOBAL_FLAG_INDEX += 1;
            format!(
                "mov rax, {}
test rax, 1
jz num_{}
mov rax, {}
jmp done_{}
num_{}:
mov rax, {}
done_{}:",
                val_to_str(dst),
                flag_index,
                val_to_str(bool),
                flag_index,
                flag_index,
                val_to_str(num),
                flag_index
            )
        },
        Instr::ICmp(dst, src) => format!("cmp {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::ICmpNum(dst, src, cmp_type) => unsafe {
            let mut jmp = val_to_str(cmp_type);
            if jmp == "1" {
                jmp = "je".to_string();
            } else if jmp == "2" {
                jmp = "jg".to_string();
            } else if jmp == "3" {
                jmp = "jge".to_string();
            } else if jmp == "4" {
                jmp = "jl".to_string();
            } else if jmp == "5" {
                jmp = "jle".to_string();
            }
            let flag_index = GLOBAL_FLAG_INDEX;
            GLOBAL_FLAG_INDEX += 1;
            format!(
                "cmp {}, {}
{} true_{}
mov rax, 1
jmp done_{}
true_{}:
mov rax, 3
done_{}:",
                val_to_str(dst),
                val_to_str(src),
                jmp,
                flag_index,
                flag_index,
                flag_index,
                flag_index,
            )
        },

        Instr::IJmp(label) => format!("jmp {}", label),
        Instr::IJmpEqual(label) => format!("je {}", label),
        Instr::ILabel(name) => format!("{}:", name),
        Instr::ITypeError => format!("jmp type_error"),
        Instr::ICheckOverflow => format!("jo overflow_error"),
    }
}

fn val_to_str(v: &Val) -> String {
    fn reg_to_str(reg: &Reg) -> String {
        match reg {
            Reg::RAX => "rax".to_string(),
            Reg::RSP => "rsp".to_string(),
            Reg::RDI => "rdi".to_string(),
        }
    }

    match v {
        Val::Reg(reg) => reg_to_str(reg),
        Val::Imm(n) => n.to_string(),
        Val::RegOffset(reg, offset) => format!("[{} - {}]", reg_to_str(reg), offset),
    }
}

fn is_keywords(s: String) -> bool {
    let keywords = vec![
        "if", "loop", "break", "set!", "block", "true", "false", "+", "-", "*", "input",
    ];
    for keyword in keywords {
        if s == *keyword {
            return true;
        }
    }
    return false;
}

fn check_is_num(val: Val) -> Vec<Instr> {
    unsafe {
        let mut instrs = Vec::new();
        let flag_index = GLOBAL_FLAG_INDEX;
        GLOBAL_FLAG_INDEX += 1;

        // check if it is a number
        instrs.push(Instr::ITestNumOrBool(val, Val::Imm(3), Val::Imm(1)));
        instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(3)));

        // jump to is_num if it is a number
        instrs.push(Instr::IJmpEqual(format!("is_num_{}", flag_index)));

        // it is not a number, throw a type error
        instrs.push(Instr::ITypeError);

        // it is a number, put the value from rsp offset to where it belonged
        instrs.push(Instr::ILabel(format!("is_num_{}", flag_index)));

        instrs
    }
}

fn check_both_type(val1: Val, val2: Val) -> Vec<Instr> {
    unsafe {
        let mut instrs = Vec::new();

        let flag_index = GLOBAL_FLAG_INDEX;
        GLOBAL_FLAG_INDEX += 1;

        // check if the first value is a boolean
        instrs.push(Instr::ITestNumOrBool(val1, Val::Imm(3), Val::Imm(1)));
        instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(1)));

        // first value is a boolean, jump to is_boolean
        instrs.push(Instr::IJmpEqual(format!("is_boolean_{}", flag_index)));

        // first value is a number, check second value
        instrs.push(Instr::ITestNumOrBool(val2, Val::Imm(3), Val::Imm(1)));
        instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(1)));

        // second value is boolean, jump to error
        instrs.push(Instr::IJmpEqual(format!("not_same_type_{}", flag_index)));

        // second value is number, jump to same_type
        instrs.push(Instr::IJmp(format!("same_type_{}", flag_index)));

        // first value is a boolean, check second value
        instrs.push(Instr::ILabel(format!("is_boolean_{}", flag_index)));
        instrs.push(Instr::ITestNumOrBool(val2, Val::Imm(3), Val::Imm(1)));
        instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(1)));

        // second value is boolean, jump to same_type
        instrs.push(Instr::IJmpEqual(format!("same_type_{}", flag_index)));

        // second value is number, throw an error
        instrs.push(Instr::ILabel(format!("not_same_type_{}", flag_index)));

        // throw a type error
        instrs.push(Instr::ITypeError);

        // end of check
        instrs.push(Instr::ILabel(format!("same_type_{}", flag_index)));

        instrs
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let expr = parse_expr(&parse(&in_contents).unwrap_or_else(|_| panic!("Invalid expression")));

    let result = compile(&expr);

    let asm_program = format!(
        "section .text
global our_code_starts_here
our_code_starts_here:
{}
jmp end_error

type_error:
push 1
mov rdi, 1
call snek_error
jmp end_error

overflow_error:
push 1
mov rdi, 2
call snek_error
jmp end_error

end_error:
ret

extern snek_error
",
        result
    );

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}

static mut GLOBAL_FLAG_INDEX: i32 = 0;
