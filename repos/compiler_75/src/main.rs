use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::Atom::*;
use sexp::*;

use im::HashMap;
use im::HashSet;

#[derive(Debug)]
enum Val {
    Reg(Reg),
    Imm(i64),
    RegOffset(Reg, i64),
    SymLabel(String),
}

#[derive(Debug)] 
#[derive(Clone)]
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
    IXor(Val, Val),
    IAnd(Val, Val),
    IPush(Val),
    ICall(Val),
    ICmp(Val, Val),
    IJmp(Val),
    IJe(Val),
    IJno(Val),
    ILabel(Val),
    ICmove(Val, Val),
    ICmovg(Val, Val),
    ICmovl(Val, Val),
    ICmovge(Val, Val),
    ICmovle(Val, Val),
    ISar(Val, Val),
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
    Less,
    GreaterEqual,
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
    Input(),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Set(String, Box<Expr>),
    Block(Vec<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
}

fn label_string(l: &mut i64, s: &str) -> String {
    let crt = *l;
    *l += 1;
    format!("{s}_{crt}")
}

fn parse_bind(s: &Sexp) -> (String, Expr) {
    match s {
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(id_name)), id_sexp] => {
                    // panic at invalid id name: conflict with keywords
                    let keywords = vec![
                        "let".to_string(),
                        "add1".to_string(),
                        "sub1".to_string(),
                        "+".to_string(),
                        "-".to_string(),
                        "*".to_string(),
                        "".to_string(),
                        "true".to_string(),
                        "false".to_string(),
                        "input".to_string(),
                        "if".to_string(),
                        "block".to_string(),
                        "loop".to_string(),
                        "break".to_string(),
                        ];
                    if keywords.contains(&id_name) {
                        panic!("keyword");
                    }

                    // panic at invalid id name: start with non-letter
                    if !id_name.as_bytes()[0].is_ascii_alphabetic() {
                        panic!("Invalid");
                    }

                    // panic at invalid id name: contains more than alphanumeric characters, hyphens, and underscores
                    for c in id_name.chars() {
                        if !(c.is_alphanumeric() || (c == '_') || (c == '-')) {
                            panic!("Invalid");
                        }
                    }

                    (id_name.clone(), parse_expr(id_sexp))
                },
                _ => panic!("Invalid"),
            }
        }
        _ => panic!("Invalid"),
    }
}

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => {
            match i64::try_from(*n) {
                Ok(res) if (res <= 4611686018427387903) && (res >= -4611686018427387904) => { Expr::Number(res) },
                _ => {
                    panic!("Invalid")
                }
            }
        }, 
        // Sexp::Atom(F(n)) => Expr::Number(f64::try_from(*n).unwrap()),
        Sexp::Atom(S(truth_value)) if truth_value == "true" => Expr::Boolean(true),
        Sexp::Atom(S(truth_value)) if truth_value == "false" => Expr::Boolean(false),
        Sexp::Atom(S(input_str)) if input_str == "input" => Expr::Input(),
        Sexp::Atom(S(id_name)) => Expr::Id(id_name.clone()),
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(op)), e] if op == "add1" => {
                    Expr::UnOp(Op1::Add1, Box::new(parse_expr(e)))
                },
                [Sexp::Atom(S(op)), e] if op == "sub1" => {
                    Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e)))
                },
                [Sexp::Atom(S(op)), e] if op == "isbool" => {
                    Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e)))
                },
                [Sexp::Atom(S(op)), e] if op == "isnum" => {
                    Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e)))
                },
                [Sexp::Atom(S(op)), e1, e2] if op == "+" => {
                    Expr::BinOp(Op2::Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
                },
                [Sexp::Atom(S(op)), e1, e2] if op == "-" => {
                    Expr::BinOp(Op2::Minus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
                },
                [Sexp::Atom(S(op)), e1, e2] if op == "*" => {
                    Expr::BinOp(Op2::Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
                },
                [Sexp::Atom(S(op)), e1, e2] if op == "=" => {
                    Expr::BinOp(Op2::Equal, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
                },
                [Sexp::Atom(S(op)), e1, e2] if op == ">" => {
                    Expr::BinOp(Op2::Greater, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
                },
                [Sexp::Atom(S(op)), e1, e2] if op == "<" => {
                    Expr::BinOp(Op2::Less, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
                },
                [Sexp::Atom(S(op)), e1, e2] if op == ">=" => {
                    Expr::BinOp(Op2::GreaterEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
                },
                [Sexp::Atom(S(op)), e1, e2] if op == "<=" => {
                    Expr::BinOp(Op2::LessEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
                },
                [Sexp::Atom(S(op)), Sexp::List(e1), e2] if op == "let" => {
                    // panic at no id name
                    if e1.len() == 0 {
                        panic!("Invalid");
                    }

                    // collect all binding pairs to bind_vec
                    let mut bind_vec = Vec::new();
                    for id_vec in &e1[..] {
                        let bind = parse_bind(id_vec);
                        bind_vec.push((bind.0, bind.1));
                    }

                    Expr::Let(bind_vec, Box::new(parse_expr(e2)))
                },
                [Sexp::Atom(S(op)), e1, e2, e3] if op == "if" => {
                    Expr::If(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)), Box::new(parse_expr(e3)))
                },
                [Sexp::Atom(S(op)), Sexp::Atom(S(id_name)), e1] if op == "set!" => {
                    Expr::Set(id_name.to_string(), Box::new(parse_expr(e1)))
                },
                [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => {
                    let mut instr_vec = Vec::new();
                    for e in exprs.into_iter() {
                        instr_vec.push(parse_expr(e));
                    }
                    
                    // Expr::Block(exprs.into_iter().map(parse_expr).collect())
                    Expr::Block(instr_vec)
                },
                [Sexp::Atom(S(op)), e1] if op == "loop" => {
                    Expr::Loop(Box::new(parse_expr(e1)))
                },
                [Sexp::Atom(S(op)), e1] if op == "break" => {
                    Expr::Break(Box::new(parse_expr(e1)))
                },
                _ => panic!("Invalid"),
            }
        },
        _ => panic!("Invalid"),
    }
}

// check if rax is bool, report error if it is
fn assure_unbool(vec_instr: &mut Vec<Instr>, si: i64, tag_cnt: &mut i64) {
    let stack_offset = si * 8;

    // backup rax
    vec_instr.push(Instr::IMov(Val::RegOffset(Reg::RSP, -stack_offset), Val::Reg(Reg::RAX)));
    
    // check if rax is bool or not
    vec_instr.push(Instr::IAnd(Val::Reg(Reg::RAX), Val::Imm(1)));
    vec_instr.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(0)));
    let skip_err_labstr = label_string(tag_cnt, "skip_err");
    vec_instr.push(Instr::IJe(Val::SymLabel(skip_err_labstr.clone())));

    // if rax is bool, report error 
    vec_instr.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(1)));
    vec_instr.push(Instr::IPush(Val::Reg(Reg::RSP)));
    vec_instr.push(Instr::ICall(Val::SymLabel("snek_error".to_string())));

    // if rax is not bool, restore rax
    vec_instr.push(Instr::ILabel(Val::SymLabel(skip_err_labstr.clone())));
    vec_instr.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, -stack_offset)));
}

// check flag to see if an overflow just happended
fn assure_not_overflow(vec_instr: &mut Vec<Instr>, tag_cnt: &mut i64) {

    // if no overflow, return
    let no_overflow_labstr = label_string(tag_cnt, "no_overflow");
    vec_instr.push(Instr::IJno(Val::SymLabel(no_overflow_labstr.clone())));

    // if overflow, report error 
    vec_instr.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(2)));
    vec_instr.push(Instr::IPush(Val::Reg(Reg::RSP)));
    vec_instr.push(Instr::ICall(Val::SymLabel("snek_error".to_string())));

    // normal return
    vec_instr.push(Instr::ILabel(Val::SymLabel(no_overflow_labstr.clone())));
}


// // helper
// fn print_type_of<T>(_: &T) {
//     println!("type of");
//     println!("{}", std::any::type_name::<T>())
// }

fn compile_to_instrs(e: &Expr, si: i64, env: &HashMap<String, i64>, tag_cnt: &mut i64, loop_exit: &String) -> Vec<Instr> {
    let mut vec_instr = Vec::new();

    match e {
        Expr::Number(n) => {
            vec_instr.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm((*n) << 1)));
            vec_instr
        },

        Expr::Boolean(true) => {
            vec_instr.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
            vec_instr
        },

        Expr::Boolean(false) => {
            vec_instr.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
            vec_instr
        },

        Expr::Input() => {
            vec_instr.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI)));
            vec_instr
        },

        Expr::UnOp(Op1::Add1, subexpr) => {
            vec_instr.extend(compile_to_instrs(subexpr, si, env, tag_cnt, loop_exit));
            assure_unbool(&mut vec_instr, si+1, tag_cnt); // check rax
            vec_instr.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(2)));
            assure_not_overflow(&mut vec_instr, tag_cnt);
            vec_instr
        },

        Expr::UnOp(Op1::Sub1, subexpr) => {
            vec_instr.extend(compile_to_instrs(subexpr, si, env, tag_cnt, loop_exit));
            assure_unbool(&mut vec_instr, si+1, tag_cnt); // check rax
            vec_instr.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(2)));
            assure_not_overflow(&mut vec_instr, tag_cnt);
            vec_instr
        },

        Expr::UnOp(Op1::IsNum, subexpr) => {
            vec_instr.extend(compile_to_instrs(subexpr, si, env, tag_cnt, loop_exit));
            vec_instr.push(Instr::IAnd(Val::Reg(Reg::RAX), Val::Imm(1)));
            vec_instr.push(Instr::IXor(Val::Reg(Reg::RAX), Val::Imm(1)));
            vec_instr.push(Instr::IMul(Val::Reg(Reg::RAX), Val::Imm(2)));
            vec_instr.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1)));
            vec_instr
        },

        Expr::UnOp(Op1::IsBool, subexpr) => {
            vec_instr.extend(compile_to_instrs(subexpr, si, env, tag_cnt, loop_exit));
            vec_instr.push(Instr::IAnd(Val::Reg(Reg::RAX), Val::Imm(1)));
            vec_instr.push(Instr::IMul(Val::Reg(Reg::RAX), Val::Imm(2)));
            vec_instr.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1)));
            vec_instr
        },

        Expr::BinOp(Op2::Plus, e1, e2) => {
            let stack_offset = si * 8;

            vec_instr.extend(compile_to_instrs(e1, si, env, tag_cnt, loop_exit));
            assure_unbool(&mut vec_instr, si+1, tag_cnt); // check rax
            vec_instr.push(Instr::IMov(Val::RegOffset(Reg::RSP, -stack_offset), Val::Reg(Reg::RAX)));

            vec_instr.extend(compile_to_instrs(e2, si + 1, env, tag_cnt, loop_exit));
            assure_unbool(&mut vec_instr, si+1, tag_cnt); // check rax
            vec_instr.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, -stack_offset)));
            assure_not_overflow(&mut vec_instr, tag_cnt);
            vec_instr
        },

        Expr::BinOp(Op2::Minus, e1, e2) => {
            let stack_offset = si * 8;

            vec_instr.extend(compile_to_instrs(e2, si, env, tag_cnt, loop_exit));
            assure_unbool(&mut vec_instr, si+1, tag_cnt); // check rax
            vec_instr.push(Instr::IMov(Val::RegOffset(Reg::RSP, -stack_offset), Val::Reg(Reg::RAX)));

            vec_instr.extend(compile_to_instrs(e1, si + 1, env, tag_cnt, loop_exit));
            assure_unbool(&mut vec_instr, si+1, tag_cnt); // check rax
            vec_instr.push(Instr::ISub(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, -stack_offset)));
            assure_not_overflow(&mut vec_instr, tag_cnt);
            vec_instr
        },

        Expr::BinOp(Op2::Times, e1, e2) => {
            let stack_offset = si * 8;

            vec_instr.extend(compile_to_instrs(e1, si, env, tag_cnt, loop_exit));
            assure_unbool(&mut vec_instr, si+1, tag_cnt); // check rax
            vec_instr.push(Instr::ISar(Val::Reg(Reg::RAX), Val::Imm(1)));
            vec_instr.push(Instr::IMov(Val::RegOffset(Reg::RSP, -stack_offset), Val::Reg(Reg::RAX)));

            vec_instr.extend(compile_to_instrs(e2, si + 1, env, tag_cnt, loop_exit));
            assure_unbool(&mut vec_instr, si+1, tag_cnt); // check rax
            vec_instr.push(Instr::ISar(Val::Reg(Reg::RAX), Val::Imm(1)));
            vec_instr.push(Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, -stack_offset)));

            vec_instr.push(Instr::IMul(Val::Reg(Reg::RAX), Val::Imm(2)));
            assure_not_overflow(&mut vec_instr, tag_cnt);
            vec_instr
        },

        Expr::BinOp(Op2::Equal, e1, e2) => {
            let stack_offset1 = si * 8;
            let stack_offset2 = si * 16;

            // execute e1, e2
            vec_instr.extend(compile_to_instrs(e1, si, env, tag_cnt, loop_exit));
            vec_instr.push(Instr::IMov(Val::RegOffset(Reg::RSP, -stack_offset1), Val::Reg(Reg::RAX)));
            vec_instr.extend(compile_to_instrs(e2, si + 1, env, tag_cnt, loop_exit));
            vec_instr.push(Instr::IMov(Val::RegOffset(Reg::RSP, -stack_offset2), Val::Reg(Reg::RAX)));

            // check type matching between e1 and e2
            let skip_err_labstr = label_string(tag_cnt, "skip_err");
            vec_instr.push(Instr::IXor(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, -stack_offset1)));
            vec_instr.push(Instr::IAnd(Val::Reg(Reg::RAX), Val::Imm(1)));
            vec_instr.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(0)));
            vec_instr.push(Instr::IJe(Val::SymLabel(skip_err_labstr.clone())));

            // report error if type mismatch
            vec_instr.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(1)));
            vec_instr.push(Instr::IPush(Val::Reg(Reg::RSP)));
            vec_instr.push(Instr::ICall(Val::SymLabel("snek_error".to_string())));

            // test: normal exec.
            vec_instr.push(Instr::ILabel(Val::SymLabel(skip_err_labstr.clone())));
            vec_instr.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, -stack_offset2)));
            vec_instr.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, -stack_offset1)));
            vec_instr.push(Instr::IMov(Val::RegOffset(Reg::RSP, -stack_offset1), Val::Reg(Reg::RBX))); // backup rbx
            vec_instr.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3))); // prepare value "true" for rax
            vec_instr.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
            vec_instr.push(Instr::ICmove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
            vec_instr.push(Instr::IMov(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, -stack_offset1))); // restore rbx

            vec_instr
        },
        
        Expr::BinOp(Op2::Greater, e1, e2) => {
            let stack_offset1 = si * 8;

            // execute e1, e2
            vec_instr.extend(compile_to_instrs(e2, si, env, tag_cnt, loop_exit));
            assure_unbool(&mut vec_instr, si+1, tag_cnt); // check rax
            vec_instr.push(Instr::IMov(Val::RegOffset(Reg::RSP, -stack_offset1), Val::Reg(Reg::RAX)));
            vec_instr.extend(compile_to_instrs(e1, si + 1, env, tag_cnt, loop_exit));
            assure_unbool(&mut vec_instr, si+1, tag_cnt); // check rax

            // compare results of e1, e2
            vec_instr.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, -stack_offset1)));
            vec_instr.push(Instr::IMov(Val::RegOffset(Reg::RSP, -stack_offset1), Val::Reg(Reg::RBX))); // backup rbx
            vec_instr.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3))); // prepare value "true" for rax
            vec_instr.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
            vec_instr.push(Instr::ICmovg(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
            vec_instr.push(Instr::IMov(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, -stack_offset1))); // restore rbx

            vec_instr
        },
        
        Expr::BinOp(Op2::Less, e1, e2) => {
            let stack_offset1 = si * 8;

            // execute e1, e2
            vec_instr.extend(compile_to_instrs(e2, si, env, tag_cnt, loop_exit));
            assure_unbool(&mut vec_instr, si+1, tag_cnt); // check rax
            vec_instr.push(Instr::IMov(Val::RegOffset(Reg::RSP, -stack_offset1), Val::Reg(Reg::RAX)));
            vec_instr.extend(compile_to_instrs(e1, si + 1, env, tag_cnt, loop_exit));
            assure_unbool(&mut vec_instr, si+1, tag_cnt); // check rax

            // compare results of e1, e2
            vec_instr.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, -stack_offset1)));
            vec_instr.push(Instr::IMov(Val::RegOffset(Reg::RSP, -stack_offset1), Val::Reg(Reg::RBX))); // backup rbx
            vec_instr.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3))); // prepare value "true" for rax
            vec_instr.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
            vec_instr.push(Instr::ICmovl(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
            vec_instr.push(Instr::IMov(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, -stack_offset1))); // restore rbx

            vec_instr
        },
        
        Expr::BinOp(Op2::GreaterEqual, e1, e2) => {
            let stack_offset1 = si * 8;

            // execute e1, e2
            vec_instr.extend(compile_to_instrs(e2, si, env, tag_cnt, loop_exit));
            assure_unbool(&mut vec_instr, si+1, tag_cnt); // check rax
            vec_instr.push(Instr::IMov(Val::RegOffset(Reg::RSP, -stack_offset1), Val::Reg(Reg::RAX)));
            vec_instr.extend(compile_to_instrs(e1, si + 1, env, tag_cnt, loop_exit));
            assure_unbool(&mut vec_instr, si+1, tag_cnt); // check rax

            // compare results of e1, e2
            vec_instr.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, -stack_offset1)));
            vec_instr.push(Instr::IMov(Val::RegOffset(Reg::RSP, -stack_offset1), Val::Reg(Reg::RBX))); // backup rbx
            vec_instr.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3))); // prepare value "true" for rax
            vec_instr.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
            vec_instr.push(Instr::ICmovge(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
            vec_instr.push(Instr::IMov(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, -stack_offset1))); // restore rbx

            vec_instr
        },
        
        Expr::BinOp(Op2::LessEqual, e1, e2) => {
            let stack_offset1 = si * 8;

            // execute e1, e2
            vec_instr.extend(compile_to_instrs(e2, si, env, tag_cnt, loop_exit));
            assure_unbool(&mut vec_instr, si+1, tag_cnt); // check rax
            vec_instr.push(Instr::IMov(Val::RegOffset(Reg::RSP, -stack_offset1), Val::Reg(Reg::RAX)));
            vec_instr.extend(compile_to_instrs(e1, si + 1, env, tag_cnt, loop_exit));
            assure_unbool(&mut vec_instr, si+1, tag_cnt); // check rax

            // compare results of e1, e2
            vec_instr.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, -stack_offset1)));
            vec_instr.push(Instr::IMov(Val::RegOffset(Reg::RSP, -stack_offset1), Val::Reg(Reg::RBX))); // backup rbx
            vec_instr.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3))); // prepare value "true" for rax
            vec_instr.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
            vec_instr.push(Instr::ICmovle(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
            vec_instr.push(Instr::IMov(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, -stack_offset1))); // restore rbx

            vec_instr
        },

        Expr::Let(ids, body) => {
            let mut modi_env = env.clone();
            let mut modi_si = si;
            let mut id_roster = HashSet::new();

            for id_pair in &ids[..] {
                // (prepare for) panic at compile error 
                if id_roster.contains(&id_pair.0) {
                    panic!("Duplicate binding")
                }
                id_roster.insert(&id_pair.0);

                // compile Expr to Instr
                vec_instr.extend(compile_to_instrs(&id_pair.1, modi_si, &modi_env, tag_cnt, loop_exit));
                vec_instr.push(Instr::IMov(Val::RegOffset(Reg::RSP, -8 * modi_si), Val::Reg(Reg::RAX)));

                // update environment and stack index
                let nenv = modi_env.update(id_pair.0.clone(), -8 * modi_si);
                modi_env = nenv;
                modi_si = modi_si + 1;
            }

            // compile Expr to Instr
            vec_instr.extend(compile_to_instrs(body, modi_si, &modi_env, tag_cnt, loop_exit));
            vec_instr
        },

        Expr::Id(id_name) => {
            // panic at compile error
            if !env.contains_key(id_name) {
                panic!("Unbound variable identifier {}", id_name);
            }

            // compile Expr to Instr
            vec_instr.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, *(env.get(id_name).unwrap()))));
            vec_instr
        },

        Expr::If(e1, e2, e3) => {
            let else_labstr = label_string(tag_cnt, "else");
            let end_labstr = label_string(tag_cnt, "end");

            // if
            vec_instr.extend(compile_to_instrs(e1, si, env, tag_cnt, loop_exit));
            vec_instr.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(1)));
            vec_instr.push(Instr::IJe(Val::SymLabel(else_labstr.clone())));

            // then
            vec_instr.extend(compile_to_instrs(e2, si, env, tag_cnt, loop_exit));
            vec_instr.push(Instr::IJmp(Val::SymLabel(end_labstr.clone())));

            // else
            vec_instr.push(Instr::ILabel(Val::SymLabel(else_labstr.clone())));
            vec_instr.extend(compile_to_instrs(e3, si, env, tag_cnt, loop_exit));
            vec_instr.push(Instr::ILabel(Val::SymLabel(end_labstr.clone())));

            vec_instr
        },

        Expr::Set(id_name, e1) => {
            // get id's stack index from env
            if !env.contains_key(id_name) {
                panic!("Unbound variable identifier {}", id_name);
            }
            let id_si = env.get(id_name).unwrap();

            // execute e1
            vec_instr.extend(compile_to_instrs(e1, si, env, tag_cnt, loop_exit));
            
            // update corresponding stack location
            vec_instr.push(Instr::IMov(Val::RegOffset(Reg::RSP, id_si.clone()), Val::Reg(Reg::RAX)));

            vec_instr
        },

        Expr::Block(exprs) => {
            // static error: empty block
            if exprs.len() == 0 {
                panic!("Invalid");
            }

            for e in &exprs[..] {
                // compile Expr to Instr
                vec_instr.extend(compile_to_instrs(e, si, env, tag_cnt, loop_exit));
            }

            vec_instr
        },

        Expr::Loop(e1) => {
            let loop_start_labstr = label_string(tag_cnt, "loop_start");
            let loop_end_labstr = label_string(tag_cnt, "loop_end");

            vec_instr.push(Instr::ILabel(Val::SymLabel(loop_start_labstr.clone())));
            vec_instr.extend(compile_to_instrs(e1, si, env, tag_cnt, &loop_end_labstr.clone()));
            vec_instr.push(Instr::IJmp(Val::SymLabel(loop_start_labstr.clone())));
            vec_instr.push(Instr::ILabel(Val::SymLabel(loop_end_labstr.clone())));

            vec_instr
        },

        Expr::Break(e1) => {
            // report error if no outer loop
            match &loop_exit[..] {
                "" => {
                    panic!("break");
                }
                _ => {}
            } 

            vec_instr.extend(compile_to_instrs(e1, si, env, tag_cnt, loop_exit));
            vec_instr.push(Instr::IJmp(Val::SymLabel(loop_exit.to_string())));

            vec_instr
        },
        
    }

}

// translate Instr to assembly code string
fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(v1, v2) => {
            format!("mov {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::IAdd(v1, v2) => {
            format!("add {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::ISub(v1, v2) => {
            format!("sub {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::IMul(v1, v2) => {
            format!("imul {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::IXor(v1, v2) => {
            format!("xor {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::IAnd(v1, v2) => {
            format!("and {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::IPush(v1) => {
            format!("push {}", val_to_str(v1))
        },
        Instr::ICall(v1) => {
            format!("call {}", val_to_str(v1))
        },
        Instr::ICmp(v1, v2) => {
            format!("cmp {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::IJmp(v1) => {
            format!("jmp {}", val_to_str(v1))
        },
        Instr::IJe(v1) => {
            format!("je {}", val_to_str(v1))
        },
        Instr::IJno(v1) => { // jump if overflow flag is not set
            format!("jno {}", val_to_str(v1))
        },
        Instr::ILabel(v1) => {
            format!("{}:", val_to_str(v1))
        },
        Instr::ICmove(v1, v2) => { // move if =
            format!("cmove {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::ICmovg(v1, v2) => { // move if >
            format!("cmovg {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::ICmovl(v1, v2) => { // move if <
            format!("cmovl {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::ICmovge(v1, v2) => { // move if >=
            format!("cmovge {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::ICmovle(v1, v2) => { // move if <=
            format!("cmovle {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::ISar(v1, v2) => { // signed right shift
            format!("sar {}, {}", val_to_str(v1), val_to_str(v2))
        },
    }
}

// translate Val to assembly code string
fn val_to_str(v: &Val) -> String {
    match v {
        Val::Reg(Reg::RAX) => "rax".to_string(),
        Val::Reg(Reg::RSP) => "rsp".to_string(),
        Val::Reg(Reg::RDI) => "rdi".to_string(),
        Val::Reg(Reg::RBX) => "rbx".to_string(),
        Val::Imm(n) => format!("{}", *n),
        Val::RegOffset(reg, n) => format!("[{} + {}]", val_to_str(&Val::Reg(reg.clone())), *n),
        // Val::RegOffset(Reg::RAX, n) => format!("[rax + {}]", *n),
        // Val::RegOffset(Reg::RSP, n) => format!("[rsp + {}]", *n),
        // Val::RegOffset(Reg::RDI, n) => format!("[rdi + {}]", *n),
        // Val::RegOffset(Reg::RBX, n) => format!("[rbx + {}]", *n),
        Val::SymLabel(s) => s.to_string()
    }
}

// compile Expr to assembly code string
fn compile(e: &Expr) -> String {
    // init helper for label generation
    let mut tag_cnt = i64::try_from(0).unwrap();

    // init environment variables
    let env = HashMap::new();

    let empty_loop_exit = "".to_string();

    // compile Expr to Vec<Instr>
    let vec_instr = compile_to_instrs(e, 2, &env, &mut tag_cnt, &empty_loop_exit);

    // translate Instr to assembly code string 
    let mut asm_str = "".to_string();
    for instr in &vec_instr {
        asm_str.push_str( &format!(
            "{} \n", instr_to_str(instr)
        ));
    }

    return asm_str
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    // read in *.snek
    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let result;
    match parse(&in_contents) {
        Ok(parsed_sexp) => {
            // parse file contents to Expr
            let expr = parse_expr(&parsed_sexp);
            // copmile Expr to asm string
            result = compile(&expr);
        },
        Err(_) => {
            panic!("Invalid");
        }
    }

    let asm_program = format!(
        "
section .text
extern snek_error
global our_code_starts_here
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
