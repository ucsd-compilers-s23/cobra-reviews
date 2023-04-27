use core::panic;
use std::collections::btree_set;

use im::{HashMap, HashSet};

use crate::{instr::{*, self}, expr::*};

pub fn compile_expr(expr: &Expr, si:i32, env: &HashMap<String, i32>, l: &mut i32, break_target: Option<String>) -> Vec<Instr>{
    match expr{
        Expr::Number(n) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n))],
        Expr::Boolean(b) => {
            match b{
                false => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))],
                true => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3))]
            }
        } 
        Expr::Id(id) => {
            if (id.as_str() == "input") {
                return vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI))]
            }

            let offset = env.get(id);
            match offset{
                Some(offset_val) =>  {
                    println!("Id lookup for {id} = {offset_val}");
                    vec![Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, *offset_val))]
                },
                None => panic!("Unbound variable identifier {id}")
            }
        },
        Expr::UnOp(op1, expr ) => {
            let mut expr_comp = compile_expr(expr, si, env, l, break_target);
            match op1{
                Op1::Add1 => {
                    expr_comp.push(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)));
                    expr_comp.push(Instr::Jne(String::from("throw_type_error")));

                    expr_comp.push( Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(2)  )  );
                    expr_comp.push(Instr::Jo(String::from("throw_overflow_error")));
                },
                Op1::Sub1 => {
                    expr_comp.push(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)));
                    expr_comp.push(Instr::Jne(String::from("throw_type_error")));

                    expr_comp.push( Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(2)  )  );
                    expr_comp.push(Instr::Jo(String::from("throw_overflow_error")));
                },
                Op1::IsBool => {
                    expr_comp.push(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)));
                    expr_comp.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    expr_comp.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
                    expr_comp.push(Instr::CMovne(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                },
                Op1::IsNum=> {
                    expr_comp.push(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)));
                    expr_comp.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    expr_comp.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
                    expr_comp.push(Instr::CMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                },
                _ => panic!("Not implemented yet")
            }
            expr_comp
        },
        Expr::BinOp(op, e1 , e2 ) => compile_bin_op(op, e1, e2, si, env, l, break_target),
        Expr::Let(bindings, e) => compile_let(bindings, e, si, env, l, break_target),
        Expr::Set(id, e) => {
            let id_offset = env.get(id);
            let mut instrs = compile_expr(e, si, env, l, break_target);

            match id_offset{
                Some(offset) => instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, *offset), Val::Reg(Reg::RAX))),
                None => panic!("Unbound variable identifier {id}")
            }
            
            instrs
        }
        Expr::If(cond, thn, els) => {
            let end_label = new_label(l, "ifend");
            let else_label = new_label((l), "ifelse");
            let cond_instr = compile_expr(cond, si, env, l, break_target.clone());
            let thn_instr = compile_expr(thn, si, env, l, break_target.clone());
            let else_instr = compile_expr(els, si, env, l, break_target.clone());

            let mut instrs: Vec<Instr> = vec![];

            instrs.extend(cond_instr);
            instrs.push(Instr::IComp(Val::Reg(Reg::RAX), Val::Imm(1)));
            instrs.push(Instr::Je(else_label.clone()));
            instrs.extend(thn_instr);
            instrs.push(Instr::Jmp(end_label.clone()));
            instrs.push(Instr::Label(else_label));
            instrs.extend(else_instr);
            instrs.push(Instr::Label(end_label));

            instrs
        },
        Expr::Loop(e) => {
            let loop_label = new_label(l, "loop");
            let loop_end_label = new_label(l, "loop_end"); // This would be the new break target

            let mut instrs: Vec<Instr> = vec![];
            instrs.push(Instr::Label(loop_label.clone()));
            instrs.extend(compile_expr(e, si, env, l, Some(loop_end_label.clone())));
            instrs.push(Instr::Jmp(loop_label));
            instrs.push(Instr::Label(loop_end_label));

            instrs
        },
        Expr::Break(expr) => {
            let mut instrs = compile_expr(expr, si, env, l, break_target.clone());
            instrs.push(Instr::Jmp(break_target.expect("Invalid break target")));
            
            instrs
        }
        Expr::Block(instructions) => {
            let mut instrs: Vec<Instr> = vec![];
            for instr in instructions{
                instrs.extend(compile_expr(instr, si, env, l, break_target.clone()));
            }

            instrs
        }
        _ => panic!("Not implemented")


    }    
}

fn new_label(l: &mut i32, s: &str) -> String{
    let current = *l;
    *l += 1;
    format!("{s}_{current}")
}

fn compile_let(bindings: &Vec<(String, Expr)>, expr: &Expr, si: i32, env: &HashMap<String, i32>, l: &mut i32, break_target: Option<String>) -> Vec<Instr>{
    println!("Compiling let");
    let mut nsi = si;
    let mut instrs: Vec<Instr> = vec![];
    let mut nenv = env.clone(); // TODO: Clone check

    
    for key in &nenv{
        println!("{si} => {}: {} ", key.0, key.1);
    }

    let mut new_binding: HashSet<String> = HashSet::new();
    for binding in bindings{
        let offset = nsi * 8;
        nsi += 1;
        if (new_binding.contains(&binding.0)){
            panic!("Duplicate binding");
        }
        else{
            new_binding.insert(binding.0.to_string());
        }
        instrs.extend(compile_expr(&binding.1, nsi, &nenv, l, break_target.clone()));
        instrs.push( Instr::IMov(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX) ) );

        nenv = nenv.update(binding.0.clone(), offset); // TODO : Clone check

    }
    // Next staxk index would be si + the number of bindings in the current env ??

    instrs.extend(compile_expr(expr, nsi, &nenv, l, break_target));
    return instrs;
}

fn compile_bin_op(op: &Op2, e1: &Expr, e2: &Expr, si: i32, env: &im::HashMap<String, i32>, l: &mut i32, break_target: Option<String>) -> Vec<Instr>{

    //let mut e1_instrs = compile_expr(e1, si, env);
    //let mut e2_instrs = compile_expr(e2, si + 1, env); // TODO: Check si and env

    let stack_offset = si * 8;

    let e1_si = match op{
        Op2::Minus => si + 1,
        _ => si
    };

    let e2_si = match op{
        Op2::Minus => si,
        _ => si + 1
    };

    let e1_instrs = compile_expr(e1, e1_si, env, l, break_target.clone());
    let e2_instrs = compile_expr(e2, e2_si, env,l, break_target.clone()); // TODO: Check si and env
    let mut instrs: Vec<Instr> = vec![];

    let cond_label = match op{
        Op2::Less | Op2::LessEqual | Op2::Greater | Op2::GreaterEqual => Some(new_label(l, "true")),
        _ => None
    };

    let end_label = match op{
        Op2::Less | Op2::LessEqual | Op2::Greater | Op2::GreaterEqual => Some(new_label(l, "trueend")),
        _ => None
    };

    let bin_op = match op{
        Op2::Minus => Some(Instr::ISub(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset))),
        Op2::Plus =>  Some(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset))),
        Op2::Times => Some(Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset))),
        Op2::Less =>  Some(Instr::Jl(cond_label.clone().unwrap())),
        Op2::LessEqual =>  Some(Instr::Jle(cond_label.clone().unwrap())),
        Op2::Greater =>  Some(Instr::Jg(cond_label.clone().unwrap())),
        Op2::GreaterEqual =>  Some(Instr::Jge(cond_label.clone().unwrap())),
        _ => None
    };

    match op{
        Op2::Less | Op2::LessEqual | Op2::Greater | Op2::GreaterEqual => {
            instrs.extend(e1_instrs);

            instrs.push(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)));
            instrs.push(Instr::Jne(String::from("throw_type_error")));

            instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
            instrs.extend(e2_instrs);
            instrs.push(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)));
            instrs.push(Instr::Jne(String::from("throw_type_error")));
          
            instrs.push(Instr::IComp(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
            instrs.push(bin_op.unwrap());
            instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
            instrs.push(Instr::Jmp(end_label.clone().unwrap()));
            instrs.push(Instr::Label(cond_label.unwrap()));
            instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
            instrs.push(Instr::Label(end_label.unwrap()));
            
            instrs
        },
        Op2::Equal =>{
            instrs.extend(e1_instrs);
            instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
            instrs.extend(e2_instrs);
            instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
            instrs.push(Instr::Xor(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, stack_offset)));
            instrs.push(Instr::Test(Val::Reg(Reg::RBX), Val::Imm(1)));
            instrs.push(Instr::Jne(String::from("throw_type_error")));
            instrs.push(Instr::IComp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
            instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
            instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
            instrs.push(Instr::CMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));

            instrs
        },
        Op2::Minus => {
            instrs.extend(e2_instrs);

            instrs.push(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)));
            instrs.push(Instr::Jne(String::from("throw_type_error")));

            instrs.push(Instr::IMov( Val::RegOffset(Reg::RSP, stack_offset) , Val::Reg(Reg::RAX)));
            instrs.extend(e1_instrs);

            instrs.push(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)));
            instrs.push(Instr::Jne(String::from("throw_type_error")));

            instrs.push(bin_op.unwrap());
            instrs.push(Instr::Jo(String::from("throw_overflow_error")));
            instrs
        }
        Op2::Plus => { // + , * 
            let mut e1_instrs = compile_expr(e1, si, env, l, break_target.clone());

            e1_instrs.push(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)));
            e1_instrs.push(Instr::Jne(String::from("throw_type_error")));

            let e2_instrs = compile_expr(e2, si + 1, env, l, break_target.clone()); // TODO: Check si and env
            e1_instrs.push(Instr::IMov( Val::RegOffset(Reg::RSP, stack_offset) , Val::Reg(Reg::RAX)));
            e1_instrs.extend(e2_instrs);

            e1_instrs.push(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)));
            e1_instrs.push(Instr::Jne(String::from("throw_type_error")));

            e1_instrs.push(bin_op.unwrap());

            e1_instrs.push(Instr::Jo(String::from("throw_overflow_error")));
            e1_instrs
        },
        Op2::Times => { // + , * 
            let mut e1_instrs = compile_expr(e1, si, env, l, break_target.clone());

            e1_instrs.push(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)));
            e1_instrs.push(Instr::Jne(String::from("throw_type_error")));

            // Dividing left expr by 2 ( right shift once )
            e1_instrs.push(Instr::Sar(Val::Reg(Reg::RAX), Val::Imm(1)));
            let e2_instrs = compile_expr(e2, si + 1, env, l, break_target.clone()); // TODO: Check si and env

            e1_instrs.push(Instr::IMov( Val::RegOffset(Reg::RSP, stack_offset) , Val::Reg(Reg::RAX)));
            e1_instrs.extend(e2_instrs);

            e1_instrs.push(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)));
            e1_instrs.push(Instr::Jne(String::from("throw_type_error")));

            e1_instrs.push(bin_op.unwrap());

            e1_instrs.push(Instr::Jo(String::from("throw_overflow_error")));

            e1_instrs
        }
    }

}

