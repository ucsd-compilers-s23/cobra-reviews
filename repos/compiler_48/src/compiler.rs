use crate::{enums::*, utils::new_label};

use im::HashMap;
use std::collections::HashSet;

pub fn compile_expr(e: &Expr) -> Vec<Instr> {
    let mut label_count = 2;
    let env = HashMap::new();

    compile_expr_to_enums(e, 2, &env, &mut label_count, &String::from(""))
}

fn compile_expr_to_enums(e: &Expr, si: i32, env: &HashMap<String, i32>, 
                         label_count: &mut i32, break_target: &String) -> Vec<Instr> {
    match e {
        // number (n << 1)
        Expr::Number(n) => {
            vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n << 1))]
        },

        // boolean (3 for true, 1 for false)
        Expr::Boolean(b) => {
            let bool_val = match b {
                true => 3,
                false => 1,
            };

            vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(bool_val))]
        },

        // user input (from rdi)
        Expr::Input => {
            vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI))]
        },

        // variable identifier
        Expr::Id(s) => {
            // check if variable exists in env
            if !env.contains_key(s) {
                panic!("Unbound variable identifier {}", s);
            }

            vec![Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, *env.get(s).unwrap()))]
        },

        // let bindings
        Expr::Let(bindings, body) => {
            let mut seen_variables: HashSet<String> = HashSet::new();

            let mut cur_si = si;
            let mut cur_env = env.clone();

            let mut instrs: Vec<Instr> = vec![];

            // parse bindings
            for (variable, var_expr) in bindings.iter() {
                // check duplicate binding
                if seen_variables.contains(variable) {
                    panic!("Duplicate binding");
                }

                // add to seen variables
                seen_variables.insert(variable.clone());

                // compile binding expr
                let mut sube_instrs = compile_expr_to_enums(var_expr, cur_si, &cur_env, label_count, break_target);

                instrs.append(&mut sube_instrs);

                // save value to stack
                instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, cur_si*8), Val::Reg(Reg::RAX)));

                // update current env and stack index
                cur_env = cur_env.update(variable.clone(), cur_si*8);
                cur_si += 1;
            }

            // compile body
            let mut body_instrs = compile_expr_to_enums(body, cur_si, &cur_env, label_count, break_target);
            instrs.append(&mut body_instrs);

            instrs
        },

        // unary operators
        Expr::UnOp(op, e) => {
            // compile expr
            let mut e_instrs = compile_expr_to_enums(e, si, env, label_count, break_target);

            // compile rest of unop instructions based on type
            let mut new_instrs = match op {
                Op1::Arith(o) => compile_unop_arith(o),
                Op1::Check(o) => compile_unop_typecheck(o),
            };
            
            e_instrs.append(&mut new_instrs);

            e_instrs
        },

        // binary operators
        Expr::BinOp(op, e1, e2) => {
            // compile subexpressions
            let mut e_instrs = compile_expr_to_enums(e2, si, env, label_count, break_target);
            let mut e1_instrs = compile_expr_to_enums(e1, si+1, env, label_count, break_target);

            let stack_offset = si*8;

            // e2 instrs + move result of e2 to stack
            e_instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));

            // e1 instrs
            e_instrs.append(&mut e1_instrs);

            // compile rest binary op instrs based on type
            let mut new_instrs = match op {
                Op2::Arith(o) => compile_binop_arith(o, stack_offset),
                Op2::Check(o) => compile_binop_compare(o, stack_offset),
            };
            e_instrs.append(&mut new_instrs);

            e_instrs
        },

        // if then else
        Expr::If(cond_e, then_e, else_e) => {
            // labels
            let then_label = new_label("then", label_count); // unused, help w/ readability
            let else_label = new_label("else", label_count);
            let endif_label = new_label("endif", label_count);

            // subexpr instructions
            let mut instrs = compile_expr_to_enums(cond_e, si, env, label_count, break_target);
            let mut then_instrs = compile_expr_to_enums(then_e, si, env, label_count, break_target);
            let mut else_instrs = compile_expr_to_enums(else_e, si, env, label_count, break_target);

            // cmp value to "false" and jump to else branch if true
            instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(1)));
            instrs.push(Instr::IJe(else_label.clone()));

            // then branch (run then instrs then jump to endif)
            instrs.push(Instr::Label(then_label));
            instrs.append(&mut then_instrs);
            instrs.push(Instr::IJmp(endif_label.clone()));

            // else branch
            instrs.push(Instr::Label(else_label));
            instrs.append(&mut else_instrs);

            // end if
            instrs.push(Instr::Label(endif_label));

            instrs
        },

        // loop
        Expr::Loop(body) => {
            // labels
            let loop_label = new_label("loop", label_count);
            let endloop_label = new_label("endloop", label_count);

            // compile loop body, passing in break target
            let mut body_instrs = compile_expr_to_enums(body, si, env, label_count, &endloop_label);

            // start loop
            let mut instrs = vec![Instr::Label(loop_label.clone())];
            
            // loop body
            instrs.append(&mut body_instrs);

            // jump back to start of loop
            instrs.push(Instr::IJmp(loop_label));

            // end loop
            instrs.push(Instr::Label(endloop_label));

            instrs
        },

        // break
        Expr::Break(e) => {
            // check if break target exists (we're in a loop)
            if break_target.len() < 1 {
                panic!("compile error: break statement outside of loop");
            }

            // evaluate break body
            let mut instrs = compile_expr_to_enums(e, si, env, label_count, break_target);

            instrs.push(Instr::IJmp(break_target.clone()));

            instrs
        },

        // set! 
        Expr::Set(s, e) => {
            // check if variable exists
            if !env.contains_key(s) {
                panic!("Unbound variable identifier {}", s);
            }

            // compile expr
            let mut instrs = compile_expr_to_enums(e, si, env, label_count, break_target);

            // save result to where variable is in stack
            instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, *env.get(s).unwrap()), Val::Reg(Reg::RAX)));

            instrs
        },

        // block
        Expr::Block(exprs) => {
            // compile all exprs in order
            let mut instrs = Vec::new();

            for e in exprs.iter() {
                let mut expr_instrs = compile_expr_to_enums(e, si, env, label_count, break_target);

                instrs.append(&mut expr_instrs);
            }

            instrs
        }
    }
}

fn compile_unop_arith(op: &Op1Arith) -> Vec<Instr> {
    let mut instrs = vec![];

    // typecheck value is integer
    // LSB should be 0
    let mut typecheck_instrs = vec![
        Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)),
        Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1)),
        Instr::IJne(String::from("invalid_type_error"))
    ];

    instrs.append(&mut typecheck_instrs);

    // add or sub 1
    let new_instr = match op {
        Op1Arith::Add1 => Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1 << 1)),
        Op1Arith::Sub1 => Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(1 << 1)),
    };

    // check overflow
    instrs.push(new_instr);
    instrs.push(Instr::IJo(String::from("overflow_error")));

    instrs
}

fn compile_unop_typecheck(op: &Op1Check) -> Vec<Instr> {
    let mut instrs = vec![];

    // check LSB
    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
    instrs.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(1)));
    
    // intended LSB value
    let tagged_bit = match op {
        Op1Check::IsNum => 0,
        Op1Check::IsBool => 1,
    };

    instrs.push(Instr::ICmp(Val::Reg(Reg::RBX), Val::Imm(tagged_bit)));
    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
    instrs.push(Instr::ICMovE(Reg::RAX, Reg::RBX));

    instrs
}

fn compile_binop_arith(op: &Op2Arith, stack_offset: i32) -> Vec<Instr> {
    let mut instrs = vec![];

    // typecheck if both are integers
    // or both values, then extract LSB
    // if both are numbers, LSB should be 0
    let mut typecheck_instrs = vec![
        Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)),
        Instr::IOr(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, stack_offset)),
        Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1)),
        Instr::IJne(String::from("invalid_type_error"))
    ];

    instrs.append(&mut typecheck_instrs);

    // arithmetics instructions (if multiply, need to rightshift one value)
    let mut new_instrs = match op {
        Op2Arith::Plus => vec![Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset))],
        Op2Arith::Minus => vec![Instr::ISub(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset))],
        Op2Arith::Times => vec![
            Instr::ISar(Reg::RAX, 1),
            Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset))
        ],
    };

    // check overflow
    instrs.append(&mut new_instrs);
    instrs.push(Instr::IJo(String::from("overflow_error")));

    instrs
}

fn compile_binop_compare(op: &Op2Check, stack_offset: i32) -> Vec<Instr> {
    let mut instrs = vec![Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX))];

    // typecheck
    let typecheck_instr = match op {
        // if Eq, check for same type
        // xor both, LSB should be 0
        Op2Check::Equal => Instr::IXor(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, stack_offset)),
        
        // else, check both are integers
        // or both, LSB should be 0
        _ => Instr::IOr(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, stack_offset)),
    };

    instrs.push(typecheck_instr);
    instrs.push(Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1)));
    instrs.push(Instr::IJne(String::from("invalid_type_error")));

    // compare
    instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));

    // conditional move
    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
    
    let cond_move_instr = match op {
        Op2Check::Equal => Instr::ICMovE(Reg::RAX, Reg::RBX),
        Op2Check::Greater => Instr::ICMovG(Reg::RAX, Reg::RBX),
        Op2Check::GreaterEqual => Instr::ICMovGE(Reg::RAX, Reg::RBX),
        Op2Check::Less => Instr::ICMovL(Reg::RAX, Reg::RBX),
        Op2Check::LessEqual => Instr::ICMovLE(Reg::RAX, Reg::RBX),
    };

    instrs.push(cond_move_instr);

    instrs
}

pub fn convert_enums_to_assembly(instrs: &Vec<Instr>) -> String {
    let mut result = String::new();

    for instr in instrs.iter() {
        result.push_str(&instr.to_string());
    }

    result
}