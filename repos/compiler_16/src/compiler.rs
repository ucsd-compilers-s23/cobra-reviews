use core::panic;
use std::fmt::format;


use crate::{Val, Reg, Instr, Op1, Op2, Expr, Bool, Context};
use Reg::*;
use im::HashMap;

pub fn compile_expr(e: &Expr, ctx: &Context, label: &mut i64) -> String {
    let inst_vec : Vec<Instr> = compile_to_instrs(e, ctx, label); 
    let mut assembly_inst = String::new(); 
    for instr in inst_vec {
        assembly_inst += &(instr_to_str(&instr) + "\n"); 
    }
  
    assembly_inst
  }
  
fn compile_to_instrs(e: &Expr, ctx: &Context, label: &mut i64) -> Vec<Instr> {
    let mut instrs = Vec::new(); 
    match e {
        Expr::Number(n) => {
            instrs.push(Instr::IMov(Val::Reg(RAX), Val::Imm(*n << 1))); 
            instrs.extend(compile_bounds_check(RAX, 2)); 
        }
        Expr::Bool(bool) => {
            match bool {
                Bool::True => instrs.push(Instr::IMov(Val::Reg(RAX), Val::Imm(3))), 
                Bool::False => instrs.push(Instr::IMov(Val::Reg(RAX), Val::Imm(1))), 
            }
        }
        Expr::Id(name) => {
            if let "input" = name.as_str() {
                instrs.push(Instr::IMov(Val::Reg(RAX), Val::Reg(RDI)))
            } else {
                match ctx.env.get(name) {
                    Some(num) => {
                        let stack_slot: i64 = num * 8; 
                        instrs.push(Instr::IMov(Val::Reg(RAX), Val::RegOffset(RSP, stack_slot)))
                    }
                    None => panic!("Unbound variable identifier {name}")
                }
            }

        },
        Expr::UnOp(op, subexpr) => {
            instrs.extend(compile_to_instrs(subexpr, ctx, label)); 
            instrs.extend(compile_syntax_check(RAX, 1)); 

            match op {
                Op1::Add1 => instrs.push(Instr::IAdd(Val::Reg(RAX), Val::Imm(1 << 1))), 
                Op1::Sub1 => instrs.push(Instr::ISub(Val::Reg(RAX), Val::Imm(1 << 1))), 
                Op1::Neg => instrs.push(Instr::INeg(Val::Reg(RAX))), 
            }
            instrs.extend(compile_bounds_check(RAX, 2)); 
        }, 
        Expr::BinOp(op, expr1, expr2) => {
            let e1 = compile_to_instrs(expr1, ctx, label); 
            let e2_ctx = Context { si: ctx.si + 1, ..*ctx };
            let e2 = compile_to_instrs(expr2, &e2_ctx, label); 
            let sp_offset: i64 = ctx.si * 8; 
            
            instrs.extend(e1); 
            instrs.extend(compile_syntax_check(RAX, 1)); 
            instrs.push(Instr::IMov(Val::RegOffset(RSP, sp_offset), Val::Reg(RAX))); 
            instrs.extend(e2);
            instrs.extend(compile_syntax_check(RAX, 1));  
            
            match op {
                Op2::Plus => {
                    instrs.push(Instr::IAdd(Val::Reg(RAX), Val::RegOffset(RSP, sp_offset))); 
                    instrs.extend(compile_bounds_check(RAX, 2)); 
                },
                Op2::Minus => {
                    instrs.push(Instr::IMov(Val::Reg(RBX), Val::Reg(RAX))); 
                    instrs.push(Instr::IMov(Val::Reg(RAX), Val::RegOffset(RSP, sp_offset))); 
                    instrs.push(Instr::ISub(Val::Reg(RAX), Val::Reg(RBX))); 
                    instrs.extend(compile_bounds_check(RAX, 2)); 
                },
                Op2::Times => {
                    instrs.push(Instr::IMul(Val::Reg(RAX), Val::RegOffset(RSP, sp_offset))); 
                    instrs.push(Instr::ISar(Val::Reg(RAX), Val::Imm(1))); 
                    instrs.extend(compile_bounds_check(RAX, 2)); 
                }, 
                Op2::Equal => {
                    instrs.push(Instr::ICmp(Val::Reg(RAX), Val::RegOffset(RSP, sp_offset))); 
                    instrs.push(Instr::IMov(Val::Reg(RBX), Val::Imm(3))); 
                    instrs.push(Instr::IMov(Val::Reg(RAX), Val::Imm(1))); 
                    instrs.push(Instr::ICmove(Val::Reg(RAX), Val::Reg(RBX))); 
                }, 
                Op2::Less => {
                    instrs.push(Instr::IMov(Val::Reg(RBX), Val::Reg(RAX))); 
                    instrs.push(Instr::IMov(Val::Reg(RAX), Val::RegOffset(RSP, sp_offset))); 
                    instrs.push(Instr::ICmp(Val::Reg(RAX), Val::Reg(RBX))); 
                    instrs.push(Instr::IMov(Val::Reg(RBX), Val::Imm(3))); 
                    instrs.push(Instr::IMov(Val::Reg(RAX), Val::Imm(1))); 
                    instrs.push(Instr::ICmovl(Val::Reg(RAX), Val::Reg(RBX))); 
                }, 
                Op2::Greater => {
                    instrs.push(Instr::IMov(Val::Reg(RBX), Val::Reg(RAX))); 
                    instrs.push(Instr::IMov(Val::Reg(RAX), Val::RegOffset(RSP, sp_offset))); 
                    instrs.push(Instr::ICmp(Val::Reg(RAX), Val::Reg(RBX))); 
                    instrs.push(Instr::IMov(Val::Reg(RBX), Val::Imm(3))); 
                    instrs.push(Instr::IMov(Val::Reg(RAX), Val::Imm(1))); 
                    instrs.push(Instr::ICmovg(Val::Reg(RAX), Val::Reg(RBX))); 
                }, 
                Op2::LessEqual => {
                    instrs.push(Instr::IMov(Val::Reg(RBX), Val::Reg(RAX))); 
                    instrs.push(Instr::IMov(Val::Reg(RAX), Val::RegOffset(RSP, sp_offset))); 
                    instrs.push(Instr::ICmp(Val::Reg(RAX), Val::Reg(RBX))); 
                    instrs.push(Instr::IMov(Val::Reg(RBX), Val::Imm(3))); 
                    instrs.push(Instr::IMov(Val::Reg(RAX), Val::Imm(1))); 
                    instrs.push(Instr::ICmovle(Val::Reg(RAX), Val::Reg(RBX))); 
                }, 
                Op2::GreaterEqual => {
                    instrs.push(Instr::IMov(Val::Reg(RBX), Val::Reg(RAX))); 
                    instrs.push(Instr::IMov(Val::Reg(RAX), Val::RegOffset(RSP, sp_offset))); 
                    instrs.push(Instr::ICmp(Val::Reg(RAX), Val::Reg(RBX))); 
                    instrs.push(Instr::IMov(Val::Reg(RBX), Val::Imm(3))); 
                    instrs.push(Instr::IMov(Val::Reg(RAX), Val::Imm(1))); 
                    instrs.push(Instr::ICmovge(Val::Reg(RAX), Val::Reg(RBX))); 
                }, 
                // _ => panic!("Unknown binop {:?}", op),
            }
        }, 
        Expr::Let(binds, body) => {
            let mut new_scope = HashMap::new(); 
            let mut offset: i64 = 0; 
            for (name, expr) in binds {
                let sp_offset = (ctx.si + offset) * 8; 
                let bind_ctx = Context { si: ctx.si + offset, ..*ctx };
                let bind_val = compile_to_instrs(expr, &bind_ctx, label); 

                instrs.extend(bind_val);
                instrs.push(Instr::IMov(Val::RegOffset(RSP, sp_offset), Val::Reg(RAX))); 
                
                let sname = name.to_string(); 
                if new_scope.contains_key(&sname) {
                    panic!("Duplicate binding {sname}")
                }
                new_scope = new_scope.update(sname, ctx.si + offset); 
                offset += 1; 
            }
            let mut new_env = ctx.env.clone(); 

            new_env.extend(new_scope); 
            
            let body_ctx = Context { si: ctx.si + offset, env: &new_env, ..*ctx };
            let body_val = compile_to_instrs(body, &body_ctx, label); 
            instrs.extend(body_val); 

        },
        Expr::If(cond, thn, els) => {
            let end_label = new_label(label, "ifend");
            let else_label = new_label(label, "ifelse");

            let cond_instrs = compile_to_instrs(cond, &ctx, label);
            let thn_instrs = compile_to_instrs(thn, ctx, label); // note original context, so store to wherever caller wants
            let els_instrs = compile_to_instrs(els, ctx, label);
            
            instrs.extend(cond_instrs); 
            instrs.push(Instr::ICmp(Val::Reg(RAX), Val::Imm(1))); 
            instrs.push(Instr::IJe(Val::Label(String::from(else_label.clone())))); 
            instrs.extend(thn_instrs); 
            instrs.push(Instr::IJmp(Val::Label(String::from(end_label.clone())))); 
            instrs.push(Instr::ILabel(Val::Label(String::from(else_label.clone())))); 
            instrs.extend(els_instrs);
            instrs.push(Instr::ILabel(Val::Label(String::from(end_label.clone())))); 
        },
        Expr::Loop(e) => {
            let startloop = new_label(label, "loop");
            let endloop = new_label(label, "loopend");
            let e_is = compile_to_instrs(e, &Context { brake: &endloop, ..*ctx }, label);
            
            instrs.push(Instr::ILabel(Val::Label(String::from(startloop.clone())))); 
            instrs.extend(e_is); 
            instrs.push(Instr::IJmp(Val::Label(String::from(startloop.clone())))); 
            instrs.push(Instr::ILabel(Val::Label(String::from(endloop.clone())))); 
        },
        Expr::Block(es) => {
            instrs.extend(es.into_iter().flat_map(|e| compile_to_instrs(e, ctx, label)).collect::<Vec<Instr>>());
        }, 
        Expr::Set(name, val) => {
            let offset = ctx.env.get(name).unwrap() * 8;
            let val_is = compile_to_instrs(val, ctx, label);

            instrs.extend(val_is); 
            instrs.push(Instr::IMov(Val::RegOffset(RSP, offset), Val::Reg(RAX))); 
        }, 
        Expr::Break(e) => {
            let e_is = compile_to_instrs(e, ctx, label);

            instrs.extend(e_is); 
            instrs.push(Instr::IJmp(Val::Label(String::from(ctx.brake.clone())))); 
        },
        Expr::Print(e) => {
            let e_is = compile_to_instrs(e, ctx, label);
            let index = if ctx.si % 2 == 1 { ctx.si + 1 } else { ctx.si };
            let offset = index * 8;

            instrs.extend(e_is); 
            instrs.push(Instr::IMov(Val::RegOffset(RSP, offset), Val::Reg(RDI))); 
            instrs.push(Instr::ISub(Val::Reg(RSP), Val::Imm(offset))); 
            instrs.push(Instr::IMov(Val::Reg(RDI), Val::Reg(RAX))); 
            instrs.push(Instr::ICall(Val::Label(String::from("snek_print")))); 
            instrs.push(Instr::IAdd(Val::Reg(RSP), Val::Imm(offset))); 
            instrs.push(Instr::IMov(Val::Reg(RDI), Val::RegOffset(RSP, offset))); 
          },
          Expr::IsNum(e) => {
            let e_is = compile_to_instrs(e, ctx, label);

            instrs.extend(e_is); 
            instrs.push(Instr::ITest(Val::Reg(RAX), Val::Imm(1)));
            instrs.push(Instr::IMov(Val::Reg(RAX), Val::Imm(1))); 
            instrs.push(Instr::IMov(Val::Reg(RBX), Val::Imm(3))); 
            instrs.push(Instr::ICmove(Val::Reg(RAX), Val::Reg(RBX))); 
          }, 
          Expr::IsBool(e) => {
            let e_is = compile_to_instrs(e, ctx, label);

            instrs.extend(e_is); 
            instrs.push(Instr::ITest(Val::Reg(RAX), Val::Imm(1)));
            instrs.push(Instr::IMov(Val::Reg(RBX), Val::Imm(1))); 
            instrs.push(Instr::IMov(Val::Reg(RAX), Val::Imm(3))); 
            instrs.push(Instr::ICmove(Val::Reg(RAX), Val::Reg(RBX))); 
          }
        _ => panic!("Unknown expr {:?}", e),
    }
    instrs
}

fn compile_syntax_check (reg: Reg, err_code: i64) -> Vec<Instr> {
    let mut instrs = Vec::new(); 
    instrs.push(Instr::ITest(Val::Reg(reg), Val::Imm(1))); 
    instrs.push(Instr::IPush(Val::Reg(RDI))); 
    instrs.push(Instr::IMov(Val::Reg(RDI), Val::Imm(err_code))); 
    instrs.push(Instr::IJnz(Val::Label(String::from("throw_error")))); 
    instrs.push(Instr::IPop(Val::Reg(RDI))); 
    instrs
}

fn compile_bounds_check (reg: Reg, err_code: i64) -> Vec<Instr> {
    let mut instrs = Vec::new(); 
    instrs.push(Instr::ICmp(Val::Reg(reg), Val::Imm(i64::MAX))); 
    instrs.push(Instr::IPush(Val::Reg(RDI))); 
    instrs.push(Instr::IMov(Val::Reg(RDI), Val::Imm(err_code)));
    instrs.push(Instr::IJa(Val::Label(String::from("throw_error")))); 
    instrs.push(Instr::IPop(Val::Reg(RDI))); 

    instrs.push(Instr::ICmp(Val::Reg(reg), Val::Imm(i64::MIN))); 
    instrs.push(Instr::IPush(Val::Reg(RDI))); 
    instrs.push(Instr::IMov(Val::Reg(RDI), Val::Imm(err_code)));
    instrs.push(Instr::IJl(Val::Label(String::from("throw_error")))); 
    instrs.push(Instr::IPop(Val::Reg(RDI))); 
    instrs

}

fn new_label(l: &mut i64, s: &str) -> String {
    let current = *l;
    *l += 1;
    format!("{s}_{current}")
}

fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(val1, val2) => format!("mov {}, {}", val_to_str(val1), val_to_str(val2)), 
        Instr::IAdd(val1, val2) => format!("add {}, {}", val_to_str(val1), val_to_str(val2)), 
        Instr::ISub(val1, val2) => format!("sub {}, {}", val_to_str(val1), val_to_str(val2)),
        Instr::IMul(val1, val2) => format!("imul {}, {}", val_to_str(val1), val_to_str(val2)),
        Instr::IXor(val1, val2) => format!("xor {}, {}", val_to_str(val1), val_to_str(val2)),
        Instr::ICmove(val1, val2) => format!("cmove {}, {}", val_to_str(val1), val_to_str(val2)), 
        Instr::ICmovl(val1, val2) => format!("cmovl {}, {}", val_to_str(val1), val_to_str(val2)),
        Instr::ICmovg(val1, val2) => format!("cmovg {}, {}", val_to_str(val1), val_to_str(val2)),
        Instr::ICmovle(val1, val2) => format!("cmovle {}, {}", val_to_str(val1), val_to_str(val2)),
        Instr::ICmovge(val1, val2) => format!("cmovge {}, {}", val_to_str(val1), val_to_str(val2)),
        Instr::ICmp(val1, val2) => format!("cmp {}, {}", val_to_str(val1), val_to_str(val2)), 
        Instr::ITest(val1, val2) => format!("test {}, {}", val_to_str(val1), val_to_str(val2)), 
        Instr::ISar(val1, val2) => format!("sar {}, {}", val_to_str(val1), val_to_str(val2)), 
        Instr::ICall(val1) => format!("call {}", val_to_str(val1)), 
        Instr::IJmp(val1) => format!("jmp {}", val_to_str(val1)), 
        Instr::IJne(val1) => format!("jne {}", val_to_str(val1)), 
        Instr::IJe(val1) => format!("je {}", val_to_str(val1)), 
        Instr::IJa(val1) => format!("ja {}", val_to_str(val1)), 
        Instr::IJl(val1) => format!("jl {}", val_to_str(val1)), 
        Instr::IJnz(val1) => format!("jnz {}", val_to_str(val1)), 
        Instr::IPush(val1) => format!("push {}", val_to_str(val1)), 
        Instr::IPop(val1) => format!("pop {}", val_to_str(val1)), 
        Instr::INeg(val1) => format!("neg {}", val_to_str(val1)), 
        Instr::ILabel(val1)=>format!("{}:", val_to_str(val1))
    }
}

fn val_to_str(v: &Val) -> String {
    match v {
        Val::Reg(reg) => {
            match reg {
                Reg::RAX => "rax".to_string(),
                Reg::RSP => "rsp".to_string(),
                Reg::RBX => "rbx".to_string(),
                Reg::RDI => "rdi".to_string(),
            }
        },
        Val::Imm(num) => num.to_string(),
        Val::RegOffset(reg, num) => {
            match reg {
                Reg::RSP => format!("[rsp - {}]", num),
                _ => panic!("Only rsp should be used for stack arithmetic.\n Instead:{:?}", reg),
            }
        }, 
        Val::Label(label) => label.to_string(),
    }
}