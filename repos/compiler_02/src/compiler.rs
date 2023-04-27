use im::hashmap;

use crate::structs::*;
use Arg::*;
use Args::*;
use Instr::*;
use Reg::*;

// Snek's true and false values
const TRUE: Arg = Imm(3);
const FALSE: Arg = Imm(1);

pub fn compile_expr(e: &Expr, co: &Context, com: &mut ContextMut) -> Vec<Instr> {
    let mut instrs: Vec<Instr> = vec![];

    // Snek error label to jump on runtime error
    let snek_error: Label = Label::new(Some("snek_error"));

    // Set "input" virtual variable to rdi and recompile
    // Input is treated as a let binding from here on
    // Rdi no longer needs to be taken care of and freely used
    if let None = co.env.get("input") {
        instrs.push(Mov(ToMem(
            MemRef {
                reg: Rsp,
                offset: co.si,
            },
            OReg(Rdi),
        )));
        instrs.extend(compile_expr(
            e,
            &co.modify(
                Some(co.si + 1),
                Some(co.env.update("input".to_string(), VarEnv::new(co.si))),
                None,
            ),
            com,
        ));
        return instrs;
    }

    match e {
        Expr::Num(n) => {
            // Overflowing mul to catch overflow
            // Since * operator does not report an overflow in Rust
            let (i, overflow) = n.overflowing_mul(2);
            if overflow {
                panic!("Invalid");
            }
            instrs.push(Mov(ToReg(Rax, Imm64(i))));
        }
        Expr::Boolean(b) => instrs.push(Mov(ToReg(
            Rax,
            match b {
                true => TRUE,
                false => FALSE,
            },
        ))),
        // Load from the stack with the offset from Context environment to Rax
        Expr::Var(x) => instrs.push(Mov(ToReg(
            Rax,
            Mem(MemRef {
                reg: Rsp,
                offset: match co.env.get(x) {
                    Some(o) => o.offset,
                    None => panic!("Unbound variable identifier {x}"),
                },
            }),
        ))),
        Expr::UnOp(op, subexpr) => {
            instrs.extend(compile_expr(subexpr, co, com));

            match op {
                Op1::Add1 => {
                    // Check if Rax is number
                    instrs.push(Test(ToReg(Rax, Imm(1))));
                    instrs.push(Mov(ToReg(Rdi, Imm(20)))); // invalid argument
                    instrs.push(JumpI(Jump::Nz(snek_error.clone())));

                    instrs.push(Add(Rax, Imm(2)));

                    // Check overflow
                    instrs.push(Mov(ToReg(Rdi, Imm(30))));
                    instrs.push(JumpI(Jump::O(snek_error)));
                }
                Op1::Sub1 => {
                    // Check if Rax is number
                    instrs.push(Test(ToReg(Rax, Imm(1))));
                    instrs.push(Mov(ToReg(Rdi, Imm(21)))); // invalid argument
                    instrs.push(JumpI(Jump::Nz(snek_error.clone())));

                    instrs.push(Sub(Rax, Imm(2)));

                    // Check overflow
                    instrs.push(Mov(ToReg(Rdi, Imm(31)))); // overflow
                    instrs.push(JumpI(Jump::O(snek_error)));
                }
                Op1::IsBool => {
                    instrs.push(And(Rax, Imm(1)));
                    instrs.push(Mov(ToReg(Rax, TRUE))); // Set true
                    instrs.push(Mov(ToReg(Rbx, FALSE)));
                    instrs.push(CMovI(CMov::Z(Rax, OReg(Rbx)))); // Set false if zero
                }
                Op1::IsNum => {
                    instrs.push(And(Rax, Imm(1)));
                    instrs.push(Mov(ToReg(Rax, FALSE))); // Set false
                    instrs.push(Mov(ToReg(Rbx, TRUE)));
                    instrs.push(CMovI(CMov::Z(Rax, OReg(Rbx)))); // Set true if zero
                }
            }
        }
        Expr::BinOp(op, left, right) => {
            instrs.extend(compile_expr(right, co, com));
            // Save result in stack
            instrs.push(Mov(ToMem(
                MemRef {
                    reg: Rsp,
                    offset: co.si,
                },
                OReg(Rax),
            )));
            instrs.extend(compile_expr(left, &co.modify_si(co.si + 1), com));

            // second argument location
            let mem = Mem(MemRef {
                reg: Rsp,
                offset: co.si,
            });

            // Equal can take both boolean input, rest cannot
            if let Op2::Equal = op {
                // Check equality with sub instead of cmp
                instrs.push(Sub(Rax, mem));
                instrs.push(Mov(ToReg(Rcx, OReg(Rax)))); // Copy to Rcx for checking type later
                instrs.push(Mov(ToReg(Rax, FALSE))); // Set false
                instrs.push(Mov(ToReg(Rbx, TRUE)));
                instrs.push(CMovI(CMov::E(Rax, OReg(Rbx))));

                // Check if both were of the same type from Rcx
                instrs.push(Test(ToReg(Rcx, Imm(1))));
                instrs.push(Mov(ToReg(Rdi, Imm(22)))); // invalid argument
                instrs.push(JumpI(Jump::Nz(snek_error.clone())));
            } else {
                // Check if Rax is a number
                instrs.push(Test(ToReg(Rax, Imm(1))));
                instrs.push(Mov(ToReg(Rdi, Imm(23)))); // invalid argument
                instrs.push(JumpI(Jump::Nz(snek_error.clone())));
                // Check if mem is a number
                instrs.push(Test(ToMem(
                    MemRef {
                        reg: Rsp,
                        offset: co.si,
                    },
                    Imm(1),
                )));
                instrs.push(Mov(ToReg(Rdi, Imm(24)))); // invalid argument
                instrs.push(JumpI(Jump::Nz(snek_error.clone())));

                match op {
                    Op2::Plus => {
                        instrs.push(Add(Rax, mem));
                        instrs.push(Mov(ToReg(Rdi, Imm(32)))); // overflow
                        instrs.push(JumpI(Jump::O(snek_error)));
                    }
                    Op2::Minus => {
                        instrs.push(Sub(Rax, mem));
                        instrs.push(Mov(ToReg(Rdi, Imm(33)))); // overflow
                        instrs.push(JumpI(Jump::O(snek_error)));
                    }
                    Op2::Times => {
                        // SHR does not preserve the sign bit!!!
                        // Have to do SAR (Shift Arithmetic Right) which does preserve sign
                        instrs.push(Sar(Rax, 1));
                        instrs.push(Mul(Rax, mem));
                        instrs.push(Mov(ToReg(Rdi, Imm(34)))); // overflow
                        instrs.push(JumpI(Jump::O(snek_error)));
                    }
                    _ => {
                        instrs.push(Cmp(ToReg(Rax, mem)));
                        instrs.push(Mov(ToReg(Rax, FALSE))); // Set false
                        instrs.push(Mov(ToReg(Rbx, TRUE)));
                        // Set true based on condition using CMovXX
                        match op {
                            Op2::Greater => instrs.push(CMovI(CMov::G(Rax, OReg(Rbx)))),
                            Op2::GreaterEqual => instrs.push(CMovI(CMov::GE(Rax, OReg(Rbx)))),
                            Op2::Less => instrs.push(CMovI(CMov::L(Rax, OReg(Rbx)))),
                            Op2::LessEqual => instrs.push(CMovI(CMov::LE(Rax, OReg(Rbx)))),
                            Op2::Plus | Op2::Minus | Op2::Times | Op2::Equal => {
                                unimplemented!("{:?}", op) // Should not execute, satisfy Rust checks
                            }
                        }
                    }
                }
            };
        }
        Expr::Let(bindings, e) => {
            // Updated env to execute the let expression with
            let mut new_env = co.env.clone();
            // Track duplicates in the let bindings
            let mut track_dup = hashmap! {};

            for (i, (x, b)) in bindings.iter().enumerate() {
                if track_dup.contains_key(x) {
                    panic!("Duplicate binding")
                }
                // Calculate offset
                let si_ = co.si + i as i32;
                // Compile the binding expression with the currently processed partial env
                instrs.extend(compile_expr(
                    b,
                    &co.modify(Some(si_), Some(new_env.clone()), None),
                    com,
                ));
                // Save to the corresponding offset of the variable in stack
                instrs.push(Mov(ToMem(
                    MemRef {
                        reg: Rsp,
                        offset: si_,
                    },
                    OReg(Rax),
                )));
                // Insert it to the environment
                track_dup.insert(x.to_string(), true);
                new_env.insert(x.to_string(), VarEnv::new(si_));
            }

            // Compile the let expression with the updated stack offset and environment
            instrs.extend(compile_expr(
                e,
                &co.modify(Some(co.si + bindings.len() as i32), Some(new_env), None),
                com,
            ));
        }
        Expr::If(c, t, e) => {
            // Else and endif labels with the same index (easier to track which labels end where)
            let else_label = com.label("else");
            let end_if_label = com.label("end_if");
            // Mark index as used
            com.index_used();

            // Evaluate the if expression
            instrs.extend(compile_expr(c, co, com));
            // If only executes else if expression is false
            instrs.push(Cmp(ToReg(Rax, FALSE)));
            instrs.push(JumpI(Jump::E(else_label.clone())));
            // Then
            instrs.extend(compile_expr(t, co, com));
            instrs.push(JumpI(Jump::U(end_if_label.clone())));
            // Else
            instrs.push(LabelI(else_label));
            instrs.extend(compile_expr(e, co, com));
            // End of if
            instrs.push(LabelI(end_if_label));
        }
        Expr::Set(x, e) => {
            instrs.extend(compile_expr(e, co, com));
            // Move the result to the offset corresponding to the variable
            instrs.push(Mov(ToMem(
                MemRef {
                    reg: Rsp,
                    offset: co
                        .env
                        .get(x)
                        .expect(format!("Unbound variable identifier {x}").as_str())
                        .offset,
                },
                OReg(Rax),
            )))
        }
        Expr::Block(es) => {
            // Compile all expressions linearly
            for e in es {
                instrs.extend(compile_expr(e, co, com));
            }
        }
        Expr::Loop(e) => {
            // Begin and end label have same label index (easier to track which labels end where)
            let begin_loop = com.label("begin_loop");
            let end_loop = com.label("end_loop");
            // Mark index as used
            com.index_used();

            // Set label for the start of loop
            instrs.push(LabelI(begin_loop.clone()));
            // Compile the loop body with the break target of end_loop
            instrs.extend(compile_expr(
                e,
                &com.new_ce_label(co, end_loop.clone()),
                com,
            ));
            // Unconditional Jump to start of loop
            instrs.push(JumpI(Jump::U(begin_loop)));
            instrs.push(LabelI(end_loop));
        }
        Expr::Break(e) => {
            if co.label.name == "" {
                panic!("dangling break");
            } else {
                // Evaluate the break expression, can have another break, which will use the same break target
                instrs.extend(compile_expr(e, co, com));
                // Jump to end_loop
                instrs.push(JumpI(Jump::U(co.label.clone())));
            }
        }
        // Used by REPL environment only
        Expr::Define(_, _) => panic!("define cannot be compiled"),
    }
    return instrs;
}

// Transform Instr to assembly string
pub fn instrs_to_string(instrs: &Vec<Instr>) -> String {
    instrs
        .iter()
        .map(|i| {
            // Indentation to the generated assembly
            // Labels are not indented
            if matches!(i, LabelI(_)) {
                format!("{i}")
            } else {
                format!(" {i}")
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}

// REPL dynamic assembler generation
pub fn instrs_to_asm(cmds: &Vec<Instr>, ops: &mut dynasmrt::x64::Assembler) {
    cmds.iter().for_each(|c| c.asm(ops))
}
