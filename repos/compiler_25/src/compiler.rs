use crate::abstract_syntax::*;
use crate::instructions::*;
use im::HashMap;
use im::HashSet;

const I63_MIN: i64 = -4611686018427387904;
const I63_MAX: i64 = 4611686018427387903;

const FALSE_INT: i64 = 1;
const TRUE_INT: i64 = 3;

const ERR_NUM_OVERFLOW: i64 = 2;
const ERR_INVALID_TYPE: i64 = 4;

const NUM_OVERFLOW_LABEL: &str = "error_numeric_overflow";
const INVALID_TYPE_LABEL: &str = "error_invalid_type";

// #[derive(Clone, Copy)]
// enum Loc {
//     LReg(Reg),
//     LStack(i64),
// }

struct Context<'a> {
    si: i64,
    env: &'a HashMap<String, i64>,
    // env: &'a HashMap<String, Loc>,
    brake: &'a str,
    // target: Loc,
}

// Returns whether the given integer is outside the range of a 63-bit signed integer.
fn int_overflow(i: i64) -> bool {
    return i < I63_MIN || i > I63_MAX;
}

// Get an incremented label
fn get_new_label(label_ctr: &mut i32, s: &str) -> String {
    let current = *label_ctr;
    *label_ctr += 1;
    return format!("{s}_{current}");
}

// API function for compiling
pub fn compile_to_instrs(expr: &Expr) -> Vec<Instr> {
    let ctxt = Context {
        si: 2,
        env: &HashMap::new(),
        brake: "",
    };
    let mut label_ctr = 0;

    let mut instrs: Vec<Instr> = Vec::new();
    instrs.push(Instr::NoInstr()); // for formatting
    instrs.append(&mut compile_expr(expr, &ctxt, &mut label_ctr));
    instrs.push(Instr::IRet());
    return instrs;
}

// Return instructions that are common to all implementations of inequality operators
fn compile_ineq_prelude(ctxt: &Context) -> Vec<Instr> {
    let mut instrs: Vec<Instr> = Vec::new();
    let stack_offset = ctxt.si * 8;

    // Move the result of e2 into RBX for the type check
    instrs.push(Instr::IMov(Val::VReg(Reg::RBX), Val::VReg(Reg::RAX)));

    // Check that both operands are of the integer type.
    // e1 OR e2 has a 0 as the LSB if both are integers, 1 otherwise.
    instrs.push(Instr::IOr(
        Val::VReg(Reg::RBX),
        Val::VRegOff(Reg::RSP, stack_offset),
    ));
    // Test if the LSB is 0
    instrs.push(Instr::ITest(Val::VReg(Reg::RBX), Val::VImm(1)));
    // If the tag bits are not both 0, jump to the error handler
    instrs.push(Instr::IJne(INVALID_TYPE_LABEL.to_string()));

    // Compare the results of e1 and e2.
    instrs.push(Instr::ICmp(
        Val::VRegOff(Reg::RSP, stack_offset),
        Val::VReg(Reg::RAX),
    ));

    // Move true into RBX
    instrs.push(Instr::IMov(Val::VReg(Reg::RBX), Val::VImm(TRUE_INT)));

    // Move false into RAX
    instrs.push(Instr::IMov(Val::VReg(Reg::RAX), Val::VImm(FALSE_INT)));

    return instrs;
}

fn compile_num_overflow_instrs() -> Vec<Instr> {
    let mut instrs: Vec<Instr> = Vec::new();

    instrs.push(Instr::IJo(NUM_OVERFLOW_LABEL.to_string()));

    return instrs;
}

// Main function for compling an expression
fn compile_expr(expr: &Expr, ctxt: &Context, label_ctr: &mut i32) -> Vec<Instr> {
    let mut instrs: Vec<Instr> = Vec::new();
    match expr {
        Expr::Number(num) => {
            if int_overflow(*num) {
                panic!("Invalid: number must be in the range of a 63-bit signed integer");
            } else {
                // Convert the number to our internal representation
                let num = *num << 1;
                instrs.push(Instr::IMov(Val::VReg(Reg::RAX), Val::VImm(num)));
            }
        }
        Expr::Boolean(false) => instrs.push(Instr::IMov(Val::VReg(Reg::RAX), Val::VImm(FALSE_INT))),
        Expr::Boolean(true) => instrs.push(Instr::IMov(Val::VReg(Reg::RAX), Val::VImm(TRUE_INT))),

        Expr::Id(keyword) if keyword == "input" => {
            instrs.push(Instr::IMov(Val::VReg(Reg::RAX), Val::VReg(Reg::RDI)))
        }
        Expr::Id(s) => {
            // If the identifier is unbound in its scope, report an error.
            let id_stack_offset = match ctxt.env.get(s) {
                Some(offset) => offset,
                None => panic!("Unbound variable identifier {s}"),
            };
            instrs.push(Instr::IMov(
                Val::VReg(Reg::RAX),
                Val::VRegOff(Reg::RSP, *id_stack_offset),
            ));
        }
        Expr::UnOp(Op1::Add1, e) => {
            let mut e_instrs = compile_expr(e, ctxt, label_ctr);
            instrs.append(&mut e_instrs);

            // Check that the result of e is a number
            instrs.push(Instr::IMov(Val::VReg(Reg::RBX), Val::VReg(Reg::RAX)));
            instrs.push(Instr::INot(Val::VReg(Reg::RBX)));
            instrs.push(Instr::IAnd(Val::VReg(Reg::RBX), Val::VImm(1)));
            instrs.push(Instr::ICmp(Val::VReg(Reg::RBX), Val::VImm(1)));
            instrs.push(Instr::IJne(INVALID_TYPE_LABEL.to_string()));

            // Increment
            instrs.push(Instr::IAdd(Val::VReg(Reg::RAX), Val::VImm(1 << 1)));

            // Check for overflow
            instrs.append(&mut compile_num_overflow_instrs());
        }
        Expr::UnOp(Op1::Sub1, e) => {
            let mut e_instrs = compile_expr(e, ctxt, label_ctr);
            instrs.append(&mut e_instrs);

            // Check that the result of e is a number
            instrs.push(Instr::IMov(Val::VReg(Reg::RBX), Val::VReg(Reg::RAX)));
            instrs.push(Instr::INot(Val::VReg(Reg::RBX)));
            instrs.push(Instr::IAnd(Val::VReg(Reg::RBX), Val::VImm(1)));
            instrs.push(Instr::ICmp(Val::VReg(Reg::RBX), Val::VImm(1)));
            instrs.push(Instr::IJne(INVALID_TYPE_LABEL.to_string()));

            // Decrement
            instrs.push(Instr::ISub(Val::VReg(Reg::RAX), Val::VImm(1 << 1)));

            // Check for overflow
            instrs.append(&mut compile_num_overflow_instrs());
        }
        Expr::UnOp(Op1::IsNum, e) => {
            let mut e_instrs = compile_expr(e, ctxt, label_ctr);
            instrs.append(&mut e_instrs);
            // Check the tag bit: and ~RAX, 1 = 1 only if RAX is a number
            instrs.push(Instr::IMov(Val::VReg(Reg::RBX), Val::VReg(Reg::RAX)));
            instrs.push(Instr::INot(Val::VReg(Reg::RBX)));
            instrs.push(Instr::IAnd(Val::VReg(Reg::RBX), Val::VImm(1)));

            // Set RAX to False by default
            instrs.push(Instr::IMov(Val::VReg(Reg::RAX), Val::VImm(FALSE_INT)));

            // Check AND result and move true into RAX if e is a number.
            instrs.push(Instr::ICmp(Val::VReg(Reg::RBX), Val::VImm(1)));
            instrs.push(Instr::IMov(Val::VReg(Reg::RBX), Val::VImm(TRUE_INT)));
            instrs.push(Instr::ICMove(Val::VReg(Reg::RAX), Val::VReg(Reg::RBX)));
        }
        Expr::UnOp(Op1::IsBool, e) => {
            let mut e_instrs = compile_expr(e, ctxt, label_ctr);
            instrs.append(&mut e_instrs);
            // Check the tag bit: and rax, 1 = 1 only if rax is a Boolean
            instrs.push(Instr::IMov(Val::VReg(Reg::RBX), Val::VImm(1)));
            instrs.push(Instr::IAnd(Val::VReg(Reg::RBX), Val::VReg(Reg::RAX)));

            // Set RAX to False by default
            instrs.push(Instr::IMov(Val::VReg(Reg::RAX), Val::VImm(FALSE_INT)));

            // Check AND result and move true into RAX if e is a number.
            instrs.push(Instr::ICmp(Val::VReg(Reg::RBX), Val::VImm(1)));
            instrs.push(Instr::IMov(Val::VReg(Reg::RBX), Val::VImm(TRUE_INT)));
            instrs.push(Instr::ICMove(Val::VReg(Reg::RAX), Val::VReg(Reg::RBX)));
        }

        Expr::BinOp(op @ (Op2::Plus | Op2::Minus | Op2::Times), e1, e2) => {
            let stack_offset: i64 = ctxt.si * 8;
            let mut e1_instrs = compile_expr(e1, ctxt, label_ctr);
            let mut e2_instrs = compile_expr(
                e2,
                &Context {
                    si: ctxt.si + 1,
                    ..*ctxt
                },
                label_ctr,
            );

            // e1 instructions
            instrs.append(&mut e1_instrs);
            // If e1 didn't evaluate to a number, jump to error code
            instrs.push(Instr::ITest(Val::VReg(Reg::RAX), Val::VImm(FALSE_INT)));
            instrs.push(Instr::IJnz(INVALID_TYPE_LABEL.to_string()));

            // Save result of e1_instrs on stack
            instrs.push(Instr::IMov(
                Val::VRegOff(Reg::RSP, stack_offset),
                Val::VReg(Reg::RAX),
            ));

            // e2 instructions
            instrs.append(&mut e2_instrs);

            // If e2 didn't evaluate to a number, jump to error code
            instrs.push(Instr::ITest(Val::VReg(Reg::RAX), Val::VImm(FALSE_INT)));
            instrs.push(Instr::IJnz(INVALID_TYPE_LABEL.to_string()));

            match op {
                Op2::Plus => {
                    instrs.push(Instr::IAdd(
                        Val::VReg(Reg::RAX),
                        Val::VRegOff(Reg::RSP, stack_offset),
                    ));
                }
                Op2::Minus => {
                    // Move result of e2 from rax into rbx
                    instrs.push(Instr::IMov(Val::VReg(Reg::RBX), Val::VReg(Reg::RAX)));

                    // Move result of e1 from stack into rax
                    instrs.push(Instr::IMov(
                        Val::VReg(Reg::RAX),
                        Val::VRegOff(Reg::RSP, stack_offset),
                    ));
                    // Do [rax] - [rbx]
                    instrs.push(Instr::ISub(Val::VReg(Reg::RAX), Val::VReg(Reg::RBX)));
                }
                Op2::Times => {
                    // For multiplication, shift either the result of e1 or e2 right 1 bit.
                    instrs.push(Instr::ISar(Val::VReg(Reg::RAX), Val::VImm(1)));

                    instrs.push(Instr::IMul(
                        Val::VReg(Reg::RAX),
                        Val::VRegOff(Reg::RSP, stack_offset),
                    ));
                }
                _ => panic!("Should not panic here: {op:?}"),
            }

            // Check for overflow
            instrs.append(&mut compile_num_overflow_instrs());
        }

        Expr::BinOp(
            op @ (Op2::Equal | Op2::Greater | Op2::GreaterEqual | Op2::Less | Op2::LessEqual),
            e1,
            e2,
        ) => {
            let stack_offset: i64 = ctxt.si * 8;
            let mut e1_instrs = compile_expr(e1, ctxt, label_ctr);
            let mut e2_instrs = compile_expr(
                e2,
                &Context {
                    si: ctxt.si + 1,
                    ..*ctxt
                },
                label_ctr,
            );

            instrs.append(&mut e1_instrs);

            // Save result of e1_instrs on stack
            instrs.push(Instr::IMov(
                Val::VRegOff(Reg::RSP, stack_offset),
                Val::VReg(Reg::RAX),
            ));

            instrs.append(&mut e2_instrs);

            // Move RBX into RAX based on the result of the CMP
            match op {
                Op2::Equal => {
                    // Move the result of e2 into RBX for the type check
                    instrs.push(Instr::IMov(Val::VReg(Reg::RBX), Val::VReg(Reg::RAX)));

                    // Check whether the tag bits are equal
                    instrs.push(Instr::IXor(
                        Val::VReg(Reg::RBX),
                        Val::VRegOff(Reg::RSP, stack_offset),
                    ));
                    instrs.push(Instr::ITest(Val::VReg(Reg::RBX), Val::VImm(FALSE_INT)));

                    // If the tag bits are unequal, jump to the error handler
                    instrs.push(Instr::IJne(INVALID_TYPE_LABEL.to_string()));

                    // Compare the results of e1 and e2
                    instrs.push(Instr::ICmp(
                        Val::VReg(Reg::RAX),
                        Val::VRegOff(Reg::RSP, stack_offset),
                    ));

                    // Move true into RBX
                    instrs.push(Instr::IMov(Val::VReg(Reg::RBX), Val::VImm(TRUE_INT)));

                    // Move false into RAX
                    instrs.push(Instr::IMov(Val::VReg(Reg::RAX), Val::VImm(FALSE_INT)));
                    instrs.push(Instr::ICMove(Val::VReg(Reg::RAX), Val::VReg(Reg::RBX)));
                }
                Op2::Greater => {
                    instrs.append(&mut compile_ineq_prelude(ctxt));
                    instrs.push(Instr::ICMovg(Val::VReg(Reg::RAX), Val::VReg(Reg::RBX)))
                }
                Op2::GreaterEqual => {
                    instrs.append(&mut compile_ineq_prelude(ctxt));
                    instrs.push(Instr::ICMovge(Val::VReg(Reg::RAX), Val::VReg(Reg::RBX)))
                }
                Op2::Less => {
                    instrs.append(&mut compile_ineq_prelude(ctxt));
                    instrs.push(Instr::ICMovl(Val::VReg(Reg::RAX), Val::VReg(Reg::RBX)));
                }
                Op2::LessEqual => {
                    instrs.append(&mut compile_ineq_prelude(ctxt));
                    instrs.push(Instr::ICMovle(Val::VReg(Reg::RAX), Val::VReg(Reg::RBX)));
                }
                _ => panic!("Should never panic here: {op:?}"),
            }
        }

        Expr::Let(bindings, body) => {
            let mut new_env: HashMap<String, i64> = ctxt.env.clone();
            let mut locally_bound_ids: HashSet<String> = HashSet::new();

            for (index, (id, e)) in bindings.iter().enumerate() {
                if locally_bound_ids.contains(id) {
                    panic!("Duplicate binding");
                }

                let id_stack_offset = i64::try_from(index).unwrap();
                let id_stack_index = ctxt.si + id_stack_offset;

                // Compile the instructions of the let binding.
                let new_ctxt = Context {
                    si: id_stack_index,
                    env: &new_env,
                    ..*ctxt
                };
                let mut e_instrs = compile_expr(e, &new_ctxt, label_ctr);
                instrs.append(&mut e_instrs);

                // Store the let-binded variable on the stack
                instrs.push(Instr::IMov(
                    Val::VRegOff(Reg::RSP, id_stack_index * 8),
                    Val::VReg(Reg::RAX),
                ));

                // Track which identifiers have been bound locally.
                locally_bound_ids = locally_bound_ids.update(id.to_string());

                // Update the environment mapping of identifier -> memory location.
                // IMPORTANT: This must be done after compiling the let expression.
                new_env = new_env.update(id.to_string(), id_stack_index * 8);
            }

            // The body is offset by the number of let bindings at the top level.
            let body_stack_index = ctxt.si + i64::try_from(bindings.len()).unwrap();
            let new_ctxt = Context {
                si: body_stack_index,
                env: &new_env,
                ..*ctxt
            };

            let mut body_instrs = compile_expr(body, &new_ctxt, label_ctr);
            instrs.append(&mut body_instrs);
        }

        Expr::If(cond, then_expr, else_expr) => {
            let mut cond_instrs = compile_expr(cond, ctxt, label_ctr);
            let mut then_instrs = compile_expr(then_expr, ctxt, label_ctr);
            let mut else_instrs = compile_expr(else_expr, ctxt, label_ctr);
            let end_label = get_new_label(label_ctr, "ifend");
            let else_label = get_new_label(label_ctr, "ifelse");

            // Evaluate the condition
            instrs.append(&mut cond_instrs);

            // If the condition evaluated to false, jump to the else branch.
            instrs.push(Instr::ICmp(Val::VReg(Reg::RAX), Val::VImm(FALSE_INT)));
            instrs.push(Instr::IJe(else_label.clone()));

            // If the condition evaluated to any other value, continue on with the then branch.
            instrs.append(&mut then_instrs);
            // Jump to the end of the if statement
            instrs.push(Instr::IJmp(end_label.clone()));

            // Insert the else branch label
            instrs.push(Instr::ILabel(else_label.clone()));
            instrs.append(&mut else_instrs);

            // Insert the end of the if statement label
            instrs.push(Instr::ILabel(end_label.clone()));
        }
        Expr::Block(exprs) => {
            for e in exprs.iter() {
                instrs.append(&mut compile_expr(e, ctxt, label_ctr));
            }
        }
        Expr::Set(name, e) => {
            let variable_loc = match ctxt.env.get(name) {
                Some(offset) => *offset,
                None => panic!("Unbound variable identifier {name}"),
            };

            // Evaluate expression
            let mut e_instrs = compile_expr(e, ctxt, label_ctr);
            instrs.append(&mut e_instrs);
            // Update value of variable
            instrs.push(Instr::IMov(
                Val::VRegOff(Reg::RSP, variable_loc),
                Val::VReg(Reg::RAX),
            ));
        }

        Expr::Loop(e) => {
            let start_label = get_new_label(label_ctr, "loop");
            let end_label = get_new_label(label_ctr, "endloop");
            let mut e_instrs = compile_expr(
                e,
                &Context {
                    brake: &end_label,
                    ..*ctxt
                },
                label_ctr,
            );
            instrs.push(Instr::ILabel(start_label.clone()));
            instrs.append(&mut e_instrs);
            instrs.push(Instr::IJmp(start_label.clone()));
            instrs.push(Instr::ILabel(end_label.clone()));
            // The result is in RAX
        }
        Expr::Break(e) => {
            // If the break label isn't defined, report an error
            if ctxt.brake.is_empty() {
                panic!("Error: break without surrounding loop");
            }

            let mut e_instrs = compile_expr(e, ctxt, label_ctr);
            instrs.append(&mut e_instrs);

            // Jump to endloop label
            instrs.push(Instr::IJmp(ctxt.brake.to_string()));
        }
    }
    return instrs;
}

// Get the instructions for the error handler associated with the given error code
fn get_error_instrs(errcode: i64) -> Vec<Instr> {
    let mut instrs: Vec<Instr> = Vec::new();

    match errcode {
        ERR_NUM_OVERFLOW => {
            instrs.push(Instr::ILabel(NUM_OVERFLOW_LABEL.to_string()));
        }
        ERR_INVALID_TYPE => {
            instrs.push(Instr::ILabel(INVALID_TYPE_LABEL.to_string()));
        }

        _ => panic!("Unknown error code: {errcode}"),
    }

    // Pass error code as first function argument to snek_error
    instrs.push(Instr::IMov(Val::VReg(Reg::RDI), Val::VImm(errcode)));

    // Save stack pointer of current function onto stack
    instrs.push(Instr::IPush(Val::VReg(Reg::RSP)));

    // Call snek_error
    instrs.push(Instr::ICall("snek_error".to_string()));

    return instrs;
}

// Generates and formats error handling code
pub fn get_error_instr_code() -> String {
    let mut error_handler_strs: Vec<String> = Vec::new();

    error_handler_strs.push(instr_to_str(&Instr::NoInstr()));

    let error_instrs = &mut get_error_instrs(ERR_NUM_OVERFLOW);
    let str_error_instrs: Vec<String> = error_instrs.iter().map(instr_to_str).collect();
    error_handler_strs.push(str_error_instrs.join("\n\t\t"));

    let error_instrs = &mut get_error_instrs(ERR_INVALID_TYPE);
    let str_error_instrs: Vec<String> = error_instrs.iter().map(instr_to_str).collect();
    error_handler_strs.push(str_error_instrs.join("\n\t\t"));

    return error_handler_strs.join("\n\t");
}
