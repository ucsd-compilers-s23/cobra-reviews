pub mod assembly;
use assembly::{Instruction::*, Register::*, Val::*, *};

use im::{HashMap, HashSet};
use rayon::prelude::*;
use sexp::{Atom::*, Sexp::*, *};

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
    /// A literal number. Must be a 63-bit signed integer.
    /// Does not check the range of the number.
    /// There is a ranged_integer crate, but it requires nightly.
    Number(i64),
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

fn new_label(counter: &mut u32, prefix: &str) -> String {
    let current = *counter;
    *counter += 1;
    format!("{}_{}", prefix, current)
}

/// Parses a S-expression into the internal representation of the program.
fn parse_expr(sexp: &Sexp) -> Expr {
    #[cfg(debug_assertions)]
    println!("Parsing: {:?}", sexp);

    match sexp {
        Atom(a) => match a {
            I(n) => Expr::Number(*n),
            S(s) => match s.as_str() {
                "true" => Expr::Boolean(true),
                "false" => Expr::Boolean(false),
                "let" | "if" | "loop" | "break" | "set!" | "block" | "add1" | "sub1" | "isnum"
                | "isbool" => {
                    panic!("Keyword cannot be used as identifier: {}", s)
                }
                _ => Expr::Id(s.clone()),
            },
            _ => panic!("Invalid atom type: {}", a),
        },
        List(l) => match l.as_slice() {
            [Atom(S(op)), cond, then, els] if op == "if" => Expr::If(
                Box::new(parse_expr(cond)),
                Box::new(parse_expr(then)),
                Box::new(parse_expr(els)),
            ),
            [Atom(S(op)), body] if op == "loop" => Expr::Loop(Box::new(parse_expr(body))),
            [Atom(S(op)), body] if op == "break" => Expr::Break(Box::new(parse_expr(body))),
            [Atom(S(op)), Atom(S(id)), expr] if op == "set!" => {
                Expr::Set(id.clone(), Box::new(parse_expr(expr)))
            }
            [Atom(S(op)), exprs @ ..] if op == "block" => {
                if exprs.is_empty() {
                    panic!("Invalid block: empty {}", sexp)
                }

                Expr::Block(exprs.par_iter().map(parse_expr).collect())
            }
            [Atom(S(op)), List(bindings), body] if op == "let" => {
                let bindings = bindings
                    .par_iter()
                    .map(|binding| match binding {
                        List(vec) => {
                            // Parse identifier
                            let id = match &vec[0] {
                                Atom(S(s)) => s.to_string(),
                                _ => panic!("Invalid identifier {} in binding {}", &vec[0], sexp),
                            };

                            // Check if identifier is a keyword
                            match id.as_str() {
                                "true" | "false" | "input" | "add1" | "sub1" | "isnum"
                                | "isbool" | "let" | "if" | "loop" | "break" | "set!" | "block" => {
                                    panic!("Identifier cannot be a keyword: {} in {}", id, sexp)
                                }
                                _ => (),
                            }

                            let expr = parse_expr(&vec[1]);
                            (id, expr)
                        }
                        _ => panic!("Invalid binding format: {:?} in {}", binding, sexp),
                    })
                    .collect::<Vec<(String, Expr)>>();

                // Error if there are no bindings
                if bindings.is_empty() {
                    panic!("Invalid let: No bindings in {}", sexp);
                }

                Expr::Let(bindings, Box::new(parse_expr(body)))
            }
            [Atom(S(op)), expr] => {
                let op = match op.as_str() {
                    "add1" => Op1::Add1,
                    "sub1" => Op1::Sub1,
                    "isnum" => Op1::IsNum,
                    "isbool" => Op1::IsBool,
                    _ => panic!("Invalid unary operator: {}", op),
                };
                Expr::UnOp(op, Box::new(parse_expr(expr)))
            }
            [Atom(S(op)), expr1, expr2] => {
                let op = match op.as_str() {
                    "+" => Op2::Plus,
                    "-" => Op2::Minus,
                    "*" => Op2::Times,
                    "=" => Op2::Equal,
                    ">" => Op2::Greater,
                    ">=" => Op2::GreaterEqual,
                    "<" => Op2::Less,
                    "<=" => Op2::LessEqual,
                    _ => panic!("Invalid binary operator: {}", op),
                };
                Expr::BinOp(op, Box::new(parse_expr(expr1)), Box::new(parse_expr(expr2)))
            }
            _ => panic!("Invalid expression: {}", sexp),
        },
    }
}

/// Compiles a Cobra program from the internal representation into an instruction sequence.
fn compile_expr(
    expr: &Expr,
    si: i64,
    env: &HashMap<String, i64>,
    label_counter: &mut u32,
    break_label: &Option<String>,
) -> InstructionSequence {
    #[cfg(debug_assertions)]
    println!("Compiling: {:?}; si: {}", expr, si);

    match expr {
        Expr::Number(n) => {
            let shifted_n = n << 1;

            if shifted_n >> 1 != *n {
                panic!("Invalid integer - Must be 63-bit signed integer: {}", n);
            }

            let mut seq = InstructionSequence::new();
            seq.push(Mov(Register(Rax), Immediate(shifted_n)));
            seq
        }
        Expr::Boolean(b) => {
            let value = if *b { 0b11 } else { 0b01 };
            let mut seq = InstructionSequence::new();
            seq.push(Mov(Register(Rax), Immediate(value)));
            seq
        }
        Expr::Id(id) => {
            let offset = env.get(id).unwrap_or_else(|| {
                panic!("Unbound variable identifier {}", id);
            });

            let mut seq = InstructionSequence::new();
            seq.push(Mov(Register(Rax), RegOffset(Rsp, -*offset * 8)));
            seq
        }
        Expr::Let(bindings, body) => {
            let mut seq = InstructionSequence::new();

            // Keep track of the stack index for each binding
            let mut si = si;
            // Use local HashSet to check for duplicate bindings
            let mut seen = HashSet::new();
            // Local environment for recursive calls
            let mut new_env = env.clone();

            for (id, expr) in bindings {
                if seen.contains(id) {
                    panic!("Duplicate binding: {}", id);
                }

                let binding_instrs = compile_expr(expr, si, &new_env, label_counter, break_label);
                seen.insert(id);
                new_env.insert(id.clone(), si);
                seq.extend(binding_instrs);

                // Move the result of the expression into the stack
                seq.push(Mov(RegOffset(Rsp, -si * 8), Register(Rax)));

                si += 1;
            }

            #[cfg(debug_assertions)]
            println!("New environment: {:?}", new_env);

            // Body
            seq.extend(compile_expr(body, si, &new_env, label_counter, break_label));

            seq
        }
        Expr::UnOp(op, expr1) => match op {
            Op1::Add1 => {
                let mut seq = compile_expr(expr1, si, env, label_counter, break_label);

                // Check that expr1 does not evaluate to a boolean
                seq.push(Test(Register(Rax), Immediate(0b01)));
                seq.push(Jcc(CC::Nz, Label("throw_invalid_arg".to_string())));

                // Add
                seq.push(Add(Register(Rax), Immediate(2)));

                // Check for overflow
                seq.push(Jcc(CC::O, Label("throw_overflow".to_string())));

                seq
            }
            Op1::Sub1 => {
                let mut seq = compile_expr(expr1, si, env, label_counter, break_label);

                // Check that expr1 does not evaluate to a boolean
                seq.push(Test(Register(Rax), Immediate(0b01)));
                seq.push(Jcc(CC::Nz, Label("throw_invalid_arg".to_string())));

                // Subtract
                seq.push(Sub(Register(Rax), Immediate(2)));

                // Check for overflow
                seq.push(Jcc(CC::O, Label("throw_overflow".to_string())));

                seq
            }
            Op1::IsNum => {
                let mut seq = compile_expr(expr1, si, env, label_counter, break_label);

                // Set flags based on LSB of Rax
                seq.push(And(Register(Rax), Immediate(0b01)));

                // Set Rax and Rbs with false and true
                seq.push(Mov(Register(Rax), Immediate(0b01))); // false
                seq.push(Mov(Register(Rbx), Immediate(0b11))); // true

                // Set Rax to true if the LSB is 0
                seq.push(Cmovcc(CC::Z, Rax, Rbx));
                seq
            }
            Op1::IsBool => {
                let mut seq = compile_expr(expr1, si, env, label_counter, break_label);

                // Set flags based on current value of Rax
                seq.push(Test(Register(Rax), Immediate(0b01)));

                // Set Rax and Rbs with false and true
                seq.push(Mov(Register(Rax), Immediate(0b01))); // false
                seq.push(Mov(Register(Rbx), Immediate(0b11))); // true

                // Set Rax to true if the parity is odd
                seq.push(Cmovcc(CC::Nz, Rax, Rbx));
                seq
            }
        },
        Expr::BinOp(op, expr1, expr2) => {
            match op {
                // Numeric operators
                Op2::Plus
                | Op2::Minus
                | Op2::Times
                | Op2::Greater
                | Op2::Less
                | Op2::GreaterEqual
                | Op2::LessEqual => {
                    // Compile the expressions
                    let expr1_instr = compile_expr(expr1, si + 1, env, label_counter, break_label);
                    let expr2_instr = compile_expr(expr2, si, env, label_counter, break_label);

                    let stack_offset = si * 8;

                    // Instructions for putting the results on the stack/rax
                    let mut seq = expr2_instr;

                    // Check that expr2 does not evaluate to a boolean
                    seq.push(Test(Register(Rax), Immediate(0b01)));
                    seq.push(Jcc(CC::Nz, Label("throw_invalid_arg".to_string())));

                    seq.push(Mov(RegOffset(Rsp, -stack_offset), Register(Rax)));
                    seq.extend(expr1_instr);

                    // Check that expr1 does not evaluate to a boolean
                    seq.push(Test(Register(Rax), Immediate(0b01)));
                    seq.push(Jcc(CC::Nz, Label("throw_invalid_arg".to_string())));

                    // Operator-specific instructions
                    match op {
                        Op2::Plus => seq.push(Add(Register(Rax), RegOffset(Rsp, -stack_offset))),
                        Op2::Minus => seq.push(Sub(Register(Rax), RegOffset(Rsp, -stack_offset))),
                        Op2::Times => {
                            // Shift Rax left by 1 then perform multiplication
                            seq.push(Sar(Register(Rax), Immediate(1)));
                            seq.push(Imul(Register(Rax), RegOffset(Rsp, -stack_offset)));
                        }
                        Op2::Greater | Op2::Less | Op2::GreaterEqual | Op2::LessEqual => {
                            // Compare the two values
                            seq.push(Cmp(Register(Rax), RegOffset(Rsp, -stack_offset)));

                            // Set Rax and Rbx with false and true
                            seq.push(Mov(Register(Rax), Immediate(0b01))); // false
                            seq.push(Mov(Register(Rbx), Immediate(0b11))); // true

                            // Set the condition code based on the operator
                            let cc = match op {
                                Op2::Greater => CC::G,
                                Op2::Less => CC::L,
                                Op2::GreaterEqual => CC::Ge,
                                Op2::LessEqual => CC::Le,
                                _ => panic!("Operator {:?} not allowed or unimplemented", op),
                            };

                            // Set Rax to true if the condition is met
                            seq.push(Cmovcc(cc, Rax, Rbx));
                        }
                        _ => panic!("Operator {:?} not allowed or unimplemented", op),
                    }

                    // Check for overflow if not a comparison operator
                    match op {
                        Op2::Greater | Op2::Less | Op2::GreaterEqual | Op2::LessEqual => {}
                        _ => seq.push(Jcc(CC::O, Label("throw_overflow".to_string()))),
                    }

                    seq
                }
                Op2::Equal => {
                    // Evaluates the expressions
                    // Error if they are not the same type

                    // Compile the expressions
                    let expr1_instr = compile_expr(expr1, si, env, label_counter, break_label);
                    let expr2_instr = compile_expr(expr2, si + 1, env, label_counter, break_label);

                    let stack_offset = si * 8;

                    // Start with the instructions for expr1
                    let mut seq = expr1_instr;
                    seq.push(Mov(RegOffset(Rsp, -stack_offset), Register(Rax)));

                    // Extract LSB of Rax to Rbx
                    seq.push(Mov(Register(Rbx), Register(Rax)));
                    seq.push(And(Register(Rbx), Immediate(0b01)));

                    // Add the instructions for expr2
                    seq.extend(expr2_instr);

                    // Extract LSB of Rax to Rcx
                    seq.push(Mov(Register(Rcx), Register(Rax)));
                    seq.push(And(Register(Rcx), Immediate(0b01)));

                    // Check that the types are the same by comparing LSBs
                    seq.push(Cmp(Register(Rbx), Register(Rcx)));
                    seq.push(Jcc(CC::Ne, Label("throw_invalid_arg".to_string())));

                    // Check if the values are equal and set Rax to true/false
                    seq.push(Cmp(Register(Rax), RegOffset(Rsp, -stack_offset)));
                    seq.push(Mov(Register(Rax), Immediate(0b01))); // false
                    seq.push(Mov(Register(Rbx), Immediate(0b11))); // true
                    seq.push(Cmovcc(CC::E, Rax, Rbx));

                    seq
                }
            }
        }
        Expr::If(cond, then, els) => {
            let end_label = new_label(label_counter, "ifend");
            let else_label = new_label(label_counter, "ifelse");

            let cond_instr = compile_expr(cond, si, env, label_counter, break_label);
            let then_instr = compile_expr(then, si, env, label_counter, break_label);
            let else_instr = compile_expr(els, si, env, label_counter, break_label);

            let mut seq = cond_instr;

            seq.push(Cmp(Register(Rax), Immediate(0b01)));

            seq.push(Jcc(CC::E, Label(else_label.clone())));
            {
                seq.extend(then_instr);
            }
            seq.push(Jmp(Label(end_label.clone())));

            seq.push(LabelInstr(Label(else_label)));
            {
                seq.extend(else_instr);
            }
            seq.push(LabelInstr(Label(end_label)));

            seq
        }
        Expr::Loop(body) => {
            let mut seq = InstructionSequence::new();

            let loop_label = new_label(label_counter, "loop");
            let end_label = new_label(label_counter, "endloop");

            seq.push(LabelInstr(Label(loop_label.clone())));
            {
                seq.extend(compile_expr(
                    body,
                    si,
                    env,
                    label_counter,
                    &Some(end_label.clone()),
                ));
                seq.push(Jmp(Label(loop_label)));
            }
            seq.push(LabelInstr(Label(end_label)));

            seq
        }
        Expr::Break(expr) => match break_label {
            Some(label) => {
                let mut seq = compile_expr(expr, si, env, label_counter, break_label);
                seq.push(Jmp(Label(label.to_string())));
                seq
            }
            None => panic!("Cannot break outside of loop"),
        },
        Expr::Set(id, expr) => {
            if id == "input" {
                panic!("Invalid set!: cannot assign to input");
            }
            let offset = env
                .get(id)
                .unwrap_or_else(|| panic!("Unbound variable identifier {}", id));

            let mut seq = compile_expr(expr, si, env, label_counter, break_label);

            seq.push(Mov(RegOffset(Rsp, -offset * 8), Register(Rax)));

            seq
        }
        Expr::Block(vec) => {
            let mut seq = InstructionSequence::new();

            for expr in vec {
                seq.extend(compile_expr(expr, si, env, label_counter, break_label));
            }

            seq
        }
    }
}

/// Compiles a Cobra program into x86-64 assembly
pub fn compile(code: &str) -> String {
    let env = HashMap::unit("input".to_string(), 2);
    let mut label_counter = 0;

    // Parse and compile the input file into a vector of assembly instructions
    let sexp = parse(code).unwrap_or_else(|e| panic!("Invalid S-Expression: {}", e));
    let expr = parse_expr(&sexp);
    let assembly_code = compile_expr(&expr, 3, &env, &mut label_counter, &None);

    let assembly_code = format!(
        "
section .text
extern snek_error
global our_code_starts_here

; Throw an overflow error with the value in RAX
throw_overflow:
\tmov rdi, 1
\tmov rsi, rax
\tpush rsp
\tcall snek_error

; Throw invalid argument error
throw_invalid_arg:
\tmov rdi, 2
\tmov rsi, rax
\tpush rsp
\tcall snek_error

; Entry point. Input value is in RDI
our_code_starts_here:
\tmov [rsp - 16], rdi ; Save the input value

{}

\tret
",
        assembly_code
    );

    assembly_code
}
