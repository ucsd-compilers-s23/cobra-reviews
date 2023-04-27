use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::error::Error;

use sexp::Atom::*;
use sexp::*;

use std::collections::HashSet;
use im::HashMap;

const ERROR_LABEL: &str = "snek_error";

trait Assembly {
    fn out(&self) -> String;
}

pub enum CobraError {
    TypeMismatch,
}

impl Assembly for bool {
    fn out(&self) -> String {
        match self {
            true => format!("{}", 3),
            false => format!("{}", 1),
        }
    }
}

#[derive(Debug, Clone)]
enum Val {
    Reg(Reg),
    Imm(i32),
    RegOffset(Reg, i32),
    Bool(bool)
}

impl Assembly for Val {
    fn out(&self) -> String {
        match self {
            Val::Reg(reg) => reg.out(),
            Val::Imm(val) => format!("{}", val),
            Val::RegOffset(reg, val) => format!("qword [{} - {}]", reg.out(), val),
            Val::Bool(b) => b.out()
        }
    }
}

#[derive(Debug, Clone)]
enum Reg {
    RAX,
    RBX,
    RCX,

    RSP,
    RDI,
}

impl Assembly for Reg {
    fn out(&self) -> String {
        match self {
            Reg::RAX => "rax",
            Reg::RBX => "rbx",
            Reg::RCX => "rcx",
            Reg::RSP => "rsp",
            Reg::RDI => "rdi",
        }.to_string()
    }
}

#[derive(Debug, Clone)]
enum Jump {
    JMP,
    JNE,
    JE,
    JGE,
    JLE,
    JNZ,
}

impl Assembly for Jump {
    fn out(&self) -> String {
        match self {
            Jump::JMP => format!("jmp"),
            Jump::JNE => format!("jne"),
            Jump::JE => format!("je"),
            Jump::JGE => format!("jge"),
            Jump::JLE => format!("jle"),
            Jump::JNZ => format!("jnz"),
        }
    }
}

#[derive(Debug, Clone)]
enum CMov {
    CMOVE,
    CMOVG,
    CMOVGE,
    CMOVL,
    CMOVLE,
}

impl Assembly for CMov {
    fn out(&self) -> String {
        match self {
            CMov::CMOVE => format!("cmove"),
            CMov::CMOVG => format!("cmovg"),
            CMov::CMOVGE => format!("cmovge"),
            CMov::CMOVL => format!("cmovl"),
            CMov::CMOVLE => format!("cmovle"),
        }
    }
}

#[derive(Debug, Clone)]
enum Instr {
    IMov(Val, Val),
    CMov(CMov, Val, Val),

    IAdd(Val, Val),
    ISub(Val, Val),
    INeg(Val),
    IMul(Val, Val),

    And(Val, Val),
    Or(Val, Val),
    Xor(Val, Val),

    Label(String),
    Cmp(Val, Val),
    Test(Val, Val),
    Jump(Jump, String),

    Call(String),
    PushRSP,

    Shl(Val),
    Sar(Val),
    Shr(Val),
}

impl Assembly for Instr {
    fn out(&self) -> String {
        match self {
            Instr::IMov(v1, v2) => format!("\tmov {}, {}", v1.out(), v2.out()),
            Instr::CMov(t, v1, v2) => format!("\t{} {}, {}", t.out(), v1.out(), v2.out()),

            Instr::IAdd(v1, v2) => format!("\tadd {}, {}", v1.out(), v2.out()),
            Instr::ISub(v1, v2) => format!("\tsub {}, {}", v1.out(), v2.out()),

            Instr::INeg(v1) => format!("\tneg {}", v1.out()),

            Instr::IMul(v1, v2) => format!("\timul {}, {}", v1.out(), v2.out()),

            Instr::And(v1, v2) => format!("\tand {}, {}", v1.out(), v2.out()),
            Instr::Or(v1, v2) => format!("\tor {}, {}", v1.out(), v2.out()),
            Instr::Xor(v1, v2) => format!("\txor {}, {}", v1.out(), v2.out()),

            Instr::Label(v) => format!("{}:", v),
            Instr::Cmp(v1, v2) => format!("\tcmp {}, {}", v1.out(), v2.out()),
            Instr::Test(v1, v2) => format!("\ttest {}, {}", v1.out(), v2.out()),
            Instr::Jump(jtype, lbl) => format!("\t{} {}", jtype.out(), lbl),

            Instr::Call(lbl) => format!("\tcall {}", lbl),
            Instr::PushRSP => format!("\tpush rsp"),

            Instr::Shl(v) => format!("\tshl {}, 1", v.out()),
            Instr::Sar(v) => format!("\tsar {}, 1", v.out()),
            Instr::Shr(v) => format!("\tshr {}, 1", v.out()),
        }
    }
}

enum Op1 { Add1, Sub1, Negate, IsNum, IsBool, }

enum Op2 { Plus, Minus, Times, Equal, Greater, GreaterEqual, Less, LessEqual, }

enum Expr {
    Number(i32),
    Boolean(bool),
    Id(String),
    Input,
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
    Set(String, Box<Expr>),
    Block(Vec<Expr>),
}

fn parse_binding(s: &Sexp) -> Result<(String, Expr), String> {
    match s {
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(var)), Sexp::Atom(I(val))] => Ok((var.to_string(), Expr::Number(i32::try_from(*val).unwrap()))),
                [Sexp::Atom(S(var)), expr] => Ok((var.to_string(), parse_expr(expr)?)),
                _ => Err(format!("Invalid: parse error"))
            }
        },
        _ => Err(format!("Invalid: parse error")),
    }
}

fn parse_expr(s: &Sexp) -> Result<Expr, String> {
    match s {
        Sexp::Atom(I(n)) => Ok(Expr::Number(i32::try_from(*n).unwrap())),
        Sexp::Atom(S(v)) => {
            match v.as_str() {
                "true" => Ok(Expr::Boolean(true)),
                "false" => Ok(Expr::Boolean(false)),
                "input" => Ok(Expr::Input),
                _ => Ok(Expr::Id(v.clone()))
            }
        },
        Sexp::List(vec) => match &vec[..] {
            // Unary Operators
            [Sexp::Atom(S(op)), e] if op == "add1" => Ok(Expr::UnOp(Op1::Add1, Box::new(parse_expr(e)?))),
            [Sexp::Atom(S(op)), e] if op == "sub1" => Ok(Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e)?))),
            [Sexp::Atom(S(op)), e] if op == "negate" => Ok(Expr::UnOp(Op1::Negate, Box::new(parse_expr(e)?))),
            [Sexp::Atom(S(op)), e] if op == "isnum" => Ok(Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e)?))),
            [Sexp::Atom(S(op)), e] if op == "isbool" => Ok(Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e)?))),

            // Binary Operators
            [Sexp::Atom(S(op)), e1, e2] if op == "+" => Ok(Expr::BinOp(Op2::Plus, Box::new(parse_expr(e1)?), Box::new(parse_expr(e2)?))),
            [Sexp::Atom(S(op)), e1, e2] if op == "-" => Ok(Expr::BinOp(Op2::Minus, Box::new(parse_expr(e1)?), Box::new(parse_expr(e2)?))),
            [Sexp::Atom(S(op)), e1, e2] if op == "*" => Ok(Expr::BinOp(Op2::Times, Box::new(parse_expr(e1)?), Box::new(parse_expr(e2)?))),

            [Sexp::Atom(S(op)), e1, e2] if op == "=" => Ok(Expr::BinOp(Op2::Equal, Box::new(parse_expr(e1)?), Box::new(parse_expr(e2)?))),
            [Sexp::Atom(S(op)), e1, e2] if op == ">" => Ok(Expr::BinOp(Op2::Greater, Box::new(parse_expr(e1)?), Box::new(parse_expr(e2)?))),
            [Sexp::Atom(S(op)), e1, e2] if op == ">=" => Ok(Expr::BinOp(Op2::GreaterEqual, Box::new(parse_expr(e1)?), Box::new(parse_expr(e2)?))),
            [Sexp::Atom(S(op)), e1, e2] if op == "<" => Ok(Expr::BinOp(Op2::Less, Box::new(parse_expr(e1)?), Box::new(parse_expr(e2)?))),
            [Sexp::Atom(S(op)), e1, e2] if op == "<=" => Ok(Expr::BinOp(Op2::LessEqual, Box::new(parse_expr(e1)?), Box::new(parse_expr(e2)?))),

            // Bindings
            [Sexp::Atom(S(op)), Sexp::List(bindings), e] if op == "let" => {
                if bindings.is_empty() {
                    return Err(format!("Invalid: let with no bindings"));
                }
                let mut bound: Vec<(String, Expr)> = vec!();
                for binding in bindings {
                    bound.push(parse_binding(binding)?);
                }

                Ok(Expr::Let(bound, Box::new(parse_expr(e)?)))
            }
            [Sexp::Atom(S(op)), Sexp::Atom(S(var)), val] if op == "set!" => {
                Ok(Expr::Set(var.clone(), Box::new(parse_expr(val)?)))
            }

            // Conditionals
            [Sexp::Atom(S(op)), cond, ife, thene] if op == "if" => {
                Ok(Expr::If(
                    Box::new(parse_expr(cond)?),
                    Box::new(parse_expr(ife)?),
                    Box::new(parse_expr(thene)?)
                ))
            }

            // Loops
            [Sexp::Atom(S(op)), e] if op == "loop" => {
                Ok(Expr::Loop(Box::new(parse_expr(e)?)))
            }
            [Sexp::Atom(S(op)), e] if op == "break" => {
                Ok(Expr::Break(Box::new(parse_expr(e)?)))
            }

            // Blocks
            [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => {
                let mut es = vec!();
                for expr in exprs {
                    es.push(parse_expr(expr)?);
                }
                Ok(Expr::Block(es))
            }
            e => Err(format!("Invalid: parse error {:?}", e))
        }
        e => Err(format!("Invalid: parse error {:?}", e))
    }
}

static mut LABEL_COUNT: u64 = 0;

#[derive(Clone)]
struct Context {
    stack_index: i32,
    environment: HashMap<String, i32>,
    loop_stack: Vec<String>
}

impl Context {
    fn new() -> Self {
        Self {
            stack_index: 2,
            environment: HashMap::new(),
            loop_stack: vec!()
        }
    }

    fn stack(&self, offset: i32) -> Self {
        Self {
            stack_index: offset + self.stack_index,
            environment: self.environment.clone(),
            loop_stack: self.loop_stack.clone()
        }
    }

    fn label(&self, l: &str) -> String {
        unsafe {
            LABEL_COUNT += 1;
            format!("{}_{}", l, LABEL_COUNT - 1)
        }
    }

    fn push(&self, binding: (String, i32)) -> Result<Self, String> {
        let mut new_context: HashMap<String, i32> = self.environment.clone();
        let (var, val) = binding;
        new_context.insert(var, val);

        Ok(Self {
            environment: new_context,
            stack_index: self.stack_index + 1,
            loop_stack: self.loop_stack.clone()
        })
    }
}

enum Type {
    Number,
    Boolean,
}

fn call_error(insts: &mut Vec<Instr>, j_type: Jump, code: i32) {
    insts.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(code)));
    insts.push(Instr::Jump(j_type, "snek_error_wrapper".to_string()));
}

fn same_type_check(insts: &mut Vec<Instr>, v1: Val, v2: Val) {
    insts.push(Instr::IMov(Val::Reg(Reg::RBX), v1));
    insts.push(Instr::Xor(Val::Reg(Reg::RBX), v2));
    insts.push(Instr::Test(Val::Reg(Reg::RBX), Val::Imm(1)));

    call_error(insts, Jump::JNE, 2);
}

fn type_check(insts: &mut Vec<Instr>, val: Val, typ: Type) {
    let mut type_check: i32 = 1;
    if let Type::Boolean = typ {
        type_check = 0;
    }

    insts.push(Instr::Test(val, Val::Imm(type_check)));

    call_error(insts, Jump::JNZ, 2);
}

fn compile_expr(e: &Expr, c: &mut Context) -> Result<Vec<Instr>, String> {
    let mut insts: Vec<Instr> = Vec::new();
    match e {
        Expr::Number(n) => {
            insts.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n << 1)));
        }
        Expr::Boolean(b) => {
            insts.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Bool(*b)));
        },
        Expr::Id(var) => {
            match c.environment.get(var) {
                None => {
                    return Err(format!("Unbound variable identifier {}", var));
                }
                Some(&offset) => {
                    insts.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset)))
                }
            }
        }
        Expr::Let(bindings, expr) => {
            let mut context = c.clone();
            let mut current_vars: HashSet<String> = HashSet::new();
            for (var, exp) in bindings {
                if current_vars.contains(var) {
                    return Err(format!("Duplicate binding for '{}'", var));
                }

                insts.extend(compile_expr(exp, &mut context)?.iter().cloned());
                let stack_offset = context.stack_index * 8;
                insts.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                context = context.push((var.clone(), stack_offset))?;
                current_vars.insert(var.clone());
            }

            insts.extend(compile_expr(expr, &mut context)?.iter().cloned());
        }
        Expr::UnOp(op, e) => {
            match op {
                Op1::Add1 => {
                    insts.extend(compile_expr(e, c)?.iter().cloned());
                    insts.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1 << 1)));
                }
                Op1::Sub1 => {
                    insts.extend(compile_expr(e, c)?.iter().cloned());
                    insts.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(1 << 1)));
                }
                Op1::Negate => {
                    insts.extend(compile_expr(e, c)?.iter().cloned());
                    insts.push(Instr::INeg(Val::Reg(Reg::RAX)));
                }
                Op1::IsNum => {
                    insts.extend(compile_expr(e, c)?.iter().cloned());
                    let type_check: i32 = 0;
                    insts.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(type_check)));
                    insts.push(Instr::And(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                    insts.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(type_check)));
                    insts.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Bool(false)));
                    insts.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Bool(true)));
                    insts.push(Instr::CMov(CMov::CMOVE,Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                }
                Op1::IsBool => {
                    insts.extend(compile_expr(e, c)?.iter().cloned());
                    let type_check: i32 = 1;
                    insts.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(type_check)));
                    insts.push(Instr::And(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                    insts.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(type_check)));
                    insts.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Bool(false)));
                    insts.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Bool(true)));
                    insts.push(Instr::CMov(CMov::CMOVE,Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                }
            }
        }
        Expr::BinOp(op, lhs, rhs) => {
            let lhs_inst = compile_expr(lhs, c)?;
            let rhs_inst = compile_expr(rhs, &mut c.stack(1))?;
            let stack_offset = c.stack_index * 8;
            insts.extend(lhs_inst.iter().cloned());
            insts.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
            insts.extend(rhs_inst.iter().cloned());
            match op {
                Op2::Plus => {
                    type_check(&mut insts, Val::Reg(Reg::RAX), Type::Number);
                    type_check(&mut insts, Val::RegOffset(Reg::RSP, stack_offset), Type::Number);
                    insts.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
                }
                Op2::Minus => {
                    type_check(&mut insts, Val::Reg(Reg::RAX), Type::Number);
                    type_check(&mut insts, Val::RegOffset(Reg::RSP, stack_offset), Type::Number);
                    insts.push(Instr::ISub(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                    insts.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)))
                }
                Op2::Times => {
                    type_check(&mut insts, Val::Reg(Reg::RAX), Type::Number);
                    type_check(&mut insts, Val::RegOffset(Reg::RSP, stack_offset), Type::Number);
                    insts.push(Instr::Sar(Val::Reg(Reg::RAX)));
                    insts.push(Instr::Sar(Val::RegOffset(Reg::RSP,stack_offset )));
                    insts.push(Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
                    insts.push(Instr::Shl(Val::Reg(Reg::RAX)));
                }
                Op2::Equal => {
                    same_type_check(&mut insts, Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset));
                    insts.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
                    insts.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Bool(false)));
                    insts.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Bool(true)));
                    insts.push(Instr::CMov(CMov::CMOVE,Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                },
                Op2::Greater => {
                    type_check(&mut insts, Val::Reg(Reg::RAX), Type::Number);
                    type_check(&mut insts, Val::RegOffset(Reg::RSP, stack_offset), Type::Number);

                    insts.push(Instr::Cmp(Val::RegOffset(Reg::RSP, stack_offset),  Val::Reg(Reg::RAX)));

                    insts.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Bool(false)));
                    insts.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Bool(true)));
                    insts.push(Instr::CMov(CMov::CMOVG,Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                },
                Op2::GreaterEqual => {
                    type_check(&mut insts, Val::Reg(Reg::RAX), Type::Number);
                    type_check(&mut insts, Val::RegOffset(Reg::RSP, stack_offset), Type::Number);

                    insts.push(Instr::Cmp(Val::RegOffset(Reg::RSP, stack_offset),  Val::Reg(Reg::RAX)));

                    insts.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Bool(false)));
                    insts.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Bool(true)));
                    insts.push(Instr::CMov(CMov::CMOVGE,Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                },
                Op2::Less => {
                    type_check(&mut insts, Val::Reg(Reg::RAX), Type::Number);
                    type_check(&mut insts, Val::RegOffset(Reg::RSP, stack_offset), Type::Number);

                    insts.push(Instr::Cmp(Val::RegOffset(Reg::RSP, stack_offset),  Val::Reg(Reg::RAX)));

                    insts.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Bool(false)));
                    insts.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Bool(true)));
                    insts.push(Instr::CMov(CMov::CMOVL,Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                },
                Op2::LessEqual => {
                    type_check(&mut insts, Val::Reg(Reg::RAX), Type::Number);
                    type_check(&mut insts, Val::RegOffset(Reg::RSP, stack_offset), Type::Number);

                    insts.push(Instr::Cmp(Val::RegOffset(Reg::RSP, stack_offset),  Val::Reg(Reg::RAX)));

                    insts.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Bool(false)));
                    insts.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Bool(true)));
                    insts.push(Instr::CMov(CMov::CMOVLE,Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                }
            }
        },
        Expr::If(cond, thene, elsee) => {
            let else_label = c.label("ifelse");
            let end_label = c.label("ifend");

            // If condition
            insts.extend(compile_expr(cond, c)?.iter().cloned());
            insts.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::Bool(false)));
            insts.push(Instr::Jump(Jump::JE, else_label.clone()));

            // Then
            insts.extend(compile_expr(thene, c)?.iter().cloned());
            insts.push(Instr::Jump(Jump::JMP, end_label.clone()));

            // Else
            insts.push(Instr::Label(else_label));
            insts.extend(compile_expr(elsee, c)?.iter().cloned());

            // End
            insts.push(Instr::Label(end_label));
        },

        Expr::Loop(e) => {
            let loop_start = c.label("loop_start");
            let loop_end = c.label("loop_end");

            c.loop_stack.push(loop_end.clone());

            insts.push(Instr::Label(loop_start.clone()));
            insts.extend(compile_expr(e, c)?.iter().cloned());
            insts.push(Instr::Jump(Jump::JMP, loop_start));
            insts.push(Instr::Label(loop_end));
        },
        Expr::Break(e) => {
            match c.loop_stack.pop() {
                None => {
                    return Err(format!("break with no loop"));
                },
                Some(loop_end) => {
                    insts.extend(compile_expr(e, c)?.iter().cloned());
                    insts.push(Instr::Jump(Jump::JMP, loop_end));
                }
            }
        },

        Expr::Set(var, e) => {
            match c.environment.get(var) {
                None => {
                    return Err(format!("Unbound variable identifier {}", var));
                }
                Some(&offset) => {
                    insts.extend(compile_expr(e, c)?.iter().cloned());
                    insts.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX)));
                }
            }
        },

        Expr::Block(es) => {
            for e in es {
                let lhs_inst = compile_expr(e, c)?;
                insts.extend(lhs_inst.iter().cloned());
            }
        },
        Expr::Input => {
            insts.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI)));
        }
    };
    Ok(insts)
}

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;
    let sexpress = parse(&in_contents);
    if let Err(e) = sexpress {
        return Err(format!("Invalid: parse error: {}", e).into());
    }

    let expr = parse_expr(&sexpress.unwrap())?;
    let comp = compile_expr(&expr, &mut Context::new())?;
    let result = comp.iter().map(|inst| inst.out()).collect::<Vec<String>>().join("\n");

    let asm_program = format!(
        "
section .text
extern {ERROR_LABEL}
global our_code_starts_here
snek_error_wrapper:
    push rsp
    call {ERROR_LABEL}
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
