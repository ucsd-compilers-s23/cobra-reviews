use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::Atom::*;
use sexp::*;

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
    Number(i64),
    Boolean(bool),
    Id(String),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),

    Let(Vec<(String, Expr)>, Box<Expr>),
    Set(String, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
    Block(Vec<Expr>),
}

// Reserved words
const CONSTANTS: [&str; 3] = ["input", "true", "false"];

const UNOPS: [&str; 4] = ["add1", "sub1", "isnum", "isbool"];

const BINOPS: [&str; 8] = ["+", "-", "*", "=", ">", ">=", "<", "<="];

const KEYWORDS: [&str; 6] = ["let", "set!", "if", "loop", "break", "block"];

fn keyword_check(name: &str) -> bool {
    CONSTANTS.contains(&name)
        || UNOPS.contains(&name)
        || BINOPS.contains(&name)
        || KEYWORDS.contains(&name)
}

const I63_MAX: i64 = 2_i64.pow(62) - 1;
const I63_MIN: i64 = -2_i64.pow(62);

fn i63_overflow(n: i64) -> bool {
    n > I63_MAX || n < I63_MIN
}

fn parse_overflow_check(n: i64) -> i64 {
    if i63_overflow(n) {
        panic!(
            "Invalid: Integer overflow {} (numbers must be i63)",
            n.to_string()
        )
    } else {
        n
    }
}

fn sexps_to_string(vec: &Vec<Sexp>) -> String {
    vec.into_iter()
        .map(|s| s.to_string())
        .collect::<Vec<String>>()
        .join(" ")
}

fn parse_unop(vec: &Vec<Sexp>) -> Expr {
    match &vec[..] {
        // Arithmetic
        [Sexp::Atom(S(op)), e] if op == "add1" => Expr::UnOp(Op1::Add1, Box::new(parse_expr(e))),
        [Sexp::Atom(S(op)), e] if op == "sub1" => Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e))),

        // Type checking
        [Sexp::Atom(S(op)), e] if op == "isnum" => Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e))),
        [Sexp::Atom(S(op)), e] if op == "isbool" => {
            Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e)))
        }

        _ => panic!(
            "Invalid: Syntax error in unary operator expression: ({})",
            sexps_to_string(vec)
        ),
    }
}

fn parse_binop(vec: &Vec<Sexp>) -> Expr {
    match &vec[..] {
        // Arithmetic
        [Sexp::Atom(S(op)), e1, e2] if op == "+" => Expr::BinOp(
            Op2::Plus,
            Box::new(parse_expr(e1)),
            Box::new(parse_expr(e2)),
        ),
        [Sexp::Atom(S(op)), e1, e2] if op == "-" => Expr::BinOp(
            Op2::Minus,
            Box::new(parse_expr(e1)),
            Box::new(parse_expr(e2)),
        ),
        [Sexp::Atom(S(op)), e1, e2] if op == "*" => Expr::BinOp(
            Op2::Times,
            Box::new(parse_expr(e1)),
            Box::new(parse_expr(e2)),
        ),

        // Comparison
        [Sexp::Atom(S(op)), e1, e2] if op == "=" => Expr::BinOp(
            Op2::Equal,
            Box::new(parse_expr(e1)),
            Box::new(parse_expr(e2)),
        ),
        [Sexp::Atom(S(op)), e1, e2] if op == ">" => Expr::BinOp(
            Op2::Greater,
            Box::new(parse_expr(e1)),
            Box::new(parse_expr(e2)),
        ),
        [Sexp::Atom(S(op)), e1, e2] if op == ">=" => Expr::BinOp(
            Op2::GreaterEqual,
            Box::new(parse_expr(e1)),
            Box::new(parse_expr(e2)),
        ),
        [Sexp::Atom(S(op)), e1, e2] if op == "<" => Expr::BinOp(
            Op2::Less,
            Box::new(parse_expr(e1)),
            Box::new(parse_expr(e2)),
        ),
        [Sexp::Atom(S(op)), e1, e2] if op == "<=" => Expr::BinOp(
            Op2::LessEqual,
            Box::new(parse_expr(e1)),
            Box::new(parse_expr(e2)),
        ),

        _ => panic!(
            "Invalid: Syntax error in binary operator expression: ({})",
            sexps_to_string(vec)
        ),
    }
}

fn parse_binding(s: &Sexp) -> (String, Expr) {
    match s {
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(name)), e] => {
                if keyword_check(&name.as_str()) {
                    panic!(
                        "Invalid: Attempted to bind to a keyword in let binding: {}",
                        s.to_string()
                    )
                }
                (name.to_string(), parse_expr(e))
            }
            _ => panic!("Invalid: syntax error in let binding: {}", s.to_string()),
        },
        _ => panic!(
            "Invalid: syntax error in let binding, binding must be a list: {}",
            s.to_string()
        ),
    }
}

fn parse_keyword(vec: &Vec<Sexp>) -> Expr {
    match &vec[..] {
        [Sexp::Atom(S(keyword)), Sexp::List(bindings), body] if keyword == "let" => {
            if bindings.is_empty() {
                panic!(
                    "Invalid: let must contain at least one binding: ({})",
                    sexps_to_string(vec)
                )
            }
            let bindings = bindings.iter().map(parse_binding).collect();
            Expr::Let(bindings, Box::new(parse_expr(body)))
        }
        [Sexp::Atom(S(keyword)), Sexp::Atom(S(name)), e] if keyword == "set!" => {
            if keyword_check(name) {
                panic!(
                    "Invalid: Attempted to redefine a keyword in set! expression: ({})",
                    sexps_to_string(vec)
                )
            }
            Expr::Set(name.to_string(), Box::new(parse_expr(e)))
        }
        [Sexp::Atom(S(keyword)), cond, thn, els] if keyword == "if" => Expr::If(
            Box::new(parse_expr(cond)),
            Box::new(parse_expr(thn)),
            Box::new(parse_expr(els)),
        ),
        [Sexp::Atom(S(keyword)), e] if keyword == "loop" => Expr::Loop(Box::new(parse_expr(e))),
        [Sexp::Atom(S(keyword)), e] if keyword == "break" => Expr::Break(Box::new(parse_expr(e))),
        [Sexp::Atom(S(keyword)), exprs @ ..] if keyword == "block" => {
            if exprs.is_empty() {
                panic!(
                    "Invalid: block must contain at least one expression: ({})",
                    sexps_to_string(vec)
                )
            }
            Expr::Block(exprs.into_iter().map(parse_expr).collect())
        }

        _ => panic!(
            "Invalid: syntax error in keyword expression: ({})",
            sexps_to_string(vec)
        ),
    }
}

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => Expr::Number(parse_overflow_check(*n)),
        Sexp::Atom(S(x)) if x == "true" => Expr::Boolean(true),
        Sexp::Atom(S(x)) if x == "false" => Expr::Boolean(false),
        Sexp::Atom(S(x)) => Expr::Id(x.to_string()),

        Sexp::List(vec) => match &vec[..] {
            // Op1
            [Sexp::Atom(S(op)), ..] if UNOPS.contains(&op.as_str()) => parse_unop(vec),

            // Op2
            [Sexp::Atom(S(op)), ..] if BINOPS.contains(&op.as_str()) => parse_binop(vec),

            // Keywords
            [Sexp::Atom(S(keyword)), ..] if KEYWORDS.contains(&keyword.as_str()) => {
                parse_keyword(vec)
            }

            _ => panic!("Invalid list expression: {}", s.to_string()),
        },
        Sexp::Atom(F(_)) => panic!("Invalid expression: float not allowed: {}", s.to_string()),
    }
}

#[derive(Debug)]
enum Val {
    Reg(REG),
    Imm(i64),
    RegOffset(REG, i64),
    Err(SnekError),
}

#[derive(Debug, Clone, Copy)]
enum SnekError {
    Overflow = 1,
    InvalidArg = 2,
}

#[derive(Debug)]
enum REG {
    RAX,
    RBX,
    RSP,
    RDI,
    RSI,
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    ISar(Val, Val),
    IXor(Val, Val),
    ITest(Val, Val), // Should only have i32 values
    ICmp(Val, Val),
    ICmov(ICond, Val, Val),
    IJcc(ICond, String),
    IJmp(String),
    ILabel(String),
}

#[derive(Debug)]
// #[allow(unused)]
enum ICond {
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Overflow,
    NotZero,
    Zero,
}

use Instr::*;
use Val::*;
use REG::*;

const SNEK_ERROR_LABEL: &str = "throw_error";

struct Context<'a> {
    si: i64,
    env: &'a im::HashMap<String, i64>,
    loopexit: &'a str,
}

fn new_label(l: &mut i64, s: &str) -> String {
    let curr = *l;
    *l += 1;
    format!("{s}_{curr}")
}

fn num_to_val(n: i64) -> Val {
    if i63_overflow(n) {
        panic!("Invalid: number overflow: {}", n.to_string())
    }
    Imm(n << 1)
}

fn bool_to_val(b: bool) -> Val {
    if b {
        Imm(3) // true: 0b0...011
    } else {
        Imm(1) // false: 0b0...001
    }
}

fn type_check_int() -> Vec<Instr> {
    vec![
        ITest(Reg(RAX), Imm(1)),
        IMov(Reg(RSI), Err(SnekError::InvalidArg)),
        IJcc(ICond::NotZero, SNEK_ERROR_LABEL.to_string()),
    ]
}

#[allow(unused)]
fn type_check_bool() -> Vec<Instr> {
    vec![
        ITest(Reg(RAX), Imm(1)),
        IMov(Reg(RSI), Err(SnekError::InvalidArg)),
        IJcc(ICond::Zero, SNEK_ERROR_LABEL.to_string()),
    ]
}

fn check_same_type(v1: Val, v2: Val) -> Vec<Instr> {
    vec![
        IMov(Reg(RBX), v1),
        IXor(Reg(RBX), v2),
        ITest(Reg(RBX), Imm(1)),
        IMov(Reg(RSI), Err(SnekError::InvalidArg)),
        IJcc(ICond::NotEqual, SNEK_ERROR_LABEL.to_string()),
    ]
}

fn overflow_check_instrs() -> Vec<Instr> {
    vec![
        IMov(Reg(RSI), Err(SnekError::Overflow)),
        IJcc(ICond::Overflow, SNEK_ERROR_LABEL.to_string()),
    ]
}

fn compile_unop(op: &Op1, e: &Expr, c: &Context, l: &mut i64) -> Vec<Instr> {
    let mut instrs = compile_expr(e, c, l);

    match op {
        Op1::Add1 | Op1::Sub1 => {
            // type check: e is an int
            instrs.extend(type_check_int());

            let operation = if matches!(op, Op1::Add1) {
                vec![IAdd(Reg(RAX), num_to_val(1))]
            } else {
                vec![ISub(Reg(RAX), num_to_val(1))]
            };
            instrs.extend(operation);

            // overflow check
            instrs.extend(overflow_check_instrs());
        }
        Op1::IsNum => {
            let operation = vec![
                ITest(Reg(RAX), Imm(1)),
                IMov(Reg(RBX), bool_to_val(true)),
                IMov(Reg(RAX), bool_to_val(false)),
                ICmov(ICond::Zero, Reg(RAX), Reg(RBX)),
            ];
            instrs.extend(operation);
        }
        Op1::IsBool => {
            let operation = vec![
                ITest(Reg(RAX), Imm(1)),
                IMov(Reg(RBX), bool_to_val(true)),
                IMov(Reg(RAX), bool_to_val(false)),
                ICmov(ICond::NotZero, Reg(RAX), Reg(RBX)),
            ];
            instrs.extend(operation);
        }
    };

    instrs
}

fn compile_binop(op: &Op2, e1: &Expr, e2: &Expr, c: &Context, l: &mut i64) -> Vec<Instr> {
    let e1_instrs = compile_expr(e1, c, l);
    let e2_instrs = compile_expr(e2, &Context { si: c.si + 1, ..*c }, l);
    let stack_offset = c.si * 8;
    let mut instrs = vec![];

    match op {
        Op2::Plus | Op2::Minus | Op2::Times => {
            instrs.extend(e1_instrs);
            instrs.extend(type_check_int());
            instrs.push(IMov(RegOffset(RSP, stack_offset), Reg(RAX)));
            instrs.extend(e2_instrs);
            instrs.extend(type_check_int());

            let operation = if matches!(op, Op2::Plus) {
                vec![IAdd(Reg(RAX), RegOffset(RSP, stack_offset))]
            } else if matches!(op, Op2::Minus) {
                vec![
                    IMov(Reg(RBX), Reg(RAX)),
                    IMov(Reg(RAX), RegOffset(RSP, stack_offset)),
                    ISub(Reg(RAX), Reg(RBX)),
                ]
            } else {
                // 2x * 2y = 4xy, need to divide one operand by 2
                vec![
                    ISar(Reg(RAX), Imm(1)),
                    IMul(Reg(RAX), RegOffset(RSP, stack_offset)),
                ]
            };

            instrs.extend(operation);

            // overflow
            instrs.extend(overflow_check_instrs());
        }
        Op2::Equal => {
            instrs.extend(e1_instrs);
            instrs.push(IMov(RegOffset(RSP, stack_offset), Reg(RAX)));
            instrs.extend(e2_instrs);

            // type check
            instrs.extend(check_same_type(Reg(RAX), RegOffset(RSP, stack_offset)));

            let operation = vec![
                ICmp(Reg(RAX), RegOffset(RSP, stack_offset)),
                IMov(Reg(RBX), bool_to_val(true)),
                IMov(Reg(RAX), bool_to_val(false)),
                ICmov(ICond::Equal, Reg(RAX), Reg(RBX)),
            ];

            instrs.extend(operation)
        }
        Op2::Greater | Op2::GreaterEqual | Op2::Less | Op2::LessEqual => {
            instrs.extend(e1_instrs);
            instrs.extend(type_check_int());
            instrs.push(IMov(RegOffset(RSP, stack_offset), Reg(RAX)));
            instrs.extend(e2_instrs);
            instrs.extend(type_check_int());

            let operation = vec![
                // swap operands and compare
                IMov(Reg(RBX), Reg(RAX)),
                IMov(Reg(RAX), RegOffset(RSP, stack_offset)),
                ICmp(Reg(RAX), Reg(RBX)),
                IMov(Reg(RBX), bool_to_val(true)),
                IMov(Reg(RAX), bool_to_val(false)),
                ICmov(
                    match op {
                        Op2::Greater => ICond::Greater,
                        Op2::GreaterEqual => ICond::GreaterEqual,
                        Op2::Less => ICond::Less,
                        Op2::LessEqual => ICond::LessEqual,
                        _ => panic!("Rust should really be able to figure this out..."),
                    },
                    Reg(RAX),
                    Reg(RBX),
                ),
            ];

            instrs.extend(operation);
        }
    }

    instrs
}

fn compile_expr(e: &Expr, c: &Context, l: &mut i64) -> Vec<Instr> {
    match e {
        Expr::Number(n) => vec![IMov(Reg(RAX), num_to_val(*n))],
        Expr::Boolean(b) => vec![IMov(Reg(RAX), bool_to_val(*b))],
        Expr::Id(name) if name == "input" => {
            vec![IMov(Reg(RAX), Reg(RDI))]
        }
        Expr::Id(name) => match c.env.get(name) {
            Some(offset) => vec![IMov(Reg(RAX), RegOffset(RSP, *offset))],
            None => panic!("Invalid: Unbound variable identifier {}", name),
        },

        // Unary Operators
        Expr::UnOp(op, e1) => compile_unop(op, e1, c, l),

        // Binary Operators
        Expr::BinOp(op, e1, e2) => compile_binop(op, e1, e2, c, l),

        // Variable Bindings
        Expr::Let(bindings, body) => {
            let mut local_bindings = std::collections::HashSet::new();
            let mut nenv = c.env.clone();

            let mut binding_si = c.si;

            let mut instrs = vec![];

            for (name, binding) in bindings {
                // Check for duplicate binding in the same scope
                if local_bindings.contains(name) {
                    panic!("Invalid: Duplicate binding {}", name)
                }
                local_bindings.insert(name.to_string());

                // evaluate the binding
                let binding_instrs = compile_expr(
                    binding,
                    &Context {
                        si: binding_si,
                        env: &nenv,
                        ..*c
                    },
                    l,
                );
                let stack_offset = binding_si * 8;

                // add binding instructions and store to stack
                instrs.extend(binding_instrs);
                instrs.push(IMov(RegOffset(RSP, stack_offset), Reg(RAX)));

                // map variable name to stack offset
                nenv = nenv.update(name.to_string(), stack_offset);
                binding_si += 1;
            }

            let body_instrs = compile_expr(
                body,
                &Context {
                    si: binding_si,
                    env: &nenv,
                    ..*c
                },
                l,
            );
            instrs.extend(body_instrs);
            instrs
        }
        Expr::Set(name, val) => {
            let offset = match c.env.get(name) {
                Some(offset) => *offset,
                None => panic!("Invalid: Unbound variable identifier {}", name),
            };

            let mut instrs = compile_expr(val, c, l);
            instrs.push(IMov(RegOffset(RSP, offset), Reg(RAX)));
            instrs
        }
        Expr::If(cond, thn, els) => {
            let else_label = new_label(l, "ifelse");
            let end_label = new_label(l, "ifend");

            let mut instrs = compile_expr(cond, c, l);
            // branch
            instrs.extend(vec![
                ICmp(Reg(RAX), bool_to_val(false)),
                IJcc(ICond::Equal, else_label.to_string()),
            ]);
            instrs.extend(compile_expr(thn, c, l));
            // jmp to end and start else block
            instrs.extend(vec![
                IJmp(end_label.to_string()),
                ILabel(else_label.to_string()),
            ]);
            instrs.extend(compile_expr(els, c, l));
            instrs.push(ILabel(end_label.to_string()));
            instrs
        }
        Expr::Loop(body) => {
            let loopstart = new_label(l, "loop");
            let loopexit = new_label(l, "loopexit");
            let mut instrs = vec![ILabel(loopstart.to_string())];

            instrs.extend(compile_expr(
                body,
                &Context {
                    loopexit: &loopexit.to_string(),
                    ..*c
                },
                l,
            ));

            // jmp and exit label
            instrs.extend(vec![
                IJmp(loopstart.to_string()),
                ILabel(loopexit.to_string()),
            ]);
            instrs
        }
        Expr::Break(res) => {
            if c.loopexit == "" {
                panic!("Invalid: break used outside of loop")
            }

            let mut instrs = compile_expr(res, c, l);
            instrs.push(IJmp(c.loopexit.to_string()));
            instrs
        }
        Expr::Block(es) => {
            let mut instrs = vec![];
            for e in es {
                instrs.extend(compile_expr(e, c, l));
            }
            instrs
        }
    }
}

fn cond_to_str(cond: &ICond) -> &str {
    match cond {
        ICond::Equal => "e",
        ICond::NotEqual => "ne",
        ICond::Greater => "g",
        ICond::GreaterEqual => "ge",
        ICond::Less => "l",
        ICond::LessEqual => "le",
        ICond::Overflow => "o",
        ICond::NotZero => "nz",
        ICond::Zero => "z",
    }
}

fn instr_to_str(i: &Instr) -> String {
    match i {
        IMov(v1, v2) => format!("mov {}, {}", val_to_str(v1), val_to_str(v2)),
        IAdd(v1, v2) => format!("add {}, {}", val_to_str(v1), val_to_str(v2)),
        ISub(v1, v2) => format!("sub {}, {}", val_to_str(v1), val_to_str(v2)),
        IMul(v1, v2) => format!("imul {}, {}", val_to_str(v1), val_to_str(v2)),
        ISar(v1, v2) => format!("sar {}, {}", val_to_str(v1), val_to_str(v2)),
        IXor(v1, v2) => format!("xor {}, {}", val_to_str(v1), val_to_str(v2)),
        ITest(v1, v2) => format!("test {}, {}", val_to_str(v1), val_to_str(v2)),
        ICmp(v1, v2) => format!("cmp {}, {}", val_to_str(v1), val_to_str(v2)),
        ICmov(cond, v1, v2) => format!(
            "cmov{} {}, {}",
            cond_to_str(cond),
            val_to_str(v1),
            val_to_str(v2)
        ),
        IJcc(cond, label) => format! {"j{} {label}", cond_to_str(cond)},
        IJmp(label) => format!("jmp {label}"),
        ILabel(name) => format!("{name}:"),
    }
}

fn reg_to_str(r: &REG) -> &str {
    match r {
        RAX => "rax",
        RBX => "rbx",
        RSP => "rsp",
        RDI => "rdi",
        RSI => "rsi",
    }
}

fn val_to_str(v: &Val) -> String {
    match v {
        Reg(reg) => reg_to_str(reg).to_string(),
        Imm(n) => n.to_string(),
        RegOffset(reg, n) => {
            if *n > 0 {
                format!("[{} - {}]", reg_to_str(reg), *n)
            } else if *n < 0 {
                format!("[{} + {}]", reg_to_str(reg), *n)
            } else {
                format!("[{}]", reg_to_str(reg))
            }
        }
        Err(e) => (*e as i64).to_string(),
    }
}

fn compile(e: &Expr) -> String {
    let mut labels = 0;
    let context = Context {
        si: 2,
        env: &im::HashMap::new(),
        loopexit: "",
    };
    let instrs = compile_expr(e, &context, &mut labels);

    return instrs.iter().map(|i| instr_to_str(i) + "\n").collect();
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;
    let sexp_ast = parse(&in_contents).expect("Invalid: not a valid s-expression.");

    let expr = parse_expr(&sexp_ast);

    let result = compile(&expr);

    let asm_program = format!(
        "
section .text

extern snek_error
global our_code_starts_here
{SNEK_ERROR_LABEL}:
    mov rdi, rsi
    push rsp
    call snek_error
our_code_starts_here:
  {result}
  ret");

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}
