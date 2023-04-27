use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::collections::HashMap;
use std::collections::HashSet;

use sexp::Atom::*;
use sexp::Sexp::*;
use sexp::*;

const STACK_BASE: i64 = 2;

#[derive(Debug, Clone)]
enum Val {
    Reg(Register),
    Imm(i64),
    RegOffset(Register, i64),
    ValTrue,
    ValFalse,
    Label(String),
}

use Val::*;

#[derive(Debug, Clone, Copy)]
enum Register {
    RAX,
    RBX,
    RCX,
    RDX,
    RSP,
    RDI,
}

use Register::*;

#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    And(Val, Val),
    Xor(Val, Val),
    Not(Val),
    Shl(Val, Val),
    Sal(Val, Val),
    Sar(Val, Val),
    Cmp(Val, Val),
    Test(Val, Val),
    Cmovg(Val, Val),
    Cmovge(Val, Val),
    Cmovl(Val, Val),
    Cmovle(Val, Val),
    Cmovz(Val, Val),
    Inc(Val),
    // Dec(Val),
    ILabel(Val),
    Jmp(Val),
    Je(Val),
    // Jg(Val),
    // Jge(Val),
    // Jl(Val),
    // Jle(Val),
    Jo(Val),
    // Jz(Val),
    Jnz(Val),
    // Ja(Val),
}

use Instr::*;

#[derive(Debug)]
enum ZeroOp {
    Number(i64),
    OpTrue,
    OpFalse,
    Input,
    Identifier(String),
}

use ZeroOp::*;

#[derive(Debug)]
enum UnitaryOp {
    Add1,
    Sub1,
    IsNum,
    IsBool,
    Loop,
    Break,
}

use UnitaryOp::*;

#[derive(Debug)]
enum BinaryOp {
    Plus,
    Minus,
    Times,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    Equal,
}

use BinaryOp::*;

#[derive(Debug)]
struct Binding (String, Box<Expr>);

#[derive(Debug, Clone)]
struct Namespace {
    h: HashMap<String, i64>,
    break_label: String,
}

#[derive(Debug)]
enum Expr {
    Let(Vec<Binding>, Box<Expr>),
    Set(Binding),
    EZeroOp(ZeroOp),
    EUnitaryOp(UnitaryOp, Box<Expr>),
    EBinaryOp(BinaryOp, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Block(Vec<Expr>),
}

use Expr::*;

fn generate_id(uid: &mut i64) -> i64 {
    *uid += 1;
    *uid
}

fn parse_bind(vec: &Vec<Sexp>) -> Vec<Binding> {
    let mut array = Vec::new();
    for binding in &vec[..] {
        if let List(inner_vec) = binding { if let [Atom(S(s)), e] = &inner_vec[..] {
            array.push(Binding(parse_identifier(s), Box::new(parse_expr(e))));
            continue
        } }
        panic!("Invalid - parse error")
    }
    array
}

fn parse_identifier(s: &str) -> String {
    let keywords = HashSet::from(["true", "false", "input", "let", "set!", "if", "block", "loop", "break", "add1", "sub1", "isnum", "isbool"]);
    (!keywords.contains(s)).then(||0).expect("Invalid - parse error - keyword used as identifier!");
    String::from(s)
}

fn parse_zero_op(sexp: &Sexp) -> Expr {
    match sexp {
        Atom(I(n)) => if *n <= 4611686018427387903 && *n >= -4611686018427387904 {EZeroOp(Number(2*i64::try_from(*n).expect("Invalid - parse error - number not valid or overflow!")))}
                            else {panic!("Invalid - parse error - number not valid or overflow!")},
        Atom(S(op)) if op == "true" => EZeroOp(OpTrue),
        Atom(S(op)) if op == "false" => EZeroOp(OpFalse),
        Atom(S(op)) if op == "input" => EZeroOp(Input),
        Atom(S(s)) => EZeroOp(Identifier(s.clone())),
        _ => panic!("Invalid - parse error - tried to treat list Sexp as atom!"),
    }
}

fn parse_block(vec: &Vec<Sexp>) -> Expr {
    let mut array = Vec::new();
    if let Atom(S(s)) = vec.first().expect("Invalid - parse error!") { if s == "block" {
        _ = &vec[1..].first().expect("Invalid - block must have subexpressions!");
        for expr in &vec[1..] {
            array.push(parse_expr(expr));
        }
        return Block(array)
    } }
    panic!("Invalid - parse error! {:?}", vec);
}

fn parse_expr(sexp: &Sexp) -> Expr {
    match sexp {
        Atom(_) => parse_zero_op(sexp),
        List(vec) => {
            match &vec[..] {
                [Atom(S(op)), e] if op == "add1" => EUnitaryOp(Add1, Box::new(parse_expr(e))),
                [Atom(S(op)), e] if op == "sub1" => EUnitaryOp(Sub1, Box::new(parse_expr(e))),
                [Atom(S(op)), e] if op == "isnum" => EUnitaryOp(IsNum, Box::new(parse_expr(e))),
                [Atom(S(op)), e] if op == "isbool" => EUnitaryOp(IsBool, Box::new(parse_expr(e))),
                [Atom(S(op)), e] if op == "loop" => EUnitaryOp(Loop, Box::new(parse_expr(e))),
                [Atom(S(op)), e] if op == "break" => EUnitaryOp(Break, Box::new(parse_expr(e))),
                [Atom(S(op)), e1, e2] if op == "+" => EBinaryOp(Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Atom(S(op)), e1, e2] if op == "-" => EBinaryOp(Minus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Atom(S(op)), e1, e2] if op == "*" => EBinaryOp(Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Atom(S(op)), e1, e2] if op == ">" => EBinaryOp(Greater, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Atom(S(op)), e1, e2] if op == ">=" => EBinaryOp(GreaterEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Atom(S(op)), e1, e2] if op == "<" => EBinaryOp(Less, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Atom(S(op)), e1, e2] if op == "<=" => EBinaryOp(LessEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Atom(S(op)), e1, e2] if op == "=" => EBinaryOp(Equal, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Atom(S(op)), List(vec_let), e] if op == "let" => Let(parse_bind(vec_let), Box::new(parse_expr(e))),
                [Atom(S(op)), Atom(S(var)), e] if op == "set!" => Set(Binding(parse_identifier(var.as_str()), Box::new(parse_expr(e)))),
                [Atom(S(op)), e1, e2, e3] if op == "if" => If(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)), Box::new(parse_expr(e3))),
                _ => parse_block(vec),
            }
        },
    }
}

fn push(v: &mut Vec<Instr>, val: &Register, stack: &mut i64) {
    v.push(IMov(RegOffset(RSP, *stack), Reg(*val)));
    *stack += 1;
}

fn pop(v: &mut Vec<Instr>, val: &Register, stack: &mut i64) {
    *stack -= 1;
    v.push(IMov(Reg(*val), RegOffset(RSP, *stack)));
}

// fn raise_error(v: &mut Vec<Instr>, code: i64) {
//     v.push(IMov(Reg(RDI), Imm(code)));
//     v.push(Jmp(Label(String::from("snek_error"))));
// }

fn fail_overflow(v: &mut Vec<Instr>) {
    v.push(Jo(Label(String::from("overflow"))));
}

fn expect_number(v: &mut Vec<Instr>, reg: &Register) {
    v.push(Test(Reg(*reg), Imm(1)));
    v.push(Jnz(Label(format!("expect_numeric"))));
}

// fn expect_boolean(v: &mut Vec<Instr>, reg: &Register) {
//     v.push(Test(Reg(*reg), Imm(1)));
//     v.push(Jz(Label(format!("expect_bool"))));
//     v.push(Cmp(Reg(*reg), Imm(3)));
//     v.push(Ja(Label(format!("expect_bool"))));
// }

fn compile_zero_op(op: &ZeroOp, v: &mut Vec<Instr>, _stack: &mut i64, namespace: &mut Namespace) {
    match op {
        Number(n) => v.push(IMov(Reg(RAX), Imm(*n))),
        OpTrue => v.push(IMov(Reg(RAX), ValTrue)),
        OpFalse => v.push(IMov(Reg(RAX), ValFalse)),
        Input => v.push(IMov(Reg(RAX), Reg(RDI))),
        Identifier(s) => v.push(IMov(Reg(RAX), RegOffset(RSP, *namespace.h.get(&s[..]).expect(format!("Unbound variable identifier {}", s).as_str())))),
    }
}

fn compile_if(expr1: &Expr, expr2: &Expr, expr3: &Expr, v: &mut Vec<Instr>, stack: &mut i64, heap: &mut i64, uid: &mut i64, namespace: &mut Namespace) {
    compile(expr1, v, stack, heap, uid, namespace);
    let id = generate_id(uid);
    let label_false = format!("if_false_{}", id);
    let label_end = format!("end_if_{}", id);
    v.push(Cmp(Reg(RAX), Imm(1)));
    v.push(Je(Label(label_false.clone())));
    compile(expr2, v, stack, heap, uid, namespace);
    v.push(Jmp(Label(label_end.clone())));
    v.push(ILabel(Label(format!("{}:", label_false.clone()))));
    compile(expr3, v, stack, heap, uid, namespace);
    v.push(Jmp(Label(label_end.clone())));
    v.push(ILabel(Label(format!("{}:", label_end.clone()))));
}

fn compile_loop(e: &Expr, v: &mut Vec<Instr>, stack: &mut i64, heap: &mut i64, uid: &mut i64, namespace: &mut Namespace) {
    let label = format!("loop_{}", generate_id(uid));
    let mut inner_namespace = namespace.clone();
    inner_namespace.break_label = label.clone();
    v.push(ILabel(Label(format!("{}:", label.as_str()))));
    compile(e, v, stack, heap, uid, &mut inner_namespace);
    v.push(Jmp(Label(label.clone())));
    v.push(ILabel(Label(format!("end_{}:", label.as_str()))));
}

fn compile_unitary_op(op: &UnitaryOp, e: &Expr, v: &mut Vec<Instr>, stack: &mut i64, heap: &mut i64, uid: &mut i64, namespace: &mut Namespace) {
    match op {
        Loop => {
            compile_loop(e, v, stack, heap, uid, namespace);
            return
        },
        _ => (),
    }
    compile(e, v, stack, heap, uid, namespace);
    match op {
        Add1 => {
            expect_number(v, &RAX);
            v.push(IAdd(Reg(RAX), Imm(2)));
            fail_overflow(v);
        }
        Sub1 => {
            expect_number(v, &RAX);
            v.push(ISub(Reg(RAX), Imm(2)));
            fail_overflow(v);
        }
        IsNum => {
            v.push(Not(Reg(RAX)));
            v.push(And(Reg(RAX), Imm(1)));
            v.push(Shl(Reg(RAX), Imm(1)));
            v.push(Inc(Reg(RAX)));
        },
        IsBool => {
            v.push(And(Reg(RAX), Imm(1)));
            v.push(Shl(Reg(RAX), Imm(1)));
            v.push(Inc(Reg(RAX)));
        },
        Break => {
            (namespace.break_label != "").then(||0).expect("Invalid - parse error - break outside a loop body!");
            v.push(Jmp(Label(format!("end_{}", namespace.break_label.as_str()))));
        },
        _ => (),
    }
}

fn compile_binary_op_to_arithmetic(op: &BinaryOp, e1: &Expr, e2: &Expr, v: &mut Vec<Instr>, stack: &mut i64, heap: &mut i64, uid: &mut i64, namespace: &mut Namespace) {
    compile(e2, v, stack, heap, uid, namespace);
    push(v, &RAX, stack);
    compile(e1, v, stack, heap, uid, namespace);
    pop(v, &RBX, stack);
    expect_number(v, &RAX);
    expect_number(v, &RBX);
    match op {
        Plus => {
            v.push(IAdd(Reg(RAX), Reg(RBX)));
            fail_overflow(v);
        }
        Minus => {
            v.push(ISub(Reg(RAX), Reg(RBX)));
            fail_overflow(v);
        }
        Times => {
            v.push(Sar(Reg(RAX), Imm(1)));
            v.push(Sar(Reg(RBX), Imm(1)));
            v.push(IMul(Reg(RAX), Reg(RBX)));
            fail_overflow(v);
            v.push(Sal(Reg(RAX), Imm(1)));
            fail_overflow(v);
        }
        _ => panic!("Compile to arithmetic binary op called for non binary to arithmetic!"),
    }
}

fn compile_binary_op_arithmetic_to_boolean(op: &BinaryOp, e1: &Expr, e2: &Expr, v: &mut Vec<Instr>, stack: &mut i64, heap: &mut i64, uid: &mut i64, namespace: &mut Namespace) {
    compile(e2, v, stack, heap, uid, namespace);
    push(v, &RAX, stack);
    compile(e1, v, stack, heap, uid, namespace);
    pop(v, &RCX, stack);
    v.push(IMov(Reg(RBX), Reg(RAX)));
    expect_number(v, &RBX);
    expect_number(v, &RCX);
    v.push(IMov(Reg(RAX), Imm(1)));
    v.push(IMov(Reg(RDX), Imm(3)));
    v.push(Cmp(Reg(RBX), Reg(RCX)));
    match op {
        Greater => v.push(Cmovg(Reg(RAX), Reg(RDX))),
        GreaterEqual => v.push(Cmovge(Reg(RAX), Reg(RDX))),
        Less => v.push(Cmovl(Reg(RAX), Reg(RDX))),
        LessEqual => v.push(Cmovle(Reg(RAX), Reg(RDX))),
        _ => panic!("Bad call to compile arithmetic to bool binary!"),
    }
}

fn compile_equal(e1: &Expr, e2: &Expr, v: &mut Vec<Instr>, stack: &mut i64, heap: &mut i64, uid: &mut i64, namespace: &mut Namespace) {
    compile(e2, v, stack, heap, uid, namespace);
    push(v, &RAX, stack);
    compile(e1, v, stack, heap, uid, namespace);
    pop(v, &RBX, stack);
    v.push(Xor(Reg(RBX), Reg(RAX)));
    v.push(IMov(Reg(RCX), Imm(3)));
    v.push(IMov(Reg(RAX), Imm(1)));
    v.push(Cmovz(Reg(RAX), Reg(RCX)));
    expect_number(v, &RBX);
}

fn compile_binary_op(op: &BinaryOp, e1: &Expr, e2: &Expr, v: &mut Vec<Instr>, stack: &mut i64, heap: &mut i64, uid: &mut i64, namespace: &mut Namespace) {
    match op {
        Equal => compile_equal(e1, e2, v, stack, heap, uid, namespace),
        Greater => compile_binary_op_arithmetic_to_boolean(op, e1, e2, v, stack, heap, uid, namespace),
        GreaterEqual => compile_binary_op_arithmetic_to_boolean(op, e1, e2, v, stack, heap, uid, namespace),
        Less => compile_binary_op_arithmetic_to_boolean(op, e1, e2, v, stack, heap, uid, namespace),
        LessEqual => compile_binary_op_arithmetic_to_boolean(op, e1, e2, v, stack, heap, uid, namespace),
        Plus => compile_binary_op_to_arithmetic(op, e1, e2, v, stack, heap, uid, namespace),
        Minus => compile_binary_op_to_arithmetic(op, e1, e2, v, stack, heap, uid, namespace),
        Times => compile_binary_op_to_arithmetic(op, e1, e2, v, stack, heap, uid, namespace),
    }
}

fn compile_binding(binding: &Binding, v: &mut Vec<Instr>, stack: &mut i64, heap: &mut i64, uid: &mut i64, namespace: &mut Namespace, must_exist: bool) {
    let Binding(s, e) = binding;
    compile(e, v, stack, heap, uid, namespace);
    let mut location = *heap;
    if must_exist {
        location = *namespace.h.get(s).expect(format!("Unbound variable identifier {}", s).as_str());
    } else {
        namespace.h.insert(s.clone(), *heap);
        *heap += 1;
    }
    v.push(IMov(RegOffset(RSP, location), Reg(RAX)));
}

fn compile_let(e: &Expr, vec: &Vec<Binding>, v: &mut Vec<Instr>, stack: &mut i64, heap: &mut i64, uid: &mut i64, namespace: &mut Namespace) {
    vec.first().expect("Invalid - Need bindings in let!");
    let mut set = HashSet::new();
    let mut inner_namespace = namespace.clone();
    for binding in vec {
            let Binding(s, _) = binding;
            set.insert(s).then(||0).expect("Duplicate binding");
            compile_binding(binding, v, stack, heap, uid, &mut inner_namespace, false);
    }
    compile(e, v, stack, heap, uid, &mut inner_namespace);
}

fn compile(e: &Expr, v: &mut Vec<Instr>, stack: &mut i64, heap: &mut i64, uid: &mut i64, namespace: &mut Namespace) {
    match e {
        EZeroOp(op) => compile_zero_op(op, v, stack, namespace),
        EUnitaryOp(op, expr) => compile_unitary_op(op, expr, v, stack, heap, uid, namespace),
        EBinaryOp(op, expr1, expr2) => compile_binary_op(op, expr1, expr2, v, stack, heap, uid, namespace),
        Let(vec, e) => compile_let(e, vec, v, stack, heap, uid, namespace),
        Set(binding) => compile_binding(binding, v, stack, heap, uid, namespace, true),
        Block(vec) => for expr in vec {compile(expr, v, stack, heap, uid, namespace);},
        If(expr1, expr2, expr3) => compile_if(expr1, expr2, expr3, v, stack, heap, uid, namespace),
    }
}

fn read_val(v: Val) -> String {
    match v {
        Reg(RAX) => String::from("rax"),
        Reg(RBX) => String::from("rbx"),
        Reg(RCX) => String::from("rcx"),
        Reg(RDX) => String::from("rdx"),
        Reg(RSP) => String::from("rsp"),
        Reg(RDI) => String::from("rdi"),
        ValTrue => String::from("3"),
        ValFalse => String::from("1"),
        Imm(n) => n.to_string(),
        RegOffset(RSP, n) => String::from(format!("[rsp - {}]", 8*n)),
        Label(s) => s,
        _ => panic!("Bad code generated!"),
    }
}

fn stringify(vec: Vec<Instr>) -> String {
    let mut s = String::new();
    for i in vec {
        s = format!("{}{}", s,
        match i {
            IMov(v1, v2) => format!("\nmov {}, {}", read_val(v1), read_val(v2)),
            IAdd(v1, v2) => format!("\nadd {}, {}", read_val(v1), read_val(v2)),
            ISub(v1, v2) => format!("\nsub {}, {}", read_val(v1), read_val(v2)),
            IMul(v1, v2) => format!("\nimul {}, {}", read_val(v1), read_val(v2)),
            And(v1, v2) => format!("\nand {}, {}", read_val(v1), read_val(v2)),
            Xor(v1, v2) => format!("\nxor {}, {}", read_val(v1), read_val(v2)),
            Shl(v1, v2) => format!("\nshl {}, {}", read_val(v1), read_val(v2)),
            Sal(v1, v2) => format!("\nsal {}, {}", read_val(v1), read_val(v2)),
            Sar(v1, v2) => format!("\nsar {}, {}", read_val(v1), read_val(v2)),
            Cmp(v1, v2) => format!("\ncmp {}, {}", read_val(v1), read_val(v2)),
            Test(v1, v2) => format!("\ntest {}, {}", read_val(v1), read_val(v2)),
            Cmovg(v1, v2) => format!("\ncmovg {}, {}", read_val(v1), read_val(v2)),
            Cmovge(v1, v2) => format!("\ncmovge {}, {}", read_val(v1), read_val(v2)),
            Cmovl(v1, v2) => format!("\ncmovl {}, {}", read_val(v1), read_val(v2)),
            Cmovle(v1, v2) => format!("\ncmovle {}, {}", read_val(v1), read_val(v2)),
            Cmovz(v1, v2) => format!("\ncmovz {}, {}", read_val(v1), read_val(v2)),
            Not(v) => format!("\nnot {}", read_val(v)),
            Inc(v) => format!("\ninc {}", read_val(v)),
            // Dec(v) => format!("\ndec {}", read_val(v)),
            ILabel(v) => format!("\n{}", read_val(v)),
            Jmp(v) => format!("\njmp {}", read_val(v)),
            Je(v) => format!("\nje {}", read_val(v)),
            // Jg(v) => format!("\njg {}", read_val(v)),
            // Jge(v) => format!("\njge {}", read_val(v)),
            // Jl(v) => format!("\njl {}", read_val(v)),
            // Jle(v) => format!("\njle {}", read_val(v)),
            Jo(v) => format!("\njo {}", read_val(v)),
            // Jz(v) => format!("\njz {}", read_val(v)),
            Jnz(v) => format!("\njnz {}", read_val(v)),
            // Ja(v) => format!("\nja {}", read_val(v)),
        })
    }
    s
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let sexp = parse(&in_contents).expect("Invalid - failed to parse sexp");
    let expr = parse_expr(&sexp);
    let mut instrs = Vec::new();
    let mut outer_namespace = Namespace{h: HashMap::new(), break_label: String::from("")};
    let mut stack = STACK_BASE;
    let mut heap = 2000;
    let mut uid = 0;
    compile(&expr, &mut instrs, &mut stack, &mut heap, &mut uid, &mut outer_namespace);

    // You will make result hold the result of actually compiling
    let result = stringify(instrs);

    let asm_program = format!(
        "
section .text
extern snek_error
global our_code_starts_here

expect_bool:
mov rdi, 5
jmp snek_error

expect_numeric:
mov rdi, 6
jmp snek_error

overflow:
mov rdi, 7
jmp snek_error

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
