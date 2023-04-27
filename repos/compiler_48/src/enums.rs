use std::fmt::Display;

// Abstract syntax
#[derive(Debug)]
pub enum Op1 {
    Arith(Op1Arith),
    Check(Op1Check),
}

#[derive(Debug)]
pub enum Op1Arith {
    Add1,
    Sub1,
}

#[derive(Debug)]
pub enum Op1Check {
    IsNum,
    IsBool,
}

#[derive(Debug)]
pub enum Op2 {
    Arith(Op2Arith),
    Check(Op2Check),
}

#[derive(Debug)]
pub enum Op2Arith {
    Plus,
    Minus,
    Times,
}

#[derive(Debug)]
pub enum Op2Check {
    Equal,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

#[derive(Debug)]
pub enum Expr {
    Number(i64),
    Boolean(bool),
    Input,
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

// Instructions
#[derive(Debug)]
pub enum Val {
    Reg(Reg),
    Imm(i64),
    RegOffset(Reg, i32),
}

#[derive(Debug)]
pub enum Reg {
    RAX,
    RBX,
    RSP,
    RDI,
}

#[derive(Debug)]
pub enum Instr {
    IMov(Val, Val),

    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),

    IOr(Val, Val),
    IAnd(Val, Val),
    IXor(Val, Val),

    ISar(Reg, i32),

    ICmp(Val, Val),
    ITest(Val, Val),

    ICMovE(Reg, Reg),
    ICMovG(Reg, Reg),
    ICMovGE(Reg, Reg),
    ICMovL(Reg, Reg),
    ICMovLE(Reg, Reg),

    IJe(String),
    IJne(String),
    IJmp(String),
    IJo(String),
    
    Label(String),
}

impl Display for Instr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match write!(f, "\n  ") {
            Err(e) => return Err(e),
            _ => (),
        }

        match self {
            Instr::IMov(v1, v2) => write!(f, "mov {}, {}", v1.to_string(), v2.to_string()),

            Instr::IAdd(v1, v2) => write!(f, "add {}, {}", v1.to_string(), v2.to_string()),
            Instr::ISub(v1, v2) => write!(f, "sub {}, {}", v1.to_string(), v2.to_string()),
            Instr::IMul(v1, v2) => write!(f, "imul {}, {}", v1.to_string(), v2.to_string()),

            Instr::IOr(v1, v2) => write!(f, "or {}, {}", v1.to_string(), v2.to_string()),
            Instr::IAnd(v1, v2) => write!(f, "and {}, {}", v1.to_string(), v2.to_string()),
            Instr::IXor(v1, v2) => write!(f, "xor {}, {}", v1.to_string(), v2.to_string()),

            Instr::ISar(r, cnt) => write!(f, "sar {}, {}", r.to_string(), cnt),
            
            Instr::ICmp(v1, v2) => write!(f, "cmp {}, {}", v1.to_string(), v2.to_string()),
            Instr::ITest(v1, v2) => write!(f, "test {}, {}", v1.to_string(), v2.to_string()),

            Instr::ICMovE(r1, r2) => write!(f, "cmove {}, {}", r1.to_string(), r2.to_string()),
            Instr::ICMovG(r1, r2) => write!(f, "cmovg {}, {}", r1.to_string(), r2.to_string()),
            Instr::ICMovGE(r1, r2) => write!(f, "cmovge {}, {}", r1.to_string(), r2.to_string()),
            Instr::ICMovL(r1, r2) => write!(f, "cmovl {}, {}", r1.to_string(), r2.to_string()),
            Instr::ICMovLE(r1, r2) => write!(f, "cmovle {}, {}", r1.to_string(), r2.to_string()),

            Instr::IJe(label) => write!(f, "je {}", label),
            Instr::IJne(label) => write!(f, "jne {}", label),
            Instr::IJmp(label) => write!(f, "jmp {}", label),
            Instr::IJo(label) => write!(f, "jo {}", label),

            Instr::Label(label) => write!(f, "\n{}:", label),
        }
    }
}

impl Display for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Val::Imm(n) => write!(f, "{}", *n),
            Val::Reg(r) => r.fmt(f),
            Val::RegOffset(r, v) => write!(f, "[{} - {}]", r.to_string(), v),
        }
    }
}

impl Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Reg::RAX => write!(f, "rax"),
            Reg::RBX => write!(f, "rbx"),
            Reg::RDI => write!(f, "rdi"),
            Reg::RSP => write!(f, "rsp"),
        }
    }
}