use dynasmrt::{dynasm, DynamicLabel, DynasmApi, DynasmLabelApi};

#[derive(Clone, Copy, Debug)]
pub enum Val {
    Reg(Reg),
    Imm(i64),
    RegOffset(Reg, i32),
}

#[derive(Clone, Copy, Debug)]
pub enum Reg {
    RAX,
    RBX,
    RSP,
    RDI,
}

#[derive(Clone, Copy, Debug)]
pub enum Cond {
    Unconditional,
    Equal,
    NotEqual,
    Zero,
    NotZero,
    Overflow,
    NotOverflow,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

#[derive(Clone, Debug)]
pub enum Instr {
    IMov(Val, Val),
    ICmov(Cond, Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    ISar(Val, Val),
    ICmp(Val, Val),
    ITest(Val, Val),
    IJmp(Cond, String),
    ILbl(String),
    IPush(Val),
    ICall(String),
}

#[derive(Clone, Copy, Debug)]
pub enum UnOp {
    Add1,
    Sub1,
    IsNum,
    IsBool,
}

#[derive(Clone, Copy, Debug)]
pub enum BinOp {
    Plus,
    Minus,
    Times,
    Equal,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

#[derive(Clone, Debug)]
pub enum Expr {
    Number(i64),
    Boolean(bool),
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(UnOp, Box<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
    Set(String, Box<Expr>),
    Block(Vec<Expr>),
}

pub const RESERVED: [&str; 13] = [
    "let", "add1", "sub1", "isnum", "isbool", "if", "loop", "break", "set!", "block", "true",
    "false", "input",
];

#[derive(Clone, Copy, Debug)]
pub enum Type {
    Number,
    Boolean,
}

#[derive(Clone, Debug)]
pub struct MutState {
    pub instrs: Vec<Instr>,
    pub label_index: usize,
}

#[derive(Clone, Debug)]
pub struct ImmState {
    pub env: im::HashMap<String, i32>,
    pub stack_index: i32,
    pub break_label: String,
}

fn reg_to_idx(r: &Reg) -> u8 {
    match r {
        Reg::RAX => 0,
        Reg::RBX => 3,
        Reg::RSP => 4,
        Reg::RDI => 7,
    }
}

macro_rules! gen_asm {
    ($ops:ident, $x:ident, $y:ident, $instr:ident) => {
        match ($x, $y) {
            (Val::Reg(r), Val::Imm(i)) => {
                dynasm!($ops ; .arch x64 ; $instr Rq(reg_to_idx(r)), *i as _);
            }
            (Val::Reg(r0), Val::Reg(r1)) => {
                dynasm!($ops ; .arch x64 ; $instr Rq(reg_to_idx(r0)), Rq(reg_to_idx(r1)));
            }
            (Val::Reg(r0), Val::RegOffset(r1, i)) => {
                dynasm!($ops ; .arch x64 ; $instr Rq(reg_to_idx(r0)), [Rq(reg_to_idx(r1)) - (*i as i32) * 8]);
            }
            (Val::RegOffset(r0, i), Val::Reg(r1)) => {
                dynasm!($ops ; .arch x64 ; $instr [Rq(reg_to_idx(r0)) - (*i as i32) * 8], Rq(reg_to_idx(r1)));
            }
            _ => panic!("cannot match with generated asm")
        }
    };
}

pub fn instr_to_asm(
    i: &Instr,
    lbls: &im::HashMap<&String, DynamicLabel>,
    ops: &mut dynasmrt::x64::Assembler,
) {
    match i {
        Instr::IMov(Val::Reg(r), Val::Imm(i)) => {
            dynasm!(ops ; .arch x64 ; mov Rq(reg_to_idx(r)), QWORD *i);
        }
        Instr::IMov(x, y) => gen_asm!(ops, x, y, mov),
        Instr::IAdd(x, y) => gen_asm!(ops, x, y, add),
        Instr::ISub(x, y) => gen_asm!(ops, x, y, sub),
        Instr::ICmp(x, y) => gen_asm!(ops, x, y, cmp),
        Instr::ITest(x, y) => gen_asm!(ops, x, y, test),
        Instr::IMul(Val::Reg(r), Val::Imm(i)) => {
            dynasm!(ops ; .arch x64 ; imul Rq(reg_to_idx(r)), Rq(reg_to_idx(r)), *i as _)
        }
        Instr::IMul(Val::Reg(r0), Val::RegOffset(r1, i)) => {
            dynasm!(ops ; .arch x64 ; imul Rq(reg_to_idx(r0)), [Rq(reg_to_idx(r1)) - (*i as i32) * 8]);
        }
        Instr::ISar(Val::Reg(r), Val::Imm(i)) => {
            dynasm!(ops ; .arch x64 ; sar Rq(reg_to_idx(r)), *i as _)
        }
        Instr::ILbl(l) => {
            dynasm!(ops ; .arch x64 ; =>lbls[l]);
        }
        Instr::ICall(_) => {
            dynasm!(ops ; .arch x64 ; mov rax, QWORD snek_error as _ ; call rax);
        }
        Instr::IPush(Val::Reg(r)) => {
            dynasm!(ops ; .arch x64 ; push Rq(reg_to_idx(r)));
        }
        Instr::ICmov(c, Val::Reg(r0), Val::Reg(r1)) => match c {
            Cond::Unconditional => panic!("invalid condition"),
            Cond::Equal => dynasm!(ops ; .arch x64 ; cmove Rq(reg_to_idx(r0)), Rq(reg_to_idx(r1))),
            Cond::NotEqual => {
                dynasm!(ops ; .arch x64 ; cmovne Rq(reg_to_idx(r0)), Rq(reg_to_idx(r1)))
            }
            Cond::Zero => dynasm!(ops ; .arch x64 ; cmovz Rq(reg_to_idx(r0)), Rq(reg_to_idx(r1))),
            Cond::NotZero => {
                dynasm!(ops ; .arch x64 ; cmovnz Rq(reg_to_idx(r0)), Rq(reg_to_idx(r1)))
            }
            Cond::Overflow => {
                dynasm!(ops ; .arch x64 ; cmovo Rq(reg_to_idx(r0)), Rq(reg_to_idx(r1)))
            }
            Cond::NotOverflow => {
                dynasm!(ops ; .arch x64 ; cmovno Rq(reg_to_idx(r0)), Rq(reg_to_idx(r1)))
            }
            Cond::Greater => {
                dynasm!(ops ; .arch x64 ; cmovg Rq(reg_to_idx(r0)), Rq(reg_to_idx(r1)))
            }
            Cond::GreaterEqual => {
                dynasm!(ops ; .arch x64 ; cmovge Rq(reg_to_idx(r0)), Rq(reg_to_idx(r1)))
            }
            Cond::Less => dynasm!(ops ; .arch x64 ; cmovl Rq(reg_to_idx(r0)), Rq(reg_to_idx(r1))),
            Cond::LessEqual => {
                dynasm!(ops ; .arch x64 ; cmovle Rq(reg_to_idx(r0)), Rq(reg_to_idx(r1)))
            }
        },
        Instr::IJmp(c, l) => match c {
            Cond::Unconditional => dynasm!(ops ; .arch x64 ; jmp =>lbls[l]),
            Cond::Equal => dynasm!(ops ; .arch x64 ; je =>lbls[l]),
            Cond::NotEqual => dynasm!(ops ; .arch x64 ; jne =>lbls[l]),
            Cond::Zero => dynasm!(ops ; .arch x64 ; jz =>lbls[l]),
            Cond::NotZero => dynasm!(ops ; .arch x64 ; jnz =>lbls[l]),
            Cond::Overflow => dynasm!(ops ; .arch x64 ; jo =>lbls[l]),
            Cond::NotOverflow => dynasm!(ops ; .arch x64 ; jno =>lbls[l]),
            Cond::Greater => dynasm!(ops ; .arch x64 ; jg =>lbls[l]),
            Cond::GreaterEqual => dynasm!(ops ; .arch x64 ; jge =>lbls[l]),
            Cond::Less => dynasm!(ops ; .arch x64 ; jl =>lbls[l]),
            Cond::LessEqual => dynasm!(ops ; .arch x64 ; jle =>lbls[l]),
        },
        _ => panic!("invalid instruction {:?}", i),
    }
}

#[export_name = "\x01snek_error"]
pub extern "C" fn snek_error(errcode: i64) {
    // TODO: print error message according to writeup
    match errcode {
        0 => eprintln!("overflow"),
        1 => eprintln!("invalid argument"),
        _ => eprintln!("unknown error code: {errcode}"),
    }
    std::process::exit(1);
}

pub fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(x, y) => format!("mov {}, {}", val_to_str(x), val_to_str(y)),
        Instr::ICmov(c, x, y) => format!(
            "cmov{} {}, {}",
            cond_to_str(c),
            val_to_str(x),
            val_to_str(y)
        ),
        Instr::IAdd(x, y) => format!("add {}, {}", val_to_str(x), val_to_str(y)),
        Instr::ISub(x, y) => format!("sub {}, {}", val_to_str(x), val_to_str(y)),
        Instr::IMul(x, y) => format!("imul {}, {}", val_to_str(x), val_to_str(y)),
        Instr::ISar(x, y) => format!("sar {}, {}", val_to_str(x), val_to_str(y)),
        Instr::ICmp(x, y) => format!("cmp {}, {}", val_to_str(x), val_to_str(y)),
        Instr::ITest(x, y) => format!("test {}, {}", val_to_str(x), val_to_str(y)),
        Instr::IJmp(c, l) => format!("j{} {l}", cond_to_str(c)),
        Instr::ILbl(l) => format!("{l}:"),
        Instr::IPush(x) => format!("push {}", val_to_str(x)),
        Instr::ICall(f) => format!("call {f}"),
    }
}

fn val_to_str(v: &Val) -> String {
    match v {
        Val::Reg(r) => reg_to_str(r),
        Val::Imm(i) => i.to_string(),
        Val::RegOffset(r, n) => format!("[{} - {}]", reg_to_str(r), n * 8),
    }
}

fn reg_to_str(r: &Reg) -> String {
    match r {
        Reg::RAX => "rax",
        Reg::RBX => "rbx",
        Reg::RSP => "rsp",
        Reg::RDI => "rdi",
    }
    .to_string()
}

fn cond_to_str(c: &Cond) -> String {
    match c {
        Cond::Unconditional => "mp",
        Cond::Equal => "e",
        Cond::NotEqual => "ne",
        Cond::Zero => "z",
        Cond::NotZero => "nz",
        Cond::Overflow => "o",
        Cond::NotOverflow => "no",
        Cond::Greater => "g",
        Cond::GreaterEqual => "ge",
        Cond::Less => "l",
        Cond::LessEqual => "le",
    }
    .to_string()
}

pub fn make_label(arg: &str, l: &mut usize) -> String {
    *l = *l + 1;
    format!("{arg}{l}")
}

pub fn print_value(i: u64) {
    if i % 2 == 0 {
        println!("{}", i as i64 >> 1);
    } else if i == 3 {
        println!("true");
    } else if i == 1 {
        println!("false");
    } else {
        println!("unknown value: {i}");
    }
}
