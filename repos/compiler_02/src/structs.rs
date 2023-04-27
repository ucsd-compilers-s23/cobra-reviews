use dynasmrt::{dynasm, DynasmApi};
use im::{hashmap, HashMap};
use std::fmt;
use std::fmt::Debug;

// Parser structs/enums

#[derive(Debug)]
pub enum Op1 {
    Add1,
    Sub1,
    IsNum,
    IsBool,
}

#[derive(Debug)]
pub enum Op2 {
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
pub enum Expr {
    Num(i64),
    Boolean(bool),
    Var(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    Define(String, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
    Set(String, Box<Expr>),
    Block(Vec<Expr>),
}

// Compiler structs/enums

// Label holds a name for an assembly label
// of the form {name}_{index}
#[derive(Clone, Debug)]
pub struct Label {
    pub name: String,
    pub index: i32,
}

// Labels created with name do not have a _{index} prefix
// Used for labling snek_error and other such predefined ones
impl Label {
    pub fn new(s: Option<&str>) -> Label {
        if let Some(s_) = s {
            // Fixed label
            Label {
                name: s_.to_string(),
                index: -1,
            }
        } else {
            // Mutable label
            Label {
                name: "".to_string(),
                index: 0,
            }
        }
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.index >= 0 {
            write!(f, "{}_{}", self.name, self.index)
        } else {
            write!(f, "{}", self.name)
        }
    }
}

// VarEnv is the compiler's env for each variable
// Currently only includes the RSP offset
#[derive(Clone, Debug)]
pub struct VarEnv {
    pub offset: i32,
}

impl VarEnv {
    pub fn new(offset: i32) -> VarEnv {
        VarEnv { offset: offset }
    }
}

// Compiler's mutable context
// Currently includes only a mutable label index counter
#[derive(Clone, Debug)]
pub struct ContextMut {
    pub label_index: i32,
}

impl ContextMut {
    pub fn new() -> ContextMut {
        ContextMut { label_index: 0 }
    }
    // index_used marks the label index as used
    pub fn index_used(&mut self) {
        self.label_index += 1;
    }
    // Get a new compiler context with the new label
    pub fn new_ce_label(&self, ce: &Context, l: Label) -> Context {
        ce.modify_label(l)
    }
    // get a new label with the current label_index
    pub fn label(&self, s: &str) -> Label {
        Label {
            name: s.to_string(),
            index: self.label_index,
        }
    }
}

// Compiler's immutable Context
// Contains the stack index, environment for scoped variables
// and the current label, which acts as a break target
#[derive(Clone, Debug)]
pub struct Context {
    pub si: i32,
    pub env: HashMap<String, VarEnv>,
    pub label: Label,
}

impl Context {
    pub fn new() -> Context {
        Context {
            si: 2,
            env: hashmap! {},
            label: Label::new(None),
        }
    }
    // Modify and get a new Context for any of the fields
    pub fn modify(
        &self,
        si: Option<i32>,
        env: Option<HashMap<String, VarEnv>>,
        label: Option<Label>,
    ) -> Context {
        Context {
            si: si.unwrap_or(self.si),
            env: env.unwrap_or(self.env.clone()),
            label: label.unwrap_or(self.label.clone()),
        }
    }
    pub fn modify_si(&self, si: i32) -> Context {
        Context {
            si: si,
            env: self.env.clone(),
            label: self.label.clone(),
        }
    }
    // pub fn modify_env(&self, env: HashMap<String, VarEnv>) -> CompileEnv {
    //     CompileEnv {
    //         si: self.si,
    //         env: env,
    //         label: self.label.clone(),
    //     }
    // }
    pub fn modify_label(&self, label: Label) -> Context {
        Context {
            si: self.si,
            env: self.env.clone(),
            label: label,
        }
    }
}

// Jump instructions
#[derive(Debug)]
#[allow(dead_code)]
pub enum Jump {
    // Unconditional
    U(Label),
    // Notequal
    Ne(Label),
    // Nonzero
    Nz(Label),
    // Equal
    E(Label),
    // Zero
    Z(Label),
    // Overflow
    O(Label),
}

// Equal and Zero are both compiled to equal, since they are same in assembly too
impl fmt::Display for Jump {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Jump::U(x) => write!(f, "jmp {}", x),
            Jump::Ne(x) | Jump::Nz(x) => write!(f, "jne {}", x),
            Jump::E(x) | Jump::Z(x) => write!(f, "je {}", x),
            Jump::O(x) => write!(f, "jo {}", x),
        }
    }
}

// CMovXX instructions
#[derive(Debug)]
pub enum CMov {
    // Equal
    E(Reg, Arg),
    // Zero
    Z(Reg, Arg),
    // Greater
    G(Reg, Arg),
    // GreaterEqual
    GE(Reg, Arg),
    // Less
    L(Reg, Arg),
    // LessEqual
    LE(Reg, Arg),
}

impl fmt::Display for CMov {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CMov::E(r, a) => write!(f, "cmove {r}, {a}"),
            CMov::Z(r, a) => write!(f, "cmovz {r}, {a}"),
            CMov::G(r, a) => write!(f, "cmovg {r}, {a}"),
            CMov::GE(r, a) => write!(f, "cmovge {r}, {a}"),
            CMov::L(r, a) => write!(f, "cmovl {r}, {a}"),
            CMov::LE(r, a) => write!(f, "cmovle {r}, {a}"),
        }
    }
}

// asm implementations are for REPL
impl CMov {
    fn asm(&self, ops: &mut dynasmrt::x64::Assembler) {
        match self {
            CMov::E(r, a) => match a {
                OReg(o) => dynasm!(ops; .arch x64; cmove Rq(r.asm()), Rq(o.asm())),
                Mem(m) => {
                    dynasm!(ops; .arch x64; cmove Rq(r.asm()), [Rq(m.reg.asm()) - m.offset*8])
                }
                Imm(_) => panic!("cannot use immediate with cmove"),
                Imm64(_) => panic!("cannot use immediate with cmove"),
            },
            CMov::Z(r, a) => match a {
                OReg(o) => dynasm!(ops; .arch x64; cmovz Rq(r.asm()), Rq(o.asm())),
                Mem(m) => {
                    dynasm!(ops; .arch x64; cmovz Rq(r.asm()), [Rq(m.reg.asm()) - m.offset*8])
                }
                Imm(_) => panic!("cannot use immediate with cmovz"),
                Imm64(_) => panic!("cannot use immediate with cmovz"),
            },
            CMov::G(r, a) => match a {
                OReg(o) => dynasm!(ops; .arch x64; cmovg Rq(r.asm()), Rq(o.asm())),
                Mem(m) => {
                    dynasm!(ops; .arch x64; cmovg Rq(r.asm()), [Rq(m.reg.asm()) - m.offset*8])
                }
                Imm(_) => panic!("cannot use immediate with cmovg"),
                Imm64(_) => panic!("cannot use immediate with cmovg"),
            },
            CMov::GE(r, a) => match a {
                OReg(o) => dynasm!(ops; .arch x64; cmovge Rq(r.asm()), Rq(o.asm())),
                Mem(m) => {
                    dynasm!(ops; .arch x64; cmovge Rq(r.asm()), [Rq(m.reg.asm()) - m.offset*8])
                }
                Imm(_) => panic!("cannot use immediate with cmovge"),
                Imm64(_) => panic!("cannot use immediate with cmovge"),
            },
            CMov::L(r, a) => match a {
                OReg(o) => dynasm!(ops; .arch x64; cmovl Rq(r.asm()), Rq(o.asm())),
                Mem(m) => {
                    dynasm!(ops; .arch x64; cmovl Rq(r.asm()), [Rq(m.reg.asm()) - m.offset*8])
                }
                Imm(_) => panic!("cannot use immediate with cmovl"),
                Imm64(_) => panic!("cannot use immediate with cmovl"),
            },
            CMov::LE(r, a) => match a {
                OReg(o) => dynasm!(ops; .arch x64; cmovle Rq(r.asm()), Rq(o.asm())),
                Mem(m) => {
                    dynasm!(ops; .arch x64; cmovle Rq(r.asm()), [Rq(m.reg.asm()) - m.offset*8])
                }
                Imm(_) => panic!("cannot use immediate with cmovle"),
                Imm64(_) => panic!("cannot use immediate with cmovle"),
            },
        }
    }
}


// Register types
#[derive(Clone, Debug)]
pub enum Reg {
    Rax,
    Rcx,
    Rbx,
    Rsp,
    Rdi,
}
use Reg::*;

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Rax => write!(f, "rax"),
            Rcx => write!(f, "rcx"),
            Rbx => write!(f, "rbx"),
            Rsp => write!(f, "rsp"),
            Rdi => write!(f, "rdi"),
        }
    }
}

impl Reg {
    fn asm(&self) -> u8 {
        // https://corsix.github.io/dynasm-doc/instructions.html#registers
        match self {
            Rax => 0,
            Rcx => 1,
            Rbx => 3,
            Rsp => 4,
            Rdi => 7,
        }
    }
}

// MemRef type includes the register and offset to consider
#[derive(Clone, Debug)]
pub struct MemRef {
    pub reg: Reg,
    pub offset: i32,
}

// We are always working with QWORD.
// QWORD is needed when moving imm to memory.
impl fmt::Display for MemRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "QWORD [{}-{}]", self.reg, self.offset * 8)
    }
}

// Arg type, containing all possible entries for a assembly isntruction input
// OReg (Other Register) is when both inputs are registers
#[derive(Clone, Debug)]
pub enum Arg {
    OReg(Reg),
    Imm(i32),
    Imm64(i64),
    Mem(MemRef),
}
use Arg::*;

impl fmt::Display for Arg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            OReg(r) => write!(f, "{r}"),
            Imm(i) => write!(f, "{i}"),
            Imm64(i) => write!(f, "{i}"),
            Mem(m) => write!(f, "{m}"),
        }
    }
}

// Args are used when an instruction supports a memory target along with a register target
#[derive(Debug)]
pub enum Args {
    ToReg(Reg, Arg),
    ToMem(MemRef, Arg),
}
use Args::*;

impl fmt::Display for Args {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ToMem(r, a) => write!(f, "{r}, {a}"),
            ToReg(m, r) => write!(f, "{m}, {r}"),
        }
    }
}

// All used assembly instructions
#[derive(Debug)]
#[allow(dead_code)]
pub enum Instr {
    Mov(Args),
    Cmp(Args),
    Test(Args),
    CMovI(CMov),
    And(Reg, Arg),
    Add(Reg, Arg),
    Sub(Reg, Arg),
    Mul(Reg, Arg),
    Xor(Reg, Arg),
    Sar(Reg, i8),
    LabelI(Label),
    JumpI(Jump),
}
use Instr::*;

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Mov(m) => write!(f, "mov {m}"),
            Cmp(m) => write!(f, "cmp {m}"),
            Test(m) => write!(f, "test {m}"),
            CMovI(c) => write!(f, "{c}"),
            JumpI(j) => write!(f, "{j}"),
            LabelI(l) => write!(f, "{l}:"),
            And(r, a) => write!(f, "and {r}, {a}"),
            Add(r, a) => write!(f, "add {r}, {a}"),
            Sub(r, a) => write!(f, "sub {r}, {a}"),
            Mul(r, a) => write!(f, "imul {r}, {a}"),
            Xor(r, a) => write!(f, "xor {r}, {a}"),
            Sar(r, i) => write!(f, "sar {r}, {i}"),
        }
    }
}

// Dynasm implementation for REPL
impl Instr {
    pub fn asm(&self, ops: &mut dynasmrt::x64::Assembler) {
        match self {
            Mov(m) => match m {
                ToReg(r, a) => match a {
                    OReg(o) => dynasm!(ops; .arch x64; mov Rq(r.asm()), Rq(o.asm())),
                    Imm(n) => dynasm!(ops; .arch x64; mov Rq(r.asm()), *n),
                    Imm64(n) => dynasm!(ops; .arch x64; mov Rq(r.asm()), QWORD *n),
                    Mem(m) => {
                        dynasm!(ops; .arch x64; mov Rq(r.asm()), [Rq(m.reg.asm()) - m.offset*8])
                    }
                },
                ToMem(m, a) => match a {
                    OReg(r) => {
                        dynasm!(ops; .arch x64; mov [Rq(m.reg.asm()) - m.offset*8], Rq(r.asm()))
                    }
                    Imm(n) => {
                        dynasm!(ops; .arch x64; mov QWORD [Rq(m.reg.asm()) - m.offset*8], *n)
                    }
                    Imm64(_) => panic!("cannot mov from imm64 to memory"),
                    Mem(_) => panic!("cannot mov from memory to memory"),
                },
            },
            Cmp(m) => match m {
                ToReg(r, a) => match a {
                    OReg(o) => dynasm!(ops; .arch x64; cmp Rq(r.asm()), Rq(o.asm())),
                    Imm(n) => dynasm!(ops; .arch x64; cmp Rq(r.asm()), *n),
                    Imm64(_) => panic!("cannot cmp with imm64"),
                    Mem(m) => {
                        dynasm!(ops; .arch x64; cmp Rq(r.asm()), [Rq(m.reg.asm()) - m.offset*8])
                    }
                },
                ToMem(m, a) => match a {
                    OReg(r) => {
                        dynasm!(ops; .arch x64; cmp [Rq(m.reg.asm()) - m.offset*8], Rq(r.asm()))
                    }
                    Imm(n) => {
                        dynasm!(ops; .arch x64; cmp QWORD [Rq(m.reg.asm()) - m.offset*8], *n)
                    }
                    Imm64(_) => panic!("cannot cmp with imm64"),
                    Mem(_) => panic!("cannot cmp from memory to memory"),
                },
            },
            Test(m) => match m {
                ToReg(r, a) => match a {
                    OReg(o) => dynasm!(ops; .arch x64; test Rq(r.asm()), Rq(o.asm())),
                    Imm(n) => dynasm!(ops; .arch x64; test Rq(r.asm()), DWORD *n),
                    Imm64(_) => panic!("cannot test with imm64"),
                    Mem(m) => {
                        dynasm!(ops; .arch x64; test Rq(r.asm()), [Rq(m.reg.asm()) - m.offset*8])
                    }
                },
                ToMem(m, a) => match a {
                    OReg(r) => {
                        dynasm!(ops; .arch x64; test [Rq(m.reg.asm()) - m.offset*8], Rq(r.asm()))
                    }
                    Imm(n) => {
                        dynasm!(ops; .arch x64; test QWORD [Rq(m.reg.asm()) - m.offset*8], DWORD *n)
                    }
                    Imm64(_) => panic!("cannot test with imm64"),
                    Mem(_) => panic!("cannot test from memory to memory"),
                },
            },
            CMovI(c) => c.asm(ops),
            And(r, a) => match a {
                OReg(o) => dynasm!(ops; .arch x64; and Rq(r.asm()), Rq(o.asm())),
                Imm(n) => dynasm!(ops; .arch x64; and Rq(r.asm()), *n),
                Imm64(_) => panic!("cannot and with imm64"),
                Mem(m) => dynasm!(ops; .arch x64; and Rq(r.asm()), [Rq(m.reg.asm()) - m.offset*8]),
            },
            Add(r, a) => match a {
                OReg(o) => dynasm!(ops; .arch x64; add Rq(r.asm()), Rq(o.asm())),
                Imm(n) => dynasm!(ops; .arch x64; add Rq(r.asm()), *n),
                Imm64(_) => panic!("cannot add with imm64"),
                Mem(m) => dynasm!(ops; .arch x64; add Rq(r.asm()), [Rq(m.reg.asm()) - m.offset*8]),
            },
            Sub(r, a) => match a {
                OReg(o) => dynasm!(ops; .arch x64; sub Rq(r.asm()), Rq(o.asm())),
                Imm(n) => dynasm!(ops; .arch x64; sub Rq(r.asm()), *n),
                Imm64(_) => panic!("cannot sub with imm64"),
                Mem(m) => dynasm!(ops; .arch x64; sub Rq(r.asm()), [Rq(m.reg.asm()) - m.offset*8]),
            },
            Mul(r, a) => match a {
                OReg(r) => dynasm!(ops; .arch x64; imul Rq(r.asm()), Rq(r.asm())),
                Mem(m) => dynasm!(ops; .arch x64; imul Rq(r.asm()), [Rq(m.reg.asm()) - m.offset*8]),
                _ => panic!("mul does not support this operand"),
            },
            Xor(r, a) => match a {
                OReg(r) => dynasm!(ops; .arch x64; xor Rq(r.asm()), Rq(r.asm())),
                Mem(m) => dynasm!(ops; .arch x64; xor Rq(r.asm()), [Rq(m.reg.asm()) - m.offset*8]),
                Imm(n) => dynasm!(ops; .arch x64; xor Rq(r.asm()), DWORD *n),
                Imm64(_) => panic!("cannot add with imm64"),
            },
            Sar(r, i) => dynasm!(ops; .arch x64; sar Rq(r.asm()), *i),
            _ => todo!(),
        }
    }
}
