use rayon::prelude::*;
/// Contains the x85_64 assembly instructions used by the compiler
use std::fmt;

pub struct InstructionSequence(pub Vec<Instruction>);

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Instruction {
    LabelInstr(Label),
    Mov(Val, Val),
    Add(Val, Val),
    Sub(Val, Val),
    Mul(Val, Val),
    Imul(Val, Val),
    And(Val, Val),
    Or(Val, Val),
    Xor(Val, Val),
    Cmp(Val, Val),
    Test(Val, Val),
    Sal(Val, Val),
    Sar(Val, Val),
    Shl(Val, Val),
    Shr(Val, Val),
    Cmovcc(CC, Register, Register),
    Jmp(Label),
    Jcc(CC, Label),
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
pub enum Val {
    Register(Register),
    Immediate(i64),
    RegOffset(Register, i64),
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
pub enum Register {
    Rax,
    Rbx,
    Rcx,
    Rsp,
}

#[derive(Debug, Clone)]
pub struct Label(pub String);

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
/// The condition codes used by the x86_64 assembly language
/// https://treeniks.github.io/x86-64-simplified/condition-codes.html
pub enum CC {
    /// Equal
    E,
    /// Zero; equivilant to [E]
    Z,
    /// Not Equal
    Ne,
    /// Not Zero; equivilant to [Ne]
    Nz,
    /// Overflow
    O,
    /// No overflow
    No,
    /// Signed
    S,
    /// Not signed
    Ns,
    /// Parity (Even)
    P,
    /// No Parity (Odd)
    Np,
    /// Parity Even
    Pe,
    /// Parity Odd
    Po,
    /// Carry
    C,
    /// Below (See [C])
    B,
    /// Not Above or Equal (See [C])
    Nae,
    /// No Carry
    Nc,
    /// Not Below (See [Nc])
    Nb,
    /// Above or Equal (See [Nc])
    Ae,
    // Above
    A,
    /// Not Below or Equal (See [A])
    Nbe,
    /// Not Above (See [A])
    Na,
    /// Below or Equal (See [Na])
    Be,
    /// Greater or Equal (Signed)
    Ge,
    /// Not Less (Signed) (See [Ge])
    Nl,
    /// Not Greater or Equal (Signed) (See [L])
    Nge,
    /// Less (Signed)
    L,
    /// Greater (Signed)
    G,
    /// Not Less or Equal (Signed) (See [G])
    Nle,
    /// Not Greater (Signed) (See [G], [Le])
    Ng,
    /// Less or Equal (Signed)
    Le,
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instruction::LabelInstr(label) => write!(f, "{}:", label),
            Instruction::Mov(dest, src) => write!(f, "mov {}, {}", dest, src),
            Instruction::Add(dest, src) => write!(f, "add {}, {}", dest, src),
            Instruction::Sub(dest, src) => write!(f, "sub {}, {}", dest, src),
            Instruction::Mul(dest, src) => write!(f, "mul {}, {}", dest, src),
            Instruction::Imul(dest, src) => write!(f, "imul {}, {}", dest, src),
            Instruction::And(dest, src) => write!(f, "and {}, {}", dest, src),
            Instruction::Or(dest, src) => write!(f, "or {}, {}", dest, src),
            Instruction::Xor(dest, src) => write!(f, "xor {}, {}", dest, src),
            Instruction::Cmp(dest, src) => write!(f, "cmp {}, {}", dest, src),
            Instruction::Test(dest, src) => write!(f, "test {}, {}", dest, src),
            Instruction::Sal(dest, amount) => write!(f, "sal {}, {}", dest, amount),
            Instruction::Sar(dest, amount) => write!(f, "sar {}, {}", dest, amount),
            Instruction::Shl(dest, amount) => write!(f, "shl {}, {}", dest, amount),
            Instruction::Shr(dest, amount) => write!(f, "shr {}, {}", dest, amount),
            Instruction::Cmovcc(cc, dest, src) => write!(f, "cmov{} {}, {}", cc, dest, src),
            Instruction::Jmp(label) => write!(f, "jmp {}", label),
            Instruction::Jcc(cc, label) => write!(f, "j{} {}", cc, label),
        }
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Val::Register(reg) => write!(f, "{}", reg),
            Val::Immediate(imm) => write!(f, "{}", imm),
            Val::RegOffset(reg, offset) => {
                let operator = if *offset < 0 { "-" } else { "+" };
                let abs_offset = offset.abs();
                write!(f, "[{} {} {}]", reg, operator, abs_offset)
            }
        }
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Register::Rax => write!(f, "rax"),
            Register::Rbx => write!(f, "rbx"),
            Register::Rcx => write!(f, "rcx"),
            Register::Rsp => write!(f, "rsp"),
        }
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Default for InstructionSequence {
    fn default() -> Self {
        Self::new()
    }
}

impl InstructionSequence {
    pub fn new() -> InstructionSequence {
        InstructionSequence(Vec::new())
    }

    pub fn push(&mut self, instr: Instruction) {
        self.0.push(instr);
    }

    pub fn extend(&mut self, other: InstructionSequence) {
        self.0.extend(other.0);
    }
}

impl fmt::Display for InstructionSequence {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let code = self
            .0
            .par_iter()
            .map(|instr| match instr {
                Instruction::LabelInstr(_) => format!("{}", instr),
                _ => format!("\t{}", instr),
            })
            .collect::<Vec<String>>()
            .join("\n");

        write!(f, "{}", code)
    }
}

impl fmt::Display for CC {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CC::E => write!(f, "e"),
            CC::Z => write!(f, "z"),
            CC::Ne => write!(f, "ne"),
            CC::Nz => write!(f, "nz"),
            CC::O => write!(f, "o"),
            CC::No => write!(f, "no"),
            CC::S => write!(f, "s"),
            CC::Ns => write!(f, "ns"),
            CC::P => write!(f, "p"),
            CC::Np => write!(f, "np"),
            CC::Pe => write!(f, "pe"),
            CC::Po => write!(f, "po"),
            CC::C => write!(f, "c"),
            CC::B => write!(f, "b"),
            CC::Nae => write!(f, "nae"),
            CC::Nc => write!(f, "nc"),
            CC::Nb => write!(f, "nb"),
            CC::Ae => write!(f, "ae"),
            CC::A => write!(f, "a"),
            CC::Nbe => write!(f, "nbe"),
            CC::Na => write!(f, "na"),
            CC::Be => write!(f, "be"),
            CC::Ge => write!(f, "ge"),
            CC::Nl => write!(f, "nl"),
            CC::Nge => write!(f, "nge"),
            CC::L => write!(f, "l"),
            CC::G => write!(f, "g"),
            CC::Nle => write!(f, "nle"),
            CC::Ng => write!(f, "ng"),
            CC::Le => write!(f, "le"),
        }
    }
}
