use std::fmt::format;

pub enum Instr{
    IMov(Val, Val),
    CMov(Val, Val),
    CMovne(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    IComp(Val, Val),
    Xor(Val, Val),
    Test(Val, Val),
    Shr(Val, Val),
    Sar(Val, Val),
    Je(String),
    Jne(String),
    Jl(String),
    Jle(String),
    Jg(String),
    Jc(String),
    Jo(String),
    Jge(String),
    Jmp(String),
    Label(String),
}

pub fn instr_to_string(instr: &Instr) -> String{
    match instr{
        Instr::IMov(v1, v2 ) => format!("\nmov {}, {}", val_to_string(v1), val_to_string(v2)),
        Instr::CMov(v1, v2 ) => format!("\ncmove {}, {}", val_to_string(v1), val_to_string(v2)),
        Instr::CMovne(v1, v2 ) => format!("\ncmovne {}, {}", val_to_string(v1), val_to_string(v2)),
        Instr::IAdd(v1, v2 ) => format!("\nadd {}, {}", val_to_string(v1), val_to_string(v2)),
        Instr::ISub(v1, v2 ) => format!("\nsub {}, {}", val_to_string(v1), val_to_string(v2)),
        Instr::IMul(v1, v2 ) => format!("\nimul {}, {}", val_to_string(v1), val_to_string(v2)),
        Instr::IComp(v1, v2) => format!("\ncmp {}, {}", val_to_string(v1), val_to_string(v2)),
        Instr::Xor(v1, v2 ) => format!("\nxor {}, {}", val_to_string(v1), val_to_string(v2)),
        Instr::Test(v1, v2 ) => format!("\ntest {}, {}", val_to_string(v1), val_to_string(v2)),
        Instr::Shr(v1, v2) => format!("\nshr {}, {}", val_to_string(v1), val_to_string(v2)),
        Instr::Sar(v1, v2) => format!("\nsar {}, {}", val_to_string(v1), val_to_string(v2)),
        Instr::Je(label) => format!("\nje {label}"),
        Instr::Jne(label) => format!("\njne {label}"),
        Instr::Jl(label) => format!("\njl {label}"),
        Instr::Jle(label) => format!("\njle {label}"),
        Instr::Jg(label) => format!("\njg {label}"),
        Instr::Jc(label) => format!("\njc {label}"),
        Instr::Jo(label) => format!("\njo {label}"),
        Instr::Jge(label) => format!("\njge {label}"),
        Instr::Label(label) => format!("\n{label}:"),
        Instr::Jmp(label) => format!("\njmp {label}"),
    }
}

pub enum Reg {
    RAX,
    RBX,
    RDI,
    RSP
}

fn reg_to_string(reg: &Reg) -> String{
    match reg{
        Reg::RAX => format!("rax "),
        Reg::RBX => format!("rbx"),
        Reg::RSP => format!("rsp "),
        Reg::RDI => format!("rdi ")
    }
}

pub enum Val{
    Reg(Reg),
    Imm(i64),
    RegOffset(Reg, i32)
}

fn val_to_string(val: &Val) -> String{
    match val{
        Val::Reg(reg) => reg_to_string(reg),
        Val::Imm(n) => format!("{}", *n),
        Val::RegOffset(reg, offset ) => format!("[ {} - {offset} ]", reg_to_string(reg))
    }
}
