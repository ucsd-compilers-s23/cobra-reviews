#[derive(Debug)]
pub enum Val {
    VReg(Reg),
    VImm(i64),
    VRegOff(Reg, i64),
}

#[derive(Debug, Clone, Copy)]
pub enum Reg {
    RAX,
    RBX,
    RDI,
    RSP,
}

#[derive(Debug)]
pub enum Instr {
    // Move
    IMov(Val, Val),

    // Arithmetic
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),

    // Comparison
    ICmp(Val, Val),
    ITest(Val, Val),

    // Conditional Move
    ICMove(Val, Val),
    ICMovg(Val, Val),
    ICMovge(Val, Val),
    ICMovl(Val, Val),
    ICMovle(Val, Val),

    // Shifts
    ISar(Val, Val),

    // Bitwise
    IAnd(Val, Val),
    IOr(Val, Val),
    IXor(Val, Val),
    INot(Val),

    // Label
    ILabel(String),

    // Jumps
    IJmp(String),
    IJe(String),
    IJne(String),
    IJnz(String),
    IJo(String),

    // Function conventions
    IPush(Val),
    ICall(String),
    IRet(),

    // Other
    NoInstr(), // similar to a nop, but represented as just an empty string.
               // ISar(Val, Val),
}

pub fn instr_to_str(instr: &Instr) -> String {
    match instr {
        // Mov
        Instr::IMov(val1, val2) => {
            let str_val1 = val_to_str(val1);
            let str_val2 = val_to_str(val2);
            return format!("mov {str_val1}, {str_val2}");
        }

        // Arithmetic
        Instr::IAdd(val1, val2) => {
            let str_val1 = val_to_str(val1);
            let str_val2 = val_to_str(val2);
            return format!("add {str_val1}, {str_val2}");
        }
        Instr::ISub(val1, val2) => {
            let str_val1 = val_to_str(val1);
            let str_val2 = val_to_str(val2);
            return format!("sub {str_val1}, {str_val2}");
        }
        Instr::IMul(val1, val2) => {
            let str_val1 = val_to_str(val1);
            let str_val2 = val_to_str(val2);
            return format!("imul {str_val1}, {str_val2}");
        }

        // Comparison
        Instr::ICmp(val1, val2) => {
            let str_val1 = val_to_str(val1);
            let str_val2 = val_to_str(val2);
            return format!("cmp {str_val1}, {str_val2}");
        }

        Instr::ITest(val1, val2) => {
            let str_val1 = val_to_str(val1);
            let str_val2 = val_to_str(val2);
            return format!("test {str_val1}, {str_val2}");
        }

        // Conditional move
        Instr::ICMove(val1, val2) => {
            let str_val1 = val_to_str(val1);
            let str_val2 = val_to_str(val2);
            return format!("cmove {str_val1}, {str_val2}");
        }
        Instr::ICMovg(val1, val2) => {
            let str_val1 = val_to_str(val1);
            let str_val2 = val_to_str(val2);
            return format!("cmovg {str_val1}, {str_val2}");
        }
        Instr::ICMovge(val1, val2) => {
            let str_val1 = val_to_str(val1);
            let str_val2 = val_to_str(val2);
            return format!("cmovge {str_val1}, {str_val2}");
        }
        Instr::ICMovl(val1, val2) => {
            let str_val1 = val_to_str(val1);
            let str_val2 = val_to_str(val2);
            return format!("cmovl {str_val1}, {str_val2}");
        }
        Instr::ICMovle(val1, val2) => {
            let str_val1 = val_to_str(val1);
            let str_val2 = val_to_str(val2);
            return format!("cmovle {str_val1}, {str_val2}");
        }

        // Bitwise
        Instr::IAnd(val1, val2) => {
            let str_val1 = val_to_str(val1);
            let str_val2 = val_to_str(val2);
            return format!("and {str_val1}, {str_val2}");
        }
        Instr::IOr(val1, val2) => {
            let str_val1 = val_to_str(val1);
            let str_val2 = val_to_str(val2);
            return format!("or {str_val1}, {str_val2}");
        }

        Instr::IXor(val1, val2) => {
            let str_val1 = val_to_str(val1);
            let str_val2 = val_to_str(val2);
            return format!("xor {str_val1}, {str_val2}");
        }
        Instr::INot(val) => {
            return format!("not {}", val_to_str(val));
        }

        // Label
        Instr::ILabel(label) => {
            return format!("{label}:");
        }

        // Jumps
        Instr::IJmp(label) => {
            return format!("jmp {label}");
        }
        Instr::IJe(label) => {
            return format!("je {label}");
        }
        Instr::IJne(label) => {
            return format!("jne {label}");
        }
        Instr::IJnz(label) => {
            return format!("jnz {label}");
        }
        Instr::IJo(label) => {
            return format!("jo {label}");
        }

        // Shifts
        Instr::ISar(src, shift_amount) => {
            let str_src = val_to_str(src);
            let str_amount = val_to_str(shift_amount);
            return format!("sar {str_src}, {str_amount}");
        }
        Instr::IPush(val) => format!("push {}", val_to_str(val)),

        Instr::ICall(label) => format!("call {label}"),
        Instr::IRet() => format!("ret"),
        Instr::NoInstr() => format!(""),
    }
}

fn val_to_str(val: &Val) -> String {
    match val {
        Val::VImm(num) => format!("{num}"),
        Val::VReg(Reg::RAX) => format!("rax"),
        Val::VReg(Reg::RBX) => format!("rbx"),
        Val::VReg(Reg::RDI) => format!("rdi"),
        Val::VReg(Reg::RSP) => format!("rsp"),
        Val::VRegOff(Reg::RAX, offset) => format!("[rax - {offset}]"),
        Val::VRegOff(Reg::RSP, offset) => format!("[rsp - {offset}]"),

        _ => panic!("Unhandled Instruction value: {val:?}"),
    }
}
