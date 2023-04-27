use im::{HashMap, HashSet};

#[derive(Debug)]
enum Val {
    Reg(Reg),
    Imm(i64),
    SysImm(i32),
    RegOffset(Reg, i32),
    Label(String),
}

#[derive(Debug)]
enum Reg {
    RAX,
    RBX,
    RSP,
    RDI,
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    NEG(Val),
    SAR(Val, Val),
    SHL(Val, Val),
    AND(Val, Val),
    OR(Val, Val),
    XOR(Val, Val),
    TEST(Val, Val),
    JMP(Val),
    JE(Val),
    JO(Val),
    JNE(Val),
    CMP(Val, Val),
    CMOVE(Val, Val),
    CMOVG(Val, Val),
    CMOVL(Val, Val),
    CMOVGE(Val, Val),
    CMOVLE(Val, Val),
    Label(String),
}

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

// Compile and Expr to a vector of instructions
fn compile_to_instrs(e: &Expr, si: i32, env: &HashMap<String, i32>, label_count: &mut i32, break_target: &String) -> Vec<Instr> {
    match e {
        // Numbers, Booleans, and Ids move their corresponding value into RAX
        Expr::Number(n) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n))],
        Expr::Boolean(b) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::SysImm(if *b { 3 } else { 1 }))],
        Expr::Id(s) => match s.as_str() {
            "input" => {
                vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI))]  
            }
            _ => {
                vec![Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, match env.get(s) {
                    Some(i) => *i,
                    None => panic!("Unbound variable identifier {}", s),
                }))]
            }
        }
        Expr::Let(bindings, body) => {
            check_duplicate_bindings(bindings);
            let mut instrs = Vec::new();
            let mut new_env = env.clone();
            for (i, (s, e)) in bindings.iter().enumerate() {
                instrs.extend(compile_to_instrs(e, si + i as i32, &new_env, label_count, break_target));     // Compile each binding
                instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, si + i as i32), Val::Reg(Reg::RAX)));       // Store the result on the stack
                new_env.insert(s.clone(), si + i as i32);                                                    // Store the stack index in the environment
            }
            instrs.extend(compile_to_instrs(body, si + bindings.len() as i32, &new_env, label_count, break_target));
            instrs
        }
        Expr::UnOp(op, e) => {
            let mut instrs = compile_to_instrs(e, si, env, label_count, break_target);
            instrs.extend(match op {
                Op1::Add1 | Op1::Sub1 => vec![Instr::TEST(Val::Reg(Reg::RAX), Val::SysImm(1)),  // Checks if argument is numeric
                                              Instr::JNE(Val::Label("throw_type_error".to_string()))],
                Op1::IsBool | Op1::IsNum => vec![] // No type checking needed
            });
            instrs.extend(match op {
                Op1::Add1 => vec![Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1))],
                Op1::Sub1 => vec![Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(1))],
                Op1::IsNum => vec![Instr::AND(Val::Reg(Reg::RAX), Val::SysImm(1)),   // Mask out all but the tag bit
                                   Instr::XOR(Val::Reg(Reg::RAX), Val::SysImm(1)),   // Flip the tag bit
                                   Instr::SHL(Val::Reg(Reg::RAX), Val::SysImm(1)),   // Shift the tag bit once to the left
                                   Instr::IAdd(Val::Reg(Reg::RAX), Val::SysImm(1))], // Make the new tag bit 1
                Op1::IsBool => vec![Instr::AND(Val::Reg(Reg::RAX), Val::SysImm(1)),     // Mask out all but the tag bit
                                    Instr::SHL(Val::Reg(Reg::RAX), Val::SysImm(1)),     // Shift the tag bit once to the left
                                    Instr::IAdd(Val::Reg(Reg::RAX), Val::SysImm(1))],   // Make the new tag bit 1
            });
            instrs.extend(match op {
                Op1::Add1 | Op1::Sub1 => vec![Instr::JO(Val::Label("throw_overflow_error".to_string()))],
                Op1::IsBool | Op1::IsNum => vec![] 
            });
            instrs
        }
        Expr::BinOp(op, e1, e2) => {
            let mut instrs = compile_to_instrs(e1, si, env, label_count, break_target);                       // Calculate the left expression
            instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, si),                                             // Save the left expression on the stack
                                    Val::Reg(Reg::RAX)));
            instrs.extend(compile_to_instrs(e2, si + 1, env, label_count, break_target));                     // Calculate the right expression, and save it in RAX
            instrs.extend(match op {
                Op2::Equal => vec![Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)),                       // Checks if both arguments are of the same type
                                   Instr::XOR(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, si)),
                                   Instr::TEST(Val::Reg(Reg::RBX), Val::SysImm(1)),
                                   Instr::JNE(Val::Label("throw_type_error".to_string()))],
                Op2::Greater | Op2::GreaterEqual | 
                Op2::Less | Op2::LessEqual | Op2::Minus | 
                Op2::Plus | Op2::Times => vec![Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)),           // Checks if both arguments are numeric
                                               Instr::OR(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, si)),
                                               Instr::TEST(Val::Reg(Reg::RBX), Val::SysImm(1)),
                                               Instr::JNE(Val::Label("throw_type_error".to_string()))], 
            });
            instrs.extend(match op {
                Op2::Plus => vec![Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si))],
                Op2::Minus => vec![Instr::ISub(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si)), 
                                   Instr::NEG(Val::Reg(Reg::RAX))],                                           // Negate the result to correct the order of the operands
                Op2::Times => vec![Instr::SAR(Val::Reg(Reg::RAX), Val::SysImm(1)),                            // Divide operand by two to counteract the effect of the multiplication
                                   Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si))], 
                Op2::Equal => vec![Instr::CMP(Val::RegOffset(Reg::RSP, si), Val::Reg(Reg::RAX)),              // Compare the two arguments
                                   Instr::IMov(Val::Reg(Reg::RBX), Val::SysImm(3)),                           // Prepare the true result in a register
                                   Instr::IMov(Val::Reg(Reg::RAX), Val::SysImm(1)),                           // Premptively set the result to false
                                   Instr::CMOVE(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))],                     // If the arguments are equal, set the result to true
                Op2::Greater => vec![Instr::CMP(Val::RegOffset(Reg::RSP, si), Val::Reg(Reg::RAX)),            // Each of the following comparison operations follow the same pattern as the equal operation
                                     Instr::IMov(Val::Reg(Reg::RBX), Val::SysImm(3)),
                                     Instr::IMov(Val::Reg(Reg::RAX), Val::SysImm(1)),
                                     Instr::CMOVG(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))],
                Op2::Less => vec![Instr::CMP(Val::RegOffset(Reg::RSP, si), Val::Reg(Reg::RAX)),
                                  Instr::IMov(Val::Reg(Reg::RBX), Val::SysImm(3)),
                                  Instr::IMov(Val::Reg(Reg::RAX), Val::SysImm(1)),
                                  Instr::CMOVL(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))],
                Op2::GreaterEqual => vec![Instr::CMP(Val::RegOffset(Reg::RSP, si), Val::Reg(Reg::RAX)),
                                          Instr::IMov(Val::Reg(Reg::RBX), Val::SysImm(3)),
                                          Instr::IMov(Val::Reg(Reg::RAX), Val::SysImm(1)),
                                          Instr::CMOVGE(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))],
                Op2::LessEqual => vec![Instr::CMP(Val::RegOffset(Reg::RSP, si), Val::Reg(Reg::RAX)),
                                       Instr::IMov(Val::Reg(Reg::RBX), Val::SysImm(3)),
                                       Instr::IMov(Val::Reg(Reg::RAX), Val::SysImm(1)),
                                       Instr::CMOVLE(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))],
                
            });
            instrs.push(Instr::JO(Val::Label("throw_overflow_error".to_string())));
            instrs
        }
        Expr::If(cond, then, els) => {
            let end_label = new_label(label_count, "ifend");
            let else_label = new_label(label_count, "ifelse");
            let mut instrs = compile_to_instrs(cond, si, env, label_count, break_target); // Calculate the condition
            instrs.push(Instr::CMP(Val::Reg(Reg::RAX), Val::SysImm(1)));                  // Check if the condition is false
            instrs.push(Instr::JE(Val::Label(else_label.clone())));                       // Jump to else if the condition is false
            instrs.extend(compile_to_instrs(then, si, env, label_count, break_target));   // Calculate the then expression
            instrs.push(Instr::JMP(Val::Label(end_label.clone())));                       // Jump to the end
            instrs.push(Instr::Label(else_label));                                        // Else label
            instrs.extend(compile_to_instrs(els, si, env, label_count, break_target));    // Calculate the else expression
            instrs.push(Instr::Label(end_label));                                         // End label
            instrs  
        }
        Expr::Loop(body) => {
            let start_label = new_label(label_count, "loopstart");
            let end_label = new_label(label_count, "loopend");
            let mut instrs = vec![Instr::Label(start_label.clone())];                 // Start label
            instrs.extend(compile_to_instrs(body, si, env, label_count, &end_label)); // Calculate the body
            instrs.push(Instr::JMP(Val::Label(start_label)));                         // Jump to the start
            instrs.push(Instr::Label(end_label));                                     // End label
            instrs
        }
        Expr::Break(break_expr) => {
            if break_target == "" { panic!("A break must be inside a loop"); }
            let mut instrs = compile_to_instrs(break_expr, si, env, label_count, break_target); // Calculate the break expression
            instrs.push(Instr::JMP(Val::Label(break_target.clone())));                          // Jump to the break target
            instrs
        }
        Expr::Set(var, val) => {
            if !env.contains_key(var) { panic!("Unbound variable identifier {}", var); }
            let mut instrs = compile_to_instrs(val, si, env, label_count, break_target);      // Calculate the value
            instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, env[var]), Val::Reg(Reg::RAX))); // Move the value to the variable
            instrs
        }
        Expr::Block(exprs) => {
            let mut instrs = vec![];
            for e in exprs {
                instrs.extend(compile_to_instrs(e, si, env, label_count, break_target)); // Calculate each expression
            }
            instrs
        }
    }
}

// Check a list of let bindings for duplicate bindings
fn check_duplicate_bindings(bindings: &Vec<(String, Expr)>) {
    let mut seen = HashSet::new();
    for (s, _) in bindings {
        if seen.contains(s) {
            panic!("Duplicate binding: {}", s);
        }
        seen.insert(s);
    }
}

// Create a new label with a given name
fn new_label(label_count: &mut i32, label_name: &str) -> String {
    let label = format!("{}{}", label_name, label_count);
    *label_count += 1;
    label
}
    
// Instr enum to its corresponding x86 assembly string
fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(dst, src) => format!("mov {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IAdd(dst, src) => format!("add {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::ISub(dst, src) => format!("sub {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IMul(dst, src) => format!("imul {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::NEG(dst) => format!("neg {}", val_to_str(dst)),
        Instr::SAR(dst, src) => format!("sar {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::SHL(dst, src) => format!("shl {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::AND(dst, src) => format!("and {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::OR(dst, src) => format!("or {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::XOR(dst, src) => format!("xor {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::TEST(dst, src) => format!("test {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::JNE(dst) => format!("jne {}", val_to_str(dst)),
        Instr::CMP(dst, src) => format!("cmp {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::CMOVE(dst, src) => format!("cmove {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::CMOVG(dst, src) => format!("cmovg {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::CMOVL(dst, src) => format!("cmovl {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::CMOVGE(dst, src) => format!("cmovge {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::CMOVLE(dst, src) => format!("cmovle {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::JMP(dst) => format!("jmp {}", val_to_str(dst)),
        Instr::JE(dst) => format!("je {}", val_to_str(dst)),
        Instr::Label(l) => format!("{}:", l),
        Instr::JO(dst) => format!("jo {}", val_to_str(dst)),
    }
}

// Val enum to its corresponding x86 assembly string
fn val_to_str(v: &Val) -> String {
    match v {
        Val::Reg(r) => match r {
            Reg::RAX => "rax".to_string(),
            Reg::RSP => "rsp".to_string(),
            Reg::RBX => "rbx".to_string(),
            Reg::RDI => "rdi".to_string(),
        },
        Val::Imm(i) => (i<<1).to_string(),
        Val::SysImm(i) => i.to_string(),
        Val::RegOffset(r, i) => format!("[{}-{}]", {
            match r {
                Reg::RAX => "rax".to_string(),
                Reg::RBX => "rbx".to_string(),
                Reg::RSP => "rsp".to_string(),
                Reg::RDI => "rdi".to_string(),
            }
        }, i * 8),
        Val::Label(l) => l.to_string(),
    }
}  

// Public interface to compile Expr to x86 assembly
pub fn compile(e: &Expr) -> String {
    let instrs = compile_to_instrs(e, 2, &HashMap::new(), &mut 0, &"".to_string());
    instrs.iter().map(instr_to_str).collect::<Vec<String>>().join("\n  ")
}
