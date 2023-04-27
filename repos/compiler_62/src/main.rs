use std::env;
use std::fs::*;

use sexp::Atom::*;
use sexp::*;

use im::HashMap;
use im::HashSet;

#[derive(Copy, Clone, Debug)]
enum Val {
    Reg(Reg),
    Imm(i64),
    RegOffset(Reg, i32),
}

#[derive(Copy, Clone, Debug)]
enum Reg {
    RAX,
    RBX,
    RDI,
    RSP,
}

#[derive(Copy, Clone, Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    IOr(Val, Val),
    IXor(Val, Val),
    INeg(Val),
    ISar(Val, i32),
    ICmp(Val, Val),
    ITest(Val, Val),
    ICmovg(Val, Val),
    ICmovge(Val, Val),
    ICmovl(Val, Val),
    ICmovle(Val, Val),
    ICmove(Val, Val),
    IJmp(i32),
    IJe(i32),
    IJne(i32),
    IJo(i32), // it's jover
    ILabel(i32),
}

#[derive(Copy, Clone, Debug)]
enum Op1 {
    Add1,
    Sub1,
    IsNum,
    IsBool,
}

#[derive(Copy, Clone, Debug)]
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
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
    Set(String, Box<Expr>),
    Block(Vec<Expr>),
}

fn parse_expr(s: &Sexp) -> Expr {
    let is_keyword = |id: &str| {
        match id {
            "add1" => true,
            "sub1" => true,
            "isnum" => true,
            "isbool" => true,
            "let" => true,
            "if" => true,
            "block" => true,
            "loop" => true,
            "set!" => true,
            "break" => true,
            "true" => true,
            "false" => true,
            "input" => true,
            _ => false,
        }
    };
    let parse_id = |id: &str| {
        if is_keyword(id) {
            match id {
                "true" => Some(Expr::Boolean(true)),
                "false" => Some(Expr::Boolean(false)),
                "input" => Some(Expr::Id("input".to_string())),
                _ => None,
            }
        } else {
            Some(Expr::Id(id.to_string()))
        }
    };
    match s {
        Sexp::Atom(I(n)) => {
            let leading_bits = (*n as u64) >> 62;
            if leading_bits == 1 || leading_bits == 2 {
                panic!("Invalid: bad numeric value");
            }
            Expr::Number(*n)
        }
        Sexp::Atom(S(id)) => {
            parse_id(&id).expect("Invalid: unexpected keyword")
        },
        Sexp::Atom(F(_)) => { panic!("Invalid: bad numeric value") },
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(op)), Sexp::List(binds), e] if op == "let" => {
                    let mut binds_p = Vec::new();
                    if binds.is_empty() {
                        panic!("Invalid: bad expression pattern");
                    }
                    for bind in binds {
                        if let Sexp::List(bind) = bind {
                            if let [Sexp::Atom(S(name)), expr] = &bind[..] {
                                if is_keyword(name) {
                                    panic!("Invalid: unexpected keyword");
                                }
                                binds_p.push((String::clone(name), parse_expr(expr)));
                            } else {
                                panic!("Invalid: bad expression pattern");
                            }
                        } else {
                            panic!("Invalid: bad expression pattern");
                        }
                    }
                    Expr::Let(binds_p, Box::new(parse_expr(e)))
                },
                [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => {
                    if exprs.is_empty() {
                        panic!("Invalid: bad expression pattern");
                    }
                    let mut exprs_p = Vec::new();
                    for e in exprs {
                        exprs_p.push(parse_expr(e));
                    }
                    Expr::Block(exprs_p)
                },
                [Sexp::Atom(S(op)), e1] if op == "add1" => Expr::UnOp(Op1::Add1, Box::new(parse_expr(e1))),
                [Sexp::Atom(S(op)), e1] if op == "sub1" => Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e1))),
                [Sexp::Atom(S(op)), e1] if op == "isnum" => Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e1))),
                [Sexp::Atom(S(op)), e1] if op == "isbool" => Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e1))),
                [Sexp::Atom(S(op)), e1, e2] if op == "+" =>
                    Expr::BinOp(Op2::Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "-" =>
                    Expr::BinOp(Op2::Minus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "*" =>
                    Expr::BinOp(Op2::Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "=" =>
                    Expr::BinOp(Op2::Equal, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == ">" =>
                    Expr::BinOp(Op2::Greater, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == ">=" =>
                    Expr::BinOp(Op2::GreaterEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "<" =>
                    Expr::BinOp(Op2::Less, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "<=" =>
                    Expr::BinOp(Op2::LessEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), eif, ethen, eelse] if op == "if" => {
                    Expr::If(Box::new(parse_expr(eif)), Box::new(parse_expr(ethen)), Box::new(parse_expr(eelse)))
                },
                [Sexp::Atom(S(op)), e] if op == "loop" => {
                    Expr::Loop(Box::new(parse_expr(e)))
                },
                [Sexp::Atom(S(op)), e] if op == "break" => {
                    Expr::Break(Box::new(parse_expr(e)))
                },
                [Sexp::Atom(S(op)), Sexp::Atom(S(name)), e] if op == "set!" => {
                    if is_keyword(name) {
                        panic!("Invalid: unexpected keyword");
                    }
                    Expr::Set(name.to_string(), Box::new(parse_expr(e)))
                },
                _ => panic!("Invalid: bad expression pattern"),
            }
        },
    }
}

fn instr_to_str(i: Instr) -> String {
    // oops! no testing for unencodable instructions
    let label_name = |l: i32| {
        if l < 0 {
            format!(".E{}", -l)
        } else {
            format!(".L{}", l)
        }
    };
    match i {
        Instr::IMov(dst, src) => format!("mov {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IAdd(dst, src) => format!("add {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::ISub(dst, src) => format!("sub {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IMul(dst, src) => format!("imul {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IOr(dst, src) => format!("or {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IXor(dst, src) => format!("xor {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::INeg(v) => format!("neg {}", val_to_str(v)),
        Instr::ISar(v, shm) => format!("sar {}, {}", val_to_str(v), shm),
        Instr::ICmp(v1, v2) => format!("cmp {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ITest(v1, v2) => format!("test {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ICmovg(v1, v2) => format!("cmovg {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ICmovge(v1, v2) => format!("cmovge {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ICmovl(v1, v2) => format!("cmovl {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ICmovle(v1, v2) => format!("cmovle {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ICmove(v1, v2) => format!("cmove {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::IJmp(l) => format!("jmp {}", label_name(l)),
        Instr::IJe(l) => format!("je {}", label_name(l)),
        Instr::IJne(l) => format!("jne {}", label_name(l)),
        Instr::IJo(l) => format!("jo {}", label_name(l)),
        Instr::ILabel(l) => format!("{}:", label_name(l)),
    }
}

fn val_to_str(v: Val) -> String {
    let reg_name = |r: Reg| {
        match r {
            Reg::RAX => "rax",
            Reg::RBX => "rbx",
            Reg::RSP => "rsp",
            Reg::RDI => "rdi",
        }
    };
    match v {
        Val::Reg(r) => format!("{}", reg_name(r)),
        Val::Imm(n) => format!("{}", n),
        Val::RegOffset(r, n) => match n {
            n if n < 0 => format!("[{} - {}]", reg_name(r), -n),
            n if n > 0 => format!("[{} + {}]", reg_name(r), n),
            _n => format!("[{}]", reg_name(r)),
        }
    }
}

fn h_compile_with(e: &Expr, instrs: &mut Vec<Instr>, si: i32, env: &HashMap<String, i32>, ls: &mut i32, brkl: Option<i32>) {
    let gen_label = |ls: &mut i32| {
        let l = *ls;
        *ls += 1;
        return l;
    };
    let rax = Val::Reg(Reg::RAX);
    let rbx = Val::Reg(Reg::RBX);
    let rdi = Val::Reg(Reg::RDI);
    let stack_slot = |si: i32| Val::RegOffset(Reg::RSP, si * -8);
    match e {
        Expr::Number(n) => {
            instrs.push(Instr::IMov(rax, Val::Imm(*n << 1)));
        },
        Expr::Boolean(b) => {
            instrs.push(Instr::IMov(rax, Val::Imm(if *b { 3 } else { 1 })));
        }
        Expr::Id(id) => {
            if id == "input" {
                instrs.push(Instr::IMov(rax, rdi));
            } else {
                let id_si = *env.get(id).expect(&format!("Unbound variable identifier {}", id));
                instrs.push(Instr::IMov(rax, stack_slot(id_si)));
            }
        },
        Expr::Let(binds, b) => {
            let mut env = env.clone();
            let mut local_bindings = HashSet::new();
            let mut si = si;
            for (name, e) in binds {
                if local_bindings.contains(name) {
                    panic!("Duplicate binding");
                }
                h_compile_with(e, instrs, si, &env, ls, brkl);
                instrs.push(Instr::IMov(stack_slot(si), rax));
                env = env.update(name.to_string(), si);
                local_bindings.insert(name.to_string());
                si += 1;
            }
            h_compile_with(b, instrs, si, &env, ls, brkl);
        },
        Expr::UnOp(op, e1) => {
            h_compile_with(e1, instrs, si, env, ls, brkl);
            let checknum1 = vec![
                Instr::ITest(rax, Val::Imm(1)),
                Instr::IJne(-1),
            ];
            let checkoverflow = vec![
                Instr::IJo(-2),
            ];
            match op {
                Op1::Add1 => {
                    instrs.extend_from_slice(&checknum1);
                    instrs.push(Instr::IAdd(rax, Val::Imm(2)));
                    instrs.extend_from_slice(&checkoverflow);
                },
                Op1::Sub1 => {
                    instrs.extend_from_slice(&checknum1);
                    instrs.push(Instr::ISub(rax, Val::Imm(2)));
                    instrs.extend_from_slice(&checkoverflow);
                },
                Op1::IsNum => {
                    instrs.push(Instr::ITest(rax, Val::Imm(1)));
                    instrs.push(Instr::IMov(rax, Val::Imm(1)));
                    instrs.push(Instr::IMov(rbx, Val::Imm(3)));
                    instrs.push(Instr::ICmove(rax, rbx));
                },
                Op1::IsBool => {
                    instrs.push(Instr::ITest(rax, Val::Imm(1)));
                    instrs.push(Instr::IMov(rax, Val::Imm(3)));
                    instrs.push(Instr::IMov(rbx, Val::Imm(1)));
                    instrs.push(Instr::ICmove(rax, rbx));
                },
            };
        },
        Expr::BinOp(op, e1, e2) => {
            h_compile_with(e1, instrs, si, env, ls, brkl);
            let tmp_stack_slot = stack_slot(si);
            instrs.push(Instr::IMov(tmp_stack_slot, rax));
            h_compile_with(e2, instrs, si + 1, env, ls, brkl);
            let checknum2 = vec![
                Instr::IMov(rbx, rax),
                Instr::IOr(rbx, tmp_stack_slot),
                Instr::ITest(rbx, Val::Imm(1)),
                Instr::IJne(-1),
            ];
            let checksame = vec![
                Instr::IMov(rbx, rax),
                Instr::IXor(rbx, tmp_stack_slot),
                Instr::ITest(rbx, Val::Imm(1)),
                Instr::IJne(-1),
            ];
            let checkoverflow = vec![
                Instr::IJo(-2),
            ];
            match op {
                Op2::Plus => {
                    instrs.extend_from_slice(&checknum2);
                    instrs.push(Instr::IAdd(rax, tmp_stack_slot));
                    instrs.extend_from_slice(&checkoverflow);
                },
                Op2::Minus => {
                    instrs.extend_from_slice(&checknum2);
                    instrs.push(Instr::INeg(rax));
                    instrs.push(Instr::IAdd(rax, tmp_stack_slot));
                    instrs.extend_from_slice(&checkoverflow);
                },
                Op2::Times => {
                    instrs.extend_from_slice(&checknum2);
                    instrs.push(Instr::ISar(rax, 1));
                    instrs.push(Instr::IMul(rax, tmp_stack_slot));
                    instrs.extend_from_slice(&checkoverflow);
                },
                Op2::Greater => {
                    instrs.extend_from_slice(&checknum2);
                    instrs.push(Instr::ICmp(tmp_stack_slot, rax));
                    instrs.push(Instr::IMov(rax, Val::Imm(1)));
                    instrs.push(Instr::IMov(rbx, Val::Imm(3)));
                    instrs.push(Instr::ICmovg(rax, rbx));
                },
                Op2::GreaterEqual => {
                    instrs.extend_from_slice(&checknum2);
                    instrs.push(Instr::ICmp(tmp_stack_slot, rax));
                    instrs.push(Instr::IMov(rax, Val::Imm(1)));
                    instrs.push(Instr::IMov(rbx, Val::Imm(3)));
                    instrs.push(Instr::ICmovge(rax, rbx));
                },
                Op2::Less => {
                    instrs.extend_from_slice(&checknum2);
                    instrs.push(Instr::ICmp(tmp_stack_slot, rax));
                    instrs.push(Instr::IMov(rax, Val::Imm(1)));
                    instrs.push(Instr::IMov(rbx, Val::Imm(3)));
                    instrs.push(Instr::ICmovl(rax, rbx));
                },
                Op2::LessEqual => {
                    instrs.extend_from_slice(&checknum2);
                    instrs.push(Instr::ICmp(tmp_stack_slot, rax));
                    instrs.push(Instr::IMov(rax, Val::Imm(1)));
                    instrs.push(Instr::IMov(rbx, Val::Imm(3)));
                    instrs.push(Instr::ICmovle(rax, rbx));
                },
                Op2::Equal => {
                    instrs.extend_from_slice(&checksame);
                    instrs.push(Instr::ICmp(tmp_stack_slot, rax));
                    instrs.push(Instr::IMov(rax, Val::Imm(1)));
                    instrs.push(Instr::IMov(rbx, Val::Imm(3)));
                    instrs.push(Instr::ICmove(rax, rbx));
                },
            };
        },
        Expr::If(eif, ethen, eelse) => {
            let labelelse = gen_label(ls);
            let labelend = gen_label(ls);
            h_compile_with(eif, instrs, si, env, ls, brkl);
            instrs.push(Instr::ICmp(rax, Val::Imm(1)));
            instrs.push(Instr::IJe(labelelse));
            h_compile_with(ethen, instrs, si, env, ls, brkl);
            instrs.push(Instr::IJmp(labelend));
            instrs.push(Instr::ILabel(labelelse));
            h_compile_with(eelse, instrs, si, env, ls, brkl);
            instrs.push(Instr::ILabel(labelend));
        },
        Expr::Loop(e1) => {
            let labelstart = gen_label(ls);
            let labelbreak = gen_label(ls);
            instrs.push(Instr::ILabel(labelstart));
            h_compile_with(e1, instrs, si, env, ls, Some(labelbreak));
            instrs.push(Instr::IJmp(labelstart));
            instrs.push(Instr::ILabel(labelbreak));
        },
        Expr::Break(e1) => {
            h_compile_with(e1, instrs, si, env, ls, brkl);
            if let Some(brkl) = brkl {
                instrs.push(Instr::IJmp(brkl));
            } else {
                panic!("No break target");
            }
        },
        Expr::Set(id, e1) => {
            let id_si = *env.get(id).expect(&format!("Unbound variable identifier {}", id));
            h_compile_with(e1, instrs, si, env, ls, brkl);
            instrs.push(Instr::IMov(stack_slot(id_si), rax));
        },
        Expr::Block(es) => {
            for e in es {
                h_compile_with(e, instrs, si, env, ls, brkl);
            }
        },
    }
}

fn compile(e: &Expr) -> String {
    let mut instrs = Vec::new();
    let mut ls = 0;
    h_compile_with(e, &mut instrs, 2, &HashMap::new(), &mut ls, None);
    instrs.iter().map(|&i| format!("  {}\n", &instr_to_str(i))).fold(String::new(), |acc, s| acc + &s)
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let in_contents = read_to_string(in_name).expect("couldn't read file");
    let sexpr = parse(&in_contents).expect("Invalid: couldn't parse into s-expr");
    let expr = parse_expr(&sexpr);

    let result = compile(&expr);

    let asm_program = format!(
        "section .text\n\
        global our_code_starts_here\n\
        extern snek_error\n\
        our_code_starts_here:\n\
        {}\
        ret\n\
        .E1:\n\
        mov rdi, 1\n\
        jmp .E\n\
        .E2:\n\
        mov rdi, 2\n\
        jmp .E
        .E:\n\
        push rsp\n\
        call snek_error\n\
        ret\n\
        ",
        result
    );

    write(out_name, asm_program).expect("couldn't write file");
}
