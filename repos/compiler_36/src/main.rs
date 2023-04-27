use dynasmrt::{dynasm, DynasmApi};

use std::env;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::mem;

use sexp::Atom::*;
use sexp::Sexp::*;
use sexp::*;

use cobra::BinOp::*;
use cobra::Expr::*;
use cobra::Instr::*;
use cobra::Reg::*;
use cobra::UnOp::*;
use cobra::Val::*;
use cobra::*;

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Atom(I(n)) => Number(*n),
        Atom(S(s)) if s == "true" => Boolean(true),
        Atom(S(s)) if s == "false" => Boolean(false),
        Atom(S(s)) => {
            if s != "input" && RESERVED.contains(&s.as_str()) {
                panic!("Cannot use keyword as identifier")
            } else {
                Id(s.clone())
            }
        }
        List(vec) => match vec.as_slice() {
            [Atom(S(op)), List(vec), e] if op == "let" && !vec.is_empty() => Let(
                vec.iter()
                    .map(|s| match s {
                        List(vec) => match vec.as_slice() {
                            [Atom(S(id)), e] => {
                                if RESERVED.contains(&id.as_str()) {
                                    panic!("Cannot use keyword as identifier");
                                }
                                (id.clone(), parse_expr(e))
                            }
                            _ => panic!("Invalid"),
                        },
                        _ => panic!("Invalid"),
                    })
                    .collect(),
                Box::new(parse_expr(e)),
            ),
            [Atom(S(op)), cnd, thn, els] if op == "if" => If(
                Box::new(parse_expr(cnd)),
                Box::new(parse_expr(thn)),
                Box::new(parse_expr(els)),
            ),
            [Atom(S(op)), e] if op == "loop" => Loop(Box::new(parse_expr(e))),
            [Atom(S(op)), e] if op == "break" => Break(Box::new(parse_expr(e))),
            [Atom(S(op)), es @ ..] if op == "block" && !es.is_empty() => {
                Block(es.iter().map(parse_expr).collect())
            }
            [Atom(S(op)), Atom(S(s)), e] if op == "set!" => {
                if RESERVED.contains(&s.as_str()) {
                    panic!("Cannot use keyword as identifier");
                } else {
                    Set(s.clone(), Box::new(parse_expr(e)))
                }
            }
            [Atom(S(s)), e] => {
                let op = match s.as_str() {
                    "add1" => Add1,
                    "sub1" => Sub1,
                    "isnum" => IsNum,
                    "isbool" => IsBool,
                    _ => panic!("Invalid"),
                };
                UnOp(op, Box::new(parse_expr(e)))
            }
            [Atom(S(s)), x, y] => {
                let op = match s.as_str() {
                    "+" => Plus,
                    "-" => Minus,
                    "*" => Times,
                    "=" => Equal,
                    ">" => Greater,
                    ">=" => GreaterEqual,
                    "<" => Less,
                    "<=" => LessEqual,
                    _ => panic!("Invalid"),
                };
                BinOp(op, Box::new(parse_expr(x)), Box::new(parse_expr(y)))
            }
            _ => panic!("Invalid"),
        },
        _ => panic!("Invalid"),
    }
}

//   test v, 1
//   (jz|jnz) endtype_check
//   push 1
//   call snek_error
// endtype_check:
fn type_check(
    t: Type,
    v: Val,
    MutState {
        instrs,
        label_index,
    }: &mut MutState,
) {
    let lbl = make_label("endtype_check", label_index);
    instrs.extend([
        IMov(Reg(RBX), v),
        ITest(Reg(RBX), Imm(1)),
        IJmp(
            match t {
                Type::Number => Cond::Zero,
                Type::Boolean => Cond::NotZero,
            },
            lbl.clone(),
        ),
        IMov(Reg(RDI), Imm(1)),
        IPush(Reg(RSP)),
        ICall("snek_error".to_string()),
        ILbl(lbl),
    ]);
}

// test v, 1
// mov rbx, 3
// mov rax, 1
// (cmovz|cmovnz) rax, rbx
fn type_test(t: Type, v: Val, instrs: &mut Vec<Instr>) {
    instrs.extend([
        IMov(Reg(RBX), v),
        ITest(Reg(RBX), Imm(1)),
        IMov(Reg(RBX), Imm(3)),
        IMov(Reg(RAX), Imm(1)),
        match t {
            Type::Number => ICmov(Cond::Zero, Reg(RAX), Reg(RBX)),
            Type::Boolean => ICmov(Cond::NotZero, Reg(RAX), Reg(RBX)),
        },
    ]);
}

// test v0, 1
// jz num_typecheck
// ... bool type check ...
// jmp end_same_typecheck
// num_typecheck:
// ... num type check ...
// end_same_typecheck:
fn same_type_check(v0: Val, v1: Val, mst: &mut MutState) {
    let num_lbl = make_label("num_typecheck", &mut mst.label_index);
    let end_lbl = make_label("end_same_typecheck", &mut mst.label_index);
    mst.instrs.extend([
        IMov(Reg(RBX), v0),
        ITest(Reg(RBX), Imm(1)),
        IJmp(Cond::Zero, num_lbl.clone()),
    ]);
    type_check(Type::Boolean, v1, mst);
    mst.instrs.push(IJmp(Cond::Unconditional, end_lbl.clone()));
    mst.instrs.push(ILbl(num_lbl));
    type_check(Type::Number, v1, mst);
    mst.instrs.push(ILbl(end_lbl));
}

fn overflow_check(mst: &mut MutState) {
    let lbl = make_label("endoverflow_check", &mut mst.label_index);
    mst.instrs.extend([
        IJmp(Cond::NotOverflow, lbl.clone()),
        IMov(Reg(RDI), Imm(0)),
        IPush(Reg(RSP)),
        ICall("snek_error".to_string()),
        ILbl(lbl),
    ])
}

fn compile_to_instrs(e: &Expr, imst: ImmState, mst: &mut MutState) {
    match e {
        Number(n) => mst
            .instrs
            .push(IMov(Reg(RAX), Imm(n.checked_mul(2).expect("Invalid")))),
        Boolean(true) => mst.instrs.push(IMov(Reg(RAX), Imm(3))),
        Boolean(false) => mst.instrs.push(IMov(Reg(RAX), Imm(1))),
        Id(x) if x == "input" => mst.instrs.push(IMov(Reg(RAX), Reg(RDI))),
        Id(x) => match imst.env.get(x) {
            Some(i) => {
                mst.instrs.push(IMov(Reg(RAX), RegOffset(RSP, *i)));
            }
            None => panic!("Unbound variable identifier {x}"),
        },
        Set(x, e) => match &imst.env.get(x) {
            Some(&i) => {
                compile_to_instrs(e, imst, mst);
                mst.instrs.push(IMov(RegOffset(RSP, i), Reg(RAX)));
            }
            None => panic!("Unbound variable identifier {x}"),
        },
        Block(vec) => vec
            .iter()
            .for_each(|e| compile_to_instrs(e, imst.clone(), mst)),
        UnOp(op, e) => {
            compile_to_instrs(e, imst, mst);
            match op {
                Add1 => {
                    type_check(Type::Number, Reg(RAX), mst);
                    mst.instrs.push(IAdd(Reg(RAX), Imm(1 * 2)));
                    overflow_check(mst);
                }
                Sub1 => {
                    type_check(Type::Number, Reg(RAX), mst);
                    mst.instrs.push(ISub(Reg(RAX), Imm(1 * 2)));
                    overflow_check(mst);
                }
                IsNum => type_test(Type::Number, Reg(RAX), &mut mst.instrs),
                IsBool => type_test(Type::Boolean, Reg(RAX), &mut mst.instrs),
            }
        }
        BinOp(op, x, y) => {
            compile_to_instrs(x, imst.clone(), mst);
            mst.instrs
                .push(IMov(RegOffset(RSP, imst.stack_index), Reg(RAX)));
            compile_to_instrs(
                y,
                ImmState {
                    stack_index: imst.stack_index + 1,
                    ..imst.clone()
                },
                mst,
            );
            mst.instrs
                .push(IMov(RegOffset(RSP, imst.stack_index + 1), Reg(RAX)));
            mst.instrs
                .push(IMov(Reg(RAX), RegOffset(RSP, imst.stack_index)));
            match op {
                Plus => {
                    type_check(Type::Number, Reg(RAX), mst);
                    type_check(Type::Number, RegOffset(RSP, imst.stack_index + 1), mst);
                    mst.instrs
                        .push(IAdd(Reg(RAX), RegOffset(RSP, imst.stack_index + 1)));
                    overflow_check(mst);
                }
                Minus => {
                    type_check(Type::Number, Reg(RAX), mst);
                    type_check(Type::Number, RegOffset(RSP, imst.stack_index + 1), mst);
                    mst.instrs
                        .push(ISub(Reg(RAX), RegOffset(RSP, imst.stack_index + 1)));
                    overflow_check(mst);
                }
                Times => {
                    type_check(Type::Number, Reg(RAX), mst);
                    type_check(Type::Number, RegOffset(RSP, imst.stack_index + 1), mst);
                    mst.instrs.extend([
                        ISar(Reg(RAX), Imm(1)), // right shift once before multiplying
                        IMul(Reg(RAX), RegOffset(RSP, imst.stack_index + 1)),
                    ]);
                    overflow_check(mst);
                }
                Equal => {
                    same_type_check(Reg(RAX), RegOffset(RSP, imst.stack_index + 1), mst);
                    mst.instrs.extend([
                        ICmp(Reg(RAX), RegOffset(RSP, imst.stack_index + 1)),
                        IMov(Reg(RBX), Imm(3)),
                        IMov(Reg(RAX), Imm(1)),
                        ICmov(Cond::Equal, Reg(RAX), Reg(RBX)),
                    ]);
                }
                Greater => {
                    type_check(Type::Number, Reg(RAX), mst);
                    type_check(Type::Number, RegOffset(RSP, imst.stack_index + 1), mst);
                    mst.instrs.extend([
                        ICmp(Reg(RAX), RegOffset(RSP, imst.stack_index + 1)),
                        IMov(Reg(RBX), Imm(3)),
                        IMov(Reg(RAX), Imm(1)),
                        ICmov(Cond::Greater, Reg(RAX), Reg(RBX)),
                    ]);
                }
                GreaterEqual => {
                    type_check(Type::Number, Reg(RAX), mst);
                    type_check(Type::Number, RegOffset(RSP, imst.stack_index + 1), mst);
                    mst.instrs.extend([
                        ICmp(Reg(RAX), RegOffset(RSP, imst.stack_index + 1)),
                        IMov(Reg(RBX), Imm(3)),
                        IMov(Reg(RAX), Imm(1)),
                        ICmov(Cond::GreaterEqual, Reg(RAX), Reg(RBX)),
                    ]);
                }
                Less => {
                    type_check(Type::Number, Reg(RAX), mst);
                    type_check(Type::Number, RegOffset(RSP, imst.stack_index + 1), mst);
                    mst.instrs.extend([
                        ICmp(Reg(RAX), RegOffset(RSP, imst.stack_index + 1)),
                        IMov(Reg(RBX), Imm(3)),
                        IMov(Reg(RAX), Imm(1)),
                        ICmov(Cond::Less, Reg(RAX), Reg(RBX)),
                    ]);
                }
                LessEqual => {
                    type_check(Type::Number, Reg(RAX), mst);
                    type_check(Type::Number, RegOffset(RSP, imst.stack_index + 1), mst);
                    mst.instrs.extend([
                        ICmp(Reg(RAX), RegOffset(RSP, imst.stack_index + 1)),
                        IMov(Reg(RBX), Imm(3)),
                        IMov(Reg(RAX), Imm(1)),
                        ICmov(Cond::LessEqual, Reg(RAX), Reg(RBX)),
                    ]);
                }
            }
        }
        Let(binds, e) => {
            if binds
                .iter()
                .map(|(s, _)| s.clone())
                .collect::<im::HashSet<String>>()
                .len()
                != binds.len()
            {
                panic!("Duplicate binding")
            }

            let nenv = binds
                .iter()
                .enumerate()
                .fold(imst.env.clone(), |env, (n, (id, e))| {
                    let stack_index = imst.stack_index + n as i32;
                    compile_to_instrs(
                        e,
                        ImmState {
                            env: env.clone(),
                            stack_index,
                            ..imst.clone()
                        },
                        mst,
                    );
                    mst.instrs.push(IMov(RegOffset(RSP, stack_index), Reg(RAX)));
                    env.update(id.clone(), stack_index)
                });

            compile_to_instrs(
                e,
                ImmState {
                    env: nenv,
                    stack_index: imst.stack_index + binds.len() as i32,
                    ..imst
                },
                mst,
            );
        }
        If(cnd, thn, els) => {
            let else_lbl = make_label("else", &mut mst.label_index);
            let end_lbl = make_label("endif", &mut mst.label_index);

            compile_to_instrs(cnd, imst.clone(), mst);
            mst.instrs.push(ICmp(Reg(RAX), Imm(1))); // only check for false
            mst.instrs.push(IJmp(Cond::Equal, else_lbl.clone()));
            compile_to_instrs(thn, imst.clone(), mst);
            mst.instrs.push(IJmp(Cond::Unconditional, end_lbl.clone())); // jump to end after then
            mst.instrs.push(ILbl(else_lbl));
            compile_to_instrs(els, imst, mst);
            mst.instrs.push(ILbl(end_lbl));
        }
        Loop(e) => {
            let start_lbl = make_label("startloop", &mut mst.label_index);
            let end_lbl = make_label("endloop", &mut mst.label_index);

            mst.instrs.push(ILbl(start_lbl.clone())); // startloop:
            compile_to_instrs(
                e,
                ImmState {
                    break_label: end_lbl.clone(),
                    ..imst
                },
                mst,
            );
            mst.instrs.push(IJmp(Cond::Unconditional, start_lbl)); // jmp startloop
            mst.instrs.push(ILbl(end_lbl)); // endloop:
        }
        Break(e) => {
            if imst.break_label.is_empty() {
                panic!("break outside of loop");
            }
            compile_to_instrs(e, imst.clone(), mst);
            mst.instrs.push(IJmp(Cond::Unconditional, imst.break_label));
        }
    }
}

fn compile(e: &Expr) -> String {
    let mut mst = MutState {
        instrs: Vec::new(),
        label_index: 0,
    };
    compile_to_instrs(
        e,
        ImmState {
            env: im::HashMap::new(),
            stack_index: 2,
            break_label: "".to_string(),
        },
        &mut mst,
    );
    mst.instrs
        .iter()
        .map(instr_to_str)
        .reduce(|a, b| a + "\n" + &b)
        .unwrap_or(String::new())
}

fn jit_compile(e: &Expr, ops: &mut dynasmrt::x64::Assembler) {
    let mut mst = MutState {
        instrs: Vec::new(),
        label_index: 0,
    };
    compile_to_instrs(
        e,
        ImmState {
            env: im::HashMap::new(),
            stack_index: 2,
            break_label: "".to_string(),
        },
        &mut mst,
    );

    let lbls: im::HashMap<_, _> = mst
        .instrs
        .iter()
        .filter_map(|i| match i {
            ILbl(l) => Some((l, ops.new_dynamic_label())),
            _ => None,
        })
        .collect();
    mst.instrs.iter().for_each(|i| instr_to_asm(i, &lbls, ops));
}

fn repl() -> std::io::Result<()> {
    fn eval(e: &Sexp) -> u64 {
        let mut ops = dynasmrt::x64::Assembler::new().unwrap();
        let start = ops.offset();
        jit_compile(&parse_expr(&e), &mut ops);
        dynasm!(ops ; .arch x64 ; ret);
        let buf = ops.finalize().unwrap();
        let jitted_fn: extern "C" fn() -> u64 = unsafe { mem::transmute(buf.ptr(start)) };
        jitted_fn()
    }

    loop {
        print!("> ");
        io::stdout().flush()?;
        let mut s = String::new();
        io::stdin().read_line(&mut s)?;
        let form = parse(&s).expect("Invalid");

        print_value(eval(&form));
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    if in_name == "-i" {
        return repl();
    }

    let out_name = &args[2];

    let result = compile(&parse_expr(
        &parse(&std::fs::read_to_string(in_name)?).expect("Invalid"),
    ));

    let asm_program = format!(
        "
section .text
extern snek_error
global our_code_starts_here
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
