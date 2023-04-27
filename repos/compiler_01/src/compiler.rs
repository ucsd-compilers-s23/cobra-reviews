use im::HashMap;
use std::collections::HashSet;
use crate::parser::Expr;
use crate::parser::Op1;
use crate::parser::Op2;
use crate::error;

const WORD_SIZE: i32 = 8;
pub const ERROR_LABEL: &str = "throw_error";

/*
    NOTE:
    In order to concatenate a vector with a value or another vector of the same 
    type while at the same time returning the value, the following idiom is used

    [
        vec1,
        vec2,
    ].concat()
 */

// Abstract Syntax for X86_64 values
#[derive(Debug, Clone)]
pub enum Val {
    Reg(Reg),
    Imm(i64),
    RegOffset(Reg, i32),
}

// Abstract Syntax for X86_64 registers
#[derive(Debug, Clone)]
pub enum Reg {
    RAX, // return value
    RSP, // stack pointer
    RDI, // "input" keyword

    RBX, // scratch work register
}
use Reg::*; // So I dont have to do Reg::RAX, Reg::RBX... every time

/// Abstract Syntax for X86_64 assembly instructions
#[derive(Debug, Clone)]
pub enum Instr {
    // Moves
    IMov(Val, Val),
    ICMove(Val, Val),
    ICMoveLess(Val, Val),
    ICMoveLessEq(Val, Val),
    ICMoveGreater(Val, Val),
    ICMoveGreaterEq(Val, Val),

    // Arithmetic
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),

    // Bitwise operations
    IAnd(Val, Val),
    IXor(Val, Val),
    IOr(Val, Val),
    IArithShiftRight(i32, Val),

    // Comparisons
    ICmp(Val, Val),
    ITest(Val, Val),

    // Jumps
    IJmp(String),
    IJmpEq(String),
    IJmpNotEq(String),
    IJmpOverflow(String),
    ILabel(String),
}

// Assembly snippets that are used throughout the compiler
impl Instr {
    /// Throws a dynamic error if the values stored in reg1 and reg2 are not
    /// either both bools or both numbers
    /// - Preserves reg1 and reg2
    /// - Mangles RBX
    fn test_same_type(reg1: Val, reg2: Val) -> Vec<Instr> {
        vec![
            Instr::IMov(Val::Reg(RBX), reg1),
            Instr::IXor(Val::Reg(RBX), reg2),
            Instr::ITest(Val::Reg(RBX), Val::Imm(1)),
            Instr::IMov(Val::Reg(RBX), Val::Imm(error::INVALID_ARGUMENT)),
            Instr::IJmpNotEq(ERROR_LABEL.to_string()),
        ]
    }

    /// Throws a dynamic error if the value stored in reg is not a number
    /// - Preserves reg
    /// - Mangles RBX
    fn test_num(reg: Val) -> Vec<Instr> {
        vec![
            Instr::IMov(Val::Reg(RBX), reg),
            Instr::IAnd(Val::Reg(RBX), Val::Imm(1)),
            Instr::ICmp(Val::Reg(RBX), Val::Imm(1)),
            Instr::IMov(Val::Reg(RBX), Val::Imm(error::INVALID_ARGUMENT)),
            Instr::IJmpEq(ERROR_LABEL.to_string()),
        ]
    }

    /// - Preserves reg1 and reg2
    /// - Mangles RBX
    /// - Dynamic error if not both nums
    fn test_both_nums(reg1: Val, reg2: Val) -> Vec<Instr> {
        vec![
            Instr::IMov(Val::Reg(RBX), reg1),
            Instr::IOr(Val::Reg(RBX), reg2),
            Instr::IAnd(Val::Reg(RBX), Val::Imm(1)),
            Instr::ICmp(Val::Reg(RBX), Val::Imm(1)),
            Instr::IMov(Val::Reg(RBX), Val::Imm(error::INVALID_ARGUMENT)),
            Instr::IJmpEq(ERROR_LABEL.to_string()),
        ]
    }

    /// - Dynamic error if overflow bit is set
    /// - Mangles RBX
    fn test_overflow() -> Vec<Instr> {
        vec![
            Instr::IMov(Val::Reg(RBX), Val::Imm(error::OVERFLOW)),
            Instr::IJmpOverflow(ERROR_LABEL.to_string()),
        ]
    }

    /// All of the different comparison operators can be reduced down to the exact same
    /// assembly, where you just have to change the type of conditional move used,
    /// so this code snippet handles all of that logic.
    /// - op should be one of = < <= > >=
    /// - Mangles RBX
    /// 
    /// # Panics
    /// - If Op2 is + - *. This is an error with the compiler itself, and should never
    /// be experienced by someone compiling their own program.
    fn eval_comparison(op: &Op2, reg_left: Val, reg_right: Val) -> Vec<Instr> {
        vec![
            Instr::ICmp(reg_left, reg_right),
            Instr::IMov(Val::Reg(RBX), Val::Imm(to_bool63(true))),
            Instr::IMov(Val::Reg(RAX), Val::Imm(to_bool63(false))),
            match op {
                Op2::Equal => Instr::ICMove,
                Op2::Less => Instr::ICMoveLess,
                Op2::LessEqual => Instr::ICMoveLessEq,
                Op2::Greater => Instr::ICMoveGreater,
                Op2::GreaterEqual => Instr::ICMoveGreaterEq,
                Op2::Plus | Op2::Times | Op2::Minus => 
                    panic!("INTERNAL COMPILER ERROR: Invalid Op2 passed to eval_comparison"),
            }(Val::Reg(RAX), Val::Reg(RBX)),
        ]
    }
}

type Env = HashMap<String, i32>;

/// Struct to handle all of the parameters that are passed throughout the many
/// recursive calls to `compile_to_instrs()`
struct CompileArgs<'a> {
    si: i32,
    env: &'a Env,
    break_target: Option<String>,
}

impl<'a> CompileArgs<'a> {
    /// Make new `CompileArgs` from scratch with all arguments explicitly specified
    fn new(si: i32, env: &'a Env, break_target: Option<String>) -> Self {
        CompileArgs {
            si,
            env,
            break_target,
        }
    }

    /// Take current args and only increase `si` by `amt`, keeping everything
    /// else constant
    fn incr_si(&self, amt: i32) -> Self {
        CompileArgs { 
            si: self.si + amt, 
            env: self.env, 
            break_target: self.break_target.clone(), 
        } 
    }

    /// Replace old `si` with `new_si` and old `env` with `nenv`, keeping
    /// everything else constant
    fn new_si_and_nenv(&self, new_si: i32, nenv: &'a Env) -> Self {
        CompileArgs { 
            si: new_si, 
            env: nenv,
            break_target: self.break_target.clone(), 
        } 
    }

    /// Replace old `break_target` with `target`, keeping everything else constant
    fn new_break_target(&self, target: String) -> Self {
        CompileArgs {
            si: self.si,
            env: self.env,
            break_target: Some(target),
        }
    }
}

/// High level compile function that takes an `Expr` instance, which is an AST
/// representation of the entire program, and outputs the resulting assembly
/// program as a String.
pub fn compile(program: &Expr) -> String {
    let program_str = 
        compile_to_instrs(program, &CompileArgs::new(2, &HashMap::new(), None))
        .iter()
        .fold(String::new(),
              |accum, instr| accum + "    " + &instr_to_str(instr) + "\n");

    // Should probably change this to be in the abstract syntax formatting but I don't 
    // want to accidentally break anything at this moment
    format!(
"
section .text
global our_code_starts_here
extern snek_error
{}:
    mov rdi, rbx
    push rsp
    and rsp, -16
    call snek_error
our_code_starts_here:
{}
    ret
",
        ERROR_LABEL,
        program_str
    )
}

/// High level call which recursively compiles all of the subexpressions
/// in `e`, with the specified `args`.
fn compile_to_instrs(e: &Expr, args: &CompileArgs) -> Vec<Instr> {
    match e {
        Expr::Number(n) 
            => vec![Instr::IMov(Val::Reg(RAX), Val::Imm(to_num63(*n)))],
        Expr::Boolean(b)
            => vec![Instr::IMov(Val::Reg(RAX), Val::Imm(to_bool63(*b)))],
        Expr::Id(name) 
            => compile_id(name, args),
        Expr::UnOp(unop, e)
            => compile_unop(unop, e, args),
        Expr::BinOp(binop, e1, e2)
            => compile_binop(binop, e1, e2, args),
        Expr::Let(binds, body)
            => compile_let(binds, body, args),
        Expr::If(e1, e2, e3)
            => compile_if(e1, e2, e3, args),
        Expr::Loop(e)
            => compile_loop(e, args),
        Expr::Break(e)
            => compile_break(e, args),
        Expr::Set(name, e)
            => compile_set(name, e, args),
        Expr::Block(exps)
            => compile_block(exps, args),
    } 
}

/// Helper function which panics if binding list has multiple vars with the same name
/// # Panics
/// - If in `binds` there is more than one item with same String value.
fn check_duplicate_binds(binds: &Vec<(String, Expr)>) {
    let mut set = HashSet::new();

    for (name, _) in binds {
        if set.contains(name) {
            panic!("Duplicate binding");
        }

        set.insert(name);
    }
}

/// Applies the bindings to the env and compiles `body`
fn compile_let(binds: &Vec<(String, Expr)>, body: &Expr, args: &CompileArgs) -> Vec<Instr> {
    check_duplicate_binds(binds);

    let mut nenv = args.env.clone(); 
    let mut curr_si = args.si;
    [
        binds
            .iter()
            // (name: String, expr: Expr) -> (offset: i32, instrs: Vec<Instr>)
            // go from bindings to instructions and the stack offset on where to 
            // store result of said instructions
            .map(|(name, expr)| {
                let curr_instrs = compile_to_instrs(expr, &args.new_si_and_nenv(curr_si, &nenv));
                let curr_offset = curr_si * WORD_SIZE;
                nenv = nenv.update(name.to_string(), curr_offset);
                curr_si += 1;
                (curr_offset, curr_instrs)
            })
            // (offset: i32, instrs: Vec<Instr>) -> Vec<Instr>
            // Combine instrs for expr with instruction to store result in designated
            // stack offset spot
            .map(|(offset, instrs)| {
                [
                    &instrs[..],
                    &[Instr::IMov(Val::RegOffset(RSP, offset), Val::Reg(RAX))]
                ].concat()
            })
            // turn iterator of Vec<Instr> into one long vector, essentially concatenating
            // all of the separate Vec<Instr> we made from the previous map
            .fold(Vec::new(), |accum, instrs| {
                [
                    &accum[..],
                    &instrs,
                ].concat()
            }),
        // Now that we have all the assembly instructions to calculate and store the bindings
        // on the stack, we can compile the instructions for the body expression while taking
        // into account the new environment and stack index.
        compile_to_instrs(body, &args.new_si_and_nenv(curr_si, &nenv))
    ].concat()
}

/// Compile assembly instructions for the operation `binop` `e1` `e2`
fn compile_binop(binop: &Op2, e1: &Expr, e2: &Expr, args: &CompileArgs) -> Vec<Instr> {
    // closure to manufacture the offset_reg as many times as we need without repeated boilerplate
    let offset_reg = || Val::RegOffset(RSP, args.si * WORD_SIZE);
    [
        // Compute first instr and store in RAX
        &compile_to_instrs(e1, args)[..], 
        // Move RAX to [RSP - stack_offset]
        &[Instr::IMov(offset_reg(), Val::Reg(RAX))],
        // Computer second instr and store in RAX, making sure to increase stack index
        &compile_to_instrs(e2, &args.incr_si(1))[..], 
        // Do specific operation with [RSP - stack_offset] and RAX
        &match binop {
            Op2::Plus => [
                Instr::test_both_nums(Val::Reg(RAX), offset_reg()),
                vec![
                    Instr::IAdd(Val::Reg(RAX), offset_reg()),
                ],
                Instr::test_overflow(),
            ].concat(),
            Op2::Minus => [
                Instr::test_both_nums(Val::Reg(RAX), offset_reg()),
                vec![
                    // Ensure that ordering is correct for subtraction
                    // offset_reg() is a temporary value, so it's okay
                    // to mangle it
                    Instr::ISub(offset_reg(), Val::Reg(RAX)),
                    Instr::IMov(Val::Reg(RAX), offset_reg()),
                ],
                Instr::test_overflow(),
            ].concat(),
            Op2::Times => [
                Instr::test_both_nums(Val::Reg(RAX), offset_reg()),
                vec![
                    // Because numbers are left shifted by 1, need to invert
                    // this for one of the operand registers to make sure the
                    // answer is also only left shifted by 1
                    Instr::IArithShiftRight(1, Val::Reg(RAX)),
                    Instr::IMul(Val::Reg(RAX), offset_reg())
                ],
                Instr::test_overflow(),
            ].concat(),
            Op2::Equal => [
                Instr::test_same_type(Val::Reg(RAX), offset_reg()),
                Instr::eval_comparison(&Op2::Equal, offset_reg(), Val::Reg(RAX)),
            ].concat(),
            Op2::Greater | Op2::GreaterEqual |
            Op2::Less    | Op2::LessEqual      => [
                Instr::test_both_nums(Val::Reg(RAX), offset_reg()),
                Instr::eval_comparison(binop, offset_reg(), Val::Reg(RAX)),
            ].concat()
        },
    ].concat()
}

/// Compile assembly instructions for operation `unop` `e`
fn compile_unop(unop: &Op1, e: &Expr, args: &CompileArgs) -> Vec<Instr> {
    [
        // Compile e and store in RAX
        &compile_to_instrs(e, args)[..], 
        // Do operation on RAX
        &match unop {
            Op1::Add1 => [
                Instr::test_num(Val::Reg(RAX)),
                vec![Instr::IAdd(Val::Reg(RAX), Val::Imm(to_num63(1)))],
                Instr::test_overflow(),
            ].concat(),
            Op1::Sub1 => [
                Instr::test_num(Val::Reg(RAX)),
                vec![Instr::ISub(Val::Reg(RAX), Val::Imm(to_num63(1)))],
                Instr::test_overflow(),
            ].concat(),
            Op1::IsBool => vec![
                // Check if rightmost bit is 1 (bool)
                Instr::IAnd(Val::Reg(RAX), Val::Imm(1)),
                Instr::ICmp(Val::Reg(RAX), Val::Imm(1)),
                Instr::IMov(Val::Reg(RBX), Val::Imm(to_bool63(true))),
                Instr::IMov(Val::Reg(RAX), Val::Imm(to_bool63(false))),
                Instr::ICMove(Val::Reg(RAX), Val::Reg(RBX)),
            ],
            Op1::IsNum => vec![
                // Check if rightmost bit is 0 (num)
                Instr::IAnd(Val::Reg(RAX), Val::Imm(1)),
                Instr::ICmp(Val::Reg(RAX), Val::Imm(0)),
                Instr::IMov(Val::Reg(RBX), Val::Imm(to_bool63(true))),
                Instr::IMov(Val::Reg(RAX), Val::Imm(to_bool63(false))),
                Instr::ICMove(Val::Reg(RAX), Val::Reg(RBX)),
            ],
        },
    ].concat()
}

/// Compile instructions to get value stored in identifier `name`. Does this
/// by searching in the environment for arbitary variables, or going to RDI
/// if the identifier refers to the program input.
/// 
/// # Panics
/// - If `name` is not a key in `args.env`
fn compile_id(name: &str, args: &CompileArgs) -> Vec<Instr> {
    vec![
        Instr::IMov(
            Val::Reg(RAX), 
            if name == "input" {
                Val::Reg(RDI)
            } else {
                Val::RegOffset(
                    RSP,
                    *args.env.get(name).expect(&format!("Unbound variable identifier {name}"))
                )
            }
        )
    ]
}

/// Compile instructions for if statement. If `cond_e` is not the 63 bit false value, run
/// `then_e`. Otherwise, run `else_e`.
fn compile_if(cond_e: &Expr, then_e: &Expr, else_e: &Expr, args: &CompileArgs) -> Vec<Instr> {
    let end_label = unsafe {new_label("ifend")};
    let else_label = unsafe {new_label("ifelse")};

    [
        compile_to_instrs(cond_e, args),
        vec![
            Instr::ICmp(Val::Reg(RAX), Val::Imm(to_bool63(false))),
            Instr::IJmpEq(else_label.clone()),
        ],
        compile_to_instrs(then_e, args),
        vec![
            Instr::IJmp(end_label.clone()),
            Instr::ILabel(else_label),
        ],
        compile_to_instrs(else_e, args),
        vec![
            Instr::ILabel(end_label)
        ],
    ].concat()
}

/// Compile instructions for loop on expression `e`. Runs forever unless a break
/// is called from within.
fn compile_loop(e: &Expr, args: &CompileArgs) -> Vec<Instr> {
    let loop_start = unsafe {new_label("loopstart")};
    let loop_end = unsafe {new_label("loopend")};

    [
        vec![
            Instr::ILabel(loop_start.clone())
        ],
        // Set break target for any nested break statements
        compile_to_instrs(e, &args.new_break_target(loop_end.clone())),
        vec![
            Instr::IJmp(loop_start),
            Instr::ILabel(loop_end),
        ],
    ].concat()
}

/// Compile instructions for break statement. Essentially jumps to the current
/// `break_target` defined in `args.env` and sets output of `e` in RAX.
/// 
/// # Panics
/// - If `args.break_target` is None
fn compile_break(e: &Expr, args: &CompileArgs) -> Vec<Instr> {
    match &args.break_target {
        None => panic!("break statement found outside of a loop"),
        Some(target) => [
            compile_to_instrs(e, args),
            vec![Instr::IJmp(target.to_string())],
        ].concat()
    }
}

/// Compile instructions for block statement. Essentially runs all of the given
/// expressions in order, leaving the last statement's RAX return value being
/// the return value for the entire expression.
fn compile_block(exprs: &Vec<Expr>, args: &CompileArgs) -> Vec<Instr> {
    exprs
        .into_iter()
        .map(|expr| compile_to_instrs(expr, args))
        .flatten()
        .collect()
}

/// Compile instructions for set! statement. Looks in `args.env` for a variable
/// with name `name` and sets value of `e` to it.
/// 
/// # Panics
/// - If `name` is not currently a variable in `args.env`
fn compile_set(name: &str, e: &Expr, args: &CompileArgs) -> Vec<Instr> {
    match args.env.get(name) {
        None => panic!("Unbound variable identifier {name}"),
        Some(offset) => [
            compile_to_instrs(e, args),
            vec![
                Instr::IMov(Val::RegOffset(RSP, *offset), Val::Reg(RAX)),
            ],
        ].concat(),
    }
}

/// Convert abstract assembly instruction into concrete X86_64 String representation
fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(dest, src) => format!("mov {}, {}", val_to_str(dest), val_to_str(src)),
        Instr::IAdd(dest, src) => format!("add {}, {}", val_to_str(dest), val_to_str(src)),
        Instr::ISub(dest, src) => format!("sub {}, {}", val_to_str(dest), val_to_str(src)),
        Instr::IMul(dest, src) => format!("imul {}, {}", val_to_str(dest), val_to_str(src)),
        Instr::ICmp(left, right) => format!("cmp {}, {}", val_to_str(left), val_to_str(right)),
        Instr::IJmp(label) => format!("jmp {label}"),
        Instr::IJmpEq(label) => format!("je {label}"),
        Instr::IJmpNotEq(label) => format!("jne {label}"),
        Instr::IAnd(dest, src) => format!("and {}, {}", val_to_str(dest), val_to_str(src)),
        Instr::ILabel(label) => format!("{label}:"),
        Instr::ICMove(dest, src) => format!("cmove {}, {}", val_to_str(dest), val_to_str(src)),
        Instr::ITest(dest, src) => format!("test {}, {}", val_to_str(dest), val_to_str(src)),
        Instr::IXor(dest, src) => format!("xor {}, {}", val_to_str(dest), val_to_str(src)),
        Instr::IOr(dest, src) => format!("or {}, {}", val_to_str(dest), val_to_str(src)),
        Instr::ICMoveLess(dest, src) => format!("cmovl {}, {}", val_to_str(dest), val_to_str(src)),
        Instr::ICMoveLessEq(dest, src) => format!("cmovle {}, {}", val_to_str(dest), val_to_str(src)),
        Instr::ICMoveGreater(dest, src) => format!("cmovg {}, {}", val_to_str(dest), val_to_str(src)),
        Instr::ICMoveGreaterEq(dest, src) => format!("cmovge {}, {}", val_to_str(dest), val_to_str(src)),
        Instr::IArithShiftRight(count, dest) => format!("sar {}, {}", val_to_str(dest), count),
        Instr::IJmpOverflow(label) => format!("jo {label}"),
    }
}

/// Converts abstract assembly value to X86_64 String representation
fn val_to_str(v: &Val) -> String {
    match v {
        Val::Reg(reg) => reg_to_str(reg),
        Val::RegOffset(reg, n) => format!("[{} - {n}]", reg_to_str(reg)),
        Val::Imm(n) => n.to_string(),
    }
}

/// Converts abstract assembly register to X86_64 String representation
fn reg_to_str(reg: &Reg) -> String {
    match reg {
        RAX => "rax".to_string(),
        RSP => "rsp".to_string(),
        RDI => "rdi".to_string(),
        RBX => "rbx".to_string(),
    }
}

/// Converts number into assembly 63 bit format, where rightmost bit is type bit.
/// Numbers have a type bit of 0.
fn to_num63(n: i64) -> i64 {
    n << 1
}

/// Converts bool into assembly 63 bit format, where rightmost bit is type bit.
/// Bools have a type bit of 1, so 0b1 is false and 0b11 is true.
fn to_bool63(b: bool) -> i64 {
    match b {
        true => 3,  // 0b11
        false => 1, // 0b01
    }
}

/// Manufacture a unique label for use in the final assembly program. Note that this 
/// function is unsafe because it uses a static mutable variable. This could be 
/// avoided by having another parameter passed throughout the program which maintains
/// the current label count, but when I tried to merge all of the compiler arguments
/// into the CompileArgs struct I ran into some lifetime errors and could not
/// figure out how to resolve them. Instead of taking this parameter outside of the
/// CompileArgs struct and passing it alongside it, I chose to just use this "unsafe"
/// approach which I think should be basically the same thing but cleaner because everything
/// is run in a single thread of execution and I dont have to pass an extra &mut label 
/// parameter every time I recursively call a compile function.
unsafe fn new_label(s: &str) -> String {
    static mut CURR_LABEL_NUM: u64 = 0;
    CURR_LABEL_NUM += 1;
    format!("{s}{CURR_LABEL_NUM}")
}