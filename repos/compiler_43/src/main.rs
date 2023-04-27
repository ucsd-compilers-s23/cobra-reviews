use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::Atom::*;
use sexp::*;

use im::HashMap;
use im::hashmap;

use im::HashSet; // Exclusively for use in duplicate binding check
use im::hashset;

mod util;
use util::encode_bool as encode_bool;
use util::encode_num as encode_num;
use util::new_label as new_label;

/*
<expr> :=
  | <number>
  | true
  | false
  | input
  | <identifier>
  | (let (<binding>+) <expr>)
  | (<op1> <expr>)
  | (<op2> <expr> <expr>)
  | (set! <name> <expr>)
  | (if <expr> <expr> <expr>)
  | (block <expr>+)
  | (loop <expr>)
  | (break <expr>)

<op1> := add1 | sub1 | isnum | isbool
<op2> := + | - | * | < | > | >= | <= | =

<binding> := (<identifier> <expr>)
*/

enum Op1 {
    Add1,
    Sub1,
    IsNum,
    IsBool,
}

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

// Enumerate all the dynamic error codes
const INVALID_ARG_ERR_CODE: i64 = 1;
const OVERFLOW_ERR_CODE: i64 = 2;

// Constant bounds for 63-bit ints in language
const INT_MAX: i64 = 4611686018427387903;
const INT_MIN: i64 = -4611686018427387904;

const START_STACK_INDEX: i64 = 2;

fn compile_expr(
    exp: &Expr,
    si: i64,
    env: &HashMap<String, i64>,
    lbl_num: &mut i64,
    bk_tgt: Option<&String>
) -> String {
    match exp {
        // Compile i64 number
        Expr::Number(n) => format!("mov rax, {}", encode_num(*n)),

        // Compilete boolean ("true", "false")
        Expr::Boolean(b) => format!("mov rax, {}", encode_bool(*b)),

        // Compile id using env
        Expr::Id(id) => {
            // TODO: Ensure input is never used as an id
            if id == "input" {
                format!("mov rax, rdi")
            } else if
                // Check that id is bound in env
                env.contains_key(id)
            {
                format!("mov rax, [rsp - {}]", env.get(id).unwrap())
            } else {
                // Handle unbound variable identifier
                panic!("Unbound variable identifier {}", id)
            }
        }

        // Compile unary operators
        Expr::UnOp(unop, exp) => {
            match unop {
                Op1::Add1 => {
                    format!(
                        "
    {}

    mov rbx, rax
    and rbx, 1
    cmp rbx, 1
    je throw_invalid_arg_err

    add rax, {}

    jo throw_overflow_err
    ",
                        compile_expr(exp, si, env, lbl_num, bk_tgt),
                        encode_num(1)
                    )
                }

                Op1::Sub1 => {
                    format!(
                        "
    {}
    
    mov rbx, rax
    and rbx, 1
    cmp rbx, 1
    je throw_invalid_arg_err
    
    sub rax, {}

    jo throw_overflow_err
    ",
                        compile_expr(exp, si, env, lbl_num, bk_tgt),
                        encode_num(1)
                    )
                }

                Op1::IsNum => {
                    let exp_instrs = compile_expr(exp, si, env, lbl_num, bk_tgt);

                    /*
                    Extract least significant bit of rax using bitwise and.
                    Compare that value to 1 to determine if the value
                    is a number (LSb = 0) or a boolean (LSb = 1). 

                    Move "false" into rbx, and move "true" into rax. If last cmp was true, i.e. the LSb is 1,  move rbx ("false") into rax. Since LSb should be 0 for all #s.
                    */
                    format!(
                        "
    {}
    and rax, 1
    cmp rax, 1
    mov rbx, {}
    mov rax, {}
    cmove rax, rbx
    ",
                        exp_instrs,
                        encode_bool(false),
                        encode_bool(true)
                    )
                }

                Op1::IsBool => {
                    let exp_instrs = compile_expr(exp, si, env, lbl_num, bk_tgt);

                    /*
                    Extract least significant bit of rax using bitwise and.
                    Compare that value to 1 to determine if the value
                    is a number (LSb = 0) or a boolean (LSb = 1). 

                    Move "true" into rbx, and move "false" into rax. If last cmp was true, i.e. the LSb is 1,  move rbx ("true") into rax
                    */
                    format!(
                        "
    {}
    and rax, 1
    cmp rax, 1
    mov rbx, {}
    mov rax, {}
    cmove rax, rbx
    ",
                        exp_instrs,
                        encode_bool(true),
                        encode_bool(false)
                    )
                }
            }
        }

        // Compile binary operators
        Expr::BinOp(binop, exp1, exp2) => {
            match binop {
                Op2::Plus => {
                    let exp1_instrs = compile_expr(exp1, si, env, lbl_num, bk_tgt);
                    let exp2_instrs = compile_expr(exp2, si + 1, env, lbl_num, bk_tgt);

                    let stk_offs = si * 8;

                    format!(
                        "
    {}

    mov rbx, rax
    and rbx, 1
    cmp rbx, 1
    je throw_invalid_arg_err

    mov [rsp - {}], rax
    
    {}

    mov rbx, rax
    and rbx, 1
    cmp rbx, 1
    je throw_invalid_arg_err

    add rax, [rsp - {}]
    
    jo throw_overflow_err",
                        exp1_instrs,
                        stk_offs,
                        exp2_instrs,
                        stk_offs
                    )
                }

                Op2::Minus => {
                    let exp1_instrs = compile_expr(exp1, si, env, lbl_num, bk_tgt);
                    let exp2_instrs = compile_expr(exp2, si + 1, env, lbl_num, bk_tgt);

                    let stk_offs = si * 8;

                    format!(
                        "
    {}

    mov rbx, rax
    and rbx, 1
    cmp rbx, 1
    je throw_invalid_arg_err

    mov [rsp - {}], rax

    {}

    mov rbx, rax
    and rbx, 1
    cmp rbx, 1
    je throw_invalid_arg_err

    sub [rsp - {}], rax
    mov rax, [rsp - {}]
    
    jo throw_overflow_err",
                        exp1_instrs,
                        stk_offs,
                        exp2_instrs,
                        stk_offs,
                        stk_offs
                    )
                }

                Op2::Times => {
                    let exp1_instrs = compile_expr(exp1, si, env, lbl_num, bk_tgt);
                    let exp2_instrs = compile_expr(exp2, si + 1, env, lbl_num, bk_tgt);

                    let stk_offs = si * 8;

                    format!(
                        "
    {}

    mov rbx, rax
    and rbx, 1
    cmp rbx, 1
    je throw_invalid_arg_err

    mov [rsp - {}], rax

    {}

    mov rbx, rax
    and rbx, 1
    cmp rbx, 1
    je throw_invalid_arg_err
    
    sar rax, 1
    imul rax, [rsp - {}]
    
    jo throw_overflow_err",
                        exp1_instrs,
                        stk_offs,
                        exp2_instrs,
                        stk_offs
                    )

                    // Need to right shift one of the operands 1 bit (divide by
                    // 12), to preserve internal encoding accuracy
                }

                Op2::Equal => {
                    let exp1_instrs = compile_expr(exp1, si, env, lbl_num, bk_tgt);
                    let exp2_instrs = compile_expr(exp2, si + 1, env, lbl_num, bk_tgt);

                    let stk_offs = si * 8;

                    /* Move "true" into rbx, and move "false" into rax. If last cmp ("=") was true move rbx ("true") into rax.

                    Note, once both sub-exprs have been compiled and evaluated, 
                    need to ensure they are both of the same type. Do this, 
                    by XOR-ing both expressions to determine. This will set 1 as
                    the LSb if one of the exprs is 1, i.e. type mismatch. Then use and + cmp to extract the LSb and determine if it is 1. 
                    If there is a type mismatch throw an error.
                    */

                    format!(
                        "
    {}
    mov [rsp - {}], rax
    {}
    
    mov rbx, rax
    xor rbx, [rsp - {}]
    and rbx, 1
    cmp rbx, 1
    je throw_invalid_arg_err

    cmp rax, [rsp - {}]
    mov rbx, {}
    mov rax, {}
    cmove rax, rbx",
                        exp1_instrs,
                        stk_offs,
                        exp2_instrs,
                        stk_offs,
                        stk_offs,
                        encode_bool(true),
                        encode_bool(false)
                    )
                }

                Op2::Greater => {
                    let exp1_instrs = compile_expr(exp1, si, env, lbl_num, bk_tgt);
                    let exp2_instrs = compile_expr(exp2, si + 1, env, lbl_num, bk_tgt);

                    let stk_offs = si * 8;

                    // Compare LHS (exp1) to RHS (exp1), i.e. return exp1 > exp2
                    format!(
                        "
    {}

    mov rbx, rax
    and rbx, 1
    cmp rbx, 1
    je throw_invalid_arg_err

    mov [rsp - {}], rax

    {}

    mov rbx, rax
    and rbx, 1
    cmp rbx, 1
    je throw_invalid_arg_err
    
    cmp [rsp - {}], rax
    mov rbx, {}
    mov rax, {}
    cmovg rax, rbx",
                        exp1_instrs,
                        stk_offs,
                        exp2_instrs,
                        stk_offs,
                        encode_bool(true),
                        encode_bool(false)
                    )
                }

                Op2::GreaterEqual => {
                    let exp1_instrs = compile_expr(exp1, si, env, lbl_num, bk_tgt);
                    let exp2_instrs = compile_expr(exp2, si + 1, env, lbl_num, bk_tgt);

                    let stk_offs = si * 8;

                    // Compare LHS (exp1) to RHS (exp1), i.e. return exp1 > exp2
                    format!(
                        "
    {}

    mov rbx, rax
    and rbx, 1
    cmp rbx, 1
    je throw_invalid_arg_err

    mov [rsp - {}], rax
    {}

    mov rbx, rax
    and rbx, 1
    cmp rbx, 1
    je throw_invalid_arg_err

    cmp [rsp - {}], rax
    mov rbx, {}
    mov rax, {}
    cmovge rax, rbx",
                        exp1_instrs,
                        stk_offs,
                        exp2_instrs,
                        stk_offs,
                        encode_bool(true),
                        encode_bool(false)
                    )
                }

                Op2::Less => {
                    let exp1_instrs = compile_expr(exp1, si, env, lbl_num, bk_tgt);
                    let exp2_instrs = compile_expr(exp2, si + 1, env, lbl_num, bk_tgt);

                    let stk_offs = si * 8;

                    // Compare LHS (exp1) to RHS (exp1), i.e. return exp1 > exp2
                    format!(
                        "
    {}

    mov rbx, rax
    and rbx, 1
    cmp rbx, 1
    je throw_invalid_arg_err

    mov [rsp - {}], rax

    {}

    mov rbx, rax
    and rbx, 1
    cmp rbx, 1
    je throw_invalid_arg_err

    cmp [rsp - {}], rax
    mov rbx, {}
    mov rax, {}
    cmovl rax, rbx",
                        exp1_instrs,
                        stk_offs,
                        exp2_instrs,
                        stk_offs,
                        encode_bool(true),
                        encode_bool(false)
                    )
                }

                Op2::LessEqual => {
                    let exp1_instrs = compile_expr(exp1, si, env, lbl_num, bk_tgt);
                    let exp2_instrs = compile_expr(exp2, si + 1, env, lbl_num, bk_tgt);

                    let stk_offs = si * 8;

                    // Compare LHS (exp1) to RHS (exp1), i.e. return exp1 > exp2
                    format!(
                        "
    {}
    
    mov rbx, rax
    and rbx, 1
    cmp rbx, 1
    je throw_invalid_arg_err

    mov [rsp - {}], rax

    {}
    
    mov rbx, rax
    and rbx, 1
    cmp rbx, 1
    je throw_invalid_arg_err

    cmp [rsp - {}], rax
    mov rbx, {}
    mov rax, {}
    cmovle rax, rbx",
                        exp1_instrs,
                        stk_offs,
                        exp2_instrs,
                        stk_offs,
                        encode_bool(true),
                        encode_bool(false)
                    )
                }
            }
        }

        // Compile let bindings
        Expr::Let(ebinds_vec, let_ebody) => {
            let mut instrs = "".to_string();

            // Create a deep copy of current env
            let mut nenv = env.clone();

            // Create copy of si
            let mut nsi = si;

            // Keep track of ids used in bindings vector, if any duplicates
            // need to throw an error
            let mut bind_ids: HashSet<String> = hashset! {};

            for ebind in ebinds_vec {
                let (id, bind_ebody) = ebind;

                // Check for duplicate binding
                if bind_ids.contains(id) {
                    panic!("Duplicate binding");
                }

                // Need to updates variable stack offset after each iteration
                // need sufficient stack space for each variable (cannot
                // overwrite)
                let var_stk_offs = nsi * 8;

                let bind_ebody_instrs = compile_expr(bind_ebody, nsi, &nenv, lbl_num, bk_tgt);

                // Create deep immutable copy of nenv with updated binding and
                // reassign it to nenv
                nenv = nenv.update(id.to_string(), var_stk_offs);

                // Create deep immutable copy of bind_ids with newly added
                // id and reassign
                bind_ids = bind_ids.update(id.to_string());

                // Store result of bind_ebody_instrs (in rax), into
                // correct place on stack
                instrs += &format!(
                    "
    {}
    mov [rsp - {}], rax
                ",
                    bind_ebody_instrs,
                    var_stk_offs
                );

                nsi += 1;
            }

            // Use updated environemnt in let_ebody. Note, nsi and nenv
            // should account for all bindings by this point
            let let_ebody_instrs = compile_expr(let_ebody, nsi, &nenv, lbl_num, bk_tgt);

            instrs + &format!("\n\t{}", let_ebody_instrs)
        }

        // Compile If expression
        Expr::If(cond_exp, then_exp, else_exp) => {
            /*
            Compile all subexprs. Note, no need to update si since 
            no intermediate/temp values stored on stacak. According to 
            write-up, jump to else if condition is false, otherwise if 
            condition is true or some other non-false value, proceed with 
            then block.
            */

            let cond_exp_instrs = compile_expr(cond_exp, si, env, lbl_num, bk_tgt);
            let then_exp_instrs = compile_expr(then_exp, si, env, lbl_num, bk_tgt);
            let else_exp_instrs = compile_expr(else_exp, si, env, lbl_num, bk_tgt);

            // Determine labels to be used
            let else_lbl = new_label("else", lbl_num);
            let end_if_lbl = new_label("end_if", lbl_num);

            format!(
                "
    {}
    cmp rax, {}
    je {}
        {}
        jmp {}
    {}:
        {}
    {}:",

                cond_exp_instrs,
                encode_bool(false),
                else_lbl,
                then_exp_instrs,
                end_if_lbl,
                else_lbl,
                else_exp_instrs,
                end_if_lbl
            )
        }

        // Compile Loop expression
        Expr::Loop(loop_body_exp) => {
            // Determine labels to be used
            let loop_lbl = new_label("loop", lbl_num);
            let end_loop_lbl = new_label("end_loop", lbl_num);

            let loop_body_exp_instrs = compile_expr(
                loop_body_exp,
                si,
                env,
                lbl_num,
                Some(&end_loop_lbl)
            );

            format!(
                "
    {}:
        {}
        jmp {}
    {}:",
                loop_lbl,
                loop_body_exp_instrs,
                loop_lbl,
                end_loop_lbl
            )
        }

        // Compile Break expression
        Expr::Break(break_body_exp) => {
            // Compile the body of the Break expression
            let break_body_exp_instrs = compile_expr(break_body_exp, si, env, lbl_num, bk_tgt);

            // Need to ensure rax contains evaluated break_body_exp_instrs
            // prior to jmp
            match bk_tgt {
                Some(bk_tgt_lbl) => format!("
    {}
    jmp {}", break_body_exp_instrs, bk_tgt_lbl),

                None => panic!("Invalid break"),
            }
        }

        // Compile Set expression
        Expr::Set(id, set_body_exp) => {
            let set_body_exp_instrs = compile_expr(set_body_exp, si, env, lbl_num, bk_tgt);

            // Retrieve offset corresponding to id from env
            let id_stk_offs = env.get(id);

            match id_stk_offs {
                Some(_id_stk_offs) =>
                    format!("
    {}
    mov [rsp - {}], rax", set_body_exp_instrs, _id_stk_offs),

                None => panic!("Unbound variable identifier {}", id),
            }
        }

        // Compile Set expression
        Expr::Block(exps_vec) => {
            let mut instrs = "".to_string();

            // Compile each expression in order (left to right)
            for exp in exps_vec {
                let exp_instrs = compile_expr(exp, si, env, lbl_num, bk_tgt);

                instrs += &format!("\n\t{}", exp_instrs);
            }

            instrs
        }
    }
}

/*
Form of S-expressions:

pub enum Sexp {
    Atom(Atom),
    List(Vec<Sexp>),
}
pub enum Atom {
    S(String),
    I(i64),
    F(f64),
}
*/

fn parse_expr(sexp: &Sexp) -> Expr {
    // println!("Parsing sexp: {:?}", sexp);

    match sexp {
        // Parse i64 number
        Sexp::Atom(I(n)) => {
            let num = i64::try_from(*n).unwrap();

            // Perform bounds check for n (should be within bounds for a signed
            //  63-bit int)
            if num < INT_MIN || num > INT_MAX {
                panic!("Invalid");
            }

            // Perform bounds
            Expr::Number(i64::try_from(*n).unwrap())
        }

        // Parse strings
        Sexp::Atom(S(_bool)) if _bool == "true" => Expr::Boolean(true),
        Sexp::Atom(S(_bool)) if _bool == "false" => Expr::Boolean(false),
        Sexp::Atom(S(id)) => Expr::Id(id.to_string()),

        Sexp::List(vec) => {
            match &vec[..] {
                // Parse unary operation expressions ("add1", "sub1", "isnum", "isbool")
                [Sexp::Atom(S(op)), sexp] if op == "add1" =>
                    Expr::UnOp(Op1::Add1, Box::new(parse_expr(sexp))),
                [Sexp::Atom(S(op)), sexp] if op == "sub1" =>
                    Expr::UnOp(Op1::Sub1, Box::new(parse_expr(sexp))),
                [Sexp::Atom(S(op)), sexp] if op == "isnum" =>
                    Expr::UnOp(Op1::IsNum, Box::new(parse_expr(sexp))),
                [Sexp::Atom(S(op)), sexp] if op == "isbool" =>
                    Expr::UnOp(Op1::IsBool, Box::new(parse_expr(sexp))),

                // Parse arithmetic binary operations ("+", "-", "*")
                [Sexp::Atom(S(op)), sexp1, sexp2] if op == "+" =>
                    Expr::BinOp(
                        Op2::Plus,
                        Box::new(parse_expr(sexp1)),
                        Box::new(parse_expr(sexp2))
                    ),
                [Sexp::Atom(S(op)), sexp1, sexp2] if op == "-" =>
                    Expr::BinOp(
                        Op2::Minus,
                        Box::new(parse_expr(sexp1)),
                        Box::new(parse_expr(sexp2))
                    ),
                [Sexp::Atom(S(op)), sexp1, sexp2] if op == "*" =>
                    Expr::BinOp(
                        Op2::Times,
                        Box::new(parse_expr(sexp1)),
                        Box::new(parse_expr(sexp2))
                    ),

                // Parse comparison binary operations
                [Sexp::Atom(S(op)), sexp1, sexp2] if op == "=" =>
                    Expr::BinOp(
                        Op2::Equal,
                        Box::new(parse_expr(sexp1)),
                        Box::new(parse_expr(sexp2))
                    ),
                [Sexp::Atom(S(op)), sexp1, sexp2] if op == ">" =>
                    Expr::BinOp(
                        Op2::Greater,
                        Box::new(parse_expr(sexp1)),
                        Box::new(parse_expr(sexp2))
                    ),
                [Sexp::Atom(S(op)), sexp1, sexp2] if op == ">=" =>
                    Expr::BinOp(
                        Op2::GreaterEqual,
                        Box::new(parse_expr(sexp1)),
                        Box::new(parse_expr(sexp2))
                    ),
                [Sexp::Atom(S(op)), sexp1, sexp2] if op == "<" =>
                    Expr::BinOp(
                        Op2::Less,
                        Box::new(parse_expr(sexp1)),
                        Box::new(parse_expr(sexp2))
                    ),
                [Sexp::Atom(S(op)), sexp1, sexp2] if op == "<=" =>
                    Expr::BinOp(
                        Op2::LessEqual,
                        Box::new(parse_expr(sexp1)),
                        Box::new(parse_expr(sexp2))
                    ),

                // Parse Let expression (multiple  bindings)
                [Sexp::Atom(S(op)), Sexp::List(sbinds_vec), slet_body] if op == "let" => {
                    let mut ebinds_vec: Vec<(String, Expr)> = Vec::new();

                    // Verify that there are bindings in sbinds_vec
                    if sbinds_vec.is_empty() {
                        panic!("Invalid");
                    }

                    for sbind in sbinds_vec {
                        ebinds_vec.push(parse_bind(sbind));
                    }

                    Expr::Let(ebinds_vec, Box::new(parse_expr(slet_body)))
                }

                // Parse If expression
                [Sexp::Atom(S(op)), cond_sexp, then_sexp, else_sexp] if op == "if" =>
                    Expr::If(
                        Box::new(parse_expr(cond_sexp)),
                        Box::new(parse_expr(then_sexp)),
                        Box::new(parse_expr(else_sexp))
                    ),

                // Parse Loop expression
                [Sexp::Atom(S(op)), loop_body_sexp] if op == "loop" =>
                    Expr::Loop(Box::new(parse_expr(loop_body_sexp))),

                // Parse Break expression
                [Sexp::Atom(S(op)), break_body_sexp] if op == "break" =>
                    Expr::Break(Box::new(parse_expr(break_body_sexp))),

                // Parse Set! expression
                [Sexp::Atom(S(op)), Sexp::Atom(S(id)), set_body_sexp] if op == "set!" => {
                    Expr::Set(id.to_string(), Box::new(parse_expr(set_body_sexp)))
                }

                // Parse Block expression (multiple expressions)
                [Sexp::Atom(S(op)), sexps @ ..] if op == "block" => {
                    let mut exps: Vec<Expr> = Vec::new();

                    // Verify that there are expressions in sexps
                    if sexps.is_empty() {
                        panic!("Invalid");
                    }

                    for sexp in sexps {
                        exps.push(parse_expr(sexp));
                    }

                    Expr::Block(exps)
                }

                _ => panic!("Invalid"),
            }
        }

        // Otherwise, panic
        _ => panic!("Invalid"),
    }
}

// Parse single binding of form (<id> <expr>)
fn parse_bind(sbind: &Sexp) -> (String, Expr) {
    match sbind {
        // Extract vector of sexp from List
        Sexp::List(vec) => {
            // Extract individual sexprs from vector
            match &vec[..] {
                [Sexp::Atom(S(id)), sbind_body] => {
                    let keywords: HashSet<&str> = hashset!(
                        "true",
                        "false",
                        "input",
                        "let",
                        "add1",
                        "sub1",
                        "isnum",
                        "isbool",
                        "set!",
                        "if",
                        "block",
                        "loop",
                        "break"
                    );

                    // Check if id is a reserved word
                    if keywords.contains(id.as_str()) {
                        panic!("Cannot use keyword \"{}\" as identifier", id);
                    }
                    let ebind_body = parse_expr(sbind_body);

                    (id.to_string(), ebind_body)
                }

                _ => panic!("Invalid"),
            }
        }

        // Otherwise, panic
        _ => panic!("Invalid"),
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    // Retrieve name of input and output files
    let in_name = &args[1];
    let out_name = &args[2];

    // Read contents of input file
    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    // Parse input file string into sexpr -> then sexpr to expr
    let expr = parse_expr(&parse(&in_contents).expect("Invalid"));

    // Initialize environemnt as empty, ummutable hashmap
    let env: HashMap<String, i64> = hashmap! {};

    let mut lbl: i64 = 0;

    let bk_tgt: Option<&String> = None;

    // Compile into result (string of assembly instructions)
    let result = compile_expr(&expr, START_STACK_INDEX, &env, &mut lbl, bk_tgt);

    let asm_program = format!(
        "
section .text
extern snek_error
global our_code_starts_here

throw_invalid_arg_err:
    mov rdi, {}
    push rsp
    call snek_error

throw_overflow_err:
    mov rdi, {}
    push rsp
    call snek_error

our_code_starts_here:
    {}
    ret
",
        INVALID_ARG_ERR_CODE,
        OVERFLOW_ERR_CODE,
        result
    );

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}