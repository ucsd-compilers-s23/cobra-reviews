use std::env;
use std::fs::File;
use std::io::prelude::*;



// from boa
use sexp::Atom::*;
use sexp::*;

use im::HashMap;

//from boa
#[derive(Debug)]
enum Val {
    Reg(Reg),
    Imm(i64), // change Imm value to 64 bits as well?
    RegOffset(Reg, i32),

    // boa need to add string for labels
    Label(String),
}

#[derive(Debug)]
enum Reg {
    RAX,
    RSP,
    // adding to store intermediate values?
    RBX,
    RDI, // for reporting errors/interacting with Rust
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),

    //adding IMul?
    IMul(Val,Val),

    //boa , adding test, jnz
    Test(Val, Val), //does a bitwise AND, and sets flags, doesn't change value of registers
    Cmp(Val, Val),

    Sar(Val, Val), //logical shift right? eg: sar rax, 1

    Jmp(Val), // unconditional jumping
    JmpE(Val), // jump equal
    JmpNZ(Val), //jump not equal to zero
    //JmpNE(Val), // jump not equal
    Label(Val), // labels

    // cmov variants: https://mudongliang.github.io/x86/html/file_module_x86_id_34.html
    CMovEq(Val, Val), // cmove for equality 
    CMovLess(Val, Val), // cmovl for less
    CMovLessEq(Val, Val), // cmovle for less than equal
    CMovGreater(Val, Val), // cmovg for greater
    CMovGreaterEq(Val, Val), // cmovge for greater than equal

    Xor(Val, Val), 



    // prob create a check number macro or smth to compare numbers
    CheckNum,
    CheckOverflow,


}
#[derive(Debug)]
enum Op1 {
    Add1,
    Sub1,

    IsNum,
    IsBool,
}

#[derive(Debug)]
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
    Number(i64), //change from i32 to i64?
    Id(String),
    // Vector is list of pairs  eg: <(x 10) (y 2) (z 20) ...>
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),

    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
    Set(String, Box<Expr>),
    Block(Vec<Expr>),
    Boolean(bool), // bool - is the rust type for true/false?
}





fn check_var_name(s: &String) -> bool {
    // pattern match convert &String to str
    match &s[..] {
        "add1" => panic!("Invalid: let bind parse error, can't use reserved keyword 'add1'"),
        "sub1" => panic!("Invalid: let bind parse error, can't use reserved keyword 'sub1'"),
        "let" => panic!("Invalid: let bind parse error, can't use reserved keyword 'let'"),

        // todo add more keywords here

        "input" => panic!("Invalid: let bind parse error, can't use reserved keyword 'input'"),

        "break" => panic!("Invalid: let bind parse error, can't use reserved keyword 'break'"),
        "if" => panic!("Invalid: let bind parse error, can't use reserved keyword 'if'"),
        "loop" => panic!("Invalid: let bind parse error, can't use reserved keyword 'loop'"),
        "true" => panic!("Invalid: let bind parse error, can't use reserved keyword 'true'"),
        "false" => panic!("Invalid: let bind parse error, can't use reserved keyword 'false'"),

        _ => true

    }


}

// parse_bind, want to make sure it is of the form ("variable name", expr)
fn parse_bind(s: &Sexp) -> (String, Expr) {
    match  s {
        Sexp::List(vec) => { 
            match &vec[..] {
			    // need to check name of the variable for not allowed ones
                [Sexp::Atom(S(name)), expr] if check_var_name(name) => (name.to_string(), parse_expr(expr)),

                _ => panic!("Invalid: parse bind error for let expressions"),
            }
        }

        _ => panic!("Invalid: parse bind error for let expressions"),

    }
    //todo!("parse_bind");
}

// helper function to return either ids or booleans
fn parse_bool(s: &String) -> Expr {
  match &s[..] {
    "true" =>  Expr::Boolean(true),
    "false" => Expr::Boolean(false),
    _ => Expr::Id(s.to_string()),
    //panic!("Invalid: parse_bool no match?"),
  }
}


// parse_expr
fn parse_expr(s: &Sexp) -> Expr {
  println!("current expr: {s}");
  let base: i64 = 2;
    match s {
        // number, want to be i64?
        Sexp::Atom(I(n)) => {
          // range of 2's compliment for 63 bits 
          if *n < -base.pow(62) || *n > base.pow(62) - 1  {
            panic!("Invalid: parse error - input number too big");
          } else {
            Expr::Number(i64::try_from(*n).expect( 
                &format!("Invalid: Parsing error")[..])
            )
          }

        },


        // parsing Let, parse_bind?


        //identifier
        Sexp::Atom(S(var)) => parse_bool(&var),
        //

        Sexp::List(vec) => {

            match &vec[..] {

                // UnOp stuff
                [Sexp::Atom(S(op)), e] if op == "add1" => Expr::UnOp(Op1::Add1, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "sub1" => Expr::UnOp(Op1::Sub1,Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "isnum" => Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "isbool" => Expr::UnOp(Op1::IsBool,Box::new(parse_expr(e))),



                // BinOp stuff

                // adding "-" parsing, no "Negate Expr", do 0 - res?
                [Sexp::Atom(S(op)), e1, e2] if op == "-" => Expr::BinOp(Op2::Minus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),

                // adding "+" parsing
                [Sexp::Atom(S(op)), e1, e2] if op == "+" => Expr::BinOp(Op2::Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),

                // adding "*" parsing
                [Sexp::Atom(S(op)), e1, e2] if op == "*" => Expr::BinOp(Op2::Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),

                // adding "<", and "<=" parsing
                [Sexp::Atom(S(op)), e1, e2] if op == "<" => Expr::BinOp(Op2::Less, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "<=" => Expr::BinOp(Op2::LessEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),

                // adding ">", and ">=" parsing
                [Sexp::Atom(S(op)), e1, e2] if op == ">" => Expr::BinOp(Op2::Greater, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == ">=" => Expr::BinOp(Op2::GreaterEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),

                // adding "=" parsing
                [Sexp::Atom(S(op)), e1, e2] if op == "=" => Expr::BinOp(Op2::Equal, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),


                // Let bindings          
                // for vector need to iterate over, then map for some operation, then collect to reform list
                // from OH rust is a lazy language 
                // need to check that vec_vars is non-empty
                [Sexp::Atom(S(op)), Sexp::List(vec_vars), body,] if op == "let" && (vec_vars.len() != 0) => 
                    Expr::Let(vec_vars.iter().map(|s| parse_bind(s)).collect::<Vec<(String, Expr)>>(),
                        Box::new(parse_expr(body))),

                //loop, break
                [Sexp::Atom(S(op)), e1 ] if op == "loop" => Expr::Loop(Box::new(parse_expr(e1))), 
                [Sexp::Atom(S(op)), e1 ] if op == "break" => Expr::Break(Box::new(parse_expr(e1))), 

                //set, block
                [Sexp::Atom(S(op)), Sexp::Atom(S(set_str)), e1 ] if op == "set!" => Expr::Set(set_str.to_string(), Box::new(parse_expr(e1))), 
                // recursive parse exprs inside

                // "tail" keyword
                // also have to check if block list is empty
                [Sexp::Atom(S(op)), tail @ ..] if op == "block" && (tail.len() != 0)  =>
                        Expr::Block(
                          tail.iter().map(|s| parse_expr(s)).collect::<Vec<Expr>>()
                        ),

                // if
                [Sexp::Atom(S(op)), cond, then, els] if op == "if" => Expr::If(Box::new(parse_expr(cond)), Box::new(parse_expr(then)), Box::new(parse_expr(els))),

                _ => panic!("Invalid: parse error"),
            }
        },

        _ => panic!("Invalid: parse error"),

    }
}

// for making new labels to jump to
fn new_label(l: &mut i32, s: &str) -> String {
    let current = *l;
    *l += 1;
    format!("{s}_{current}")
}

// from write up these need to be implemented, seem to take place of some parts of the
// can still write complile_expr first, then translate them here
// compile expr will take Expr -> Instr and Vals?
// need to the turn Vals/Instrs to String

// @params
// si - stack index
// env - hash map mapping variables to stack index
// brake - keeps track of the inner-most break label to jump to
// l - label counter
fn compile_to_instrs(e: &Expr, mut si: i32, mut env:HashMap<String, i32>, mut l: i32, brake: &str  ) -> Vec<Instr> {

    match e {
        //Numbers, need to shift to the left for tag value
        Expr::Number(n) => vec![Instr::IMov(Val::Reg(crate::Reg::RAX), Val::Imm(*n << 1).try_into().expect("overflow error, number too big"))],

        //Booleans, hard code values?
        Expr::Boolean(true) => vec![Instr::IMov(Val::Reg(crate::Reg::RAX), Val::Imm(3) )],
        Expr::Boolean(false) => vec![Instr::IMov(Val::Reg(crate::Reg::RAX), Val::Imm(1) )],

        // IDs for variable "input"
        // reserving [rsp - 16] to be input
        Expr::Id(s) if s == "input" => vec![Instr::IMov(Val::Reg(crate::Reg::RAX), Val::RegOffset(crate::Reg::RSP, 8 * 2))],

		    // IDs, 
        Expr::Id(s) => vec![Instr::IMov(Val::Reg(crate::Reg::RAX),
            // dereferencing to get the value?
            Val::RegOffset(crate::Reg::RSP, *env.get(s).expect(
                &format!("Unbound variable identifier {s}")[..]))
            )],

        Expr::UnOp(Op1::Add1 , subexpr ) => {
            //think need to bitshift numerical values as well
            let add1 = vec![Instr::IAdd(Val::Reg(crate::Reg::RAX), Val::Imm(1 << 1))];
            let mut rest = compile_to_instrs(subexpr,si, env, l, brake);

            rest.extend(vec![Instr::CheckNum]);
            rest.extend(add1);
            rest.extend(vec![Instr::CheckOverflow]);

            rest

        },

        Expr::UnOp(Op1::Sub1 , subexpr ) => { 
            // similar to Add1 above
            let sub1= vec![Instr::ISub(Val::Reg(crate::Reg::RAX), Val::Imm(1 << 1))];
            let mut rest = compile_to_instrs(subexpr, si, env, l, brake);

            rest.extend(vec![Instr::CheckNum]);
            rest.extend(sub1);
            rest.extend(vec![Instr::CheckOverflow]);

            rest

        },

        Expr::UnOp(Op1::IsBool, subexpr ) => {
            let test_bool = vec![Instr::Test(Val::Reg(crate::Reg::RAX), Val::Imm(1)),
                                              Instr::IMov(Val::Reg(crate::Reg::RBX), Val::Imm(1)), // move into RBX false first
                                              Instr::IMov(Val::Reg(crate::Reg::RAX), Val::Imm(3)), // move into RAX True
                                              Instr::CMovEq(Val::Reg(crate::Reg::RAX), Val::Reg(crate::Reg::RBX)), // move false into RAX if cmp is true eq       

            ];
            let mut rest = compile_to_instrs(subexpr, si, env, l, brake);

            rest.extend(test_bool);
            rest
        },

        Expr::UnOp(Op1::IsNum, subexpr ) => {
            //same as Isbool just swapping original stored value in RAX and RBC
            let test_num= vec![Instr::Test(Val::Reg(crate::Reg::RAX), Val::Imm(1)),
                                              Instr::IMov(Val::Reg(crate::Reg::RBX), Val::Imm(3)), // move into RBX True first
                                              Instr::IMov(Val::Reg(crate::Reg::RAX), Val::Imm(1)), // move into RAX False
                                              Instr::CMovEq(Val::Reg(crate::Reg::RAX), Val::Reg(crate::Reg::RBX)), // move True into RAX if cmp is true eq       

            ];
            let mut rest = compile_to_instrs(subexpr, si, env, l, brake);

            rest.extend(test_num);
            rest
        },

        Expr::BinOp(Op2::Plus, subexpr1 , subexpr2 ) => { 

            let mut add_inst1= compile_to_instrs(subexpr1,si, env.clone(), l, brake);

            // also store it on the stack
            let add = vec![Instr::IAdd(Val::Reg(crate::Reg::RAX), 
                                        Val::RegOffset( crate::Reg::RSP, 8*si))];

            let mov= vec![Instr::IMov(Val::RegOffset(crate::Reg::RSP, 8*si),
                                         Val::Reg(crate::Reg::RAX))];
            si = si + 1;


            let add_inst2= compile_to_instrs(subexpr2, si, env, l, brake);

			      // can't chain them together, need to do it 1 by 1
            add_inst1.extend(vec![Instr::CheckNum]); //check subexpr1 is a number
            add_inst1.extend(mov);

            add_inst1.extend(add_inst2);
            add_inst1.extend(vec![Instr::CheckNum]); // check subexpr2 is a number
            add_inst1.extend(add);
            add_inst1.extend(vec![Instr::CheckOverflow]);
            add_inst1

        },

        Expr::BinOp(Op2::Minus, subexpr1 , subexpr2 ) => { 
            let sub= vec![Instr::ISub(Val::Reg(crate::Reg::RAX), Val::RegOffset( crate::Reg::RSP, 8*si))];
            let mov= vec![Instr::IMov(Val::RegOffset(crate::Reg::RSP, 8*si), Val::Reg(crate::Reg::RAX))];
            si = si + 1;

            // why do i need to clone here, but not above?
            // subtract need to flip, subtract expr2 from expr1
            let mut sub_inst2= compile_to_instrs(subexpr2,si, env.clone(), l, brake);
            let sub_inst1= compile_to_instrs(subexpr1, si, env, l, brake);

			// can't chain them together, need to do it 1 by 1
            sub_inst2.extend(vec![Instr::CheckNum]); //check subexpr1 is a number
            sub_inst2.extend(mov);

            sub_inst2.extend(sub_inst1);
            sub_inst2.extend(vec![Instr::CheckNum]); //check subexpr2 is a number
            sub_inst2.extend(sub);
            sub_inst2.extend(vec![Instr::CheckOverflow]);

            sub_inst2

        },

        Expr::BinOp(Op2::Times, subexpr1 , subexpr2 ) => { 
            let mult = vec![Instr::IMul(Val::Reg(crate::Reg::RAX), Val::RegOffset( crate::Reg::RSP, 8*si))];
            let mov= vec![Instr::IMov(Val::RegOffset(crate::Reg::RSP, 8*si), Val::Reg(crate::Reg::RAX))];
            si = si + 1;


            // why do i need to clone here, but not above?
            // because rust only gets mad if you "pass away ownership"
            // the need to clone is to avoid transferring ownership
            // below, rust will get mad if you don't clone the first  env, since the second env will try
            // to access something after its ownership is passed away
            let mut mult_inst1= compile_to_instrs(subexpr1,si, env.clone(), l,brake);
            let mult_inst2= compile_to_instrs(subexpr2, si, env, l, brake);

            mult_inst1.extend(vec![Instr::CheckNum]); //check subexpr1 is a number
            mult_inst1.extend(mov);

            mult_inst1.extend(mult_inst2);
            mult_inst1.extend(vec![Instr::CheckNum]); //check subexpr2 is a number

            // want to divide RAX by 2 before multiplying, so we don't get *4
            mult_inst1.extend(vec![Instr::Sar(Val::Reg(crate::Reg::RAX), Val::Imm(1))]);

            mult_inst1.extend(mult);
            mult_inst1.extend(vec![Instr::CheckOverflow]);

            mult_inst1

        },

        Expr::BinOp(Op2::Equal, subexpr1, subexpr2 ) => {
            let mut eq_inst1= compile_to_instrs(subexpr1,si, env.clone(), l, brake);

            // store it on the stack

            let mov= vec![Instr::IMov(Val::RegOffset(crate::Reg::RSP, 8*si),
                                         Val::Reg(crate::Reg::RAX))];

            let compare_eq: Vec<Instr> = vec![Instr::IMov(Val::Reg(crate::Reg::RBX), Val::Reg(crate::Reg::RAX)),
            									                  // potential bug--- No, as long as this happens before "si" increments
                                                Instr::Xor(Val::Reg(crate::Reg::RBX), Val::RegOffset(crate::Reg::RSP, 8*si)),
                                                // want to compare if they are the type (int = int) or (bool = bool)
                                                Instr::Test(Val::Reg(crate::Reg::RBX), Val::Imm(1) ),
                                                // maybe want to mov errorcode into rdi or smth
                                                Instr::IMov(Val::Reg(crate::Reg::RDI), Val::Imm(4)),
                                                Instr::JmpNZ(Val::Label("snek_error".to_string())),

											
                                                Instr::Cmp(Val::Reg(crate::Reg::RAX), Val::RegOffset(crate::Reg::RSP, 8*si)),
                                                Instr::IMov(Val::Reg(crate::Reg::RBX), Val::Imm(3)), // move into RBX True first
                                                Instr::IMov(Val::Reg(crate::Reg::RAX), Val::Imm(1)), // move into RAX False
                                                Instr::CMovEq(Val::Reg(crate::Reg::RAX), Val::Reg(crate::Reg::RBX)), // move True into RAX if cmp is true eq
            ];

            si = si + 1;


            let eq_inst2= compile_to_instrs(subexpr2, si, env, l, brake);


			      // can't chain them together, need to do it 1 by 1
            eq_inst1.extend(mov); //check subexpr1 is a number
            eq_inst1.extend(eq_inst2);
            eq_inst1.extend(compare_eq);

            eq_inst1
        },

        Expr::BinOp(Op2::Less, subexpr1, subexpr2 ) => {
            let mut less_inst1= compile_to_instrs(subexpr1,si, env.clone(), l, brake);

            // store it on the stack

            let mov= vec![Instr::IMov(Val::RegOffset(crate::Reg::RSP, 8*si),
                                         Val::Reg(crate::Reg::RAX))];


            // moves and compares the 2 values in RAX and RAX, careful use of CMov variant
            // want to compare "subexpr1 < subexpr2" where former is on stack
            let compare_less: Vec<Instr> = vec![ Instr::Cmp(Val::RegOffset(crate::Reg::RSP, 8*si), Val::Reg(crate::Reg::RAX) ),
                                                Instr::IMov(Val::Reg(crate::Reg::RBX), Val::Imm(3)), // move into RBX True first
                                                Instr::IMov(Val::Reg(crate::Reg::RAX), Val::Imm(1)), // move into RAX False
                                                Instr::CMovLess(Val::Reg(crate::Reg::RAX), Val::Reg(crate::Reg::RBX)), // move True into RAX if cmp is false
            ];

            si = si + 1;


            let less_inst2= compile_to_instrs(subexpr2, si, env, l, brake);


			      // can't chain them together, need to do it 1 by 1
            less_inst1.extend(vec![Instr::CheckNum]); //check subexpr1 is a number
            less_inst1.extend(mov); 
            less_inst1.extend(less_inst2);

            less_inst1.extend(vec![Instr::CheckNum]); //check subexpr2 is a number
            less_inst1.extend(compare_less);

            less_inst1

        },

        Expr::BinOp(Op2::LessEqual, subexpr1, subexpr2 ) => {
            let mut less_eq_inst1= compile_to_instrs(subexpr1,si, env.clone(), l, brake);

            // store it on the stack

            let mov= vec![Instr::IMov(Val::RegOffset(crate::Reg::RSP, 8*si),
                                         Val::Reg(crate::Reg::RAX))];


            // moves and compares the 2 values in RAX and RAX, careful use of CMov variant
            // want to compare "subexpr1 < subexpr2" where former is on stack
            let compare_less_eq: Vec<Instr> = vec![ Instr::Cmp(Val::RegOffset(crate::Reg::RSP, 8*si), Val::Reg(crate::Reg::RAX) ),
                                                Instr::IMov(Val::Reg(crate::Reg::RBX), Val::Imm(3)), // move into RBX True first
                                                Instr::IMov(Val::Reg(crate::Reg::RAX), Val::Imm(1)), // move into RAX False
                                                Instr::CMovLessEq(Val::Reg(crate::Reg::RAX), Val::Reg(crate::Reg::RBX)), // move True into RAX if cmp is false
            ];

            si = si + 1;


            let less_eq_inst2= compile_to_instrs(subexpr2, si, env, l, brake);


			      // can't chain them together, need to do it 1 by 1
            less_eq_inst1.extend(vec![Instr::CheckNum]); //check subexpr1 is a number
            less_eq_inst1.extend(mov); 
            less_eq_inst1.extend(less_eq_inst2);

            less_eq_inst1.extend(vec![Instr::CheckNum]); //check subexpr2 is a number
            less_eq_inst1.extend(compare_less_eq);

            less_eq_inst1

        },

        Expr::BinOp(Op2::Greater, subexpr1, subexpr2 ) => {
            let mut greater_inst1= compile_to_instrs(subexpr1,si, env.clone(), l, brake);

            // store it on the stack

            let mov= vec![Instr::IMov(Val::RegOffset(crate::Reg::RSP, 8*si),
                                         Val::Reg(crate::Reg::RAX))];


            // moves and compares the 2 values in RAX and RAX, careful use of CMov variant
            // want to compare "subexpr1 > subexpr2" where former is on stack
            let compare_greater: Vec<Instr> = vec![ Instr::Cmp(Val::RegOffset(crate::Reg::RSP, 8*si), Val::Reg(crate::Reg::RAX) ),
                                                Instr::IMov(Val::Reg(crate::Reg::RBX), Val::Imm(3)), // move into RBX False first
                                                Instr::IMov(Val::Reg(crate::Reg::RAX), Val::Imm(1)), // move into RAX True
                                                Instr::CMovGreater(Val::Reg(crate::Reg::RAX), Val::Reg(crate::Reg::RBX)), // move True into RAX if cmp is false
            ];

            si = si + 1;


            let greater_inst2= compile_to_instrs(subexpr2, si, env, l, brake);


			      // can't chain them together, need to do it 1 by 1
            greater_inst1.extend(vec![Instr::CheckNum]); //check subexpr1 is a number
            greater_inst1.extend(mov); 
            greater_inst1.extend(greater_inst2);

            greater_inst1.extend(vec![Instr::CheckNum]); //check subexpr2 is a number
            greater_inst1.extend(compare_greater);

            greater_inst1

        },

        Expr::BinOp(Op2::GreaterEqual, subexpr1, subexpr2 ) => {
            let mut greater_eq_inst1= compile_to_instrs(subexpr1,si, env.clone(), l, brake);

            // store it on the stack

            let mov= vec![Instr::IMov(Val::RegOffset(crate::Reg::RSP, 8*si),
                                         Val::Reg(crate::Reg::RAX))];


            // moves and compares the 2 values in RAX and RAX, careful use of CMov variant
            // want to compare "subexpr1 >= subexpr2" where former is on stack
            let compare_greater_eq: Vec<Instr> = vec![ Instr::Cmp(Val::RegOffset(crate::Reg::RSP, 8*si), Val::Reg(crate::Reg::RAX) ),
                                                Instr::IMov(Val::Reg(crate::Reg::RBX), Val::Imm(3)), // move into RBX True first
                                                Instr::IMov(Val::Reg(crate::Reg::RAX), Val::Imm(1)), // move into RAX False
                                                Instr::CMovGreaterEq(Val::Reg(crate::Reg::RAX), Val::Reg(crate::Reg::RBX)), // move True into RAX if cmp is true 
            ];

            si = si + 1;


            let greater_eq_inst2= compile_to_instrs(subexpr2, si, env, l, brake);


			      // can't chain them together, need to do it 1 by 1
            greater_eq_inst1.extend(vec![Instr::CheckNum]); //check subexpr1 is a number
            greater_eq_inst1.extend(mov); 
            greater_eq_inst1.extend(greater_eq_inst2);

            greater_eq_inst1.extend(vec![Instr::CheckNum]); //check subexpr2 is a number
            greater_eq_inst1.extend(compare_greater_eq);

            greater_eq_inst1

        }


		    // VALUES IN "env" ARE ADDRESS DIRECTLY, ALREADY MUL BY 8
        Expr::Let(bindings, body) => {
            // want update env here, should be able to change
            let mut bindings_instrs:Vec<Instr> = vec![];

            let mut curr_vars:Vec<String> = Vec::new();
                  for(s, exp) in bindings.iter() {
                      // check first here is "s" is already in the HashMap
                      // update will automatically overwrite it, not error
                      // if so, error for duplicate binding
                      if curr_vars.contains(s) {
                          panic!("Duplicate binding");
                      }
                      curr_vars.push(s.to_string());


                      let exp_instr = compile_to_instrs(exp, si, env.clone(), l, brake);

                      env = env.update(s.to_string(), 8* si).clone();

                      let mov_var = vec![Instr::IMov(Val::RegOffset(crate::Reg::RSP, 8* si), Val::Reg(crate::Reg::RAX))];
                      // don't increment si un til we actually move something
                      // REMEMBER: always keep si fresh and ready to use, don't increment it until AFTER it is used
                      si = si + 1;

                      // combingins instrs
                      bindings_instrs.extend(exp_instr);
                      bindings_instrs.extend(mov_var);

                  }
                  // remember "Duplicate Binding"

                  let body_inst= compile_to_instrs(body, si, env, l, brake);
                  bindings_instrs.extend(body_inst);
                  bindings_instrs


            // combine instructions together bindings ++ body_inst

        },

        Expr::If(cond, then, els) => {
          let end_label = new_label(&mut l, "ifend"); // will automatically increment l by 1
          let else_label = new_label(&mut l, "ifelse"); // ig that means these labels will be different l but don't think that matters

          // want to update label counter for subsequent compliles
          // potential bug here if you don't "update globally"

          let mut cond_instrs = compile_to_instrs(cond, si, env.clone(), l, brake);
          let then_instrs = compile_to_instrs(then, si, env.clone(), l, brake);
          let els_instrs = compile_to_instrs(els, si, env, l, brake);


          cond_instrs.extend(vec![ Instr::Cmp(Val::Reg(crate::Reg::RAX), Val::Imm(1)),
                                          // if false, then jump to else branch
                                          Instr::JmpE(Val::Label(else_label.to_string())),
          ]);

          // have then branch after
          cond_instrs.extend(then_instrs);

          // make sure to unconditionally jump after, past the else branch
          // and add the else label
          cond_instrs.extend(vec![Instr::Jmp(Val::Label(end_label.to_string())),
                                          Instr::Label(Val::Label(else_label.to_string())),
          
          ]);
          // add else instrs and end label
          cond_instrs.extend(els_instrs);
          cond_instrs.extend(vec![Instr::Label(Val::Label(end_label.to_string()))]);


          cond_instrs

        }

        Expr::Block(exprs ) => {
          // flatten will eliminate outer array and "merge them"
          exprs.into_iter().map(|e| { compile_to_instrs(e, si, env.clone(), l, brake) }).flatten().collect()

        },

        Expr::Set(id_str, expr ) => {
          // evaluate expr first, then update the id_str variable to it, could error if variable is unbound
          // QUESTION: proper behavior?
          let mut value_instr = compile_to_instrs(expr, si, env.clone(), l, brake);
          let mov_var = vec![
            Instr::IMov(Val::RegOffset(crate::Reg::RSP, *env.get(id_str).expect( &format!("Unbound variable identifier {id_str}")[..])), 
                    Val::Reg(crate::Reg::RAX))];
            
          value_instr.extend(mov_var);
          value_instr

        },

        Expr::Break(expr) => {
          // check break label
          if brake == "" {
            panic!("Outside break, 'break' not inside a loop");
          }

          let mut instrs = compile_to_instrs(expr, si, env, l, brake);
          let break_instrs = vec![ Instr::Jmp(Val::Label(brake.to_string()))];

          instrs.extend(break_instrs);
          instrs

        },

        Expr::Loop(expr) => {
          let start_loop_str = new_label(&mut l, "begin_loop");
          let end_loop_str = new_label(&mut l, "end_loop");

          let mut start_loop = vec![Instr::Label(Val::Label(start_loop_str.clone()))];
          let end_loop = vec![Instr::Label(Val::Label(end_loop_str.clone()))];


          // need to clone above, or else will be using "borrowed" value of strings
          let expr_instrs = compile_to_instrs(expr, si, env, l, &end_loop_str.to_string());
          let jmp_to_loop = vec![ Instr::Jmp(Val::Label(start_loop_str.to_string()))];
          

          start_loop.extend(expr_instrs);
          start_loop.extend(jmp_to_loop);
          start_loop.extend(end_loop);

          start_loop

          // does anything need to be moved to RAX?

        }

		    // unreachable?
        //_ => panic!("compile error")

    }
}



fn instr_to_str(i: &Instr) -> String {
    match i  {
        // easy conversion? check, seems redundant
        Instr::IMov(v1, v2) => {
            let s1 = val_to_str(v1);
            let s2 = val_to_str(v2);
            //return format string
            format!("mov {s1}, {s2}\n")

        },
        Instr::IAdd(v1, v2) => {
            let s1 = val_to_str(v1);
            let s2 = val_to_str(v2);
            format!("add {s1}, {s2}\n")

        },
        Instr::ISub(v1, v2) => {
            let s1 = val_to_str(v1);
            let s2 = val_to_str(v2);
            format!("sub {s1}, {s2}\n")

        },
        Instr::IMul(v1, v2) => {
            let s1 = val_to_str(v1);
            let s2 = val_to_str(v2);
            format!("imul {s1}, {s2}\n")

        },
        Instr::Cmp(v1, v2 ) => {
            let s1 = val_to_str(v1);
            let s2 = val_to_str(v2);
            format!("cmp {s1}, {s2}\n")
        },
        Instr::JmpE(v1, ) => {
            let s1 = val_to_str(v1);
            format!("je {s1}\n")
        },
        Instr::JmpNZ(v1, ) => {
            let s1 = val_to_str(v1);
            format!("jnz {s1}\n")
        },
        Instr::Test(v1, v2) => {
            let s1 = val_to_str(v1);
            let s2 = val_to_str(v2);
            format!("test {s1}, {s2}\n")
        }
        /*
        Instr::JmpNE(v1) => {
            let s1 = val_to_str(v1);
            format!("jne {s1}\n")

        },
         */
        Instr::CMovEq(v1, v2) => {
            let s1 = val_to_str(v1);
            let s2 = val_to_str(v2);
            format!("cmove {s1}, {s2}\n")
        },
        Instr::CMovLess(v1, v2) => {
            let s1 = val_to_str(v1);
            let s2 = val_to_str(v2);
            format!("cmovl {s1}, {s2}\n")
        },
        Instr::CMovLessEq(v1, v2) => {
            let s1 = val_to_str(v1);
            let s2 = val_to_str(v2);
            format!("cmovle {s1}, {s2}\n")
        },

        Instr::CMovGreater(v1, v2) => {
            let s1 = val_to_str(v1);
            let s2 = val_to_str(v2);
            format!("cmovg {s1}, {s2}\n")
        },
        Instr::CMovGreaterEq(v1, v2) => {
            let s1 = val_to_str(v1);
            let s2 = val_to_str(v2);
            format!("cmovge {s1}, {s2}\n")
        },

        Instr::Xor(v1, v2) => {
            let s1 = val_to_str(v1);
            let s2 = val_to_str(v2);
            format!("xor {s1}, {s2}\n")
        },

        Instr::Sar(v1 , v2 ) => {
            let s1 = val_to_str(v1);
            let s2 = val_to_str(v2);
            format!("sar {s1}, {s2}\n")
        },

        // macro for checking if RAX is a number type
        Instr::CheckNum => {
          // want to put error code in RDI, but may overwrite "input"?
          format!("test RAX, 1\n
                    mov RDI, 3\n
                    jne snek_error\n
                  ")
        },
        //macro for checking if overflow occurred, jo
        Instr::CheckOverflow => {
          // will place error code in RDI after jumping
          format!("jo overflow\n")
        },
        Instr::Jmp(v1) => {
          let s1 = val_to_str(v1);
          format!("jmp {s1}\n")

        },
        Instr::Label(v1) => {
          let s1 = val_to_str(v1);
          format!("{s1}:\n")
        },

    }
}

fn val_to_str(v: &Val) -> String {
    match v { 
        Val::Reg(reg) => {
            // get the str version, or pattern on all Reg?
            format!("{:?}", reg)
        },
        Val::Imm(num) => {
            format!("{num}")

        },
        Val::RegOffset(reg, offset) => {
            format!("[{:?} - {offset}]", reg)

        },
        Val::Label(lab) => {
          format!("{}", lab)
        }

        // unreachable
        //_ => panic!("val_to_string match error"), 
    }

    //todo!("val_to_str");
}


// wrapper function
fn compile(e: &Expr) -> String {
	// declare env, have compile can access it?

    // declare env, have compile can access it?
    // could also do "hashmap!{}" ?
    let env:HashMap<String, i32> = HashMap::new();

    // compile_to_instrs() returns a vector of instrs
    // map to strings?
    // want to set label = 0
    let labels = 0; // pass into conpile
    let brake = "";
    // have si start at 3 since at index 2 will be input
    // QUESTION: should I have it at 3? storing RDI at 1 is safe?
    let prog: String = compile_to_instrs(&e,3, env, labels, brake).iter()
        .map(|i| instr_to_str(i))
        .collect();

    prog
}


fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];


    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();

    in_file.read_to_string(&mut in_contents)?;

    let expr = parse_expr(&parse(&in_contents).expect(
            &format!("Invalid: Parsing error")[..])
    );
    println!("The parsed is:\n{:?}", expr);

    let result = compile(&expr);


    let asm_program = format!(
        "
section .text
extern snek_error
global our_code_starts_here
our_code_starts_here:
mov [rsp-16], RDI
  {}
  ret

overflow:
mov RDI, 5
jmp snek_error

",
        result
    );

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}

