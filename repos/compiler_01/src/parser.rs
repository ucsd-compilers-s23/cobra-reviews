use sexp::Atom::*;
use sexp::*;

const KEYWORDS : &'static [&'static str] = &["add1", "sub1", "isnum", "isbool", "let", "set!", ];

/// Abstract syntax for unary operators
#[derive(Debug)]
pub enum Op1 {
    Add1,
    Sub1,
    IsNum,
    IsBool,
}

/// Abtract syntax for binary operators
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

/// Abstract Syntax Tree for the snek language.
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

/// High level parse function which takes the program as a String and outputs
/// the abstract Expr version of it.
/// 
/// # Panics
/// - If input program is malformed
pub fn parse_str(file_contents: &str) -> Expr {
    parse_expr(&parse(&file_contents).unwrap_or_else(|_| {
        panic!("Invalid");
    }))
}

/// Parses the given Sexp recursively into its component Expr representations
/// 
/// # Panics
/// - If input program is malformed
fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => parse_num(n),
        Sexp::Atom(S(b)) if b == "true" => Expr::Boolean(true),
        Sexp::Atom(S(b)) if b == "false" => Expr::Boolean(false),
        Sexp::Atom(S(s)) => parse_id(s),
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(op)), e] if op == "add1" => parse_unop(Op1::Add1, e),
                [Sexp::Atom(S(op)), e] if op == "sub1" => parse_unop(Op1::Sub1, e),
                [Sexp::Atom(S(op)), e] if op == "isnum" => parse_unop(Op1::IsNum, e),
                [Sexp::Atom(S(op)), e] if op == "isbool" => parse_unop(Op1::IsBool, e),
                [Sexp::Atom(S(op)), e1, e2] if op == "+" => parse_binop(Op2::Plus, e1, e2),
                [Sexp::Atom(S(op)), e1, e2] if op == "-" => parse_binop(Op2::Minus, e1, e2),
                [Sexp::Atom(S(op)), e1, e2] if op == "*" => parse_binop(Op2::Times, e1, e2),
                [Sexp::Atom(S(op)), e1, e2] if op == "=" => parse_binop(Op2::Equal, e1, e2),
                [Sexp::Atom(S(op)), e1, e2] if op == "<" => parse_binop(Op2::Less, e1, e2),
                [Sexp::Atom(S(op)), e1, e2] if op == "<=" => parse_binop(Op2::LessEqual, e1, e2),
                [Sexp::Atom(S(op)), e1, e2] if op == ">" => parse_binop(Op2::Greater, e1, e2),
                [Sexp::Atom(S(op)), e1, e2] if op == ">=" => parse_binop(Op2::GreaterEqual, e1, e2),
                [Sexp::Atom(S(op)), Sexp::List(binds), e] if op == "let" => parse_let(binds, e),
                [Sexp::Atom(S(op)), e1, e2, e3] if op == "if" => parse_if(e1, e2, e3),
                [Sexp::Atom(S(op)), e] if op == "loop" => parse_loop(e),
                [Sexp::Atom(S(op)), e] if op == "break" => parse_break(e),
                [Sexp::Atom(S(op)), Sexp::Atom(S(name)), e] if op == "set!" => parse_set(name, e),
                [Sexp::Atom(S(op)), exps @ ..] if op == "block" => parse_block(exps),
                _ => panic!("Invalid"),
            }
        },
        _ => panic!("Invalid"),
    }
}

/// Converts i64 in original program code into 63 bit format used inside of the assembly
/// program
/// 
/// # Panics
/// - If number is outside the maximum or minimum bounds of a 63 bit signed integer.
fn parse_num(n: &i64) -> Expr {
    const I63_MAX: i64 = 4611686018427387903;
    const I63_MIN: i64 = -4611686018427387904;
    if *n > I63_MAX {
        panic!("Invalid");
    }
    if *n < I63_MIN {
        panic!("Invalid");
    }

    Expr::Number(i64::try_from(*n).unwrap())
}

/// Makes sure id does not overlap with any reserved keywords.
/// We are assuming it is a valid string from the assignment instructions
/// 
/// # Panics
/// - if id name overlaps with reserved keyword
fn parse_id(s: &str) -> Expr {
    if KEYWORDS.contains(&s) {
        panic!("Invalid id overlaps with keyword");
    }
    Expr::Id(s.to_string())
}

/// Parse unary operator expression into abstract syntax form.
fn parse_unop(unop: Op1, e: &Sexp) -> Expr {
    Expr::UnOp(unop, Box::new(parse_expr(e)))
}

/// Parse binary operator expression into abstract syntax form.
fn parse_binop(binop: Op2, e1: &Sexp, e2: &Sexp) -> Expr {
    Expr::BinOp(binop, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
}

/// Parse let expression into abstract syntax form.
/// 
/// # Panics
/// - If the binding list given has no entries
fn parse_let(binds: &Vec<Sexp>, e: &Sexp) -> Expr {
    if binds.len() == 0 {
        panic!("Invalid");
    }

    Expr::Let(binds.iter().map(|bind| parse_bind(bind)).collect(), 
              Box::new(parse_expr(e)))
}

/// Parse an individual binding found inside the list of bindings of a let expression.
/// 
/// # Panics
/// - If binding name overlaps with "input"
/// - If expression is malformed
fn parse_bind(s: &Sexp) -> (String, Expr) {
    if let Sexp::List(vec) = s {
        match &vec[..] {
            [Sexp::Atom(S(name)), _] if name == "input" => panic!("binding overlaps with reserved keyword input"),
            [Sexp::Atom(S(name)), e] => (name.to_string(), parse_expr(e)),
            _ => panic!("Invalid"),
        }
    } else {
        panic!("Invalid")
    }
}

/// Parse if expression into abstract syntax format.
fn parse_if(e1: &Sexp, e2: &Sexp, e3: &Sexp) -> Expr {
    Expr::If(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)), Box::new(parse_expr(e3)))
}

/// Parse loop expression into abstract syntax format.
fn parse_loop(e: &Sexp) -> Expr {
    Expr::Loop(Box::new(parse_expr(e)))
}

/// Parse break expresion into abstract syntax format.
fn parse_break(e: &Sexp) -> Expr {
    Expr::Break(Box::new(parse_expr(e)))
}

/// Parse set expression into abstract syntax format.
fn parse_set(name: &str, e: &Sexp) -> Expr {
    Expr::Set(name.to_string(), Box::new(parse_expr(e)))
}

/// Parse block expression into abstract syntax format.
/// 
/// # Panics
/// - If there are no expressions inside of the block
fn parse_block(exps: &[Sexp]) -> Expr {
    if exps.len() == 0 {
        panic!("Invalid");
    }

    Expr::Block(exps.into_iter().map(|e| parse_expr(e)).collect())
}