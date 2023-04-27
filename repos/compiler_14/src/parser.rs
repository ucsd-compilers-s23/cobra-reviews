use sexp::*;
use crate::compiler::{Expr, Op1, Op2};

// Public interface for parsing a string into an Expr
pub fn parse_string(s: &str) -> Expr {
    let sexp = match parse(s) {
        Ok(s) => s,
        Err(e) => panic!("Invalid Parse error: {}", e),
    };
    parse_expr(&sexp)
}

// Parse Sexp into Expr
fn parse_expr(s: &Sexp) -> Expr {
    match s {
        // Atomic string
        Sexp::Atom(Atom::S(s)) => {
            match s as &str{
                "true" => Expr::Boolean(true),
                "false" => Expr::Boolean(false),
                _ => Expr::Id(s.to_string()),
            }
        }
        // Atomic integer
        Sexp::Atom(Atom::I(i)) => Expr::Number(
            match (*i as u64) & (0xC000_0000_0000_0000 as u64) {    // Mask out everything but the first two bits
                0 | 0xC000_0000_0000_0000 => *i,                    // First two bits match, no loss of information
                _ => panic!("Invalid: input out of range {}", *i),  // First two bits don't match, out of range
            }
        ),
        // Lists of Sexp
        Sexp::List(v) => {
            if v.len() == 0 { panic!("Invalid: Zero length list"); }
            match &v[0] { 
                Sexp::Atom(Atom::S(s)) => { // First element must be an atomic string which denotes some operation
                    match s.as_str() {
                        "let" => {
                            if v.len() != 3 { panic!("Invalid: Let has an unexpected number of arguments"); }
                            let bindings = parse_binds(&v[1]);
                            let body = parse_expr(&v[2]);
                            Expr::Let(bindings, Box::new(body))
                        }
                        "set!" => {
                            if v.len() != 3 { panic!("Invalid: Set has an unexpected number of arguments"); }
                            let name = match &v[1] {
                                Sexp::Atom(Atom::S(s)) => s.to_string(),
                                _ => panic!("Invalid: Set has an unexpected argument"),
                            };
                            let body = parse_expr(&v[2]);
                            Expr::Set(name, Box::new(body))
                        }
                        "add1" => {
                            if v.len() != 2 { panic!("Invalid: Add1 has an unexpected number of arguments"); }
                            Expr::UnOp(Op1::Add1, Box::new(parse_expr(&v[1])))
                        }
                        "sub1" => {
                            if v.len() != 2 { panic!("Invalid: Sub1 has an unexpected number of arguments"); }
                            Expr::UnOp(Op1::Sub1, Box::new(parse_expr(&v[1])))
                        }
                        "+" => {
                            if v.len() != 3 { panic!("Invalid: Addition has an unexpected number of arguments"); }
                            Expr::BinOp(Op2::Plus, Box::new(parse_expr(&v[1])), Box::new(parse_expr(&v[2])))
                        }
                        "-" => {
                            if v.len() != 3 { panic!("Invalid: Subtraction has an unexpected number of arguments"); }
                            Expr::BinOp(Op2::Minus, Box::new(parse_expr(&v[1])), Box::new(parse_expr(&v[2])))
                        }
                        "*" => {
                            if v.len() != 3 { panic!("Invalid: Multiplication has an unexpected number of arguments"); }
                            Expr::BinOp(Op2::Times, Box::new(parse_expr(&v[1])), Box::new(parse_expr(&v[2])))
                        }
                        "isnum" => {
                            if v.len() != 2 { panic!("Invalid: Isnum has an unexpected number of arguments"); }
                            Expr::UnOp(Op1::IsNum, Box::new(parse_expr(&v[1])))
                        }
                        "isbool" => {
                            if v.len() != 2 { panic!("Invalid: Isbool has an unexpected number of arguments"); }
                            Expr::UnOp(Op1::IsBool, Box::new(parse_expr(&v[1])))
                        }
                        "<" => {
                            if v.len() != 3 { panic!("Invalid: Less than has an unexpected number of arguments"); }
                            Expr::BinOp(Op2::Less, Box::new(parse_expr(&v[1])), Box::new(parse_expr(&v[2])))
                        }
                        ">" => {
                            if v.len() != 3 { panic!("Invalid: Greater than has an unexpected number of arguments"); }
                            Expr::BinOp(Op2::Greater, Box::new(parse_expr(&v[1])), Box::new(parse_expr(&v[2])))
                        }
                        ">=" => {
                            if v.len() != 3 { panic!("Invalid: Greater than or equal to has an unexpected number of arguments"); }
                            Expr::BinOp(Op2::GreaterEqual, Box::new(parse_expr(&v[1])), Box::new(parse_expr(&v[2])))
                        }
                        "<=" => {
                            if v.len() != 3 { panic!("Invalid: Less than or equal to has an unexpected number of arguments"); }
                            Expr::BinOp(Op2::LessEqual, Box::new(parse_expr(&v[1])), Box::new(parse_expr(&v[2])))
                        }
                        "=" => {
                            if v.len() != 3 { panic!("Invalid: Equal to has an unexpected number of arguments"); }
                            Expr::BinOp(Op2::Equal, Box::new(parse_expr(&v[1])), Box::new(parse_expr(&v[2])))
                        }
                        "if" => {
                            if v.len() != 4 { panic!("Invalid: If has an unexpected number of arguments"); }
                            let cond = parse_expr(&v[1]);
                            let then = parse_expr(&v[2]);
                            let els = parse_expr(&v[3]);
                            Expr::If(Box::new(cond), Box::new(then), Box::new(els))
                        }
                        "block" => {
                            if v.len() < 2 { panic!("Invalid: Block must contain at least one expression"); }
                            let mut exprs = Vec::new();
                            for i in 1..v.len() {
                                exprs.push(parse_expr(&v[i]));
                            }
                            Expr::Block(exprs)
                        }
                        "loop" => {
                            if v.len() != 2 { panic!("Invalid: Loop has an unexpected number of arguments"); }
                            Expr::Loop(Box::new(parse_expr(&v[1])))
                        }
                        "break" => {
                            if v.len() != 2 { panic!("Invalid: Break has an unexpected number of arguments"); }
                            Expr::Break(Box::new(parse_expr(&v[1])))
                        }
                        _ => panic!("Invalid: Unrecognized operation"),
                    }
                }
                _ => panic!("Invalid: Not an operation"),
            }
        }
        _ => panic!("Invalid"),
    }
}

// Parse a list of Sexp bindings into Expr spec bindings
fn parse_binds(s: &Sexp) -> Vec<(String, Expr)> {
    match s {
        Sexp::List(v) => {
            let mut bindings = Vec::new();
            if v.len() == 0 { panic!("Invalid: Let statement must contain at least one binding");   }
            for i in 0..v.len() {
                bindings.push(parse_bind(&v[i]));
            }
            bindings
        }
        _ => panic!("Invalid: Let bindings must be enclosed in parentheses"),
    }
}

// Parse single Sexp binding into Expr spec binding
fn parse_bind(s: &Sexp) -> (String, Expr) {
    match s {
        Sexp::List(v) => {
            if v.len() != 2 {
                panic!("Invalid: Bindings must have exactly two elements");
            }
            match &v[0] {
                Sexp::Atom(Atom::S(s)) => {
                    reserved_keyword_check(&s);
                    (s.clone(), parse_expr(&v[1]))
                }
                _ => panic!("Invalid Variable Name: Must be a string"),
            }
        }
        _ => panic!("Invalid"),
    }
}

// Check if a string is a reserved keyword
fn reserved_keyword_check(s: &str) {
    match s {
        "let" => panic!("Invalid: Reserved keyword"),
        "add1" => panic!("Invalid: Reserved keyword"),
        "sub1" => panic!("Invalid: Reserved keyword"),
        "isnum" => panic!("Invalid: Reserved keyword"),
        "isbool" => panic!("Invalid: Reserved keyword"),
        "if" => panic!("Invalid: Reserved keyword"),
        "block" => panic!("Invalid: Reserved keyword"),
        "loop" => panic!("Invalid: Reserved keyword"),
        "break" => panic!("Invalid: Reserved keyword"),
        "true" => panic!("Invalid: Reserved keyword"),
        "false" => panic!("Invalid: Reserved keyword"),
        "input" => panic!("Invalid: Reserved keyword"),
        _ => (),
    }
}