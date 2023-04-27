use crate::enums::*;
use crate::utils::*;

use sexp::Atom::*;
use sexp::*;

pub fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => parse_number(n),
        Sexp::Atom(S(s)) => parse_string(s),
        Sexp::List(vec) => {
            let arr = &vec[..];
            parse_arr_sexp(arr)
        },
        _ => panic!("parse error: Invalid expression")
    }
}

fn parse_number(n: &i64) -> Expr {
    let val = i64::try_from(*n).expect("parse error: Invalid number");
    if !is_valid_number(val) {
        panic!("parse error: Invalid number"); 
    }

    Expr::Number(val)
}

fn parse_string(s: &String) -> Expr {
    match s.as_str() {
        "true" => Expr::Boolean(true),
        "false" => Expr::Boolean(false),
        "input" => Expr::Input,
        _ => {
            // check valid variable name
            if !is_valid_variable_name(s) {
                panic!("parse error: variable identifier {} contains disallowed characters or is a keyword", s);
            }

            Expr::Id(s.to_string())
        }
    }
}

fn parse_arr_sexp(arr: &[Sexp]) -> Expr {
    match arr {
        // block: string is "block" and rest is 1 or more exprs
        [Sexp::Atom(S(s)), rest @ ..] 
            if s == "block" && rest.len() > 0 => parse_block_expr(rest),

        // if: string is "if" and includes 3 exprs
        [Sexp::Atom(S(s)), e1, e2, e3]
            if s == "if" => Expr::If(Box::new(parse_expr(e1)), 
                                     Box::new(parse_expr(e2)), 
                                     Box::new(parse_expr(e3))),

        // any s, e1, e2
        [Sexp::Atom(S(s)), e1, e2] => parse_triplet_expr(s, e1, e2),

        // any s, e
        [Sexp::Atom(S(s)), e] => parse_tuplet_expr(s, e),

        // else, fail
        _ => panic!("parse error: Invalid expression"),
    } 
}

fn parse_tuplet_expr(op: &String, e: &Sexp) -> Expr {
    match op.as_str() {
        "loop" => Expr::Loop(Box::new(parse_expr(e))),
        "break" => Expr::Break(Box::new(parse_expr(e))),
        _ => parse_unop_expr(op, e),
    }
}

fn parse_unop_expr(op: &String, e: &Sexp) -> Expr {
    let op = match op.as_str() {
        "add1" => Op1::Arith(Op1Arith::Add1),
        "sub1" => Op1::Arith(Op1Arith::Sub1),
        "isnum" => Op1::Check(Op1Check::IsNum),
        "isbool" => Op1::Check(Op1Check::IsBool),
        _ => panic!("parse error: Invalid UnOp operator")
    };

    Expr::UnOp(op, Box::new(parse_expr(e)))
}

fn parse_triplet_expr(op: &String, e1: &Sexp, e2: &Sexp) -> Expr {
    match op.as_str() {
        "let" => parse_binding_expr(e1, e2),
        "set!" => {
            // e1 is a variable name
            match e1 {
                Sexp::Atom(S(s)) => {
                    // check valid variable name
                    if !is_valid_variable_name(s) {
                        panic!("parse error: variable identifier {} contains disallowed characters or is a keyword", s);
                    }

                    Expr::Set(s.to_string(), Box::new(parse_expr(e2)))
                },
                _ => panic!("parse error: Invalid set! expression"),
            }
        }
        _ => parse_binop_expr(op, e1, e2),
    }
}

fn parse_binop_expr(op: &String, e1: &Sexp, e2: &Sexp) -> Expr {
    let op = match op.as_str() {
        "+" => Op2::Arith(Op2Arith::Plus),
        "-" => Op2::Arith(Op2Arith::Minus),
        "*" => Op2::Arith(Op2Arith::Times),
        "<" => Op2::Check(Op2Check::Less),
        "<=" => Op2::Check(Op2Check::LessEqual),
        ">" => Op2::Check(Op2Check::Greater),
        ">=" => Op2::Check(Op2Check::GreaterEqual),
        "=" => Op2::Check(Op2Check::Equal),
        _ => panic!("parse error: Invalid BinOp operator")
    };

    Expr::BinOp(op, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
}

fn parse_binding_expr(bindings: &Sexp, body: &Sexp) -> Expr {
    let binding_vec = match bindings {
        Sexp::Atom(_) => panic!("parse error: Invalid binding"),
        Sexp::List(vec) if vec.len() > 0 => vec,
        _ => panic!("parse error: Invalid binding"),
    };

    let mut parsed_bindings: Vec<(String, Expr)> = Vec::new();

    for binding in binding_vec.iter() {
        let parsed_binding = match binding {
            Sexp::Atom(_) => panic!("parse error: Invalid binding"),
            Sexp::List(vec) => {
                match &vec[..] {
                    [Sexp::Atom(S(s)), e] => {
                        // check valid variable name
                        if !is_valid_variable_name(s) {
                            panic!("parse error: variable identifier {} contains disallowed characters or is a keyword", s);
                        }

                        (s.to_string(), parse_expr(e))
                    },
                    _ => panic!("parse error: Invalid binding")
                }
            },
        };

        parsed_bindings.push(parsed_binding);
    }

    Expr::Let(parsed_bindings, Box::new(parse_expr(body)))
}

fn parse_block_expr(sexprs: &[Sexp]) -> Expr {
    let mut exprs = Vec::new();

    for s_expr in sexprs.iter() {
        exprs.push(parse_expr(s_expr));
    }

    Expr::Block(exprs)
}