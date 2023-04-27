use crate::structs::*;
use sexp::Atom::*;
use sexp::*;

pub fn parse_expr(s: &Sexp) -> Expr {
    // Reserved keywords
    let keywords = [
        "add1", "sub1", "let", "isnum", "isbool", "if", "loop", "break", "set!", "block", "input",
    ];
    match s {
        // Num
        Sexp::Atom(I(n)) => Expr::Num(i64::try_from(*n).expect("Invalid")),
        // Boolean or variable
        Sexp::Atom(S(x)) => match x.as_str() {
            "true" => Expr::Boolean(true),
            "false" => Expr::Boolean(false),
            _ => Expr::Var(String::from(x)),
        },
        // List of tokens
        Sexp::List(vec) => match &vec[..] {
            // Block
            [Sexp::Atom(S(op)), ..] if op == "block" => {
                let mut pes = vec![];
                for e in &vec[1..] {
                    pes.push(parse_expr(e));
                }
                // Enforce non zero block expressions
                if pes.len() == 0 {
                    panic!("Invalid")
                }
                Expr::Block(pes)
            }
            // Define for REPL
            [Sexp::Atom(S(op)), Sexp::Atom(S(x)), e] if op == "define" => {
                Expr::Define(x.to_string(), Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), e] => match op.as_str() {
                // Loop
                "loop" => Expr::Loop(Box::new(parse_expr(e))),
                // Break
                "break" => Expr::Break(Box::new(parse_expr(e))),
                // Unary Operators
                _ => Expr::UnOp(
                    match op.as_str() {
                        "add1" => Op1::Add1,
                        "sub1" => Op1::Sub1,
                        "isnum" => Op1::IsNum,
                        "isbool" => Op1::IsBool,
                        _ => panic!("Invalid"),
                    },
                    Box::new(parse_expr(e)),
                ),
            },
            // Set!
            [Sexp::Atom(S(op)), Sexp::Atom(S(x)), b] if op == "set!" => {
                Expr::Set(x.to_string(), Box::new(parse_expr(b)))
            }
            // Let
            [Sexp::Atom(S(op)), Sexp::List(bindings), e] if op == "let" => {
                let mut vb = vec![];
                // Enforce nonzero let bindings
                if bindings.len() == 0 {
                    panic!("Invalid")
                }
                for binding in bindings {
                    match binding {
                        Sexp::List(bind) => match &bind[..] {
                            [Sexp::Atom(S(x)), b] => {
                                // Check for keywords
                                if keywords.contains(&x.as_str()) {
                                    panic!("keyword cannot be used")
                                }
                                vb.push((x.to_string(), parse_expr(b)));
                            }
                            _ => panic!("Invalid"),
                        },
                        _ => panic!("Invalid"),
                    }
                }
                Expr::Let(vb, Box::new(parse_expr(e)))
            }
            // Binary Operators
            [Sexp::Atom(S(op)), l, r] => Expr::BinOp(
                match op.as_str() {
                    "+" => Op2::Plus,
                    "-" => Op2::Minus,
                    "*" => Op2::Times,
                    "=" => Op2::Equal,
                    ">" => Op2::Greater,
                    "<" => Op2::Less,
                    ">=" => Op2::GreaterEqual,
                    "<=" => Op2::LessEqual,
                    _ => panic!("Invalid {op}"),
                },
                Box::new(parse_expr(l)),
                Box::new(parse_expr(r)),
            ),
            // If
            [Sexp::Atom(S(op)), c, t, e] if op == "if" => Expr::If(
                Box::new(parse_expr(c)),
                Box::new(parse_expr(t)),
                Box::new(parse_expr(e)),
            ),
            _ => panic!("Invalid"),
        },
        _ => panic!("Invalid"),
    }
}
