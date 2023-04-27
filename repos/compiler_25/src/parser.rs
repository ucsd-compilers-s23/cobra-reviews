/**
 * The parser for the compiler.
 */
use sexp::Atom::*;
use sexp::*;

use crate::abstract_syntax::*;

use im::HashSet;

// Converts an Sexp to an Expr. Panics if there was an error.
pub fn parse_sexpr(sexpr: &Sexp) -> Expr {
    let reserved_strings: HashSet<String> = [
        "add1", "sub1", "let", "if", "block", "loop", "break", "set!", "isnum", "isbool",
    ]
    .iter()
    .cloned()
    .collect();
    match sexpr {
        Sexp::Atom(I(num)) => {
            // Allow overflow
            let parsed_num = i64::try_from(*num).ok();
            return Expr::Number(match parsed_num {
                Some(n) => n,
                None => 0,
            });
        }
        // Sexp::Atom(F(num)) => {
        //     // Allow truncation
        //     let parsed_float = f64::try_from(*num).ok();
        //     return Expr::Number(match parsed_float {
        //         Some(n) => n.trunc() as i64,
        //         None => 0,
        //     });
        // }
        // Boolean
        Sexp::Atom(S(name)) if name == "false" => Expr::Boolean(false),
        Sexp::Atom(S(name)) if name == "true" => Expr::Boolean(true),
        // Identifier
        Sexp::Atom(S(name)) => {
            if reserved_strings.contains(name) {
                panic!("Invalid: reserved keyword");
            }
            Expr::Id(name.to_string())
        }

        // List pattern
        Sexp::List(vec) => match &vec[..] {
            // Type checks
            [Sexp::Atom(S(op)), e] if op == "isnum" => {
                Expr::UnOp(Op1::IsNum, Box::new(parse_sexpr(e)))
            }
            [Sexp::Atom(S(op)), e] if op == "isbool" => {
                Expr::UnOp(Op1::IsBool, Box::new(parse_sexpr(e)))
            }

            // Unary operators
            [Sexp::Atom(S(op)), e] if op == "add1" => {
                Expr::UnOp(Op1::Add1, Box::new(parse_sexpr(e)))
            }
            [Sexp::Atom(S(op)), e] if op == "sub1" => {
                Expr::UnOp(Op1::Sub1, Box::new(parse_sexpr(e)))
            }

            // If
            [Sexp::Atom(S(keyword)), cond, thn, els] if keyword == "if" => Expr::If(
                Box::new(parse_sexpr(cond)),
                Box::new(parse_sexpr(thn)),
                Box::new(parse_sexpr(els)),
            ),

            // Arithmetic
            [Sexp::Atom(S(op)), e1, e2] if op == "+" => Expr::BinOp(
                Op2::Plus,
                Box::new(parse_sexpr(e1)),
                Box::new(parse_sexpr(e2)),
            ),
            [Sexp::Atom(S(op)), e1, e2] if op == "-" => Expr::BinOp(
                Op2::Minus,
                Box::new(parse_sexpr(e1)),
                Box::new(parse_sexpr(e2)),
            ),
            [Sexp::Atom(S(op)), e1, e2] if op == "*" => Expr::BinOp(
                Op2::Times,
                Box::new(parse_sexpr(e1)),
                Box::new(parse_sexpr(e2)),
            ),
            // Comparison
            [Sexp::Atom(S(op)), e1, e2] if op == "=" => Expr::BinOp(
                Op2::Equal,
                Box::new(parse_sexpr(e1)),
                Box::new(parse_sexpr(e2)),
            ),
            [Sexp::Atom(S(op)), e1, e2] if op == ">" => Expr::BinOp(
                Op2::Greater,
                Box::new(parse_sexpr(e1)),
                Box::new(parse_sexpr(e2)),
            ),
            [Sexp::Atom(S(op)), e1, e2] if op == ">=" => Expr::BinOp(
                Op2::GreaterEqual,
                Box::new(parse_sexpr(e1)),
                Box::new(parse_sexpr(e2)),
            ),
            [Sexp::Atom(S(op)), e1, e2] if op == "<" => Expr::BinOp(
                Op2::Less,
                Box::new(parse_sexpr(e1)),
                Box::new(parse_sexpr(e2)),
            ),
            [Sexp::Atom(S(op)), e1, e2] if op == "<=" => Expr::BinOp(
                Op2::LessEqual,
                Box::new(parse_sexpr(e1)),
                Box::new(parse_sexpr(e2)),
            ),

            // let
            [Sexp::Atom(S(keyword)), bindings, body] if keyword == "let" => {
                let bindings = parse_bindings(bindings);
                // Check for no bindings
                if bindings.is_empty() {
                    panic!("Invalid: no bindings");
                }
                Expr::Let(bindings, Box::new(parse_sexpr(body)))
            }

            // Set!
            [Sexp::Atom(S(op)), Sexp::Atom(S(name)), e] if op == "set!" => {
                if reserved_strings.contains(name)
                    || name == "true"
                    || name == "false"
                    || name == "input"
                {
                    panic!("Invalid: reserved keyword");
                }
                Expr::Set(name.to_string(), Box::new(parse_sexpr(e)))
            }

            // Block
            [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => {
                let parsed_exprs: Vec<Expr> = exprs.iter().map(parse_sexpr).collect();
                if parsed_exprs.is_empty() {
                    panic!("Invalid: no expressions for block");
                }
                Expr::Block(parsed_exprs)
            }

            // Loop
            [Sexp::Atom(S(op)), e] if op == "loop" => Expr::Loop(Box::new(parse_sexpr(e))),
            // Break
            [Sexp::Atom(S(op)), e] if op == "break" => Expr::Break(Box::new(parse_sexpr(e))),

            // Singleton list - invalid:
            // [sexpr] => {
            //     return parse_sexpr(sexpr);
            // }

            // Unrecognized list pattern
            _ => {
                panic!("Invalid: {sexpr:?}")
            }
        },

        _ => {
            panic!("Invalid");
        }
    }
}

// Parses 1 or more let bindings
fn parse_bindings(sexpr: &Sexp) -> Vec<(String, Expr)> {
    match sexpr {
        Sexp::List(vec) => {
            return vec.iter().map(parse_bind).collect();
        }
        _ => {
            panic!("Invalid");
        }
    }
}

// Parses a single let binding
fn parse_bind(sexpr: &Sexp) -> (String, Expr) {
    let reserved_strings: HashSet<String> = [
        "add1", "sub1", "let", "if", "block", "loop", "break", "set!", "isnum", "isbool", "true",
        "false", "input",
    ]
    .iter()
    .cloned()
    .collect();
    match sexpr {
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(name)), e] => {
                if reserved_strings.contains(name) {
                    panic!("Invalid: reserved keyword");
                } else {
                    return (name.to_string(), parse_sexpr(e));
                }
            }
            _ => {
                panic!("Invalid");
            }
        },
        _ => {
            panic!("Invalid")
        }
    }
}
