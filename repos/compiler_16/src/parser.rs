use sexp::Atom::*;
use sexp::*;

use crate::{Bool, Op1, Op2, Expr};
use Bool::{True, False}; 

pub fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => {
            match n.to_string().parse::<i64>() {
                Ok(num) => {
                    if num <= i64::MAX >> 1 && num >= i64::MIN >> 1 {
                        Expr::Number(num)
                    } else {
                        panic!("Invalid number: integer overflow: {}", *n)
                    }

                }
                Err(_) => panic!("parse error: invalid number: {}", *n)
            }
        }, 
        Sexp::Atom(S(name)) => {
            match name.as_str(){
                "true" => Expr::Bool(True),
                "false" => Expr::Bool(False), 
                _=> Expr::Id(String::from(name))
            }

        }
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(op)), e] if op == "add1" => Expr::UnOp(Op1::Add1, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "sub1" => Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "loop" => Expr::Loop(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "break" => Expr::Break(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "print" => Expr::Print(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "isnum" => Expr::IsNum(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "isbool" => Expr::IsBool(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), exprs @ ..] if op == "block" =>  Expr::Block(exprs.into_iter().map(parse_expr).collect()),
                [Sexp::Atom(S(op)), e1, e2] if op == "+" => Expr::BinOp(Op2::Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "-" => Expr::BinOp(Op2::Minus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "*" => Expr::BinOp(Op2::Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "=" => Expr::BinOp(Op2::Equal, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == ">" => Expr::BinOp(Op2::Greater, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == ">=" => Expr::BinOp(Op2::GreaterEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "<" => Expr::BinOp(Op2::Less, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "<=" => Expr::BinOp(Op2::LessEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)),Sexp::Atom(S(name)), e] if op == "set!" => Expr::Set(name.to_string(), Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), bindings , e] if op == "let"=> {
                                                                                if let Sexp::List(binds) = bindings {
                                                                                    Expr::Let(binds.into_iter().map(parse_bind).collect(), Box::new(parse_expr(e)))
                                                                                } else {
                                                                                    panic!("Invalid bind expression: {:?}", bindings)
                                                                                }  
                                                                            }   
                [Sexp::Atom(S(op)), cond, then, els] if op == "if"=>Expr::If(Box::new(parse_expr(cond)), Box::new(parse_expr(then)), Box::new(parse_expr(els))),
                _ => panic!("Invalid List expression: {:?}", vec),
            }
        },
        _ => panic!("Invalid expression: {s}"),
    }
}
  
fn parse_bind(s: &Sexp) -> (String, Expr) {
    match s {
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(name)), e] => {
                    parse_bind_grammar_check(&name); 
                    (String::from(name), *Box::new(parse_expr(e)))
                }
                _ => panic!("Invalid bind syntax: {:?}", s),
            }
        },
        _ => panic!("Invalid bind list syntax: {:?}", s),
    }
}

fn parse_bind_grammar_check(s : &String) {
    match s.as_str() {
        "add1"|"sub1"|"let"|"input"|"block"|"print"|"set!"|"loop"|"break"|"if" => panic!("Invalid bind id: can't use the keyword {s}"),
        _ => {}
    }
}