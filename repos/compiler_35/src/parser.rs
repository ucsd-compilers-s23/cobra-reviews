use im::HashSet;
use sexp::{Sexp, Atom::*};
use crate::expr::*;

pub fn parse_s_expr(s: &Sexp) -> Expr{
    println!("{}", s);
    match s{
        Sexp::Atom(I(n)) => {
            if *n > 4611686018427387903 || *n < -4611686018427387904{
                panic!("(Invalid) Overflow - number out of bounds");
            }    
            
            Expr::Number(2 * (*n))
        },
        Sexp::Atom(S(s)) => { 
            match s.as_str(){
                "true" => Expr::Boolean(true),
                "false" => Expr::Boolean(false),
                _ => Expr::Id(s.to_string())
            }
        },
        Sexp::List(vec) => {
            match &vec[..]{
                [Sexp::Atom(S(op)), e] if op.as_str() == "loop" => Expr::Loop(Box::new(parse_s_expr(e))),
                [Sexp::Atom(S(op)), es @ ..] if op.as_str() == "block" => {
                    if es.len() == 0{
                        panic!("Invalid - block can not be empty");
                    }
                    Expr::Block(es.iter().map(|e| parse_s_expr(e)).collect())
                } 
                [Sexp::Atom(S(op1)), e] => parse_unary_op(op1, e),
                [Sexp::Atom(S(op)), e1 ,e2] if op.as_str() == "let" => parse_let(e1, e2),
                [Sexp::Atom(S(op)), Sexp::Atom(S(id)), e] if op.as_str() == "set!" => Expr::Set(id.to_string(), Box::new(parse_s_expr(e))),
                [Sexp::Atom(S(op2)), e1, e2] => parse_binary_op(op2, e1, e2),
                [Sexp::Atom(S(op)), e1, e2, e3] if op.as_str() == "if" => parse_if(e1, e2, e3),
                _ => panic!("Invalid could not create expr tree")
            }
        },
        _ => panic!("Invalid")
    }
}

fn parse_if(e1: &Sexp, e2: &Sexp, e3: &Sexp) -> Expr{
    Expr::If(Box::new(parse_s_expr(e1)), Box::new(parse_s_expr(e2)), Box::new(parse_s_expr(e3)))
}

fn parse_binary_op(op: &String, e1: &Sexp, e2: &Sexp) -> Expr{
    match op.as_str(){
        "+" => Expr::BinOp( Op2::Plus, Box::new(parse_s_expr(e1)), Box::new(parse_s_expr(e2)) ),
        "-" => Expr::BinOp( Op2::Minus, Box::new(parse_s_expr(e1)), Box::new(parse_s_expr(e2)) ),
        "*" => Expr::BinOp( Op2::Times, Box::new(parse_s_expr(e1)), Box::new(parse_s_expr(e2)) ),
        "=" => Expr::BinOp( Op2::Equal, Box::new(parse_s_expr(e1)), Box::new(parse_s_expr(e2)) ),
        "<" => Expr::BinOp( Op2::Less,  Box::new(parse_s_expr(e1)), Box::new(parse_s_expr(e2))),
        "<=" => Expr::BinOp( Op2::LessEqual,  Box::new(parse_s_expr(e1)), Box::new(parse_s_expr(e2))),
        ">" => Expr::BinOp( Op2::Greater,  Box::new(parse_s_expr(e1)), Box::new(parse_s_expr(e2))),
        ">=" => Expr::BinOp( Op2::GreaterEqual,  Box::new(parse_s_expr(e1)), Box::new(parse_s_expr(e2))),
        _ => panic!("Invalid - Bin op {} not implemented", op)
    }
}

fn parse_unary_op(op1: &String, sexp: &Sexp) -> Expr{
    match op1.as_str(){
        "add1" => Expr::UnOp(Op1::Add1, Box::new(parse_s_expr(sexp))),
        "sub1" => Expr::UnOp(Op1::Sub1, Box::new(parse_s_expr(sexp))),
        "isnum" => Expr::UnOp(Op1::IsNum, Box::new(parse_s_expr(sexp))),
        "isbool" => Expr::UnOp(Op1::IsBool, Box::new(parse_s_expr(sexp))),
        "break" => Expr::Break(Box::new(parse_s_expr(sexp))),
        _ => panic!("Invalid unary op - could not parse\n")
    }
}

fn check_keyword(id: &str) -> bool{
    let keywords:HashSet<&str> = vec!["add1", "let", "true", "false", "block", "set!", "sub1", "isbool", "isnum", "if", "input"].into_iter().collect();
    return keywords.contains(id);
}

fn parse_let(bindings: &Sexp, e2: &Sexp) -> Expr{
    let binds = match bindings{
        Sexp::List(vec) => vec,
        _ => panic!("Invalid")
    };

    if binds.len() == 0 {
        panic!("Invalid - Empty Binding list in let")
    }
    let mut binding_vec: Vec<(String, Expr)> = vec![];
    for bind in binds{
        println!("Bind: {}", bind);
        match bind{
            Sexp::List(vec) => {
                match &vec[..] {
                    [Sexp::Atom(S(id)), expr] => {
                        if !check_keyword(id){
                            binding_vec.push((id.to_string(), parse_s_expr(expr)));
                        }
                        else{
                            panic!("Invalid - keyword");
                        }
                    },
                    _ => panic!("Invalid let structure")
                }
            }
            _ => panic!("Invalid Need expression body in bind: {}\n", bind)
        }
    }
    Expr::Let(binding_vec, Box::new(parse_s_expr(e2)))
}

