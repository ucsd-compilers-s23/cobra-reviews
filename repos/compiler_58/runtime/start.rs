use std::env;
use std::convert::TryFrom;
use std::convert::TryInto;

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: u64) -> u64;
}

#[export_name = "\x01snek_error"]
pub extern "C" fn snek_error(errcode: i64) {
    match errcode {
        3 => {
            eprintln!("overflow");
        }
        2 => {
            eprintln!("invalid argument");
        }
        _ => {
            eprintln!("an error ocurred {errcode}");
        }
    }
    // TODO: print error message according to writeup
    std::process::exit(1);
}

fn overflow_check(input: i64) -> bool {
    let typed_value = input << 1;
    let div_value = typed_value / 2;

    return div_value == input;
}

fn parse_input(input: &str) -> u64 {
    // TODO: parse the input string into internal value representation
    //if v.len() < 2 { return 1 } 
    let s = input; 
    if s == "true" { 3 } 
    else if s == "false" { 1 }
    else {
        let i64_val = s.parse::<i64>().unwrap();
        //println!("orig value: {i64_val}");
        if overflow_check(i64_val) == false {
            eprintln!("Invalid");
            std::process::exit(1);
        } else {
            (i64_val << 1).try_into().unwrap() 
        }
    }
}

fn print_value(value: u64) {
    if value == 1 {
        println!("false");
    } else if value == 3 {
        println!("true");
    } else if value % 2 == 0 {
        let int_value = (value as i64) >> 1;
        println!("{}", int_value );
    } else {
        println!("Unknown value: {}", value);
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let i: u64 = unsafe { our_code_starts_here(input) };
    //println!("{i}");
    print_value(i);
}
