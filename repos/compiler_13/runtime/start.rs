use std::env;

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: i64) -> i64;
}

#[export_name = "\x01snek_error"]
pub extern "C" fn snek_error(errcode: i64) {
    // TODO: print error message according to writeup
    match errcode {
        1 => eprintln!("Runtime Error: unexpected input."),
        2 => eprintln!("Runtime Error: neumerical operation (op1) causes overflow."),
        3 => eprintln!("Runtime Error: neumerical operation (op2:+) causes overflow."),
        4 => eprintln!("Runtime Error: neumerical operation (op2:-) causes overflow."),
        5 => eprintln!("Runtime Error: neumerical operation (op2:*) causes overflow."),
        6 => eprintln!("Runtime Error: invalid argument in comparison."),
        7 => eprintln!("Runtime Error: invalid argument in equal comparison."),
        _ => eprintln!("Unknown error: error code: {}", errcode),
    }
    std::process::exit(errcode as i32);
}

fn parse_input(input: &str) -> i64 {
    // parse the input string into internal value representation
    if input == "false" {
        return 1;
    } else if input == "true" {
        return 3;
    } else if let Ok(i) = input.parse::<i64>() {
        return i << 1;
    } else {
        snek_error(1);
        return 0;
    }
}

fn print_value(i: i64) {
    if i == 1 {
        println!("false");
    } else if i == 3 {
        println!("true");
    } else if (i & 1) == 0 {
        println!("{}", i >> 1);
    } else {
        println!("NaN, with value {}", i);
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let i: i64 = unsafe { our_code_starts_here(input) };
    print_value(i);
}
