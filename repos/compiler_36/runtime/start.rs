use std::env;

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
    // TODO: print error message according to writeup
    match errcode {
        0 => eprintln!("overflow"),
        1 => eprintln!("invalid argument"),
        _ => eprintln!("unknown error code: {errcode}"),
    }
    std::process::exit(1);
}

// TODO: pull this function from lib.rs
fn print_value(i: u64) {
    if i % 2 == 0 {
        println!("{}", i as i64 >> 1);
    } else if i == 3 {
        println!("true");
    } else if i == 1 {
        println!("false");
    } else {
        println!("unknown value: {i}");
    }
}

fn parse_input(input: &str) -> u64 {
    match input {
        "true" => 3,
        "false" => 1,
        _ => (input.parse::<i64>().unwrap() as u64) << 1, // TODO: check for overflow
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };

    let i: u64 = unsafe { our_code_starts_here(parse_input(&input)) };
    print_value(i);
}
