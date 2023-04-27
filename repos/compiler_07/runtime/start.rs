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
    // print error message according to writeup
    let message = match errcode {
        1 => "invalid argument",
        2 => "overflow",
        _ => "unknown error",
    };
    eprintln!("runtime error: {message}");
    std::process::exit(1);
}

fn parse_input(input: &str) -> u64 {
    // TODO: parse the input string into internal value representation
    if input == "true" {
        return 3;
    } else if input == "false" {
        return 1;
    } else {
        return (input.parse::<i64>().unwrap() << 1) as u64;
    }
}

fn print_value(i: u64) {
    if i % 2 == 0 {
        println!("{}", i as i64 >> 1)
    } else if i == 3 {
        println!("true")
    } else if i == 1 {
        println!("false")
    } else {
        println!("Unknown: {:#x}", i)
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let i: u64 = unsafe { our_code_starts_here(input) };
    print_value(i);
}
