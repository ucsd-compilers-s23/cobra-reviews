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
    // eprintln!("an error ocurred {errcode}");
    if errcode == 6 {
        eprintln!("overflow");
    } else {
        eprintln!("invalid argument");
    }
    std::process::exit(1);
}

// parse the input string into internal value representation
fn parse_input(input: &str) -> u64 {
    if input == "true" { 3 }
    else if input == "false" { 1 }
    else { input.parse::<u64>().unwrap() << 1 }
}

// convert and print out the output value accordingly.
fn print_value(val: u64) {
    if val % 2 == 0 {
        println!("{}", (val as i64)/2);
    }
    else if val == 3 {
        println!("true");
    }
    else if val == 1 {
        println!("false");
    }
    else {
        eprintln!("invalid argument");
        std::process::exit(1);
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let i: u64 = unsafe { our_code_starts_here(input) };
    print_value(i);
}
