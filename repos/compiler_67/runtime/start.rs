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
    if errcode == 3 {
        eprintln!("invalid argument");
    } else if errcode == 7 {
        eprintln!("invalid argument");
    } else if errcode == 9 {
        eprintln!("overflow");
    } else {
        eprintln!("an error ocurred {errcode}");
    }
    std::process::exit(1);
}

fn parse_input(args: &Vec<String>) -> u64 {
    // No args, return value as false (1)
    if args.len() < 2 {
        return 1;
    }
    let input = &args[1];
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
    let i_i64 = i as i64;
    if i_i64 == 3 {
        println!("true",);
    } else if i_i64 == 1 {
        println!("false");
    } else if i_i64 % 2 == 0 {
        println!("{}", i_i64 >> 1);
    } else {
        println!("Unknown {}", i_i64);
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = parse_input(&args);
    let i: u64 = unsafe { our_code_starts_here(input) };
    print_value(i);
}
