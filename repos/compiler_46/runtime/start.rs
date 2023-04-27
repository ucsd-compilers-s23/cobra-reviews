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
        2 => eprintln!("invalid argument. can only check equality between the same type"),
        3 => eprintln!("invalid argument"),
        4 => eprintln!("overflow"),
        _ => eprintln!("an error ocurred {errcode}"),
    }
    std::process::exit(1);
}

fn parse_input(input: &str) -> u64 {
    if input.is_empty() {
        return 0b01;
    }
    match input {
        "true" => 0b11,
        "false" => 0b01,
        _ => u64::from_ne_bytes((i64::from_str_radix(input, 10).expect("invalid argument") << 1).to_ne_bytes()),
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let i: u64 = unsafe { our_code_starts_here(input) };
    // println!("{i:b}");
    // println!("{i}");
    let output = match i {
        0b01 => String::from("false"),
        0b11 => String::from("true"),
        _ => format!("{}", (i as i64) >> 1), // convert numbers back to their actual values
    };
    println!("{output}");
}
