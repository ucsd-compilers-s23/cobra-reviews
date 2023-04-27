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
        7 => eprintln!("invalid argument: the arguments around = should be the same type"),
        3 => eprintln!("invalid argument: should be a number, not a boolean"),
        9 => eprintln!("overflow"),
        11 => eprintln!("invalid argument: should be a boolean, not a number"),
        _ => eprintln!("an unknown error ocurred: {errcode}"),
    }
    // eprintln!("an error ocurred {errcode}");
    std::process::exit(1);
}

fn parse_input(input: &str) -> i64 {
    match input {
        "true" => 3,
        "false" => 1,
        _ => input.parse::<i64>().unwrap() << 1,
    }
}

fn snek_print(val: i64) -> () {
    if val == 3 {
        println!("true");
    }else if val == 1 {
        println!("false");
    }else if val % 2 == 0 {
        println!("{}", val >> 1);
    }else {
        eprintln!("invalid argument: Unknown value {val}");
        std::process::exit(1);
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let i: i64 = unsafe { our_code_starts_here(input) };
    snek_print(i);
}
