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
    let err_name = match errcode {
        101 => "invalid argument",
        102 => "error: overflow",
        _ => "unknown error occured",
    };
    eprintln!("Error: {}", err_name);
    std::process::exit(1);
}

fn parse_input(input: &str) -> i64 {
    // TODO: parse the input string into internal value representation

    if input == "true" { 3 } else if input == "false" { 1 } else {
        let input_pre = input.parse::<i64>().unwrap_or_else(
                |_| panic!("Invalid input")
            );
        if input_pre > 4611686018427387903 || input_pre < -4611686018427387904 {
            panic!("Invalid input: overflowing")
        }
        input_pre << 1
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let i: i64 = unsafe { our_code_starts_here(input) };
    if i == 1 {
        println!("false");
    } else if i == 3{
        println!("true");
    } else {
        println!("{}", i>>1);
    }
}
