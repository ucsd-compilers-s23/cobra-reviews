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
        1 => eprintln!("Error: overflow argument"),
        2 => eprintln!("Error: invalid argument"),
        3 => eprintln!("Error: invalid input"),
        4 => eprintln!("Error: invalid output"),
        _ => eprintln!("Error: unknown error"),
    }
    std::process::exit(1);
}

fn parse_input(input: &str) -> i64 {
    // TODO: parse the input string into internal value representation
    if input == "true" {
        3
    } else if input == "false" {
        1
    } else {
        input.parse::<i64>().unwrap() << 1
    }
}

fn print_res(res: i64) {
    if res % 2 == 0 {
        println!("{}", res / 2);
    } else if res == 3 {
        println!("true");
    } else if res == 1 {
        println!("false");
    } else {
        panic!("wrong result: {}", res);
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let i: i64 = unsafe { our_code_starts_here(input) };
    print_res(i);
}
