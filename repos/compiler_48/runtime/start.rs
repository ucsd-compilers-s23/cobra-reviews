

use std::env;

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: u64) -> u64;
}

/*
 * errcode:
 * 1 = overflow
 * 2 = invalid types
 */
#[export_name = "\x01snek_error"]
pub extern "C" fn snek_error(errcode: i64) {
    // TODO: print error message according to writeup
    let err_msg = match errcode {
        1 => "overflow",
        2 => "invalid argument",
        _ => "other",
    };

    eprintln!("runtime error: {}", err_msg);
    std::process::exit(1);
}

fn parse_input(input: &str) -> u64 {
    if input == "true" {
        3
    } else if input == "false" {
        1
    } else {
        let input = input.parse::<i64>().expect("runtime error: invalid argument");

        let lower_bound: i64 = -(1 << 62);
        let upper_bound: i64 = (1 << 62) - 1;

        if input < lower_bound || input > upper_bound {
            panic!("runtime error: overflow");
        }
        
        (input << 1) as u64
    }
}

fn print_output(i: u64) {
    match i {
        1 => println!("false"),
        3 => println!("true"),
        val if val % 2 == 0 => {
            let val: i64 = val as i64;
            println!("{}", val >> 1);
        },
        _ => panic!("runtime error: invalid value"),
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let i: u64 = unsafe { our_code_starts_here(input) };
    print_output(i);
}
