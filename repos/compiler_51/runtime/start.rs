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
    match errcode {
        1 => eprintln!("integer overflow"),
        2 => eprintln!("invalid argument"),
        _ => panic!("invalid error code {}", errcode),
    }

    std::process::exit(1);
}

fn to_internal_repr(val: i64) -> u64 {
    if val < -(1 << 62) || val > ((1 << 62) - 1) {
        panic!("Invalid integer literal {}", val)
    }
    (val << 1) as u64
}

fn parse_input(input: &str) -> u64 {
    match input {
        "true" => 3,
        "false" => 1,
        _ => match input.parse() {
            Ok(val) => to_internal_repr(val),
            _ => panic!("Invalid input {}", input),
        },
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let i: u64 = unsafe { our_code_starts_here(input) };
    let i = i as i64;
    match i {
        1 => println!("false"),
        3 => println!("true"),
        i if i % 2 == 0 => println!("{}", i / 2),
        _ => panic!("Invalid return value {}", i),
    }
}
