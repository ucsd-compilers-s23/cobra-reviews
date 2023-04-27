use std::{env, fmt};

#[repr(C)]
pub struct InternalValue (pub i64);

impl fmt::Display for InternalValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0 & 1 == 1 {
            // Is not a number
            match self.0 {
                3 => write!(f, "true"),
                1 => write!(f, "false"),
                _ => write!(f, "Unknown type: {:#x}", self.0),
            }
        } else {
            // Is a number
            write!(f, "{}", self.0 >> 1)
        }
    }
}

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: u64) -> u64;
}

#[export_name = "\x01snek_error"]
pub extern "C" fn snek_error(errcode: i64, value: InternalValue) -> ! {
    // TODO: print error message according to writeup
    match errcode {
        1 => eprintln!("Runtime error: overflow to {}", value),
        2 => eprintln!("Runtime error: invalid argument"),
        _ => eprintln!("Runtime error: unknown error code: {}", errcode),
    }
    std::process::exit(1);
}

fn parse_input(input: &str) -> u64 {
    match input {
        "true" => 3,
        "false" => 1,
        _ => {
            let n = input.parse::<i64>().unwrap_or_else(|_| panic!("Invalid input: {}", input));
            if n < 0 {
                panic!("Invalid input: cannot parse {} as an i64", input);
            }

            let shifted = n << 1;
            if shifted >> 1 != n {
                panic!("Invalid input: {} is not a 63-bit signed integer", input);
            }
            (n << 1) as u64
        }
    }
}

/// Prints a value in the internal representation
fn print_value(value: i64) {
    println!("{}", InternalValue(value));
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let i: u64 = unsafe { our_code_starts_here(input) };
    print_value(i as i64);
}
