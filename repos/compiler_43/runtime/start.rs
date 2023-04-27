use std::env;

// Enumerate all the dynamic error codes
const INVALID_ARG_ERR_CODE: i64 = 1;
const OVERFLOW_ERR_CODE: i64 = 2;

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
        INVALID_ARG_ERR_CODE => {
            eprintln!("invalid argument");
            std::process::exit(1);
        }
        OVERFLOW_ERR_CODE => {
            eprintln!("overflow");
            std::process::exit(1);
        }

        _ => {
            eprintln!("an error ocurred {errcode}");
            std::process::exit(1);
        }
    }
}

fn parse_input(input: &str) -> u64 {
    // TODO: parse the input string into internal value representation
    if input == "false" {
        1
    } else if input == "true" {
        3
    } else {
        let result = input.parse::<i64>();

        match result {
            Ok(num) => (num << 1) as u64,
            _ => panic!("Invalid input"),
        }
    }
}

// Decode 64-bit internal value into appropriate string
fn decode_internal_val(i: u64) -> String {
    // 63-bit signed int
    if i % 2 == 0 {
        format!("{}", (i as i64) >> 1)
    } else if
        // Booleans
        i == 1
    {
        "false".to_string()
    } else if i == 3 {
        "true".to_string()
    } else {
        // Other
        format!("Unknown: {}", i)
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);
    // println!("Input: {}", decode_internal_val(input));

    let i: u64 = unsafe { our_code_starts_here(input) };
    // println!("Program Result: {}", decode_internal_val(i));
    println!("{}", decode_internal_val(i));
}