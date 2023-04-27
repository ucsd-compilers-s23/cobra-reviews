use std::env;

const I63_MIN: i64 = -4611686018427387904;
const I63_MAX: i64 = 4611686018427387903;

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
        2 => eprintln!("an error occurred: numeric overflow"),
        4 => eprintln!("an error occurred: invalid argument (incompatible types)"),
        _ => eprintln!("Unknown error code: {errcode}"),
    }
    std::process::exit(1);
}

fn parse_input(input: &str) -> u64 {
    // TODO: parse the input string into internal value representation
    match input {
        "false" => 1,
        "true" => 3,
        _ => match input.parse::<u64>() {
            Ok(num) => {
                let signed_num = num as i64;
                if signed_num < I63_MIN || signed_num > I63_MAX {
                    panic!("Invalid: input overflows a 63-bit signed integer");
                }
                return num << 1;
            }
            Err(_) => {
                panic!("Invalid: error occurred parsing input");
            }
        },
    }
}

fn format_output(val: u64) -> String {
    match val {
        1 => "false".to_string(),
        3 => "true".to_string(),
        _ => {
            let signed_val = val as i64;
            let shifted_val = signed_val >> 1;
            shifted_val.to_string()
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let output: u64 = unsafe { our_code_starts_here(input) };
    println!("{}", format_output(output));
}
