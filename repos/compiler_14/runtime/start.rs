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
        1 => eprintln!("Runtime Error invalid argument: {errcode}"),
        2 => eprintln!("Runtime Error overflow: {errcode}"),
        _ => eprintln!("Runtime Error unknown: {errcode}"),
    }
    std::process::exit(1);
}

// Parse input from command line
fn parse_input(input: &str) -> u64 {
    match input {
        "true" => 3,
        "false" => 1,
        _ => {
            let i: i64 = match input.parse() {
                Ok(i) => i,
                Err(_) => {
                    eprintln!("Invalid Input: {}", input);
                    std::process::exit(1);
                }
            };
            match (i as u64) & (0xC000_0000_0000_0000 as u64) { // Mask out everything but the first two bits
                0 | 0xC000_0000_0000_0000 => (i as u64) << 1,   // First two bits match, no loss of information
                _ => {                                          // First two bits don't match, throw error
                    eprintln!("Invalid Input: {}", input);
                    std::process::exit(1);
                }
            }
        }
    }
}

// Parse output to command line
fn parse_output(output: u64) -> String {
    match output & 1 {
        1 => match output {
            1 => "false".to_string(),
            3 => "true".to_string(),
            _ => panic!("Unexpected return value: {}", output),
        },
        _ => (output as i64 >> 1).to_string(),
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let i: u64 = unsafe { our_code_starts_here(input) };
    let output = parse_output(i);
    println!("{}", output);
}
