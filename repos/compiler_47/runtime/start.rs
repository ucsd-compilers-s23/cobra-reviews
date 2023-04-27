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
        5 => eprintln!("invalid argument"),
        7 => eprintln!("overflow error"),
        _ => eprintln!("an error ocurred {errcode}"),
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
        let temp = input.parse::<i64>();
        match temp {
            Ok(_ok) => {
                let mut val = temp.unwrap();
                if val > i64::pow(2, 62) - 1 || val < -i64::pow(2, 62) {
                    eprintln!("error: overflow for value {val}");
                    std::process::exit(1);
                } else {
                    val = val << 1;
                    val
                }
            }
            Err(_e) => {
                eprintln!("error: invalid input value");
                std::process::exit(1);
            }
        }
    }
}

fn print_value(i: i64) {
    if i % 2 == 0 {
        println!("{}", i >> 1);
    } else if i == 3 {
        println!("true");
    } else if i == 1 {
        println!("false");
    } else {
        eprintln!("error: invalid return value")
    }
}
fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let i: i64 = unsafe { our_code_starts_here(input) };
    print_value(i);
}
