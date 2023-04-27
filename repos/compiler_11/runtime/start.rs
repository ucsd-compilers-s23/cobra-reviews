use std::env;
use std::convert::TryFrom;
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
    if (errcode == 1) {
        eprintln!("invalid argument");
    } else if (errcode == 2) {
        eprintln!("overflow");
    }
    std::process::exit(1);
}

fn snek_print(input: i64) -> i64 {
    // TODO: parse the input string into internal value representation

    if input == 3 { println!("true"); }
    else if input == 1 { println!("false"); }
    else if input % 2 == 0 {
        println!("{}", input >> 1);
    }
    else {
      println!("Unknown value: {}", input);
    }
    return input;
}


fn parse_arg(v : &str) -> i64 {
    if v == "true" { 3 }
    else if v == "false" { 1 }
    else {   
        let a = (v.parse::<i64>());
        match a {
            Ok(number) => {
                if (number > 4611686018427387903 || number < -4611686018427387904) {
                    panic!("Invalid")
                } else {
                    number << 1
                }
            }
            _ => panic!("Invalid")
        }
        
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_arg(&input);

    let i: i64 = unsafe { our_code_starts_here(input) };
    snek_print(i);
}
