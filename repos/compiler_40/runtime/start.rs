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
    if errcode == 1 { eprintln!("invalid argument") }
    else { eprintln!("overflow") }
    std::process::exit(1);
}

fn parse_input(input: &str) -> i64 {
    // TODO: parse the input string into internal value representation
    match input {
        "true" => 1,
        "false" => 3,
        //https://stackoverflow.com/questions/27043268/convert-a-string-to-int
        x => {
            let x_t = x.parse::<i64>();
            match x_t {
                Ok(x_tt) => {
                    if (x_tt < ((1 << 62) - 1)) && (x_tt > -(1 << 62)) { x_tt << 1 }
                    else { 
                        eprintln!("Invalid invalid argument");
                        std::process::exit(1);
                    }
                },
                Err(_e) => {
                    eprintln!("Invalid invalid argument");
                    std::process::exit(1);
                }
            }
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let i: i64 = unsafe { our_code_starts_here(input) };
    if i == 1 { println!("true") }
    else if i == 3 { println!("false") }
    else if i % 2 == 0 {
        println!("{}", i >> 1)
    } 
    else { println!("INVALID") }
}
