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
    // TODO: print error message according to writeup
    if errcode == 7 { eprintln!("invalid argument"); }
    else if errcode == 8 { eprintln!("overflow"); }
    else { eprintln!("an error ocurred {errcode}"); }
    std::process::exit(1);
}

#[export_name = "\x01snek_print"]
fn snek_print(val: u64) {
    if val == 3 { println!("true"); }
    else if val == 1 { println!("false"); }
    else if val % 2 == 0 { println!("{}", (val as i64) >> 1); }
    else { println!("Unknown value: {}", (val as i64)) }
}

fn parse_input(input: &str) -> u64 {
    if input == "true" { 3 }
    else if input == "false" { 1 }
    else if input.parse::<i64>().is_ok() {
        let n = input.parse::<i64>().unwrap();
        if n < 2i64.pow(62) && n >= -2i64.pow(62) {
            (n as u64) << 1
        } else {
            panic!("Invalid")
        }
    }
    else { panic!("Invalid") }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let i: u64 = unsafe { our_code_starts_here(input) };
    snek_print(i);
}
