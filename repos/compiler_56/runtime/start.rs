use std::env;

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: u64) -> u64;
}

#[no_mangle]
#[export_name = "\x01snek_error"]
pub extern "C" fn snek_error(errcode: i64) {
    // TODO: print error message according to writeup
    if errcode > 9 {
        eprintln!("invalid argument");
    } else if errcode == 9 {
        eprintln!("overflow");
    }
    // eprintln!("an error ocurred {errcode}");
    std::process::exit(1);
}

fn print_value(val : i64) {
    if val == 3 { 
        println!("true"); 
    } else if val == 1 { 
        println!("false"); 
    } else if val % 2 == 0 { 
        println!("{}", val >> 1);
    }
  }

fn parse_input(input: &str) -> u64 {
    // TODO: parse the input string into internal value representation
    if input == "false" { return 1 }
    let s = input;
    if s == "true" { 3 }
    else if s == "false" { 1 }
    else { (s.parse::<i64>().unwrap() as u64) << 1 }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let i: u64 = unsafe { our_code_starts_here(input) };
    print_value(i as i64);
}
