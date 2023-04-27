use std::env;
use std::convert::TryInto;
#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: u64) -> i64;
}

#[export_name = "\x01snek_error"]
pub extern "C" fn snek_error(errcode: i64) {
    match errcode {
        1 => eprintln!("invalid argument: Unidentifiable number"), 
        2 => eprintln!("invalid argument: Integer overflow"), 
        _ => eprintln!("Program crashed with err code {errcode}:")
    }

    std::process::exit(1);
}

#[no_mangle]
#[export_name = "\x01snek_print"]
fn snek_print(val : i64) -> i64 {
  if val == 3 { println!("true"); }
  else if val == 1 { println!("false"); }
  else if val % 2 == 0 { println!("{}", val >> 1); }
  else {
    println!("Unknown value: {}", val);
  }
  return val;
}

fn parse_input(input: &str) -> u64 {
    if input.len() ==0 { return 1 }
    let s = input;
    if s == "true" { 3 }
    else if s == "false" { 1 }
    else { 
        match s.parse::<u64>() {
            Ok(val) => val << 1,
            Err(_) => panic!("invalid argument: {}", s)

        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let i: i64 = unsafe { our_code_starts_here(input) };
    snek_print(i); 
}
