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
    eprintln!("an error ocurred {}: {}", errcode, 
        match errcode {
            1 => "overflow",
            2 => "invalid argument",
            _ => "unknown error",
        }
    );
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
    // TODO: parse the input string into internal value representation
    if input == "true" { 3 }
    else if input == "false" { 1 }
    else { 
        match input.parse::<i64>() {
            Ok(n) => {
                let m: u64 = n as u64;
                if ((m >> 62) & 1) ^ ((m >> 63) & 1) == 1 {
                    panic!("invalid argument")
                } else {
                    m << 1
                }
            },
            Err(e) => panic!("{}, invalid argument", e),
        }
    }
}

fn main() {let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let i: u64 = unsafe { our_code_starts_here(input) };
    snek_print(i as i64);
}
