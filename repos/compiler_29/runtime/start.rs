use std::env;

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: i64) -> i64;
}

#[no_mangle]
#[export_name = "\x01snek_error"]
pub extern "C" fn snek_error(errcode: i64) {
    match errcode {
      3 => eprintln!("invalid argument {errcode}"),
      7 => eprintln!("invalid argument {errcode}"),
      9 => eprintln!("overflow {errcode}"),
      _ => eprintln!("an error ocurred {errcode}")
    }
    std::process::exit(1);
}

fn parse_input(input: &str) -> i64 {
    if input == "true" { 3 }
    else if input == "false" { 1 }
    else { 
      let parsed = input.parse::<i64>().expect("Invalid");
      if(parsed < -4611686018427387904 || parsed > 4611686018427387903) {
        eprintln!("overflow");
        std::process::exit(1);

      }
      else { parsed << 1}
    }
}

#[no_mangle]
#[export_name = "\x01snek_print"]
fn snek_print(val : i64) -> i64 {
  if val == 3 { println!("true"); }
  else if val == 1 { println!("false"); }
  else if val % 2 == 0 { println!("{}", val >> 1); }
  else {
    eprintln!("invalid argument: {}", val); //???????
    std::process::exit(1);
  }
  return val;
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let i: i64 = unsafe { our_code_starts_here(input) };
    snek_print(i);
}
