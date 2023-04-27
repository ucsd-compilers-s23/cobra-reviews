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
  if(errcode == 10) {
    eprintln!("overflow error");
  }
  else if (errcode == 2) {
    eprintln!("invalid argument");
  }
  else {
    eprintln!("an error ocurred {errcode}");
  }
  std::process::exit(1);
}

fn parse_input(input: &str) -> i64 {
  if input.to_string() == "true" {
    return 3;
  }
  else if input.to_string() == "false" {
    return 1;
  }
  else {
    // Idea to check for invalid input from: https://edstem.org/us/courses/38748/discussion/3001414
    // Error checking/handling code: https://doc.rust-lang.org/std/string/struct.String.html#method.parse
    // Idea on how to do error handling with parse: https://edstem.org/us/courses/38748/discussion/2963326 (used code from my PA2 which was inspired by this post)
    // Parsing input taken from/inspired by course GitHub
    let val = input.parse::<i64>();
    if val.is_err() {
      panic!("invalid input");
    }
    return val.unwrap()<<1;
  }
}

// //Code taken from lecture with minor change
fn print_value(i: i64) {
  if i % 2 == 0 {
    println!("{}", i >> 1);
  }
  else if i == 1 {
    println!("false");
  }
  else if i == 3{
    println!("true");
  }
  else {
    panic!("invalid argument");
  }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let i: i64 = unsafe { our_code_starts_here(input) };
    print_value(i);
}
