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
        1 => eprintln!("an overflow error ocurred"),
        2 => eprintln!("an invalid argument error ocurred"),
        _ => eprintln!("an unkown error ocurred, code {errcode}"),
    }
    std::process::exit(1);
}

fn parse_input(input: &str) -> u64 {
    if input == "true" {3}
    else if input == "false" {1}
    else {
        let num = input.parse::<i64>().expect("Invalid");
        if num > (i64::pow(2,62) - 1) || num < (0 - i64::pow(2,62)) {
            panic!("Invalid")
          } else {
            (num as u64) << 1
          }
    } 
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let i: u64 = unsafe { our_code_starts_here(input) };
    if i%2 == 0 {
        println!("{}", (i as i64) / 2);
    } else if i == 3 {
        println!("true");
    } else if i == 1 {
        println!("false");
    } else {
        println!("Unknown: {}",i);
    }
}
