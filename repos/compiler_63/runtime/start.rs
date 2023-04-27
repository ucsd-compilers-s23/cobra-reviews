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
    if errcode == 7 {
        eprintln!("overflow");
        std::process::exit(1);
    } else if errcode == 3 {
        eprintln!("invalid argument");
        std::process::exit(1);
    } else {
        eprintln!("an error ocurred {errcode}");
        std::process::exit(1);
    }
}

fn parse_input(input: &str) -> u64 {
    if input == "true" {
        return 3
    } else if input == "false" {
        return 1
    } else {
        match input.parse::<i64>() {
            Ok(n) => {
                match n.checked_mul(2) {
                    Some(num) => num as u64,
                    None => panic!("overflow"),
                }
            },
            Err(_) => {
                eprintln!("input is not valid");
                std::process::exit(1);
            },
        }
    }
}

fn print_value(i: u64) {
    if i%2 == 0 {
        println!("{}", i as i64 >> 1);
    } else if i == 3 {
        println!("true");
    } else if i == 1 {
        println!("false");
    } else {
        println!("unknown {}", i);
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let i: u64 = unsafe { our_code_starts_here(input) };
    print_value(i);
}
