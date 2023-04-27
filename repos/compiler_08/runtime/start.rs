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
    if errcode == 0 {
        eprintln!("an error ocurred::overflow");
    }
    else{
        eprintln!("an error ocurred::invalid argument");
    }
    std::process::exit(1);
}
#[export_name = "\x01snek_print"]
pub extern "C" fn snek_print(val: u64){
    if val == 3 {
        println!("true");
    }
    else if val == 1 {
        println!("false");
    }
    else if val % 2 == 0 {
        let print_val = (val as i64) >>1;
        println!("{}", print_val);
    }
    else {
        println!("Unknown value: {}", val);
        std::process::exit(1);
    }
}

fn parse_input(input: &str) -> u64 {
    // TODO: parse the input string into internal value representation
    if input == "true" {
        3
    }
    else if input == "false" {
        1
    }
    else{
        let num = input.parse::<i64>().unwrap();
        if num > -4611686018427387904 && num <= 4611686018427387903 {
            (input.parse::<i64>().unwrap() << 1) as u64
        }
        else{
            println!("invalid argument:: overflow");
            std::process::exit(1);
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let i: u64 = unsafe { our_code_starts_here(input) };
    snek_print(i);
}
