use std::env;
// use std::convert::TryInto;

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: i64) -> u64;
}

#[export_name = "\x01snek_error"]
pub extern "C" fn snek_error(errcode: i64) {
    // TODO: print error message according to writeup
    eprintln!("invalid argument {errcode}");
    std::process::exit(1);
}

#[export_name = "\x01snek_error2"]
pub extern "C" fn snek_error2() {
    // TODO: print error message according to writeup
    eprintln!("overflow");
    std::process::exit(1);
}

fn parse_input(input: &str) -> i64 {
    // parse the input string into internal value representation
    if input == "true" {
        3
    } else if input == "false" {
        1
    }
    // If no command-line argument is provided, the value of input is false
    else {
        // negative number
        if input.contains("-") {
            let b = input[1..].chars().all(char::is_numeric);
            if b == false {
                panic!("Invalid")
            }
            let mut res = input.parse::<i64>().unwrap();
            if res <= -4611686018427387904 {
                panic!("overflow")
            }
            res = -res; // -7 -> 7 = 0111
            let num = !res + 1; // 0111 -> 1x000 -> 1x001
            res = num << 1; // 1x0010
                            // println!("{:#b}", num);
                            // println!("{}", res);
            res
        } else {
            // positive number
            let b = input.chars().all(char::is_numeric);
            if b == false {
                panic!("Invalid")
            }
            let mut res = input.parse::<i64>().unwrap();
            if res >= 4611686018427387903 {
                panic!("overflow")
            }
            res = res << 1;
            res
        }
    }
}

fn print_value(i: u64) {
    if i % 2 == 0 {
        // 1x0010
        let num: u64 = i / 2; // 1x001
        let comp = 1 << 62;
        if num >= comp {
            // println!("num: {:#b}", num);
            // println!("num: {}", num);
            let mut new_num = !num;
            let mut shift = 1 << 63;
            shift = shift - 1;
            // println!("shift: {:#b}", shift);
            new_num = new_num & shift; // change 63-bit to 0 
            // println!("new num: {:#b}", new_num);
            if new_num >= 4611686018427387903 {
                panic!("overflow")
            }
            println!("-{}", new_num + 1)
        } else {
            if num >= 4611686018427387903 {
                panic!("overflow")
            }
            println!("{}", num);
        }
    } else {
        if i == 1 {
            println!("false")
        } else if i == 3 {
            println!("true")
        } else {
            panic!("Invalid")
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let i: u64 = unsafe { our_code_starts_here(input) };
    print_value(i);
}
