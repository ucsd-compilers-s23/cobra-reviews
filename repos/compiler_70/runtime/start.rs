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
    if errcode == 300{
        eprintln!("Error Code {errcode} - invalid argument");
    } else if errcode == 301{
        eprintln!("Error Code {errcode} - overflow");
    }
    std::process::exit(1);
}

fn parse_input(input: &str) -> i64 {
    if input == "true" { // If input is true, return 3 (representation of true in snek)
        3 
    } else if input == "false" { // If input is false, return 1 (representation of false in snek)
        1
    } else { // Else parse input. If it is some number, check if it is within bounds. If yes "return number << 1" (left shift by 1). Else return error.
        match input.parse::<i64>() {
            Ok(n) => {
                if n > -4611686018427387904 && n < 4611686018427387903 {
                    n << 1
                } else {
                    eprintln!("Error Code 300 - invalid argument");
                    std::process::exit(1);
                }
            }
            Err(..) => {
                eprintln!("Error Code 300 - invalid argument");
                std::process::exit(1);
            }
        }
    }
}

fn print_value(i: i64){
    if i % 2 == 0{ // If i is a multiple of 2 (all valid numbers should be a multiple of 2 since we have left shifted them)
        let n = i/2;
        if n > -4611686018427387904 && n < 4611686018427387903 { // If result is within limits, print it
            print!("{n}\n")
        } else{ // Its a overflow, return an error
            panic!("Limits Error - {} caused an overflow", n)
        }
    } else if i == 3 { // 3 represents true in snek
        print!("true\n")
    } else if i == 1{ // 1 represents false in snek
        print!("false\n")
    } else{ // Value is not as defined in snek
        panic!("Value Error - {} Undefined\n", i)
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let i: i64 = unsafe { our_code_starts_here(input) };
    print_value(i)
}
