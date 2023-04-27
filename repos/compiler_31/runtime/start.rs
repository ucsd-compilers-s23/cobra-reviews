use std::env;

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: u64) -> u64;
}

/*
    error codes while I do them
    3 - boolean in something that should be a number (e: add, < , >, )
    4 - not matching in equality operator

 */

#[export_name = "\x01snek_error"]
pub extern "C" fn snek_error(errcode: i64) {
    // TODO: print error message according to writeup
    match errcode {
        1 => eprintln!("an error ocurred {errcode}"),
        3 => eprintln!("an error ocurred {errcode}: invalid argument - using type boolean in an operator for integers"),
        4 => eprintln!("an error ocurred {errcode}: invalid argument - mismatched types in equality"),
        5 => eprintln!("an error ocurred {errcode}: overflow error - arithmetic caused overflow on integers"),


        _ => eprintln!("an error ocurred {errcode}"),
    }


    
    std::process::exit(1);
}

fn parse_input(input: &str) -> u64 {
    // TODO: parse the input string into internal value representation

    // valid inputs are numbers, True/False
    match input {
        "true" => 3, // true is b'00...11

        "false" => 1, // false is b'00...01

        // else try to parse into an integer from string need to shift by 1?
        _ =>   input.parse::<u64>().expect(
                &format!("invalid argument: '{}'", input)
            ) << 1,
    }
    
}


fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);


    let i: u64 = unsafe { our_code_starts_here(input) };

	// parsing output to print
    if i % 2 == 0 {
        let base: u64 = 2;
        // check if number is negative, aka if the leading bit is 1 or not
        if i/(base.pow(63)) == 1 {
            // negative number
            println!("-{}", i.wrapping_neg() >> 1);
        } else {
            // positive number
            println!("{}", ( i >> 1));
        }
    } else {
        if i == 1 {
            println!("false");

        } else if i == 3 {
            println!("true");

        } else {
            println!("ERROR: wanted to print something that is a non boolean: {i}");
        }
    }
}
