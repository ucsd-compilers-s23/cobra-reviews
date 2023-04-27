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
    match errcode {
        1 => eprintln!("Invalid: overflow/underflow (1)"),
        2 => eprintln!("Invalid: type mismatch / invalid argument (2)"),
        3 => eprintln!("Invalid: bad boolean (3)"),
        _ => eprintln!("an error ocurred {errcode}")
    };

    std::process::exit(1);
}

fn parse_input(input: &str) -> u64 {
    // TODO: parse the input string into internal value representation
    match input {
        // bools
        "true" => 3,
        "false" => 1,
        // nums
        _ => {
            let ie = input.parse::<i64>();
            if let Err(_) = ie {
                snek_error(1);
                0
            } else {
                let i = ie.unwrap();
                if i as u64> (1 << 63) - 1 {
                    snek_error(1);
                }
                (i << 1) as u64
            }
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let i: u64 = unsafe { our_code_starts_here(input) };
    // Convert to real values
    if i % 2 == 1 {
        // Boolean
        if i == 1 {
            println!("{}", false);
        } else if i == 3 {
            println!("{}", true);
        } else {
            println!("SJDFLKJSDGLFKJSDF");
            // snek_error(3)
        }
    } else {
        let val: i64 = (i as i64) >> 1;
        println!("{}", val);
    }
}
