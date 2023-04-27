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
pub extern "C" fn snek_error(errcode: i64) -> ! {
    match errcode {
        1 => eprintln!("invalid argument"),
        2 => eprintln!("overflow"),
        3 => eprintln!("invalid input"),
        _ => eprintln!("unknown error {errcode}"),
    }
    std::process::exit(1);
}

fn parse_input(input: &str) -> i64 {
    match input {
        "true" => 3,
        "false" => 1,
        rest => match rest.parse::<i64>() {
            Ok(value) if value >= -4611686018427387904 && value <= 4611686018427387903 => value << 1,
            _ => snek_error(3),
        },
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let i: i64 = unsafe { our_code_starts_here(input) };
    //println!("{i}");
    match i {
        1 => println!("false"),
        3 => println!("true"),
        rest if rest & 1 == 0 => println!("{}", rest>>1),
        unk => println!("<Unknown value 0x{:x}>", unk),
    }
}
