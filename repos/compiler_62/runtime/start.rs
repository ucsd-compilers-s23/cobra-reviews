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
        1 => eprintln!("invalid argument"),
        2 => eprintln!("overflow"),
        _ => eprintln!("unknown error: {errcode}"),
    }
    std::process::exit(1);
}

fn parse_input(input: &str) -> u64 {
    if input == "true" {
        3
    } else if input == "false" {
        1
    } else if let Ok(i) = input.parse::<i64>() {
        let leading_bits = (i as u64) >> 62;
        if leading_bits == 1 || leading_bits == 2 {
            eprintln!("input overflow");
            std::process::exit(1);
        }
        (i << 1) as u64
    } else {
        eprintln!("invalid input");
        std::process::exit(1);
    }
}

fn render_snek(i: u64) -> String {
    if i % 2 == 0 {
        format!("{}", (i as i64) >> 1)
    } else {
        match i >> 1 {
            0 => format!("false"),
            1 => format!("true"),
            _ => format!("unknown: {i}"),
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let i: u64 = unsafe { our_code_starts_here(input) };
    println!("{}", render_snek(i));
}
