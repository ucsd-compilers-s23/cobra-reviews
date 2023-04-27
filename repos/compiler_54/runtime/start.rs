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
    if errcode == 1 {
        eprintln!("invalid argument: type error");
    } else if errcode == 2 {
        eprintln!("Invalid: overflow error");
    }
    std::process::exit(1);
}

fn parse_input(input: &str) -> i64 {
    // TODO: parse the input string into internal value representation
    match input {
        "true" => 3,
        "false" => 1,
        _ => {
            let value = input
                .parse::<i64>()
                .unwrap_or_else(|_| panic!("Invalid: input overflow"));

            match value.checked_shl(1) {
                Some(shifted_value) => shifted_value,
                None => panic!("Invalid: overflow occurred when shifting input."),
            }
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input_i64 = parse_input(&input);

    let input_u64 = input_i64 as u64;

    let i: u64 = unsafe { our_code_starts_here(input_u64) };
    let i_type = i & 1;
    if i_type == 1 {
        let result;
        if i == 3 {
            result = true;
        } else {
            result = false;
        }
        println!("{result}");
    } else {
        let mut result = i as i64;
        result = result >> 1;
        println!("{result}");
    }
}
