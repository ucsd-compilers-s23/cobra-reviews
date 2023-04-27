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
    let message = match errcode {
        2 =>
            "invalid input: not an integer or magnitude too large",
        3 =>
            "invalid input: not a signed 63-bit number",
        4 =>
            "overflow: add1",
        5 =>
            "overflow: sub1",
        6 =>
            "overflow: +",
        7 =>
            "overflow: -",
        8 =>
            "overflow: *",
        9 =>
            "invalid argument: =",
        10 =>
            "invalid argument: >",
        11 =>
            "invalid argument: >=",
        12 =>
            "invalid argument: <",
        13 =>
            "invalid argument: <=",
        14 =>
            "invalid argument: add1",
        15 =>
            "invalid argument: sub1",
        16 =>
            "invalid argument: +",
        17 =>
            "invalid argument: -",
        18 =>
            "invalid argument: *",
        _ => "unknown error",
    };
    eprintln!("{message}");
    std::process::exit(1);
}

const I63_MAX: i64 = 0x3fff_ffff_ffff_ffff;
const I63_MIN: i64 = -0x4000_0000_0000_0000;

fn parse_input(input: &str) -> i64 {
    if input == "false" {
        1
    } else if input == "true" {
        3
    } else {
        let res = input.parse::<i64>();
        match res {
            Ok(i) =>
                if i > I63_MAX || i < I63_MIN { snek_error(3) } else { return i << 1 },
            Err(_) => snek_error(2),
        }
        0
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let i: i64 = unsafe { our_code_starts_here(input) };
    if i == 3 {
        println!("true")
    } else if i == 1 {
        println!("false")
    } else if i & 1 == 0 {
        println!("{}", i >> 1)
    } else {
        panic!("invalid output: {}", i)
    }
}
