use std::env;

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: i64) -> i64;
}

const I63_MAX: i64 = 2_i64.pow(62) - 1;
const I63_MIN: i64 = -2_i64.pow(62);

fn i63_overflow(n: i64) -> bool {
    n > I63_MAX || n < I63_MIN
}

#[no_mangle]
#[export_name = "\x01snek_error"]
pub extern "C" fn snek_error(errcode: i64) {
    // TODO: print error message according to writeup
    
    eprintln!("An error ocurred {}", match errcode {
        1 => "overflow",
        2 => "invalid argument",
        _ => "unknown error code",
    });
    std::process::exit(1);
}

fn parse_input(v : &Vec<String>) -> i64 {
    if v.len() < 2 { return 1 }
    let s = &v[1];
    if s == "true" { 3 }
    else if s == "false" { 1 }
    else { 
        let n = s.parse::<i64>().expect("input overflow");
        if i63_overflow(n) {
            panic!("input overflow")
        } else {
            n << 1
        }
    }
  }

fn print_value(i: i64) {
    if (i & 1) == 0 {
        println!("{}", i >> 1)
    } else if i == 3 {
        println!("true");
    } else if i == 1 {
        println!("false");
    } else {
        eprintln!("Unknown value: {}", i);
        std::process::exit(1);
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = parse_input(&args);

    let i: i64 = unsafe { our_code_starts_here(input) };
    print_value(i);
}
