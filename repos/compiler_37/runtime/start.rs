use std::env;

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: u64) -> u64;
}

fn print_value(i:u64){
    if i % 2 == 0 { 
        let val:i64 = i as i64;
        println!("{}", val >> 1) 
    }
    else if i == 3 { println!("true") }
    else if i == 1 { println!("false") }
    else { println!("Unknown: {}",i) }
}

#[export_name = "\x01snek_error"]
pub extern "C" fn snek_error(errcode: i64) {
    let mut err_msg = "";
    if errcode == 5 {
        err_msg = "overflow";
    } else if errcode == 7 {
        err_msg = "invalid argument"
    }
    eprintln!("an error ocurred - {err_msg}");
    std::process::exit(1);
}

fn parse_input(s: &str) -> u64 {
    if s == "true" { 3 }
    else if s == "false" { 1 }
    else { 
        let res = s.parse::<u64>();
        match res {
            Ok(val) => val << 1,
            Err(_) => panic!("Invalid")
        }
    }    
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let i: u64 = unsafe { our_code_starts_here(input) };
    print_value(i)
}
