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
    if errcode == 3 {
        eprintln!("invalid argument");
    }
    else if errcode == 7 {
        eprintln!("invalid argument");
    }
    else {
        eprintln!("an error ocurred {errcode}");
    }
    
    std::process::exit(1);
}

fn parse_input(input: &str) -> u64 {

    if input == "true" {
        3
    }
    else if input == "false" {
        1
    } 
    else {
        match input.parse::<u64>() {
            Ok(_n) => {
                if input.parse::<i64>().unwrap() > 4611686018427387903 || input.parse::<i64>().unwrap() < -4611686018427387904 {
                    panic!("overflow")
                }
            input.parse::<u64>().unwrap()<<1},
            Err(_e) => panic!("Invalid")
    }
}
}

fn print_value (i:u64) {
    let is_neg = (i & (1<<63)) != 0;
    if i%2 == 0 {
        if is_neg {
            if ((((!i)<<1)>>2)+1) > 4611686018427387904{
                panic!("overflow")
            }
            println! ("-{}", (((!i)<<1)>>2)+1);
        } else {
            if i >= 4611686018427387903 {
                panic!("overflow")
            }
            println! ("{}", i>>1);
        }
      
    }
    else if i==3 {println!("true");}
    else if i==1 {println!("false");}
    else {println!("Unknown : {}", i);}
  }

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let i: u64 = unsafe { our_code_starts_here(input) };
    print_value(i);
}
