pub fn is_valid_number(num: i64) -> bool {
    let lower_bound: i64 = -(1 << 62);
    let upper_bound: i64 = (1 << 62) - 1;

    if num < lower_bound || num > upper_bound {
        false
    } else {
        true
    }
}

pub fn is_valid_variable_name(name: &String) -> bool {
    let reserved_keywords = vec!["true", "false", "input",
                                            "add1", "sub1", "isnum", "isbool",
                                            "+", "-", "*", "<", "<=", ">", ">=", "=",
                                            "let", "set!", "if", "block", "loop", "break"];

    if name.len() < 1 {
        return false;
    }

    // contains alphanumeric, _, or -
    if !name.chars().all(|x| x.is_alphanumeric() || x == '_' || x == '-') {
        return false;
    }

    // starts with letter
    if !name.chars().next().unwrap().is_alphabetic() {
        return false;
    }

    // contains reserved keyword
    if reserved_keywords.contains(&name.as_str()) {
        return false;
    }

    true
}

pub fn new_label(label: &str, label_count: &mut i32) -> String {
    let l = format!("{}_{}", label, *label_count);
    *label_count = *label_count + 1;

    l
}