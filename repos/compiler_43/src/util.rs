/*
Recall, least significant bit will serve as the tag (indicating either bool 1, or int 3)
*/

pub fn encode_bool(_bool: bool) -> i64 {
    let encoded_bool = if _bool { 3 } else { 1 };
    return encoded_bool;
}

pub fn encode_num(num: i64) -> i64 {
    return num << 1;
}

pub fn new_label(s: &str, l: &mut i64) -> String {
    let current = *l;
    *l += 1;
    format!("{s}_{current}")
}