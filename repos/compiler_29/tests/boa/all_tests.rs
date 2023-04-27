mod infra;

// Your tests go here!
success_tests! {
    add1: "73",
    add: "15",
    add_comprehensive: "4",
    sub1: "7",
    sub: "1",
    sub_comprehensive: "0",
    times: "8",
    times_comprehensive: "6",
    nested_arith: "25",
    nested_arith_add: "6",
    nested_arith_sub: "9",
    nested_arith_comp: "5",
    binding: "5",
    let_multiple_bindings: "6",
    let_nested: "20",
    let_expr: "12",
    let_dependency: "21",
    let_succ: "5",
    let_inner: "10",
    let_outer: "7",
}

failure_tests! {
    unbound_id: "Unbound variable identifier x",
    unbound_letid: "Unbound variable identifier galen",
    let_scoping_error: "Unbound variable identifier x",
    duplicate_binding: "Duplicate binding",
    duplicate_binding_space: "Duplicate binding",
    duplicate_binding_nested: "Duplicate binding",
    let_nobinding: "Invalid",
    let_nobody: "Invalid",
    let_emptybinding: "Invalid",
    let_emptybody: "Invalid",
    invalid_basic: "Invalid",
    invalid: "Invalid",
    invalid_add: "Invalid",
    invalid_let: "Invalid",
    empty_paren: "Invalid",
    empty: "Invalid",
    hello_world: "Invalid",

}
