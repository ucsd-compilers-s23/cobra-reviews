mod infra;

// Your tests go here!
success_tests! {
    {name: false_val, file: "false_val.snek", expected: "false"},
    {name: input_compare_1, file: "input_compare.snek", input: "2", expected: "false"},
    {name: input_compare_2, file: "input_compare.snek", input: "10", expected: "true"},
    // My tests
    // All comparison tests are of the form (<cmp> 5 input)
    {name: g_greater, file: "greater.snek", input: "4", expected: "true"},
    {name: g_equal, file: "greater.snek", input: "5", expected: "false"},
    {name: g_lessthan, file: "greater.snek", input: "6", expected: "false"},
    {name: ge_greater, file: "greater_equal.snek", input: "4", expected: "true"},
    {name: ge_equal, file: "greater_equal.snek", input: "5", expected: "true"},
    {name: ge_lessthan, file: "greater_equal.snek", input: "6", expected: "false"},
    {name: l_greater, file: "less.snek", input: "4", expected: "false"},
    {name: l_equal, file: "less.snek", input: "5", expected: "false"},
    {name: l_lessthan, file: "less.snek", input: "6", expected: "true"},
    {name: le_greater, file: "less_equal.snek", input: "4", expected: "false"},
    {name: le_equal, file: "less_equal.snek", input: "5", expected: "true"},
    {name: le_lessthan, file: "less_equal.snek", input: "6", expected: "true"},
    {name: eq_5_equal, file: "equals_5.snek", input: "5", expected: "true"},
    {name: eq_5_unequal, file: "equals_5.snek", input: "6", expected: "false"},
    {name: eq_true_equal, file: "equals_true.snek", input: "true", expected: "true"},
    {name: eq_true_unequal, file: "equals_true.snek", input: "false", expected: "false"},
    // If
    {name: if_true, file: "if.snek", input: "true", expected: "true"},
    {name: if_number, file: "if.snek", input: "5", expected: "true"},
    {name: if_else, file: "if.snek", input: "false", expected: "false"},
    // Block
    {name: block, file: "block.snek", expected: "6"},
    // isnum
    {name: isnum_true, file: "isnum.snek", input: "5", expected: "true"},
    {name: isnum_false, file: "isnum.snek", input: "true", expected: "false"},
    // isbool
    {name: isbool_true, file: "isbool.snek", input: "true", expected: "true"},
    {name: isbool_false, file: "isbool.snek", input: "5", expected: "false"},
    // Set
    {name: set, file: "set.snek", expected: "6"},
    // Break
    {name: break_1, file: "break.snek", expected: "5"},
    // Times
    {name: times, file: "times.snek", expected: "50"},
}

runtime_error_tests! {
    {name: invalid_argument, file: "invalid_argument.snek", expected: "invalid argument"},
    {name: input_compare_3, file: "input_compare.snek", input: "true", expected: "invalid argument"},
    // Comparators
    {name: comparator_boolean, file: "greater.snek", input: "true", expected: "invalid argument"},
    // Equals
    {name: eq_5_boolean, file: "equals_5.snek", input: "true", expected: "invalid argument"},
    {name: eq_true_boolean, file: "equals_true.snek", input: "5", expected: "invalid argument"},
    // Overflow
    {name: overflow, file: "overflow_times.snek", expected: "overflow"},
    // Add1 bool
    {name: add1_bool, file: "add1_bool.snek", expected: "invalid argument"},
}

static_error_tests! {
    {name: number_bounds_fail, file: "number_bounds_fail.snek", expected: "Invalid"},
    // Unbound id
    {name: unbound_id, file: "unbound_id.snek", expected: "Unbound variable identifier x"},
    // Set unbound id
    {name: set_unbound_id, file: "set_unbound_id.snek", expected: "Unbound variable identifier y"},
    // Break outside loop
    {name: break_outside_loop, file: "break_outside_loop.snek", expected: "Break outside of loop"},
    // Let no bindings
    {name: let_nobindings, file: "let_nobindings.snek", expected: "Invalid"},
    // Block no expressions
    {name: parse_block_fail, file: "parse_block_fail.snek", expected: "Invalid"},
}
