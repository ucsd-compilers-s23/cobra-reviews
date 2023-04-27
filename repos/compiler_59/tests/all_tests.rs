mod infra;

// Your tests go here!
success_tests! {
    // Values
    {
        name: num_val,
        file: "num_val.snek",
        expected: "10",
    },
    {
        name: false_val,
        file: "false_val.snek",
        expected: "false",
    },
    // UnOps
    {
        name: add1,
        file: "add1.snek",
        expected: "10",
    },
    {
        name: sub1,
        file: "sub1.snek",
        expected: "9",
    },
    {
        name: isnum_true,
        file: "isnum_true.snek",
        expected: "true",
    },
    {
        name: isnum_false,
        file: "isnum_false.snek",
        expected: "false",
    },
    {
        name: isbool_true,
        file: "isbool_true.snek",
        expected: "true",
    },
    {
        name: isbool_false,
        file: "isbool_false.snek",
        expected: "false",
    },
    // Arithmetic BinOps
    {
        name: plus,
        file: "plus.snek",
        expected: "11",
    },
    {
        name: minus,
        file: "minus.snek",
        expected: "4",
    },
    {
        name: times,
        file: "times.snek",
        expected: "120",
    },
    {
        name: nested_arithmetic,
        file: "nested_arithmetic.snek",
        expected: "53",
    },
    {
        name: unary_in_binary,
        file: "unary_in_binary.snek",
        expected: "32",
    },
    // Comparator BinOps
    {
        name: equal,
        file: "equal.snek",
        expected: "true",
    },
    {
        name: not_equal,
        file: "not_equal.snek",
        expected: "false",
    },
    {
        name: less_than_true,
        file: "less_than_true.snek",
        expected: "true",
    },
    {
        name: less_than_false,
        file: "less_than_false.snek",
        expected: "false",
    },
    {
        name: greater_than_true,
        file: "greater_than_true.snek",
        expected: "true",
    },
    {
        name: greater_than_false,
        file: "greater_than_false.snek",
        expected: "false",
    },
    {
        name: less_than_equal_equal,
        file: "less_than_equal_equal.snek",
        expected: "true",
    },
    {
        name: less_than_equal_true,
        file: "less_than_equal_true.snek",
        expected: "true",
    },
    {
        name: less_than_equal_false,
        file: "less_than_equal_false.snek",
        expected: "false",
    },
    {
        name: greater_than_equal_equal,
        file: "greater_than_equal_equal.snek",
        expected: "true",
    },
    {
        name: greater_than_equal_true,
        file: "greater_than_equal_true.snek",
        expected: "true",
    },
    {
        name: greater_than_equal_false,
        file: "greater_than_equal_false.snek",
        expected: "false",
    },
    // Let bindings
    {
        name: let_single,
        file: "let_single.snek",
        expected: "10",
    },
    {
        name: let_multiple,
        file: "let_multiple.snek",
        expected: "111",
    },
    {
        name: let_nested,
        file: "let_nested.snek",
        expected: "171",
    },
    // Conditionals
    {
        name: if_true,
        file: "if_true.snek",
        expected: "true",
    },
    {
        name: if_false,
        file: "if_false.snek",
        expected: "false",
    },
    // Blocks/sets
    {
        name: block_with_set,
        file: "block_with_set.snek",
        expected: "10",
    },
    // Loop/breaks
    {
        name: loop_break_with_val,
        file: "loop_break_with_val.snek",
        expected: "5",
    },
    // Input
    {
        name: input_compare_1,
        file: "input_compare.snek",
        input: "2",
        expected: "false",
    },
    {
        name: input_compare_2,
        file: "input_compare.snek",
        input: "10",
        expected: "true",
    },
}

runtime_error_tests! {
    {
        name: unbound_break,
        file: "unbound_break.snek",
        expected: "break",
    },
    {
        name: unbound_variable,
        file: "unbound_variable.snek",
        expected: "Unbound variable identifier y",
    },
    {
        name: invalid_argument,
        file: "invalid_argument.snek",
        expected: "invalid argument",
    },
    {
        name: input_compare_3,
        file: "input_compare.snek",
        input: "true",
        expected: "invalid argument",
    },
    {
        name: plus_overflow,
        file: "plus_overflow.snek",
        expected: "overflow",
    },
    {
        name: minus_overflow,
        file: "minus_overflow.snek",
        expected: "overflow",
    },
    {
        name: test_overflow,
        file: "times_overflow.snek",
        expected: "overflow",
    },
}

static_error_tests! {
    {
        name: number_bounds_fail,
        file: "number_bounds_fail.snek",
        expected: "Invalid",
    },
    {
        name: let_nobindings,
        file: "let_nobindings.snek",
        expected: "Invalid",
    },
}
