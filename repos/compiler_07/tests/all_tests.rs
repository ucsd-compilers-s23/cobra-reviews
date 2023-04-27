mod infra;

// Your tests go here!
success_tests! {
    {
        name: false_val,
        file: "false_val",
        expected: "false",
    },
    {
        name: input_compare_1,
        file: "input_compare",
        input: "2",
        expected: "false",
    },
    {
        name: input_compare_2,
        file: "input_compare",
        input: "10",
        expected: "true",
    },
    {
        name: add,
        file: "add",
        expected: "15",
    },
    {
        name: let_test,
        file: "let",
        input: "7",
        expected: "4",
    },
    {
        name: set,
        file: "set",
        expected: "6",
    },
    {
        name: block,
        file: "block",
        expected: "6",
    },
    {
        name: complex,
        file: "complex",
        expected: "-6",
    },
    {
        name: factorial,
        file: "factorial",
        input: "7",
        expected: "5040",
    },
}

runtime_error_tests! {
    {
        name: invalid_argument,
        file: "invalid_argument",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_2,
        file: "invalid_argument_2",
        expected: "invalid argument",
    },
    {
        name: input_compare_3,
        file: "input_compare",
        input: "true",
        expected: "invalid argument",
    },
    {
        name: let_test_invalid,
        file: "let",
        expected: "invalid argument",
    },
    {
        name: overflow,
        file: "overflow",
        expected: "overflow",
    },
    {
        name: overflow_mult,
        file: "overflow_mult",
        expected: "overflow",
    },
}

static_error_tests! {
    {
        name: number_bounds_fail,
        file: "number_bounds_fail",
        expected: "invalid",
    },
    {
        name: set_unbound,
        file: "set_unbound",
        expected: "Unbound",
    },
    {
        name: block_no_args,
        file: "set_unbound",
        expected: "",
    }
}
