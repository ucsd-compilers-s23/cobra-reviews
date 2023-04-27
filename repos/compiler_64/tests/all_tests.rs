mod infra;

// Your tests go here!
success_tests! {
    {
        name: false_val,
        file: "false_val.snek",
        expected: "false",
    },
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
    {
        name: add1,
        file: "add1.snek",
        expected: "73",
    },
    {
        name: binding,
        file: "binding.snek",
        expected: "5",
    },
    {
        name: nested,
        file: "nested_arith.snek",
        expected: "1793",
    },
    {
        name: add,
        file: "add.snek",
        expected: "15",
    },
    {
        name: cond,
        file: "cond.snek",
        expected: "false",
    },
    {
        name: lets,
        file: "lettest.snek",
        expected: "21",
    },
}

runtime_error_tests! {
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
}

static_error_tests! {
    {
        name: number_bounds_fail,
        file: "number_bounds_fail.snek",
        expected: "Invalid",
    }
}
