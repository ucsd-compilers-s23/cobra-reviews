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
        name: let_and_set,
        file: "let_and_set.snek",
        expected: "6",
    },
    {
        name: loop_1,
        file: "loop_1.snek",
        expected: "-6",
    },
    {
        name: input_loop_1,
        file: "input_loop.snek",
        input: "-1",
        expected: "1",
    },
    {
        name: input_loop_2,
        file: "input_loop.snek",
        input: "4",
        expected: "24",
    },
    {
        name: condition,
        file: "condition.snek",
        expected: "true"
    },
    {
        name: condition_2,
        file: "condition_2.snek",
        expected: "false"
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
    {
        name: overflow_add1,
        file: "overflow_add1.snek",
        expected: "overflow",
    },
    {
        name: condition_1,
        file: "condition_1.snek",
        expected: "invalid argument",
    },
}

static_error_tests! {
    {
        name: number_bounds_fail,
        file: "number_bounds_fail.snek",
        expected: "Invalid",
    },
    {
        name: block_fail,
        file: "block_fail.snek",
        expected: "Invalid",
    },
}
