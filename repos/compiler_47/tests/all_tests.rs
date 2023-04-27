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
        name: is_num,
        file: "is_num.snek",
        expected: "true",
    },
    {
        name: is_num_fail,
        file: "is_num_fail.snek",
        expected: "false",
    },
    {
        name: block_basic,
        file: "block.snek",
        expected: "6",
    },
    {
        name: nested_if,
        file: "nested_if.snek",
        expected: "4",
    },
    {
        name: loop_basic,
        file: "loop.snek",
        input: "3",
        expected: "6",
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
        name: overflow_add,
        file: "add.snek",
        input: "4611686018427387903",
        expected: "overflow error",
    },
}

static_error_tests! {
    {
        name: number_bounds_fail,
        file: "number_bounds_fail.snek",
        expected: "error: overflow for value 4611686018427387904",
    },
    {
        name: number_bounds_fail2,
        file: "number_bounds_fail2.snek",
        expected: "error: overflow for value -4611686018427387905",
    }
}
