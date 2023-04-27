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
        name: my_1,
        file: "my1.snek",
        expected: "15",
    },
    {
        name: my_2,
        file: "my2.snek",
        expected: "false",
    },
    {
        name: my_4_1,
        file: "my4.snek",
        input: "4",
        expected: "24",
    },
    {
        name: my_8_1,
        file: "my8.snek",
        input: "1",
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
        name: my_3,
        file: "my3.snek",
        expected: "overflow",
    },
    {
        name: my_4_2,
        file: "my4.snek",
        input: "1000",
        expected: "overflow",
    },
    {
        name: my_8_2,
        file: "my8.snek",
        input: "true",
        expected: "invalid argument",
    },
    {
        name: my_8_3,
        file: "my8.snek",
        input: "input",
        expected: "",
    },
}

static_error_tests! {
    {
        name: number_bounds_fail,
        file: "number_bounds_fail.snek",
        expected: "Invalid",
    },
    {
        name: my_5,
        file: "my5.snek",
        expected: "Invalid",
    },
    {
        name: my_6,
        file: "my6.snek",
        expected: "break",
    },
    {
        name: my_7,
        file: "my7.snek",
        expected: "break",
    },
}
