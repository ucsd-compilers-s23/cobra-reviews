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
        name: nested1,
        file: "nested1.snek",
        expected: "1793",
    },
    {
        name: nested2,
        file: "nested2.snek",
        expected: "-90",
    },
    {
        name: nested3,
        file: "nested3.snek",
        expected: "25",
    },
    {
        name: nested4,
        file: "nested4.snek",
        expected: "40",
    },
    {
        name: mul1,
        file: "mul1.snek",
        expected: "-4611686018427387904",
    },
    {
        name: mul2,
        file: "mul2.snek",
        expected: "4611686018427387903",
    },
    {
        name: loop1,
        file: "loop1.snek",
        expected: "-6",
    },
    {
        name: block1,
        file: "block1.snek",
        expected: "6",
    },
    {
        name: factorial1,
        file: "factorial.snek",
        input: "10",
        expected: "3628800",
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
        name: overflow1,
        file: "overflow1.snek",
        expected: "overflow",
    },
}

static_error_tests! {
    {
        name: number_bounds_fail,
        file: "number_bounds_fail.snek",
        expected: "Invalid",
    }
}
