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
        name: typecheck1,
        file: "typecheck1.snek",
        expected: "true",
    },
    {
        name: typecheck2,
        file: "typecheck2.snek",
        expected: "false",
    },
    {
        name: typecheck3,
        file: "typecheck3.snek",
        expected: "true",
    },
    {
        name: typecheck4,
        file: "typecheck4.snek",
        expected: "false",
    },
    {
        name: add1,
        file: "add1.snek",
        input: "10",
        expected: "11",
    },
    {
        name: add,
        file: "add.snek",
        input: "10",
        expected: "-12",
    },
    {
        name: add2,
        file: "add2.snek",
        expected: "15",
    },
    {
        name: example1,
        file: "example1.snek",
        expected: "6",
    },
    {
        name: example2,
        file: "example2.snek",
        expected: "-6",
    }
}

runtime_error_tests! {
    {
        name: invalid_argument,
        file: "invalid_argument.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument2,
        file: "invalid_argument2.snek",
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
    {
        name: overflow2,
        file: "overflow2.snek",
        input: "-4611686018427387904",
        expected: "overflow",
    }
}

static_error_tests! {
    {
        name: number_bounds_fail,
        file: "number_bounds_fail.snek",
        expected: "Invalid",
    }
}
