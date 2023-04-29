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
        name: ex1,
        file: "ex1.snek",
        expected: "6",
    },
    {
        name: ex2,
        file: "ex2.snek",
        expected: "-6",
    },
    {
        name: ex3,
        file: "ex3.snek",
        input: "10",
        expected: "3628800",
    },
    {
        name: ex4,
        file: "ex4.snek",
        expected: "1000000",
    },
    {
        name: t2,
        file: "t2.snek",
        expected: "2",
    },
    {
        name: t3,
        file: "t3.snek",
        expected: "24",
    },
    {
        name: t4,
        file: "t4.snek",
        expected: "4611686018427387903",
    },
    {
        name: t8,
        file: "t8.snek",
        expected: "-2",
    },
    {
        name: t12,
        file: "t12.snek",
        expected: "true",
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
        name: t9,
        file: "t9.snek",
        expected: "overflow",
    },
    {
        name: t10,
        file: "t10.snek",
        expected: "overflow",
    },
    {
        name: t11,
        file: "t11.snek",
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
        name: t6,
        file: "t6.snek",
        expected: "Invalid",
    },
    {
        name: t7,
        file: "t7.snek",
        expected: "keyword",
    },
}
