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
        name: factorial,
        file: "fact.snek",
        input: "10",
        expected: "3628800"
    },
    {
        name: loop1,
        file: "loop1.snek",
        expected: "30"
    },
    {
        name: test1,
        file: "test1.snek",
        expected: "131"
    },
    {
        name: test2,
        file: "test2.snek",
        expected: "30"
    },
    {
        name: test3,
        file: "test3.snek",
        expected: "30"
    },
    {
        name: test4,
        file: "test4.snek",
        expected: "200"
    },
    {
        name: test6,
        file: "test6.snek",
        expected: "13"
    },
    {
        name: test8,
        file: "test8.snek",
        expected: "126720"
    },
    {
        name: test9,
        file: "test9.snek",
        expected: "11000"
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
        name: mult_overflow,
        file: "multoverflow.snek",
        expected: "overflow",
    },
    {
        name: test5,
        file: "test5.snek",
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
        name: test7,
        file: "test7.snek",
        expected: "Invalid",
    },
    {
        name: test10,
        file: "test10.snek",
        expected: "Invalid",
    },
}
