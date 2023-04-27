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
        name: input_num,
        file: "input.snek",
        input: "4611686018427387903",
        expected: "4611686018427387903",
    },
    {
        name: input_true,
        file: "input.snek",
        input: "true",
        expected: "true",
    },
    {
        name: input_false,
        file: "input.snek",
        input: "false",
        expected: "false",
    },
    {
        name: number,
        file: "number.snek",
        expected: "233",
    },
    {
        name: equal,
        file: "equal.snek",
        expected: "true",
    },
    {
        name: equal1,
        file: "equal1.snek",
        expected: "false",
    },
    {
        name: equal2,
        file: "equal2.snek",
        expected: "true",
    },
    {
        name: equal3,
        file: "equal3.snek",
        expected: "false",
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
    {
        name: greater_true,
        file: "greater_true.snek",
        expected: "true",
    },
    {
        name: greater_false,
        file: "greater_false.snek",
        expected: "false",
    },
    {
        name: times,
        file: "times.snek",
        expected: "60",
    },
    {
        name: times_negative,
        file: "times_negative.snek",
        expected: "58596",
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
        name: equal_invalid,
        file: "equal_invalid.snek",
        expected: "invalid argument",
    },
    {
        name: add1_invalid,
        file: "add1_invalid.snek",
        expected: "invalid argument",
    },
    {
        name: input_invalid,
        file: "input.snek",
        input: "4611686018427387904",
        expected: "Invalid",
    },
    {
        name: input_invalid1,
        file: "input.snek",
        input: "-4611686018427387905",
        expected: "Invalid",
    },
    {
        name: add1_of,
        file: "add1_of.snek",
        expected: "overflow",
    },
    {
        name: sub1_of,
        file: "sub1_of.snek",
        expected: "overflow",
    },
    {
        name: plus_of,
        file: "plus_of.snek",
        expected: "overflow",
    },
    {
        name: plus_of1,
        file: "plus_of1.snek",
        expected: "overflow",
    },
    {
        name: minus_of,
        file: "minus_of.snek",
        expected: "overflow",
    },
    {
        name: times_of,
        file: "times_of.snek",
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
        name: outside_break,
        file: "outside_break.snek",
        expected: "break",
    },
}
