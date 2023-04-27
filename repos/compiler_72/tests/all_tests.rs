mod infra;

// Your tests go here!
success_tests! {
    {
        name: false_val,
        file: "false_val.snek",
        expected: "false",
    },
    {
        name: min_num,
        file: "min_num.snek",
        expected: "-4611686018427387903",
    },
    {
        name: input_compare_1,
        file: "input_compare.snek",
        input: "2",
        expected: "false",
    },
    {
        name: fraction_1,
        file: "fraction.snek",
        input: "2",
        expected: "2",
    },
    {
        name: input_compare_2,
        file: "input_compare.snek",
        input: "10",
        expected: "true",
    },
    {
        name: large_loop,
        file: "large_loop.snek",
        expected: "6",
    },
    {
        name: isnum_1,
        file: "isnum.snek",
        input:"3",
        expected: "true",
    },
    {
        name: isnum_2,
        file: "isnum.snek",
        input:"false",
        expected: "false",
    },
    {
        name: isbool_1,
        file: "isbool.snek",
        input:"3",
        expected: "false",
    },
    {
        name: isbool_2,
        file: "isbool.snek",
        input:"false",
        expected: "true",
    },
    {
        name: large_loop_sub,
        file: "large_loop_sub.snek",
        expected: "-6",
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
        name: input_bounds_fail_1,
        file: "unbound_input.snek",
        input: "4611686018427387904",
        expected: "Invalid",
    },
    {
        name: input_bounds_fail_2,
        file: "unbound_input.snek",
        input: "-4611686018427387905",
        expected: "Invalid",
    }, 
    {
        name: min_num_fail,
        file: "min_num_fail.snek",
        expected: "overflow",
    },
    {
        name: add1_bool,
        file: "add1_bool.snek",
        expected: "invalid argument",
    },
    {
        name: sub1_bool,
        file: "sub1_bool.snek",
        expected: "invalid argument",
    },
    {
        name: comp_bool_1,
        file: "com_bool.snek",
        expected: "invalid argument",
    },
    {
        name: comp_bool_2,
        file: "com_bool2.snek",
        expected: "invalid argument",
    },
    {
        name: comp_bool_3,
        file: "com_bool3.snek",
        expected: "invalid argument",
    },
    {
        name: plus_bool,
        file: "plus_bool.snek",
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
        name: unbound_fail,
        file: "unbound.snek",
        expected: "Unbound variable identifier x",
    }
}
