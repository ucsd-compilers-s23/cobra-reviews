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
        name: add1_sub1_int_input_10,
        file: "add1_sub1_input.snek",
        input: "10",
        expected: "10"
    },
    {
        name: simple_if_then_else,
        file: "simple_if_then_else.snek",
        expected: "500"
    },
    {
        name: factorial_5,
        file: "factorial_from_input.snek",
        input: "5",
        expected: "120"
    },
    {
        name: power_of_2_calculator_7,
        file: "power_of_2_calculator.snek",
        input: "7",
        expected: "128"
    },
    {
        name: complex_set,
        file: "complex_set.snek",
        input: "3",
        expected: "14"
    }
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
        name: add1_sub1_int_input_true,
        file: "add1_sub1_input.snek",
        input: "true",
        expected: "invalid argument"
    },
    {
        name: add1_sub1_int_input_default_false,
        file: "add1_sub1_input.snek",
        expected: "invalid argument"
    },
    {
        name: add1_sub1_int_input_default_overflow_after_one_op,
        file: "add1_sub1_input.snek",
        input: "-4611686018427387904",
        expected: "overflow"
    },
    {
        name: add1_sub1_int_input_default_overflow_from_input,
        file: "add1_sub1_input.snek",
        input: "-4611686018427387905",
        expected: "Invalid"
    },
    {
        name: factorial_1000,
        file: "factorial_from_input.snek",
        input: "1000",
        expected: "overflow"
    },
    {
        name: power_of_2_calculator_1000,
        file: "power_of_2_calculator.snek",
        input: "1000",
        expected: "overflow"
    },
    {
        name: power_of_2_calculator_boolean_input,
        file: "power_of_2_calculator.snek",
        input: "true",
        expected: "invalid"
    }
}

static_error_tests! {
    {
        name: number_bounds_fail,
        file: "number_bounds_fail.snek",
        expected: "Invalid",
    },
    {
        name: invalid_let_binding_input,
        file: "invalid_let_binding_input.snek",
        expected: "keyword"
    },
    {
        name: invalid_set_updating_input,
        file: "invalid_set_updating_input.snek",
        expected: "keyword"
    },
    {
        name: break_outside_loop,
        file: "single_loop_double_break.snek",
        expected: "break"
    },
    {
        name: empty_block,
        file: "empty_block.snek",
        expected: "Invalid"
    }
}
