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
        name: basic_mult,
        file: "basic_mult.snek",
        expected: "200",
    },
    {
        name: numeric_compare,
        file: "numeric_compare.snek",
        expected: "false"
    },
    {
        name: basic_compare,
        file: "basic_compare.snek",
        expected: "true"
    },
    {
        name: add1,
        file: "add1.snek",
        expected: "6"
    },
    {
        name: add1sub1,
        file: "add1sub1.snek",
        expected: "771"
    },
    {
        name: is_num_true,
        file: "isNum.snek",
        input: "77",
        expected: "true"
    },
    {
        name: is_num_false,
        file: "isNum.snek",
        input: "true",
        expected: "false"
    },
    {
        name: is_bool_true,
        file: "isBool.snek",
        input: "false",
        expected: "true"
    },
    {
        name: is_bool_false,
        file: "isBool.snek",
        input: "3",
        expected: "false"
    },
    {
        name: factorial_of_5,
        file: "factorial.snek",
        input: "5",
        expected: "120"
    },
    {
        name: test_negative,
        file: "test_negative.snek",
        expected: "-1"
    },
    {
        name: input_no_overflow,
        file: "input_no_overflow.snek",
        input: "4611686018427387903",
        expected: "4611686018427387903",
    },
    {
        name: block_single_statement,
        file: "block_single_statement.snek",
        input: "4",
        expected: "24",
    },
    {
        name: nested_loop,
        file: "nested_loop.snek",
        expected: "-6"
    },
    {
        name: num_compare_num_false,
        file: "all_compare_num.snek",
        input: "7",
        expected: "false",
    },
    {
        name: num_compare_num_true,
        file: "all_compare_num.snek",
        input: "2",
        expected: "true",
    },
    {
        name: fibnoacci_15,
        file: "fibonacci.snek",
        input: "15",
        expected: "377"
    },
    {
        name: fibnoacci_loop_break,
        file: "fibonacci.snek",
        input: "1",
        expected: "0"
    }

}

runtime_error_tests! {
    {
        name: invalid_argument,
        file: "invalid_argument.snek",
        expected: "invalid argument",
    },
    {
        name: test_add1_overflow,
        file: "add1_tests.snek",
        input: "4611686018427387903",
        expected: "Error: overflow occurred",
    },
    {
        name: test_add1_bool,
        file: "add1_tests.snek",
        input: "true",
        expected: "invalid argument",
    },
    {
        name: test_sub1_overflow,
        file: "sub1_tests.snek",
        input: "-4611686018427387904",
        expected: "Error: overflow occurred",
    },
    {
        name: test_sub1_bool,
        file: "sub1_tests.snek",
        input: "false",
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
        file: "overflow_add_err.snek",
        expected: "Error: overflow occurred",
    },
    {
        name: mult_overflow,
        file: "mult_overflow.snek",
        expected: "Error: overflow occurred",
    },
    {
        name: bool_compare_num_true,
        file: "all_compare_num.snek",
        input: "true",
        expected: "Error: invalid argument",
    },
    {
        name: bool_compare_num_false,
        file: "all_compare_num.snek",
        input: "false",
        expected: "Error: invalid argument",
    },
    {
        name: infinite_loop_overflow,
        file: "infinite_loop.snek",
        input: "0",
        expected: "Error: overflow occurred",
    }
}

static_error_tests! {
    {
        name: number_bounds_fail,
        file: "number_bounds_fail.snek",
        expected: "Invalid",
    },
    {
        name: let_fail,
        file: "let_fail.snek",
        expected: "Invalid Identifier: keyword used as identifier"
    },
    {
        name: break_err,
        file: "break_no_loop1.snek",
        expected: "break appears outside any loop"
    },
    {
        name: break_err2,
        file: "break_no_loop2.snek",
        input: "5",
        expected: "break appears outside any loop"
    },
    {
        name: break_err3,
        file: "break_no_loop3.snek",
        expected: "break appears outside any loop"
    },
    {
        name: unbound_set,
        file: "set_unbound.snek",
        expected: "Unbound variable identifier k"
    },
    {
        name: dup_binding,
        file: "dup_binding.snek",
        expected: "Duplicate binding"
    },
    {
        name: dup_binding_loop,
        file: "dup_binding_loop.snek",
        expected: "Duplicate binding"
    }
}
