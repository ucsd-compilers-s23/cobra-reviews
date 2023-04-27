mod infra;

// Your tests go here!
success_tests! {
    {
        name: false_val,
        file: "false_val.snek",
        expected: "false",
    },
    {
        name: addsimple,
        file: "addsimple.snek",
        expected: "5",
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
        name: letcase,
        file: "letcase.snek",
        expected: "102",
    },
    {
        name: arithmetic_op_1,
        file: "arithmeticOp1.snek",
        expected: "-90",
    },
    {
        name: letcase2,
        file: "letcase2.snek",
        expected: "5",
    },
    {
        name: add,
        file: "add.snek",
        expected: "793873926",
    },
    {
        name: normal_set,
        file: "normal_set.snek",
        expected: "6",
    },
    {
        name: shadow_binding_2,
        file: "shadow_binding_2.snek",
        expected: "10",
    },
    {
        name: if_test_equal,
        file: "if_test_equal.snek",
        expected: "4",
    },
    {
        name: check_bool,
        file: "check_bool.snek",
        expected: "true",
    },
    {
        name: check_num,
        file: "check_num.snek",
        expected: "false",
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
        name: overflow_fail,
        file: "overflow_fail.snek",
        expected: "overflow",
    },
    {
        name: invalid_argument_2,
        file: "invalid_argument_2.snek",
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
        name: duplicate_binding,
        file: "duplicateBinding.snek",
        expected: "Duplicate binding",
    },
    {
        name: shadow_binding,
        file: "shadowBinding.snek",
        expected: "Invalid",
    },
    {
        name: parse_error,
        file: "parseError.snek",
        expected: "Invalid",
    },
    {
        name: unbound_identifier,
        file: "unbound_identifier.snek",
        expected: "Unbound variable identifier y",
    },
    {
        name: invalid_break,
        file: "invalid_break.snek",
        expected: "break",
    },
    {
        name: number_boundsfail,
        file: "number_boundsfail.snek",
        expected: "Invalid",
    },
    {
        name: parse_let_no_bindings_fail,
        file: "parse_let_no_bindings_fail.snek",
        expected: "Invalid",
    },
    {
        name: block_length_0,
        file: "block_length_0.snek",
        expected: "Invalid",
    },
    {
        name: break_fails,
        file: "break_fails.snek",
        expected: "break",
    },
}
