mod infra;

// Your tests go here!
success_tests! {
    {
        name: input_add_1,
        file: "input_add_1.snek",
        input: "4",
        expected: "5",
    },
    {
        name: false_val,
        file: "false_val.snek",
        expected: "false",
    },
    {
        name: compare_g_false,
        file: "compare_g_false.snek",
        expected: "false",
    },
    {
        name: compare_g_true,
        file: "compare_g_true.snek",
        expected: "true",
    },
    {
        name: compare_ge_false,
        file: "compare_ge_false.snek",
        expected: "false",
    },
    {
        name: compare_ge_true,
        file: "compare_ge_true.snek",
        expected: "true",
    },
    {
        name: compare_l_false,
        file: "compare_l_false.snek",
        expected: "false",
    },
    {
        name: compare_l_true,
        file: "compare_l_true.snek",
        expected: "true",
    },
    {
        name: compare_le_false,
        file: "compare_le_false.snek",
        expected: "false",
    },
    {
        name: compare_le_true,
        file: "compare_le_true.snek",
        expected: "true",
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
        name: loop_simple,
        file: "loop_simple.snek",
        expected: "2",
    },
    {
        name: block_set,
        file: "block_set.snek",
        expected: "12",
    },
    {
        name: is_num_true,
        file: "is_num_true.snek",
        expected: "true",
    },
    {
        name: is_num_false,
        file: "is_num_false.snek",
        expected: "false",
    },
    {
        name: is_bool_true,
        file: "is_bool_true.snek",
        expected: "true",
    },
    {
        name: is_bool_false,
        file: "is_bool_false.snek",
        expected: "false",
    },
    {
        name: input_valid,
        file: "input.snek",
        input: "10",
        expected: "10",
    },
    {
        name: input_negative,
        file: "input.snek",
        input: "-10",
        expected: "-10",
    },
    {
        name: if_true,
        file: "if_true.snek",
        expected: "true",
    },
    {
        name: if_false,
        file: "if_false.snek",
        expected: "false",
    },
    {
        name: if_compare_bool_true,
        file: "if_compare_bool_true.snek",
        expected: "true",
    },
    {
        name: if_compare_bool_false,
        file: "if_compare_bool_false.snek",
        expected: "false",
    },
    {
        name: if_num,
        file: "if_num.snek",
        expected: "1",
    },
    {
        name: loop_eg,
        file: "loop_eg.snek",
        expected: "-6",
    },
    {
        name: if_num_2,
        file: "if_num_2.snek",
        expected: "true",
    },
    {
        name: multiply,
        file: "mult.snek",
        input: "1537228672809129301",
        expected: "4611686018427387903",
    },
    {
        name: multiply2,
        file: "mult.snek",
        input: "1537228672809129300",
        expected: "4611686018427387900",
    },
    {
        name: multiply3,
        file: "mult.snek",
        input: "-1537228672809129301",
        expected: "-4611686018427387903",
    },
    {
        name: multiply_neg,
        file: "mult.snek",
        input: "-4",
        expected: "-12",
    },
    {
        name: multiply_neg_neg,
        file: "mult_neg.snek",
        input: "-5",
        expected: "20",
    },
    {
        name: evaluation_order,
        file: "evaluation_order.snek",
        expected: "3",
    }
}

runtime_error_tests! {
    {
        name: invalid_argument,
        file: "invalid_argument.snek",
        expected: "invalid argument",
    },
    {
        name: compare_invalid,
        file: "compare_invalid.snek",
        input: "true",
        expected: "invalid argument",
    },
    {
        name: compare_invalid_eq,
        file: "compare_invalid_eq.snek",
        expected: "invalid argument",
    },
    {
        name: input_invalid,
        file: "input.snek",
        input: "10a",
        expected: "Invalid",
    },
    {
        name: input_invalid_overflow,
        file: "input.snek",
        input: "36893488147419103000",
        expected: "Invalid",
    },
    {
        name: input_overflow,
        file: "input.snek",
        input: "9223372036854775807",
        expected: "Invalid",
    },
    {
        name: input_overflow_2,
        file: "input.snek",
        input: "-4611686018427387905",
        expected: "Invalid",
    },
    {
        name: invalid_if_condition,
        file: "invalid_if_condition.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_type_add,
        file: "invalid_type_add.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_type_sub,
        file: "invalid_type_sub.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_type_mult,
        file: "invalid_type_mult.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_type_add1,
        file: "invalid_type_add1.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_type_sub1,
        file: "invalid_type_sub1.snek",
        expected: "invalid argument",
    },
    {
        name: overflow_add,
        file: "overflow_add.snek",
        expected: "overflow",
    },
    {
        name: overflow_sub,
        file: "overflow_sub.snek",
        expected: "overflow",
    },
    {
        name: overflow_mult,
        file: "overflow_mult.snek",
        expected: "overflow",
    },
    {
        name: overflow_add1,
        file: "overflow_add1.snek",
        expected: "overflow",
    },
    {
        name: overflow_sub1,
        file: "overflow_sub1.snek",
        expected: "overflow",
    },
    {
        name: mult_error,
        file: "mult.snek",
        input: "1537228672809129302",
        expected: "overflow",
    },
    {
        name: multiply_neg_error_2,
        file: "mult.snek",
        input: "-1537228672809129302",
        expected: "overflow",
    },
}

static_error_tests! {
    {
        name: block_parse_fail,
        file: "block_parse_fail.snek",
        expected: "Invalid",
    },
    {
        name: number_bounds_fail,
        file: "number_bounds_fail.snek",
        expected: "Invalid",
    },
    {
        name: sexpr_parse_err,
        file: "sexpr_parse_err.snek",
        expected: "Invalid",
    },
    {
        name: invalid_binop,
        file: "invalid_binop.snek",
        expected: "Invalid",
    },
    {
        name: invalid_binop2,
        file: "invalid_binop2.snek",
        expected: "Invalid",
    },
    {
        name: invalid_binop3,
        file: "invalid_binop3.snek",
        expected: "Invalid",
    },
    {
        name: invalid_unop,
        file: "invalid_unop.snek",
        expected: "Invalid",
    },
    {
        name: duplicate_binding,
        file: "duplicate_binding.snek",
        expected: "Duplicate binding",
    },
    {
        name: break_without_loop,
        file: "break_without_loop.snek",
        expected: "break",
    },
    {
        name: invalid_op,
        file: "invalid_op.snek",
        expected: "Invalid",
    }
}
