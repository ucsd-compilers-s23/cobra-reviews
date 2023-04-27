mod infra;

// Your tests go here!
success_tests! {
    {
        name: true_val,
        file: "true_val.snek",
        expected: "true",
    },
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
        name: is_num_0,
        file: "is_num_0.snek",
        expected: "true",
    },
    {
        name: is_num_1,
        file: "is_num_1.snek",
        expected: "true",
    },
    {
        name: is_num_3,
        file: "is_num_1.snek",
        expected: "true",
    },
    {
        name: is_num_99,
        file: "is_num_99.snek",
        expected: "true",
    },
    {
        name: is_num_false,
        file: "is_num_false.snek",
        expected: "false",
    },
    {
        name: is_num_true,
        file: "is_num_true.snek",
        expected: "false",
    },
    // {
    //     name: close_bound_pos,
    //     file: "close_bound_pos.snek",
    //     expected: "4611686018427387903",
    // },
    // {
    //     name: close_bound_neg,
    //     file: "close_bound_neg.snek",
    //     expected: "-4611686018427387904",
    // },
    {
        name: negative_one,
        file: "negative_one.snek",
        expected: "-1"
    },
    {
        name: equal_1,
        file: "equal_1.snek",
        expected: "true",
    },
    {
        name: equal_0,
        file: "equal_0.snek",
        expected: "true",
    },
    {
        name: equal_1_0,
        file: "equal_1_0.snek",
        expected: "false",
    },
    {
        name: equal_n1_0,
        file: "equal_n1_0.snek",
        expected: "false",
    },
    {
        name: equal_true_true,
        file: "equal_true_true.snek",
        expected: "true",
    },
    {
        name: equal_false_false,
        file: "equal_false_false.snek",
        expected: "true",
    },
    {
        name: equal_true_false,
        file: "equal_true_false.snek",
        expected: "false",
    },
    {
        name: equal_false_true,
        file: "equal_false_true.snek",
        expected: "false",
    },
    {
        name: g_2_1,
        file: "g_2_1.snek",
        expected: "true",
    },
    {
        name: g_1_2,
        file: "g_1_2.snek",
        expected: "false",
    },
    {
        name: g_1_1,
        file: "g_1_1.snek",
        expected: "false",
    },
    {
        name: g_0_0,
        file: "g_0_0.snek",
        expected: "false",
    },
    {
        name: g_n1_n5,
        file: "g_n1_n5.snek",
        expected: "true",
    },
    {
        name: g_10_n10,
        file: "g_10_n10.snek",
        expected: "true",
    },
    {
        name: ge_2_1,
        file: "ge_2_1.snek",
        expected: "true",
    },
    {
        name: ge_1_2,
        file: "ge_1_2.snek",
        expected: "false",
    },
    {
        name: ge_1_1,
        file: "ge_1_1.snek",
        expected: "true",
    },
    {
        name: ge_0_0,
        file: "ge_0_0.snek",
        expected: "true",
    },
    {
        name: ge_n1_n5,
        file: "ge_n1_n5.snek",
        expected: "true",
    },
    {
        name: ge_10_n10,
        file: "ge_10_n10.snek",
        expected: "true",
    },
    {
        name: l_2_1,
        file: "l_2_1.snek",
        expected: "false",
    },
    {
        name: l_1_2,
        file: "l_1_2.snek",
        expected: "true",
    },
    {
        name: l_1_1,
        file: "l_1_1.snek",
        expected: "false",
    },
    {
        name: l_0_0,
        file: "l_0_0.snek",
        expected: "false",
    },
    {
        name: l_n1_n5,
        file: "l_n1_n5.snek",
        expected: "false",
    },
    {
        name: l_10_n10,
        file: "l_10_n10.snek",
        expected: "false",
    },
    {
        name: le_2_1,
        file: "le_2_1.snek",
        expected: "false",
    },
    {
        name: le_1_2,
        file: "le_1_2.snek",
        expected: "true",
    },
    {
        name: le_1_1,
        file: "le_1_1.snek",
        expected: "true",
    },
    {
        name: le_0_0,
        file: "le_0_0.snek",
        expected: "true",
    },
    {
        name: le_n1_n5,
        file: "le_n1_n5.snek",
        expected: "false",
    },
    {
        name: le_10_n10,
        file: "le_10_n10.snek",
        expected: "false",
    },
    {
        name: if_true,
        file: "if_true.snek",
        expected: "4",
    },
    {
        name: if_false,
        file: "if_false.snek",
        expected: "5",
    },
    {
        name: if_1,
        file: "if_1.snek",
        expected: "4",
    },
    {
        name: if_10,
        file: "if_10.snek",
        expected: "4",
    },
    {
        name: if_n6,
        file: "if_n6.snek",
        expected: "4",
    },
    {
        name: if_0,
        file: "if_0.snek",
        expected: "4",
    },
    {
        name: loop_break_1,
        file: "loop_break_1.snek",
        expected: "1",
    },
    {
        name: count_to_10,
        file: "count_to_10.snek",
        expected: "10",
    },
    {
        name: add1_4,
        file: "add1_4.snek",
        expected: "5",
    },
    {
        name: add1_x,
        file: "add1_x.snek",
        expected: "1",
    },
    {
        name: add1_0,
        file: "add1_0.snek",
        expected: "1",
    },
    {
        name: sub1_1,
        file: "sub1_1.snek",
        expected: "0",
    },
    {
        name: sub1_0,
        file: "sub1_0.snek",
        expected: "-1",
    },
    {
        name: setx_6,
        file: "setx_6.snek",
        expected: "6",
    },
    {
        name: n6,
        file: "n6.snek",
        expected: "-6",
    },
    {
        name: input,
        file: "input.snek",
        input: "100",
        expected: "100",
    },
    {
        name: input_factorial,
        file: "input_factorial.snek",
        input: "5",
        expected: "120",
    },
    {
        name: let_4,
        file: "4_let.snek",
        expected: "110",
    },
    {
        name: plus_input_input_5,
        file: "plus_input_input.snek",
        input: "5",
        expected: "10",
    },
    {
        name: incr_x,
        file: "incr_x.snek",
        expected: "2",
    },
}

runtime_error_tests! {
    {
        name: invalid_argument,
        file: "invalid_argument.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_mul,
        file: "invalid_mul.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_sub,
        file: "invalid_sub.snek",
        expected: "invalid argument",
    },
    {
        name: input_compare_3,
        file: "input_compare.snek",
        input: "true",
        expected: "invalid argument",
    },
    {
        name: equal_true_1,
        file: "equal_true_1.snek",
        expected: "invalid argument",
    },
    {
        name: g_true_0,
        file: "g_true_0.snek",
        expected: "invalid argument",
    },
    {
        name: g_1_false,
        file: "g_1_false.snek",
        expected: "invalid argument",
    },
    {
        name: add1_false,
        file: "add1_false.snek",
        expected: "invalid argument",
    },
    {
        name: sub1_true,
        file: "sub1_true.snek",
        expected: "invalid argument",
    },
    {
        name: plus_input_input,
        file: "plus_input_input.snek",
        expected: "invalid argument",
    },
    {
        name: plus_input_input_input,
        file: "plus_input_input.snek",
        input: "input",
        expected: "invalid argument",
    },
    // {
    //     name: close_bound_pos_fail,
    //     file: "close_bound_pos_fail.snek",
    //     expected: "overflow",
    // },
    // {
    //     name: close_bound_neg_fail,
    //     file: "close_bound_neg_fail.snek",
    //     expected: "overflow",
    // },
    {
        name: input_overflow,
        file: "input_overflow.snek",
        input: "4611686018427387904",
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
        name: is_num_overflow,
        file: "is_num_overflow.snek",
        expected: "Invalid",
    },
    {
        name: break_outside,
        file: "break_outside.snek",
        expected: "break",
    },
    {
        name: unbound,
        file: "unbound.snek",
        expected: "Unbound variable identifier y",
    },
    {
        name: unbound_set,
        file: "unbound_set.snek",
        expected: "Unbound variable identifier x",
    },
    {
        name: empty_block,
        file: "empty_block.snek",
        expected: "Invalid",
    }
}
