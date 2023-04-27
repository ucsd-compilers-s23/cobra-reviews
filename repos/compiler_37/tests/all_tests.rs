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
        name: add,
        file: "old_tests/add.snek",
        expected: "15",
    },
    {
        name: add1,
        file: "old_tests/add1.snek",
        expected: "73",
    },
    {
        name: sub1,
        file: "old_tests/sub1.snek",
        expected: "71",
    },
    {
        name: num,
        file: "old_tests/num.snek",
        expected: "5",
    },
    {
        name: add1sub1,
        file: "old_tests/add1sub1.snek",
        expected: "9",
    },
    {
        name: nested_arith,
        file: "old_tests/nested_arith.snek",
        expected: "25",
    },
    {
        name: binding,
        file: "old_tests/binding.snek",
        expected: "5",
    },
    {
        name: sub,
        file: "old_tests/sub.snek",
        expected: "-5",
    },
    {
        name: mul,
        file: "old_tests/mul.snek",
        expected: "27",
    },
    {
        name: let1,
        file: "old_tests/let1.snek",
        expected: "6",
    },
    {
        name: let2,
        file: "old_tests/let2.snek",
        expected: "6",
    },
    {
        name: nested_add,
        file: "old_tests/nested_add.snek",
        expected: "555",
    },
    {
        name: nested_add_2,
        file: "old_tests/nested_add_2.snek",
        expected: "555",
    },
    {
        name: nested_binding,
        file: "old_tests/nested_binding.snek",
        expected: "7",
    },
    {
        name: let3,
        file: "old_tests/let3.snek",
        expected: "6",
    },
    {
        name: nested_bind2,
        file: "old_tests/nested_bind2.snek",
        expected: "4",
    },
    {
        name: chain_bind,
        file: "old_tests/chain_bind.snek",
        expected: "111",
    },
    {
        name: nested_arith_2,
        file: "old_tests/nested_arith_2.snek",
        expected: "-445",
    },
    {
        name: nested_arith_3,
        file: "old_tests/nested_arith_3.snek",
        expected: "545",
    },
    {
        name: repeat_bind,
        file: "old_tests/repeat_bind.snek",
        expected: "5",
    },
    {
        name: nested_arith_4,
        file: "old_tests/nested_arith_4.snek",
        expected: "0",
    },
    {
        name: add1_let,
        file: "old_tests/add1_let.snek",
        expected: "6",
    },
    {
        name: let_add1_let,
        file: "old_tests/let_add1_let.snek",
        expected: "6",
    },
    {
        name: true_val,
        file: "true_val.snek",
        expected: "true",
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
        name: input_add,
        file: "input_add.snek",
        input: "4",
        expected: "9",
    },
    {
        name: equal_op_bool_true,
        file: "equal_op_bool_true.snek",
        expected: "true",
    },
    {
        name: equal_op_num_true,
        file: "equal_op_num_true.snek",
        expected: "true",
    },
        {
        name: greater_true,
        file: "greater/true.snek",
        expected: "true",
    },
    {
        name: greater_false,
        file: "greater/false.snek",
        expected: "false",
    },
    {
        name: greater_equal_true,
        file: "greater_equal/true.snek",
        expected: "true",
    },
    {
        name: greater_equal_false,
        file: "greater_equal/false.snek",
        expected: "false",
    },
    {
        name: if_cond_true,
        file: "if/if.snek",
        expected: "1",
    },
    {
        name: if_cond_false,
        file: "if/if2.snek",
        expected: "0",
    },
    {
        name: neg_num,
        file: "neg_num.snek",
        expected: "-5",
    },
    {
        name: complex,
        file: "complex.snek",
        expected: "55",
    },
    {
        name: simple_loop,
        file: "simple_loop.snek",
        expected: "5",
    },
    {
        name: loop1,
        file: "loop1.snek",
        expected: "-6",
    },
    {
        name: factorial_loop,
        file: "factorial_loop.snek",
        input: "5",
        expected: "120",
    },
    {
        name: greater_than_equal_2,
        file: "greater_equal/true2.snek",
        expected: "true",
    },
    {
        name: less_equal_true,
        file: "less_equal/true.snek",
        expected: "true",
    },
    {
        name: less_equal_true2,
        file: "less_equal/true2.snek",
        expected: "true",
    },
    {
        name: less_equal_false,
        file: "less_equal/false.snek",
        expected: "false",
    },
    {
        name: less_true,
        file: "less/true.snek",
        expected: "true",
    },
    {
        name: less_false,
        file: "less/false.snek",
        expected: "false",
    },
    {
        name: less_false2,
        file: "less/false2.snek",
        expected: "false",
    },   
    {
        name: if_complex,
        file: "if/if_complex.snek",
        expected: "true",
    },
    {
        name: if_num,
        file: "if/if_num.snek",
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
        name: overflow_add1,
        file: "overflow_add.snek",
        expected: "overflow",
    },
    {
        name: overflow_add2,
        file: "overflow_add2.snek",
        expected: "overflow",
    },
    {
        name: overflow_sub1,
        file: "overflow_sub.snek",
        expected: "overflow",
    },

    {
        name: overflow_mult,
        file: "overflow_mult.snek",
        expected: "overflow",
    },
    {
        name: equal_op_bool_false,
        file: "equal_op_bool_false.snek",
        expected: "invalid argument",
    },
    {
        name: equal_op_num_false,
        file: "equal_op_num_false.snek",
        expected: "invalid argument",
    },
    {
        name: greater_error,
        file: "greater/error.snek",
        expected: "invalid argument",
    },
    {
        name: greater_equal_error,
        file: "greater_equal/error.snek",
        expected: "invalid argument",
    },
    {
        name: less_error,
        file: "less_equal/error.snek",
        expected: "invalid argument",
    },
    {
        name: less,
        file: "less/error.snek",
        expected: "invalid argument",
    },
    {
        name: overflow_sub2,
        file: "overflow_sub2.snek",
        expected: "overflow",
    },
    {
        name: bad_arg_add1,
        file: "bad_arg_add1.snek",
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
        name: number_bounds_fail1,
        file: "number_bounds_fail1.snek",
        expected: "Invalid",
    },
    {
        name: unbound_id,
        file: "old_tests/unbound_id.snek",
        expected: "Error - Unbound variable identifier x",
    },
    {
        name: duplicate_binding,
        file: "old_tests/duplicate_binding.snek",
        expected: "Duplicate binding",
    },
    {
        name: invalid_plus,
        file: "old_tests/invalid_plus.snek",
        expected: "Invalid",
    },
    {
        name: invalid_let_body,
        file: "old_tests/invalid_let_body.snek",
        expected: "Invalid",
    },
    {
        name: empty,
        file: "old_tests/empty.snek",
        expected: "Invalid",
    },
    {
        name: improper_binding,
        file: "old_tests/improper_binding.snek",
        expected: "Invalid",
    },
    {
        name: no_let_binding,
        file: "old_tests/no_let_binding.snek",
        expected: "Invalid",
    },
    {
        name: reserved_words,
        file: "reserved_word.snek",
        expected: "keyword",
    },
    {
        name: break_bad,
        file: "break_bad.snek",
        expected: "break",
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
    },
}
