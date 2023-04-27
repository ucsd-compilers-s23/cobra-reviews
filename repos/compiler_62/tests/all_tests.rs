mod infra;

// Your tests go here!
success_tests! {
    { name: boa_add1, file: "boa_add1.snek", expected: "73", },
    { name: boa_add, file: "boa_add.snek", expected: "15", },
    { name: boa_nested_arith, file: "boa_nested_arith.snek", expected: "25", },
    { name: boa_binding, file: "boa_binding.snek", expected: "5", },
    { name: boa_shadow_binding, file: "boa_shadow_binding.snek", expected: "11", },
    { name: boa_nested_let, file: "boa_nested_let.snek", expected: "210", },
    { name: boa_nested_arith_ultra_deluxe, file: "boa_nested_arith_ultra_deluxe.snek", expected: "13500", },
    { name: boa_shadow_chain_binding, file: "boa_shadow_chain_binding.snek", expected: "710", },
    { name: boa_nested_let_many, file: "boa_nested_let_many.snek", expected: "90", },
    {
        name: if_input_t,
        file: "if_input.snek",
        input: "true",
        expected: "1234",
    },
    {
        name: if_input_f,
        file: "if_input.snek",
        input: "false",
        expected: "7",
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
        name: factorial_0,
        file: "factorial.snek",
        input: "0",
        expected: "1",
    },
    {
        name: factorial_5,
        file: "factorial.snek",
        input: "5",
        expected: "120",
    },
    {
        name: typecheck_num,
        file: "typecheck.snek",
        input: "8",
        expected: "1000",
    },
    {
        name: typecheck_bool,
        file: "typecheck.snek",
        input: "false",
        expected: "1",
    },
    {
        name: check_args_eq_bb,
        file: "check_args_eq_bb.snek",
        expected: "false",
    },
    {
        name: big_2,
        file: "big_2.snek",
        expected: "4611686018427387903",
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
        name: check_args_add1_t,
        file: "check_args_add1.snek",
        input: "true",
        expected: "invalid argument",
    },
    {
        name: check_args_add1_f,
        file: "check_args_add1.snek",
        input: "false",
        expected: "invalid argument",
    },
    {
        name: check_args_add_nb,
        file: "check_args_add_nb.snek",
        expected: "invalid argument",
    },
    {
        name: check_args_add_bn,
        file: "check_args_add_bn.snek",
        expected: "invalid argument",
    },
    {
        name: check_args_add_bb,
        file: "check_args_add_bb.snek",
        expected: "invalid argument",
    },
    {
        name: big_1,
        file: "big_1.snek",
        expected: "overflow",
    },
    {
        name: big_3,
        file: "big_3.snek",
        expected: "overflow",
    },
}

static_error_tests! {
    { name: boa_unbound_id, file: "boa_unbound_id.snek", expected: "Unbound variable identifier x", },
    { name: boa_duplicate_binding, file: "boa_duplicate_binding.snek", expected: "Duplicate binding", },
    { name: boa_bad_sexp, file: "boa_bad_sexp.snek", expected: "Invalid", },
    { name: boa_parse_empty, file: "boa_bad_parse_empty.snek", expected: "Invalid", },
    { name: boa_parse_args1, file: "boa_bad_parse_args1.snek", expected: "Invalid", },
    { name: boa_parse_args2, file: "boa_bad_parse_args2.snek", expected: "Invalid", },
    { name: boa_parse_let1, file: "boa_bad_parse_let1.snek", expected: "Invalid", },
    { name: boa_parse_let2, file: "boa_bad_parse_let2.snek", expected: "Invalid", },
    { name: boa_parse_let3, file: "boa_bad_parse_let3.snek", expected: "Invalid", },
    {
        name: number_bounds_fail,
        file: "number_bounds_fail.snek",
        expected: "Invalid",
    }
}
