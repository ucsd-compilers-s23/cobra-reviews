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
        name: input_echo_10,
        file: "input_echo.snek",
        input: "10",
        expected: "10",
    },
    {
        name: input_echo_minus_10,
        file: "input_echo.snek",
        input: "-10",
        expected: "-10",
    },
    {
        name: input_echo_true,
        file: "input_echo.snek",
        input: "true",
        expected: "true",
    },
    {
        name: input_echo_false,
        file: "input_echo.snek",
        input: "false",
        expected: "false",
    },
    {
        name: if_input_true,
        file: "if_input.snek",
        input: "true",
        expected: "10",
    },
    {
        name: if_input_false,
        file: "if_input.snek",
        input: "false",
        expected: "-10",
    },
    {
        name: input_add,
        file: "input_add.snek",
        input: "10",
        expected: "20",
    },
    {
        name: isbool_true,
        file: "isbool.snek",
        input: "true",
        expected: "true",
    },
    {
        name: isbool_false,
        file: "isbool.snek",
        input: "false",
        expected: "true",
    },
    {
        name: isbool_10,
        file: "isbool.snek",
        input: "10",
        expected: "false",
    },
    {
        name: isnum_true,
        file: "isnum.snek",
        input: "true",
        expected: "false",
    },
    {
        name: isnum_10,
        file: "isnum.snek",
        input: "10",
        expected: "true",
    },
    {
        name: equals_input_false,
        file: "equals_input.snek",
        input: "5",
        expected: "false",
    },
    {
        name: equals_input_true,
        file: "equals_input.snek",
        input: "10",
        expected: "true",
    },
    {
        name: true_val,
        file: "true.snek",
        input: "true",
        expected: "true",
    },
    {
        name: greater_1_0,
        file: "greater.snek",
        input: "1",
        expected: "true",
    },
    {
        name: greater_0_0,
        file: "greater.snek",
        input: "0",
        expected: "false",
    },
    {
        name: greater_equal_1_0,
        file: "ge.snek",
        input: "1",
        expected: "true",
    },
    {
        name: greater_equal_0_0,
        file: "ge.snek",
        input: "0",
        expected: "true",
    },
    {
        name: greater_equal_minus_1_0,
        file: "ge.snek",
        input: "-1",
        expected: "false",
    },
    {
        name: less_minus_1_0,
        file: "less.snek",
        input: "-1",
        expected: "true",
    },
    {
        name: less_0_0,
        file: "less.snek",
        input: "0",
        expected: "false",
    },
    {
        name: less_equal_minus_1_0,
        file: "le.snek",
        input: "-1",
        expected: "true",
    },
    {
        name: less_equal_0_0,
        file: "le.snek",
        input: "0",
        expected: "true",
    },
    {
        name: less_equal_1_0,
        file: "le.snek",
        input: "1",
        expected: "false",
    },
    {
        name: eq_true_true,
        file: "eq_true_true.snek",
        expected: "true",
    },
    {
        name: eq_true_false,
        file: "eq_true_false.snek",
        expected: "false",
    },
    {
        name: loop1,
        file: "loop1.snek",
        expected: "55",
    },
    {
        name: loop2,
        file: "loop2.snek",
        input: "4",
        expected: "24",
    },
    {
        name: loop3,
        file: "loop3.snek",
        expected: "-6",
    },
    {
        name: fac0,
        file: "fac.snek",
        input: "0",
        expected: "1",
    },
    {
        name: fac1,
        file: "fac.snek",
        input: "1",
        expected: "1",
    },
    {
        name: fac6,
        file: "fac.snek",
        input: "6",
        expected: "720",
    },
    {
        name: fac14,
        file: "fac.snek",
        input: "14",
        expected: "87178291200",
    },
    {
        name: add1,
        file: "add1.snek",
        expected: "73"
    },
    {
        name: add,
        file: "add.snek",
        expected: "15"
    },
    {
        name: nested_arith,
        file: "nested_arith.snek",
        expected: "25"
    },
    {
        name: binding,
        file: "binding.snek",
        expected: "5"
    },
    {
        name: lettest,
        file: "lettest.snek",
        expected: "12"
    },
    {
        name: shadowbind1,
        file: "shadowbind1.snek",
        expected: "21"
    },
    {
        name: shadowbind2,
        file: "shadowbind2.snek",
        expected: "550"
    },
    {
        name: mul_1,
        file: "mul_1.snek",
        expected: "-400"
    },
    {
        name: sub_1,
        file: "sub_1.snek",
        expected: "-29"
    },
    {
        name: nesting,
        file: "nesting.snek",
        expected: "8734284"
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
        name: add1_overflow,
        file: "add1_overflow.snek",
        expected: "overflow",
    },
    {
        name: sub1_overflow,
        file: "sub1_overflow.snek",
        expected: "overflow",
    },
    {
        name: add_overflow,
        file: "add_overflow.snek",
        expected: "overflow",
    },
    {
        name: sub_overflow,
        file: "sub_overflow.snek",
        expected: "overflow",
    },
    {
        name: mul_overflow,
        file: "mul_overflow.snek",
        expected: "overflow",
    },
    {
        name: add1bool,
        file: "add1bool.snek",
        expected: "invalid argument",
    },
    {
        name: sub1bool,
        file: "sub1bool.snek",
        expected: "invalid argument",
    },
    {
        name: addbool1,
        file: "addbool1.snek",
        expected: "invalid argument",
    },
    {
        name: addbool2,
        file: "addbool2.snek",
        expected: "invalid argument",
    },
    {
        name: subbool1,
        file: "subbool1.snek",
        expected: "invalid argument",
    },
    {
        name: subbool2,
        file: "subbool2.snek",
        expected: "invalid argument",
    },
    {
        name: timesbool1,
        file: "timesbool1.snek",
        expected: "invalid argument",
    },
    {
        name: timesbool2,
        file: "timesbool2.snek",
        expected: "invalid argument",
    },
    {
        name: gtbool1,
        file: "gtbool1.snek",
        expected: "invalid argument",
    },
    {
        name: gtbool2,
        file: "gtbool2.snek",
        expected: "invalid argument",
    },
    {
        name: ltbool1,
        file: "ltbool1.snek",
        expected: "invalid argument",
    },
    {
        name: ltbool2,
        file: "ltbool2.snek",
        expected: "invalid argument",
    },
    {
        name: eq_true_10,
        file: "eq_true_10.snek",
        expected: "invalid argument",
    },
    {
        name: eq_20_true,
        file: "eq_20_true.snek",
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
        name: unbound_id,
        file: "unbound_id.snek",
        expected: "Unbound variable identifier x",
    },
    {
        name: duplicate_binding,
        file: "duplicate_binding.snek",
        expected: "Duplicate binding",
    },
    {
        name: invalid_op,
        file: "invalid_op.snek",
        expected: "Invalid",
    },
    {
        name: invalid_paren,
        file: "invalid_paren.snek",
        expected: "Invalid",
    },
    {
        name: invalid_type,
        file: "invalid_type.snek",
        expected: "Invalid",
    },
    {
        name: invalid_bind1,
        file: "invalid_bind1.snek",
        expected: "Invalid",
    },
    {
        name: invalid_bind2,
        file: "invalid_bind2.snek",
        expected: "Invalid",
    },
    {
        name: let_keyword,
        file: "let_keyword.snek",
        expected: "keyword",
    },
}
