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
        name: positivenum,
        file: "positivenum.snek",
        expected: "32768",
    },
    {
        name: negativenum,
        file: "negativenum.snek",
        expected: "-32768",
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
        name: overflow_1,
        file: "overflow_1.snek",
        expected: "-1",
    },
    {
        name: overflow_2,
        file: "overflow_2.snek",
        expected: "-4611686018427387903",
    },
    {
        name: overflow_3,
        file: "overflow_3.snek",
        expected: "-4611686018427387904",
    },
    {
        name: eq_bool_input_1,
        file: "eq_bool_input.snek",
        expected: "false",
    },
    {
        name: eq_bool_input_2,
        file: "eq_bool_input.snek",
        input: "true",
        expected: "true",
    },
    {
        name: eq_num_input_1,
        file: "eq_num_input.snek",
        input: "3",
        expected: "false",
    },
    {
        name: eq_num_input_2,
        file: "eq_num_input.snek",
        input: "5",
        expected: "true",
    },
    {
        name: factorial_neg1,
        file: "fact.snek",
        input: "-1",
        expected: "1"
    },
    {
        name: factorial_5,
        file: "fact.snek",
        input: "5",
        expected: "120"
    },
    {
        name: greater_compare_1,
        file: "g_compare.snek",
        input: "2",
        expected: "false"
    },
    {
        name: greater_compare_2,
        file: "g_compare.snek",
        input: "5",
        expected: "false"
    },
    {
        name: greater_compare_3,
        file: "g_compare.snek",
        input: "6",
        expected: "true"
    },
    {
        name: greaterequal_compare_1,
        file: "ge_compare.snek",
        input: "2",
        expected: "true"
    },
    {
        name: greaterequal_compare_2,
        file: "ge_compare.snek",
        input: "5",
        expected: "true"
    },
    {
        name: greaterequal_compare_3,
        file: "ge_compare.snek",
        input: "6",
        expected: "false"
    },
    {
        name: lesser_compare_1,
        file: "l_compare.snek",
        input: "2",
        expected: "true"
    },
    {
        name: lesser_compare_2,
        file: "l_compare.snek",
        input: "5",
        expected: "false"
    },
    {
        name: lesser_compare_3,
        file: "l_compare.snek",
        input: "6",
        expected: "false"
    },
    {
        name: lesserequal_compare_1,
        file: "le_compare.snek",
        input: "2",
        expected: "false"
    },
    {
        name: lesserequal_compare_2,
        file: "le_compare.snek",
        input: "5",
        expected: "true"
    },
    {
        name: lesserequal_compare_3,
        file: "le_compare.snek",
        input: "6",
        expected: "true"
    },
    {
        name: isnum_1,
        file: "isnum_input.snek",
        expected: "false"
    },
    {
        name: isnum_2,
        file: "isnum_input.snek",
        input: "true",
        expected: "false"
    },
    {
        name: isnum_3,
        file: "isnum_input.snek",
        input: "-9",
        expected: "true"
    },
    {
        name: isbool_1,
        file: "isbool_input.snek",
        expected: "true"
    },
    {
        name: isbool_2,
        file: "isbool_input.snek",
        input: "true",
        expected: "true"
    },
    {
        name: isbool_3,
        file: "isbool_input.snek",
        input: "-9",
        expected: "false"
    },
    {
        name: add_input_max,
        file: "add_input.snek",
        input: "4611686018427387898",
        expected: "4611686018427387903"
    },
    {
        name: sub_input_max,
        file: "sub_input.snek",
        input: "-4611686018427387899",
        expected: "-4611686018427387904"
    },
    {
        name: bound_set,
        file: "bound_set.snek",
        input: "true",
        expected: "true",
    },
    {
        name: let_input_valid,
        file: "let_input_valid.snek",
        input: "200",
        expected: "400",
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
        name: eq_num_input_fail1,
        file: "eq_num_input.snek",
        input: "true",
        expected: "invalid argument",
    },
    {
        name: eq_num_input_fail2,
        file: "eq_num_input.snek",
        expected: "invalid argument",
    },
    {
        name: eq_bool_input_fail,
        file: "eq_bool_input.snek",
        input: "0",
        expected: "invalid argument",
    },
    {
        name: factorial_bool1,
        file: "fact.snek",
        expected: "invalid argument"
    },
    {
        name: factorial_bool2,
        file: "fact.snek",
        input: "true",
        expected: "invalid argument"
    },
    {
        name: lesserequal_compare_fail,
        file: "le_compare.snek",
        expected: "invalid argument"
    },
    {
        name: lesser_compare_fail,
        file: "l_compare.snek",
        expected: "invalid argument"
    },
    {
        name: greaterequal_compare_fail,
        file: "ge_compare.snek",
        expected: "invalid argument"
    },
    {
        name: greater_compare_fail,
        file: "g_compare.snek",
        expected: "invalid argument"
    },
    {
        name: add1_bool,
        file: "simple_input.snek",
        expected: "invalid argument"
    },
    {
        name: add1_overflow,
        file: "simple_input.snek",
        input: "4611686018427387903",
        expected: "overflow"
    },
    {
        name: add_input_bool,
        file: "add_input.snek",
        expected: "invalid argument"
    },
    {
        name: add_input_overflow,
        file: "add_input.snek",
        input: "4611686018427387899",
        expected: "overflow"
    },
}


static_error_tests! {
    {
        name: number_bounds_fail,
        file: "number_bounds_fail.snek",
        expected: "Invalid",
    },
    {
        name: empty_block,
        file: "empty_block.snek",
        expected: "Invalid",
    },
    {
        name: invalid_break,
        file: "invalid_break.snek",
        expected: "break",
    },
    {
        name: unbound_set,
        file: "unbound_set.snek",
        expected: "Unbound variable identifier x",
    },
    {
        name: let_duplicatebind,
        file: "let_duplicatebind.snek",
        expected: "Duplicate binding",
    },
    {
        name: let_keyword_1,
        file: "let_keyword_1.snek",
        expected: "keyword",
    },
    {
        name: let_keyword_2,
        file: "let_keyword_2.snek",
        expected: "keyword",
    },
    {
        name: let_keyword_3,
        file: "let_keyword_3.snek",
        expected: "keyword",
    },
    {
        name: let_keyword_4,
        file: "let_keyword_4.snek",
        expected: "keyword",
    },
    {
        name: let_keyword_5,
        file: "let_keyword_5.snek",
        expected: "keyword",
    },
    {
        name: let_keyword_6,
        file: "let_keyword_6.snek",
        expected: "keyword",
    },
    {
        name: let_keyword_7,
        file: "let_keyword_7.snek",
        expected: "keyword",
    },
    {
        name: let_keyword_8,
        file: "let_keyword_8.snek",
        expected: "keyword",
    },
    {
        name: let_keyword_9,
        file: "let_keyword_9.snek",
        expected: "keyword",
    },
    {
        name: let_keyword_10,
        file: "let_keyword_10.snek",
        expected: "keyword",
    },
    {
        name: let_keyword_11,
        file: "let_keyword_11.snek",
        expected: "keyword",
    },
    {
        name: let_keyword_12,
        file: "let_keyword_12.snek",
        expected: "keyword",
    },
    {
        name: let_keyword_13,
        file: "let_keyword_13.snek",
        expected: "keyword",
    },
}
