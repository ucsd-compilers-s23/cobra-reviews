mod infra;

// Your tests go here!
success_tests! {
    {
        name: number1,
        file: "number1.snek",
        expected: "1512",
    },
    {
        name: number2,
        file: "number2.snek",
        expected: "123",
    },
    {
        name: number3,
        file: "number3.snek",
        expected: "4611686018427387903",
    },
    {
        name: number4,
        file: "number4.snek",
        expected: "-4611686018427387904",
    },
    {
        name: false_val,
        file: "false_val.snek",
        expected: "false",
    },
    {
        name: true_val,
        file: "true_val.snek",
        expected: "true",
    },
    {
        name: add1,
        file: "add1.snek",
        expected: "73",
    },
    {
        name: sub1,
        file: "sub1.snek",
        expected: "71",
    },
    {
        name: isnum_num,
        file: "isnum_num.snek",
        expected: "true",
    },
    {
        name: isnum_true,
        file: "isnum_true.snek",
        expected: "false",
    },
    {
        name: isnum_false,
        file: "isnum_false.snek",
        expected: "false",
    },
    {
        name: isbool_num,
        file: "isbool_num.snek",
        expected: "false",
    },
    {
        name: isbool_true,
        file: "isbool_true.snek",
        expected: "true",
    },
    {
        name: isbool_false,
        file: "isbool_false.snek",
        expected: "true",
    },
    {
        name: plus,
        file: "plus.snek",
        expected: "97531",
    },
    {
        name: minus,
        file: "minus.snek",
        expected: "97000",
    },
    {
        name: times,
        file: "times.snek",
        expected: "1223227",
    },
    {
        name: times2,
        file: "times2.snek",
        expected: "-10",
    },
    {
        name: input1,
        file: "input.snek",
        input: "123",
        expected: "123",
    },
    {
        name: input2,
        file: "input.snek",
        input: "true",
        expected: "true",
    },
    {
        name: input3,
        file: "input.snek",
        input: "false",
        expected: "false",
    },
    {
        name: let1,
        file: "let1.snek",
        expected: "20",
    },
    {
        name: greater_true,
        file: "greater_true.snek",
        expected: "true",
    },
    {
        name: greater_false,
        file: "greater_false.snek",
        expected: "false",
    },
    {
        name: greater_equals_true,
        file: "greater_equals_true.snek",
        expected: "true",
    },
    {
        name: greater_equals_false,
        file: "greater_equals_false.snek",
        expected: "false",
    },
    {
        name: less_true,
        file: "less_true.snek",
        expected: "true",
    },
    {
        name: less_false,
        file: "less_false.snek",
        expected: "false",
    },
    {
        name: less_equals_true,
        file: "less_equals_true.snek",
        expected: "true",
    },
    {
        name: less_equals_false,
        file: "less_equals_false.snek",
        expected: "false",
    },
    {
        name: equals_num_true,
        file: "equals_number.snek",
        input: "5",
        expected: "true",
    },
    {
        name: equals_num_false,
        file: "equals_number.snek",
        input: "6",
        expected: "false",
    },
    {
        name: equals_bool_true,
        file: "equals_bool.snek",
        input: "true",
        expected: "true",
    },
    {
        name: equals_bool_false,
        file: "equals_bool.snek",
        input: "false",
        expected: "false",
    },
    {
        name: cmp_no_overflow,
        file: "cmp_no_overflow.snek",
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
        name: example1,
        file: "example1.snek",
        expected: "6",
    },
    {
        name: example2,
        file: "example2.snek",
        expected: "-6",
    },
    {
        name: factorial1,
        file: "factorial.snek",
        input: "1",
        expected: "1",
    },
    {
        name: factorial5,
        file: "factorial.snek",
        input: "5",
        expected: "120",
    },
    {
        name: factorial10,
        file: "factorial.snek",
        input: "10",
        expected: "3628800",
    },
}

runtime_error_tests! {
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
        name: plus_overflow,
        file: "plus_overflow.snek",
        expected: "overflow",
    },
    {
        name: minus_overflow,
        file: "minus_overflow.snek",
        expected: "overflow",
    },
    {
        name: times_overflow,
        file: "times_overflow.snek",
        expected: "overflow",
    },
    {
        name: invalid_input1,
        file: "input.snek",
        input: "Something invalid",
        expected: "Invalid input",
    },
    {
        name: invalid_input2,
        file: "input.snek",
        input: "123.456",
        expected: "Invalid input",
    },
    {
        name: invalid_input3,
        file: "input.snek",
        input: "19832719823791872398173987",
        expected: "Invalid input",
    },
    {
        name: invalid_argument,
        file: "invalid_argument.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument2,
        file: "invalid_argument2.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument3,
        file: "invalid_argument3.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument4,
        file: "invalid_argument4.snek",
        expected: "invalid argument",
    },
    {
        name: equals_num_error,
        file: "equals_number.snek",
        input: "true",
        expected: "invalid argument",
    },
    {
        name: equals_bool_error,
        file: "equals_bool.snek",
        input: "6",
        expected: "invalid argument",
    },
    {
        name: input_compare_3,
        file: "input_compare.snek",
        input: "true",
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
        name: let_duplicate_binding1,
        file: "let_duplicate_binding1.snek",
        expected: "Duplicate binding",
    },
    {
        name: let_keyword1,
        file: "let_keyword1.snek",
        expected: "keyword",
    },
    {
        name: let_keyword2,
        file: "let_keyword2.snek",
        expected: "keyword",
    },
    {
        name: unbound_identifier1,
        file: "unbound_identifier1.snek",
        expected: "Unbound variable identifier x",
    },
    {
        name: block_empty,
        file: "block_empty.snek",
        expected: "Invalid",
    },
}
