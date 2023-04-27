mod infra;

// Your tests go here!
success_tests! {
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
        name: ex1,
        file: "ex1.snek",
        expected: "6",
    },
    {
        name: ex2,
        file: "ex2.snek",
        expected: "-6",
    },
    {
        name: ex3,
        file: "ex3.snek",
        input: "5",
        expected: "120",
    },

    // unops
    {
        name: isnum_true,
        file: "isnum_true.snek",
        expected: "true"
    },
    {
        name: isnum_false,
        file: "isnum_false.snek",
        expected: "false"
    },
    {
        name: isbool_true,
        file: "isbool_true.snek",
        expected: "true"
    },
    {
        name: isbool_false,
        file: "isbool_false.snek",
        expected: "false"
    },
    {
        name: add1,
        file: "add1.snek",
        expected: "6"
    },
    {
        name: sub1,
        file: "sub1.snek",
        expected: "9"
    },

    // keywords
    {
        name: set,
        file: "set.snek",
        expected: "2"
    },
    {
        name: if_then,
        file: "if_then.snek",
        expected: "5"
    },
    {
        name: if_else,
        file: "if_else.snek",
        expected: "10"
    },
    {
        name: if_num,
        file: "if_num.snek",
        expected: "true"
    },
    {
        name: break1,
        file: "break1.snek",
        expected: "5"
    },
    {
        name: loop1,
        file: "loop1.snek",
        expected: "55"
    },

    // binops - arithmetic
    {
        name: plus_overflow_pass,
        file: "plus_overflow_pass.snek",
        expected: "0"
    },
    {
        name: minus_nested,
        file: "minus_nested.snek",
        expected: "95"
    },
    {
        name: mul1,
        file: "mul1.snek",
        expected: "20"
    },


    // binops - comparison
    {
        name: eq1,
        file: "eq1.snek",
        expected: "true"
    },
    {
        name: eq2,
        file: "eq2.snek",
        expected: "false"
    },
    {
        name: eq3,
        file: "eq3.snek",
        expected: "true"
    },
    {
        name: eq4,
        file: "eq4.snek",
        expected: "false"
    },
    {
        name: greater1,
        file: "greater1.snek",
        expected: "false"
    },
    {
        name: greater2,
        file: "greater2.snek",
        expected: "true"
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
        name: mul_invalid,
        file: "mul_invalid.snek",
        expected: "invalid argument",
    },
    {
        name: sub1_invalid,
        file: "sub1_invalid.snek",
        expected: "invalid argument",
    },

    // overflow
    {
        name: mul_overflow,
        file: "mul_overflow.snek",
        expected: "overflow",
    },
    {
        name: plus_overflow,
        file: "plus_overflow.snek",
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
        name: block_fail,
        file: "block_fail.snek",
        expected: "Invalid",
    },
    {
        name: unary_invalid,
        file: "unary_invalid.snek",
        expected: "Invalid",
    },
    {
        name: binary_invalid,
        file: "binary_invalid.snek",
        expected: "Invalid",
    },
    {
        name: keyword_invalid,
        file: "keyword_invalid.snek",
        expected: "Invalid",
    },
    // let
    {
        name: let_invalid1,
        file: "let_invalid1.snek",
        expected: "Invalid",
    },
    {
        name: let_invalid2,
        file: "let_invalid2.snek",
        expected: "Invalid",
    },
    {
        name: let_invalid3,
        file: "let_invalid3.snek",
        expected: "Invalid",
    },
    {
        name: let_invalid4,
        file: "let_invalid4.snek",
        expected: "Invalid",
    },
    {
        name: let_invalid5,
        file: "let_invalid5.snek",
        expected: "Invalid",
    },
    {
        name: let_invalid6,
        file: "let_invalid6.snek",
        expected: "Invalid",
    },
}
