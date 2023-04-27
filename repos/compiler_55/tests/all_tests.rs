mod infra;

// Your tests go here!
success_tests! {
    {
        name: add1,
        file: "add1.snek",
        expected: "4",
    },
    {
        name: arith,
        file: "arith.snek",
        expected: "105",
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
        name: if_succ_1,
        file: "if_succ.snek",
        input: "80",
        expected: "-32",
    },
    {
        name: if_succ_2,
        file: "if_succ.snek",
        input: "1042",
        expected: "42",
    },
    {
        name: is_num_1,
        file: "is_num.snek",
        input: "-789",
        expected: "true",
    },
    {
        name: is_num_2,
        file: "is_num.snek",
        input: "true",
        expected: "false",
    },
    {
        name: is_num_3,
        file: "is_num.snek",
        expected: "false",
    },
    {
        name: block,
        file: "block.snek",
        expected: "495"
    },
    {
        name: loop_succ_1,
        file: "loop_succ.snek",
        input: "10",
        expected: "55",
    },
    {
        name: loop_succ_2,
        file: "loop_succ.snek",
        input: "100",
        expected: "5050",
    },
}

runtime_error_tests! {
    {
        name: invalid_argument,
        file: "invalid_argument.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_1,
        file: "invalid_argument_1.snek",
        input: "true",
        expected: "invalid argument",
    },
    {
        name: input_compare_3,
        file: "input_compare.snek",
        input: "true",
        expected: "invalid argument",
    },
    {
        name: overflow,
        file: "overflow.snek",
        expected: "overflow"
    },
    {
        name: eq_fail,
        file: "eq_fail.snek",
        input: "55",
        expected: "invalid argument"
    }
}

static_error_tests! {
    {
        name: number_bounds_fail,
        file: "number_bounds_fail.snek",
        expected: "Invalid",
    },
    {
        name: keyword_fail,
        file: "keyword_fail.snek",
        expected: "keyword",
    },
    {
        name: parse_block_fail,
        file: "parse_block_fail.snek",
        expected: "Invalid",
    }
}
