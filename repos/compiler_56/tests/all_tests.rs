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
        name: factorial,
        file: "factorial.snek",
        input: "5",
        expected: "120",
    },
    {
        name: forloop,
        file: "forloop.snek",
        input: "1",
        expected: "55",
    },
    {
        name: input,
        file: "input.snek",
        input: "1",
        expected: "1",
    },
    {
        name: set,
        file: "set.snek",
        input: "1",
        expected: "6",
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
        name: runtime_overflow,
        file: "runtime_overflow.snek",
        input: "1",
        expected: "overflow",
    },
    {
        name: invalid_compare,
        file: "invalid_compare.snek",
        input: "1",
        expected: "invalid argument",
    },
    {
        name: invalid_plus,
        file: "invalid_plus.snek",
        input: "1",
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
        name: bad_break,
        file: "bad_break.snek",
        expected: "break",
    },
    {
        name: empty_block,
        file: "empty_block.snek",
        expected: "Invalid",
    },
    {
        name: keyword,
        file: "keyword.snek",
        expected: "keyword",
    },    

}
