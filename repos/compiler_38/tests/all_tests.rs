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
        name: nested_setq,
        file: "nested-setq.snek",
        expected: "9",
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
        input: "9",
        expected: "362880",
    },
    {
        name: valid_if,
        file: "valid-if.snek",
        expected: "1",
    },
    {
        name: isnum,
        file: "isnum.snek",
        expected: "1",
    },
    {
        name: isbool,
        file: "isbool.snek",
        expected: "1",
    },
    {
        name: compare,
        file: "compare.snek",
        expected: "11",
    },
    {
        name: equal_bool,
        file: "equal-bool.snek",
        expected: "true",
    },
    {
        name: myex1,
        file: "myex1.snek",
        expected: "false",
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
        name: equal_diff_types,
        file: "equal-diff-type.snek",
        expected: "invalid argument",
    },
    {
        name: overflow,
        file: "overflow.snek",
        expected: "overflow",
    },
    {
        name: invalid_arg1,
        file: "invalid-arg1.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_arg2,
        file: "invalid-arg2.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_arg3,
        file: "invalid-arg3.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_arg4,
        file: "invalid-arg4.snek",
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
        name: keyword_as_name,
        file: "keyword_as_name.snek",
        expected: "keyword"
    }
}
