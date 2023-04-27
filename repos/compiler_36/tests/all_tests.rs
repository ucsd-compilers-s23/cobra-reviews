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
        name: unary_math,
        file: "unary_math.snek",
        expected: "4",
    },
    {
        name: unary_predicates,
        file: "unary_predicates.snek",
        expected: "true",
    },
    {
        name: arithmetic,
        file: "arithmetic.snek",
        expected: "188",
    },
    {
        name: subtract,
        file: "subtract.snek",
        expected: "-5",
    },
    {
        name: factorial,
        file: "factorial.snek",
        input: "4",
        expected: "24",
    },
    {
        name: if_typecheck,
        file: "if_typecheck.snek",
        expected: "43",
    },
    {
        name: if_truthy,
        file: "if_truthy.snek",
        expected: "true",
    }
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
        name: overflow,
        file: "overflow.snek",
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
        name: reserved_binding,
        file: "reserved_binding.snek",
        expected: "Cannot use keyword as identifier",
    }
}
