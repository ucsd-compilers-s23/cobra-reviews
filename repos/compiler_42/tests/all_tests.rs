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
        name: int,
        file: "int.snek",
        expected: "5"
    },
    {
        name: add1,
        file: "add1.snek",
        expected: "73"
    },
    {
        name: binding,
        file: "binding.snek",
        expected: "5"
    },
    {
        name: arith1,
        file: "arith1.snek",
        expected: "310"
    },
    {
        name: times,
        file: "times.snek",
        expected: "6"
    },
    {
        name: nested_arith,
        file: "nested_arith.snek",
        expected: "25"
    },
    {
        name: isNum,
        file: "isNum.snek",
        expected: "true"
    },
    {
        name: true_test,
        file: "true.snek",
        expected: "true"
    },
    {
        name: false_test,
        file: "false.snek",
        expected: "false"
    },
    {
        name: print,
        file: "print.snek",
        expected: "5"
    },
    {
        name: eq,
        file: "eq.snek",
        expected: "true"
    },
    {
        name: greater,
        file: "greater.snek",
        expected: "false"
    },
    {
        name: less,
        file: "less.snek",
        expected: "false"
    },
    {
        name: LessEq_test,
        file: "LessEq.snek",
        expected: "true"
    },
    {
        name: GreaterEq_test,
        file: "GreaterEq.snek",
        expected: "true"
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
}

static_error_tests! {
    {
        name: number_bounds_fail,
        file: "number_bounds_fail.snek",
        expected: "Invalid",
    }
}
