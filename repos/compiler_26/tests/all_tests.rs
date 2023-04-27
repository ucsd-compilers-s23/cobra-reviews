mod infra;

// Your tests go here!
success_tests! {
    {
        name: false_val,
        file: "false_val.snek",
        expected: "false",
    },
    {
        name: input_num,
        file: "input.snek",
        input: "42",
        expected: "42",
    },
    {
        name: input_bool,
        file: "input.snek",
        input: "true",
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
        name: add1,
        file: "add1.snek",
        expected: "73",
    },
    {
        name: add,
        file: "add.snek",
        expected: "15",
    },
    {
        name: nested_arith,
        file: "nested_arith.snek",
        expected: "25",
    },
    {
        name: binding,
        file: "binding.snek",
        expected: "5",
    },
    {
        name: mul,
        file: "mul.snek",
        expected: "50",
    },
    {
        name: shadowing,
        file: "shadowing.snek",
        expected: "10",
    },
    {
        name: isnum,
        file: "isnum.snek",
        expected: "true",
    },
    {
        name: isntnum,
        file: "isntnum.snek",
        expected: "false",
    },
    {
        name: isbool,
        file: "isbool.snek",
        expected: "true",
    },
    {
        name: isntbool,
        file: "isntbool.snek",
        expected: "false",
    },
    {
        name: isgreater,
        file: "greater.snek",
        input: "10",
        expected: "true",
    },
    {
        name: isntgreater,
        file: "greater.snek",
        input: "1",
        expected: "false",
    },
    {
        name: complex_loop,
        file: "complex_loop.snek",
        expected: "-6",
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
    }
}
