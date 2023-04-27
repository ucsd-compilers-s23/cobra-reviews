mod infra;

// Your tests go here!
success_tests! {
    {
        name: times,
        file: "times.snek",
        expected: "4"
    },
    {
        name: isnum,
        file: "isnum.snek",
        expected: "true"
    },
    {
        name: isbool,
        file: "isbool.snek",
        expected: "false"
    },
    {
        name: input_1,
        file: "input.snek",
        input: "true",
        expected: "true"
    },
    {
        name: input_2,
        file: "input.snek",
        input: "12",
        expected: "12"
    },
    {
        name: input_4,
        file: "input.snek",
        input: "-12",
        expected: "-12"
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
        name: input_compare_equal_1,
        file: "input_compare_equal.snek",
        input: "5",
        expected: "true",
    },
    {
        name: input_compare_equal_2,
        file: "input_compare_equal.snek",
        input: "4",
        expected: "false",
    },
    {
        name: set,
        file: "set.snek",
        expected: "6"
    },
    {
        name: loop1,
        file: "loop1.snek",
        expected: "-6"
    },
    {
        name: loop2,
        file: "loop_add1.snek",
        expected: "10"
    },
    {
        name: fac,
        file: "fac.snek",
        input: "3",
        expected: "6"
    },
    {
        name: false_val,
        file: "false_val.snek",
        expected: "false"
    },
    {name: add1, file: "boa/add1.snek", expected: "73"},
    {name: add, file: "boa/add.snek", expected: "15"},
    {name: binding, file: "boa/binding.snek", expected: "5"},
    {name: let_dependency, file: "boa/let_dependency.snek", expected: "21"},
    {name: let_expr, file: "boa/let_expr.snek", expected: "12"},
    {name: nested_arith_comp, file: "boa/nested_arith_comp.snek", expected: "5"},

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
        name: input_3,
        file: "input.snek",
        input: "4611686018427387904",
        expected: "Invalid"
    },
    {
        name: compare_boolean,
        file: "compare_boolean.snek",
        expected: "invalid argument",
    },
}

static_error_tests! {
    {
        name: number_bounds_fail,
        file: "number_bounds_fail.snek",
        expected: "Invalid",
    },
    {name: duplicate_binding_nested, file: "boa/duplicate_binding_nested.snek", expected: "Duplicate binding"},
    {name: duplicate_binding_space, file: "boa/duplicate_binding_space.snek", expected: "Duplicate binding"},
    {name: duplicate_binding, file: "boa/duplicate_binding.snek", expected: "Duplicate binding"},
    {name: emmpty_paren, file: "boa/empty_paren.snek", expected: "Invalid"},
    {name: emmpty, file: "boa/empty.snek", expected: "Invalid"},
    {name: hello_word, file: "boa/hello_world.snek", expected: "Invalid"},
    {name: invalid_basic, file: "boa/invalid_basic.snek", expected: "Invalid"},
    {name: invalid, file: "boa/invalid.snek", expected: "Invalid"},
    {name: let_invalid, file: "let_invalid.snek", expected: "Invalid"},
}
