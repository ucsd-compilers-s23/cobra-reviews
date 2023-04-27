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
        name: add1_sub1,
        file: "test1.snek",
        expected: "51",
    },
    {
        name: nested_arith,
        file: "test2.snek",
        input: "2",
        expected: "5",
    },
    {
        name: let_1,
        file: "test3.snek",
        expected: "6",
    },
    {
        name: let_2,
        file: "test4.snek",
        expected: "5",
    },
    {
        name: if_eq_3_3_t3_f5,
        file: "test6.snek",
        expected: "3",
    },
    {
        name: if_eq_3_5_t3_f5,
        file: "test7.snek",
        expected: "5",
    },
    {
        name: if_f_t3_f5,
        file: "test8.snek",
        expected: "3",
    },
    {
        name: if_true_t3_f5,
        file: "test10.snek",
        expected: "3",
    },
    {
        name: if_false_t3_f5,
        file: "test11.snek",
        expected: "5",
    },
    {
        name: eq_3_3,
        file: "test12.snek",
        expected: "true",
    },
    {
        name: eq_3_5,
        file: "test13.snek",
        expected: "false",
    },
    {
        name: block,
        file: "test15.snek",
        expected: "6",
    },
    {
        name: loop1,
        file: "test16.snek",
        expected: "-6",
    },
    {
        name: input,
        file: "test18.snek",
        input: "2",
        expected: "2",
    },
    {
        name: isnum_5,
        file: "test21.snek",
        expected: "true",
    },
    {
        name: isnum_true,
        file: "test22.snek",
        expected: "false",
    },
    {
        name: if_eq_t_t_t3_f5,
        file: "test23.snek",
        expected: "3",
    },
    {
        name: if_eq_t_f_t3_f5,
        file: "test24.snek",
        expected: "5",
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
        name: overflow,
        file: "test17.snek",
        expected: "overflow",
    },
    {
        name: not_num,
        file: "test19.snek",
        expected: "should be a number",
    },
    {
        name: not_bool,
        file: "test20.snek",
        expected: "should be a bool",
    },
    {
        name: input2,
        file: "test18.snek",
        input: "2 4",
        expected: "",
    },
}

static_error_tests! {
    {
        name: number_bounds_fail,
        file: "number_bounds_fail.snek",
        expected: "Invalid",
    },
    
}
