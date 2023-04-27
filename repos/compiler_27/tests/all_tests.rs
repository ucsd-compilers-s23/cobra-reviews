mod infra;




// Your tests go here!
success_tests! {

    {
        name: test1,
        file: "test1.snek",
        expected: "10",
    },

    {
        name: test2,
        file: "test2.snek",
        expected: "1980",
    },

    {
        name: test3,
        file: "test3.snek",
        expected: "25",
    },
    {
        name: test4,
        file: "test4.snek",
        expected: "-15",
    },
    {
        name: test6,
        file: "test6.snek",
        expected: "1050",
    },
    {
        name: test7,
        file: "test7.snek",
        expected: "-110",
    },
    {
        name: test8,
        file: "test8.snek",
        expected: "-110",
    },
    {
        name: test9,
        file: "test9.snek",
        expected: "2",
    },
    {
        name: test10,
        file: "test10.snek",
        expected: "6",
    },
    {
        name: test11,
        file: "test11.snek",
        expected: "20",
    },
    {
        name: test12,
        file: "test12.snek",
        expected: "17",
    },
    {
        name: test13,
        file: "test13.snek",
        expected: "20",
    },








    




    {
        name: add1, 
        file: "add1.snek",
        expected: "4",
    },

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
        name: test5,
        file: "test5.snek",
        expected: "Duplicate binding",
    },
}
