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
        name: test1,
        file: "test1.snek",
        expected: "20",
    },
    {
        name: test2,
        file: "test2.snek",
        expected: "12",
    },
    {
        name: test3,
        file: "test3.snek",
        expected: "21",
    },
    {
        name: test4,
        file: "test4.snek",
        expected: "6",
    },
    {
        name: test5,
        file: "test5.snek",
        expected: "15",
    },
    {
        name: test6,
        file: "test6.snek",
        expected: "3333",
    },
    {
        name: sub1,
        file: "sub1.snek",
        expected: "0",
    },
    {
        name: let_1,
        file: "let_1.snek",
        expected: "5",
    },
    {
        name: plus,
        file: "plus.snek",
        expected: "123",
    },
    {
        name: minus,
        file: "minus.snek",
        expected: "-456",
    },
    {
        name: times,
        file: "times.snek",
        expected: "32",
    },
    {
        name: number,
        file: "number.snek",
        expected: "10",
    },
    {
        name: let_2,
        file: "let_2.snek",
        expected: "1234",
    },
    {
        name: let_3,
        file: "let_3.snek",
        expected: "10",
    },
    {
        name: let_4,
        file: "let_4.snek",
        expected: "10",
    },
    {
        name: let_5,
        file: "let_5.snek",
        expected: "10",
    },
    {
        name: shadow,
        file: "shadow.snek",
        expected: "10",
    },
    {
        name: shadow2,
        file: "shadow2.snek",
        expected:"21"
    },
    {   name: loop1,
        file: "loop.snek",
        expected: "-6",
    },
    {
        name: loop2,
        file: "loop2.snek",
        input: "5",
        expected: "120",
    },
    {
        name: loop3,
        file: "loop3.snek",
        input: "5",
        expected: "-25",
    },
    {
        name: loop4,
        file: "loop4.snek",
        input: "4",
        expected: "24",
    },
    {
        name: manyloop,
        file: "manyloop.snek",
        expected: "10",
    },
    {
        name: input_compare_4,
        file: "input_compare.snek",
        input: "5",
        expected: "true",
    },
    {
        name: input_compare_5,
        file: "input_compare.snek",
        input: "3",
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
        name: invalid_argument_equal,
        file: "invalid_argument_2.snek",
        input: "true",
        expected: "invalid argument",
    },
    {
        name: plus_invalid,
        file: "+.snek",
        input: "true",
        expected: "invalid argument",
    },
    // create tests for -.snke * > >= <= <
    {
        name: minus_invalid,
        file: "-.snek",
        input: "true",
        expected: "invalid argument",
    },
    
    {
        name: times_invalid, 
        file: "*.snek",
        input: "true",
        expected: "invalid argument", 
    },
    {
        name: equal_invalid,
        file: "=.snek",
        input: "true",
        expected: "invalid argument", 
    },
    {
        name: greater_invalid,  
        file: ">.snek",
        input: "true",
        expected: "invalid argument",
    },
    {
        name: greater_equal_invalid,
        file: ">=.snek",
        input: "true",
        expected: "invalid argument", 
    },
    {
        name: less_invalid,
        file: "<.snek",
        input: "true",
        expected: "invalid argument",},
    {   
        name: less_equal_invalid,
        file: "<=.snek",
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
        expected: "Unbound variable identifier x"
    },
    {
        name: duplicate_binding,
        file: "duplicate_binding.snek",
        expected: "Duplicate binding"
    },
    {
        name: unbound_id_2,
        file: "unbound_id_2.snek",
        expected: "Unbound variable identifier x"
    },
    {
        name: sexp_fail,
        file: "sexp_fail.snek",
        expected: "Invalid"
    },
    {
        name: no_bindings,
        file: "no_bindings.snek",
        expected: "Invalid"
    },
}
