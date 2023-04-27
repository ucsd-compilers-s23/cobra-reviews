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
        name: sub1,
        file: "sub1.snek",
        expected: "82",
    },
    {
        name: add, 
        file: "add.snek",
        expected: "15",
    }, 
    {
        name: sub, 
        file: "sub.snek",
        expected: "-38",
    },
    {
        name: mul,
        file: "mul.snek",
        expected: "380",
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
        name: nested_operations,
        file: "nested_operations.snek",
        expected: "1793",
    },
    {
        name: nested_bindings,
        file: "nested_bindings.snek",
        expected: "-90",
    }, 
    {
        name: shadowing, 
        file: "shadowing.snek",
        expected: "21",
    }, 
    {
        name: binding_chain, 
        file: "binding_chain.snek",
        expected: "90",
    },
    {
        name: binding_nested, 
        file: "binding_nested.snek",
        expected: "400",
    },
    {
        name: negative_input, 
        file: "simple_input.snek",
        input: "-10",
        expected: "-10"
    }, 
    {
        name: writeup_example1,
        file: "writeup_example1.snek",
        expected: "6",
    }, 
    {
        name: writeup_example2,
        file: "writeup_example2.snek",
        expected: "-6",
    },
    {
        name: writeup_example3,
        file: "writeup_example3.snek",
        input: "5",
        expected: "120",
    }, 
    {
        name: default_input, 
        file: "simple_input.snek",
        expected: "false",
    },
    {
        name: overflow_mul, 
        file: "overflow_mul.snek",
        expected: "-4611686018427387904",
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
        name: factorial_overflow, 
        file: "writeup_example3.snek",
        input: "30", 
        expected: "overflow",
    },
}

static_error_tests! {
    {
        name: number_bounds_fail,
        file: "number_bounds_fail.snek",
        expected: "Invalid",
    }
}
