mod infra;

// Run all tests:
//  cargo test --package cobra --test all_tests

// Run specific test:
// cargo test --package cobra --test all_tests -- name_of_test --exact --nocapture

// Your tests go here!
success_tests! {
    // Integer tests
    {
        name: int1,
        file: "int1.snek",
        expected: "65",
    },

    {
        name: int2,
        file: "int2.snek",
        expected: "-65",
    },

    {
        name: int3,
        file: "int3.snek",
        expected: "4611686018427387903",
    },

    {
        name: int4,
        file: "int4.snek",
        expected: "-4611686018427387904",
    },

    // Boolean tests
    {
        name: false_val,
        file: "false_val.snek",
        expected: "false",
    },

    {
        name: true_val,
        file: "true_val.snek",
        expected: "true",
    },

    // Type check tests
    {
        name: isnum1,
        file: "isnum1.snek",
        expected: "true",
    },
    {
        name: isnum2,
        file: "isnum2.snek",
        expected: "false",
    },

    {
        name: isbool1,
        file: "isbool1.snek",
        expected: "true",
    },
    {
        name: isbool2,
        file: "isbool2.snek",
        expected: "false",
    },

    // Equal tests
    {
        name: equals1,
        file: "equals1.snek",
        expected: "true",
    },

    {
        name: equals2,
        file: "equals2.snek",
        expected: "false",
    },

    {
        name: equals3,
        file: "equals3.snek",
        expected: "true",
    },

    {
        name: equals4,
        file: "equals4.snek",
        expected: "true",
    },

    {
        name: equals5,
        file: "equals5.snek",
        expected: "false",
    },

    // Greater tests
    {
        name: greater1,
        file: "greater1.snek",
        expected: "false",
    },
    {
        name: greater2,
        file: "greater2.snek",
        expected: "true",
    },
    {
        name: greater3,
        file: "greater3.snek",
        expected: "false",
    },

    // Greater Equal tests
    {
        name: greater_equal1,
        file: "greater_equal1.snek",
        expected: "true",
    },
    {
        name: greater_equal2,
        file: "greater_equal2.snek",
        expected: "true",
    },

    {
        name: greater_equal3,
        file: "greater_equal3.snek",
        expected: "false",
    },

    // Less tests
    {
        name: less1,
        file: "less1.snek",
        expected: "false",
    },
    {
        name: less2,
        file: "less2.snek",
        expected: "false",
    },
    {
        name: less3,
        file: "less3.snek",
        expected: "true",
    },

    // Less Equal tests
    {
        name: less_equal1,
        file: "less_equal1.snek",
        expected: "true",
    },
    {
        name: less_equal2,
        file: "less_equal2.snek",
        expected: "false",
    },
    {
        name: less_equal3,
        file: "less_equal3.snek",
        expected: "true",
    },

    // Arithmetic tests
    {
        name: plus1,
        file: "plus1.snek",
        expected: "15"
    },
    {
        name: minus1,
        file: "minus1.snek",
        expected: "15"
    },
    {
        name: times1,
        file: "times1.snek",
        expected: "50"
    },

    // Block tests
    {
        name: block1,
        file: "block1.snek",
        expected: "4"
    },
    {
        name: block2,
        file: "block2.snek",
        expected: "6"
    },
    {
        name: block3,
        file: "block3.snek",
        expected: "18"
    },
    {
        name: block4,
        file: "block4.snek",
        expected: "100"
    },

    // If
    {
        name: if1,
        file: "if1.snek",
        expected: "true"
    },

    {
        name: if2,
        file: "if2.snek",
        expected: "false"
    },

    {
        name: if3,
        file: "if3.snek",
        expected: "200"
    },

    // Input
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

    // Set!
    {
        name: set1,
        file: "set1.snek",
        expected: "6",
    },

    {
        name: set2,
        file: "set2.snek",
        expected: "10",
    },

    // Loop / Break
    {
        name: loop1,
        file: "loop1.snek",
        expected: "15",
    },

    {
        name: loop2,
        file: "loop2.snek",
        expected: "-6",
    },

    {
        name: factorial_0,
        file: "factorial.snek",
        input: "0",
        expected: "1",
    },

    {
        name: factorial_1,
        file: "factorial.snek",
        input: "1",
        expected: "1",
    },

    {
        name: factorial_5,
        file: "factorial.snek",
        input: "5",
        expected: "120",
    },

    // Regression tests
    {
        name: add1,
        file: "add1.snek",
        expected: "3",
    }


}

runtime_error_tests! {
    // Equals
    {
        name: fail_equals1,
        file: "fail_equals1.snek",
        expected: "an error occurred: invalid argument (incompatible types)",
    },

    {
        name: fail_equals2,
        file: "fail_equals2.snek",
        expected: "an error occurred: invalid argument (incompatible types)",
    },

    // Greater
    {
        name: fail_greater1,
        file: "fail_greater1.snek",
        expected: "an error occurred: invalid argument (incompatible types)",
    },
    {
        name: fail_greater2,
        file: "fail_greater2.snek",
        expected: "an error occurred: invalid argument (incompatible types)",
    },

    // Greater Equal
    {
        name: fail_greater_equal1,
        file: "fail_greater_equal1.snek",
        expected: "an error occurred: invalid argument (incompatible types)",
    },
    {
        name: fail_greater_equal2,
        file: "fail_greater_equal2.snek",
        expected: "an error occurred: invalid argument (incompatible types)",
    },

    // Less
    {
        name: fail_less1,
        file: "fail_less1.snek",
        expected: "an error occurred: invalid argument (incompatible types)",
    },
    {
        name: fail_less2,
        file: "fail_less2.snek",
        expected: "an error occurred: invalid argument (incompatible types)",
    },

    // Less Equal
    {
        name: fail_less_equal_1,
        file: "fail_less_equal1.snek",
        expected: "an error occurred: invalid argument (incompatible types)",
    },
    {
        name: fail_less_equal_2,
        file: "fail_less_equal2.snek",
        expected: "an error occurred: invalid argument (incompatible types)",
    },

    // Arithmetic
    {
        name: fail_plus1,
        file: "fail_plus1.snek",
        expected: "an error occurred: invalid argument (incompatible types)",
    },
    {
        name: fail_plus2,
        file: "fail_plus2.snek",
        expected: "an error occurred: invalid argument (incompatible types)",
    },

    // Overflow
    {
        name: overflow1,
        file: "overflow1.snek",
        expected: "an error occurred: numeric overflow",
    },

    {
        name: overflow2,
        file: "overflow2.snek",
        expected: "an error occurred: numeric overflow",
    },

    // Default
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
        expected: "Invalid: number must be in the range of a 63-bit signed integer",
    },
    {
        name: fail_int1,
        file: "fail_int1.snek",
        expected: "Invalid: number must be in the range of a 63-bit signed integer",
    },
    {
        name: fail_int2,
        file: "fail_int2.snek",
        expected: "Invalid: number must be in the range of a 63-bit signed integer",
    }
}
