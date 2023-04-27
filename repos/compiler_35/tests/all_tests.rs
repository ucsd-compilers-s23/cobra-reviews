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
        name: ta1,
        file: "ta1.snek",
        expected: "11",
    },
    {
        name: ta2,
        file: "ta2.snek",
        expected: "15",
    },
    {
        name: combo1,
        file: "combo1.snek",
        expected: "1",
    },
    {
        name: ta3,
        file: "ta3.snek",
        expected: "6",
    },
    {
        name: if2,
        file: "if2.snek",
        expected: "true"
    },
    {
        name: loop1,
        file: "loop1.snek",
        expected: "22"
    },
    {
        name: bool_check,
        file: "bool.snek",
        expected: "false"
    },
    { 
        name: binding1, // Reference: https://edstem.org/us/courses/38748/discussion/2996666
        file: "binding1.snek",
        expected: "10"

    },
    {
        name: binding2,
        file: "binding2.snek",
        expected: "5"
    },
    {
        name: binding3,
        file: "binding3.snek",
        expected: "false"
    },
    {
        name: shadow1,
        file: "shadow1.snek",
        expected: "10"
    },
    {
        name: shadow2,
        file: "shadow2.snek",
        expected: "15"
    },
    {
        name: shadow3,
        file: "shadow3.snek",
        expected: "10"
    }
    ,
    {
        name: input1_true,
        file: "input1.snek",
        input: "5",
        expected: "true"

    }
    ,
    {
        name: input1_false,
        file: "input1.snek",
        input: "4",
        expected: "false"

    },
    {
        name: input2_1,
        file: "input2.snek",
        input: "4",
        expected: "8"
    },
    {
        name: input2_2,
        file: "input2.snek",
        input: "-4",
        expected: "0"
    },
    {
        name: input_large,
        file: "input_overflow_1.snek",
        input: "-4611686018427387903",
        expected: "-4611686018427387904"
    },
    {
        name: loop3,
        file: "loop3.snek",
        expected: "true"
    },
    {
        name: input_unary1,
        file: "input_unary.snek",
        input: "1",
        expected: "2"
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
        name: input1_err,
        file: "input1.snek",
        input: "false",
        expected: "invalid argument",
    },
    {
        name: input_overflow,
        file: "input_overflow.snek",
        input: "4611686018427387903",
        expected: "overflow"
    },
    {
        name: input_overflow_1,
        file: "input_overflow_1.snek",
        input: "-4611686018427387904",
        expected: "overflow"
    },
    {
        name: type1,
        file: "type1.snek",
        input: "false",
        expected: "invalid argument"
    },
    {
        name: input_unary,
        file: "input_unary.snek",
        input: "false",
        expected: "invalid argument"
    }
    
}

static_error_tests! {
    {
        name: number_bounds_fail,
        file: "number_bounds_fail.snek",
        expected: "Invalid",
    },
    {
        name: break_fail,
        file: "break.snek",
        expected: "break"
    },
    {
        name: set_fail,
        file: "set1.snek",
        expected: "Invalid"
    },
    {
        name: unbound,
        file: "unbound1.snek",
        expected: "Unbound variable identifier"
    },
    {
        name: block_empty,
        file: "block_empty.snek",
        expected: "Invalid"   
    },
    {
        name: keyword,
        file: "keyword.snek",
        expected: "keyword"
    },
    {
        name: identifier,
        file: "identifier.snek",
        expected: "Unbound variable identifier"
    },
    {
        name: set2,
        file: "set2.snek",
        expected: "keyword"
    }
}
