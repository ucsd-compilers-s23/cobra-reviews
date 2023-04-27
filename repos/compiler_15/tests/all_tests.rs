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
        name: nested_arith2,
        file: "nested_arith2.snek",
        expected: "740",
    },
    {
        name: nested_arith3,
        file: "nested_arith3.snek",
        expected: "739",
    },
    {
        name: nested_arith4,
        file: "nested_arith4.snek",
        expected: "1793",
    },
    {
        name: binding,
        file: "binding.snek",
        expected: "5",
    },
    {
        name: number,
        file: "number.snek",
        expected: "25",
    },
    {
        name: number2,
        file: "number2.snek",
        expected: "-25",
    },
    {
        name: let1,
        file: "let1.snek",
        expected: "5000",
    },
    {
        name: let2,
        file: "let2.snek",
        expected: "7400",
    },
    {
        name: let3,
        file: "let3.snek",
        expected: "100",
    },
    {
        name: let4,
        file: "let4.snek",
        expected: "20",
    },
    {
        name: let6,
        file: "let6.snek",
        expected: "12",
    },
    {
        name: let7,
        file: "let7.snek",
        expected: "21",
    },
    {
        name: let8,
        file: "let8.snek",
        expected: "5",
    },
    {
        name: let9,
        file: "let9.snek",
        expected: "-90",
    },
    {
        name: let10,
        file: "let10.snek",
        expected: "20",
    },
    {
        name: input1,
        file: "input.snek",
        input: "25",
        expected: "25",
    },
    {
        name: input2,
        file: "input.snek",
        input: "true",
        expected: "true",
    },
    {
        name: input3,
        file: "input.snek",
        input: "false",
        expected: "false",
    },
    {
        name: block,
        file: "block.snek",
        expected: "6",
    },
    {
        name: loops,
        file: "loops.snek",
        expected: "-6",
    },
    {
        name: factorial1,
        file: "loops2.snek",
        input: "5",
        expected: "120",
    },
    {
        name: factorial2,
        file: "loops2.snek",
        input: "1",
        expected: "1",
    },
    {
        name: factorial3,
        file: "loops2.snek",
        input: "-1",
        expected: "1",
    },
    {
        name: factorial4,
        file: "loops2.snek",
        input: "-1889",
        expected: "1",
    },
    {
        name: conditional,
        file: "conditional.snek",
        input: "44",
        expected: "false",
    },
    {
        name: conditional2,
        file: "conditional.snek",
        input: "2",
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
        name: invalid_argument2,
        file: "invalid_argument2.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument3,
        file: "invalid_argument3.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument4,
        file: "invalid_argument4.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument5,
        file: "invalid_argument5.snek",
        expected: "invalid argument",
    },
    {
        name: input_compare_3,
        file: "input_compare.snek",
        input: "true",
        expected: "invalid argument",
    },
    {
        name: number_overflow,
        file: "number_overflow.snek",
        expected: "overflow",
    },
    {
        name: number_overflow2,
        file: "number_overflow2.snek",
        expected: "overflow",
    },
    {
        name: factorial5,
        file: "loops2.snek",
        input: "true",
        expected: "invalid argument",
    },
    {
        name: factorial6,
        file: "loops2.snek",
        input: "false",
        expected: "invalid argument",
    },
    {
        name: factorial_overflow,
        file: "loops2.snek",
        input: "12345678",
        expected: "overflow",
    },
    {
        name: invalid_input1,
        file: "input.snek",
        input: "abcd",
        expected: "Invalid input",
    },
    {
        name: invalid_input2,
        file: "input.snek",
        input: "9999999999999999999999",
        expected: "Invalid input",
    },
    {
        name: invalid_input3,
        file: "input.snek",
        input: "-9999999999999999999999",
        expected: "Invalid input",
    },
    {
        name: invalid_input4,
        file: "input.snek",
        input: "-4611686018427387906",
        expected: "Invalid input",
    },
    {
        name: invalid_input5,
        file: "input.snek",
        input: "4611686018427387905",
        expected: "Invalid input",
    },
}

static_error_tests! {
    {
        name: number_bounds_fail,
        file: "number_bounds_fail.snek",
        expected: "Invalid",
    },
    {
        name: number_bounds_fail2,
        file: "number_bounds_fail2.snek",
        expected: "Invalid",
    },
    {
        name: number_bounds_fail3,
        file: "number_bounds_fail3.snek",
        expected: "Invalid",
    },
    {
        name: unbound_id,
        file: "unbound_id.snek",
        expected: "Unbound variable identifier x",
    },
    {
        name: unbound_id2,
        file: "unbound_id2.snek",
        expected: "Unbound variable identifier x",
    },
    {
        name: duplicate_binding,
        file: "duplicate_binding.snek",
        expected: "Duplicate binding",
    },
    {
        name: duplicate_binding2,
        file: "duplicate_binding2.snek",
        expected: "Duplicate binding",
    },
    {
        name: let5,
        file: "let5.snek",
        expected: "Unbound variable identifier x",
    },
    {
        name: fail1,
        file: "fail1.snek",
        expected: "Invalid",
    },
    {
        name: fail2,
        file: "fail2.snek",
        expected: "Invalid",
    },
    {
        name: fail3,
        file: "fail3.snek",
        expected: "Reserved keyword add1 cannot be used as an identifier name",
    },
    {
        name: fail4,
        file: "fail4.snek",
        expected: "Reserved keyword let cannot be used as an identifier name",
    },
}
