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

    // my tests
    {
        name: example0_goodinput,
        file: "example0.snek",
        input: "12344",
        expected: "12345",
    },
    {
        name: example1_goodinput1,
        file: "example1.snek",
        expected: "2",
    },
    {
        name: example1_goodinput2,
        file: "example1.snek",
        input: "false",
        expected: "2",
    },
    {
        name: example3,
        file: "example3.snek",
        expected: "10",
    },
    {
        name: example4,
        file: "example4.snek",
        expected: "-10",
    },
    {
        name: example5,
        file: "example5.snek",
        expected: "4611686018427387903",
    },
    {
        name: example7,
        file: "example7.snek",
        expected: "-4611686018427387904",
    },
    {
        name: example9,
        file: "example9.snek",
        expected: "-2",
    },
    {
        name: example11,
        file: "example11.snek",
        expected: "102",
    },
    {
        name: example12,
        file: "example12.snek",
        expected: "9",
    },
    {
        name: example13_success1,
        file: "example13.snek",
        input: "-123334",
        expected: "-1",
    },
    {
        name: example13_success2,
        file: "example13.snek",
        input: "420",
        expected: "1",
    },
    {
        name: example13_success3,
        file: "example13.snek",
        input: "0",
        expected: "0",
    },
    {
        name: example16,
        file: "example16.snek",
        expected: "0",
    },
    {
        name: example17,
        file: "example17.snek",
        expected: "4",
    },
    {
        name: example18_success,
        file: "example18.snek",
        input: "0",
        expected: "1000",
    },
    {
        name: example19_success1,
        file: "example19.snek",
        input: "13242",
        expected: "false",
    },
    {
        name: example19_success2,
        file: "example19.snek",
        input: "true",
        expected: "false",
    },
    {
        name: example19_success3,
        file: "example19.snek",
        input: "false",
        expected: "false",
    },
    {
        name: example19_success4,
        file: "example19.snek",
        input: "01234",
        expected: "false",
    },
    {
        name: example19_success5,
        file: "example19.snek",
        expected: "false",
    },
    {
        name: example23_success1,
        file: "example23.snek",
        input: "0",
        expected: "105",
    },
    {
        name: example23_success2,
        file: "example23.snek",
        input: "true",
        expected: "105",
    },
    {
        name: example25_success1,
        file: "example25.snek",
        input: "-73",
        expected: "-146",
    },
    {
        name: example25_success2,
        file: "example25.snek",
        input: "85",
        expected: "170",
    },
    {
        name: example25_success3,
        file: "example25.snek",
        input: "3",
        expected: "6",
    },
    {
        name: example25_success4,
        file: "example25.snek",
        input: "-38",
        expected: "-76",
    },
    {
        name: example25_success5,
        file: "example25.snek",
        input: "-15",
        expected: "-30",
    },
    {
        name: example25_success6,
        file: "example25.snek",
        expected: "false",
    },
    {
        name: example25_success7,
        file: "example25.snek",
        input: "false",
        expected: "false",
    },
    {
        name: example25_success8,
        file: "example25.snek",
        input: "true",
        expected: "true",
    },
    {
        name: fibonacci_success1,
        file: "fibonacci.snek",
        input: "50",
        expected: "12586269025",
    },
    {
        name: fibonacci_success2,
        file: "fibonacci.snek",
        input: "42",
        expected: "267914296",
    },
    {
        name: fibonacci_success3,
        file: "fibonacci.snek",
        input: "-4",
        expected: "0",
    },
    {
        name: arithmeticsum_success1,
        file: "arithmeticsum.snek",
        input: "-1",
        expected: "0",
    },
    {
        name: arithmeticsum_success2,
        file: "arithmeticsum.snek",
        input: "5",
        expected: "15",
    },
    {
        name: arithmeticsum_success3,
        file: "arithmeticsum.snek",
        input: "3123",
        expected: "4878126",
    },
    {
        name: arithmeticsum_success4,
        file: "arithmeticsum.snek",
        input: "3037000499",
        expected: "4611686016981624750",
    },
    {
        name: geometricsum_success1,
        file: "geometricsum.snek",
        input: "-123532",
        expected: "0",
    },
    {
        name: geometricsum_success2,
        file: "geometricsum.snek",
        input: "0",
        expected: "1",
    },
    {
        name: geometricsum_success3,
        file: "geometricsum.snek",
        input: "5",
        expected: "63",
    },
    {
        name: geometricsum_success4,
        file: "geometricsum.snek",
        input: "61",
        expected: "4611686018427387903",
    },
    {
        name: example26_success1,
        file: "example26.snek",
        input: "61",
        expected: "64",
    },
    {
        name: example26_success2,
        file: "example26.snek",
        input: "6",
        expected: "8",
    },
    {
        name: example26_success3,
        file: "example26.snek",
        input: "-39",
        expected: "64",
    },
    {
        name: example26_success4,
        file: "example26.snek",
        input: "16",
        expected: "16",
    },
    {
        name: example26_success5,
        file: "example26.snek",
        input: "-55",
        expected: "64",
    },
    {
        name: example26_success6,
        file: "example26.snek",
        input: "-69",
        expected: "128",
    },
    {
        name: example26_success7,
        file: "example26.snek",
        input: "86",
        expected: "128",
    },
    {
        name: example26_success8,
        file: "example26.snek",
        input: "3",
        expected: "4",
    },
    {
        name: example26_success9,
        file: "example26.snek",
        input: "78",
        expected: "128",
    },
    {
        name: example26_success10,
        file: "example26.snek",
        input: "0",
        expected: "1",
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

    // my tests
    {
        name: example0_badinput1,
        file: "example0.snek",
        input: "true",
        expected: "invalid argument",
    },
    {
        name: example0_badinput2,
        file: "example0.snek",
        input: "false",
        expected: "invalid argument",
    },
    {
        name: example1_badinput1,
        file: "example1.snek",
        input: "123",
        expected: "overflow",
    },
    {
        name: example1_badinput2,
        file: "example1.snek",
        input: "true",
        expected: "overflow",
    },
    {
        name: example2,
        file: "example2.snek",
        expected: "invalid argument",
    },
    {
        name: example6,
        file: "example6.snek",
        expected: "overflow",
    },
    {
        name: example18_invalidarg,
        file: "example18.snek",
        input: "300",
        expected: "invalid argument",
    },
    {
        name: example13_invalidarg1,
        file: "example13.snek",
        expected: "invalid argument",
    },
    {
        name: example13_invalidarg2,
        file: "example13.snek",
        input: "false",
        expected: "invalid argument",
    },
    {
        name: example13_invalidarg3,
        file: "example13.snek",
        input: "true",
        expected: "invalid argument",
    },
    {
        name: example19_overflow1,
        file: "example19.snek",
        input: "9223372036854775807",
        expected: "overflow",
    },
    {
        name: example19_invalidarg1,
        file: "example19.snek",
        input: "dietz nuts",
        expected: "invalid argument",
    },
    {
        name: example20_invalidarg1,
        file: "example20.snek",
        input: "true",
        expected: "invalid argument",
    },
    {
        name: example20_invalidarg2,
        file: "example20.snek",
        input: "false",
        expected: "invalid argument",
    },
    {
        name: example20_invalidarg3,
        file: "example20.snek",
        expected: "invalid argument",
    },
    {
        name: example20_overflow1,
        file: "example20.snek",
        input: "-23",
        expected: "overflow",
    },
    {
        name: example20_overflow2,
        file: "example20.snek",
        input: "0",
        expected: "overflow",
    },
    {
        name: example20_overflow3,
        file: "example20.snek",
        input: "2345",
        expected: "overflow",
    },
    {
        name: fibonacci_invalidarg1,
        file: "fibonacci.snek",
        expected: "invalid argument",
    },
    {
        name: fibonacci_invalidarg2,
        file: "fibonacci.snek",
        input: "true",
        expected: "invalid argument",
    },
    {
        name: fibonacci_invalidarg3,
        file: "fibonacci.snek",
        input: "false",
        expected: "invalid argument",
    },
    {
        name: fibonacci_invalidarg4,
        file: "fibonacci.snek",
        input: "deez",
        expected: "invalid argument",
    },
    {
        name: fibonacci_overflow1,
        file: "fibonacci.snek",
        input: "124",
        expected: "overflow",
    },
    {
        name: fibonacci_overflow2,
        file: "fibonacci.snek",
        input: "99",
        expected: "overflow",
    },
    {
        name: arithmeticsum_overflow,
        file: "arithmeticsum.snek",
        input: "3037000500",
        expected: "overflow",
    },
    {
        name: geometricsum_overflow,
        file: "geometricsum.snek",
        input: "62",
        expected: "overflow",
    },
}

static_error_tests! {
    {
        name: number_bounds_fail,
        file: "number_bounds_fail.snek",
        expected: "Invalid",
    },
    {
        name: example8,
        file: "example8.snek",
        expected: "Invalid",
    },
    {
        name: example10,
        file: "example10.snek",
        expected: "keyword",
    },
    {
        name: example14,
        file: "example14.snek",
        expected: "Invalid",
    },
    {
        name: example15,
        file: "example15.snek",
        expected: "Unbound variable identifier x",
    },
    {
        name: example21,
        file: "example21.snek",
        expected: "break",
    },
    {
        name: example22_break,
        file: "example22.snek",
        expected: "break",
    },
    {
        name: example24_keyword,
        file: "example24.snek",
        expected: "keyword",
    },
}