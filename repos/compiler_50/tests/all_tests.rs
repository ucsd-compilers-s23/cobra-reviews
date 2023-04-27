mod infra;

// Your tests go here!
success_tests! {
    {
        name: false_val,
        file: "false_val",
        expected: "false",
    },
    {
        name: if_test,
        file: "if_test",
        input: "true",
        expected: "1",
    },
    {
        name: if_test2,
        file: "if_test2",
        input: "false",
        expected: "100",
    },
    {
        name: if_test3,
        file: "if_test3",
        input: "false",
        expected: "-7",
    },
    {
        name: isbool,
        file: "isbool",
        expected: "true",
    },
    {
        name: isbool2,
        file: "isbool2",
        expected: "false",
    },
    {
        name: isnum,
        file: "isnum",
        expected: "true",
    },
    {
        name: isnum2,
        file: "isnum2",
        expected: "false",
    },
    {
        name: compare,
        file: "compare",
        expected: "true",
    },
    {
        name: example1,
        file: "example1",
        expected: "6",
    },
    {
        name: example2,
        file: "example2",
        expected: "-6",
    },
    {
        name: example3,
        file: "example3",
        input: "5",
        expected: "120",
    },
    {
        name: loop_test,
        file: "loop",
        expected: "55",
    },
    {
        name: add1,
        file: "add1",
        expected: "73",
    },
    {
        name: sub1,
        file: "sub1",
        expected: "73",
    },
    {
        name: add,
        file: "add",
        expected: "15",
    },
    {
        name: sub,
        file: "sub",
        expected: "-5",
    },
    {
        name: mul,
        file: "mul",
        expected: "50",
    },
    {
        name: nested_arith,
        file: "nested_arith",
        expected: "25",
    },
    {
        name: nested_arith2,
        file: "nested_arith2",
        expected: "135",
    },
    {
        name: binding,
        file: "binding",
        expected: "5",
    },
    {
        name: binding2,
        file: "binding2",
        expected: "6",
    },
    {
        name: binding3,
        file: "binding3",
        expected: "20",
    },
    {
        name: binding4,
        file: "binding4",
        expected: "12",
    },
    {
        name: binding5,
        file: "binding5",
        expected: "21",
    },
    {
        name: binding6,
        file: "binding6",
        expected: "1400",
    },
    {
        name: misc,
        file: "misc",
        expected: "5",
    },
    {
        name: misc2,
        file: "misc2",
        expected: "35",
    },
    {
        name: misc3,
        file: "misc3",
        expected: "31",
    },
    {
        name: misc4,
        file: "misc4",
        expected: "-7",
    },
    {
        name: misc5,
        file: "misc5",
        expected: "171",
    },
    {
        name: shadowed,
        file: "shadowed",
        expected: "111",
    },
    {
        name: shadowed2,
        file: "shadowed2",
        expected: "6",
    },
    {
        name: shadowed3,
        file: "shadowed3",
        expected: "35",
    },
    {
        name: set,
        file: "set",
        expected: "8",
    },
    {
        name: block,
        file: "block",
        expected: "30",
    },
}

runtime_error_tests! {
    {
        name: invalid_argument,
        file: "invalid_argument",
        expected: "invalid argument",
    },
    {
        name: input_compare,
        file: "input_compare",
        input: "true",
        expected: "invalid argument",
    },
    {
        name: overflow,
        file: "overflow",
        expected: "overflow",
    },
    {
        name: if_test4,
        file: "if_test4",
        input: "false",
        expected: "invalid argument",
    },
}

static_error_tests! {
    {
        name: number_bounds_fail,
        file: "number_bounds_fail",
        expected: "Invalid",
    },
    {
        name: invalid_break,
        file: "invalid_break",
        expected: "Invalid break",
    },
    {
        name: unbound_id,
        file: "unbound_id",
        expected: "Unbound variable identifier x",
    },
    {
        name: unbound_id2,
        file: "unbound_id2",
        expected: "Unbound variable identifier x",
    },
    {
        name: unbound_id3,
        file: "unbound_id3",
        expected: "Unbound variable identifier z",
    },
    {
        name: unbound_id4,
        file: "unbound_id4",
        expected: "Unbound variable identifier y",
    },
    {
        name: duplicate_binding,
        file: "duplicate_binding",
        expected: "Duplicate binding",
    },
    {
        name: invalid_id,
        file: "invalid_id",
        expected: "Invalid: identifier matches keyword",
    },
    {
        name: invalid_op,
        file: "invalid_op",
        expected: "Invalid",
    },
    {
        name: invalid_op_format,
        file: "invalid_op_format",
        expected: "Invalid",
    },
    {
        name: invalid_id_format,
        file: "invalid_id_format",
        expected: "Invalid",
    },
    {
        name: sexp,
        file: "sexp",
        expected: "Invalid",
    },
    {
        name: let_nobindings,
        file: "let_nobindings",
        expected: "Invalid",
    },
    {
        name: let_improperargs,
        file: "let_improperargs",
        expected: "invalid argument",
    },
    {
        name: let_improperargs2,
        file: "let_improperargs2",
        expected: "Invalid",
    },
}
