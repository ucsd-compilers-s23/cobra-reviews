mod infra;

// Your tests go here!
success_tests! {
    {
        name:add1 ,
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
        expected: "1793",
    },
    {
        name: binding,
        file: "binding.snek",
        expected: "5",
    },
    {
        name: nested_let,
        file: "nested_let.snek",
        expected: "25",
    },
    {
        name: nested_let2,
        file: "nested_let2.snek",
        expected: "-90",
    },
    {
        name: just_31,
        file: "just_31.snek",
        expected: "31",
    },
    {
        name: sub1_sub1_add1,
        file: "sub1_sub1_add1.snek",
        expected: "72",
    },
    {
        name: times,
        file: "times.snek",
        expected: "50",
    },
    {
        name: minus_times,
        file: "minus_times.snek",
        expected: "74",
    },
    {
        name: double_let,
        file: "double_let.snek",
        expected: "30",
    },
    {
        name: alldressing,
        file: "alldressing.snek",
        expected: "9",
    },
    {
        name: allbin,
        file: "allbin.snek",
        expected: "-1",
    },
    {
        name: so_many_bindings,
        file: "so_many_bindings.snek",
        expected: "20",
    },
    {
        name: double_bind,
        file: "double_bind.snek",
        expected: "20",
    },
    {
        name: let_inception,
        file: "let_inception.snek",
        expected: "30",
    },
    {
        name: bind_to_bind,
        file: "bind_to_bind.snek",
        expected: "5",
    },

    {
        name: false_val,
        file: "false_val.snek",
        expected: "false",
    },
    {
        name: equal_yes,
        file: "equal_yes.snek",
        expected: "true",
    },
    {
        name: equal_no,
        file: "equal_no.snek",
        expected: "false",
    },
    {
        name: greater,
        file: "greater.snek",
        expected: "true",
    },
    {
        name: greaterequal,
        file: "greaterequal.snek",
        expected: "true",
    },
    {
        name: less,
        file: "less.snek",
        expected: "true",
    },
    {
        name: lessequal,
        file: "lessequal.snek",
        expected: "true",
    },

    {
        name: notgreater,
        file: "notgreater.snek",
        expected: "false",
    },
    {
        name: notgreaterequal,
        file: "notgreaterequal.snek",
        expected: "false",
    },
    {
        name: notless,
        file: "notless.snek",
        expected: "false",
    },
    {
        name: notlessequal,
        file: "notlessequal.snek",
        expected: "false",
    },
    {
        name: isnum,
        file: "isnum.snek",
        expected: "true",
    },
    {
        name: isnotnum,
        file: "isnotnum.snek",
        expected: "false",
    },
    {
        name: block_set,
        file: "block_set.snek",
        expected: "6",
    },
    {
        name: isbool,
        file: "isbool.snek",
        expected: "true",
    },
    {
        name: looping,
        file: "looping.snek",
        expected: "-6",
    },
    {
        name: looping2,
        file: "looping2.snek",
        input: "4",
        expected: "24",
    },
    {
        name: isnotbool,
        file: "isnotbool.snek",
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
        name: input_check,
        file: "input_check.snek",
        input: "10",
        expected: "true",
    },
}

runtime_error_tests! {

    {
        name: equal_mismatch,
        file: "equal_mismatch.snek",
        expected: "invalid argument",
    },
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
        name: duplicate_binding,
        file: "duplicate_binding.snek",
        expected: "Duplicate binding",
    },
    {
        name: mismatch,
        file: "mismatch.snek",
        expected: "Invalid",
    },
    {
        name: parse_empty,
        file: "parse_empty.snek",
        expected: "Invalid",
    },

    {
        name: binding_empty,
        file: "binding_empty.snek",
        expected: "Invalid",
    },
    {
        name: let_unbound,
        file: "let_unbound.snek",
        expected: "Unbound variable identifier x",
    },
    {
        name: unbound_id,
        file: "unbound_id.snek",
        expected: "Unbound variable identifier x",
    },
    {
        name: number_bounds_fail,
        file: "number_bounds_fail.snek",
        expected: "overflow",
    },
    {
        name: no_binding,
        file: "no_binding.snek",
        expected: "Invalid",
    },
    {
        name: brake_fail,
        file: "brake_fail.snek",
        expected: "break",
    },
    
}
