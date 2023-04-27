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
        name: mul,
        file: "mul.snek",
        input: "42",
        expected: "84",
    },
    {
        name: let_set_block,
        file: "let_set_block.snek",
        expected: "91",
    },
    {
        name: fib5,
        file: "fibonacci.snek",
        input: "5",
        expected: "5",
    },
    {
        name: fib15,
        file: "fibonacci.snek",
        input: "15",
        expected: "610",
    },
    {
        name: isnum_true,
        file: "isnum_true.snek",
        expected: "true",
    },
    {
        name: isnum_false,
        file: "isnum_false.snek",
        expected: "false",
    },
    {
        name: isbool_true,
        file: "isbool_true.snek",
        expected: "true",
    },
    {
        name: isbool_false,
        file: "isbool_false.snek",
        expected: "false",
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
        name: let_eval,
        file: "let_eval.snek",
        expected: "5",
    },
    {
        name: fuzz00,
        file: "fuzz00.snek",
        expected: "89",
    },
    {
        name: fuzz01,
        file: "fuzz01.snek",
        expected: "-11042",
    },
    {
        name: fuzz02,
        file: "fuzz02.snek",
        expected: "70",
    },
    {
        name: fuzz03,
        file: "fuzz03.snek",
        expected: "-6785",
    },
    {
        name: fuzz04,
        file: "fuzz04.snek",
        expected: "28728355",
    },
    {
        name: fuzz05,
        file: "fuzz05.snek",
        expected: "56",
    },
    {
        name: fuzz06,
        file: "fuzz06.snek",
        expected: "70",
    },
    {
        name: fuzz07,
        file: "fuzz07.snek",
        expected: "44838983",
    },
    {
        name: fuzz08,
        file: "fuzz08.snek",
        expected: "-46",
    },
    {
        name: fuzz09,
        file: "fuzz09.snek",
        expected: "19",
    },
    {
        name: fuzz10,
        file: "fuzz10.snek",
        expected: "167",
    },
    {
        name: fuzz11,
        file: "fuzz11.snek",
        expected: "-53",
    },
    {
        name: fuzz12,
        file: "fuzz12.snek",
        expected: "-305",
    },
    {
        name: fuzz13,
        file: "fuzz13.snek",
        expected: "-1613065",
    },
    {
        name: fuzz14,
        file: "fuzz14.snek",
        expected: "2853",
    },
    {
        name: fuzz15,
        file: "fuzz15.snek",
        expected: "2040",
    },
    {
        name: fuzz16,
        file: "fuzz16.snek",
        expected: "-13043",
    },
    {
        name: fuzz17,
        file: "fuzz17.snek",
        expected: "3624",
    },
    {
        name: fuzz18,
        file: "fuzz18.snek",
        expected: "-29126",
    },
    {
        name: fuzz19,
        file: "fuzz19.snek",
        expected: "534",
    },
    {
        name: fuzz20,
        file: "fuzz20.snek",
        expected: "-47",
    },
    {
        name: fuzz21,
        file: "fuzz21.snek",
        expected: "-21",
    },
    {
        name: fuzz22,
        file: "fuzz22.snek",
        expected: "85",
    },
    {
        name: fuzz23,
        file: "fuzz23.snek",
        expected: "-18",
    },
    {
        name: fuzz24,
        file: "fuzz24.snek",
        expected: "159501",
    },
    {
        name: fuzz25,
        file: "fuzz25.snek",
        expected: "345205198",
    },
    {
        name: fuzz26,
        file: "fuzz26.snek",
        expected: "8",
    },
    {
        name: fuzz27,
        file: "fuzz27.snek",
        expected: "-126",
    },
    {
        name: fuzz28,
        file: "fuzz28.snek",
        expected: "-6",
    },
    {
        name: fuzz29,
        file: "fuzz29.snek",
        expected: "48196687",
    },
    {
        name: fuzz30,
        file: "fuzz30.snek",
        expected: "42",
    },
    {
        name: fuzz31,
        file: "fuzz31.snek",
        expected: "808",
    },
    {
        name: fuzz32,
        file: "fuzz32.snek",
        expected: "-47",
    },
    {
        name: fuzz33,
        file: "fuzz33.snek",
        expected: "32",
    },
    {
        name: fuzz34,
        file: "fuzz34.snek",
        expected: "-544",
    },
    {
        name: fuzz35,
        file: "fuzz35.snek",
        expected: "8",
    },
    {
        name: fuzz36,
        file: "fuzz36.snek",
        expected: "116",
    },
    {
        name: fuzz37,
        file: "fuzz37.snek",
        expected: "-8496140",
    },
    {
        name: fuzz38,
        file: "fuzz38.snek",
        expected: "68",
    },
    {
        name: fuzz39,
        file: "fuzz39.snek",
        expected: "-62",
    },
    {
        name: fuzz40,
        file: "fuzz40.snek",
        expected: "-2624",
    },
    {
        name: fuzz41,
        file: "fuzz41.snek",
        expected: "-62",
    },
    {
        name: fuzz42,
        file: "fuzz42.snek",
        expected: "-63118",
    },
    {
        name: fuzz43,
        file: "fuzz43.snek",
        expected: "-38",
    },
    {
        name: fuzz44,
        file: "fuzz44.snek",
        expected: "-80",
    },
    {
        name: fuzz45,
        file: "fuzz45.snek",
        expected: "-211960",
    },
    {
        name: fuzz46,
        file: "fuzz46.snek",
        expected: "-16578535",
    },
    {
        name: fuzz47,
        file: "fuzz47.snek",
        expected: "-8",
    },
    {
        name: fuzz48,
        file: "fuzz48.snek",
        expected: "-38958",
    },
    {
        name: fuzz49,
        file: "fuzz49.snek",
        expected: "34",
    },
    {
        name: fuzz50,
        file: "fuzz50.snek",
        expected: "1176",
    },
    {
        name: fuzz51,
        file: "fuzz51.snek",
        expected: "-43",
    },
    {
        name: fuzz52,
        file: "fuzz52.snek",
        expected: "-410498306",
    },
    {
        name: fuzz53,
        file: "fuzz53.snek",
        expected: "1004528",
    },
    {
        name: fuzz54,
        file: "fuzz54.snek",
        expected: "50383407",
    },
    {
        name: fuzz55,
        file: "fuzz55.snek",
        expected: "100",
    },
    {
        name: fuzz56,
        file: "fuzz56.snek",
        expected: "-160",
    },
    {
        name: fuzz57,
        file: "fuzz57.snek",
        expected: "42",
    },
    {
        name: fuzz58,
        file: "fuzz58.snek",
        expected: "70",
    },
    {
        name: fuzz59,
        file: "fuzz59.snek",
        expected: "53",
    },
    {
        name: fuzz60,
        file: "fuzz60.snek",
        expected: "-1101472731",
    },
    {
        name: fuzz61,
        file: "fuzz61.snek",
        expected: "-56",
    },
    {
        name: fuzz62,
        file: "fuzz62.snek",
        expected: "-99",
    },
    {
        name: fuzz63,
        file: "fuzz63.snek",
        expected: "47",
    },
    {
        name: fuzz64,
        file: "fuzz64.snek",
        expected: "61",
    },
    {
        name: fuzz65,
        file: "fuzz65.snek",
        expected: "-687302",
    },
    {
        name: fuzz66,
        file: "fuzz66.snek",
        expected: "76",
    },
    {
        name: fuzz67,
        file: "fuzz67.snek",
        expected: "-36",
    },
    {
        name: fuzz68,
        file: "fuzz68.snek",
        expected: "-77",
    },
    {
        name: fuzz69,
        file: "fuzz69.snek",
        expected: "1328841",
    },
    {
        name: fuzz70,
        file: "fuzz70.snek",
        expected: "246381414",
    },
    {
        name: fuzz71,
        file: "fuzz71.snek",
        expected: "66",
    },
    {
        name: fuzz72,
        file: "fuzz72.snek",
        expected: "25449",
    },
    {
        name: fuzz73,
        file: "fuzz73.snek",
        expected: "-31783666",
    },
    {
        name: fuzz74,
        file: "fuzz74.snek",
        expected: "89",
    },
    {
        name: fuzz75,
        file: "fuzz75.snek",
        expected: "49",
    },
    {
        name: fuzz76,
        file: "fuzz76.snek",
        expected: "133092074",
    },
    {
        name: fuzz77,
        file: "fuzz77.snek",
        expected: "-858618566",
    },
    {
        name: fuzz78,
        file: "fuzz78.snek",
        expected: "-1429288",
    },
    {
        name: fuzz79,
        file: "fuzz79.snek",
        expected: "-79",
    },
    {
        name: fuzz80,
        file: "fuzz80.snek",
        expected: "78",
    },
    {
        name: fuzz81,
        file: "fuzz81.snek",
        expected: "-19",
    },
    {
        name: fuzz82,
        file: "fuzz82.snek",
        expected: "-32",
    },
    {
        name: fuzz83,
        file: "fuzz83.snek",
        expected: "511",
    },
    {
        name: fuzz84,
        file: "fuzz84.snek",
        expected: "-42",
    },
    {
        name: fuzz85,
        file: "fuzz85.snek",
        expected: "-2372",
    },
    {
        name: fuzz86,
        file: "fuzz86.snek",
        expected: "10",
    },
    {
        name: fuzz87,
        file: "fuzz87.snek",
        expected: "-44",
    },
    {
        name: fuzz88,
        file: "fuzz88.snek",
        expected: "1724973424",
    },
    {
        name: fuzz89,
        file: "fuzz89.snek",
        expected: "-1574270059",
    },
    {
        name: fuzz90,
        file: "fuzz90.snek",
        expected: "-58",
    },
    {
        name: fuzz91,
        file: "fuzz91.snek",
        expected: "5",
    },
    {
        name: fuzz92,
        file: "fuzz92.snek",
        expected: "-15339",
    },
    {
        name: fuzz93,
        file: "fuzz93.snek",
        expected: "29",
    },
    {
        name: fuzz94,
        file: "fuzz94.snek",
        expected: "66",
    },
    {
        name: fuzz95,
        file: "fuzz95.snek",
        expected: "65",
    },
    {
        name: fuzz96,
        file: "fuzz96.snek",
        expected: "-86",
    },
    {
        name: fuzz97,
        file: "fuzz97.snek",
        expected: "-709399218",
    },
    {
        name: fuzz98,
        file: "fuzz98.snek",
        expected: "85",
    },
    {
        name: fuzz99,
        file: "fuzz99.snek",
        expected: "-67",
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
        name: add_overflow,
        file: "add_overflow.snek",
        expected: "overflow",
    },
    {
        name: add1_overflow,
        file: "add1_overflow.snek",
        expected: "overflow",
    },
    {
        name: sub_overflow,
        file: "sub_overflow.snek",
        expected: "overflow",
    },
    {
        name: sub1_overflow,
        file: "sub1_overflow.snek",
        expected: "overflow",
    },
    {
        name: mul_overflow,
        file: "mul.snek",
        input: "2305843009213693952",
        expected: "overflow",
    },
    {
        name: mul_argument,
        file: "mul.snek",
        input: "true",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_00,
        file: "invalid_argument_00.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_01,
        file: "invalid_argument_01.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_02,
        file: "invalid_argument_02.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_03,
        file: "invalid_argument_03.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_04,
        file: "invalid_argument_04.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_05,
        file: "invalid_argument_05.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_06,
        file: "invalid_argument_06.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_07,
        file: "invalid_argument_07.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_08,
        file: "invalid_argument_08.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_09,
        file: "invalid_argument_09.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_10,
        file: "invalid_argument_10.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_11,
        file: "invalid_argument_11.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_12,
        file: "invalid_argument_12.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_13,
        file: "invalid_argument_13.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_14,
        file: "invalid_argument_14.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_15,
        file: "invalid_argument_15.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_16,
        file: "invalid_argument_16.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_17,
        file: "invalid_argument_17.snek",
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
        expected: "Unbound variable identifier x",
    },
    {
        name: duplicate_binding,
        file: "duplicate_binding.snek",
        expected: "Duplicate binding",
    },
    {
        name: let_eval_2,
        file: "let_eval_2.snek",
        expected: "Unbound variable identifier x",
    },
    {
        name: invalid_id_1,
        file: "invalid_id_1.snek",
        expected: "keyword",
    },
    {
        name: invalid_id_2,
        file: "invalid_id_2.snek",
        expected: "keyword",
    },
    {
        name: invalid_id_3,
        file: "invalid_id_3.snek",
        expected: "keyword",
    },
    {
        name: invalid_id_4,
        file: "invalid_id_4.snek",
        expected: "keyword",
    },
    {
        name: invalid_id_5,
        file: "invalid_id_5.snek",
        expected: "keyword",
    },
    {
        name: invalid_id_6,
        file: "invalid_id_6.snek",
        expected: "keyword",
    },
    {
        name: invalid_op1,
        file: "invalid_op1.snek",
        expected: "parse error: Invalid unary op ?",
    },
    {
        name: invalid_op2,
        file: "invalid_op2.snek",
        expected: "parse error: Invalid binary op ?",
    },
    {
        name: invalid_op3,
        file: "invalid_op3.snek",
        expected: "parse error",
    },
    {
        name: invalid_bind_1,
        file: "invalid_bind_1.snek",
        expected: "keyword",
    },
    {
        name: invalid_bind_2,
        file: "invalid_bind_2.snek",
        expected: "parse error: Invalid bind",
    },
    {
        name: invalid_float,
        file: "invalid_float.snek",
        expected: "parse error",
    },
    {
        name: parse_let_nobindings_fail,
        file: "parse_let_nobindings_fail.snek",
        expected: "Invalid",
    },
    {
        name: parse_sexp_fail,
        file: "parse_sexp_fail.snek",
        expected: "Invalid",
    },
}
