mod infra;

success_tests! {
	{
		name: factorial_12,
		file: "factorial.snek",
		input: "2",
		expected: "2",
	},
	{
		name: factorial_13,
		file: "factorial.snek",
		input: "3",
		expected: "6",
	},
	{
		name: factorial_14,
		file: "factorial.snek",
		input: "20",
		expected: "2432902008176640000",
	},
	{
		name: letset_1,
		file: "letset.snek",
		expected: "5",
	},
	{
		name: loop1_17,
		file: "loop1.snek",
		expected: "-6",
	},
	{
		name: add1_1,
		file: "add1.snek",
		expected: "73",
	},
	{
		name: nested_binding_1,
		file: "nested_binding.snek",
		expected: "30",
	},
	{
		name: bool_2_1,
		file: "bool_2.snek",
		expected: "false",
	},
	{
		name: bool_2_3,
		file: "bool_2.snek",
		input: "true",
		expected: "true",
	},
	{
		name: bool_2_4,
		file: "bool_2.snek",
		input: "false",
		expected: "false",
	},
	{
		name: set_1_3,
		file: "set_1.snek",
		expected: "true",
	},
	{
		name: set_1_4,
		file: "set_1.snek",
		input: "true",
		expected: "false",
	},
	{
		name: set_1_5,
		file: "set_1.snek",
		input: "5",
		expected: "25",
	},
	{
		name: set_1_6,
		file: "set_1.snek",
		input: "-5",
		expected: "25",
	},
	{
		name: complex_1_13,
		file: "complex_1.snek",
		expected: "4",
	},
	{
		name: long_int_1,
		file: "long_int.snek",
		expected: "4611686018427387903",
	},
	{
		name: multiply_1_1,
		file: "multiply_1.snek",
		input: "3",
		expected: "15",
	},
	{
		name: multiply_1_2,
		file: "multiply_1.snek",
		input: "-3",
		expected: "-15",
	},
	{
		name: multiply_1_3,
		file: "multiply_1.snek",
		input: "0",
		expected: "0",
	},
	{
		name: if_input_unknown_1,
		file: "if_input_unknown.snek",
		expected: "1",
	},
	{
		name: if_input_unknown_2,
		file: "if_input_unknown.snek",
		input: "true",
		expected: "-1",
	},
	{
		name: if_input_unknown_3,
		file: "if_input_unknown.snek",
		input: "2",
		expected: "-1",
	},
	{
		name: input_compare_2,
		file: "input_compare.snek",
		input: "2",
		expected: "false",
	},
	{
		name: input_compare_3,
		file: "input_compare.snek",
		input: "6",
		expected: "true",
	},
	{
		name: binding_1,
		file: "binding.snek",
		expected: "5",
	},
	{
		name: multiply_2_1,
		file: "multiply_2.snek",
		input: "3",
		expected: "15",
	},
	{
		name: multiply_2_2,
		file: "multiply_2.snek",
		input: "-3",
		expected: "-15",
	},
	{
		name: multiply_2_3,
		file: "multiply_2.snek",
		input: "0",
		expected: "0",
	},
	{
		name: level_binding_1,
		file: "level_binding.snek",
		expected: "9",
	},
	{
		name: long_int_opr_1,
		file: "long_int_opr.snek",
		input: "4611686018427387902",
		expected: "4611686018427387903",
	},
	{
		name: isbool_1,
		file: "isbool.snek",
		expected: "2",
	},
	{
		name: bool_1_2,
		file: "bool_1.snek",
		input: "3",
		expected: "true",
	},
	{
		name: bool_1_3,
		file: "bool_1.snek",
		input: "4",
		expected: "false",
	},
	{
		name: set_2_1,
		file: "set_2.snek",
		expected: "2",
	},
	{
		name: modulo_11,
		file: "modulo.snek",
		input: "2",
		expected: "2",
	},
	{
		name: modulo_12,
		file: "modulo.snek",
		input: "3",
		expected: "3",
	},
	{
		name: modulo_13,
		file: "modulo.snek",
		input: "4",
		expected: "0",
	},
	{
		name: modulo_14,
		file: "modulo.snek",
		input: "7",
		expected: "3",
	},
	{
		name: modulo_15,
		file: "modulo.snek",
		input: "8",
		expected: "0",
	},
	{
		name: modulo_16,
		file: "modulo.snek",
		input: "24",
		expected: "0",
	},
	{
		name: modulo_17,
		file: "modulo.snek",
		input: "25",
		expected: "1",
	},
	{
		name: modulo_18,
		file: "modulo.snek",
		input: "30",
		expected: "2",
	},
	{
		name: complex_2_26,
		file: "complex_2.snek",
		input: "-1",
		expected: "4",
	},
	{
		name: complex_2_27,
		file: "complex_2.snek",
		input: "0",
		expected: "8",
	},
	{
		name: complex_2_28,
		file: "complex_2.snek",
		input: "1",
		expected: "12",
	},
	{
		name: complex_2_29,
		file: "complex_2.snek",
		input: "2",
		expected: "16",
	},
	{
		name: complex_2_30,
		file: "complex_2.snek",
		input: "3",
		expected: "20",
	},
	{
		name: complex_2_31,
		file: "complex_2.snek",
		input: "7",
		expected: "20",
	},
	{
		name: complex_2_32,
		file: "complex_2.snek",
		input: "10",
		expected: "16",
	},
	{
		name: complex_2_33,
		file: "complex_2.snek",
		input: "20",
		expected: "8",
	},
	{
		name: nested_arith_1,
		file: "nested_arith.snek",
		expected: "25",
	},
	{
		name: binding_chain_1,
		file: "binding_chain.snek",
		expected: "-1",
	},
	{
		name: mul_1,
		file: "mul.snek",
		input: "2",
		expected: "4",
	},
	{
		name: mul_2,
		file: "mul.snek",
		input: "2305843009213693951",
		expected: "4611686018427387902",
	},
	{
		name: block_set_2,
		file: "block_set.snek",
		expected: "6",
	},
	{
		name: if_input_known_1,
		file: "if_input_known.snek",
		expected: "-1",
	},
	{
		name: false_val_1,
		file: "false_val.snek",
		expected: "false",
	},
	{
		name: nested_binding_arith_11,
		file: "nested_binding_arith.snek",
		expected: "170",
	},
	{
		name: loop2_3,
		file: "loop2.snek",
		expected: "2",
	},
	{
		name: add_1,
		file: "add.snek",
		expected: "15",
	},
}

runtime_error_tests! {
	{
		name: factorial_15,
		file: "factorial.snek",
		input: "21",
		expected: "overflow",
	},
	{
		name: factorial_16,
		file: "factorial.snek",
		expected: "invalid argument",
	},
	{
		name: invalid_argument_1,
		file: "invalid_argument.snek",
		expected: "invalid argument",
	},
	{
		name: bool_2_2,
		file: "bool_2.snek",
		input: "3",
		expected: "invalid",
	},
	{
		name: input_compare_1,
		file: "input_compare.snek",
		expected: "invalid argument",
	},
	{
		name: long_int_opr_2,
		file: "long_int_opr.snek",
		input: "4611686018427387903",
		expected: "overflow",
	},
	{
		name: bool_1_1,
		file: "bool_1.snek",
		expected: "invalid",
	},
	{
		name: mul_3,
		file: "mul.snek",
		input: "2305843009213693952",
		expected: "overflow",
	},
}

static_error_tests! {
	{
		name: sexp_fail_1,
		file: "sexp_fail.snek",
		expected: "Invalid",
	},
	{
		name: improperargslet_1,
		file: "improperargslet.snek",
		expected: "keyword",
	},
	{
		name: duplicate_binding_1,
		file: "duplicate_binding.snek",
		expected: "Duplicate binding",
	},
	{
		name: no_binding_1,
		file: "no_binding.snek",
		expected: "Invalid",
	},
	{
		name: number_bounds_fail_1,
		file: "number_bounds_fail.snek",
		expected: "Invalid",
	},
	{
		name: invalid_string_1,
		file: "invalid_string.snek",
		expected: "Invalid",
	},
	{
		name: unbound_id_1,
		file: "unbound_id.snek",
		expected: "Unbound variable identifier x",
	},
}

