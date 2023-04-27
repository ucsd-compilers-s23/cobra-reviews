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
      name: simple_if,
      file: "simple_if.snek",
      expected: "10",
    },
    {
      name: simple_if_greater,
      file: "simple_if_greater.snek",
      expected: "40",
    },
    {
      name: simple_if_less,
      file: "simple_if_less.snek",
      expected: "21",
    },
    {
      name: simple_if_less_equal,
      file: "simple_if_less_equal.snek",
      expected: "21",
    },
    {
      name: simple_if_greater_equal,
      file: "simple_if_greater_equal.snek",
      expected: "12",
    }, 
    {
      name: if_false,
      file: "if_false.snek",
      expected: "1",
    }, 
    {
      name: set_1,
      file: "set_1.snek",
      expected: "1",
    }, 
    {
      name: loop_1,
      file: "loop_1.snek",
      expected: "5",
    },
    {
      name: block_1,
      file: "block_1.snek",
      expected: "9",
    },
    {
      name: is_bool,
      file: "is_bool.snek",
      expected: "false",
    }, 
    {
      name: if_num,
      file: "if_num.snek",
      expected: "-10",
    },
    {
      name: nested_loop_1,
      file: "nested_loop_1.snek",
      expected: "-6",
    },
    {
      name: factorial_1,
      file: "factorial.snek",
      input: "4",
      expected: "24",
    },
    {
      name: neg_mul,
      file: "neg_mul.snek",
      expected: "-10",
    },
    {
      name: neg_mul_2,
      file: "neg_mul_2.snek",
      expected: "24",
    },
    {
      name: nested_let,
      file: "nested_let.snek",
      expected: "7",
    },
    {
      name: nested_let_2,
      file: "nested_let_2.snek",
      expected: "10",
    },
    {
      name: multiple_binding,
      file: "multiple_binding.snek",
      expected: "5",
    },
    {
      name: shadow_test,
      file: "shadow_test.snek",
      expected: "true",
    }, 
    //Idea to test IsBool with input taken from: https://edstem.org/us/courses/38748/discussion/3016787
    {
      name: is_bool_input,
      file: "is_bool_input.snek",
      input: "5",
      expected: "false",
    }, 
    {
      name: shadow_test_2,
      file: "shadow_test_2.snek",
      expected: "5",
    }, 
    {
      name: shadow_test_3,
      file: "shadow_test_3.snek",
      expected: "93",
    }, 
    {
      name: shadow_test_4,
      file: "shadow_test_4.snek",
      expected: "21",
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
    name: add1_overflow,
    file: "add1_overflow.snek",
    expected: "overflow",
  },
  {
    name: mul_overflow,
    file: "mul_overflow.snek",
    expected: "overflow",
  },
  {
    name: invalid_input,
    file: "invalid_input.snek",
    input: "BAD_INPUT",
    expected: "invalid input",
  },
  {
    name: compare_boolean,
    file: "compare_boolean.snek",
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
    name: unbound_set,
    file: "unbound_set.snek",
    expected: "Unbound variable identifier",
  },
  {
    name: break_without_loop,
    file: "break_without_loop.snek",
    expected: "break without loop",
  }, 
  {
    name: empty_block,
    file: "empty_block.snek",
    expected: "invalid use of keyword",
  }, 
  {
    name: two_block,
    file: "two_block.snek",
    expected: "Invalid",
  }, 
}
