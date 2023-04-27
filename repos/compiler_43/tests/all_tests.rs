mod infra;

// Your tests go here!
success_tests! {

    // Atom tests 
    {
        name: true_bool,
        file: "atoms/true_bool.snek",
        expected: "true",
    },
    {
        name: false_bool,
        file: "atoms/false_bool.snek",
        expected: "false",
    },
    {
        name: pos_num,
        file: "atoms/pos_num.snek",
        expected: "100",
    },
    
    {
        name: neg_num,
        file: "atoms/neg_num.snek",
        expected: "-100",
    },

    // Unop tests
    {
        name: add1_1,
        file:"unops/add1_1.snek",
        expected:"100001"
    },
    {
        name: add1_2,
        file:"unops/add1_2.snek",
        expected:"2"
    },
    {
        name: add1_3,
        file:"unops/add1_3.snek",
        expected:"2"
    },
    {
        name: sub1_1,
        file:"unops/sub1_1.snek",
        expected:"-100001"
    },
    {
        name: sub1_2,
        file:"unops/sub1_2.snek",
        expected:"-2"
    },
    {
        name: sub1_3,
        file:"unops/sub1_3.snek",
        expected:"-2"
    },
    {
        name: isnum_1,
        file:"unops/isnum_1.snek",
        expected:"true"
    },
    {
        name: isnum_2,
        file:"unops/isnum_2.snek",
        expected:"true"
    },
    {
        name: isnum_3,
        file:"unops/isnum_3.snek",
        expected:"false"
    },
    {
        name: isnum_4,
        file:"unops/isnum_4.snek",
        expected:"false"
    },
    {
        name: isbool_1,
        file:"unops/isbool_1.snek",
        expected:"true"
    },
    {
        name: isbool_2,
        file:"unops/isbool_2.snek",
        expected:"true"
    },
    {
        name: isbool_3,
        file:"unops/isbool_3.snek",
        expected:"true"
    },
    {
        name: isbool_4,
        file:"unops/isbool_4.snek",
        expected:"false"
    },
    {
        name: isbool_5,
        file:"unops/isbool_5.snek",
        expected:"false"
    },

    // Binop tests
    {
        name: cmp_leq_1,
        file:"binops/cmp_leq_1.snek",
        expected:"false"
    },
    {
        name: cmp_leq_2,
        file:"binops/cmp_leq_2.snek",
        expected:"true"
    },
    {
        name: cmp_leq_3,
        file:"binops/cmp_leq_3.snek",
        expected:"true"
    },
    {
        name: add_1,
        file:"binops/add_1.snek",
        expected:"20"
    },
    {
        name: cmp_g_1,
        file:"binops/cmp_g_1.snek",
        expected:"true"
    },
    {
        name: cmp_g_2,
        file:"binops/cmp_g_2.snek",
        expected:"false"
    },
    {
        name: cmp_g_3,
        file:"binops/cmp_g_3.snek",
        expected:"false"
    },
    {
        name: sub_1,
        file:"binops/sub_1.snek",
        expected:"10"
    },
    {
        name: cmp_l_1,
        file:"binops/cmp_l_1.snek",
        expected:"false"
    },
    {
        name: cmp_l_2,
        file:"binops/cmp_l_2.snek",
        expected:"false"
    },
    {
        name: cmp_l_3,
        file:"binops/cmp_l_3.snek",
        expected:"true"
    },
    {
        name: mul_1,
        file:"binops/mul_1.snek",
        expected:"0"
    },
    {
        name: mul_2,
        file:"binops/mul_2.snek",
        expected:"900"
    },
    {
        name: mul_3,
        file:"binops/mul_3.snek",
        expected:"-60"
    },
    {
        name: mul_4,
        file:"binops/mul_4.snek",
        expected:"12"
    },
    {
        name: cmp_eq_1,
        file:"binops/cmp_eq_1.snek",
        expected:"true"
    },
    {
        name: cmp_eq_2,
        file:"binops/cmp_eq_2.snek",
        expected:"false"
    },
    {
        name: cmp_eq_3,
        file:"binops/cmp_eq_3.snek",
        expected:"true"
    },
    {
        name: cmp_eq_4,
        file:"binops/cmp_eq_4.snek",
        expected:"true"
    },
    {
        name: cmp_eq_5,
        file:"binops/cmp_eq_5.snek",
        expected:"false"
    },
    {
        name: cmp_geq_1,
        file:"binops/cmp_geq_1.snek",
        expected:"true"
    },
    {
        name: cmp_geq_2,
        file:"binops/cmp_geq_2.snek",
        expected:"true"
    },
    {
        name: cmp_geq_3,
        file:"binops/cmp_geq_3.snek",
        expected:"false"
    },


    // Let-binding tests
    {
        name: id_to_id_assign_1,
        file:"let/id_to_id_assign_1.snek",
        expected:"5"
    },
    {
        name: id_to_id_assign_2,
        file:"let/id_to_id_assign_2.snek",
        expected:"15"
    },
    {
        name: id_to_id_assign_3,
        file:"let/id_to_id_assign_3.snek",
        expected:"14"
    },
    {
        name: lec5_let_1,
        file:"let/lec5_let_1.snek",
        expected:"20"
    },
    {
        name: lec5_let_2,
        file:"let/lec5_let_2.snek",
        expected:"12"
    },
    {
        name: lec5_let_3,
        file:"let/lec5_let_3.snek",
        expected:"21"
    },

    
    // Control flow tests
    {
        name: if_1,
        file:"control_flow/if_1.snek",
        expected:"1"
    },
    {
        name: if_2,
        file:"control_flow/if_2.snek",
        expected:"2"
    },
    {
        name: if_3,
        file:"control_flow/if_3.snek",
        expected:"1"
    },
    {
        name: if_4,
        file:"control_flow/if_4.snek",
        expected:"1"
    },

    // Write up tests
    {
        name: write_up_ex_1,
        file:"public/write_up_ex_1.snek",
        expected:"6",
    },
    {
        name: write_up_ex_2,
        file:"public/write_up_ex_2.snek",
        expected:"-6",
    },
    {
        name: write_up_ex_3a,
        file:"public/write_up_ex_3.snek",
        input: "1",
        expected:"1",
    },
    {
        name: write_up_ex_3b,
        file:"public/write_up_ex_3.snek",
        input: "5",
        expected:"120",
    },
    {
        name: write_up_ex_3c,
        file:"public/write_up_ex_3.snek",
        input: "10",
        expected:"3628800",
    },

    // Public tests
    {
        name: false_val,
        file: "public/false_val.snek",
        expected: "false",
    },
    {
        name: input_compare_1,
        file: "public/input_compare.snek",
        input: "2",
        expected: "false",
    },
    {
        name: input_compare_2,
        file: "public/input_compare.snek",
        input: "10",
        expected: "true",
    },
}

runtime_error_tests! {
    {
        name: invalid_argument,
        file: "public/invalid_argument.snek",
        expected: "invalid argument",
    },
    {
        name: input_compare_3,
        file: "public/input_compare.snek",
        input: "true",
        expected: "invalid argument",
    },
}

static_error_tests! {
    {
        name: number_bounds_fail,
        file: "public/number_bounds_fail.snek",
        expected: "Invalid",
    }
}
