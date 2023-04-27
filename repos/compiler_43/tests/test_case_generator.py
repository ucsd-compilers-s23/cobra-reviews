import json
TEST_DIR_PATH = "/home/linux/ieng6/oce/7j/ecflores/Desktop/CSE-131/A3/cobra-emmanuelclflores/tests"

def generate_tests(tests_dict=None):


    for test_subdir in tests_dict:

        for test_group, tests in tests_dict[test_subdir].items():

            test_name_prefix = test_group

            for i, test in enumerate(tests):    

                test_name = "%s_%d" % (test_name_prefix, i + 1)

                relative_test_file_path = "%s/%s.snek" % (test_subdir, test_name)
 
                program, expected_output = test

                generate_test_file(test_subdir, test_name, program)

                print_test_file_config(test_name, relative_test_file_path, expected_output)

def print_test_file_config(test_name, relative_test_file_path, expected_output):
    print("""{
    name: %s,
    file:\"%s\",
    expected:\"%s\"
},""" % (test_name, relative_test_file_path, expected_output))

def generate_test_file(test_subdir=None,test_name=None, program=None):

    test_file_path = TEST_DIR_PATH
    if test_subdir is not None:
       test_file_path += "/%s" % (test_subdir)
    test_file_path += "/%s.snek" % (test_name)

    # Write program to test file 
    with open(test_file_path, 'w') as tf:
        tf.write(program)



"""
Tests dictionary of form 

{
    <test_subdir> : {
            <test_group> : [
            (<program>, <expected output>),
            (<program>, <expected output>),
            ...
        ]
    }
}
"""
tests_dict =  {}
# tests_dict["unops"] = {
    # "add1" : [
    #         ("(add1 100000)", "100001"),
    #         ("(add1 1)", "2"),
    #         ("(add1 (add1 (add1 -1)))", "2"),
    #     ],
    # "sub1" : [
    #         ("(sub1 -100000)", "-100001"),
    #         ("(sub1 -1)", "-2"),
    #         ("(sub1 (sub1 (sub1 1)))", "-2"),
    #     ],
    # "isnum" : [
    #         ("(isnum 23)", "true"),
    #         ("(isnum (add1 (sub1 7)))", "true"),
    #         ("(isnum (isnum (add1 (sub1 7))))", "false"),
    #         ("(isnum  false)", "false"),
    #     ],
    # "isbool" : [
    #         ("(isbool true)", "true"),
    #         ("(isbool (isnum (isbool 7)))", "true"),
    #         ("(isbool (isbool (add1 (sub1 7))))", "true"),
    #         ("(isbool  3)", "false"),
    #         ("(isbool  (sub1 0))", "false"),
    #     ]
# }

# tests_dict["binops"] = {
#     "add" : [
#             ("(+ 10 10)", "20"),
#         ],
#     "sub" : [
#             ("(- 20 10)", "10"),
#         ],
#     "mul" : [
#             ("(* 1 0)", "0"),
#             ("(* 30 30)", "900"),
#             ("(* 15 -4)", "-60"),
#             ("(* -3 -4)", "12"),
#     ],
#     "cmp_eq" : [
#         ("(= 2 2)", "true"),
#         ("(= 2 -1)", "false"),
#         ("(= (* 2 3) (+ 3 3))", "true"),
#         ("(= true true)", "true"),
#         ("(= true false)", "false"),
#     ],
#     "cmp_g" : [
#         ("(> 2 1)", "true"),
#         ("(> 2 2)", "false"),
#         ("(> 2 3)", "false"),
#     ],
#     "cmp_geq" : [
#         ("(>= 2 1)", "true"),
#         ("(>= 2 2)", "true"),
#         ("(> 2 3)", "false"),
#     ],
#     "cmp_l" : [
#         ("(< 2 1)", "false"),
#         ("(< 2 2)", "false"),
#         ("(< 2 3)", "true"),
#     ],
#     "cmp_leq" : [
#         ("(<= 2 1)", "false"),
#         ("(<= 2 2)", "true"),
#         ("(<= 2 3)", "true"),
#     ]
# }


# tests_dict["let"] = {
#     "lec5_let": [
#             ("(let ((x 10)) (let ((y 10)) (+ x y)))", "20"),
#             ("(let ((x (let ((y 10)) (add1 y)))) (add1 x))", "12"),
#             ("(let ((x 10)) (let ((x (add1 x))) (+ x 10)))", "21"),

#             ],
#     "id_to_id_assign": [
#         ("(let ((x 5) (z x)) z)", "5"),
#         ("(let ((x 5) (z x) (y z)) (+ (+ x y) z))", "15"),
#         ("(let ((x 2) (z (* x 2)) (y (* z 2))) (+ (+ x y) z))", "14"),
#     ],
# }


tests_dict["control_flow"] = {
    "if": [
            ("(if true 1 2)", "1"),
            ("(if false 1 2)", "2"),
            ("(if 0 1 2)", "1"),
            ("(if -1 1 2)", "1"),
            ],
}





# print(json.dumps(tests_dict, indent=2))
generate_tests(tests_dict)