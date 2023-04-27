from itertools import permutations
import os

op1 = ['add1', 'sub1']
op2 = ['+', '-', '*', '<', '>', '>=', '<=', '=']

tests = []
for op in op1:
    tests.append(f'({op} false)')

for op in op2:
    for a, b in permutations(['false', '0'], 2):
        tests.append(f'({op} {a} {b})')

for i, test in enumerate(tests):
    name = f'invalid_argument_{i:02d}'
    with open(os.path.join('tests', f'{name}.snek'), 'w', encoding='utf8') as output:
        output.write(test + '\n')
    print(f'''{{
        name: {name},
        file: "{name}.snek",
        expected: "invalid argument",
    }},''')