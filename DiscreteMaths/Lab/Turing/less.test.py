import random
import os
import subprocess

def test():
    n = 1000
    a = random.randint(0,n)
    b = random.randint(0,n)

    s = ' '.join("{0:b}".format(a) + "<" + "{0:b}".format(b)) + '\n'

    with open('in', 'w') as f:
        f.write(s)

    result = subprocess.run(['python3', 'visualizer.py', 'less.out', 'in', '0'])

    ans_corr = a < b
    ans_fact = result.returncode == 0

    is_ok = ans_corr == ans_fact

    print(a, b, ans_corr, ans_fact)

    return (s, is_ok)

while True:
    (s, ok) = test()
    if not ok:
        print ("Not OK: ", s)
        break

