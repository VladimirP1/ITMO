import random as rnd
from subprocess import Popen, PIPE

def LIST(depth):
    return "L(" + gen_sample(depth - 1) + ")"

def MSET(depth):
    return "S(" + gen_sample(depth - 1) + ")"

def PAIR(depth):
    return "P(" + gen_sample(depth - 1) + "," + gen_sample(depth - 1) + ")"

def TERM(depth):
    return "B"

def gen_sample(depth):
    if depth < 0:
        return TERM(depth)

    return rnd.choices([LIST, PAIR, MSET, TERM], k = 1)[0](depth)

def run(program, input):
    with Popen([program],stdin = PIPE,stdout = PIPE,text=True) as proc:
        proc.stdin.write(input)
        proc.stdin.close()
        return proc.stdout.read()

def test():
    inp = gen_sample(200)

    inp += '\n'
    
    A = run('./ref', inp).strip()
    B = run('./main', inp).strip()

    print(A, '\n', B, sep='')

    if A != B:
        print('Caused by: ', inp, end='')
        raise AssertionError("A != B")

def get_sample_len(x):
    l = 0
    s = ""
    while  0.9*x >= len(s) or len(s) >= 1.1*x:
        s = gen_sample(300)
    return s

print(get_sample_len(200))

quit()

while True:
    test()
