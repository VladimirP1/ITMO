import filecmp
from random import randint
from subprocess import call

def countset(x):
    c = 0
    while(x):
        c+=(x&1)
        x >>= 1
    return c

def check():
    print("iter")
    f = open("check.in", "w")
    n = 10
    print(n, 2**n, file = f)
    for i in range(2**n):
        print(countset(i), end=' ', file=f)
        x = i
        for j in range(10):
            if x & 1:
                print(j + 1, file=f, end=' ')
            x>>=1
        print(file=f)

    f.close()
    return call(["./a.out"]) == 0


#check()
while check():
    pass
