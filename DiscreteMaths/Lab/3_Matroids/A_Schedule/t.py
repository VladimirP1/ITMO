import filecmp
from random import randint
from subprocess import call

def check():
    print("iter")
    f = open("schedule.in", "w")
    n = 100
    print(n, file = f)
    for i in range(n):
        print(randint(0,n), randint(0,1000), file = f)
        #print(1,1,file=f)

    f.close()
    #call(["pypy3", "main.py"])
    call(["./a.out"])
    call(["./ref"])
    #call(["ls", "-tl"])

    return filecmp.cmp("schedule.out", "schedule.out2")

#check()
while check():
    pass
