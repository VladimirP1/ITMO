import sys
import time
from subprocess import Popen, PIPE

def run(program, input):
    with Popen([program],stdin = PIPE,stdout = PIPE) as proc:
        for line in input:
            print("Writing", line)
            time.sleep(0.01)
            proc.stdin.write((line + '\n').encode(encoding='UTF-8'))
        proc.stdin.close()
        return proc.stdout.read()

l = sys.stdin.read()
l = l.split('\n')
print(run(sys.argv[1], l).decode(encoding='UTF-8'),end='')
