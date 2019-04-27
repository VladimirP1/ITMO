import random
import scipy.optimize
import sys

n = random.randint(2,300)
A = [[random.randint(0,10**6) for j in range(n)] for i in range(n)]

print(n)
for row in A:
    for el in row:
        print(el, end=' ')
    print()

R = scipy.optimize.linear_sum_assignment(A)

S = 0
for i in range(n):
    #print(R[0][i], R[1][i])
    S += A[R[0][i]][R[1][i]]

print(S, file = sys.stderr)
