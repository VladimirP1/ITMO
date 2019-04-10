MOD = 10**9 + 7
 
(k,m) = map(int, input().split())
c = list(map(int, input().split()))
 
x = [0 for i in range(2001)]
x[0] = 1
 
for k in range(1, m + 1):
    for cc in c:
        for j in range(0,k-cc+1):
            x[k] = (x[k] + x[j] * x[k - cc - j]) % MOD
    print(x[k], end=' ')
 
print()
