import random

n = 30
apm = random.randint(0, n*(n-1)/2 - n)

hamil = [i + 1 for i in range(n)]
random.shuffle(hamil)

edges = set()

for i in range(n - 1):
    edges.add(tuple(sorted((hamil[i], hamil[i + 1]))))
edges.add(tuple(sorted((hamil[0], hamil[n - 1]))))

for i in range(apm):
    a = random.randint(1, n)
    b = random.randint(1, n)
    if a != b:
        edges.add(tuple(sorted((a, b))))

print(n, len(edges))
for i in edges:
    print(i[0], i[1])

for p in hamil:
    print(p, end=' ')
print()
#print(hamil)
#print(edges)
