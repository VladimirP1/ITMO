
#n = 15
#print(2**n)
#for i in range(2**n):
#    print("{0:015b}".format(i)[(15-n):])

n = 15
print(2**15 + 2**10 + 2**6 + 2**3 + 2**1)
for i in range(2**n):
    print("{0:015b}".format(i)[(15-n):])
n = 10
for i in range(2**n):
    print("{0:015b}".format(i)[(15-n):])
n = 6
for i in range(2**n):
    print("{0:015b}".format(i)[(15-n):])
n = 3
for i in range(2**n):
    print("{0:015b}".format(i)[(15-n):])
n = 1
for i in range(2**n):
    print("{0:015b}".format(i)[(15-n):])
