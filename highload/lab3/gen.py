import random

n = 840
with open('test.txt', 'w') as f:
    f.write(str(n))
    f.write('\n')
    for i in range(n):
        for j in range(n+1):
            f.write(str(random.randint(0, 1000)))
            f.write(' ')
        f.write('\n')

