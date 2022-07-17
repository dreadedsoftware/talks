# Free Monoid with fold
free = [1,2,3,4,5,6]
def c1(a, b): return a + b
def c2(a, b): return a * b
def c3(a, b): return str(a) + str(b)

from functools import reduce

print(reduce(c1, free, 0))
print(reduce(c2, free, 1))
print(reduce(c3, free, ""))