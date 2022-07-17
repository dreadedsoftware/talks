# Lambdas
free = [1,2,3,4,5,6]

from functools import reduce

print(reduce(lambda a,b: a + b, free))
print(reduce(lambda a,b: a * b, free))
print(reduce(lambda a,b: str(a) + str(b), free))