# Use functions as values to simplify

from functools import reduce

a = [1,2,3,4]

def add(a,b):
    return a + b

def multiply(a,b):
    return a*b


def black_box_extreme_computation(sequence):
    def inner(f):
        return reduce(f, sequence)
    return inner


print(f"add: {black_box_extreme_computation(a)(add)}")
print(f"multiply: {black_box_extreme_computation(a)(multiply)}")

# Why is this not "good enough"
