# Use functions as values to simplify

from functools import reduce

a = [1,2,3,4]

def add(a,b):
    return a + b

def multiply(a,b):
    return a*b


def alternate():
    def switcher():
        n = True
        while True:
            n = not n
            yield n
    gen = switcher()
    def inner(a, b):
        if next(gen): return add(a,b)
        else: return multiply(a,b)
    return inner

def black_box_extreme_computation(sequence):
    def inner(f):
        return reduce(f, sequence)
    return inner


print(f"add: {black_box_extreme_computation(a)(add)}")
print(f"multiply: {black_box_extreme_computation(a)(multiply)}")
print(f"alternate forward: {black_box_extreme_computation(a)(alternate())}")
print(f"alternate backward: {black_box_extreme_computation(reversed(a))(alternate())}")

