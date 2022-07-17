# Let's do it

from functools import reduce

a = [1,2,3,4]

def black_box_extreme_computation_1(sequence):
    return reduce(lambda a,b: a+b, sequence)
def black_box_extreme_computation_2(sequence):
    return reduce(lambda a,b: a*b, sequence)


print(f"add: {black_box_extreme_computation_1(a)}")
print(f"multiply: {black_box_extreme_computation_2(a)}")
