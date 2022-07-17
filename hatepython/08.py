# Associative Operator; adds context

from abc import ABC, abstractmethod


class AssociativeOperator(ABC):
    @abstractmethod
    def combine(self, a, b): pass


from functools import reduce

a = [1,2,3,4]


class Add(AssociativeOperator):
    def combine(self, a,b):
        return a+b


class Multiply(AssociativeOperator):
    def combine(self, a,b):
        return a*b


def black_box_extreme_computation(sequence):
    def inner(f: AssociativeOperator):
        return reduce(f.combine, sequence)
    return inner


print(f"add: {black_box_extreme_computation(a)(Add())}")
print(f"multiply: {black_box_extreme_computation(a)(Multiply())}")