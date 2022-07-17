# Better yet call it a Monoid

from abc import ABC, abstractmethod


class Monoid(ABC):
    a = None
    @abstractmethod
    def combine(self, a, b): pass


from functools import reduce

a = [1,2,3,4]


class Add(Monoid):
    a = 0
    def combine(self, a,b):
        return a+b


class Multiply(Monoid):
    a = 1
    def combine(self, a,b):
        return a*b


def black_box_extreme_computation(sequence):
    def inner(f: Monoid):
        return reduce(f.combine, sequence, f.a)
    return inner


print(f"add: {black_box_extreme_computation(a)(Add())}")
print(f"multiply: {black_box_extreme_computation(a)(Multiply())}")
