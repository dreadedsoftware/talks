# Typeclasses

from functools import reduce
from abc import ABC, abstractmethod
import random


a = [1,2,3,4]


class Monoid(ABC):
    def combine(a, b): pass
    


class Add(Monoid):
    def combine(a,b):
        return a+b


class Multiply(Monoid):
    def combine(a,b):
        return a*b


def monoid(F: Monoid):
    def decorator(f):
        def inner(*args, **kwargs):
            return f(*args, **kwargs)(F)

        return inner
    return decorator


def black_box_extreme_computation(sequence):
    def inner(f: Monoid):
        return reduce(f.combine, sequence)
    return inner


@monoid(Multiply)
def pi(sequence):
    return black_box_extreme_computation(sequence)

@monoid(Add)
def sigma(sequence):
    return black_box_extreme_computation(sequence)

print(f"pi(a) =\n   {pi(a)}")
print(f"pi(random.sample(a, len(a))) =\n   {pi(random.sample(a, len(a)))}")
print(f"pi([*a, 5]) =\n   {pi([*a, 5])}")

print(f"sigma(a) =\n   {sigma(a)}")
print(f"sigma(random.sample(a, len(a))) =\n   {sigma(random.sample(a, len(a)))}")
print(f"sigma([*a, 5]) =\n   {sigma([*a, 5])}")
