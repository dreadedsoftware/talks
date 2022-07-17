# Monads

from functools import reduce
from abc import ABC
a = [1,2,3,4]


class Functor(ABC):
    def lift(a): pass
    def map(f): pass

class ListFunctor(Functor):
    def lift(a): return [a]
    def map(f):
        def inner(a):
            return map(f, a)
        return inner

class AnyFunctor(Functor):
    def lift(a): return a
    def map(f):
        def inner(a):
            if a != None: return f(a)
            else: return a
        return inner


class Monad(ABC):
    F: Functor = None
    def bind(a, f): pass


class ListMonad(Monad):
    F = ListFunctor
    def bind(self, a, f):
        listList = self.F.map(f)(a)
        return [b for a in listList for b in a]


class AnyMonad(Monad):
    F = AnyFunctor
    def bind(self, a, f):
        optOpt = self.F.map(f)(a)
        return optOpt


def monad(F: Monad):
    def decorator(f):
        def inner(*args, **kwargs):
            return f(*args, **kwargs)(F)

        return inner
    return decorator


def chain(a, *args):
    def inner(F: Monad):
        return reduce(lambda a,b: F.bind(F, a, b), args, a)
    return inner


@monad(ListMonad)
def flatMap(a, *args):
    return chain(a, *args)


@monad(AnyMonad)
def noneCheck(a, *args):
    return chain(a, *args)

def toRange(a): return list(range(a))
def repeater(a): return [a] * 2
def doubler(a): return [a*2]
print(f"flatMap(a, doubler, repeater, toRange) =\n   {flatMap(a, doubler, repeater, toRange)}")

def stringToInt(a):
    try:
        return int(a)
    except:
        None
def intToList(a):
    return list(range(a))
print(f"noneCheck('a', stringToInt, intToList) =\n   {noneCheck('a', stringToInt, intToList)}")
print(f"noneCheck('7', stringToInt, intToList) =\n   {noneCheck('7', stringToInt, intToList)}")
print(f"flatMap(noneCheck('7', stringToInt, intToList), intToList) =\n   {flatMap(noneCheck('7', stringToInt, intToList), intToList)}")