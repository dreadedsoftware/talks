# Functions as values
def add(a,b):
    return a + b

compose = add

print(f"compose is add: {compose(1, 2) == add(1, 2)}")

def same(f, g, a, b):
    print(f"{f(a, b) == g(a, b)} for {a} and {b}")

same(add, compose, 1, 2)