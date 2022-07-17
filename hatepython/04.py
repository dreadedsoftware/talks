# Strong Types
### There is a difference between `Strong` and `Static` Typing
### Static types are not necessary for FP, just nice to have

a = '1'
b = 1

print(f"a={a} is a {type(a)}; b={b} is a {type(b)}; a == b is {a == b}")
print(f"a+a gives {a+a}")
print(f"b+b gives {b+b}")
print(f"a+b error")
print(a+b)